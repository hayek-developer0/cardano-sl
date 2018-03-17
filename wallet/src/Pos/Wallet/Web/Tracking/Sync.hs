{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- To support actual wallet accounts we listen to applications and rollbacks
-- of blocks, extract transactions from block and extract our
-- accounts (such accounts which we can decrypt).
-- We synchronise wallet-db (acidic-state) with node-db
-- and support last seen tip for each walletset.
-- There are severals cases when we must  synchronise wallet-db and node-db:
-- • When we relaunch wallet. Desynchronization can be caused by interruption
--   during blocks application/rollback at the previous launch,
--   then wallet-db can fall behind from node-db (when interrupted during rollback)
--   or vice versa (when interrupted during application)
--   @syncWSetsWithGStateLock@ implements this functionality.
-- • When a user wants to import a secret key. Then we must rely on
--   Utxo (GStateDB), because blockchain can be large.

module Pos.Wallet.Web.Tracking.Sync
       ( syncWallet
       , processSyncRequest
       , trackingApplyTxs
       , trackingRollbackTxs
       , applyModifierToWallet
       , rollbackModifierFromWallet

       , txMempoolToModifier

       , buildTHEntryExtra
       , isTxEntryInteresting

       -- For tests
       , evalChange
       ) where

import           Control.Monad.Except (MonadError (throwError))
import           Universum
import           UnliftIO (MonadUnliftIO)
import           Unsafe (unsafeLast)

import           Control.Concurrent.STM (readTBQueue)
import           Control.Exception.Safe (handleAny)
import           Control.Lens (to)
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import           Formatting (build, float, sformat, (%))
import           System.Wlog (HasLoggerName, WithLogger, logDebug, logError, logInfo, logWarning,
                              modifyLoggerName)

import           Pos.Block.Types (Blund, undoTx)
import           Pos.Client.Txp.History (TxHistoryEntry (..), txHistoryListToMap)
import           Pos.Core (ChainDifficulty, HasConfiguration, HasDifficulty (..),
                           HasProtocolConstants, HeaderHash, Timestamp, blkSecurityParam,
                           genesisHash, headerHash, headerSlotL, timestampToPosix)
import           Pos.Core.Block (BlockHeader (..), getBlockHeader, mainBlockTxPayload)
import           Pos.Core.Txp (TxAux (..), TxId, TxUndo)
import           Pos.Crypto (WithHash (..), shortHashF, withHash)
import           Pos.DB.Block (getBlund)
import qualified Pos.DB.Block.Load as GS
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead (..))
import qualified Pos.GState as GS
import           Pos.GState.BlockExtra (resolveForwardLink)
import           Pos.Slotting (MonadSlots (..), MonadSlotsData, getSlotStartPure, getSystemStartM)
import           Pos.Slotting.Types (SlottingData)
import           Pos.StateLock (Priority (..), StateLock, withStateLockNoMetrics)
import           Pos.Txp (UndoMap, flattenTxPayload, topsortTxs, _txOutputs)
import           Pos.Util.Chrono (getNewestFirst)
import           Pos.Util.LogSafe (buildSafe, logErrorSP, logInfoSP, logWarningSP, secretOnlyF,
                                   secure)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Servant (encodeCType)
import           Pos.Util.Util (HasLens (..), getKeys)

import           Pos.Wallet.Web.ClientTypes (Addr, CId, CTxMeta (..), CWAddressMeta (..), Wal,
                                             addrMetaToAccount)
import           Pos.Wallet.Web.Error.Types (WalletError (..))
import           Pos.Wallet.Web.Pending.Types (PtxBlockInfo,
                                               PtxCondition (PtxApplying, PtxInNewestBlocks))
import           Pos.Wallet.Web.State (CustomAddressType (..), WalletDB, WalletDbReader,
                                       WalletSnapshot, WalletTip (..))
import qualified Pos.Wallet.Web.State as WS
import qualified Pos.Wallet.Web.State.State as WS
import           Pos.Wallet.Web.Tracking.Decrypt (THEntryExtra (..), WalletDecrCredentials,
                                                  buildTHEntryExtra, isTxEntryInteresting)
import           Pos.Wallet.Web.Tracking.Modifier (CAccModifier (..), VoidModifier,
                                                   deleteAndInsertIMM, deleteAndInsertMM,
                                                   deleteAndInsertVM, indexedDeletions,
                                                   sortedInsertions)
import           Pos.Wallet.Web.Tracking.Types


-- | Sync a wallet with the last state of the blockchain, given its 'WalletDecrCredentials'.
-- The update of the balance will be done immediately and synchronously, the transaction history
-- will instead be recovered asynchronously.
syncWallet :: ( WalletDbReader ctx m
              , MonadDBRead m
              , WithLogger m
              , HasLens StateLock ctx StateLock
              , HasLens SyncQueue ctx SyncQueue
              , MonadMask m
              , MonadSlotsData ctx m
              , MonadUnliftIO m
              ) => WalletDecrCredentials -> m ()
syncWallet credentials = submitSyncRequest (newSyncRequest credentials)

-- | Asynchronously process a 'SyncRequest' by reading incoming
-- requests from a 'SyncQueue', in an infinite loop.
processSyncRequest :: ( WalletDbReader ctx m
                      , BlockLockMode ctx m
                      , MonadSlotsData ctx m
                      , HasConfiguration
                      , MonadIO m
                      ) => SyncQueue -> m ()
processSyncRequest syncQueue = do
    newRequest <- atomically (readTBQueue syncQueue)
    syncWalletWithBlockchain newRequest >>= either processSyncError pure
    processSyncRequest syncQueue

txMempoolToModifier :: WalletTrackingEnv ctx m
                    => WalletSnapshot
                    -> ([(TxId, TxAux)], UndoMap) -- ^ Transactions and UndoMap from mempool
                    -> WalletDecrCredentials
                    -> m CAccModifier
txMempoolToModifier ws (txs, undoMap) credentials = do
    let wHash (i, TxAux {..}, _) = WithHash taTx i
        getDiff       = const Nothing  -- no difficulty (mempool txs)
        getTs         = const Nothing  -- don't give any timestamp
        getPtxBlkInfo = const Nothing  -- no slot of containing block

    txsWUndo <- forM txs $ \(id, tx) -> case HM.lookup id undoMap of
        Just undo -> pure (id, tx, undo)
        Nothing -> do
            let errMsg sl = sformat ("There is no undo corresponding to TxId #"%secretOnlyF sl build%" from txp mempool") id
            logErrorSP errMsg
            throwM $ InternalError (errMsg secure)

    case topsortTxs wHash txsWUndo of
        Nothing      -> mempty <$ logWarning "txMempoolToModifier: couldn't topsort mempool txs"
        Just ordered -> do
            tipH <- DB.getTipHeader
            let dbUsed = WS.getCustomAddresses ws WS.UsedAddr
            pure $
                trackingApplyTxs credentials dbUsed getDiff getTs getPtxBlkInfo $
                map (\(_, tx, undo) -> (tx, undo, tipH)) ordered


-- | Process each 'SyncError'. The current implementation just logs the errors without exposing it
-- to the upper layers.
processSyncError :: ( WithLogger m , MonadIO m ) => SyncError -> m ()
processSyncError sr = case sr of
    GenesisBlockHeaderNotFound -> logError "Couldn't extract the genesis block header from the database."
    GenesisHeaderHashNotFound  -> logError "Couldn't extract the genesis header hash from the database."
    NoSyncTipAvailable walletId ->
        logWarningSP $ \sl -> sformat ("There is no syncTip corresponding to wallet #"%secretOnlyF sl build) walletId
    NotSyncable walletId walletError -> do
        logErrorSP   $ \sl -> sformat ("Wallet #" % secretOnlyF sl build
                                                  % " is not syncable. Error was: "
                                                  % build) walletId walletError
    SyncFailed  walletId exception -> do
        let errMsg sl = "Sync failed for Wallet #" % secretOnlyF sl build
                                                   % ". An exception was raised during the sync process: "
                                                   % build
        logErrorSP $ \sl -> sformat (errMsg sl) walletId exception

-- | Iterates over blocks (using forward links) and reconstructs the transaction
-- history for the given wallet.
syncWalletWithBlockchain
    :: forall ctx m.
    ( WalletDbReader ctx m
    , BlockLockMode ctx m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => SyncRequest
    -> m (Either SyncError ())
syncWalletWithBlockchain syncRequest = setLogger $ do
    ws <- WS.askWalletSnapshot
    let (_, walletId) = srCredentials syncRequest
    let onError       = pure . Left . SyncFailed walletId
    handleAny onError $ do
        let currentTip = WS.getWalletSyncTip ws walletId
        case currentTip of
            Nothing                -> pure $ Left (NoSyncTipAvailable walletId)
            Just NotSynced         -> do
                genesisHeader <- firstGenesisHeader
                either (pure . Left . identity) syncDo genesisHeader
            Just (SyncedWith wTip) -> DB.getHeader wTip >>= \case
                Nothing ->
                    let err = InternalError $
                              sformat ("Couldn't get block header of wallet by last synced hh: "%build) wTip
                    in pure $ Left (NotSyncable walletId err)
                Just wHeader -> syncDo wHeader
  where
    syncDo :: BlockHeader -> m (Either SyncError ())
    syncDo walletTipHeader = do
        let wdiff = (fromIntegral . ( ^. difficultyL) $ walletTipHeader) :: Word32
        gstateTipH <- DB.getTipHeader
        -- If account's syncTip is before the current gstate's tip,
        -- then it loads accounts and addresses starting with @wHeader@.
        -- syncTip can be before gstate's the current tip
        -- when we call @syncWalletSetWithTip@ at the first time
        -- or if the application was interrupted during rollback.
        -- We don't load all blocks explicitly, because blockchain can be long.
        (syncResult, wNewTip) <-
            if (gstateTipH ^. difficultyL > fromIntegral blkSecurityParam + fromIntegral wdiff) then do
                -- Wallet tip is "far" from gState tip,
                -- rollback can't occur more then @blkSecurityParam@ blocks,
                -- so we can sync wallet and GState without the block lock
                -- to avoid blocking of blocks verification/application.
                bh <- unsafeLast . getNewestFirst <$> GS.loadHeadersByDepth (blkSecurityParam + 1) (headerHash gstateTipH)
                logInfo $
                    sformat ("Wallet's tip is far from GState tip. Syncing with "%build%" without the block lock")
                    (headerHash bh)
                result <- syncWalletWithBlockchainUnsafe syncRequest walletTipHeader bh
                pure $ (Just result, bh)
            else pure (Nothing, walletTipHeader)

        let finaliseSyncUnderBlockLock = withStateLockNoMetrics HighPriority $ \tip -> do
                logInfo $ sformat ("Syncing wallet with "%build%" under the block lock") tip
                tipH <- maybe (error "No block header corresponding to tip") pure =<< DB.getHeader tip
                syncWalletWithBlockchainUnsafe syncRequest wNewTip tipH

        case syncResult of
            Nothing         -> finaliseSyncUnderBlockLock
            Just (Right ()) -> finaliseSyncUnderBlockLock
            Just failedSync -> pure failedSync

----------------------------------------------------------------------------
-- Unsafe operations. Core logic.
----------------------------------------------------------------------------
-- These operation aren't atomic and don't take the block lock.

-- BE CAREFUL! This function iterates over blockchain, the blockchain can be large.
syncWalletWithBlockchainUnsafe
    :: forall ctx m .
    ( WalletDbReader ctx m
    , MonadDBRead m
    , WithLogger m
    , MonadSlotsData ctx m
    , HasConfiguration
    )
    => SyncRequest
    -> BlockHeader
    -- ^ Block header corresponding to wallet's tip. It can map
    -- to the genesis BlockHeader if this is a brand new wallet being
    -- synced or restored.
    -> BlockHeader
    -- ^ Blockchain's tip header hash
    -> m (Either SyncError ())
syncWalletWithBlockchainUnsafe syncRequest walletTip blockchainTip = setLogger $ do
    let credentials@(_, walletId) = srCredentials syncRequest
    systemStart  <- getSystemStartM
    slottingData <- GS.getSlottingData
    db <- WS.askWalletDB
    ws <- WS.getWalletSnapshot db

    let getBlockHeaderTimestamp = blockHeaderTimestamp systemStart slottingData

    let dbUsed = WS.getCustomAddresses ws WS.UsedAddr
    logDebug "Starting sync via computeAccModifier..."
    (mapModifier, newSyncTip) <- computeAccModifier credentials getBlockHeaderTimestamp walletTip dbUsed mempty 0
    logInfo $ sformat ("Found new interesting mapModifier, new sync tip will be " %  build) newSyncTip
    applyModifierToWallet db (srOperation syncRequest) walletId (headerHash newSyncTip) mapModifier
    logInfoSP $ \sl -> sformat ("Applied " %buildSafe sl) mapModifier

    case headerHash newSyncTip == headerHash blockchainTip of
        True -> do
            logInfoSP $ \sl ->
                sformat ("Wallet "%secretOnlyF sl build%" has been synced with tip "
                        %shortHashF%", "%buildSafe sl)
                        walletId (headerHash newSyncTip) mapModifier
            pure $ Right ()
        False -> syncWalletWithBlockchainUnsafe syncRequest newSyncTip blockchainTip

    where
        -- | Main workhorse which iterates over the blockchain and reconstruct the transaction
        -- history. Yields a new 'CAccModifier', give or take, every 10000 blocks.
        computeAccModifier :: WalletDecrCredentials
                           -> (BlockHeader -> Maybe Timestamp)
                           -> BlockHeader
                           -- ^ The current wallet 'HeaderHash'.
                           -> [(CId Addr, HeaderHash)]
                           -- ^ Used addresses.
                           -> CAccModifier
                           -- ^ The initial CAccModifier we will fold on.
                           -> Int
                           -- ^ The initial block count accumulator. Every X blocks we will stop the recursion.
                           -> m (CAccModifier, BlockHeader)
                           -- ^ The new wallet modifier and the new sync tip for the wallet.
        computeAccModifier credentials getBlockTimestamp wHeader dbUsed currentModifier currentBlockCount = do
            case currentBlockCount >= 10000 of
                True -> do
                    let progress localDepth totalDepth = ((fromIntegral localDepth) * 100.0) / fromIntegral totalDepth
                    logDebug $ sformat ("Progress: " % float @Double % "%") (progress (depthOf wHeader) (depthOf blockchainTip))
                    pure (currentModifier, wHeader)
                False -> do
                    let walletId = snd credentials
                    if | depthOf blockchainTip > depthOf wHeader -> do
                             -- If wallet's syncTip is before than the current tip in the blockchain,
                             -- then it loads wallets starting with @wHeader@.
                             -- Sync tip can be before the current tip
                             -- when we call @syncWalletSetWithTip@ at the first time
                             -- or if the application was interrupted during rollback.
                             -- We don't load blocks explicitly, because blockain can be long.
                             nextBlund <- resolveForwardLink wHeader >>= (maybe (pure Nothing) getBlund)
                             case nextBlund of
                                 Nothing -> pure (currentModifier, blockchainTip)
                                 Just blund@(currentBlock, _) -> do
                                     newModifier <- (currentModifier <>) <$> applyBlock credentials getBlockTimestamp dbUsed blund
                                     computeAccModifier credentials
                                                        getBlockTimestamp
                                                        (getBlockHeader currentBlock)
                                                        dbUsed
                                                        newModifier
                                                        (currentBlockCount + 1)

                       | depthOf blockchainTip < depthOf wHeader -> do
                             -- This rollback can occur
                             -- if the application was interrupted during blocks application.
                             blunds <- getNewestFirst <$> GS.loadBlundsWhile (\b -> getBlockHeader b /= blockchainTip) (headerHash wHeader)
                             let newModifier = foldl' (\r b -> r <> rollbackBlock credentials getBlockTimestamp dbUsed b) currentModifier blunds
                             pure (newModifier, getBlockHeader . fst . unsafeLast $ blunds)
                       | otherwise -> do
                             logInfoSP $ \sl -> sformat ("Wallet " % secretOnlyF sl build %" is already synced") walletId
                             pure (mempty, blockchainTip)

        gbTxs = either (const []) (^. mainBlockTxPayload . to flattenTxPayload)

        blockHeaderTimestamp :: Timestamp -> SlottingData -> BlockHeader -> Maybe Timestamp
        blockHeaderTimestamp systemStart slottingData = \case
            BlockHeaderGenesis _ -> Nothing
            BlockHeaderMain h -> getSlotStartPure systemStart (h ^. headerSlotL) slottingData

        -- assuming that transactions are not created until syncing is complete
        ptxBlkInfo = const Nothing

        rollbackBlock :: WalletDecrCredentials
                      -> (BlockHeader -> Maybe Timestamp)
                      -> [(CId Addr, HeaderHash)]
                      -> Blund
                      -> CAccModifier
        rollbackBlock credentials blkHeaderTs dbUsed (b, u) =
            trackingRollbackTxs credentials dbUsed (Just . depthOf) blkHeaderTs $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)

        applyBlock :: WalletDecrCredentials
                   -> (BlockHeader -> Maybe Timestamp)
                   -> [(CId Addr, HeaderHash)]
                   -> Blund
                   -> m CAccModifier
        applyBlock credentials blkHeaderTs dbUsed (b, u) = pure $
            trackingApplyTxs credentials dbUsed (Just . depthOf) blkHeaderTs ptxBlkInfo $
            zip3 (gbTxs b) (undoTx u) (repeat $ getBlockHeader b)


-- | Gets the 'ChainDifficulty' for either a 'GenesisBlock' or
-- a 'MainBlock'.
depthOf :: HasDifficulty a => a -> ChainDifficulty
depthOf = view difficultyL

firstGenesisHeader :: MonadDBRead m => m (Either SyncError BlockHeader)
firstGenesisHeader = runExceptT $ do
    genesisHeaderHash  <- resolveForwardLink (genesisHash @BlockHeader)
    case genesisHeaderHash of
        Nothing  -> throwError GenesisHeaderHashNotFound
        Just ghh -> do
            genesisBlockHeader <- DB.getHeader ghh
            maybe (throwError GenesisBlockHeaderNotFound) pure genesisBlockHeader

constructAllUsed
    :: [(CId Addr, HeaderHash)]
    -> VoidModifier (CId Addr, HeaderHash)
    -> HashSet (CId Addr)
constructAllUsed dbUsed modif =
    HS.map fst $
    getKeys $
    MM.modifyHashMap modif $
    HM.fromList $
    zip dbUsed (repeat ()) -- not so good performance :(

-- Process transactions on block application,
-- decrypt our addresses, and add/delete them to/from wallet-db.
-- Addresses are used in TxIn's will be deleted,
-- in TxOut's will be added.
trackingApplyTxs
    :: HasConfiguration
    => WalletDecrCredentials
    -> [(CId Addr, HeaderHash)]               -- ^ All used addresses from db along with their HeaderHashes
    -> (BlockHeader -> Maybe ChainDifficulty) -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)       -- ^ Function to determine tx timestamp in history
    -> (BlockHeader -> Maybe PtxBlockInfo)    -- ^ Function to determine pending tx's block info
    -> [(TxAux, TxUndo, BlockHeader)]         -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingApplyTxs credentials dbUsed getDiff getTs getPtxBlkInfo txs =
    foldl' applyTx mempty txs
  where
    applyTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    applyTx CAccModifier{..} (tx, undo, blkHeader) = do
        let hh = headerHash blkHeader
            hhs = repeat hh
            wh@(WithHash _ txId) = withHash (taTx tx)
        let thee@THEntryExtra{..} =
                buildTHEntryExtra credentials (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxIns = map (fst . fst) theeInputs
            ownTxOuts = map fst theeOutputs

            addedHistory = maybe camAddedHistory (flip DL.cons camAddedHistory) (isTxEntryInteresting thee)

            usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs = evalChange
                              (constructAllUsed dbUsed camUsed)
                              (map snd theeInputs)
                              (map snd theeOutputs)
                              (length theeOutputs == NE.length (_txOutputs $ taTx tx))

            mPtxBlkInfo = getPtxBlkInfo blkHeader
            addedPtxCandidates =
                if | Just ptxBlkInfo <- mPtxBlkInfo
                     -> DL.cons (txId, ptxBlkInfo) camAddedPtxCandidates
                   | otherwise
                     -> camAddedPtxCandidates
        CAccModifier
            (deleteAndInsertIMM [] (map snd theeOutputs) camAddresses)
            (deleteAndInsertVM [] (zip usedAddrs hhs) camUsed)
            (deleteAndInsertVM [] (zip changeAddrs hhs) camChange)
            (deleteAndInsertMM ownTxIns ownTxOuts camUtxo)
            addedHistory
            camDeletedHistory
            addedPtxCandidates
            camDeletedPtxCandidates

-- Process transactions on block rollback.
-- Like @trackingApplyTxs@, but vise versa.
trackingRollbackTxs
    :: HasConfiguration
    => WalletDecrCredentials
    -> [(CId Addr, HeaderHash)]                -- ^ All used addresses from db along with their HeaderHashes
    -> (BlockHeader -> Maybe ChainDifficulty)  -- ^ Function to determine tx chain difficulty
    -> (BlockHeader -> Maybe Timestamp)        -- ^ Function to determine tx timestamp in history
    -> [(TxAux, TxUndo, BlockHeader)]          -- ^ Txs of blocks and corresponding header hash
    -> CAccModifier
trackingRollbackTxs credentials dbUsed getDiff getTs txs =
    foldl' rollbackTx mempty txs
  where
    rollbackTx :: CAccModifier -> (TxAux, TxUndo, BlockHeader) -> CAccModifier
    rollbackTx CAccModifier{..} (tx, undo, blkHeader) = do
        let wh@(WithHash _ txId) = withHash (taTx tx)
            hh = headerHash blkHeader
            hhs = repeat hh
            thee@THEntryExtra{..} =
                buildTHEntryExtra credentials (wh, undo) (getDiff blkHeader, getTs blkHeader)

            ownTxOutIns = map (fst . fst) theeOutputs
            deletedHistory = maybe camDeletedHistory (DL.snoc camDeletedHistory) (isTxEntryInteresting thee)
            deletedPtxCandidates = DL.cons (txId, theeTxEntry) camDeletedPtxCandidates

        -- Rollback isn't needed, because we don't use @utxoGet@
        -- (undo contains all required information)
        let usedAddrs = map (cwamId . snd) theeOutputs
            changeAddrs =
                evalChange
                    (constructAllUsed dbUsed camUsed)
                    (map snd theeInputs)
                    (map snd theeOutputs)
                    (length theeOutputs == NE.length (_txOutputs $ taTx tx))
        CAccModifier
            (deleteAndInsertIMM (map snd theeOutputs) [] camAddresses)
            (deleteAndInsertVM (zip usedAddrs hhs) [] camUsed)
            (deleteAndInsertVM (zip changeAddrs hhs) [] camChange)
            (deleteAndInsertMM ownTxOutIns (map fst theeInputs) camUtxo)
            camAddedHistory
            deletedHistory
            camAddedPtxCandidates
            deletedPtxCandidates

applyModifierToWallet
    :: (MonadIO m, HasConfiguration)
    => WalletDB
    -> TrackingOperation
    -> CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
applyModifierToWallet db _trackingOperation wid newTip CAccModifier{..} = do

    let cMetas = mapMaybe (\THEntry {..} -> (\mts -> (encodeCType _thTxId
                                                     , CTxMeta . timestampToPosix $ mts)
                                            ) <$> _thTimestamp)
               $ DL.toList camAddedHistory

    -- FIXME(adn) If this is a sync operation we do want the balance to be influenced, if this is
    -- a restore we don't.
    WS.applyModifierToWallet
      db
      wid
      (sortedInsertions camAddresses)
      [ (UsedAddr, fst <$> MM.insertions camUsed)
      , (ChangeAddr, fst <$> MM.insertions camChange)
      ]
      camUtxo
      cMetas
      (txHistoryListToMap $ DL.toList camAddedHistory)
      (DL.toList $ second PtxInNewestBlocks <$> camAddedPtxCandidates)
      newTip

rollbackModifierFromWallet
    :: ( MonadSlots ctx m
       , HasProtocolConstants
       , HasConfiguration
       )
    => WalletDB
    -> CId Wal
    -> HeaderHash
    -> CAccModifier
    -> m ()
rollbackModifierFromWallet db wid newTip CAccModifier{..} = do
    curSlot <- getCurrentSlotInaccurate

    WS.rollbackModifierFromWallet
      db
      wid
      (indexedDeletions camAddresses)
      [ (UsedAddr, MM.deletions camUsed)
      , (ChangeAddr, MM.deletions camChange)
      ]
      camUtxo
      (txHistoryListToMap (DL.toList camDeletedHistory))
      ((\(txId, poolInfo) -> ( txId, PtxApplying poolInfo
                             , WS.PtxResetSubmitTiming curSlot))
        <$> DL.toList camDeletedPtxCandidates
      )
      newTip

-- Change address is an address which money remainder is sent to.
-- We will consider output address as "change" if:
-- 1. it belongs to source account (taken from one of source addresses)
-- 2. it's not mentioned in the blockchain (aka isn't "used" address)
-- 3. there is at least one non "change" address among all outputs ones

-- The first point is very intuitive and needed for case when we
-- send tx to somebody, i.e. to not our address.
-- The second point is needed for case when
-- we send a tx from our account to the same account.
-- The third point is needed for case when we just created address
-- in an account and then send a tx from the address belonging to this account
-- to the created.
-- In this case both output addresses will be treated as "change"
-- by the first two rules.
-- But the third rule will make them not "change".
-- This decision is controversial, but we can't understand from the blockchain
-- which of them is really "change".
-- There is an option to treat both of them as "change", but it seems to be more puzzling.
evalChange
    :: HashSet (CId Addr)
    -> [CWAddressMeta] -- ^ Own input addresses of tx
    -> [CWAddressMeta] -- ^ Own outputs addresses of tx
    -> Bool            -- ^ Whether all tx's outputs are our own
    -> [CId Addr]
evalChange allUsed inputs outputs allOutputsOur
    | [] <- inputs = [] -- It means this transaction isn't our outgoing transaction.
    | inp : _ <- inputs =
        let srcAccount = addrMetaToAccount inp in
        -- Apply the first point.
        let addrFromSrcAccount = HS.fromList $ map cwamId $ filter ((== srcAccount) . addrMetaToAccount) outputs in
        -- Apply the second point.
        let potentialChange = addrFromSrcAccount `HS.difference` allUsed in
        -- Apply the third point.
        if allOutputsOur && potentialChange == HS.fromList (map cwamId outputs) then []
        else HS.toList potentialChange

setLogger :: HasLoggerName m => m a -> m a
setLogger = modifyLoggerName (const "syncWalletWorker")
