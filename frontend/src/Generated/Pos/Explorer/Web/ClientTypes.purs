-- File auto generated by purescript-bridge! --
module Pos.Explorer.Web.ClientTypes where

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe, Maybe(..))
import Data.Time.NominalDiffTime (NominalDiffTime)
import Data.Tuple (Tuple)
import Pos.Core.Types (Coin)
import Prim (Array, Int, String)

import Prelude
import Data.Generic (class Generic)

newtype CAddress =
    CAddress String

derive instance genericCAddress :: Generic CAddress

--------------------------------------------------------------------------------
_CAddress :: Prism' CAddress String
_CAddress = prism' CAddress f
  where
    f (CAddress a) = Just $ a


--------------------------------------------------------------------------------
newtype CAddressSummary =
    CAddressSummary {
      caAddress :: CAddress
    , caTxNum :: Int
    , caBalance :: Coin
    , caTxList :: Array CTxBrief
    }

derive instance genericCAddressSummary :: Generic CAddressSummary

--------------------------------------------------------------------------------
_CAddressSummary :: Prism' CAddressSummary { caAddress :: CAddress, caTxNum :: Int, caBalance :: Coin, caTxList :: Array CTxBrief}
_CAddressSummary = prism' CAddressSummary f
  where
    f (CAddressSummary r) = Just r


--------------------------------------------------------------------------------
newtype CBlockEntry =
    CBlockEntry {
      cbeEpoch :: Int
    , cbeSlot :: Int
    , cbeBlkHash :: CHash
    , cbeTimeIssued :: Maybe NominalDiffTime
    , cbeTxNum :: Int
    , cbeTotalSent :: Coin
    , cbeSize :: Int
    , cbeRelayedBy :: Maybe String
    }

derive instance genericCBlockEntry :: Generic CBlockEntry

--------------------------------------------------------------------------------
_CBlockEntry :: Prism' CBlockEntry { cbeEpoch :: Int, cbeSlot :: Int, cbeBlkHash :: CHash, cbeTimeIssued :: Maybe NominalDiffTime, cbeTxNum :: Int, cbeTotalSent :: Coin, cbeSize :: Int, cbeRelayedBy :: Maybe String}
_CBlockEntry = prism' CBlockEntry f
  where
    f (CBlockEntry r) = Just r


--------------------------------------------------------------------------------
newtype CBlockSummary =
    CBlockSummary {
      cbsEntry :: CBlockEntry
    , cbsPrevHash :: CHash
    , cbsNextHash :: Maybe CHash
    , cbsMerkleRoot :: CHash
    }

derive instance genericCBlockSummary :: Generic CBlockSummary

--------------------------------------------------------------------------------
_CBlockSummary :: Prism' CBlockSummary { cbsEntry :: CBlockEntry, cbsPrevHash :: CHash, cbsNextHash :: Maybe CHash, cbsMerkleRoot :: CHash}
_CBlockSummary = prism' CBlockSummary f
  where
    f (CBlockSummary r) = Just r


--------------------------------------------------------------------------------
newtype CHash =
    CHash String

derive instance genericCHash :: Generic CHash

--------------------------------------------------------------------------------
_CHash :: Prism' CHash String
_CHash = prism' CHash f
  where
    f (CHash a) = Just $ a


--------------------------------------------------------------------------------
newtype CNetworkAddress =
    CNetworkAddress String

derive instance genericCNetworkAddress :: Generic CNetworkAddress

--------------------------------------------------------------------------------
_CNetworkAddress :: Prism' CNetworkAddress String
_CNetworkAddress = prism' CNetworkAddress f
  where
    f (CNetworkAddress a) = Just $ a


--------------------------------------------------------------------------------
newtype CTxBrief =
    CTxBrief {
      ctbId :: CTxId
    , ctbTimeIssued :: NominalDiffTime
    , ctbInputs :: Array (Tuple CAddress Coin)
    , ctbOutputs :: Array (Tuple CAddress Coin)
    }

derive instance genericCTxBrief :: Generic CTxBrief

--------------------------------------------------------------------------------
_CTxBrief :: Prism' CTxBrief { ctbId :: CTxId, ctbTimeIssued :: NominalDiffTime, ctbInputs :: Array (Tuple CAddress Coin), ctbOutputs :: Array (Tuple CAddress Coin)}
_CTxBrief = prism' CTxBrief f
  where
    f (CTxBrief r) = Just r


--------------------------------------------------------------------------------
newtype CTxEntry =
    CTxEntry {
      cteId :: CTxId
    , cteTimeIssued :: NominalDiffTime
    , cteAmount :: Coin
    }

derive instance genericCTxEntry :: Generic CTxEntry

--------------------------------------------------------------------------------
_CTxEntry :: Prism' CTxEntry { cteId :: CTxId, cteTimeIssued :: NominalDiffTime, cteAmount :: Coin}
_CTxEntry = prism' CTxEntry f
  where
    f (CTxEntry r) = Just r


--------------------------------------------------------------------------------
newtype CTxId =
    CTxId CHash

derive instance genericCTxId :: Generic CTxId

--------------------------------------------------------------------------------
_CTxId :: Prism' CTxId CHash
_CTxId = prism' CTxId f
  where
    f (CTxId a) = Just $ a


--------------------------------------------------------------------------------
newtype CTxSummary =
    CTxSummary {
      ctsId :: CTxId
    , ctsTxTimeIssued :: NominalDiffTime
    , ctsBlockTimeIssued :: Maybe NominalDiffTime
    , ctsBlockHeight :: Maybe Int
    , ctsRelayedBy :: Maybe CNetworkAddress
    , ctsTotalInput :: Coin
    , ctsTotalOutput :: Coin
    , ctsFees :: Coin
    , ctsInputs :: Array (Tuple CAddress Coin)
    , ctsOutputs :: Array (Tuple CAddress Coin)
    }

derive instance genericCTxSummary :: Generic CTxSummary

--------------------------------------------------------------------------------
_CTxSummary :: Prism' CTxSummary { ctsId :: CTxId, ctsTxTimeIssued :: NominalDiffTime, ctsBlockTimeIssued :: Maybe NominalDiffTime, ctsBlockHeight :: Maybe Int, ctsRelayedBy :: Maybe CNetworkAddress, ctsTotalInput :: Coin, ctsTotalOutput :: Coin, ctsFees :: Coin, ctsInputs :: Array (Tuple CAddress Coin), ctsOutputs :: Array (Tuple CAddress Coin)}
_CTxSummary = prism' CTxSummary f
  where
    f (CTxSummary r) = Just r


--------------------------------------------------------------------------------
