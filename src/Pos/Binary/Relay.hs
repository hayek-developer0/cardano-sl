-- | Pos.Communication.Relay serialization instances

module Pos.Binary.Relay () where

import           Universum

import           Pos.Binary.Class                 (Bi (..), label, putField)
import           Pos.Binary.Crypto                ()
import           Pos.Binary.Ssc                   ()
import           Pos.Binary.Update                ()
import           Pos.Communication.Types.Relay    (DataMsg (..))
import           Pos.Crypto                       (hash)
import           Pos.Delegation.Types             (ProxySKLightConfirmation)
import           Pos.Types                        (ProxySKHeavy, ProxySKLight)
import           Pos.Update.Core                  (UpdateProposal, UpdateVote (..))

instance Bi (DataMsg (UpdateProposal, [UpdateVote])) where
    sizeNPut = putField dmContents
    get = label "DataMsg (UpdateProposal, [UpdateVote])" $ do
        c@(up, votes) <- get
        let !id = hash up
        unless (all ((id ==) . uvProposalId) votes) $
            fail "get@DataMsg@Update: vote's uvProposalId must be equal UpId"
        pure $ DataMsg c

instance Bi (DataMsg UpdateVote) where
    sizeNPut = putField dmContents
    get = label "DataMsg UpdateVote" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLight) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKLight" $ DataMsg <$> get

instance Bi (DataMsg ProxySKHeavy) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKHeavy" $ DataMsg <$> get

instance Bi (DataMsg ProxySKLightConfirmation) where
    sizeNPut = putField dmContents
    get = label "DataMsg ProxySKLightConfirmation" $ DataMsg <$> get
