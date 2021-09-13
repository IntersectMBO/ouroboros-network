{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Inspect (
    ProtocolUpdate (..)
  , ShelleyLedgerUpdate (..)
  , UpdateProposal (..)
  , UpdateState (..)
  , protocolUpdates
  ) where

import           Control.Monad
import           Data.List (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Ord (comparing)
import           Data.Tuple (swap)
import           Data.Void
import           Data.Word (Word64)
import           GHC.Records

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Condense

import           Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import qualified Cardano.Ledger.Core as Core
import qualified Shelley.Spec.Ledger.API as SL

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto)
import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

data ProtocolUpdate era = ProtocolUpdate {
      protocolUpdateProposal :: UpdateProposal era
    , protocolUpdateState    :: UpdateState (EraCrypto era)
    }
deriving instance Eq (Core.PParamsDelta era) => Eq (ProtocolUpdate era)
deriving instance Show (Core.PParamsDelta era) => Show (ProtocolUpdate era)

-- | Update proposal
--
-- As in Byron, a proposal is a partial map from parameters to their values.
data UpdateProposal era = UpdateProposal {
      -- | The protocol parameters changed by this update proposal
      --
      -- An update is /identified/ by how it updates the protocol parameters.
      proposalParams  :: Core.PParamsDelta era

      -- | New version (if changed by this proposal)
      --
      -- The protocol version itself is also considered to be just another
      -- parameter, and parameters can change /without/ changing the protocol
      -- version, although a convention /could/ be established that the protocol
      -- version must change if any of the parameters do; but the specification
      -- itself does not mandate this.
      --
      -- We record the version separately for the convenience of the HFC.
    , proposalVersion :: Maybe SL.ProtVer

      -- | The 'EpochNo' the proposal becomes active in, if it is adopted
    , proposalEpoch   :: EpochNo
    }

deriving instance Eq (Core.PParamsDelta era) => Eq (UpdateProposal era)
deriving instance Show (Core.PParamsDelta era) => Show (UpdateProposal era)

-- | Proposal state
--
-- The update mechanism in Shelley is simpler than it is in Byron. There is no
-- distinction between votes and proposals: to \"vote\" for a proposal one
-- merely submits the exact same proposal. There is also no separate
-- endorsement step. The procedure is as follows:
--
-- 1. During each epoch, a genesis key can submit (via its delegates) zero,
--    one, or many proposals; each submission overrides the previous one.
-- 2. \"Voting\" (submitting of proposals) ends @2 * stabilityWindow@ slots
--    (i.e. @6k/f@) before the end of the epoch. In other words, proposals
--    for the upcoming epoch must be submitted within the first @4k/f@ slots
--    of this one.
-- 3. At the end of an epoch, if the majority of nodes (as determined by the
--    @Quorum@ specification constant, which must be greater than half the
--    nodes) have most recently submitted the same exact proposal, then it is
--    adopted.
-- 4. The next epoch is always started with a clean slate, proposals from the
--    previous epoch that didn't make it are discarded (except for "future
--    proposals" that are explicitly marked for future epochs).
data UpdateState c = UpdateState {
      -- | The genesis delegates that voted for this proposal
      proposalVotes         :: [SL.KeyHash 'SL.Genesis c]

      -- | Has this proposal reached sufficient votes to be adopted?
    , proposalReachedQuorum :: Bool
    }
  deriving (Show, Eq)

protocolUpdates ::
       forall era. ShelleyBasedEra era
    => SL.ShelleyGenesis era
    -> LedgerState (ShelleyBlock era)
    -> [ProtocolUpdate era]
protocolUpdates genesis st = [
      ProtocolUpdate {
          protocolUpdateProposal = UpdateProposal {
              proposalParams  = proposal
            , proposalEpoch   = succ currentEpoch
            , proposalVersion = strictMaybeToMaybe $
                                  getField @"_protocolVersion" proposal
            }
        , protocolUpdateState = UpdateState {
              proposalVotes         = votes
            , proposalReachedQuorum = length votes >= fromIntegral quorum
            }
        }
    | (proposal, votes) <- proposalsInv
    ]
  where
    proposalsInv :: [(Core.PParamsDelta era, [SL.KeyHash 'SL.Genesis (EraCrypto era)])]
    proposalsInv =
          groupSplit id
        . sortBy (comparing fst)
        $ map swap (Map.toList proposals)

    -- Updated proposed within the proposal window
    proposals :: Map (SL.KeyHash 'SL.Genesis (EraCrypto era)) (Core.PParamsDelta era)
    SL.ProposedPPUpdates proposals =
          SL.proposals
        . SL._ppups
        . SL._utxoState
        . SL.esLState
        . SL.nesEs
        . shelleyLedgerState
        $ st

    -- A proposal is accepted if the number of votes is equal to or greater
    -- than the quorum. The quorum itself must be strictly greater than half
    -- the number of genesis keys, but we do not rely on that property here.
    quorum :: Word64
    quorum = SL.sgUpdateQuorum genesis

    -- The proposals in 'SL.proposals' are for the upcoming epoch
    -- (we ignore 'futureProposals')
    currentEpoch :: EpochNo
    currentEpoch = SL.nesEL . shelleyLedgerState $ st

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data ShelleyLedgerUpdate era =
    ShelleyUpdatedProtocolUpdates [ProtocolUpdate era]

deriving instance Eq (Core.PParamsDelta era) => Eq (ShelleyLedgerUpdate era)
deriving instance Show (Core.PParamsDelta era) => Show (ShelleyLedgerUpdate era)

instance Show (Core.PParamsDelta era) => Condense (ShelleyLedgerUpdate era) where
  condense = show

instance ShelleyBasedEra era => InspectLedger (ShelleyBlock era) where
  type LedgerWarning (ShelleyBlock era) = Void
  type LedgerUpdate  (ShelleyBlock era) = ShelleyLedgerUpdate era

  inspectLedger tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate $ ShelleyUpdatedProtocolUpdates updatesAfter
    where
      genesis :: SL.ShelleyGenesis era
      genesis = shelleyLedgerGenesis (configLedger tlc)

      updatesBefore, updatesAfter :: [ProtocolUpdate era]
      updatesBefore = protocolUpdates genesis before
      updatesAfter  = protocolUpdates genesis after
