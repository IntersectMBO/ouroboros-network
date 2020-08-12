{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Shelley.Ledger.Inspect (
    ProtocolUpdate(..)
  , UpdateState(..)
  ) where

import           Control.Monad
import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void
import           Data.Word (Word64)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Util.Condense

import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import qualified Shelley.Spec.Ledger.Genesis as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.PParams as SL

import           Ouroboros.Consensus.Shelley.Ledger.Block
import           Ouroboros.Consensus.Shelley.Ledger.Ledger

data ProtocolUpdate c = ProtocolUpdate {
      protocolUpdateVersion :: SL.ProtVer
    , protocolUpdateState   :: UpdateState c
    }
  deriving (Show, Eq)

-- | Update mechanism
--
-- The update mechanism in Shelley is simpler than it is in Byron. There is no
-- distinction between votes and proposals: to \"vote\" for a proposal one
-- merely submits the exact same proposal. There is also no separate
-- endorsement step. The procedure is as follows:
--
-- 1. As in Byron, a proposal is a partial map from parameters to their
--    values.
-- 2. During each epoch, a genesis key can submit (via its delegates) zero,
--    one, or many proposals; each submission overrides the previous one.
-- 3. \"Voting\" (submitting of proposals) ends @3k/f@ (constant
--    @stabilityWindow@ parameter) slots before the end of the epoch.
-- 4. At the end of an epoch, if the majority of nodes (as determined by the
--    @Quorum@ specification constant, which must be greater than half the
--    nodes) have most recently submitted the same exact proposal, then it is
--    adopted.
-- 5. The next epoch is always started with a clean slate, proposals from the
--    previous epoch that didn't make it are discarded.
--
-- The protocol version itself is also considered to be merely another
-- parameter, and parameters can change /without/ changing the protocol
-- version, although a convention /could/ be established that the protocol
-- version must change if any of the parameters do; but the specification
-- itself does not mandate this.

data UpdateState c =
    -- | The update was proposed/voted on.
    --
    -- We record all update proposals that change the protocol version to the
    -- same new value. Multiple such proposals can occur when they want to
    -- change different parameters at the same time, possibly in different
    -- ways. For example, two update proposals may want to change the protocol
    -- version, but one of them may additionally want to change the max
    -- transaction size too. Or, the second proposal may want to change the
    -- transaction size to a different value than the first.
    --
    -- Consensus must be reached on a single update proposal, i.e., a single
    -- value of 'SL.PParamsUpdate' must reach quorum.
    --
    -- Additionally, we record for each update proposal which genesis
    -- delegates voted for it, as well as the epoch in which the update would
    -- take effect.
    UpdateVoted
      (Map SL.PParamsUpdate (NonEmpty (SL.KeyHash 'SL.Genesis c)))
      EpochNo

    -- | The update reached quorum at the end of the voting period, i.e., the
    -- end of epoch - @stabilityWindow@ (= @3k/f@).
    --
    -- We record the 'EpochNo' of the epoch in which it will become active,
    -- when it gets stably adopted 'UpdateStablyAdopted'.
  | UpdateAdopted EpochNo

    -- | The update was /stably/ adopted, i.e., it is @k@ blocks deep in the
    -- chain and cannot be rolled back.
    --
    -- We record the 'EpochNo' of the epoch in which it will become active.
  | UpdateStablyAdopted EpochNo
  deriving (Show, Eq)

protocolUpdates ::
       forall c.
       LedgerConfig (ShelleyBlock c)
    -> LedgerState (ShelleyBlock c)
    -> [ProtocolUpdate c]
protocolUpdates cfg st =
    [ -- TODO detect 'UpdateStablyAdopted'. Track BlockNo in ledger?
      ProtocolUpdate {
          protocolUpdateVersion = protVer
        , protocolUpdateState   =
            case quorumReached updates of
              Just _update -> UpdateAdopted       (succ currentEpoch)
              Nothing      -> UpdateVoted updates (succ currentEpoch)
        }
    | (protVer, updates) <- Map.toList protocolUpdateProposals
    ]
  where
    -- | Updates proposed before the end of this epoch _minus_ the stability
    -- window. When accepted, these can take effect at the start of the next
    -- epoch.
    --
    -- TODO what about 'futureProposals'?
    proposals :: SL.ProposedPPUpdates c
    proposals =
          SL.proposals
        . SL._ppups
        . SL._utxoState
        . SL.esLState
        . SL.nesEs
        . shelleyState
        $ st

    -- | Proposal that want to update the protocol version.
    --
    -- TODO what about other updates that don't change the 'SL.ProtVer'?
    protocolUpdateProposals ::
         Map SL.ProtVer
             (Map SL.PParamsUpdate (NonEmpty (SL.KeyHash 'SL.Genesis c)))
    protocolUpdateProposals = case proposals of
        SL.ProposedPPUpdates proposals' ->
          Map.foldrWithKey
            (\delegate proposal ->
                case SL._protocolVersion proposal of
                  SNothing      -> id
                  SJust protVer ->
                    Map.insertWith
                      (Map.unionWith (<>))
                      protVer
                      (Map.singleton proposal (pure delegate))
             )
            Map.empty
            proposals'

    currentEpoch :: EpochNo
    currentEpoch = SL.nesEL . shelleyState $ st

    quorum :: Word64
    quorum = SL.sgUpdateQuorum . shelleyLedgerGenesis $ cfg

    -- TODO adapt and try to reuse @votedValue@ from
    -- @Shelley.Spec.Ledger.STS.Epoch@.
    quorumReached ::
         Map SL.PParamsUpdate (NonEmpty (SL.KeyHash 'SL.Genesis c))
      -> Maybe SL.PParamsUpdate
    quorumReached updates =
        case updatesThatReachedQuorum of
          []       -> Nothing
          [update] -> Just update
          -- Impossible when 'quorum' is correctly defined. See @votedValue@.
          _        -> error "multiple updates reached quorum"
      where
        updatesThatReachedQuorum =
            [ update
            | (update, delegates) <- Map.toList updates
            , fromIntegral (length delegates) > quorum
            ]

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data ShelleyLedgerUpdate c =
    ShelleyUpdatedProtocolUpdates [ProtocolUpdate c]
  deriving (Show, Eq)

instance Condense (ShelleyLedgerUpdate c) where
  condense = show

instance InspectLedger (ShelleyBlock c) where
  type LedgerWarning (ShelleyBlock c) = Void
  type LedgerUpdate  (ShelleyBlock c) = ShelleyLedgerUpdate c

  inspectLedger tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate $ ShelleyUpdatedProtocolUpdates updatesAfter
    where
      updatesBefore, updatesAfter :: [ProtocolUpdate c]
      updatesBefore = protocolUpdates (configLedger tlc) before
      updatesAfter  = protocolUpdates (configLedger tlc) after
