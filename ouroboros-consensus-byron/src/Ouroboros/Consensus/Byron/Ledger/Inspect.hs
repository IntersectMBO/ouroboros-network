{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Byron.Ledger.Inspect (
    ByronLedgerUpdate (..)
    -- * Layer around the Byron protocol update inteface
  , ProtocolUpdate (..)
  , UpdateState (..)
  , protocolUpdates
  ) where

import           Control.Monad
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.Genesis as CC.Genesis
import qualified Cardano.Chain.ProtocolConstants as CC
import qualified Cardano.Chain.Slotting as CC

import qualified Cardano.Chain.Update as U
import qualified Cardano.Chain.Update.Validation.Endorsement as U.E
import qualified Cardano.Chain.Update.Validation.Interface as U.I
import qualified Cardano.Chain.Update.Validation.Registration as U.R

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import qualified Ouroboros.Consensus.HardFork.History.Util as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Consensus.Byron.Ledger.Block
import           Ouroboros.Consensus.Byron.Ledger.Conversions
import           Ouroboros.Consensus.Byron.Ledger.Ledger

{-------------------------------------------------------------------------------
  Protocol update
-------------------------------------------------------------------------------}

-- | Wrapper around a Byron protocol update with information about its state
--
-- NOTE: We don't currently record the 'U.ProtocolParameters' here because  we
-- don't really need to track them, and adding them would add a lot of  output
-- to the 'Show' instance. We could easily add them however if that would be
-- useful.
data ProtocolUpdate = ProtocolUpdate {
      protocolUpdateVersion :: U.ProtocolVersion
    , protocolUpdateState   :: UpdateState
    }
  deriving (Show, Eq)

-- | The various states a protocol update goes through
--
-- Listed in chronological order.
data UpdateState =
    -- | The update was registered, but does not yet have any votes
    --
    -- We record the 'SlotNo' of the slot in which the update was registered.
    -- After registration, nodes must vote on it.
    UpdateRegistered SlotNo

    -- | The update is accumulating votes
    --
    -- We record which nodes have voted for the proposal. The proposal must
    -- accumulate a sufficient number of votes before it can be confirmed.
  | UpdateActive (Set CC.KeyHash)

    -- | The update has amassed a sufficient number of votes
    --
    -- We record the 'SlotNo' of the slot in which the required threshold of
    -- votes was met. At this point @2k@ slots need to pass before the update
    -- can be endorsed.
  | UpdateConfirmed SlotNo

    -- | The votes are stable. We can start to accumulate endorsements.
    --
    -- We record which nodes have endorsed the proposal. The proposal must
    -- accumulate a sufficient number of endorsements before it is nominated
    -- and becomes a candidate.
  | UpdateStablyConfirmed (Set CC.KeyHash)

    -- | The update has amassed a sufficient number of endorsements
    --
    -- We record the 'SlotNo' of the slot in which the required threshold of
    -- endorsement was met. At this point a further @2k@ slots need to pass
    -- before the update becomes a stable candidate and can be adopted.
    --
    -- We additionally record the 'EpochNo' in which the candidate will be
    -- adopted, /if/ it becomes stable.
  | UpdateCandidate SlotNo EpochNo

    -- | The endorsements are stable. The update will be accepted.
    --
    -- We record the 'EpochNo' of the epoch in which it will become active.
  | UpdateStableCandidate EpochNo
  deriving (Show, Eq)

-- | All proposal updates, from new to old
protocolUpdates ::
       LedgerConfig ByronBlock
    -> LedgerState ByronBlock
    -> [ProtocolUpdate]
protocolUpdates genesis st = concat [
      map fromCandidate candidates

      -- Don't record an update both as a proposal and a candidate
    , map fromRegistered . Map.toList $
        Map.filter (not . hasCandidate . U.R.pupProtocolVersion) registered
    ]
  where
    -- Configuration

    k                :: CC.BlockCount
    epochSize        :: CC.EpochSlots
    stableAfter      :: Word64
    takesEffectAfter :: Word64

    k                = CC.Genesis.gdK $ CC.Genesis.configGenesisData genesis
    epochSize        = CC.Genesis.configEpochSlots genesis
    stableAfter      = CC.unSlotCount $ CC.kSlotSecurityParam    k
    takesEffectAfter = CC.unSlotCount $ CC.kUpdateStabilityParam k

    -- The impossible cases are impossible because these slots refer to
    -- the slots of blocks on the chain.
    isStable :: SlotNo -> Bool
    isStable slotNo = depth >= stableAfter
      where
        depth :: Word64
        depth = case ledgerTipSlot st of
                  Origin       -> error "isStable: impossible"
                  NotOrigin s  -> if s < slotNo
                                    then error "isStable: impossible"
                                    else History.countSlots s slotNo

    -- Extract relevant bits from the update state

    updState     :: U.I.State
    registered   :: U.R.ProtocolUpdateProposals
    registeredAt :: Map U.UpId CC.SlotNumber
    confirmed    :: Map U.UpId CC.SlotNumber
    votes        :: Map U.UpId (Set CC.KeyHash)
    candidates   :: [U.E.CandidateProtocolUpdate]
    endorsements :: Map U.ProtocolVersion (Set CC.KeyHash)

    updState     = CC.cvsUpdateState $ byronLedgerState st
    registered   = U.I.registeredProtocolUpdateProposals updState
    registeredAt = U.I.proposalRegistrationSlot          updState
    confirmed    = U.I.confirmedProposals                updState
    votes        = U.I.proposalVotes                     updState
    candidates   = U.I.candidateProtocolUpdates          updState
    endorsements = Map.fromListWith Set.union
                 . map (\e -> ( U.E.endorsementProtocolVersion        e
                              , Set.singleton (U.E.endorsementKeyHash e)
                              ))
                 . Set.toList
                 $ U.I.registeredEndorsements updState

    -- From registered proposals

    fromRegistered :: (U.UpId, U.R.ProtocolUpdateProposal) -> ProtocolUpdate
    fromRegistered (upId, proposal) = ProtocolUpdate {
          protocolUpdateVersion = version
        , protocolUpdateState   =
            -- We do the checks in reverse chronological order
            if | not (Set.null updEndorsed) ->
                   UpdateStablyConfirmed updEndorsed

               | Just confirmedInSlot <- updConfirmed ->
                   if isStable confirmedInSlot
                     then UpdateStablyConfirmed Set.empty
                     else UpdateConfirmed confirmedInSlot

               | not (Set.null updVotes) ->
                   UpdateActive updVotes

               | otherwise ->
                   UpdateRegistered updSlot
        }
      where
        version :: U.ProtocolVersion
        version = U.R.pupProtocolVersion proposal

        updVotes     :: Set CC.KeyHash
        updConfirmed :: Maybe SlotNo
        updEndorsed  :: Set CC.KeyHash
        updSlot      :: SlotNo

        updVotes     = Map.findWithDefault Set.empty upId votes
        updConfirmed = fromByronSlotNo <$> Map.lookup upId confirmed
        updEndorsed  = Map.findWithDefault Set.empty version endorsements
        updSlot      = case Map.lookup upId registeredAt of
                         Nothing   -> error "updSlot: invalid Byron state"
                         Just slot -> fromByronSlotNo slot

    -- From candidate proposals

    fromCandidate :: U.E.CandidateProtocolUpdate -> ProtocolUpdate
    fromCandidate candidate = ProtocolUpdate {
          protocolUpdateVersion = version
        , protocolUpdateState   =
            if not (isStable slot)
              then UpdateCandidate slot  (cpuEpoch slot)
              else UpdateStableCandidate (cpuEpoch slot)
        }
      where
        slot    :: SlotNo
        version :: U.ProtocolVersion

        slot    = fromByronSlotNo $ U.E.cpuSlot candidate
        version = U.E.cpuProtocolVersion        candidate

    -- Is there a candidate for this version?
    hasCandidate :: U.ProtocolVersion -> Bool
    hasCandidate v = any ((== v) . U.E.cpuProtocolVersion) candidates

    -- Given the 'SlotNo' of a candidate, compute in which 'Epoch' it will
    -- become active.
    --
    -- This follows the same structure as the computation in the A/B test. Let
    -- @s@ be the slot the update proposal was endorsed (gathered enough
    -- endorsements). Note that the very first slot in which the transition
    -- /could/ occur is @s + 1@; adding the required stability, the first slot
    -- in which the transition could occur is @s + 4k + 1@. This means that the
    -- last slot which /must/ be in /this/ era is @s + 4k@. Hence the last
    -- /epoch/ that must be in this era is @epoch (s + 4k)@, and the first epoch
    -- of the /next/ era is @succ (epoch (s + 4k))@.
    cpuEpoch :: SlotNo -> EpochNo
    cpuEpoch = succ . slotToEpoch . History.addSlots takesEffectAfter

    -- Slot conversion
    --
    -- This is valid for slots in the Byron era only; just like the Byron
    -- ledger itself, it assumes the Byron era is the /first/ era.
    slotToEpoch :: SlotNo -> EpochNo
    slotToEpoch (SlotNo s) = EpochNo (s `div` CC.unEpochSlots epochSize)

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data ByronLedgerUpdate =
    ByronUpdatedProtocolUpdates [ProtocolUpdate]
  deriving (Show, Eq)

instance Condense ByronLedgerUpdate where
  condense = show

instance InspectLedger ByronBlock where
  type LedgerWarning ByronBlock = Void
  type LedgerUpdate  ByronBlock = ByronLedgerUpdate

  inspectLedger tlc before after = do
      guard $ updatesBefore /= updatesAfter
      return $ LedgerUpdate $ ByronUpdatedProtocolUpdates updatesAfter
    where
      updatesBefore, updatesAfter :: [ProtocolUpdate]
      updatesBefore = protocolUpdates (configLedger tlc) before
      updatesAfter  = protocolUpdates (configLedger tlc) after
