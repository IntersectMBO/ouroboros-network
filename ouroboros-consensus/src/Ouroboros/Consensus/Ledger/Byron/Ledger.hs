{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances requires for consensus/ledger integration
module Ouroboros.Consensus.Ledger.Byron.Ledger (
    -- * Ledger integration
    LedgerConfig(..)
  , LedgerState(..)
    -- * Serialisation
  , encodeByronLedgerState
  , decodeByronLedgerState
    -- * Auxiliary
  , validationErrorImpossible
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq.Lazy
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import qualified Cardano.Chain.Block as CC
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Scheduling as D.Sched
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.ValidationMode as CC

import           Ouroboros.Network.Block (Point (..), SlotNo (..), blockSlot)
import           Ouroboros.Network.Point (WithOrigin (..))
import qualified Ouroboros.Network.Point as Point

import           Ouroboros.Consensus.Ledger.Abstract
import qualified Ouroboros.Consensus.Ledger.Byron.Auxiliary as Aux
import           Ouroboros.Consensus.Ledger.Byron.Block
import           Ouroboros.Consensus.Ledger.Byron.Config
import           Ouroboros.Consensus.Ledger.Byron.ContainsGenesis
import           Ouroboros.Consensus.Ledger.Byron.Conversions
import           Ouroboros.Consensus.Ledger.Byron.DelegationHistory
                     (DelegationHistory)
import qualified Ouroboros.Consensus.Ledger.Byron.DelegationHistory as History
import           Ouroboros.Consensus.Ledger.Byron.PBFT
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.PBFT

instance UpdateLedger ByronBlock where

  data LedgerState ByronBlock = ByronLedgerState {
        byronLedgerState       :: !CC.ChainValidationState
      , byronDelegationHistory :: !DelegationHistory
      }
    deriving (Eq, Show, Generic, NoUnexpectedThunks)

  type LedgerError ByronBlock = CC.ChainValidationError

  newtype LedgerConfig ByronBlock = ByronLedgerConfig {
      unByronLedgerConfig :: Gen.Config
    }

  ledgerConfigView PBftNodeConfig{..} = ByronLedgerConfig $
      pbftGenesisConfig pbftExtConfig

  applyChainTick cfg slotNo ByronLedgerState{..} =
      TickedLedgerState ByronLedgerState {
          byronDelegationHistory = byronDelegationHistory
        , byronLedgerState       = Aux.applyChainTick
                                      (unByronLedgerConfig cfg)
                                      (toByronSlotNo slotNo)
                                      byronLedgerState
        }

  applyLedgerBlock = applyByronBlock validationMode
    where
      validationMode = CC.fromBlockValidationMode CC.BlockValidation

  reapplyLedgerBlock cfg blk st =
      validationErrorImpossible $
        applyByronBlock validationMode cfg blk st
    where
      validationMode = CC.fromBlockValidationMode CC.NoBlockValidation

  ledgerTipPoint (ByronLedgerState state _) =
      case CC.cvsPreviousHash state of
        -- In this case there are no blocks in the ledger state. The genesis
        -- block does not occupy a slot, so its point is Origin.
        Left _genHash -> Point Point.origin
        Right hdrHash -> Point (Point.block slot (ByronHash hdrHash))
          where
            slot = fromByronSlotNo (CC.cvsLastSlot state)

instance ConfigContainsGenesis (LedgerConfig ByronBlock) where
  getGenesisConfig = unByronLedgerConfig

instance ProtocolLedgerView ByronBlock where
  protocolLedgerView _cfg =
        toPBftLedgerView
      . Aux.getDelegationMap
      . byronLedgerState

  -- Delegation state for a particular point in time
  --
  -- The situation looks something like this:
  --
  -- > (Label for reference)        0             1      2             3      4
  -- > -------------------------------------------------------------------------
  -- > Delegation changes           v             v      v             v      v
  -- > Snapshots            [......)[............)[.....)      NOW
  -- > Requested slot                      A                B       C      D
  --
  -- where NOW refers to the slot number of the last block we applied, and
  -- the requested slot must be within a @[-2k .. +2k)@ window around NOW.
  --
  -- Note that there can be no delegation changes between (2) and (NOW): if
  -- there were, we'd have another snapshot. Four possibilities:
  --
  -- A. We have a historical snapshot of the delegation state that contains the
  --    requested slot. If so, we just return that.
  -- B. The slot is in the past, but we have no snapshot. In this case, it must
  --    be that the current ledger state is valid, between points (2) and (3).
  -- C. The slot is in the future, but before the first scheduled change to the
  --    delegation state. Again, current ledger state is valid, also between
  --    points (2) and (3).
  -- D. The slot is in the future, but after the first scheduled update. We must
  --    apply that scheduled update; the resulting delegation state will be
  --    valid from the point of that first schedulded update (3) until the
  --    next (4).
  --
  -- We can collapse cases (B, C, D) into a single one as follows: split the
  -- scheduled delegations into the ones that should have been applied at the
  -- requested slot, and those that are still in the future. For the example
  -- above this amounts to
  --
  -- B. ([], (3, 4)]
  -- C. ([], (3, 4)]
  -- D. ([3], [4])
  --
  -- Then take the delegation state from in current ledger state, and apply the
  -- updates that should be applied. The resulting delegation state must be
  -- given the following validity bounds:
  --
  -- * The lower bound will be the slot number of the last update that was
  --   applied, or the upper bound of the last historical snapshot if no
  --   updates needed to be applied. If there are no historical snapshots,
  --   then the lower bound is genesis (the history is only empty if the
  --   delegation state never changed).
  -- * The upper bound will be the slot number of the first update that was
  --   not yet applied; if no such update is known, it will be set to the
  --   the maximum upper bound @(NOW + 2k)@.
  --
  -- TODO: verify that the sdSlot of ScheduledDelegation is the slot at which
  -- it becomes active (i.e., that delegation should be applied /in/ that slot)
  -- i.e., that delegate is allowed to issue a block in that very same slot.
  anachronisticProtocolLedgerView cfg (ByronLedgerState ls ss) slot =
      case History.find slot ss of
        Just sb -> Right (toPBftLedgerView sb) -- Case (A)
        Nothing -- Case (B), (C) or (D)
          | slot <  At maxLo -> Left TooFarBehind -- lower bound is inclusive
          | slot >= At maxHi -> Left TooFarAhead  -- upper bound is exclusive
          | otherwise        -> Right $ toPBftLedgerView $

              let toApply :: Seq D.Sched.ScheduledDelegation
                  _future :: Seq D.Sched.ScheduledDelegation
                  (toApply, _future) = splitScheduledDelegations slot $
                                         Aux.getScheduledDelegations ls

              in Aux.applyScheduledDelegations toApply dsNow
    where
      SecurityParam k = pbftSecurityParam . pbftParams $ cfg

      dsNow :: Delegation.Map
      dsNow = Aux.getDelegationMap ls

      now, maxHi, maxLo :: SlotNo
      now   = fromByronSlotNo $ CC.cvsLastSlot ls
      maxLo = SlotNo $ if (2 * k) > unSlotNo now
                        then 0
                        else unSlotNo now - (2 * k)
      maxHi = SlotNo $ unSlotNo now + (2 * k)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Mark computation as validation error free
--
-- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
-- 'applyByronBlock' shouldn't fail since the ledger layer won't be performing
-- any block validation checks. However, because 'applyByronBlock' can fail in
-- the event it is given a 'BlockValidationMode' of 'BlockValidation', it still
-- /looks/ like it can fail (since its type doesn't change based on the
-- 'ValidationMode') and we must still treat it as such.
validationErrorImpossible :: forall err a. Except err a -> a
validationErrorImpossible = cantBeError . runExcept
  where
    cantBeError :: Either err a -> a
    cantBeError (Left  _) = error "validationErrorImpossible: unexpected error"
    cantBeError (Right a) = a

-- | Split scheduled delegations into past and future
splitScheduledDelegations :: WithOrigin SlotNo
                          -> Seq D.Sched.ScheduledDelegation
                          -> ( Seq D.Sched.ScheduledDelegation
                             , Seq D.Sched.ScheduledDelegation
                             )
splitScheduledDelegations slot =
    -- spanl finds the longest prefix of elements that satisfy the predicate
    Seq.Lazy.spanl shouldApply
  where
    shouldApply :: D.Sched.ScheduledDelegation -> Bool
    shouldApply sd = At (fromByronSlotNo (D.Sched.sdSlot sd)) <= slot

{-------------------------------------------------------------------------------
  Applying a block

  Most of the work here is done by the ledger layer. We just need to pass
  the right arguments, and maintain the snapshots.
-------------------------------------------------------------------------------}

applyByronBlock :: CC.ValidationMode
                -> LedgerConfig ByronBlock
                -> ByronBlock
                -> LedgerState ByronBlock
                -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyByronBlock validationMode
                fcfg@(ByronLedgerConfig cfg)
                fblk@(ByronBlock blk _ (ByronHash blkHash))
                ls = do
    let TickedLedgerState ls' = applyChainTick fcfg (blockSlot fblk) ls
    case blk of
      CC.ABOBBlock    blk' -> applyABlock validationMode cfg blk' blkHash ls'
      CC.ABOBBoundary blk' -> applyABoundaryBlock        cfg blk'         ls'

applyABlock :: CC.ValidationMode
            -> Gen.Config
            -> CC.ABlock ByteString
            -> CC.HeaderHash
            -> LedgerState (ByronBlock)
            -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABlock validationMode cfg blk blkHash ByronLedgerState{..} = do
    state' <- Aux.validateBlock cfg validationMode blk blkHash byronLedgerState
    -- If the delegation state changed, take a snapshot of the old state
    let history'
          |    CC.cvsDelegationState state'
            == CC.cvsDelegationState byronLedgerState
                      = byronDelegationHistory
          | otherwise = History.snapOld
                          (Gen.configK cfg)
                          (fromByronSlotNo $ CC.blockSlot blk)
                          (Aux.getDelegationMap byronLedgerState) -- the old state!
                          byronDelegationHistory
    return $ ByronLedgerState state' history'

-- | Apply boundary block
--
-- Since boundary blocks don't modify the delegation state, they also don't
-- modify the delegation history.
applyABoundaryBlock :: Gen.Config
                    -> CC.ABoundaryBlock ByteString
                    -> LedgerState ByronBlock
                    -> Except (LedgerError ByronBlock) (LedgerState ByronBlock)
applyABoundaryBlock cfg blk ByronLedgerState{..} = do
    current' <- Aux.validateBoundary cfg blk byronLedgerState
    return $ ByronLedgerState current' byronDelegationHistory

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeByronLedgerState :: LedgerState ByronBlock -> Encoding
encodeByronLedgerState ByronLedgerState{..} = mconcat
    [ CBOR.encodeListLen 2
    , encode byronLedgerState
    , History.encodeDelegationHistory byronDelegationHistory
    ]

decodeByronLedgerState :: Decoder s (LedgerState ByronBlock)
decodeByronLedgerState = do
    CBOR.decodeListLenOf 2
    ByronLedgerState
      <$> decode
      <*> History.decodeDelegationHistory
