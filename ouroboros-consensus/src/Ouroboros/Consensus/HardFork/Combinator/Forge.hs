{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Forge (
    undistribMaintainForgeState
  ) where

import           Crypto.Random (MonadRandom)
import           Data.Coerce
import           Data.Functor.Product
import           Data.SOP.Strict

import           Cardano.Slotting.Slot (SlotNo)

import           Ouroboros.Consensus.Block.Forge
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

instance (CanHardFork xs, All CanForge xs) => CanForge (HardForkBlock xs) where
  type ForgeState (HardForkBlock xs) = PerEraForgeState xs

  forgeBlock cfg forgeState blockNo
             Ticked { tickedSlotNo, tickedLedgerState }
             txs isLeader =
      -- First establish the 'IsLeader' and the 'LedgerState' are from the
      -- same era. As we have passed the ledger view of the ticked ledger to
      -- obtain the 'IsLeader' value, it __must__ be from the same era.
      case State.match
             (getOneEraIsLeader isLeader)
             (getHardForkLedgerState tickedLedgerState) of
        Left _mismatch ->
          error "IsLeader from different era than the TickedLedgerState"
        Right matched  ->
          -- Although we get a list with transactions that each could be from
          -- a different era, we know they have been validated against the
          -- 'LedgerState', which means they __must__ be from the same era.
          fmap (HardForkBlock . OneEraBlock) $
          hsequence $
          hcpure (Proxy @CanForge) (fn_4 matchedForgeBlock)
            `hap` (distribTopLevelConfig ei cfg)
            `hap` (getPerEraForgeState forgeState)
            `hap` (partition_NS (map (getOneEraGenTx . getHardForkGenTx) txs))
            `hap` (State.tip matched)
    where
      ei :: EpochInfo Identity
      ei = State.epochInfoLedger
             (configLedger cfg)
             (getHardForkLedgerState tickedLedgerState)

      -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
      matchedForgeBlock
        :: forall m blk. (MonadRandom m, CanForge blk)
        => TopLevelConfig blk
        -> WrapForgeState blk
        -> ([] :.: GenTx) blk
        -> Product WrapIsLeader LedgerState blk
        -> m blk
      matchedForgeBlock matchedCfg
                        matchedForgeState
                        (Comp matchedTxs)
                        (Pair matchedIsLeader matchedLedgerState) =
          forgeBlock
            matchedCfg
            (unwrapForgeState matchedForgeState)
            blockNo
            (Ticked tickedSlotNo matchedLedgerState)
            matchedTxs
            (unwrapIsLeader matchedIsLeader)

{-------------------------------------------------------------------------------
  Maintaining the 'ForgeState'
-------------------------------------------------------------------------------}

undistribMaintainForgeState
  :: forall xs m. (SListI xs, Monad m)
  => NP (MaintainForgeState m) xs
  -> MaintainForgeState m (HardForkBlock xs)
undistribMaintainForgeState np = MaintainForgeState {
      initForgeState   = initForgeStateHardFork
    , updateForgeState = updateForgeStateHardFork
    }
  where
    initForgeStateHardFork :: PerEraForgeState xs
    initForgeStateHardFork =
        PerEraForgeState $ hmap (WrapForgeState . initForgeState) np

    updateForgeStateHardFork
      :: Update m (ForgeState (HardForkBlock xs))
      -> SlotNo
      -> m ()
    updateForgeStateHardFork updateAll slotNo =
        htraverse_ updateOne $
          hzipWith Pair np (distribUpdateForgeState updateAll)
      where
        updateOne
          :: Product (MaintainForgeState m) (Update m :.: WrapForgeState) blk
          -> m ()
        updateOne (Pair mfs (Comp update)) =
            updateForgeState mfs (coerceUpdate update) slotNo

distribUpdateForgeState
  :: forall xs m. (SListI xs, Functor m)
  => Update m (ForgeState (HardForkBlock xs))
  -> NP (Update m :.: WrapForgeState) xs
distribUpdateForgeState updateAll = hliftA (Comp . mkSingleEraUpdate) lenses_NP
  where
    mkSingleEraUpdate
      :: Lens WrapForgeState xs blk
      -> Update m (WrapForgeState blk)
    mkSingleEraUpdate Lens { getter, setter } =
        liftUpdate
          (getter . coerce)
          (coerce . setter)
          updateAll
