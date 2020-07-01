{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Forge (
    undistribMaintainForgeState
  ) where

import           Data.Functor.Product
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

instance (CanHardFork xs, All CanForge xs) => CanForge (HardForkBlock xs) where
  type ExtraForgeState (HardForkBlock xs) = PerEraExtraForgeState xs

  forgeBlock cfg forgeState bno
             Ticked { tickedSlotNo, tickedState }
             txs isLeader =
      -- First establish the 'IsLeader' and the 'LedgerState' are from the
      -- same era. As we have passed the ledger view of the ticked ledger to
      -- obtain the 'IsLeader' value, it __must__ be from the same era.
      case State.match
             (getOneEraIsLeader isLeader)
             (getHardForkLedgerState tickedState) of
        Left _mismatch ->
          error "IsLeader from different era than the TickedLedgerState"
        Right matched  ->
          -- Although we get a list with transactions that each could be from
          -- a different era, we know they have been validated against the
          -- 'LedgerState', which means they __must__ be from the same era.
          HardForkBlock . OneEraBlock $
          hcpure (Proxy @CanForge) (fn_4 matchedForgeBlock)
            `hap` (distribTopLevelConfig ei cfg)
            `hap` (distribForgeState forgeState)
            `hap` (partition_NS (map (getOneEraGenTx . getHardForkGenTx) txs))
            `hap` (State.tip matched)
    where
      ei :: EpochInfo Identity
      ei = State.epochInfoLedger
             (configLedger cfg)
             (getHardForkLedgerState tickedState)

      -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
      matchedForgeBlock
        :: CanForge blk
        => TopLevelConfig blk
        -> ForgeState blk
        -> ([] :.: GenTx) blk
        -> Product WrapIsLeader LedgerState blk
        -> I blk
      matchedForgeBlock matchedCfg
                        matchedForgeState
                        (Comp matchedTxs)
                        (Pair matchedIsLeader matchedLedgerState) = I $
          forgeBlock
            matchedCfg
            matchedForgeState
            bno
            (Ticked tickedSlotNo matchedLedgerState)
            matchedTxs
            (unwrapIsLeader matchedIsLeader)

{-------------------------------------------------------------------------------
  Distributive properties
-------------------------------------------------------------------------------}

distribForgeState ::
     forall xs. SListI xs
  => ForgeState (HardForkBlock xs)
  -> NP ForgeState xs
distribForgeState = \ForgeState{..} ->
    hzipWith
      aux
      (getPerEraChainIndepState chainIndepState)
      (getPerEraExtraForgeState extraForgeState)
  where
    aux :: WrapChainIndepState blk -> WrapExtraForgeState blk -> ForgeState blk
    aux chainIndepState extraForgeState = ForgeState {
          chainIndepState = unwrapChainIndepState chainIndepState
        , extraForgeState = unwrapExtraForgeState extraForgeState
        }

undistribForgeState ::
     forall xs. SListI xs
  => NP ForgeState xs
  -> ForgeState (HardForkBlock xs)
undistribForgeState np = ForgeState {
      chainIndepState = PerEraChainIndepState $
                          hmap (WrapChainIndepState . chainIndepState) np
    , extraForgeState = PerEraExtraForgeState $
                          hmap (WrapExtraForgeState . extraForgeState) np
    }

undistribMaintainForgeState
  :: forall xs m. (SListI xs, Monad m)
  => NP (MaintainForgeState m) xs
  -> MaintainForgeState m (HardForkBlock xs)
undistribMaintainForgeState np = MaintainForgeState {
      initForgeState   = initForgeStateHardFork
    , updateForgeState = updateForgeStateHardFork
    }
  where
    initForgeStateHardFork :: ForgeState (HardForkBlock xs)
    initForgeStateHardFork = undistribForgeState $ hmap initForgeState np

    updateForgeStateHardFork ::
         ChainIndepStateConfig (BlockProtocol (HardForkBlock xs))
      -> SlotNo
      -> ForgeState (HardForkBlock xs)
      -> m (ForgeState (HardForkBlock xs))
    updateForgeStateHardFork cfg slot =
          fmap undistribForgeState
        . hsequence'
        . hap updates
        . distribForgeState
      where
        cfgs = getPerEraChainIndepStateConfig cfg

        updates :: NP (ForgeState -.-> m :.: ForgeState) xs
        updates =
          hzipWith
            (\(WrapChainIndepStateConfig cfg') mfs ->
                fn (Comp . updateForgeState mfs cfg' slot))
            cfgs
            np
