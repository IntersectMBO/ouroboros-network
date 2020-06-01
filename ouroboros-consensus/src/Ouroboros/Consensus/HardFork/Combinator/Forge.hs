{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.HardFork.Combinator.Forge (

  ) where

import           Crypto.Random (MonadRandom)
import           Data.Functor.Product
import           Data.SOP.Strict

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

  forgeBlock cfg updateForgeState blockNo
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
            `hap` (getPerEraForgeState updateForgeState)
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
