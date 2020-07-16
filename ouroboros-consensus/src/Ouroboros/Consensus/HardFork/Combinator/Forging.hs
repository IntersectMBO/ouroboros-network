{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Consensus.HardFork.Combinator.Forging (
    HardForkCannotForge
  , hardForkBlockForging
  , HardForkForgeStateInfo
  ) where

import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (..))
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

-- | If we cannot forge, it's because the current era could not forge
type HardForkCannotForge xs = OneEraCannotForge xs

type instance CannotForge (HardForkBlock xs) = HardForkCannotForge xs

-- | When we can lead in an era, we also have a 'ForgeStateInfo' for that era
-- (through 'BlockForging'). This means that when the hard fork combinator can
-- be a leader, it must at least have one 'ForgeStateInfo'. Hence
-- 'PerEraForgeStateInfo' is defined to be @OptNP False@.
type HardForkForgeStateInfo xs = PerEraForgeStateInfo xs

type instance ForgeStateInfo (HardForkBlock xs) = HardForkForgeStateInfo xs

hardForkBlockForging ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> BlockForging m (HardForkBlock xs)
hardForkBlockForging blockForgings =
    BlockForging {
        canBeLeader      = hmap (WrapCanBeLeader . canBeLeader) blockForgings
      , updateForgeState = hardForkUpdateForgeState             blockForgings
      , checkCanForge    = hardForkCheckCanForge                blockForgings
      , forgeBlock       = hardForkForgeBlock                   blockForgings
      }

hardForkUpdateForgeState ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> SlotNo
  -> m (HardForkForgeStateInfo xs)
hardForkUpdateForgeState blockForgings curSlot =
    PerEraForgeStateInfo <$> htraverse' updateOne blockForgings
  where
    updateOne :: BlockForging m a -> m (WrapForgeStateInfo a)
    updateOne blockForging =
        WrapForgeStateInfo <$> updateForgeState blockForging curSlot

hardForkCheckCanForge ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> SlotNo
  -> Ticked (HardForkChainDepState xs)
  -> HardForkIsLeader xs
  -> m (Maybe (HardForkCannotForge xs))
hardForkCheckCanForge blockForgings
                      cfg
                      curSlot
                      (TickedHardForkChainDepState chainDepState ei)
                      isLeader =
    -- First establish the 'IsLeader' and the 'ChainDepState' are from the
    -- same era. As we have obtained 'IsLeader from 'checkIsLeader' by giving
    -- it the 'ChainDepState', it __must__ be from the same era.
    case State.match (getOneEraIsLeader isLeader) chainDepState of
      Left _mismatch ->
        error "IsLeader from different era than the TickedChainDepState"
      Right matched  ->
        distrib $
          hpure (fn_3 checkOne)
            `hap` fromOptNP blockForgings
            `hap` distribTopLevelConfig ei cfg
            `hap` State.tip matched
  where
    distrib ::
         NS (m :.: (Maybe :.: WrapCannotForge)) xs
      -> m (Maybe (HardForkCannotForge xs))
    distrib = fmap (fmap OneEraCannotForge . hsequence') . hsequence'

    checkOne ::
         (Maybe :.: BlockForging m) blk
      -> TopLevelConfig blk
      -> Product WrapIsLeader (Ticked :.: WrapChainDepState) blk
      -> (m :.: Maybe :.: WrapCannotForge) blk
    checkOne (Comp mBlockForging)
             cfg'
             (Pair (WrapIsLeader isLeader') (Comp tickedChainDepState)) =
        Comp $ Comp . fmap WrapCannotForge <$>
          checkCanForge
            blockForging
            cfg'
            curSlot
            (unwrapTickedChainDepState tickedChainDepState)
            isLeader'
      where
        blockForging = case mBlockForging of
          Just bf -> bf
          -- We are given an 'IsLeader' proof of the era while we don't have a
          -- 'BlockForging' record for that era, impossible
          Nothing ->
            error "checkCanForge an era in which we cannot lead"

hardForkForgeBlock ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> BlockNo
  -> SlotNo
  -> TickedLedgerState (HardForkBlock xs)
  -> [GenTx (HardForkBlock xs)]
  -> HardForkIsLeader xs
  -> m (HardForkBlock xs)
hardForkForgeBlock blockForgings
                   cfg
                   bno
                   sno
                   (TickedHardForkLedgerState transition ledgerState)
                   txs
                   isLeader = do
    -- First establish the 'IsLeader' and the 'LedgerState' are from the
    -- same era. As we have passed the ledger view of the ticked ledger to
    -- obtain the 'IsLeader' value, it __must__ be from the same era.
    -- Unfortunately, we cannot avoid this 'error' call: the 'IsLeader'
    -- evidence could conceivably include the ledger /view/, but not the
    -- ledger /state/.
    case State.match (getOneEraIsLeader isLeader) ledgerState of
      Left _mismatch ->
        error "IsLeader from different era than the TickedLedgerState"
      Right matched  ->
        -- Although we get a list with transactions that each could be from
        -- a different era, we know they have been validated against the
        -- 'LedgerState', which means they __must__ be from the same era.
        fmap (HardForkBlock . OneEraBlock) $
        hsequence $
        hpure (fn_4 forgeBlockOne)
          `hap` fromOptNP blockForgings
          `hap` distribTopLevelConfig ei cfg
          `hap` (partition_NS (map (getOneEraGenTx . getHardForkGenTx) txs))
          `hap` (State.tip matched)
  where
    ei = State.epochInfoPrecomputedTransitionInfo
           (hardForkLedgerConfigShape (configLedger cfg))
           transition
           ledgerState

    -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
    forgeBlockOne ::
         (Maybe :.: BlockForging m) blk
      -> TopLevelConfig blk
      -> ([] :.: GenTx) blk
      -> Product WrapIsLeader (Ticked :.: LedgerState) blk
      -> m blk
    forgeBlockOne (Comp mBlockForging)
                  matchedCfg
                  (Comp matchedTxs)
                  (Pair matchedIsLeader (Comp matchedLedgerState)) =
        forgeBlock
          blockForging
          matchedCfg
          bno
          sno
          matchedLedgerState
          matchedTxs
          (unwrapIsLeader matchedIsLeader)
      where
        blockForging = case mBlockForging of
          Just bf -> bf
          Nothing ->
            error "forging a block in an era in which we cannot lead"
