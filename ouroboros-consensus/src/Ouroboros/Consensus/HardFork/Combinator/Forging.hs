{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Consensus.HardFork.Combinator.Forging (
    HardForkCannotForge
  , hardForkBlockForging
  , HardForkForgeStateInfo
  , HardForkForgeStateUpdateError
  ) where

import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.OptNP (OptNP (..))
import qualified Ouroboros.Consensus.Util.OptNP as OptNP
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

-- | We update the forge state of each era for which we have a 'BlockForging'.
-- Each of those eras can individually fail.
--
-- When one or more eras fail to update, we consider the update to be failed.
-- This means we won't forge a block. For example, when the Shelley forge
-- state failed to update, we won't even forge a block in the Byron era.
type HardForkForgeStateUpdateError xs = PerEraForgeStateUpdateError xs

type instance ForgeStateUpdateError (HardForkBlock xs) =
  HardForkForgeStateUpdateError xs

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
  -> m (ForgeStateUpdateInfo (HardForkBlock xs))
hardForkUpdateForgeState blockForgings curSlot =
    undistribForgeStateUpdateInfo <$> htraverse' updateOne blockForgings
  where
    updateOne :: BlockForging m blk -> m (ForgeStateUpdateInfo blk)
    updateOne blockForging = updateForgeState blockForging curSlot

    -- | We use the following rules, from top to bottom:
    --
    -- * When one or more eras failed to update, we return 'UpdateFailed' with
    --   the eras that didn't fail left out of the
    --   'HardForkForgeStateUpdateError' ('OptNP').
    --
    -- * When one or more eras updated, we return 'Updated' even when some
    --   eras didn't. The 'HardForkForgeStateInfo' ('OptNP') will contain all
    --   eras that were included in the 'BlockForging' 'OptNP', including the
    --   eras that returned 'Unchanged'.
    --
    -- * When all eras were unchanged, we return 'Unchanged'. The
    --   'HardForkForgeStateInfo' ('OptNP') will contain all eras that were
    --   included in the 'BlockForging' 'OptNP'.
    undistribForgeStateUpdateInfo ::
         OptNP 'False ForgeStateUpdateInfo xs
      -> ForgeStateUpdateInfo (HardForkBlock xs)
    undistribForgeStateUpdateInfo updateInfos = ForgeStateUpdateInfo $
        case go updateInfos of
          Updated      infos -> Updated      (PerEraForgeStateInfo        infos)
          Unchanged    infos -> Unchanged    (PerEraForgeStateInfo        infos)
          UpdateFailed errs  -> UpdateFailed (PerEraForgeStateUpdateError errs)
      where
        go :: SListI xs'
           => OptNP empty ForgeStateUpdateInfo xs'
           -> UpdateInfo (OptNP 'False WrapForgeStateInfo        xs')
                         (OptNP empty  WrapForgeStateInfo        xs')
                         (OptNP 'False WrapForgeStateUpdateError xs')
        go OptNil         = Unchanged OptNil
        go (OptCons x xs) = consUpdateInfo x (go xs)
        go (OptSkip   xs) = skipUpdateInfo (go xs)

hardForkCheckCanForge ::
     forall m xs. CanHardFork xs
  => OptNP 'False (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> SlotNo
  -> Ticked (HardForkChainDepState xs)
  -> HardForkIsLeader xs
  -> HardForkForgeStateInfo xs
  -> Either (HardForkCannotForge xs) ()
hardForkCheckCanForge blockForgings
                      cfg
                      curSlot
                      (TickedHardForkChainDepState chainDepState ei)
                      isLeader
                      forgeStateInfo =
    -- First establish the 'IsLeader' and the 'ChainDepState' are from the
    -- same era. As we have obtained 'IsLeader from 'checkIsLeader' by giving
    -- it the 'ChainDepState', it __must__ be from the same era.
    case State.match (getOneEraIsLeader isLeader) chainDepState of
      Left _mismatch ->
        error "IsLeader from different era than the TickedChainDepState"
      Right matched  ->
        distrib $
          hpure (fn_4 checkOne)
            `hap` OptNP.toNP blockForgings
            `hap` distribTopLevelConfig ei cfg
            `hap` OptNP.toNP (getPerEraForgeStateInfo forgeStateInfo)
            `hap` State.tip matched
  where
    distrib ::
         NS (Maybe :.: WrapCannotForge) xs
      -> Either (HardForkCannotForge xs) ()
    distrib = maybe (Right ()) (Left . OneEraCannotForge) . hsequence'

    checkOne ::
         (Maybe :.: BlockForging m) blk
      -> TopLevelConfig blk
      -> (Maybe :.: WrapForgeStateInfo) blk
      -> Product WrapIsLeader (Ticked :.: WrapChainDepState) blk
      -> (Maybe :.: WrapCannotForge) blk
         -- ^ We use @Maybe x@ instead of @Either x ()@ because the former can
         -- be partially applied.
    checkOne (Comp mBlockForging)
             cfg'
             (Comp mForgeStateInfo')
             (Pair (WrapIsLeader isLeader') (Comp tickedChainDepState)) =
        Comp $ either (Just . WrapCannotForge) (const Nothing) $
          checkCanForge
            blockForging
            cfg'
            curSlot
            (unwrapTickedChainDepState tickedChainDepState)
            isLeader'
            forgeStateInfo'
      where
        blockForging = case mBlockForging of
          Just bf -> bf
          -- We are given an 'IsLeader' proof of the era while we don't have a
          -- 'BlockForging' record for that era, impossible
          Nothing ->
            error "checkCanForge in an era in which we cannot lead"

        forgeStateInfo' = case mForgeStateInfo' of
          Just (WrapForgeStateInfo fsi) -> fsi
          -- We are given an 'IsLeader' proof of the era, so we must have a
          -- 'BlockForging' record for the same era. We are also given
          -- 'ForgeStateInfo's for all eras for which we have a 'BlockForging'
          -- record, so it must be that there is a 'ForgeStateInfo' for this
          -- era.
          Nothing ->
            error "checkCanForge in an era in which we cannot lead"

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
          `hap` OptNP.toNP blockForgings
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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

wrapForgeStateUpdateInfo ::
     ForgeStateUpdateInfo blk
  -> UpdateInfo
       (WrapForgeStateInfo        blk)
       (WrapForgeStateInfo        blk)
       (WrapForgeStateUpdateError blk)
wrapForgeStateUpdateInfo (ForgeStateUpdateInfo updateInfo) = case updateInfo of
    Updated      info -> Updated      (WrapForgeStateInfo        info)
    Unchanged    info -> Unchanged    (WrapForgeStateInfo        info)
    UpdateFailed err  -> UpdateFailed (WrapForgeStateUpdateError err)

consUpdateInfo ::
     SListI xs
  => ForgeStateUpdateInfo x
  -> UpdateInfo (OptNP 'False WrapForgeStateInfo              xs)
                (OptNP empty  WrapForgeStateInfo              xs)
                (OptNP 'False WrapForgeStateUpdateError       xs)
  -> UpdateInfo (OptNP 'False WrapForgeStateInfo        (x ': xs))
                (OptNP 'False WrapForgeStateInfo        (x ': xs))
                (OptNP 'False WrapForgeStateUpdateError (x ': xs))
consUpdateInfo updateInfo updateInfos =
    case (wrapForgeStateUpdateInfo updateInfo, updateInfos) of
      -- Updated & Unchanged
      (Updated   info, Updated   infos) -> Updated   $ OptCons info infos
      (Unchanged info, Updated   infos) -> Updated   $ OptCons info infos
      (Updated   info, Unchanged infos) -> Updated   $ OptCons info infos
      (Unchanged info, Unchanged infos) -> Unchanged $ OptCons info infos
      -- UpdateFailed
      (UpdateFailed err, UpdateFailed errs) -> UpdateFailed $ OptCons err errs
      (UpdateFailed err, _)                 -> UpdateFailed $ OptCons err OptNP.empty
      (_               , UpdateFailed errs) -> UpdateFailed $ OptSkip errs

skipUpdateInfo ::
     UpdateInfo
       (OptNP empty1 WrapForgeStateInfo              xs)
       (OptNP empty2 WrapForgeStateInfo              xs)
       (OptNP empty3 WrapForgeStateUpdateError       xs)
  -> UpdateInfo
       (OptNP empty1 WrapForgeStateInfo        (x ': xs))
       (OptNP empty2 WrapForgeStateInfo        (x ': xs))
       (OptNP empty3 WrapForgeStateUpdateError (x ': xs))
skipUpdateInfo updateInfo =
    case updateInfo of
      Updated      infos -> Updated      (OptSkip infos)
      Unchanged    infos -> Unchanged    (OptSkip infos)
      UpdateFailed errs  -> UpdateFailed (OptSkip errs)
