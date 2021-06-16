{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Ouroboros.Consensus.HardFork.Combinator.Forging (
    HardForkCannotForge
  , HardForkForgeStateInfo (..)
  , HardForkForgeStateUpdateError
  , hardForkBlockForging
  ) where

import           Data.Functor.Product
import           Data.Maybe (fromMaybe)
import           Data.SOP.BasicFunctors
import           Data.SOP.Strict
import           Data.Text (Text)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.OptNP (OptNP (..), ViewOptNP (..))
import qualified Ouroboros.Consensus.Util.OptNP as OptNP
import           Ouroboros.Consensus.Util.SOP

import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.InjectTxs
import           Ouroboros.Consensus.HardFork.Combinator.Ledger (Ticked (..))
import           Ouroboros.Consensus.HardFork.Combinator.Mempool
import           Ouroboros.Consensus.HardFork.Combinator.Protocol
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Util.Functors
                     (Product2 (..))
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match

-- | If we cannot forge, it's because the current era could not forge
type HardForkCannotForge xs = OneEraCannotForge xs

type instance CannotForge (HardForkBlock xs) = HardForkCannotForge xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateInfo'.
--
-- When we update the hard fork forge state, we only update the forge state of
-- the current era. However, the current era /might not/ have a forge state as
-- it lacks a 'BlockForging'.
--
-- TODO #2766: expire past 'ForgeState'
data HardForkForgeStateInfo xs where
    -- | There is no 'BlockForging' record for the current era.
    CurrentEraLacksBlockForging ::
         EraIndex (x ': y ': xs)
      -> HardForkForgeStateInfo (x ': y ': xs)

    -- | The 'ForgeState' of the current era was updated.
    CurrentEraForgeStateUpdated ::
         OneEraForgeStateInfo xs
      -> HardForkForgeStateInfo xs

deriving instance CanHardFork xs => Show (HardForkForgeStateInfo xs)

type instance ForgeStateInfo (HardForkBlock xs) = HardForkForgeStateInfo xs

-- | For each era in which we want to forge blocks, we have a 'BlockForging',
-- and thus 'ForgeStateUpdateError'.
type HardForkForgeStateUpdateError xs = OneEraForgeStateUpdateError xs

type instance ForgeStateUpdateError (HardForkBlock xs) =
  HardForkForgeStateUpdateError xs

hardForkBlockForging ::
     forall m xs. (CanHardFork xs, Monad m)
  => Text
     -- ^ Used as the 'forgeLabel', the labels of the given 'BlockForging's will
     -- be ignored.
  -> OptNP 'False (BlockForging m) xs
  -> BlockForging m (HardForkBlock xs)
hardForkBlockForging forgeLabel blockForging =
    BlockForging {
        forgeLabel       = forgeLabel
      , canBeLeader      = hardForkCanBeLeader               blockForging
      , updateForgeState = hardForkUpdateForgeState          blockForging
      , checkCanForge    = hardForkCheckCanForge             blockForging
      , forgeBlock       = hardForkForgeBlock                blockForging
      }

hardForkCanBeLeader ::
     CanHardFork xs
  => OptNP 'False (BlockForging m) xs -> HardForkCanBeLeader xs
hardForkCanBeLeader =
      SomeErasCanBeLeader
    . hmap (WrapCanBeLeader . canBeLeader)

-- | POSTCONDITION: the returned 'ForgeStateUpdateInfo' is from the same era as
-- the ticked 'ChainDepState'.
hardForkUpdateForgeState ::
     forall m xs. (CanHardFork xs, Monad m)
  => OptNP 'False (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> SlotNo
  -> Ticked (HardForkChainDepState xs)
  -> m (ForgeStateUpdateInfo (HardForkBlock xs))
hardForkUpdateForgeState blockForging
                         cfg
                         curSlot
                         (TickedHardForkChainDepState chainDepState ei) =
    case OptNP.view blockForging of
      OptNP_ExactlyOne blockForging' ->
        injectSingle <$>
          updateForgeState
            blockForging'
            (hd (distribTopLevelConfig ei cfg))
            curSlot
            (unwrapTickedChainDepState . unComp . State.fromTZ $ chainDepState)
      OptNP_AtLeastTwo ->
          fmap undistrib
        $ hsequence'
        $ hzipWith3
            aux
            (OptNP.toNP blockForging)
            (distribTopLevelConfig ei cfg)
        $ State.tip chainDepState
  where
    injectSingle ::
         xs ~ '[blk]
      => ForgeStateUpdateInfo blk
      -> ForgeStateUpdateInfo (HardForkBlock '[blk])
    injectSingle forgeStateUpdateInfo =
        case forgeStateUpdateInfo of
          ForgeStateUpdated      info -> ForgeStateUpdated      $ injInfo        index info
          ForgeStateUpdateFailed err  -> ForgeStateUpdateFailed $ injUpdateError index err
          ForgeStateUpdateSuppressed  -> ForgeStateUpdateSuppressed
      where
        index :: Index '[blk] blk
        index = IZ

    aux ::
         (Maybe :.: BlockForging m)               blk
      -> TopLevelConfig                           blk
      -> (Ticked :.: WrapChainDepState)           blk
      -> (m :.: (Maybe :.: ForgeStateUpdateInfo)) blk
    aux (Comp mBlockForging) cfg' (Comp chainDepState') =
        Comp $ fmap Comp $ case mBlockForging of
          Nothing -> return Nothing
          Just blockForging' -> Just <$>
            updateForgeState
              blockForging'
              cfg'
              curSlot
              (unwrapTickedChainDepState chainDepState')

    injInfo ::
         Index xs blk
      -> ForgeStateInfo blk
      -> ForgeStateInfo (HardForkBlock xs)
    injInfo index =
          CurrentEraForgeStateUpdated
        . OneEraForgeStateInfo
        . injectNS index
        . WrapForgeStateInfo

    injUpdateError ::
         Index xs blk
      -> ForgeStateUpdateError blk
      -> ForgeStateUpdateError (HardForkBlock xs)
    injUpdateError index =
          OneEraForgeStateUpdateError
        . injectNS index
        . WrapForgeStateUpdateError

    undistrib ::
         xs ~ (x ': y ': zs)
      => NS (Maybe :.: ForgeStateUpdateInfo) xs
      -> ForgeStateUpdateInfo (HardForkBlock xs)
    undistrib = hcollapse . himap inj
      where
        inj :: forall blk.
               Index xs blk
            -> (Maybe :.: ForgeStateUpdateInfo) blk
            -> K (ForgeStateUpdateInfo (HardForkBlock xs)) blk
        inj index (Comp mForgeStateUpdateInfo) =
            K $ case mForgeStateUpdateInfo of
                Nothing -> ForgeStateUpdated $ CurrentEraLacksBlockForging $ eraIndexFromIndex index
                Just forgeStateUpdateInfo ->
                  case forgeStateUpdateInfo of
                    ForgeStateUpdated      info -> ForgeStateUpdated      $ injInfo        index info
                    ForgeStateUpdateFailed err  -> ForgeStateUpdateFailed $ injUpdateError index err
                    ForgeStateUpdateSuppressed  -> ForgeStateUpdateSuppressed

-- | PRECONDITION: the ticked 'ChainDepState', the 'HardForkIsLeader', and the
-- 'HardForkStateInfo' are all from the same era, and we must have a
-- 'BlockForging' for that era.
--
-- This follows from the postconditions of 'check' and
-- 'hardForkUpdateForgeState'.
hardForkCheckCanForge ::
     forall m xs empty. CanHardFork xs
  => OptNP empty (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> SlotNo
  -> Ticked (HardForkChainDepState xs)
  -> HardForkIsLeader xs
  -> HardForkForgeStateInfo xs
  -> Either (HardForkCannotForge xs) ()
hardForkCheckCanForge blockForging
                      cfg
                      curSlot
                      (TickedHardForkChainDepState chainDepState ei)
                      isLeader
                      forgeStateInfo =
    distrib $
      hizipWith3
        checkOne
        (distribTopLevelConfig ei cfg)
        (OptNP.toNP blockForging)
        -- We know all three NSs must be from the same era, because they were
        -- all produced from the same 'BlockForging'. Unfortunately, we can't
        -- enforce it statically.
        ( Match.mustMatchNS "ForgeStateInfo" forgeStateInfo'
        $ Match.mustMatchNS "IsLeader"       (getOneEraIsLeader isLeader)
        $ State.tip chainDepState
        )
  where
    distrib ::
         NS (Maybe :.: WrapCannotForge) xs
      -> Either (HardForkCannotForge xs) ()
    distrib = maybe (Right ()) (Left . OneEraCannotForge) . hsequence'

    missingBlockForgingImpossible :: EraIndex xs -> String
    missingBlockForgingImpossible eraIndex =
        "impossible: current era lacks block forging but we have an IsLeader proof "
        <> show eraIndex

    forgeStateInfo' :: NS WrapForgeStateInfo xs
    forgeStateInfo' = case forgeStateInfo of
      CurrentEraForgeStateUpdated info     -> getOneEraForgeStateInfo info
      CurrentEraLacksBlockForging eraIndex ->
        error $ missingBlockForgingImpossible eraIndex

    checkOne ::
         Index xs blk
      -> TopLevelConfig blk
      -> (Maybe :.: BlockForging m) blk
      -> Product
           WrapForgeStateInfo
           (Product
             WrapIsLeader
             (Ticked :.: WrapChainDepState))
           blk
      -> (Maybe :.: WrapCannotForge) blk
         -- ^ We use @Maybe x@ instead of @Either x ()@ because the former can
         -- be partially applied.
    checkOne index
             cfg'
             (Comp mBlockForging')
             (Pair
               (WrapForgeStateInfo forgeStateInfo'')
               (Pair
                 (WrapIsLeader isLeader')
                 (Comp tickedChainDepState))) =
        Comp $ either (Just . WrapCannotForge) (const Nothing) $
          checkCanForge
            (fromMaybe
              (error (missingBlockForgingImpossible (eraIndexFromIndex index)))
              mBlockForging')
            cfg'
            curSlot
            (unwrapTickedChainDepState tickedChainDepState)
            isLeader'
            forgeStateInfo''

-- | PRECONDITION: the ticked 'LedgerState' and 'HardForkIsLeader' are from the
-- same era, and we must have a 'BlockForging' for that era.
--
-- This follows from the postcondition of 'check' and the fact that the ticked
-- 'ChainDepState' and ticked 'LedgerState' are from the same era.
hardForkForgeBlock ::
     forall m xs empty. (CanHardFork xs, Monad m)
  => OptNP empty (BlockForging m) xs
  -> TopLevelConfig (HardForkBlock xs)
  -> BlockNo
  -> SlotNo
  -> TickedLedgerState (HardForkBlock xs)
  -> MaxTxCapacityOverride
  -> [Validated (GenTx (HardForkBlock xs))]
  -> HardForkIsLeader xs
  -> m (HardForkBlock xs)
hardForkForgeBlock blockForging
                   cfg
                   bno
                   sno
                   (TickedHardForkLedgerState transition ledgerState)
                   maxTxCapacityOverride
                   txs
                   isLeader =
        fmap (HardForkBlock . OneEraBlock)
      $ hsequence
      $ hizipWith3
          forgeBlockOne
          cfgs
          (OptNP.toNP blockForging)
      $ injectValidatedTxs (map (getOneEraValidatedGenTx . getHardForkValidatedGenTx) txs)
      -- We know both NSs must be from the same era, because they were all
      -- produced from the same 'BlockForging'. Unfortunately, we can't enforce
      -- it statically.
      $ Match.mustMatchNS
          "IsLeader"
          (getOneEraIsLeader isLeader)
          (State.tip ledgerState)
  where
    cfgs = distribTopLevelConfig ei cfg
    ei   = State.epochInfoPrecomputedTransitionInfo
             (hardForkLedgerConfigShape (configLedger cfg))
             transition
             ledgerState

    missingBlockForgingImpossible :: EraIndex xs -> String
    missingBlockForgingImpossible eraIndex =
        "impossible: current era lacks block forging but we have an IsLeader proof "
        <> show eraIndex

    injectValidatedTxs ::
         [NS WrapValidatedGenTx xs]
      -> NS f xs
      -> NS (Product f ([] :.: WrapValidatedGenTx)) xs
    injectValidatedTxs = noMismatches .: flip (matchValidatedTxsNS injTxs)
      where
        injTxs :: InPairs InjectValidatedTx xs
        injTxs =
              InPairs.hmap (\(Pair2 _ x) -> x)
            $ InPairs.requiringBoth
                (hmap (WrapLedgerConfig . configLedger) cfgs)
                hardForkInjectTxs

        -- | We know the transactions must be valid w.r.t. the given ledger
        -- state, the Mempool maintains that invariant. That means they are
        -- either from the same era, or can be injected into that era.
        noMismatches ::
             ([Match.Mismatch WrapValidatedGenTx f xs], NS (Product f ([] :.: WrapValidatedGenTx)) xs)
           -> NS (Product f ([] :.: WrapValidatedGenTx)) xs
        noMismatches ([], xs)   = xs
        noMismatches (_errs, _) = error "unexpected unmatchable transactions"

    -- | Unwraps all the layers needed for SOP and call 'forgeBlock'.
    forgeBlockOne ::
         Index xs blk
      -> TopLevelConfig blk
      -> (Maybe :.: BlockForging m) blk
      -> Product
           (Product
              WrapIsLeader
              (Ticked :.: LedgerState))
           ([] :.: WrapValidatedGenTx)
           blk
      -> m blk
    forgeBlockOne index
                  cfg'
                  (Comp mBlockForging')
                  (Pair
                    (Pair (WrapIsLeader isLeader') (Comp ledgerState'))
                    (Comp txs')) =
        forgeBlock
          (fromMaybe
              (error (missingBlockForgingImpossible (eraIndexFromIndex index)))
              mBlockForging')
          cfg'
          bno
          sno
          ledgerState'
          maxTxCapacityOverride
          (map unwrapValidatedGenTx txs')
          isLeader'
