{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.HardFork.Combinator.State.Infra (
    -- * Initialization
    initHardForkState
    -- * GC
  , tickAllPast
    -- * Lifting 'Telescope' operations
  , tip
  , match
  , sequence
  , bihczipWith
  , fromTZ
    -- * Aligning
  , align
    -- * Rewinding
  , retractToSlot
    -- * EpochInfo/Summary
  , transitionOrTip
  , reconstructSummary
  ) where

import           Prelude hiding (sequence)

import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State.Lift
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Tails as Tails
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), Retract (..), Telescope (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Telescope as Telescope

{-------------------------------------------------------------------------------
  Initialization
-------------------------------------------------------------------------------}

initHardForkState :: f x -> HardForkState f (x ': xs)
initHardForkState st = HardForkState $ TZ $ Current {
      currentStart = History.initBound
    , currentState = st
    }

{-------------------------------------------------------------------------------
  GC
-------------------------------------------------------------------------------}

tickAllPast :: SListI xs
            => SecurityParam -> HardForkState_ g f xs -> HardForkState_ g f xs
tickAllPast k (HardForkState st) = HardForkState $
    Telescope.bihmap (tickPast k) id st

-- | Tick all past states
--
-- This is used to GC past snapshots when they exceed @k@.
tickPast :: SecurityParam -> Past g blk -> Past g blk
tickPast k past = past { pastSnapshot = tickSnapshot k (pastSnapshot past) }

tickSnapshot :: SecurityParam -> Snapshot f blk -> Snapshot f blk
tickSnapshot (SecurityParam k) = \case
    Snapshot n st | n < k -> Snapshot (n + 1) st
    _                     -> NoSnapshot

{-------------------------------------------------------------------------------
  Lift telescope operations
-------------------------------------------------------------------------------}

tip :: SListI xs => HardForkState_ g f xs -> NS f xs
tip (HardForkState st) = hmap currentState $ Telescope.tip st

match :: SListI xs
      => NS h xs
      -> HardForkState_ g f xs
      -> Either (Mismatch h (Current f) xs) (HardForkState_ g (Product h f) xs)
match ns (HardForkState t) =
    HardForkState . hmap distrib <$> Match.matchTelescope ns t
  where
    distrib :: Product h (Current f) blk -> Current (Product h f) blk
    distrib (Pair x (Current start y)) =
        Current start (Pair x y)

sequence :: forall g f m xs. (SListI xs, Functor m)
         => HardForkState_ g (m :.: f) xs -> m (HardForkState_ g f xs)
sequence = \(HardForkState st) -> HardForkState <$>
    Telescope.sequence (hmap distrib st)
  where
    distrib :: Current (m :.: f) blk -> (m :.: Current f) blk
    distrib (Current start st) = Comp $
        Current start <$> unComp st

bihczipWith :: forall xs h g' g f' f. All SingleEraBlock xs
            => (forall blk. SingleEraBlock blk => h blk -> g blk -> g' blk)
            -> (forall blk. SingleEraBlock blk => h blk -> f blk -> f' blk)
            -> NP h xs -> HardForkState_ g f xs -> HardForkState_ g' f' xs
bihczipWith g f ns (HardForkState st) = HardForkState $
    Telescope.bihczipWith proxySingle (liftPast . g) (lift . f) ns st

fromTZ :: HardForkState_ g f '[blk] -> f blk
fromTZ = currentState . Telescope.fromTZ . getHardForkState

{-------------------------------------------------------------------------------
  Aligning
-------------------------------------------------------------------------------}

align :: forall xs h f f' f''. All SingleEraBlock xs
      => InPairs (Translate f) xs
      -> NP (f' -.-> f -.-> f'') xs
      -> HardForkState_ h f'  xs -- ^ State we are aligning with
      -> HardForkState_ f f   xs -- ^ State we are aligning
      -> HardForkState_ f f'' xs
align fs updTip (HardForkState alignWith) (HardForkState toAlign) =
    HardForkState . unI $
      Telescope.alignExtend
        (InPairs.hmap (\f    -> Require $
                       \past -> Extend  $
                       \cur  -> I       $
                         newCurrent f past cur) fs)
        (hmap (fn_2 . liftUpdTip) updTip)
        alignWith
        toAlign
  where
    liftUpdTip :: (f' -.-> f -.-> f'') blk
               -> Current f' blk -> Current f blk -> Current f'' blk
    liftUpdTip f = lift . apFn . apFn f . currentState

    newCurrent :: Translate f blk blk'
               -> Past g' blk
               -> Current f blk
               -> (Past f blk, Current f blk')
    newCurrent f pastG curF = (
          Past    { pastStart    = currentStart curF
                  , pastEnd      = curEnd
                  , pastSnapshot = Snapshot 0 (currentState curF)
                  }
        , Current { currentStart = curEnd
                  , currentState = translateWith f
                                     (boundEpoch curEnd)
                                     (currentState curF)
                  }
        )
      where
        curEnd :: Bound
        curEnd = pastEnd pastG

{-------------------------------------------------------------------------------
  Rewinding
-------------------------------------------------------------------------------}

-- | Rewind until the specified slot is within the era at the tip
retractToSlot :: forall f xs. SListI xs
              => WithOrigin SlotNo
              -> HardForkState f xs -> Maybe (HardForkState f xs)
retractToSlot slot (HardForkState st) =
    HardForkState <$>
      Telescope.retractIf
        (Tails.hpure retract)
        (hmap (fn . containsSlot) (markFirst True sList))
        st
  where
    markFirst :: Bool -> SList xs' -> NP (K Bool) xs'
    markFirst _ SNil  = Nil
    markFirst b SCons = K b :* markFirst False sList

    containsSlot :: K Bool blk -> Past f blk -> K Bool blk
    containsSlot (K isFirst) Past{..} = K $
        case slot of
          Origin -> isFirst -- Assume 'Origin' in the first era
          At s   -> boundSlot pastStart <= s && s < boundSlot pastEnd

    retract :: Retract Maybe (Past f) (Current f) blk blk'
    retract = Retract $ \past _oldCur ->
        Current (pastStart past) <$> getSnapshot (pastSnapshot past)

getSnapshot :: Snapshot f blk -> Maybe (f blk)
getSnapshot (Snapshot _ st) = Just st
getSnapshot NoSnapshot      = Nothing

{-------------------------------------------------------------------------------
  Summary/EpochInfo
-------------------------------------------------------------------------------}

transitionOrTip :: SingleEraBlock blk
                => WrapPartialLedgerConfig blk
                -> LedgerState blk
                -> TransitionOrTip
transitionOrTip cfg st =
    case singleEraTransition' cfg st of
      Just epoch -> TransitionAt (ledgerTipSlot st) epoch
      Nothing    -> LedgerTip (ledgerTipSlot st)

reconstructSummary :: forall g f xs. All SingleEraBlock xs
                   => History.Shape xs
                   -> NP (f -.-> K TransitionOrTip) xs
                   -- ^ Return the 'EpochNo' of the transition to the next
                   -- era if known, or the 'SlotNo' at the tip otherwise.
                   -> HardForkState_ g f xs
                   -> History.Summary xs
reconstructSummary (History.Shape shape) transition (HardForkState st) =
    History.Summary $ go shape transition st
  where
    go :: All SingleEraBlock xs'
       => Exactly xs' EraParams
       -> NP (f -.-> K TransitionOrTip) xs'
       -> Telescope (Past g) (Current f) xs'
       -> NonEmpty xs' EraSummary
    go (K params :* ss) (_ :* ts) (TS Past{..} t) =
        NonEmptyCons (EraSummary pastStart (EraEnd pastEnd) params) $ go ss ts t
    go (K params :* Nil) _ (TZ Current{..}) =
        -- The current era is the last. We assume it lasts until all eternity.
        NonEmptyOne (EraSummary currentStart EraUnbounded params)
    go (K params :* K nextParams :* _) (t :* _) (TZ Current{..}) =
        case unK $ apFn t currentState of
          TransitionAt _tip epoch ->
            -- We haven't reached the next era yet, but the transition is
            -- already known. The safe zone applies from the start of the
            -- next era.
            let currentEnd = History.mkUpperBound params currentStart epoch
                nextStart  = currentEnd
            in NonEmptyCons EraSummary {
                   eraStart  = currentStart
                 , eraParams = params
                 , eraEnd    = EraEnd currentEnd
                 }
             $ NonEmptyOne EraSummary {
                   eraStart  = nextStart
                 , eraParams = nextParams
                 , eraEnd    = applySafeZone
                                 nextParams
                                 nextStart
                                 (At (boundSlot nextStart))
                 }
          LedgerTip ledgerTip -> NonEmptyOne $
            -- The transition to the /next/ era is not yet known, but it's
            -- possible that the tip was actually in the previous era. If that
            -- is the case, the safe zone of /this/ era extends from the start
            -- of this era.  Otherwise, the safe zone extends from the current
            -- ledger tip.
            EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              (max ledgerTip (At (boundSlot currentStart)))
              }

    go Nil _ t = case t of {}

    -- Apply safe zone from the specified 'SlotNo'
    --
    -- All arguments must be referring to or in the same era.
    applySafeZone :: EraParams -> Bound -> WithOrigin SlotNo -> EraEnd
    applySafeZone params@EraParams{..} start =
          History.mkEraEnd params start
        . History.maxMaybeEpoch (History.safeBeforeEpoch eraSafeZone)
        . History.slotToEpochBound params start
        . History.addSlots (History.safeFromTip eraSafeZone)
        . fromWithOrigin (boundSlot start)
