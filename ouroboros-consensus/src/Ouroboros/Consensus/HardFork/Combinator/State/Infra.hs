{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
  , dropAllPast
    -- * Lifting 'Telescope' operations
  , tip
  , match
  , sequence
  , bihczipWith
  , fromTZ
    -- * Situated
  , Situated(..)
  , situate
    -- * Aligning
  , align
    -- * Rewinding
  , retractToSlot
    -- * EpochInfo/Summary
  , reconstructSummary
  ) where

import           Prelude hiding (sequence)

import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..), SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
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

-- | Drop all past snapshots
dropAllPast :: SListI xs => HardForkState_ g f xs -> HardForkState_ g' f xs
dropAllPast (HardForkState st) = HardForkState $
    Telescope.bihmap (\past -> past { pastSnapshot = NoSnapshot }) id st

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
  Situated
-------------------------------------------------------------------------------}

-- | A @h@ situated in time
data Situated h f xs where
  SituatedCurrent :: Current f x ->    h x  -> Situated h f (x ': xs)
  SituatedNext    :: Current f x ->    h y  -> Situated h f (x ': y ': xs)
  SituatedFuture  :: Current f x -> NS h xs -> Situated h f (x ': y ': xs)
  SituatedPast    :: Past    f x ->    h x  -> Situated h f (x ': xs)
  SituatedShift   :: Situated h f xs        -> Situated h f (x ': xs)

situate :: NS h xs -> HardForkState f xs -> Situated h f xs
situate ns = go ns . getHardForkState
  where
    go :: NS h xs'
       -> Telescope (Past f) (Current f) xs'
       -> Situated h f xs'
    go (Z    era)  (TZ cur)    = SituatedCurrent cur era
    go (S (Z era)) (TZ cur)    = SituatedNext    cur era
    go (S (S era)) (TZ cur)    = SituatedFuture  cur era
    go (Z    era)  (TS past _) = SituatedPast   past era
    go (S    era)  (TS _ st)   = SituatedShift $ go era st

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
          Origin      -> isFirst -- Assume 'Origin' in the first era
          NotOrigin s -> boundSlot pastStart <= s && s < boundSlot pastEnd

    retract :: Retract Maybe (Past f) (Current f) blk blk'
    retract = Retract $ \past _oldCur ->
        Current (pastStart past) <$> getSnapshot (pastSnapshot past)

getSnapshot :: Snapshot f blk -> Maybe (f blk)
getSnapshot (Snapshot _ st) = Just st
getSnapshot NoSnapshot      = Nothing

{-------------------------------------------------------------------------------
  Summary/EpochInfo
-------------------------------------------------------------------------------}

reconstructSummary :: History.Shape xs
                   -> TransitionInfo         -- ^ At the tip
                   -> HardForkState_ g f xs
                   -> History.Summary xs
reconstructSummary (History.Shape shape) transition (HardForkState st) =
    History.Summary $ go shape st
  where
    go :: Exactly xs' EraParams
       -> Telescope (Past g) (Current f) xs'
       -> NonEmpty xs' EraSummary
    go (K params :* ss) (TS Past{..} t) =
        NonEmptyCons (EraSummary pastStart (EraEnd pastEnd) params) $ go ss t
    go (K params :* Nil) (TZ Current{..}) =
        -- The current era is the last. We assume it lasts until all eternity.
        NonEmptyOne (EraSummary currentStart EraUnbounded params)
    go (K params :* K nextParams :* _) (TZ Current{..}) =
        case transition of
          TransitionKnown epoch ->
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
                                 (boundSlot nextStart)
                 }
          TransitionUnknown ledgerTip -> NonEmptyOne $ EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              -- Even if the safe zone is 0, the first slot at
                              -- which the next era could begin is the /next/
                              (next ledgerTip)
              }
          -- 'TransitionImpossible' is used in one of two cases: we are in the
          -- final era (clearly not the case here) or this era is a future era
          -- that hasn't begun yet, in which case the safe zone must start at
          -- the beginning of this era.
          TransitionImpossible -> NonEmptyOne $ EraSummary {
                eraStart  = currentStart
              , eraParams = params
              , eraEnd    = applySafeZone
                              params
                              currentStart
                              (boundSlot currentStart)
              }

    go Nil t = case t of {}

    -- Apply safe zone from the specified 'SlotNo'
    --
    -- All arguments must be referring to or in the same era.
    applySafeZone :: EraParams -> Bound -> SlotNo -> EraEnd
    applySafeZone params@EraParams{..} start =
        case eraSafeZone of
          UnsafeIndefiniteSafeZone ->
              const EraUnbounded
          StandardSafeZone safeFromTip safeBefore ->
              EraEnd
            . History.mkUpperBound params start
            . History.maxSafeBeforeEpoch safeBefore
            . History.slotToEpochBound params start
            . History.addSlots safeFromTip

    next :: WithOrigin SlotNo -> SlotNo
    next Origin        = SlotNo 0
    next (NotOrigin s) = succ s
