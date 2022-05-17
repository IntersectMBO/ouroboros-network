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
    -- * Lifting 'Telescope' operations
  , fromTZ
  , match
  , sequence
  , tip
    -- * Situated
  , Situated (..)
  , situate
    -- * Aligning
  , align
    -- * EpochInfo/Summary
  , reconstructSummary
  ) where

import           Prelude hiding (sequence)

import           Data.Functor.Product
import           Data.SOP.Strict hiding (shape)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraEnd (..),
                     EraParams (..), EraSummary (..), SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Util.Counting

import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.HardFork.Combinator.Basics
                     (EraExtensibility (..))
import           Ouroboros.Consensus.HardFork.Combinator.State.Lift
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     Requiring (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Ouroboros.Consensus.HardFork.Combinator.Util.Match (Mismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.Util.Match as Match
import           Ouroboros.Consensus.HardFork.Combinator.Util.Telescope
                     (Extend (..), Telescope (..))
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
  Lift telescope operations
-------------------------------------------------------------------------------}

tip :: SListI xs => HardForkState f xs -> NS f xs
tip (HardForkState st) = hmap currentState $ Telescope.tip st

match :: SListI xs
      => NS h xs
      -> HardForkState f xs
      -> Either (Mismatch h (Current f) xs) (HardForkState (Product h f) xs)
match ns (HardForkState t) =
    HardForkState . hmap distrib <$> Match.matchTelescope ns t
  where
    distrib :: Product h (Current f) blk -> Current (Product h f) blk
    distrib (Pair x (Current start y)) =
        Current start (Pair x y)

sequence :: forall f m xs. (SListI xs, Functor m)
         => HardForkState (m :.: f) xs -> m (HardForkState f xs)
sequence = \(HardForkState st) -> HardForkState <$>
    Telescope.sequence (hmap distrib st)
  where
    distrib :: Current (m :.: f) blk -> (m :.: Current f) blk
    distrib (Current start st) = Comp $
        Current start <$> unComp st

fromTZ :: HardForkState f '[blk] -> f blk
fromTZ = currentState . Telescope.fromTZ . getHardForkState

{-------------------------------------------------------------------------------
  Situated
-------------------------------------------------------------------------------}

-- | A @h@ situated in time
data Situated h f xs where
  SituatedCurrent :: Current f x ->    h x  -> Situated h f (x ': xs)
  SituatedNext    :: Current f x ->    h y  -> Situated h f (x ': y ': xs)
  SituatedFuture  :: Current f x -> NS h xs -> Situated h f (x ': y ': xs)
  SituatedPast    :: K Past    x ->    h x  -> Situated h f (x ': xs)
  SituatedShift   :: Situated h f xs        -> Situated h f (x ': xs)

situate :: NS h xs -> HardForkState f xs -> Situated h f xs
situate ns = go ns . getHardForkState
  where
    go :: NS h xs'
       -> Telescope (K Past) (Current f) xs'
       -> Situated h f xs'
    go (Z    era)  (TZ cur)    = SituatedCurrent cur era
    go (S (Z era)) (TZ cur)    = SituatedNext    cur era
    go (S (S era)) (TZ cur)    = SituatedFuture  cur era
    go (Z    era)  (TS past _) = SituatedPast   past era
    go (S    era)  (TS _ st)   = SituatedShift $ go era st

{-------------------------------------------------------------------------------
  Aligning
-------------------------------------------------------------------------------}

align :: forall xs f f' f''. All SingleEraBlock xs
      => InPairs (Translate f) xs
      -> NP (f' -.-> f -.-> f'') xs
      -> HardForkState f'  xs -- ^ State we are aligning with
      -> HardForkState f   xs -- ^ State we are aligning
      -> HardForkState f'' xs
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
               -> K Past blk
               -> Current f blk
               -> (K Past blk, Current f blk')
    newCurrent f (K past) curF = (
          K Past  { pastStart    = currentStart curF
                  , pastEnd      = curEnd
                  }
        , Current { currentStart = curEnd
                  , currentState = translateWith f
                                     (boundEpoch curEnd)
                                     (currentState curF)
                  }
        )
      where
        curEnd :: Bound
        curEnd = pastEnd past

{-------------------------------------------------------------------------------
  Summary/EpochInfo
-------------------------------------------------------------------------------}

reconstructSummary :: EraExtensibility
                   -> History.Shape xs
                   -> TransitionInfo         -- ^ At the tip
                   -> HardForkState f xs
                   -> History.Summary xs
reconstructSummary ext (History.Shape shape) transition (HardForkState st) =
    History.Summary $ go shape st
  where
    go :: Exactly xs' EraParams
       -> Telescope (K Past) (Current f) xs'
       -> NonEmpty xs' EraSummary
    go ExactlyNil t = case t of {}
    go (ExactlyCons params ss) (TS (K Past{..}) t) =
        NonEmptyCons (EraSummary pastStart (EraEnd pastEnd) params) $ go ss t
    go (ExactlyCons params ss) (TZ Current{..}) = case (transition, ss, ext) of

        (TransitionNone, ExactlyCons{}, _ext)       ->
          error "TransitionNone should only arise in the last known era"
        (TransitionNone, ExactlyNil, EraExtensible) ->
          error "TransitionNone should not arise if the chain is era-extensible"

        (_transition, ExactlyNil, NotEraExtensible) ->
          -- Since the last era in xs is the last era this chain will ever have,
          -- we can translate any future slot/time.
          one EraUnbounded

        -- Note: the above covered all possible TransitionNone cases

        (TransitionUnknowable, _ss, _ext) ->
          one $ applySafeZone
                  params
                  currentStart
                  (boundSlot currentStart)

        (TransitionUnknown ledgerTip, _ss, _ext) ->
          -- We don't yet know when the next era transition will happen. Even
          -- if this is the last era that the code knows about, there may be a
          -- next era on this chain (eg after a code update).
          one $ applySafeZone
                  params
                  currentStart
                  -- Even if the safe zone is 0, the first slot at -- which the
                  -- next era could begin is the /next/
                  (next ledgerTip)

        -- Note: the above covered all ExactlyNil and NotEraExtensible cases

        (TransitionKnown epoch, ExactlyCons nextParams ss', _ext) ->
          -- We haven't reached the next era yet, but the transition is
          -- already known. The safe zone applies from the start of the
          -- next era.
          let currentEnd = curEnd epoch
              nextStart  = curEnd epoch
              unbounded  = case ext of
                EraExtensible    -> False
                NotEraExtensible -> case ss' of
                  ExactlyNil     -> True
                  ExactlyCons{}  -> False
          in cons (EraEnd currentEnd)
           $ NonEmptyOne EraSummary {
                 eraStart  = nextStart
               , eraParams = nextParams
               , eraEnd    =
                   if unbounded then EraUnbounded else
                   applySafeZone
                     nextParams
                     nextStart
                     (boundSlot nextStart)
               }
        (TransitionKnown epoch, ExactlyNil, EraExtensible)        ->
          -- We haven't reached the next era yet, but the transition is
          -- already known, even though this code is not aware of a next
          -- era.
          --
          -- This is actually a reachable case! A user who forgot to update
          -- their node code could reach this in the hours preceding an era
          -- transition, after which their node would not longer be able to
          -- follow the network's chain (which continues to exist if enough
          -- other nodes did update). I haven't double-checked, but I would
          -- anticipate that this node could not even forge blocks, because
          -- it'd know that the wallclock is beyond the end of last era that
          -- it is aware of.
          one $ EraEnd $ curEnd epoch

      where
        one :: EraEnd -> NonEmpty (x : dummy) EraSummary
        one = NonEmptyOne . curSummary

        cons ::
             EraEnd
          -> NonEmpty      xs'  EraSummary
          -> NonEmpty (x : xs') EraSummary
        cons = NonEmptyCons . curSummary

        curEnd epoch   = History.mkUpperBound params currentStart epoch
        curSummary end = EraSummary {
             eraStart  = currentStart
           , eraParams = params
           , eraEnd    = end
           }

    -- Apply safe zone from the specified 'SlotNo'
    --
    -- All arguments must be referring to or in the same era.
    applySafeZone :: EraParams -> Bound -> SlotNo -> EraEnd
    applySafeZone params@EraParams{..} start =
        case eraSafeZone of
          UnsafeIndefiniteSafeZone ->
              const EraUnbounded
          StandardSafeZone safeFromTip ->
              EraEnd
            . History.mkUpperBound params start
            . History.slotToEpochBound params start
            . History.addSlots safeFromTip

    next :: WithOrigin SlotNo -> SlotNo
    next Origin        = SlotNo 0
    next (NotOrigin s) = succ s
