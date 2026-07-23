{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Window.Internal.Timed where

import Control.DeepSeq
import Control.Monad.Class.MonadTime.SI (Time)
import Data.Coerce
import Data.FingerTree (FingerTree, ViewL (..), ViewR (..))
import Data.FingerTree qualified as FT
import Data.Foldable
import Data.Maybe (listToMaybe)
import Data.Monoid (Last(..), Sum(..))
import Data.Time (UTCTime)
import GHC.Generics

import Data.Window.Internal.Measures
import Data.Window.TimeLike


-- | A measure that tracks the count of elements, the timestamp
-- bounds, and the user-supplied measure.
--
data TimedMeasure t v = TimedMeasure
  { tmStart :: !(Maybe t)
    -- ^ oldest timestamp in the window prefix
  , tmCount :: {-# UNPACK #-} !Int
    -- ^ number of samples in the window prefix
  , tmV     :: v
    -- ^ user supplied measure
  }
  deriving (Generic, NFData, Show)

instance Semigroup v => Semigroup (TimedMeasure t v) where
  {-# INLINE (<>) #-}
  TimedMeasure _s1 c1 v1 <> TimedMeasure s2 c2 v2 =
    TimedMeasure s2 (c1 + c2) (v1 <> v2)

instance Monoid v => Monoid (TimedMeasure t v) where
  {-# INLINE mempty #-}
  mempty = TimedMeasure Nothing 0 mempty

instance FT.Measured v a => FT.Measured (TimedMeasure t v) (TimedSample t a) where
  {-# INLINE measure #-}
  measure (TimedSample t a) =
    TimedMeasure
      { tmStart = Just t
      , tmCount = 1
      , tmV     = FT.measure a
      }


-- | Configured maximum duration of the window.
windowMaxDuration :: TimedWindow t v a -> Dur t
windowMaxDuration = twDuration
{-# INLINE windowMaxDuration #-}


-- | A sample wrapper with an associated timestamp.
--
data TimedSample t a = TimedSample
  { tsNow   :: !t
  , tsValue :: !a
  }
  deriving (Generic, NFData, Show)


-- | A window over a sequence of timestamped samples within the given
-- duration. Values are stored in non-increasing timestamp order, ie.
-- with freshest samples at the head.
data TimedWindow t v a = TimedWindow
  { twDuration :: !(Dur t)
  , twTree     :: !(FingerTree (TimedMeasure t v) (TimedSample t a))
  }
  deriving Generic

deriving instance (Show (Dur t), Show t, Show v, Show a)
  => Show (TimedWindow t v a)
deriving instance (NFData (Dur t), NFData t, NFData v, NFData a)
  => NFData (TimedWindow t v a)

instance Foldable (TimedWindow t v) where
  foldMap f TimedWindow { twTree } =
    foldMap (f . tsValue) twTree


-- | The user-supplied measure of the current window contents.
--
windowMeasure :: FT.Measured v a
              => TimedWindow t v a
              -> v
windowMeasure = tmV . FT.measure . twTree
{-# INLINE windowMeasure #-}


-- | retrieve the sum over the sliding window
-- \(O(1)\)
--
windowSum :: Num a
          => TimedWindow t (Sum a) (SumSample a) -> Maybe a
windowSum w
  | size w == 0 = Nothing
  | otherwise   = Just . getSum . windowMeasure $ w
{-# INLINE windowSum #-}


-- | retrieve min/max values over the sliding window
-- \(O(1)\)
--
windowMinMax :: Ord a
             => TimedWindow t (MinMaxV a) (MinMaxSample a) -> Maybe (a, a)
windowMinMax = getMinMaxV . windowMeasure
{-# INLINE windowMinMax #-}


-- | retrieve the running mean (via Welford's algorithm) over the
-- sliding window.
-- \(O(1)\)
--
windowMean :: Fractional a
           => TimedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowMean w
  | size w == 0 = Nothing
  | otherwise   = Just . welfordMean . windowMeasure $ w
{-# INLINE windowMean #-}


-- | retrieve the running sample variance (denominator @n - 1@) over
-- the sliding window.
-- \(O(1)\)
--
windowVariance :: Fractional a
               => TimedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowVariance w
    | welfordN m < 2 = Nothing
    | otherwise      = Just (welfordM2 m / fromIntegral (welfordN m - 1))
  where m = windowMeasure w
{-# INLINE windowVariance #-}


-- | retrieve the running sample standard deviation over the sliding
-- window.
-- \(O(1)\)
--
windowStdDev :: Floating a
             => TimedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowStdDev = fmap sqrt . windowVariance
{-# INLINE windowStdDev #-}


-- | The number of elements currently in the window.
-- \(O(1)\)
--
size :: FT.Measured v a
     => TimedWindow t v a
     -> Int
size = tmCount . FT.measure . twTree
{-# INLINE size #-}


-- | Actual duration covered by the samples in the window. 'Nothing'
-- for an empty window.
-- \(O(1)\)
--
windowDuration :: (TimeLike t, FT.Measured v a)
               => TimedWindow t v a
               -> Maybe (Dur t)
windowDuration TimedWindow { twTree } =
    do
    start  <- tmStart $ FT.measure twTree
    finish <- finish'
    pure $! finish `diffT` start
  where
    finish' = case FT.viewl twTree of
                EmptyL -> error "TimedWindow: empty tree with non-empty measure - internal invariant broken"
                TimedSample t _a :< _suffix -> Just t


-- | Constructs an empty time window of the given @timedWindowDuration@ duration
--
empty :: FT.Measured v a
      => Dur t
      -> TimedWindow t v a
empty twDuration =
  TimedWindow { twDuration, twTree = FT.empty }


-- | Constructs a window with duration of @timedWindowDuration@ containing a single sample
--
singleton :: (FT.Measured v b, Coercible a b)
          => Dur t
          -> (t, a)
          -> TimedWindow t v b
singleton twDuration (t, a) =
  TimedWindow
    { twDuration
    , twTree = FT.singleton (TimedSample t (coerce a))
    }


-- | Insert a new timestamped sample, then evict any elements
-- that have spilled past the window duration.
--
-- @a@ must be the unwrapped version of b.
-- \(O(1)\) amortized, \(O(\log w)\) w/c
--
insert :: (TimeLike t, FT.Measured v b, Coercible a b)
       => (t, a)
       -> TimedWindow t v b
       -> TimedWindow t v b
insert (t, a) win@TimedWindow { twDuration, twTree } =
  evictBefore cutoff win { twTree = TimedSample t (coerce a) FT.<| twTree }
  where
    cutoff = negate twDuration `addT` t
{-# INLINEABLE insert #-}


-- | Insert a sequence of timestamped samples into the window, performing
-- a single trim operation at the end.
--
-- Elements should be provided in __non-decreasing__ timestamp order for
-- correct eviction behaviour.
-- @a@ must be the unwrapped version of b.
-- \(O(\n log w)\), use when n >> w
--
insertMany :: (TimeLike t, FT.Measured v b, Foldable f, Coercible a b)
           => f (t, a)
           -> TimedWindow t v b
           -> TimedWindow t v b
insertMany as win@TimedWindow { twDuration, twTree } = maybe win (`evictBefore` toWin twTree') cutoff
  where
    step (ft, finish') (t, a) = (TimedSample t (coerce a) FT.<| ft, finish' <> pure t)
    (!twTree', !finish) = foldl' step (twTree, mempty) as
    cutoff = do
      finish' <- getLast finish
      pure $ negate twDuration `addT` finish'

    toWin ft = win { twTree = ft }


-- | Build a 'TimedWindow' from a list of timestamped samples, retaining
-- only elements within the given duration of the newest element.
--
-- Elements should be provided in __non-increasing__ timestamp order
-- for correct behaviour (__note__: this is opposite from 'insertMany' and 'fromFoldable')
-- \(O(\w log w)\)
--
fromListN :: (TimeLike t, FT.Measured v b, Coercible a b)
         => Dur t
         -> [(t, a)]
         -> TimedWindow t v b
fromListN twDuration as = maybe (empty twDuration) (`evictBefore` win) cutoff
  where
    step ft (t, a) = ft FT.|> TimedSample t (coerce a)
    twTree = foldl' step mempty as
    cutoff = do
      finish' <- fst <$> listToMaybe as
      pure $ negate twDuration `addT` finish'

    win = TimedWindow { twDuration, twTree }


-- | Construct a window from a 'Foldable' collection, performing
-- a single trim operation at the end.
-- Elements should be provided in __non-decreasing__ timestamp order for
-- correct eviction behaviour.
-- \(O(\n log n)\), use when n ~= w
--
fromFoldable :: (TimeLike t, FT.Measured v b, Foldable f, Coercible a b)
             => Dur t
             -> f (t, a)
             -> TimedWindow t v b
fromFoldable twDuration as = maybe (empty twDuration) (`evictBefore` win) cutoff
  where
    step (ft, finish') (t, a) = (TimedSample t (coerce a) FT.<| ft, finish' <> pure t)
    (!twTree, !finish) = foldl' step (mempty, mempty) as
    cutoff = do
      finish' <- getLast finish
      pure $ negate twDuration `addT` finish'

    win = TimedWindow { twDuration, twTree }


-- | Trim the window from the oldest end, keeping the longest
-- newest-end prefix whose cumulative user-supplied measure @v@ does
-- __not__ satisfy the predicate.
--
-- /Precondition:/ the predicate must be __monotone__ along the prefix.
-- See 'takeUntil' for the full caveat.
-- \(O(\log w)\)
--
trimByMeasure :: FT.Measured v a
              => (v -> Bool)
              -> TimedWindow t v a
              -> TimedWindow t v a
trimByMeasure p = takeUntil (p . tmV)
{-# INLINE trimByMeasure #-}


-- | Discard the oldest sample.
-- \(O(1)\) amortized, \(O(\log w)\) w/c
--
evictOldest :: FT.Measured v a
            => TimedWindow t v a
            -> TimedWindow t v a
evictOldest win@TimedWindow { twTree } =
  case FT.viewr twTree of
    EmptyR      -> win
    prefix :> _ -> win { twTree = prefix }


-- | Discard the @n@ oldest samples.
-- \(O(\log w)\)
--
evictOldestN :: FT.Measured v a
             => Int
             -> TimedWindow t v a
             -> TimedWindow t v a
evictOldestN n win
    | n <= 0     = win
    | n >= total = win { twTree = FT.empty }
    | otherwise  = takeUntil (\m -> tmCount m > keep) win
  where
    total = size win
    keep  = total - n


-- | Evict all elements whose timestamp is older than the given cutoff time.
-- \(O(\log w)\)
--
evictBefore :: (TimeLike t, FT.Measured v a)
            => t
            -> TimedWindow t v a
            -> TimedWindow t v a
evictBefore cutoff =
  takeUntil (maybe False (<= cutoff) . tmStart)

{-# INLINEABLE evictBefore #-}
{-# SPECIALIZE evictBefore
      :: FT.Measured v a
      => UTCTime -> TimedWindow UTCTime v a -> TimedWindow UTCTime v a #-}
{-# SPECIALIZE evictBefore
      :: FT.Measured v a
      => Time -> TimedWindow Time v a -> TimedWindow Time v a #-}


-- | Set a new maximum duration for the window, evicting any samples
-- now older than that duration relative to the newest element.
-- \(O(\log w)\)
--
resize :: (TimeLike t, FT.Measured v a)
       => Dur t
       -> TimedWindow t v a
       -> TimedWindow t v a
resize twDuration win@TimedWindow { twTree } =
  case FT.viewl twTree of
    EmptyL -> win { twDuration }
    TimedSample t _a :< _suffix ->
      let cutoff = negate twDuration `addT` t
       in evictBefore cutoff win { twDuration }


-- | Tests if window is empty
-- \(O(1)\)
--
isEmpty :: FT.Measured v a => TimedWindow t v a -> Bool
isEmpty = (== 0) . size
{-# INLINE isEmpty #-}


-- | Yields the window in order, ie. newest elements at the head
-- \(O(w)\)
--
toNewestFirst :: Coercible a b => TimedWindow t v a -> [b]
toNewestFirst = coerce . toList
{-# INLINE toNewestFirst #-}


-- | Yields the window in reverse, ie. oldest elements at the head
--
toOldestFirst :: Coercible a b => TimedWindow t v a -> [b]
toOldestFirst = coerce . foldl' (flip (:)) []


-- | Yields the window in order, ie. newest elements at the head,
-- with timestamps.
-- \(O(w)\)
--
toTimedNewestFirst :: Coercible a b => TimedWindow t v a -> [(t, b)]
toTimedNewestFirst =
  foldr (\(TimedSample t a) acc -> (t, coerce a) : acc) []
  . twTree
{-# INLINE toTimedNewestFirst #-}


-- | Yields the window in reverse, ie. oldest elements at the head
-- with timestamps
-- \(O(w)\)
--
toTimedOldestFirst :: Coercible a b => TimedWindow t v a -> [(t, b)]
toTimedOldestFirst =
  foldl' (\acc (TimedSample t a) -> (t, coerce a) : acc) []
  . twTree


--- INTERNAL ---

-- | Returns the longest prefix of the window whose
-- accumulated 'TimedMeasure' does __not__ satisfy the predicate.
-- Equivalently, walks the window from newest to oldest, accumulating
-- the measure, and stops at the first point where the predicate flips
-- to 'True' — discarding everything from that point on (the oldest
-- tail).
--
-- /Precondition:/ the predicate must be __monotone__ along the prefix:
-- once it returns 'True' for some prefix, it must continue to return
-- 'True' for every longer prefix. Calling 'takeUntil' with a
-- non-monotone predicate (for example one phrased over the running
-- variance carried by 'WelfordMeasure') yields an unspecified split
-- point and is almost never what you want.
--
-- This function lives in the internal module as the primitive used by
-- 'evictBefore' and 'trimByMeasure'. Public callers should prefer
-- 'Data.Window.Timed.trimByMeasure' (which hides 'TimedMeasure' and
-- works on the user-supplied measure @v@) or the dedicated helper
-- 'evictBefore'.
-- \(O(\log w)\)
--
takeUntil :: FT.Measured v a
          => (TimedMeasure t v -> Bool)
          -> TimedWindow t v a
          -> TimedWindow t v a
takeUntil p win@TimedWindow { twTree } = win { twTree = twTree' }
  where
    twTree' = FT.takeUntil p twTree
    !_ = tmV $ FT.measure twTree'
