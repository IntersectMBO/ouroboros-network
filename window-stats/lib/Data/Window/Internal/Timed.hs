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
import Data.FingerTree qualified as FT
import Data.Foldable
import Data.Semigroup (Sum (..))
import Data.Time (DiffTime, NominalDiffTime, UTCTime)
import GHC.Generics

import Data.Window.Internal.Measures
import Data.Window.TimeLike


-- | Compact, strict carrier for the oldest- and newest-timestamp
-- bounds of a (possibly empty) sub-window.
--
data Bounds t = NoBounds | Bounds !t !t
  deriving (Generic, NFData, Show)

instance Ord t => Semigroup (Bounds t) where
  {-# INLINE (<>) #-}
  NoBounds     <> b            = b
  a            <> NoBounds     = a
  Bounds o1 n1 <> Bounds o2 n2 = Bounds (min o1 o2) (max n1 n2)

instance Ord t => Monoid (Bounds t) where
  {-# INLINE mempty #-}
  mempty = NoBounds


-- | A measure that tracks the count of elements, the timestamp
-- bounds, and the user-supplied measure. The 'Bounds' field is
-- 'NoBounds' for empty subtrees.
--
data TimedMeasure t v = TimedMeasure
  { timedMeasureCount  :: {-# UNPACK #-} !Int
  , timedMeasureBounds :: !(Bounds t)
  , timedMeasureV      :: !v
  }
  deriving (Generic, NFData, Show)

instance (Ord t, Semigroup v) => Semigroup (TimedMeasure t v) where
  {-# INLINE (<>) #-}
  TimedMeasure c1 b1 v1 <> TimedMeasure c2 b2 v2 =
    TimedMeasure (c1 + c2) (b1 <> b2) (v1 <> v2)

instance (Ord t, Monoid v) => Monoid (TimedMeasure t v) where
  {-# INLINE mempty #-}
  mempty = TimedMeasure 0 NoBounds mempty

instance (Ord t, FT.Measured v a) => FT.Measured (TimedMeasure t v) (TimedSample t a) where
  {-# INLINE measure #-}
  measure (TimedSample t a) =
    TimedMeasure
      { timedMeasureCount  = 1
      , timedMeasureBounds = Bounds t t
      , timedMeasureV      = FT.measure a
      }


-- | Oldest timestamp of the window, if any.
timedMeasureOldest :: TimedMeasure t v -> Maybe t
timedMeasureOldest m = case timedMeasureBounds m of
  NoBounds   -> Nothing
  Bounds o _ -> Just o

-- | Newest timestamp of the window, if any.
timedMeasureNewest :: TimedMeasure t v -> Maybe t
timedMeasureNewest m = case timedMeasureBounds m of
  NoBounds   -> Nothing
  Bounds _ n -> Just n


-- | Configured maximum duration of the window.
windowMaxDuration :: TimedWindow t v a -> Dur t
windowMaxDuration = timedWindowDuration
{-# INLINE windowMaxDuration #-}


-- | A sample wrapper with an associated timestamp.
--
data TimedSample t a = TimedSample
  { timedSampleTime  :: !t
  , timedSampleValue :: !a
  }
  deriving (Generic, NFData, Show)


-- | A window over a stream of timestamped samples within the given
-- duration (ie. max/min timestamp range).
--
data TimedWindow t v a = TimedWindow
  { timedWindowDuration :: !(Dur t)
  , timedWindowTree     :: !(FT.FingerTree (TimedMeasure t v) (TimedSample t a))
  }
  deriving Generic

deriving instance (Show (Dur t), Show t, Show v, Show a)
  => Show (TimedWindow t v a)
deriving instance (NFData (Dur t), NFData t, NFData v, NFData a)
  => NFData (TimedWindow t v a)

instance Foldable (TimedWindow t v) where
  foldMap f TimedWindow { timedWindowTree } =
    foldMap (f . timedSampleValue) timedWindowTree


-- | The user-supplied measure of the current window contents.
--
windowMeasure :: (Ord t, FT.Measured v a)
              => TimedWindow t v a
              -> v
windowMeasure = timedMeasureV . FT.measure . timedWindowTree
{-# INLINE windowMeasure #-}


-- | retrieve the sum over the sliding window
-- \(O(1)\)
--
windowSum :: (Ord t, Num a)
          => TimedWindow t (Sum a) (SumSample a) -> Maybe a
windowSum w
  | size w == 0 = Nothing
  | otherwise   = Just . getSum . windowMeasure $ w
{-# INLINE windowSum #-}


-- | retrieve min/max values over the sliding window
-- \(O(1)\)
--
windowMinMax :: (Ord t, Ord a)
             => TimedWindow t (MinMaxV a) (MinMaxSample a) -> Maybe (a, a)
windowMinMax = getMinMaxV . windowMeasure
{-# INLINE windowMinMax #-}


-- | retrieve the running mean (via Welford's algorithm) over the
-- sliding window.
-- \(O(1)\)
--
windowMean :: (Ord t, Fractional a)
           => TimedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowMean w
  | size w == 0 = Nothing
  | otherwise   = Just . welfordMean . windowMeasure $ w
{-# INLINE windowMean #-}


-- | retrieve the running sample variance (denominator @n - 1@) over
-- the sliding window.
-- \(O(1)\)
--
windowVariance :: (Ord t, Fractional a)
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
windowStdDev :: (Ord t, Floating a)
             => TimedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowStdDev = fmap sqrt . windowVariance
{-# INLINE windowStdDev #-}


-- | The number of elements currently in the window.
-- \(O(1)\)
--
size :: (Ord t, FT.Measured v a)
     => TimedWindow t v a
     -> Int
size = timedMeasureCount . FT.measure . timedWindowTree
{-# INLINE size #-}


-- | Actual duration covered by the samples in the window. 'Nothing'
-- for an empty window.
-- \(O(1)\)
--
windowDuration :: (TimeLike t, FT.Measured v a)
               => TimedWindow t v a
               -> Maybe (Dur t)
windowDuration w =
    case timedMeasureBounds (FT.measure (timedWindowTree w)) of
      NoBounds             -> Nothing
      Bounds oldest newest -> Just (newest `diffT` oldest)
{-# INLINEABLE windowDuration #-}
{-# SPECIALIZE windowDuration
      :: FT.Measured v a
      => TimedWindow UTCTime v a -> Maybe NominalDiffTime #-}
{-# SPECIALIZE windowDuration
      :: FT.Measured v a
      => TimedWindow Time v a -> Maybe DiffTime #-}


-- | Constructs an empty time window of the given @timedWindowDuration@ duration
--
empty :: (Ord t, FT.Measured v a)
      => Dur t
      -> TimedWindow t v a
empty timedWindowDuration =
  TimedWindow { timedWindowDuration, timedWindowTree = FT.empty }


-- | Constructs a window with duration of @timedWindowDuration@ containing a single sample
--
singleton :: (TimeLike t, FT.Measured v b, Coercible a b)
          => Dur t
          -> (t, a)
          -> TimedWindow t v b
singleton timedWindowDuration (t, a) =
  TimedWindow
    { timedWindowDuration
    , timedWindowTree = FT.singleton (TimedSample t (coerce a))
    }
{-# INLINEABLE singleton #-}


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
insert (t, a) win@TimedWindow { timedWindowTree } =
  evictStale win { timedWindowTree = TimedSample t (coerce a) FT.<| timedWindowTree }
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
insertMany as win@TimedWindow { timedWindowTree } = evictStale . toWin $ foldl' step timedWindowTree as
  where
    step ft (t, a) = TimedSample t (coerce a) FT.<| ft
    toWin ft = win { timedWindowTree = ft }
{-# INLINEABLE insertMany #-}


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
fromListN timedWindowDuration as = evictStale . toWin $ foldl' step FT.empty as
  where
    toWin ft = TimedWindow { timedWindowDuration, timedWindowTree = ft }
    step ft (t, a) = ft FT.|> TimedSample t (coerce a)
{-# INLINEABLE fromListN #-}


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
fromFoldable timedWindowDuration ats =
  evictStale . toWin $ foldl' step FT.empty ats
  where
    toWin timedWindowTree = TimedWindow { timedWindowDuration, timedWindowTree }
    step ft (t, a) = TimedSample t (coerce a) FT.<| ft
{-# INLINEABLE fromFoldable #-}


-- | Trim the window from the oldest end, keeping the longest
-- newest-end prefix whose cumulative user-supplied measure @v@ does
-- __not__ satisfy the predicate.
--
-- /Precondition:/ the predicate must be __monotone__ along the prefix.
-- See 'takeUntil' for the full caveat.
-- \(O(\log w)\)
--
trimByMeasure :: (Ord t, FT.Measured v a)
              => (v -> Bool)
              -> TimedWindow t v a
              -> TimedWindow t v a
trimByMeasure p = takeUntil (p . timedMeasureV)


-- | Discard the oldest sample.
-- \(O(1)\) amortized, \(O(\log w)\) w/c
--
evictOldest :: (Ord t, FT.Measured v a)
            => TimedWindow t v a
            -> TimedWindow t v a
evictOldest win@TimedWindow { timedWindowTree } =
  case FT.viewr timedWindowTree of
    FT.EmptyR      -> win
    prefix FT.:> _ -> win { timedWindowTree = prefix }


-- | Discard the @n@ oldest samples.
-- \(O(\log w)\)
--
evictOldestN :: (Ord t, FT.Measured v a)
             => Int
             -> TimedWindow t v a
             -> TimedWindow t v a
evictOldestN n win
    | n <= 0     = win
    | n >= total = win { timedWindowTree = FT.empty }
    | otherwise  = takeUntil (\m -> timedMeasureCount m > keep) win
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
  takeUntil $ \m -> case timedMeasureBounds m of
    NoBounds   -> False
    Bounds o _ -> o < cutoff
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
resize d win = evictStale win { timedWindowDuration = d }
{-# INLINEABLE resize #-}
{-# SPECIALIZE resize
      :: FT.Measured v a
      => NominalDiffTime -> TimedWindow UTCTime v a -> TimedWindow UTCTime v a #-}
{-# SPECIALIZE resize
      :: FT.Measured v a
      => DiffTime -> TimedWindow Time v a -> TimedWindow Time v a #-}


-- | Tests if window is empty
-- \(O(1)\)
--
isEmpty :: (Ord t, FT.Measured v a) => TimedWindow t v a -> Bool
isEmpty = (== 0) . size


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
  . timedWindowTree
{-# INLINE toTimedNewestFirst #-}


-- | Yields the window in reverse, ie. oldest elements at the head
-- with timestamps
-- \(O(w)\)
--
toTimedOldestFirst :: Coercible a b => TimedWindow t v a -> [(t, b)]
toTimedOldestFirst =
  foldl' (\acc (TimedSample t a) -> (t, coerce a) : acc) []
  . timedWindowTree


--- INTERNAL ---

-- | Evict all elements older than 'timedWindowDuration' relative to
-- the newest element in the window.
--
evictStale :: (TimeLike t, FT.Measured v a)
           => TimedWindow t v a
           -> TimedWindow t v a
evictStale win@TimedWindow { timedWindowDuration, timedWindowTree } =
  case timedMeasureBounds (FT.measure timedWindowTree) of
    NoBounds       -> win
    Bounds _ newest ->
      let cutoff = negate timedWindowDuration `addT` newest
      in  takeUntil (\m -> case timedMeasureBounds m of
                             NoBounds   -> False
                             Bounds o _ -> o < cutoff) win
{-# INLINEABLE evictStale #-}


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
takeUntil :: (Ord t, FT.Measured v a)
          => (TimedMeasure t v -> Bool)
          -> TimedWindow t v a
          -> TimedWindow t v a
takeUntil p win@TimedWindow { timedWindowTree } =
  win { timedWindowTree = FT.takeUntil p timedWindowTree }
