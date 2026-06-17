{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Window.Internal.Count where

import Control.DeepSeq
import Data.Coerce
import Data.FingerTree qualified as FT
import Data.Foldable
import Data.Monoid
import GHC.Generics

import Data.Window.Internal.Measures

-- | A measure that tracks the count of elements,
-- and the user-supplied measure.
--
data WindowMeasure v = WindowMeasure
  { windowMeasureCount :: {-# UNPACK #-} !Int
  , windowMeasureV     :: !v
  }
  deriving (Generic, NFData, Show)

instance Semigroup v => Semigroup (WindowMeasure v) where
  WindowMeasure c1 v1 <> WindowMeasure c2 v2 =
    WindowMeasure (c1 + c2) (v1 <> v2)

instance Monoid v => Monoid (WindowMeasure v) where
  {-# INLINE mempty #-}
  mempty = WindowMeasure 0 mempty

-- | A sample wrapper
--
newtype Sample a = Sample { sampleValue :: a }
  deriving (Generic, Show)
  deriving anyclass NFData

instance FT.Measured v a => FT.Measured (WindowMeasure v) (Sample a) where
  {-# INLINE measure #-}
  measure = WindowMeasure 1 . FT.measure . sampleValue


-- | A window over a stream of samples, evicting elements
-- spilling past 'windowMaxSize'
--
data Window v a = Window
  { windowMaxSize :: {-# UNPACK #-} !Int
  , windowTree    :: !(FT.FingerTree (WindowMeasure v) (Sample a))
  }
  deriving (Generic, NFData, Show)

instance Foldable (Window v) where
  foldMap f Window { windowTree } = foldMap (f . sampleValue) windowTree


-- | The user-supplied measure of the current window contents.
--
windowMeasure :: FT.Measured v a
              => Window v a
              -> v
windowMeasure = windowMeasureV . FT.measure . windowTree
{-# INLINE windowMeasure #-}


-- | retrieve the sum over the sliding window
-- \(O(1)\)
--
windowSum :: Num a => Window (Sum a) (SumSample a) -> Maybe a
windowSum w
  | size w == 0 = Nothing
  | otherwise   = Just . getSum . windowMeasure $ w
{-# INLINE windowSum #-}


-- | retrieve min/max values over the sliding window
-- \(O(1)\)
--
windowMinMax :: Ord a => Window (MinMaxV a) (MinMaxSample a) -> Maybe (a, a)
windowMinMax = getMinMaxV . windowMeasure
{-# INLINE windowMinMax #-}


-- | retrieve the running mean (via Welford's algorithm) over the
-- sliding window.
-- \(O(1)\)
--
windowMean :: Fractional a => Window (WelfordMeasure a) (MomentSample a) -> Maybe a
windowMean w
  | size w == 0 = Nothing
  | otherwise   = Just . welfordMean . windowMeasure $ w
{-# INLINE windowMean #-}


-- | retrieve the running sample variance (denominator @n - 1@) over
-- the sliding window.
-- \(O(1)\)
--
windowVariance :: Fractional a => Window (WelfordMeasure a) (MomentSample a) -> Maybe a
windowVariance w
    | welfordN m < 2 = Nothing
    | otherwise      = Just (welfordM2 m / fromIntegral (welfordN m - 1))
  where m = windowMeasure w
{-# INLINE windowVariance #-}


-- | retrieve the running sample standard deviation over the sliding
-- window.
-- \(O(1)\)
--
windowStdDev :: Floating a => Window (WelfordMeasure a) (MomentSample a) -> Maybe a
windowStdDev = fmap sqrt . windowVariance
{-# INLINE windowStdDev #-}


-- | Returns the count of elements in the window
-- \(O(1)\)
--
size :: FT.Measured v a
     => Window v a
     -> Int
size = windowMeasureCount . FT.measure . windowTree
{-# INLINE size #-}


-- | Constructs an empty window with capacity of @windowMaxSize@
--
empty :: FT.Measured v a => Int -> Window v a
empty windowMaxSize = Window { windowMaxSize, windowTree = FT.empty }


-- | Constructs a window with capacity of @windowMaxSize@ containing a single sample
--
singleton :: (FT.Measured v b, Coercible a b) => Int -> a -> Window v b
singleton windowMaxSize a =
  Window { windowMaxSize, windowTree = FT.singleton (Sample (coerce a)) }


-- | Insert a new sample, then evict if the window has
-- exceeded @windowMaxSize@.
--
-- @a@ must be the unwrapped version of b.
--
-- /Invariant:/ assumes the input window satisfies
-- @'size' w '<=' 'windowMaxSize' w@. Insert evicts at most one
-- element, so handing 'insert' an over-full window leaves it
-- over-full. Windows produced by any function in this module satisfy
-- the invariant, so this only matters if you construct or mutate the
-- 'Window' record by hand.
--
-- \(O(1)\) amortized, \(O(\log w)\) w/c
--
insert :: (FT.Measured v b, Coercible a b)
       => a
       -> Window v b
       -> Window v b
insert val win@Window { windowMaxSize, windowTree } = win { windowTree = ftree' }
  where
    ftree = Sample (coerce val) FT.<| windowTree
    ftree' = if size win >= windowMaxSize
                  then case FT.viewr ftree of
                         FT.EmptyR      -> ftree
                         prefix FT.:> _ -> prefix
                  else ftree


-- | Insert many samples into the window, evicting as soon
-- as the size exceeds @windowMaxSize@. For a bulk add followed by single trim
-- operation, see 'fromFoldable'.
-- Elements should be provided in in order of freshest samples in the __tail__.
-- \(O(\n log w)\), use when n >> w
--
insertMany :: (FT.Measured v b, Foldable f, Coercible a b)
           => f a
           -> Window v b
           -> Window v b
insertMany as w = foldl' (flip insert) w as


-- | Discard the oldest sample
-- \(O(1)\) amortized, \(O(\log w)\) w/c
--
evictOldest :: FT.Measured v a
            => Window v a
            -> Window v a
evictOldest win@Window { windowTree } =
  case FT.viewr windowTree of
    FT.EmptyR      -> win
    prefix FT.:> _ -> win { windowTree = prefix }


-- | Discard N oldest samples
-- \(O(\log w)\)
--
evictOldestN :: FT.Measured v a
             => Int
             -> Window v a
             -> Window v a
evictOldestN n win
    | n <= 0      = win
    | n >= total  = win { windowTree = FT.empty }
    | otherwise  = takeUntil (\m -> windowMeasureCount m > keep) win
  where
    total = size win
    keep  = total - n


-- | Returns the longest newest-end prefix of the window whose
-- accumulated 'WindowMeasure' does __not__ satisfy the predicate.
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
-- 'evictOldestN' and 'trimByMeasure'. Public callers should prefer
-- 'Data.Window.Count.trimByMeasure' (which hides 'WindowMeasure' and
-- works on the user-supplied measure @v@) or the dedicated helper
-- 'evictOldestN'.
-- \(O(\log w)\)
--
takeUntil :: FT.Measured v a
          => (WindowMeasure v -> Bool)
          -> Window v a
          -> Window v a
takeUntil p win@Window { windowTree } =
  win { windowTree = FT.takeUntil p windowTree }


-- | Trim the window from the oldest end, keeping the longest
-- newest-end prefix whose cumulative user-supplied measure @v@ does
-- __not__ satisfy the predicate.
--
-- For example, to retain only the most recent samples whose cumulative
-- weight (a 'Data.Monoid.Sum') stays under a budget:
--
-- @
-- trimByMeasure (\\s -> 'Data.Monoid.getSum' s > budget) window
-- @
--
-- /Precondition:/ the predicate must be __monotone__ along the prefix.
-- See 'takeUntil' for the full caveat.
-- \(O(\log w)\)
--
trimByMeasure :: FT.Measured v a
              => (v -> Bool)
              -> Window v a
              -> Window v a
trimByMeasure p = takeUntil (p . windowMeasureV)


-- | Set a new maximum size for the window, evicting the oldest
-- samples if the new size is smaller than the current count.
-- \(O(\log w)\)
--
resize :: FT.Measured v a
       => Int
       -> Window v a
       -> Window v a
resize n win@Window { windowTree }
  | n <= 0    = win { windowMaxSize = 0, windowTree = FT.empty }
  | size win > n =
      win { windowMaxSize = n
          , windowTree    = FT.takeUntil (\m -> windowMeasureCount m > n) windowTree
          }
  | otherwise = win { windowMaxSize = n }


-- | Tests if window is full
-- \(O(1)\)
--
isFull :: FT.Measured v a => Window v a -> Bool
isFull w = size w >= windowMaxSize w
{-# INLINE isFull #-}


-- | Tests if window is empty
-- \(O(1)\)
--
isEmpty :: FT.Measured v a => Window v a -> Bool
isEmpty = (== 0) . size
{-# INLINE isEmpty #-}


-- | This is a more efficient alternative to 'fromFoldable' for
-- creating a window from a list for cases where the length of the input
-- list is significantly greater than the provided window size:
-- only the first @windowMaxSize@ elements of the input are consumed.
--
-- Elements should be provided in the order of freshest samples at the __head__
-- for correct behaviour (__note__: this is opposite from 'insertMany' and 'fromFoldable')
-- \(O(\w log w)\)
--
fromListN :: forall v a b. (FT.Measured v b, Coercible a b)
          => Int -- ^ window size
          -> [a]
          -> Window v b
fromListN windowMaxSize as =
  Window
    { windowMaxSize
    , windowTree = FT.fromList . map (Sample . coerce) . take windowMaxSize $ as
    }


-- | Construct a window from a 'Foldable' collection,
-- which first creates a window from all available elements and performs
-- a single trim operation at the end.
-- Elements should be provided in the order of freshest samples in the __tail__
-- \(O(\n log n)\), use when n ~= w
--
fromFoldable :: forall t v a b. (FT.Measured v b, Foldable t, Coercible a b)
             => Int -- ^ window size
             -> t a
             -> Window v b
fromFoldable n as =
    if count > n then
      Window { windowMaxSize = n
             , windowTree = FT.takeUntil (\v -> windowMeasureCount v > n) ftree
             }
    else
      Window { windowMaxSize = n, windowTree = ftree }
  where
    ftree = foldl' (\ft a -> Sample (coerce a) FT.<| ft) FT.empty as
    count = windowMeasureCount . FT.measure $ ftree


-- | Yields the window in order, ie. newest elements at the head
-- \(O(n)\)
--
toNewestFirst :: Coercible a b => Window v a -> [b]
toNewestFirst = coerce . toList
{-# INLINE toNewestFirst #-}


-- | Yields the window in reverse, ie. oldest elements at the head
-- \(O(n)\)
--
toOldestFirst :: Coercible a b => Window v a -> [b]
toOldestFirst = coerce . foldl' (flip (:)) []
