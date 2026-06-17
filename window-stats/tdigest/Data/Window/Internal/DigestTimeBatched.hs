{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Window.Internal.DigestTimeBatched where

import Control.DeepSeq
import Control.Exception (assert)
import Data.FingerTree (FingerTree, ViewL (..))
import Data.FingerTree qualified as FT
import Data.TDigest (TDigest)
import Data.TDigest qualified as TD
import Data.TDigest.Internal (Mean)
import GHC.Generics
import GHC.TypeLits

import Data.Window.TimeLike


-- | Finger-tree measure over the sealed buckets.
data BucketMeasure t (comp :: Nat) = BucketMeasure
  { bmFinish :: !(Maybe t)
    -- ^ Time at insertion of a sealed bucket
  , bmCount  :: {-# UNPACK #-} !Int
    -- ^ Number of samples in the prefix
    -- the sliding window is bounded and therefore so is space usage,
    -- but tactical forcing will even allow us to not compute the measure
    -- for the elements which are evicted from the window, which is
    -- fortunate as combining digests, which the finger tree does frequently,
    -- is \(O(delta)\).
  , bmDigest :: TDigest comp
  }
  deriving (Show, Generic, NFData)

-- | A sealed bucket
data SealedBucket t (comp :: Nat) = SealedBucket
  { sbFinish :: !t
    -- ^ newest sample time ie. when bucket is inserted into the finger tree
  , sbCount  :: {-# UNPACK #-} !Int
    -- ^ Number of samples in the bucket
  , sbDigest :: !(TDigest comp)
  }
  deriving (Show, Generic, NFData)

instance KnownNat comp => Semigroup (BucketMeasure t comp) where
  BucketMeasure _f1 c1 d1 <> BucketMeasure f2 c2 d2 = BucketMeasure f2 (c1 + c2) (d1 <> d2)

instance KnownNat comp => Monoid (BucketMeasure t comp) where
  mempty = BucketMeasure Nothing 0 mempty

instance KnownNat comp => FT.Measured (BucketMeasure t comp) (SealedBucket t comp) where
  {-# INLINE measure #-}
  measure (SealedBucket sealed count digest) = BucketMeasure (pure sealed) count digest

-- | The open (in-progress) bucket.
data OpenBucket t (comp :: Nat) = OpenBucket
  { obStart  :: !t -- ^ first sample time
  , obFinish :: !t -- ^ latest sample time
  , obCount  :: {-# UNPACK #-} !Int
  , obDigest :: !(TDigest comp)
  }
  deriving (Show, Generic, NFData)

-- | A time-bucketed sliding window.  The finger tree holds sealed buckets,
-- newest at the right; 'tdwBucket' is the bucket currently being filled.
data TimedDigestWindow t (comp :: Nat) = TimedDigestWindow
  { tdwDuration       :: !(Dur t) -- ^ duration of the whole window
  , tdwBucketDuration :: !(Dur t) -- ^ duration of a single bucket
  , tdwBucket         :: !(Maybe (OpenBucket t comp))
  , tdwTree           :: !(FingerTree (BucketMeasure t comp) (SealedBucket t comp))
  }

deriving instance (Show (Dur t), Show t) => Show (TimedDigestWindow t comp)


-- | An empty window with the given bucket width and retention duration (a multiple
-- of bucket width).
empty :: (TimeLike t, KnownNat comp) => Dur t -> Int -> TimedDigestWindow t comp
empty tdwBucketDuration r =
  TimedDigestWindow
    { tdwDuration = fromIntegral r * tdwBucketDuration
    , tdwBucketDuration
    , tdwBucket = Nothing
    , tdwTree = FT.empty
    }


-- | Resets the window, keeping only the retention durations
reset :: KnownNat comp => TimedDigestWindow t comp -> TimedDigestWindow t comp
reset tdw = tdw { tdwBucket = Nothing, tdwTree = FT.empty }


-- | Returns the number of samples which were inserted into the fingertree
-- and which are still within the retention duration.
sampleCount :: KnownNat comp => TimedDigestWindow t comp -> Int
sampleCount TimedDigestWindow { tdwBucket, tdwTree } =
  case tdwBucket of
    Nothing -> 0
    Just bucket -> let count = bmCount $ FT.measure tdwTree
                   in count + obCount bucket


-- | Returns the digest of the sliding window, or Nothing if there are no samples
windowDigest :: KnownNat comp => TimedDigestWindow t comp -> Maybe (TDigest comp)
windowDigest TimedDigestWindow { tdwTree, tdwBucket } =
  case tdwBucket of
    Nothing -> Nothing
    Just OpenBucket { obDigest } ->
      let treeDigest = bmDigest $ FT.measure tdwTree
       in Just $! treeDigest <> obDigest


-- | Insert a new timestamped sample, then evict any buckets
-- that have spilled past the window duration. Note that eviction
-- occurs at the granularity of buckets so only when a new bucket
-- is added.
insert :: (TimeLike t, KnownNat comp)
       => (t, Double)
       -> TimedDigestWindow t comp
       -> TimedDigestWindow t comp
insert (t, a) tdw@TimedDigestWindow { tdwBucketDuration, tdwDuration, tdwBucket, tdwTree} =
    case fillBucket of
      (Nothing, bucket) -> tdw { tdwBucket = Just $! bucket }
      (Just sealed, bucket) ->
        let sample   = SealedBucket (obFinish sealed) (obCount sealed) (obDigest sealed)
            tdwTree' = tdwTree FT.|> sample
            cutoff   = negate tdwDuration `addT` t
            -- evictBefore will force the measure once after it's done
         in evictBefore cutoff tdw { tdwBucket = Just $! bucket
                                   , tdwTree = tdwTree' }
  where
    newBucket = OpenBucket
                  { obStart = t
                  , obFinish = t
                  , obCount = 1
                  , obDigest = TD.singleton a
                  }

    fillBucket = case tdwBucket of
      Nothing -> (Nothing, newBucket)
      Just bucket@OpenBucket { obStart, obFinish, obCount, obDigest } ->
        let dt = t `diffT` obStart
         in if dt < tdwBucketDuration
               then (Nothing, bucket { obFinish = max obFinish t
                                     , obDigest = TD.insert a obDigest
                                     , obCount  = succ obCount
                                     })
               else (tdwBucket, newBucket)


-- | Drop all buckets whose newest sample timestamp is older than the given value
--
evictBefore :: (KnownNat comp, TimeLike t)
            => t
            -> TimedDigestWindow t comp
            -> TimedDigestWindow t comp
evictBefore cutoff win = win { tdwTree = tdwTree', tdwBucket = tdwBucket' }
  where
    tdwBucket' = case tdwBucket win of
      Nothing -> Nothing
      Just bucket -> if obFinish bucket > cutoff
                       then tdwBucket win
                       else assert (FT.null tdwTree') Nothing

    tdwTree' = FT.dropUntil (maybe False (> cutoff) . bmFinish)
                            (tdwTree win)
    -- we will not have to combine t-digests of the buckets
    -- which we have dropped from the window
    !_ = bmDigest $ FT.measure tdwTree'

-- | Approximate duration covered by the samples in the window. 'Nothing'
-- for an empty window. The reported value may exceed the actual duration of
-- samples held by up to one bucket period over which actually there aren't
-- any samples held.
--
windowDuration :: (KnownNat comp, TimeLike t)
               => TimedDigestWindow t comp
               -> Maybe (Dur t)
windowDuration TimedDigestWindow { tdwBucket, tdwBucketDuration, tdwTree } =
    do
    start  <- start'
    finish <- obFinish <$> tdwBucket
    pure $! finish `diffT` start
  where
    start' = case FT.viewl tdwTree of
               EmptyL -> obStart <$> tdwBucket
               SealedBucket sealed _c _s :< _suffix -> Just $ negate tdwBucketDuration `addT` sealed


-- Convenience API --

-- | Returns the means of the outermost centroids - appoximations, not the actual min/max samples
windowMinMaxValues :: KnownNat comp => TimedDigestWindow t comp -> Maybe (Mean, Mean)
windowMinMaxValues TimedDigestWindow { tdwTree, tdwBucket } =
  case tdwBucket of
    Nothing     -> Nothing
    Just bucket ->
      let !bucketDigest = obDigest bucket
          !treeDigest  = bmDigest $ FT.measure tdwTree
          combined     = treeDigest <> bucketDigest
       in Just (TD.minimumValue combined, TD.maximumValue combined)


windowMedian :: KnownNat comp => TimedDigestWindow t comp -> Maybe Double
windowMedian = maybe Nothing TD.median . windowDigest


windowQuantile :: KnownNat comp => Double -> TimedDigestWindow t comp -> Maybe Double
windowQuantile q = maybe Nothing (TD.quantile q) . windowDigest


windowMean :: KnownNat comp => TimedDigestWindow t comp -> Maybe Double
windowMean = maybe Nothing TD.mean . windowDigest


windowVariance :: KnownNat comp => TimedDigestWindow t comp -> Maybe Double
windowVariance = maybe Nothing TD.variance . windowDigest


windowStdDev :: KnownNat comp => TimedDigestWindow t comp -> Maybe Double
windowStdDev = maybe Nothing TD.stddev . windowDigest
