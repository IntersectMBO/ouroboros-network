{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      : Data.Window.Internal.Bucketed
-- Description : Time-bucketed sliding window backed by a finger tree.
-- Stability   : internal
--
-- A time-based sliding window that, unlike "Data.Window.Internal.Timed",
-- does not keep every sample as a finger-tree element.  Samples are
-- accumulated into fixed-width time buckets: an /open/ bucket absorbs the
-- most recent samples (cheap, no finger-tree work per sample), and once a
-- sample crosses the bucket width the open bucket is sealed into the finger
-- tree and a fresh one is started.  Sealed buckets older than the window
-- duration are evicted whole.
--
-- The cached measure @v@ (a 'Monoid') is merged up the finger-tree spine, so
-- the merged measure over the retained buckets is available in @O(log b)@ for
-- @b@ buckets.  Per-sample cost is just folding the sample into the open
-- bucket's measure; the finger tree is touched only once per bucket.  The
-- window is generic over the 'TimeLike' time type and the monoidal measure.
--
module Data.Window.Internal.Bucketed where

import Data.FingerTree (FingerTree, Measured (..))
import Data.FingerTree qualified as FT
import Data.Maybe (isNothing)

import Data.Window.Internal.Timed (Bounds (..))
import Data.Window.TimeLike

-- | A sealed bucket: newest sample time, sample count, accumulated measure.
data Bucket t v = Bucket !t {-# UNPACK #-} !Int !v

-- | Finger-tree measure over sealed buckets: total sample count, time bounds
-- (for eviction) and the merged user measure.
data BucketMeasure t v = BucketMeasure
  { bmCount  :: {-# UNPACK #-} !Int
  , bmBounds :: !(Bounds t)
  , bmV      :: !v
  }

instance (Ord t, Semigroup v) => Semigroup (BucketMeasure t v) where
  {-# INLINE (<>) #-}
  BucketMeasure c1 b1 v1 <> BucketMeasure c2 b2 v2 =
    BucketMeasure (c1 + c2) (b1 <> b2) (v1 <> v2)

instance (Ord t, Monoid v) => Monoid (BucketMeasure t v) where
  {-# INLINE mempty #-}
  mempty = BucketMeasure 0 NoBounds mempty

instance (Ord t, Monoid v) => Measured (BucketMeasure t v) (Bucket t v) where
  {-# INLINE measure #-}
  measure (Bucket t n v) = BucketMeasure n (Bounds t t) v

-- | The open (in-progress) bucket.
data OpenBucket t v = OpenBucket
  { obStart :: !t
  , obEnd   :: !t
  , obCount :: {-# UNPACK #-} !Int
  , obV     :: !v
  }

-- | A time-bucketed sliding window.  The finger tree holds sealed buckets,
-- newest at the right; 'bwOpen' is the bucket currently being filled.
data BucketedWindow t v a = BucketedWindow
  { bwBucketDur :: !(Dur t)
  , bwWindowDur :: !(Dur t)
  , bwOpen      :: !(Maybe (OpenBucket t v))
  , bwTree      :: !(FingerTree (BucketMeasure t v) (Bucket t v))
  }

-- | An empty window with the given bucket width and retention duration.
empty :: (Ord t, Monoid v) => Dur t -> Dur t -> BucketedWindow t v a
empty bwBucketDur bwWindowDur =
  BucketedWindow { bwBucketDur, bwWindowDur, bwOpen = Nothing, bwTree = FT.empty }

-- | 'True' when the window holds no buckets at all.
isEmpty :: BucketedWindow t v a -> Bool
isEmpty BucketedWindow { bwOpen, bwTree } = isNothing bwOpen && FT.null bwTree

-- | Drop sealed buckets whose newest sample is at or before the cutoff.
evictTree :: (Ord t, Monoid v)
          => t
          -> FingerTree (BucketMeasure t v) (Bucket t v)
          -> FingerTree (BucketMeasure t v) (Bucket t v)
evictTree cutoff = FT.dropUntil newer
  where
    newer m = case bmBounds m of
                NoBounds        -> False
                Bounds _ newest -> newest > cutoff
{-# INLINEABLE evictTree #-}

-- | Insert a timestamped sample.  Accumulates into the open bucket; rolls the
-- open bucket into the finger tree when the sample crosses the bucket width;
-- evicts sealed buckets older than the window relative to the sample time.
insert :: (TimeLike t, Measured v a)
       => (t, a) -> BucketedWindow t v a -> BucketedWindow t v a
insert (t, a) w@BucketedWindow { bwBucketDur, bwWindowDur, bwOpen, bwTree } =
    w { bwOpen = Just open', bwTree = evictTree cutoff tree' }
  where
    !mv    = measure a
    cutoff = negate bwWindowDur `addT` t
    (open', tree') =
      case bwOpen of
        Nothing -> (OpenBucket t t 1 mv, bwTree)
        Just OpenBucket { obStart, obEnd, obCount, obV }
          | t `diffT` obStart < bwBucketDur ->
              (OpenBucket obStart (max obEnd t) (obCount + 1) (obV <> mv), bwTree)
          | otherwise ->
              (OpenBucket t t 1 mv, bwTree FT.|> Bucket obEnd obCount obV)
{-# INLINEABLE insert #-}

-- | Merged measure over the samples within the window ending at @now@.  The
-- open bucket is included while its newest sample is still inside the window.
windowMeasure :: (TimeLike t, Monoid v)
              => t -> BucketedWindow t v a -> v
windowMeasure now BucketedWindow { bwWindowDur, bwOpen, bwTree } =
    bmV (measure (evictTree cutoff bwTree)) <> openV
  where
    cutoff = negate bwWindowDur `addT` now
    openV  = case bwOpen of
               Just OpenBucket { obEnd, obV } | obEnd > cutoff -> obV
               _                                               -> mempty
{-# INLINEABLE windowMeasure #-}

-- | Number of samples within the window ending at @now@.
windowSize :: (TimeLike t, Monoid v)
           => t -> BucketedWindow t v a -> Int
windowSize now BucketedWindow { bwWindowDur, bwOpen, bwTree } =
    bmCount (measure (evictTree cutoff bwTree)) + openN
  where
    cutoff = negate bwWindowDur `addT` now
    openN  = case bwOpen of
               Just OpenBucket { obEnd, obCount } | obEnd > cutoff -> obCount
               _                                                   -> 0
{-# INLINEABLE windowSize #-}
