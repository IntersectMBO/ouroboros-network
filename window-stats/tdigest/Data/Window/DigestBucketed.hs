{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Module      : Data.Window.DigestBucketed
-- Description : Approximate quantiles over a time-bucketed sliding window.
-- Stability   : experimental
--
-- Approximate-quantile statistics over a "Data.Window.Bucketed" window whose
-- per-bucket measure is a t-digest.  Each fixed-width time bucket accumulates
-- its samples into one t-digest; the finger tree merges the buckets' digests,
-- so the window's merged digest (and hence its quantiles) is read cheaply,
-- while per-sample cost is a single t-digest insertion into the open bucket.
--
-- This module is intended to be imported qualified.
--
module Data.Window.DigestBucketed
  ( -- * Types
    BucketedDigestWindow
    -- * Construction
  , empty
    -- * Insertion
  , insert
    -- * Querying
  , isEmpty
  , windowSize
  , windowMeasure
    -- ** Statistics
  , windowQuantile
  , windowMedian
  , windowMean
    -- * re-exports
  , TimeLike (..)
  ) where

import Data.TDigest (TDigest)
import Data.TDigest qualified as TD
import GHC.TypeLits

import Data.Window.DigestCount (DigestSample (..))
import Data.Window.Internal.Bucketed (BucketedWindow)
import Data.Window.Internal.Bucketed qualified as B
import Data.Window.TimeLike

-- | A time-bucketed window of 'Double' samples, backed by per-bucket
-- t-digests merged across the retained buckets.
type BucketedDigestWindow t comp = BucketedWindow t (TDigest comp) (DigestSample comp)

-- | An empty window with the given bucket width and retention duration.
empty :: (Ord t, KnownNat comp)
      => Dur t -> Dur t -> BucketedDigestWindow t comp
empty = B.empty

-- | Insert a timestamped sample.
insert :: (TimeLike t, KnownNat comp)
       => (t, Double) -> BucketedDigestWindow t comp -> BucketedDigestWindow t comp
insert (t, x) = B.insert (t, DigestSample x)

isEmpty :: BucketedDigestWindow t comp -> Bool
isEmpty = B.isEmpty

-- | Number of samples within the window ending at @now@.
windowSize :: (TimeLike t, KnownNat comp)
           => t -> BucketedDigestWindow t comp -> Int
windowSize = B.windowSize

-- | The merged t-digest over the samples within the window ending at @now@.
windowMeasure :: (TimeLike t, KnownNat comp)
              => t -> BucketedDigestWindow t comp -> TDigest comp
windowMeasure = B.windowMeasure

-- | Approximate @q@-quantile over the window ending at @now@.
windowQuantile :: (TimeLike t, KnownNat comp)
               => Double -> t -> BucketedDigestWindow t comp -> Maybe Double
windowQuantile q now = TD.quantile q . B.windowMeasure now

-- | Approximate median over the window ending at @now@.
windowMedian :: (TimeLike t, KnownNat comp)
             => t -> BucketedDigestWindow t comp -> Maybe Double
windowMedian = windowQuantile 0.5

-- | Mean over the window ending at @now@.
windowMean :: (TimeLike t, KnownNat comp)
           => t -> BucketedDigestWindow t comp -> Maybe Double
windowMean now = TD.mean . B.windowMeasure now
