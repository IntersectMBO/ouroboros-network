{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |
-- Module      : Data.Window.Bucketed
-- Description : Time-bucketed sliding window backed by a finger tree.
-- Stability   : experimental
--
-- A time-based sliding window that accumulates samples into fixed-width time
-- buckets rather than retaining every sample.  An /open/ bucket absorbs the
-- most recent samples; once a sample crosses the bucket width the open bucket
-- is sealed into a finger tree and a fresh one is started, and sealed buckets
-- older than the window duration are evicted whole.  Per-sample cost is just
-- folding the sample into the open bucket's measure; the finger tree is
-- touched only once per bucket.
--
-- Generic over the 'TimeLike' time type and the cached monoidal measure @v@,
-- like "Data.Window.Timed".  For approximate-quantile statistics see
-- "Data.Window.DigestBucketed" in the @with-tdigest@ sublibrary.
--
-- This module is intended to be imported qualified.
--
module Data.Window.Bucketed
  ( -- * Types
    BucketedWindow
    -- * Time abstraction
  , TimeLike (..)
    -- * Construction
  , empty
    -- * Insertion
  , insert
    -- * Querying
  , isEmpty
  , windowSize
  , windowMeasure
    -- ** Statistics
  , SumSample (..)
  , windowSum
  , MinMaxV
  , MinMaxSample (..)
  , windowMinMax
  , MomentSample (..)
  , WelfordMeasure
  , windowMean
  ) where

import Data.Monoid (Sum (..))

import Data.Window.Internal.Bucketed
import Data.Window.Internal.Measures
import Data.Window.TimeLike

-- | Running sum over the window ending at @now@; 'Nothing' when empty.
windowSum :: (TimeLike t, Num a)
          => t -> BucketedWindow t (Sum a) (SumSample a) -> Maybe a
windowSum now w
  | windowSize now w == 0 = Nothing
  | otherwise             = Just (getSum (windowMeasure now w))

-- | Running min/max over the window ending at @now@; 'Nothing' when empty.
windowMinMax :: (TimeLike t, Ord a)
             => t -> BucketedWindow t (MinMaxV a) (MinMaxSample a) -> Maybe (a, a)
windowMinMax now = getMinMaxV . windowMeasure now

-- | Running mean over the window ending at @now@; 'Nothing' when empty.
windowMean :: (TimeLike t, Fractional a)
           => t -> BucketedWindow t (WelfordMeasure a) (MomentSample a) -> Maybe a
windowMean now w =
    let m = windowMeasure now w in
    if welfordN m == 0
       then Nothing
       else Just (welfordMean m)
