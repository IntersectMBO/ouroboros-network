{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module      : Data.Window.DigestCount
-- Description : Approximate quantile statistics over a sliding window.
-- Stability   : experimental
--
-- This module provides a sliding window backed by a t-digest
-- enabling approximate quantile queries over a fixed-size window
-- of the most recently inserted 'Double' samples.
--
-- The t-digest is maintained incrementally as the window slides —
-- it is cached at the root of the underlying finger tree and accessible
-- in O(1) via 'windowMeasure'. This makes it efficient to query multiple
-- quantiles over the same window without recomputing the digest:
--
-- @
-- let prices = [12.3, 47.8, 5.1, 89.0, 34.2, 67.5, 23.9, 58.4, 41.7, 76.2]
--     w = fromListN 5 prices :: DigestWindow 5
-- in (windowQuantile 0.50 w, windowQuantile 0.95 w, windowQuantile 0.99 w)
-- @
--
-- = Approximate quantiles
--
-- Quantile estimates are __approximate__, with error bounded by the
-- compression parameter @comp@. A higher compression parameter gives
-- more accurate estimates at the cost of a larger digest. For most
-- monitoring and analytics use cases the approximation is acceptable;
-- applications requiring exact quantiles should use a sorted-structure
-- approach (e.g. @streamly-statistics@) which trades O(δ) memory for
-- O(w).
--
-- = Memory
--
-- The digest size is O(δ) where δ is the compression parameter,
-- independent of the window size. This makes the t-digest backed window
-- particularly attractive for large windows where an exact sorted
-- structure would require O(w) memory.
--
-- = Ordering convention
--
-- The construction functions differ in the expected input order —
-- see the per-function haddocks — so refer to those when feeding
-- pre-collected data.
--
-- = Integration
--
-- For time-based windowing over 'Double' samples, see
-- "Data.Window.DigestTimed". For the generic windowing machinery this module
-- is built on, see "Data.Window.Count".
--
module Data.Window.DigestCount
  ( -- * Types
    DigestWindow
  , DigestSample (..)
    -- * Construction
  , W.empty
  , singleton
    -- ** From collections
  , fromListN
  , fromFoldable
    -- * Insertion
  , insert
  , insertMany
    -- * Querying
  , W.size
  , W.windowMaxSize
  , W.isFull
  , W.isEmpty
  , W.windowMeasure
    -- ** Statistics
  , windowQuantile
  , windowMedian
  , windowMean
    -- * Splitting
  , W.evictOldest
  , W.evictOldestN
  , W.trimByMeasure
  , W.resize
    -- * Conversions
  , W.toNewestFirst
  , W.toOldestFirst
    -- * Convenience
  , castComp
    -- * re-exports
  , W.Window
  ) where

import Data.Coerce
import Data.FingerTree qualified as FT
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import Data.TDigest (TDigest)
import Data.TDigest qualified as TD
import GHC.TypeLits

import Data.Window.Internal.Count qualified as W


type DigestWindow comp = W.Window (TDigest comp) (DigestSample comp)

-- sadly the compression parameter is needed for the instance below,
-- other than that it has no intrinsic semantics.
newtype DigestSample (comp :: Nat) = DigestSample { getDigestSample :: Double }
  deriving (Show, Eq, Ord)

instance KnownNat comp => FT.Measured (TDigest comp) (DigestSample comp) where
  {-# INLINE measure #-}
  measure (DigestSample x) = TD.singleton x


-- | Constructs a window with capacity of  @windowMaxSize@ containing a single sample
--
singleton :: (KnownNat comp)
          =>Int
          -> Double
          -> DigestWindow comp
singleton = W.singleton


-- | Insert a new sample, then evict if the window has
-- exceeded @windowMaxSize@.
--
-- Unlike the other window types in this package, t-digest insertion is
-- __not__ \(O(1)\) amortised: each insertion triggers up to
-- \(O(\log w)\) 'TDigest' merges along the finger tree spine, and each
-- merge runs in \(O(\delta)\) where δ is the compression parameter.
-- Effective cost is \(O(\delta \log w)\) per insert.
--
insert :: (KnownNat comp)
       => Double
       -> DigestWindow comp
       -> DigestWindow comp
insert = W.insert


-- | Insert many samples into the window, evicting as soon
-- as the size exceeds @windowMaxSize@. For a bulk add followed
-- by single trim operation, see 'fromFoldable'.
-- Elements should be provided in in order of freshest samples in the tail.
-- \(O(n \delta \log w)\), use when n >> w
--
insertMany :: (KnownNat comp, Foldable f)
           => f Double
           -> DigestWindow comp
           -> DigestWindow comp
insertMany xs w = foldl' (flip insert) w xs


-- | This is a more efficient alternative to 'fromFoldable' for
-- creating a window from a list because the input is only processed
-- up to window size @w@ of elements.
-- \(O(\w log w)\)
--
fromListN :: (KnownNat comp)
          => Int
          -> [Double]
          -> DigestWindow comp
fromListN = W.fromListN


-- | Construct a window from a 'Foldable' collection,
-- which first creates a window from all available elements and performs
-- a single trim operation at the end.
-- \(O(\n log n)\), use when n ~= w
--
fromFoldable :: forall t comp. (KnownNat comp, Foldable t)
             => Int
             -> t Double
             -> DigestWindow comp
fromFoldable = W.fromFoldable


-- CONVENIENCE UTILITIES --

-- | cast the compression phantom type parameter on the
-- sample type.
--
castComp :: DigestSample comp -> DigestSample comp2
castComp = coerce


-- | Retrieve the @q@ quantile
-- \(O(1)\)
--
windowQuantile :: KnownNat comp
               => Double
               -> DigestWindow comp
               -> Maybe Double
windowQuantile q = TD.quantile q . W.windowMeasureV . FT.measure . W.windowTree


-- | Retrieve the window median
-- \(O(1)\)
--
windowMedian :: KnownNat comp
             => DigestWindow comp
             -> Maybe Double
windowMedian = windowQuantile 0.5


-- | Retrieve the window mean
-- \(O(1)\)
--
windowMean :: KnownNat comp
           => DigestWindow comp
           -> Maybe Double
windowMean = TD.mean . W.windowMeasureV . FT.measure . W.windowTree
