{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- |
-- Module      : Data.Window.DigestTimed
-- Description : Approximate quantile statistics over a time-based sliding window.
-- Stability   : experimental
--
-- A time-based sliding window backed by a t-digest, supporting
-- approximate quantile queries over the most recent samples within a
-- configured duration of the newest one. The digest is cached at the
-- root of the underlying finger tree and accessible in \(O(1)\), so
-- multiple quantiles can be queried cheaply over the same window
-- without recomputing.
--
-- The time representation is abstracted via 'TimeLike' from
-- "Data.Window.TimeLike", so the same combinators work for wall-clock
-- ('UTCTime') and monotonic
-- ('Control.Monad.Class.MonadTime.SI.Time') timestamps.
--
-- = Approximate quantiles and memory
--
-- Estimates are approximate; error is bounded by the compression
-- parameter @comp@ (higher @comp@ = more accurate, larger digest).
-- The digest size is \(O(\delta)\) in @comp@, independent of the
-- window's element count — attractive for large or long-running
-- windows where an exact sorted structure would need \(O(w)\) memory.
--
-- = Ordering convention
--
-- The construction and insertion functions differ in the expected
-- input order — most expect non-decreasing timestamps, 'fromListN'
-- expects non-increasing. Consult each function's haddock; violating
-- the documented order corrupts the eviction behaviour.
--
-- = Integration
--
-- For a count-based digest window, see "Data.Window.DigestCount". For
-- the underlying time-based windowing machinery, see
-- "Data.Window.Timed". For composing with the @foldl@ package, see
-- "Data.Window.Fold.Timed" — its combinators are polymorphic over the
-- sample type and apply to digest windows directly.
--
module Data.Window.DigestTimed
  ( -- * Types
    TimedDigestWindow
  , DigestSample (..)
    -- * Construction
  , T.empty
  , singleton
    -- ** From collections
  , fromListN
  , fromFoldable
    -- * Insertion
  , insert
  , insertMany
    -- * Querying
  , T.size
  , T.windowMaxDuration
  , T.isEmpty
  , T.windowDuration
  , T.windowMeasure
    -- ** Statistics
  , windowQuantile
  , windowMedian
  , windowMean
    -- * Splitting
  , T.evictOldest
  , T.evictOldestN
  , T.evictBefore
  , T.trimByMeasure
  , T.resize
    -- * Conversions
  , T.toNewestFirst
  , T.toTimedNewestFirst
  , T.toOldestFirst
  , T.toTimedOldestFirst
    -- * re-exports
  , TimeLike (..)
  , TimedWindow
  ) where

import Data.TDigest (TDigest, mean, quantile)
import GHC.TypeLits

import Data.Window.DigestCount (DigestSample (..))
import Data.Window.Internal.Timed (TimedWindow)
import Data.Window.Internal.Timed qualified as T
import Data.Window.TimeLike


type TimedDigestWindow t comp = T.TimedWindow t (TDigest comp) (DigestSample comp)


-- | Constructs a window with duration of @timedWindowDuration@ containing a single sample
-- pair @(t, a)@.
--
singleton :: (TimeLike t, KnownNat comp)
          => Dur t
          -> (t, Double)
          -> TimedDigestWindow t comp
singleton = T.singleton


-- | Insert a new timestamped sample, then evict any elements that have
-- fallen outside the window duration.
--
-- Elements should be inserted in __non-decreasing__ timestamp order for
-- correct eviction behaviour.
--
-- Unlike the other window types in this package, t-digest insertion is
-- __not__ \(O(1)\) amortised: each insertion triggers up to
-- \(O(\log w)\) 'TDigest' merges along the finger tree spine, and each
-- merge runs in \(O(\delta)\) where δ is the compression parameter.
-- Effective cost is \(O(\delta \log w)\) per insert.
--
insert :: (TimeLike t, KnownNat comp)
       => (t, Double)
       -> TimedDigestWindow t comp
       -> TimedDigestWindow t comp
insert = T.insert


-- | Insert many timestamped samples into the window, evicting stale
-- elements after all insertions are complete.
--
-- Elements should be provided in __non-decreasing__ timestamp order.
-- @a@ must be the unwrapped version of b.
-- \(O(n \delta \log w)\), use when n >> w
--
insertMany :: (TimeLike t, KnownNat comp, Foldable f)
           => f (t, Double)
           -> TimedDigestWindow t comp
           -> TimedDigestWindow t comp
insertMany = T.insertMany


-- | Build a 'TimedWindow' from a list of timestamped samples, retaining
-- only elements within the given duration of the newest element.
--
-- Elements should be provided in __non-increasing__ timestamp order
-- (__note__: this is opposite from 'insertMany' and 'fromFoldable')
-- \(O(\w log w)\)
--
fromListN :: (TimeLike t, KnownNat comp)
         => Dur t
         -> [(t, Double)]
         -> TimedDigestWindow t comp
fromListN = T.fromListN


-- | Construct a window from a 'Foldable' collection, performing
-- a single trim operation at the end.
-- Elements should be provided in __non-decreasing__ timestamp order for
-- correct eviction behaviour.
-- \(O(\n log n)\), use when n ~= w
--
fromFoldable :: forall t f comp. (TimeLike t, KnownNat comp, Foldable f)
             => Dur t
             -> f (t, Double)
             -> TimedDigestWindow t comp
fromFoldable = T.fromFoldable


-- CONVENIENCE UTILITIES --

-- | Retrieve the @q@ quantile
-- \(O(1)\)
--
windowQuantile :: (TimeLike t, KnownNat comp)
               => Double
               -> TimedDigestWindow t comp
               -> Maybe Double
windowQuantile q = quantile q . T.windowMeasure


-- | Retrieve the window median
-- \(O(1)\)
--
windowMedian :: (TimeLike t, KnownNat comp)
             => TimedDigestWindow t comp
             -> Maybe Double
windowMedian = windowQuantile 0.5


-- | Retrieve the window mean
-- \(O(1)\)
--
windowMean :: (TimeLike t, KnownNat comp)
           => TimedDigestWindow t comp
           -> Maybe Double
windowMean = mean . T.windowMeasure
