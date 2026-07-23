-- |
-- Module      : Data.Window.DigestTimeBatched
-- Description : Approximate quantile statistics over a time-based sliding window.
-- Stability   : experimental
--
-- A time-based sliding window backed by a t-digest, supporting
-- approximate quantile queries over the specified duration.
-- The window is quantized by buckets of some shorter duration, and
-- samples are inserted into the newest bucket's t-digest object.
-- Once the bucket is sealed, it is inserted into the window, and buckets
-- which spill past the end of the window are evicted. This implies that
-- the granularity of statistics is determined by the ratio of bucket duration
-- to the overall window duration. The digest is cached at the
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
module Data.Window.DigestTimeBatched
  ( -- * Types
    TimedDigestWindow
    -- * Construction
  , empty
    -- TODO: From collections
    -- , fromListN
    -- , fromFoldable
    -- * Insertion
  , insert
    -- TODO: , insertMany
    -- * Querying
  , sampleCount
  , windowDuration
  , windowDigest
    -- ** Statistics
  , windowQuantile
  , windowMedian
  , windowMean
  , windowVariance
  , windowStdDev
  , windowMinMaxValues
    -- * Adjustments
  , evictBefore
  , reset
    -- * re-exports
  , TimeLike (..)
  , TDigest
  ) where

import Data.TDigest (TDigest)

import Data.Window.Internal.DigestTimeBatched
import Data.Window.TimeLike
