-- |
-- Module      : Data.Window.Timed
-- Description : Time-based sliding window backed by a finger tree.
-- Stability   : experimental
--
-- This module provides a time-based sliding window data structure,
-- 'TimedWindow', which retains elements within a specified duration
-- of the newest element in the window. Elements older than the
-- duration are evicted automatically on insertion.
--
-- The time representation is abstracted via the 'TimeLike' class. Two
-- stock instances are provided in "Data.Window.TimeLike":
--
-- * @'TimedWindow' 'Data.Time.UTCTime' v a@ for wall-clock timestamps
--   from the @time@ package.
--
-- * @'TimedWindow' 'Control.Monad.Class.MonadTime.SI.Time' v a@ for
--   monotonic timestamps from @io-classes@/@si-timers@.
--
-- The window is backed by a finger tree ('Data.FingerTree.FingerTree'),
-- giving O(1) amortised insertion and O(log w) eviction. The window
-- measure is cached at the root of the finger tree and accessible in
-- O(1) via 'windowMeasure', making it efficient to query rolling
-- statistics.
--
-- This module is intended to be imported qualified.
--
-- = Ordering convention
--
-- The window's underlying finger tree always has the newest element
-- at the left end and the oldest at the right. The construction
-- functions differ in the expected input order — see the per-function
-- haddocks — so refer to those when feeding pre-collected data.
--
module Data.Window.Timed
  ( -- * Types
    TimedWindow
    -- * Time abstraction
  , TimeLike (..)
    -- * Construction
  , empty
  , singleton
    -- * From collections
  , fromListN
  , fromFoldable
    -- * Insertion
  , insert
  , insertMany
    -- * Querying
  , size
  , windowMaxDuration
  , isEmpty
  , windowDuration
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
  , windowVariance
  , windowStdDev
    -- * Splitting
  , evictOldest
  , evictOldestN
  , evictBefore
  , trimByMeasure
  , resize
    -- * Conversions
  , toNewestFirst
  , toTimedNewestFirst
  , toOldestFirst
  , toTimedOldestFirst
  ) where

import Data.Window.Internal.Measures
import Data.Window.Internal.Timed
import Data.Window.TimeLike
