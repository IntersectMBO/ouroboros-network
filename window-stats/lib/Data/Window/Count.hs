-- |
-- Module      : Data.Window.Count
-- Description : Count-based sliding window backed by a finger tree.
-- Stability   : experimental
--
-- This module provides the core sliding window data structure, 'Window',
-- which retains a fixed number of the most recently inserted elements.
-- Elements are evicted from the oldest end of the window automatically
-- on insertion once the window reaches its maximum size.
--
-- The window is backed by a finger tree ('Data.FingerTree.FingerTree'),
-- giving O(1) amortised insertion (and on-insert eviction); the bulk
-- eviction primitives ('evictOldestN', 'trimByMeasure', 'resize') run
-- in O(log w). The window measure is a product of an element count
-- and a user-supplied monoid @v@, cached at the root of the finger
-- tree and accessible in O(1) via 'windowMeasure'.
--
-- The user-supplied measure @v@ is updated incrementally as elements
-- are inserted and evicted — no recomputation of the entire window is
-- needed. Combine this module with "Data.Window.DigestCount" from the
-- @with-tdigest@ sublibrary for approximate-quantile statistics, or
-- define your own measure type for other rolling statistics.
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
-- = Example
--
-- @
-- -- Build a window of size 5 tracking a running sum:
-- let samples = [10.0, 9.0 .. 1.0] :: [Double]
--     window  = fromListN 5 samples :: Window (Sum Double) (SumSample Double)
-- in  windowSum window
-- -- => Just 40.0
--
-- let window' = insert (11.0 :: Double) window
-- in  windowSum window'
-- -- => Just 45.0
-- @
--
-- = Integration with other libraries
--
-- For approximate quantile statistics, see "Data.Window.DigestCount".
-- For time-based windowing, see "Data.Window.Timed".
-- For a combination of the above two, see "Data.Window.DigestTimed".
-- For integration with the @foldl@ package, see the @with-foldl@ sublibrary.
--
module Data.Window.Count
  ( -- * Types
    Window
    -- * Construction
  , empty
  , singleton
    -- ** From collections
  , fromListN
  , fromFoldable
    -- * Insertion
  , insert
  , insertMany
    -- * Querying
  , size
  , windowMaxSize
  , isFull
  , isEmpty
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
  , trimByMeasure
  , resize
    -- * Conversions
  , toNewestFirst
  , toOldestFirst
  ) where

import Data.Window.Internal.Count
import Data.Window.Internal.Measures
