{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Module      : Data.Window.Internal.Measures
-- Description : Prebuilt sample wrappers and cached measure types.
-- Stability   : internal
--
-- The sample wrappers ('SumSample', 'MinMaxSample', 'MomentSample')
-- and their associated cached-measure types ('MinMaxV',
-- 'WelfordMeasure') used by the prebuilt statistics in
-- 'Data.Window.Count' / 'Data.Window.Timed'. The public modules
-- re-export the sample wrappers (with constructors) and the measure
-- types (opaquely); the measure internals — constructors and field
-- accessors — live here.
--
-- This module is __internal__. Importing it grants the access needed
-- for things like combining 'WelfordMeasure' values from disjoint
-- windows, computing population variance from the running
-- 'welfordM2' / 'welfordN', serialising the cached measure for
-- telemetry, or seeding a window with a precomputed measure. Use at
-- your own risk: field names and representations may change between
-- minor versions without corresponding changes to the stable public
-- API.
--
module Data.Window.Internal.Measures
  ( -- * Sum
    SumSample (..)
    -- * Min/Max
  , MinMaxSample (..)
  , MinMaxV (..)
    -- * Welford moments
  , MomentSample (..)
  , WelfordMeasure (..)
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Data.FingerTree
import Data.Monoid
import GHC.Generics

-- | Wrapper for computing sums over the sliding window
--
newtype SumSample a = SumSample { getSumSample :: a }
  deriving (Generic, Show)
  deriving anyclass NFData
  deriving newtype Num

instance Num a => Measured (Sum a) (SumSample a) where
  {-# INLINE measure #-}
  measure (SumSample s) = Sum s


newtype MinMaxSample a = MinMaxSample { getMinMaxSample :: a }
  deriving (Generic, Show)
  deriving anyclass NFData

-- | Wrapper for capturing min/max over the sliding window
newtype MinMaxV a = MinMaxV { getMinMaxV :: Maybe (a, a) }
  deriving (Eq, Show)

instance Ord a => Semigroup (MinMaxV a) where
  {-# INLINEABLE (<>) #-}
  MinMaxV v1 <> MinMaxV v2 =
    case (v1, v2) of
      (Just (l1, u1), Just (l2, u2)) ->
        let !l = min l1 l2
            !u = max u1 u2
        in MinMaxV $ Just (l, u)
      _otherwise -> MinMaxV $ v1 <|> v2

instance Ord a => Monoid (MinMaxV a) where
  {-# INLINE mempty #-}
  mempty = MinMaxV Nothing

instance Ord a => Measured (MinMaxV a) (MinMaxSample a) where
  {-# INLINE measure #-}
  measure (MinMaxSample s) = MinMaxV $ Just (s, s)


newtype MomentSample a = MomentSample { getMomentSample :: a }
  deriving (Generic, Show)
  deriving anyclass NFData

data WelfordMeasure a = WelfordMeasure
    { welfordN    :: {-# UNPACK #-} !Int
    , welfordMean :: !a
    , welfordM2   :: !a
    }
    deriving (Generic, NFData, Eq, Show)

instance (Fractional a) => Semigroup (WelfordMeasure a) where
    {-# INLINEABLE (<>) #-}
    WelfordMeasure nA mA m2A <> WelfordMeasure nB mB m2B
        | nA == 0   = WelfordMeasure nB mB m2B
        | nB == 0   = WelfordMeasure nA mA m2A
        | otherwise =
            let n     = nA + nB
                delta = mB - mA
                mean  = mA + delta * fromIntegral nB / fromIntegral n
                m2    = m2A + m2B + delta * delta
                            * fromIntegral nA
                            * fromIntegral nB
                            / fromIntegral n
            in  WelfordMeasure n mean m2

instance Fractional a => Monoid (WelfordMeasure a) where
    {-# INLINE mempty #-}
    mempty = WelfordMeasure 0 0 0

instance Fractional a => Measured (WelfordMeasure a) (MomentSample a) where
    {-# INLINE measure #-}
    measure (MomentSample x) = WelfordMeasure 1 x 0
