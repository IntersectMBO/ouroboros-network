{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.QuickCheck.Randomized.Common.Interval (
  -- * Interval
  Interval (..),
  chooseInIntegralInterval,
  compareToInterval,
  inInterval,
  smallestInInterval,
  smallestInIntervalMaybe,
  ) where

import           Data.Maybe (isJust)
import           GHC.Generics
import           Numeric.Search.Range (searchFromTo)

import           Test.QuickCheck

import           Test.QuickCheck.Randomized.Common.Bounds
import           Test.QuickCheck.Randomized.Common.Validation

-- | A non-empty closed interval.
data Interval a =
    Interval !(MinBound a) !(MaxBound a)
  deriving (Eq, Generic, Ord, Show)

-- | Non-empty, @lo <= hi@
instance (Ord a, Validate a) => Validate (Interval a) where
  validate interval =
      invalidityUnless (lo <= hi) "empty interval!"
      <> validateViaRep interval
    where
      Interval (MinBound lo) (MaxBound hi) = interval

compareToInterval :: Ord a => a -> Interval a -> Ordering
compareToInterval x interval
    | x < lo    = LT
    | x > hi    = GT
    | otherwise = EQ
  where
    Interval (MinBound lo) (MaxBound hi) = interval

-- | Whether @lo <= x && x <= hi@.
inInterval :: Ord a => Interval a -> a -> Bool
inInterval interval x = EQ == compareToInterval x interval

-- | The predicate must be /upward-closed/: if @p k@ then @p ('succ' k)@.
--
-- It's 'Nothing' if the predicate is false for the whole interval.
smallestInInterval ::
    Integral a => Interval a -> (a -> Bool) -> Maybe a
smallestInInterval interval predicate =
    searchFromTo predicate lo hi
  where
    Interval (MinBound lo) (MaxBound hi) = interval

-- | The predicate must be /upward-closed/: if @'isJust' (f k)@ then @'isJust'
-- (f ('succ' k))@.
--
-- It's 'Nothing' if the predicate is false for the whole interval.
smallestInIntervalMaybe ::
    Integral a => Interval a -> (a -> Maybe b) -> Maybe b
smallestInIntervalMaybe interval f =
    (proj . f) <$> searchFromTo (isJust . f) lo hi
  where
    Interval (MinBound lo) (MaxBound hi) = interval

    proj = \case
        Nothing -> error "impossible"
        Just x  -> x

chooseInIntegralInterval :: Integral a => Interval a -> Gen a
chooseInIntegralInterval interval
    | hi < lo   = error msg
    | otherwise = do
        d <- choose (0, toInteger $ hi - lo)
        pure $ lo + fromInteger d
  where
    Interval (MinBound lo) (MaxBound hi) = interval

    msg :: String
    msg =
        "chooseInIntegralInterval: empty " <>
        show (toInteger lo, toInteger hi)
