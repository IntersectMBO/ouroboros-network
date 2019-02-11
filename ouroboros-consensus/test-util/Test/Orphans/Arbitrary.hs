{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Orphans.Arbitrary () where

import           Data.Time
import           Test.QuickCheck hiding (Fixed (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo

minNumCoreNodes, minNumSlots :: Int
minNumCoreNodes = 2
minNumSlots     = 1

instance Arbitrary NumCoreNodes where
  arbitrary = NumCoreNodes <$> choose (minNumCoreNodes, 5)
  shrink (NumCoreNodes n) = NumCoreNodes <$> (filter (>= minNumCoreNodes) $ shrink n)

-- TODO: We shouldn't really pick the number of slots independent from k
instance Arbitrary NumSlots where
  arbitrary = NumSlots <$> choose (minNumSlots, 100)
  shrink (NumSlots n) = NumSlots <$> (filter (>= minNumSlots) $ shrink n)

-- | Picks time span between 0 seconds and (roughly) 50 years
instance Arbitrary NominalDiffTime where
  arbitrary = conv <$> choose (0, 50 * daysPerYear * secondsPerDay)
    where
      conv :: Double -> NominalDiffTime
      conv = realToFrac

-- | Picks moment between 'dawnOfTime' and (roughly) 50 years later
--
-- Uses instance for 'NominalDiffTime'
instance Arbitrary UTCTime where
  arbitrary = (`addUTCTime` dawnOfTime) <$> arbitrary

-- | Defined in terms of instance for 'NominalDiffTime'
instance Arbitrary FixedDiffTime where
   arbitrary = fixedDiffFromNominal <$> arbitrary

-- | Defined in terms of instance for 'UTCTime'
instance Arbitrary FixedUTC where
  arbitrary = fixedFromUTC <$> arbitrary

-- | Length between 0.001 and 20 seconds, millisecond granularity
instance Arbitrary SlotLength where
  arbitrary = slotLengthFromMillisec <$> choose (1, 20 * 1_000)

-- | Defined in terms of 'FixedUTC'
instance Arbitrary SystemStart where
  arbitrary = SystemStart <$> arbitrary

instance Arbitrary Slot where
  arbitrary = Slot . getPositive <$> arbitrary

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Average number of days per year
--
-- <https://en.wikipedia.org/wiki/Year>
daysPerYear :: Double
daysPerYear = 365.2425

-- | Seconds per day
secondsPerDay :: Double
secondsPerDay = 24 * 60 * 60

-- | Dawn of time
--
-- Everybody knows nothing happened before 2000-01-01 00:00:00
dawnOfTime :: UTCTime
dawnOfTime = read "2000-01-01 00:00:00"
