{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.Arbitrary () where

import           Data.Time
import           Test.QuickCheck hiding (Fixed (..))

import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Ouroboros.Consensus.Crypto.Hash.Class (Hash,
                     HashAlgorithm (..), hash)
import           Ouroboros.Consensus.Crypto.VRF.Class (VRFAlgorithm (..))
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Util.Random (Seed (..), withSeed)

import           Ouroboros.Storage.ImmutableDB.CumulEpochSizes (EpochSlot (..),
                     RelativeSlot (..))
import           Ouroboros.Storage.ImmutableDB.Types (Epoch (..),
                     EpochSize (..), Slot (..))


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

deriving via FixedUTC      instance Arbitrary SystemStart
deriving via Positive Word instance Arbitrary Slot
deriving via Word          instance Arbitrary Epoch
deriving via Positive Word instance Arbitrary EpochSize
deriving via Word          instance Arbitrary RelativeSlot

instance Arbitrary EpochSlot where
  arbitrary = EpochSlot <$> arbitrary <*> arbitrary
  shrink    = genericShrink

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

instance Arbitrary Seed where

    arbitrary = do  (\w1 w2 w3 w4 w5 -> Seed (w1, w2, w3, w4, w5))
                <$> gen <*> gen <*> gen <*> gen <*> gen
      where
        gen = getLarge <$> arbitrary

    shrink = const []

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where

    arbitrary = do
        seed <- arbitrary
        return $ withSeed seed genKeyDSIGN

    shrink = const []

instance (Serialise a, Arbitrary a, HashAlgorithm h) => Arbitrary (Hash h a) where

    arbitrary = hash <$> arbitrary
    shrink = const []

instance DSIGNAlgorithm v => Arbitrary (VerKeyDSIGN v) where
    arbitrary = deriveVerKeyDSIGN <$> arbitrary
    shrink = const []

instance DSIGNAlgorithm v => Arbitrary (SigDSIGN v) where

    arbitrary = do
        a    <- arbitrary :: Gen Int
        sk   <- arbitrary
        seed <- arbitrary
        return $ withSeed seed $ signDSIGN a sk

    shrink = const []

instance VRFAlgorithm v => Arbitrary (SignKeyVRF v) where

    arbitrary = do
        seed <- arbitrary
        return $ withSeed seed genKeyVRF

    shrink = const []

instance VRFAlgorithm v => Arbitrary (VerKeyVRF v) where
    arbitrary = deriveVerKeyVRF <$> arbitrary
    shrink = const []

instance VRFAlgorithm v => Arbitrary (CertVRF v) where

    arbitrary = do
        a    <- arbitrary :: Gen Int
        sk   <- arbitrary
        seed <- arbitrary
        return $ withSeed seed $ fmap snd $ evalVRF a sk

    shrink = const []

{-------------------------------------------------------------------------------
  Auxiliary: time
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
