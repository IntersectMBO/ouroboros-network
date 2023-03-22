{-# LANGUAGE TypeApplications #-}
-- | Seed used for the ThreadNet tests
module Test.ThreadNet.Util.Seed (
    Seed (..)
  , combineWith
  , runGen
  ) where

import           Data.Bits (xor)
import           Data.Coerce (coerce)
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random (mkQCGen)

newtype Seed = Seed Int
  deriving (Eq, Show)

instance Semigroup Seed where
  (<>) = coerce (xor @Int)

combineWith :: Integral a => Seed -> a -> Seed
combineWith seed x = seed <> Seed (fromIntegral x)

runGen :: Seed -> Gen a -> a
runGen (Seed seed) g =
    unGen g qcSeed qcSize
  where
    -- The traditional initial QC size
    qcSize = 30 :: Int
    qcSeed = mkQCGen seed

instance Arbitrary Seed where
  arbitrary = Seed <$> choose (minBound, maxBound)
