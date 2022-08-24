-- | Utilities for very randomly generating very small integers.
module AntiDiff.Util.X (X (..)) where

import           Test.QuickCheck



newtype X = X Int
  deriving (Eq, Show, Ord)

instance Arbitrary X where
    arbitrary = X <$> choose (-5, 5)
