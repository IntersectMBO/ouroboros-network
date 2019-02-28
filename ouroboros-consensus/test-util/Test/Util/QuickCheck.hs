module Test.Util.QuickCheck (
    -- * Natural numbers
    genNatBetween
  , genNat
  , shrinkNat
  ) where

import           Numeric.Natural (Natural)
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Natural numbers
-------------------------------------------------------------------------------}

genNatBetween :: Natural -> Natural -> Gen Natural
genNatBetween from to = do
    i <- choose (toInteger from, toInteger to)
    return $ fromIntegral i

genNat :: Gen Natural
genNat = do
    NonNegative i <- arbitrary :: Gen (NonNegative Integer)
    return $ fromIntegral i

shrinkNat :: Natural -> [Natural]
shrinkNat = map fromIntegral . shrink . toInteger
