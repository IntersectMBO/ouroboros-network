{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.MonadTimer
  ( tests
  ) where

import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadTimer
import           GHC.Real

import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup "Control.Monad.Class.MonadTimer"
    [ testProperty "diffTimeToMicroseconds left inverse"
        prop_diffTimeToMicrosecondsAsIntLeftInverse
    , testProperty "diffTimeToMicroseconds right inverse"
        prop_diffTimeToMicrosecondsAsIntRightInverse
    ]

newtype IntDistr = IntDistr Int
    deriving (Show, Eq)

instance Arbitrary IntDistr where
    arbitrary = oneof
      [ IntDistr <$> arbitrary
      , IntDistr . (maxBound - ) . getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
      , IntDistr . (minBound + ) . getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
      ]

    shrink (IntDistr a) = IntDistr `map` shrink a

prop_diffTimeToMicrosecondsAsIntLeftInverse :: IntDistr -> Bool
prop_diffTimeToMicrosecondsAsIntLeftInverse (IntDistr usec) =
    usec == diffTimeToMicrosecondsAsInt (microsecondsAsIntToDiffTime usec)


newtype DiffTimeDistr = DiffTimeDistr DiffTime
    deriving (Show, Eq)

instance Arbitrary DiffTimeDistr where
    arbitrary = frequency
        [ -- arbitrary DiffTime
          (6, DiffTimeDistr . fromRational <$> arbitrary)
          -- large positive DiffTimes, but smaller than `maxBound :: Int` microseconds
        , (3, DiffTimeDistr
               . (fromRational (toRational (maxBound :: Int) / 1_000_000) - )
               . fromRational
               . getNonNegative
              <$> resize 100 arbitrary
          )
          -- large negative DiffTimes, but larger than `minBound :: Int` microseconds
        , (3, DiffTimeDistr
               . (fromRational (toRational (minBound :: Int) / 1_000_000) + )
               . fromRational
               . getNonNegative
              <$> arbitrary
          )
          -- smaller than 1 :% 1_000_000
        , (1, DiffTimeDistr . fromRational . (/ 1_000_000) <$> resize 1 arbitrary)
        ]

    shrink (DiffTimeDistr a) = (DiffTimeDistr . fromRational) `map` shrink (toRational a)

prop_diffTimeToMicrosecondsAsIntRightInverse :: DiffTimeDistr -> Property
prop_diffTimeToMicrosecondsAsIntRightInverse (DiffTimeDistr a) = 
    label (labelRational (toRational a)) $
      abs (toRational a - a') < (1 :% 1_000_000)
      .&&.
      r === microsecondsAsIntToDiffTime (diffTimeToMicrosecondsAsInt r)


  where
    a' = toRational (microsecondsAsIntToDiffTime (diffTimeToMicrosecondsAsInt a))

    -- 'a' rounded to microseconds
    r :: DiffTime
    r = fromRational (toRational x / 1_000_000)
      where
        x :: Integer
        x = round $ (toRational a * 1_000_000)

    labelRational x =
      if | abs x < 1 :% 1_000_000
         -> "small"
         | abs x > toRational (maxBound :: Int) / 1_000_000 - 100
         -> "large"
         | otherwise
         -> "average"
