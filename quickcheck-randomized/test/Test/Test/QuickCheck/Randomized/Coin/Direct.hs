{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Test.QuickCheck.Randomized.Coin.Direct (
  TestSetup (..),
  tests,
  ) where

import           Data.Functor ((<&>))

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.QuickCheck.Randomized
import           Test.QuickCheck.Randomized.Coin (Face (..))
import qualified Test.QuickCheck.Randomized.Coin as Coin
import           Test.QuickCheck.Randomized.Distribution.Binomial
                     (Binomial (..))

import           Test.Test.QuickCheck.Randomized.Coin.Common

tests :: TestTree
tests = testGroup ""
    [ testProperty
        "Direct.sanity check: tuneLeniency worstCase is non-degenerate" $
      once $
      case Coin.tuneLeniency <$> worstCase of
        Just (Coin.OkLeniency leniency) ->
            leniency === MinBound (Leniency 0.491118)
        _                               -> property False
    , askOption $ \(QuickCheckTests n) ->
      testProperty "correct coin passes" (prop_CorrectCoinPass n)
    , askOption $ \(QuickCheckTests n) ->
      testProperty "incorrect coin fails" (prop_IncorrectCoinFail n)
    ]

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

data TestSetup = TestSetup {
    bias   :: !(Prob Heads)
  , tosses :: !(Count (Trial Face))
  }
  deriving (Show)

minTosses :: MinBound (Count (Trial Face))
minTosses = MinBound 100

maxTosses :: MaxBound (Count (Trial Face))
maxTosses = MaxBound 10000

instance Arbitrary TestSetup where
  arbitrary = do
    bias   <- arbitrary
    tosses <- chooseInIntegralInterval $ Interval minTosses maxTosses
    pure $ TestSetup {
        bias
      , tosses
      }

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | This property applies a very conservative rule to a correct coin and
-- requires it to always pass.
prop_CorrectCoinPass :: Int -> TestSetup -> Property
prop_CorrectCoinPass n testSetup =
    withRule arguments $ \rule ->
    Coin.applyRule rule (Coin.fromBias bias)
  where
    TestSetup {
        bias
      , tosses
      } = testSetup

    alpha :: MaxBound (Prob FalseFail)
    alpha = MaxBound $ neverProb (Count n) (Prob 1e-9 :: Prob (Ever FalseFail))

    arguments :: Coin.Arguments
    arguments = Coin.Arguments {
        -- aka "one in a billion, even despite so many repetitions per
        -- invocation of the test suite"
        alpha
      , expected = Binomial {
          bias
        , tosses
        }
      }

-- | This property applies a very conservative rule to a coin with a bias that
-- is outside of the leniency region and requires it to always fail.
prop_IncorrectCoinFail :: Int -> TestSetup -> Property
prop_IncorrectCoinFail n testSetup =
    ( \prop ->
        counterexample
          "sanity check doesn't apply for n>1e6"
          (n <= 10 ^ (6 :: Int)) .&&.
        prop
    ) $
    withRule arguments $ \rule ->
    withValid (leniencyArguments rule) $
    case Coin.tuneLeniency (leniencyArguments rule) of
      Coin.DegenerateLeniency  ->
          -- This is impossible because even 'worstCase' induces a leniency of
          -- @0.491118@. If this test somehow surpasses 'worstCase', then the
          -- guard atop this 'Property' should fail before this case happens.
          counterexample
            ( "Impossible! The " <> show (leniencyArguments rule) <>
              " induce a degenerate leniency."
            )
            False
      Coin.OkLeniency leniency ->
          counterexample (show leniency) $
          withBiasBeyondLeniency bias (unMinBound leniency) $ \bias' ->
          negateProperty $
          Coin.applyRule rule (Coin.fromBias bias')
  where
    TestSetup {
        bias
      , tosses
      } = testSetup

    -- IE "one in a billion, even despite so many test cases, each testing a
    -- coin"
    beta :: MaxBound (Prob FalsePass)
    beta  = MaxBound $ neverProb (Count n) (Prob 1e-9 :: Prob (Ever FalsePass))

    leniencyArguments rule = Coin.LeniencyArguments {
        beta
      , leniencyGrid = Count $ 10 ^ (5 :: Int)
      , rule
      }

    arguments :: Coin.Arguments
    arguments = Coin.Arguments {
        -- Because we use a coin with a bias beyond the leniency, @alpha@ is
        -- only relevant to this 'Property' in so much as it affects the size
        -- of the 'Coin.Rule''s @interval@.
        alpha    = worstCaseAlpha
      , expected = Binomial {
          bias
        , tosses
        }
      }

-- | A reference point used in the design of 'prop_IncorrectCoinFail'.
worstCaseAlpha :: MaxBound (Prob FalseFail)
worstCaseAlpha = MaxBound $ neverProb (Count (10 ^ (6 :: Int))) 1e-6

-- | A reference point used in the design of 'prop_IncorrectCoinFail'.
worstCaseBeta :: MaxBound (Prob FalsePass)
worstCaseBeta = MaxBound $ neverProb (Count (10 ^ (6 :: Int))) 1e-9

-- | A reference point used in the design of 'prop_IncorrectCoinFail'.
worstCase :: Maybe Coin.LeniencyArguments
worstCase =
    mbRule <&> \rule -> Coin.LeniencyArguments {
        beta         = worstCaseBeta
      , leniencyGrid = Count $ 10 ^ (6 :: Int)
      , rule
      }
  where
    mbRule = Coin.mkRule Coin.Arguments {
        alpha    = worstCaseAlpha
      , expected = Binomial {
          bias   = Prob 0.5   -- fair coin maximizes tuned leniency
        , tosses = unMinBound minTosses
        }
      }
