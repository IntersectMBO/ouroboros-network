{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Test.QuickCheck.Randomized.Coin.Meta (tests) where

import           Data.Type.Coercion (coerceWith)

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.QuickCheck.Randomized
import           Test.QuickCheck.Randomized.Coin (Coin, Face (..))
import qualified Test.QuickCheck.Randomized.Coin as Coin
import           Test.QuickCheck.Randomized.Distribution.Binomial
                     (Binomial (..))

import           Test.Test.QuickCheck.Randomized.Coin.Common
import           Test.Test.QuickCheck.Randomized.Coin.Direct (TestSetup (..))

tests :: TestTree
tests = testGroup "" $
    [ testProperty
        ( "Meta.sanity check: " <>
          "tuneLeniency worstCase is within generator's bound"
        ) $ sanityCheck
    , adjustOption (min (QuickCheckTests 5)) $
      askOption $ \(QuickCheckTests n) ->
      testProperty "rule as a meta coin" (prop_MetaCorrectCoinPass n)
    ]

{-------------------------------------------------------------------------------
  Infrastructure
-------------------------------------------------------------------------------}

-- | A test that treats a rule applied to a coin as another coin itself.
--
-- KEY IDEA Define the outer coin's heads as the inner rule failing on a
-- correct inner coin.
--
-- Thus the 'bias' of the outer coin is the inner rule's actual probability of
-- a 'FalseFail'.
data MetaTestSetup = MetaTestSetup {
    innerAlpha     :: !(MaxBound (Prob FalseFail))
    -- ^ The actual @'Prob' 'FalseFail'@ of the resulting inner rule is the
    -- bias of the outer coin.
  , innerTestSetup :: !TestSetup
  , outerLeniency  :: !(Leniency (Prob Heads))
  }
  deriving (Show)

maxTosses :: MaxBound (Count (Trial Face))
maxTosses = MaxBound 10000

-- | If 'leniency' is this large, then 'worstCase' is guaranteed to be 'Just'.
minOuterLeniency :: MinBound (Leniency (Prob Coin.Heads))
minOuterLeniency = MinBound $ Leniency 0.1

instance Arbitrary MetaTestSetup where
  arbitrary = do
      innerTestSetup <- arbitrary

      innerAlpha <- (MaxBound . Prob) <$> choose (0.05, 0.95)

      -- This is a reasonable default value. Big enough that we should not
      -- require so many tosses. Small enough that bugs are unlikely to be this
      -- subtle.
      let outerLeniency = unMinBound minOuterLeniency

      pure MetaTestSetup {
          innerAlpha
        , innerTestSetup
        , outerLeniency
        }

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

sanityCheck :: Property
sanityCheck =
    once $
    case worstCase of
      Just leniency ->
          counterexample (show (leniency, minOuterLeniency)) $
          leniency <= minOuterLeniency
      Nothing       -> property False

-- | See 'MetaTestSetup'. This property requires that the bias of the outer
-- coin is the inner rule's actual probability of @'Prob' 'FalseFail'@.
prop_MetaCorrectCoinPass :: Int -> MetaTestSetup -> Property
prop_MetaCorrectCoinPass n testSetup =
    ( \prop ->
        counterexample
          "sanity check doesn't apply for n >1e6"
          (n < 10 ^ (6 :: Int)) .&&.
        prop
    ) $
    let MetaTestSetup {
            innerAlpha
          , innerTestSetup
          , outerLeniency
          } = testSetup
        TestSetup {
            bias   = innerBias
          , tosses = innerTosses
          } = innerTestSetup

        innerCoin :: Coin
        innerCoin = Coin.fromBias innerBias

        innerArguments :: Coin.Arguments
        innerArguments = Coin.Arguments {
            alpha    = innerAlpha
          , expected = Binomial {
              bias   = innerBias
            , tosses = innerTosses
            }
          }
    in
    withRule innerArguments $ \innerRule ->
    let -- See KEY IDEA above.
        outerCoin :: Coin
        outerCoin =
            Coin.fromProperty $
            negateProperty $   -- Heads is FalseFail
            Coin.applyRule innerRule innerCoin

        outerAlpha :: MaxBound (Prob FalseFail)
        outerAlpha = mkOuterAlpha n

        outerBeta :: MaxBound (Prob FalsePass)
        outerBeta = mkOuterBeta n

        -- See KEY IDEA above.
        outerBias :: Prob Heads
        outerBias =
            coerceWith unsafeCoerceProb
              (Coin.falseFail innerRule :: Prob FalseFail)

        mbOuterTosses :: Coin.TunedTosses
        mbOuterTosses = Coin.tuneTosses Coin.TossesArguments {
            alpha     = outerAlpha
          , beta      = outerBeta
          , bias      = outerBias
          , leniency  = outerLeniency
          , maxTosses
          }
    in
    case mbOuterTosses of
      Coin.InsufficientMaxTosses                     ->
          -- The 'MetaTestSetup' 'Arbitrary' instance chooses 'outerLeniency'
          -- such that we don't need more than 'maxTosses'. If that's not true,
          -- then the 'worstCase' sanity check would fail.
          counterexample "InsufficientMaxTosses" False
      Coin.OkTosses (MinBound outerTosses) outerRule ->
          tabulate
            "(totalTosses, innerRule, outerRule)"
            [show (innerTosses * outerTosses, innerRule, outerRule)] $
          Coin.applyRule outerRule outerCoin

-- | Used for 'prop_MetaCorrectCoinPass'.
mkOuterAlpha :: Int -> MaxBound (Prob FalseFail)
mkOuterAlpha n =
    MaxBound $ neverProb (Count n) (Prob 1e-9 :: Prob (Ever FalseFail))

-- | Used for 'prop_MetaCorrectCoinPass'.
mkOuterBeta :: Int -> MaxBound (Prob FalsePass)
mkOuterBeta n =
    MaxBound $ neverProb (Count n) (Prob 1e-9 :: Prob (Ever FalsePass))

-- | A reference point used in the design of 'prop_MetaCorrectCoinPass'.
worstCase :: Maybe (MinBound (Leniency (Prob Coin.Heads)))
worstCase = do
    let arguments = Coin.Arguments {
            alpha    = mkOuterAlpha $ 10 ^ (6 :: Int)
          , expected = Binomial {
              bias   = Prob 0.5   -- fair coin maximizes tuned leniency
            , tosses = unMaxBound maxTosses
            }
          }
    rule <- Coin.mkRule arguments

    let hyper = Coin.tuneLeniency Coin.LeniencyArguments {
            beta         = mkOuterBeta $ 10 ^ (6 :: Int)
          , leniencyGrid = Count $ 10 ^ (6 :: Int)
          , rule
          }
    case hyper of
      Coin.DegenerateLeniency  -> Nothing
      Coin.OkLeniency leniency -> Just leniency
