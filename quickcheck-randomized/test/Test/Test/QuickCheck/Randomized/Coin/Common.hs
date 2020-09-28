{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Test.QuickCheck.Randomized.Coin.Common (
  withBiasBeyondLeniency,
  withRule,
  ) where

import           Test.QuickCheck

import           Test.QuickCheck.Randomized
import           Test.QuickCheck.Randomized.Coin (Face (..))
import qualified Test.QuickCheck.Randomized.Coin as Coin

-- | A 'classify' wrapper around 'Coin.withRule'.
withRule :: Coin.Arguments -> (Coin.Rule -> Property) -> Property
withRule arguments k =
    Coin.withRule arguments $ \rule ->
    classify (Coin.isActuallyTwoSided rule) "two-sided" $
    k rule

-- | Generate a bias that is /strictly outside/ of the leniency range.
withBiasBeyondLeniency ::
     Prob Heads
  -> Leniency (Prob Heads)
  -> (Prob Heads -> Property)
  -> Property
withBiasBeyondLeniency bias leniency k =
    withValid (bias, leniency) $
    counterexample "Degenerate leniency" (0 < lo || hi < 1) .&&.
    forAll gen k
  where
    lo = clipProb $ bias - unLeniency leniency
    hi = clipProb $ bias + unLeniency leniency

    gen :: Gen (Prob Heads)
    gen = raw `suchThat` predicate

    raw :: Gen (Prob Heads)
    raw = Prob <$> oneof [choose (0, unProb lo), choose (unProb hi, 1)]

    predicate :: Prob Heads -> Bool
    predicate p = p < lo || hi < p
