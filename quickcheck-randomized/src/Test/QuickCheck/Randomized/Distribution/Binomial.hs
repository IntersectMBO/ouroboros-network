{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Key functions for the binomial distribution.
--
-- Import this module qualified.

module Test.QuickCheck.Randomized.Distribution.Binomial (
  Binomial (..),
  cdf,
  intervalCdf,
  pmf,
  quantile,
  support,
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Type.Coercion (coerceWith)
import           GHC.Generics

import qualified Statistics.Distribution as Stats
import qualified Statistics.Distribution.Binomial as Stats

import           Test.QuickCheck.Randomized.Common

-- | The parameters of the common @Binomial(n, p)@ notation.
data Binomial (ev :: k) = Binomial {
    bias   :: !(Prob ev)
    -- ^ @p@
  , tosses :: !(Count (Trial k))
    -- ^ @n@
  }
  deriving (Generic, Show, Validate)

-- | The /support/ of the distribution, all values with positive probability.
support :: Binomial ev -> Interval (Count ev)
support d =
    MinBound 0 `Interval` MaxBound (coerceWith unsafeCoerceCount tosses)
  where
    Binomial {
        tosses
      } = d

-- | A more precise type for @statistics@'s 'Stats.probability'.
--
-- INVARIANT @'pmf' d k = Pr[ X = k | X ~ d ]@
pmf :: forall k (ev :: k).
    Binomial ev -> Count ev -> Prob (InRange ev)
pmf d (Count k) =
    Prob $ Stats.probability (Stats.binomial n p) k
  where
    Binomial {
        bias
      , tosses
      } = d

    Count n = tosses
    Prob  p = bias

-- | A more precise type for @statistics@'s 'Stats.cumulative'.
--
-- INVARIANT @'cdf' d k = Pr[ X <= k | X ~ d ]@
--
-- INVARIANT @'unMinBound' q <= 'cdf' d ('unMinBound' $ 'quantile' d q)@
cdf :: forall k (ev :: k).
    Binomial ev -> MaxBound (Count ev) -> Prob (InRange ev)
cdf d (MaxBound (Count k)) =
    Prob $ Stats.cumulative (Stats.binomial n p) $ fromIntegral k
  where
    Binomial {
        bias
      , tosses
      } = d

    Count n = tosses
    Prob  p = bias

-- | INVARIANT @'unMinBound' q <= 'cdf' d ('unMinBound' $ 'quantile' d q)@
--
-- The @statistics@ library only defines @quantile@ for continuous
-- distributions. We recover it for the (discrete) Binomial distribution from
-- the monotonic 'cdf' via binary search.
quantile :: forall k (ev :: k).
     Binomial ev
  -> MinBound (Prob (InRange ev))
  -> MinBound (Count ev)
quantile d q =
    MinBound $ fromMaybe (unMaxBound hi) $
    smallestInInterval interval predicate
  where
    Binomial {
        tosses
      } = d

    interval :: Interval (Count ev)
    interval = MinBound 0 `Interval` hi

    hi :: MaxBound (Count ev)
    hi = MaxBound $ coerceWith unsafeCoerceCount tosses

    -- Upward-closed because cdf is monotonic and @(>= q)@ is upward-closed.
    predicate :: Count ev -> Bool
    predicate k = cdf d (MaxBound k) >= unMinBound q

-- | A generalization of 'cdf'.
--
-- INVARIANT @'intervalCdf' d ('Interval' ('MinBound' lo) ('MaxBound' hi)) = Pr[ lo <= X <= hi | X ~ d ]@
--
-- Both 'cdf' and 'intervalCdf' are definite integrals of the probability mass
-- function; 'cdf' only considers a lower-bound of 0.
--
-- INVARIANT @'cdf' d k = 'intervalCdf' d ('Interval' ('MinBound' 0) ('MaxBound' k))@
intervalCdf :: forall k (ev :: k).
     Binomial (ev :: k)
  -> Interval (Count ev)
  -> Prob (InRange ev)
intervalCdf d (Interval lo hi) =
    lowEnough - tooLow   -- TODO improve numerical stability?
  where
    lowEnough :: Prob (InRange ev)
    lowEnough = cdf d hi

    -- NOTE Underflow prevented by the if-expression.
    tooLow :: Prob (InRange ev)
    tooLow =
        if MinBound 0 == lo then 0 else cdf d $ MaxBound $ unMinBound lo - 1
