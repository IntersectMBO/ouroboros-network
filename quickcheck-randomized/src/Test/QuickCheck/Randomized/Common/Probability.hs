{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.QuickCheck.Randomized.Common.Probability (
  -- * Probability
  Prob_ (..),
  Prob,
  ProbNot,
  clipProb,
  complementProb,
  neverProb,
  tradeProb,
  unsafeCoerceProb,
  ) where

import           Data.Proxy
import           Data.Type.Coercion
import           GHC.Generics
import           Quiet (Quiet (..))

import           Test.QuickCheck

import           Test.QuickCheck.Randomized.Common.Complements
import           Test.QuickCheck.Randomized.Common.Count
import           Test.QuickCheck.Randomized.Common.Events
import           Test.QuickCheck.Randomized.Common.Validation

{-------------------------------------------------------------------------------
  Key values
-------------------------------------------------------------------------------}

-- | A probability, @0 <= p <= 1@.
--
-- The phantom arguments indicate the corresponding event, including whether or
-- not it is complemented.
newtype Prob_ (pol :: Bool) (ev :: k) = Prob {unProb :: Double}   -- TODO Rational? Fixed?
  deriving stock (Generic)
  deriving newtype (Eq, Fractional, Num, Ord)
  deriving (Show) via Quiet (Prob_ pol ev)

instance Arbitrary (Prob_ pol ev) where
  arbitrary = Prob <$> choose (0, 1)

-- | Unit interval, @0 <= p <= 1@
instance Validate (Prob_ pol ev) where
  validate (Prob p) =
      invalidityUnless (0 <= p && p <= 1 ) $
      "Invalid probability! " <> show p <> " is outside the unit interval."

-- | The event is not complemented; @Pr[ ev ]@.
type Prob    = Prob_ True

-- | The event is complemented; @Pr[ not ev ]@.
type ProbNot = Prob_ False

-- | In some context, two events may be definitionally equivalent. This
-- coercion is thus crucial for expresiveness, but also error prone.
--
-- Use @'coerceWith' 'unsafeCoerceProb'@ instead of @'Prob' . 'unProb'@ to make
-- such definitions explicit.
unsafeCoerceProb :: Prob_ pol1 (ev1 :: k1) `Coercion` Prob_ pol2 (ev2 :: k2)
unsafeCoerceProb = Coercion

-- | Complement a probability.
--
-- NOTE Avoid complements of small probabilites when possible:
-- @'complementProb'@ loses floating-point precision.
complementProb :: forall pol1 pol2 k ev.
    Complements pol1 pol2 => Prob_ pol1 (ev :: k) -> Prob_ pol2 ev
complementProb (Prob p) =
    Prob $ 1 - p
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)

-- | A 'Prob_' of some event is also a 'Prob_' of the complement of its
-- complement.
--
-- EG @'Prob' ev ``Coercion`` 'ProbNot' ('Not' ev)@
tradeProb :: forall pol1 pol2 k ev1 ev2.
     (Complements pol1 pol2, Complements ev1 ev2)
  => Prob_ pol1 (ev1 :: k) `Coercion` Prob_ pol2 ev2
tradeProb =
    Coercion
  where
    _ = complements (Proxy :: Proxy pol1) (Proxy :: Proxy pol2)
    _ = complements (Proxy :: Proxy ev1)  (Proxy :: Proxy ev2)

-- | Clip a probability to the unit interval. Used to tidy-up after adjusting a
-- probability (cf 'Test.QuickCheck.Randomized.Bounds.Leniency').
clipProb :: Prob_ pol (ev :: k) -> Prob_ pol ev
clipProb (Prob p) = Prob $ max 0 $ min 1 p

-- | Compute the probability of an event happening in each independent
-- repetition, as required by the probability for the event ever happening.
--
-- TODO For very small @p@, how different is this from @p / n@?
neverProb :: Count (Trial k) -> Prob (Ever ev) -> Prob ev
neverProb (Count n) (Prob ever) =
    Prob each
  where
    -- Solve ever = 1 - (1 - each) ^ n for each.

    -- ever = 1 - (1 - each) ^ n
    -- 1 - ever = (1 - each) ^ n
    -- (1 - ever) ** (1 / n) = 1 - each
    --  = each

    each = 1 - (1 - ever) ** (1 / fromIntegral n)
