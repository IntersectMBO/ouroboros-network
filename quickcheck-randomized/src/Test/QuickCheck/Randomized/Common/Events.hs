{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | General /events/: happenings and\/or things that can be counted and\/or
-- can have a probability, etc.

module Test.QuickCheck.Randomized.Common.Events (
  -- * Events
  Ever,
  FalseResult (FalseFail, FalsePass),
  InRange,
  Trial,
  -- * Property combinators
  negateProperty,
  ) where

import           Test.QuickCheck.Property

data FalseResult
  = FalseFail
    -- ^ Event: The statistical test fails even though the code indeed
    -- satisfies its specification.
    --
    -- NOTE 'FalsePass' may use a less precise specification.
  | FalsePass
    -- ^ Event: The statistical test passes even though the code does not
    -- satisfy its specification.
    --
    -- NOTE 'FalseFail' may use a more precise specification.

-- | Event: The realization of a one-dimensional random variable is in some
-- range.
--
-- NOTE It would be too burdensome to include the range itself in the type, so
-- we settle for the imprecise type.
data InRange (ev :: k)

-- | Event: A trial in which each outcome has a corresponding type value of
-- kind @outcome@. Examples include: tossing a coin, rolling a die, drawing a
-- ball from an urn, drawing a sample from a PRNG, etc.
--
-- NOTE The complement of this event does not seem to have useful semantics.
-- Relatedly, we only use it with 'Count'.
data Trial (outcome :: *)

-- | Event: The event happens at least once during some number of repititions.
data Ever (ev :: k)

{-------------------------------------------------------------------------------
  Property combinators
-------------------------------------------------------------------------------}

-- | Negate the underlying predicate of a 'Property'.
--
-- This is different than 'expectFailure'.
--
-- o 'expectFailure' means " quickly check that the property does not hold ",
--   and so it causes QuickCheck to stop iterating once it falsifies the
--   predicate. That's not what we want here.
--
-- o 'negateProperty' instead means " quickly check that the negation of the
--   property holds ", and so it does /not/ short-circuit the QuickCheck
--   iteration loop.
negateProperty :: Property -> Property
negateProperty =
    MkProperty . fmap (MkProp . fmap neg . unProp) . unProperty
  where
    neg :: Result -> Result
    neg res = res{ok = not <$> ok res}
