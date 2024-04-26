{-# LANGUAGE ExistentialQuantification #-}

-- | Monoids using `.&&.` and `.||.`.
--
-- They satisfy monoid laws with respect to the `isSuccess` unless one is using
-- `checkCoverage` (see test for a counterexample).
--
module Test.QuickCheck.Monoids
  ( All (..)
  , Any (..)
  ) where

import Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Semigroup (..))
import Test.QuickCheck

-- | Conjunction monoid build with `.&&.`.
--
-- Use `property @All` as an accessor which doesn't leak
-- existential variables.
--
data All = forall p. Testable p => All { getAll :: p }

instance Testable All where
    property (All p) = property p

instance Semigroup All where
    All p <> All p' = All (p .&&. p')
    sconcat = All . conjoin . NonEmpty.toList

instance Monoid All where
    mempty = All True
    mconcat = All . conjoin


-- | Disjunction monoid build with `.||.`.
--
-- Use `property @Any` as an accessor which doesn't leak
-- existential variables.
--
data Any = forall p. Testable p => Any { getAny :: p }

instance Testable Any where
    property (Any p) = property p

instance Semigroup Any where
    Any p <> Any p' = Any (p .||. p')
    sconcat = Any . disjoin . NonEmpty.toList

instance Monoid Any where
    mempty = Any False
    mconcat = Any . disjoin
