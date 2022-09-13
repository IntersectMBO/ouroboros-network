-- | Typeclasses for Groupoid-like structures.
module Data.Semigroupoid (
    -- * @'Semigroupoid'@ (partial @'Semigroup'@)
    Semigroupoid (..)
  , pappend
  , pappendM
  , pappendUnsafe
    -- * @'Groupoid'@ (partial @'Group'@)
  , Groupoid (..)
  ) where

import           Control.Monad
import           GHC.Stack (HasCallStack)

{------------------------------------------------------------------------------
  @'Semigroupoid'@ (partial @'Semigroup'@)
------------------------------------------------------------------------------}

infix 6 `pappend`
infix 6 <>?

-- | A semigroup with a partial operator
--
-- The following laws are from
-- <https://en.wikipedia.org/wiki/Groupoid#Algebraic>, with @*@ representing
-- 'poperatorM'.
--
-- /Associativity/ If @Just x ∗ Just y@ and @Just y ∗ Just z@ are defined, then
-- @(Just x ∗ Just y) ∗ Just z@ and @Just x ∗ (Just y ∗ Just z)@ are defined and
-- are equal. Conversely, if one of @(Just x ∗ Just y) ∗ Just z@ and @Just x ∗
-- (Just y ∗ Just z)@ is defined, then so are both @Just x ∗ Just y@ and @Just y
-- ∗ Just z@ as well as @(Just x ∗ Just y) ∗ Just z = Just x ∗ (Just y ∗ Just
-- z)@.
class Semigroupoid a where (<>?) :: a -> a -> Maybe a

pappend :: Semigroupoid a => a -> a -> Maybe a
pappend = (<>?)

-- | 'pappend' lifted as a binary operator on a @Maybe@ monad.
pappendM :: Semigroupoid a => Maybe a -> Maybe a -> Maybe a
pappendM l r = join $ (<>?) <$> l <*> r

pappendUnsafe :: (Semigroupoid a, HasCallStack) => a -> a -> a
pappendUnsafe l r = case l <>? r of
    Nothing -> error "bad poperatorUnsafe"
    Just x  -> x

{------------------------------------------------------------------------------
  @'Groupoid'@ (partial @'Group'@)
------------------------------------------------------------------------------}

-- | A group with a partial operator
--
-- The following laws are from
-- <https://en.wikipedia.org/wiki/Groupoid#Algebraic>, with @*@ representing
-- 'poperatorM'.
--
-- /Inverse/ @Just ('pinv' x) ∗ Just x@ and @Just x ∗ Just ('pinv' x)@ are
-- always defined.
--
-- /Identity/ If @x ∗ y@ is defined, then @Just x ∗ Just y ∗ Just ('pinv' y) =
-- Just x@ and @Just ('pinv' x) ∗ Just x ∗ Just y = Just y@. (The previous two
-- axioms already show that these expressions are defined and unambiguous.)
class Semigroupoid a => Groupoid a where pinv :: a -> a
