{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Semigroupoid.Auto (Auto (..)) where

import           Data.Group (Group (..))

import           Test.QuickCheck

import           Data.Semigroupoid

-- | Automatically derive type class instances for groupoid-like structures
-- from type class instances for group-like structures.
--
-- This is a consequence of groupoid-like structures being generalised versions
-- of group-like structures:
-- * A semigroup is a semigroupoid.
-- * A group is a groupoid.
newtype Auto a = Auto a
  deriving stock (Show, Eq, Ord)
  deriving newtype (Semigroup, Monoid, Group)

instance Semigroup (Auto a) => Semigroupoid (Auto a) where
  x <>? y = Just $ x <> y

instance Group (Auto a) => Groupoid (Auto a) where
  pinv = invert

deriving newtype instance Arbitrary a => Arbitrary (Auto a)
