module Cabal.Lint.Summygroup (
  -- *
  Summygroup (..),
  -- *
  Always (..),
  Sometimes (..),
  oneAlways,
  oneSometimes,
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

-----

-- | Given a semigroup interpreted here as multiplication, this class adds an additive operator with distributivity.
--
-- > (a <+> b) <+> c = a <+> (b <+> c)
--
-- > (a <+> b) <> x = (a <> x) <+> (b <> x)
--
-- This abstraction is a semiring without the additive identity; we don't think
-- there's an effective established name for that, so we keep the cheeky
-- @Summygroup@.
--
-- This class does not constrain how 'mempty' and @<+>@ relate when @m@ is a 'Monoid'.
-- EG it annihilates for 'Always' and is neutral for 'Sometimes'.
class Semigroup m => Summygroup m where (<+>) :: m -> m -> m

instance Summygroup b => Summygroup (a -> b) where
  f <+> g = \x -> f x <+> g x

instance (Summygroup m, Summygroup n) => Summygroup (m, n) where
  (l1, l2) <+> (r1, r2) = (l1 <+> r1, l2 <+> r2)

-----

-- | A multi-set using the lesser count from either side of '<+>'
--
-- INVARIANT: all elements >0.
newtype Always a = Always {getAlways :: Map a Int}

oneAlways :: a -> Always a
oneAlways x = Always $ Map.singleton x 1

instance Ord a => Monoid     (Always a) where mempty                = Always Map.empty
instance Ord a => Semigroup  (Always a) where Always l <>  Always r = Always $ Map.unionWith (+) l r
instance Ord a => Summygroup (Always a) where Always l <+> Always r = Always $ Map.intersectionWith min l r

-----

oneSometimes :: a -> Sometimes a
oneSometimes x = Sometimes $ Map.singleton x 1

-- | A multi-set using the larger count from either side of '<+>'
--
-- INVARIANT: all elements >0.
newtype Sometimes a = Sometimes {getSometimes :: Map a Int}

instance Ord a => Monoid     (Sometimes a) where mempty                      = Sometimes Map.empty
instance Ord a => Semigroup  (Sometimes a) where Sometimes l <>  Sometimes r = Sometimes $ Map.unionWith (+) l r
instance Ord a => Summygroup (Sometimes a) where Sometimes l <+> Sometimes r = Sometimes $ Map.unionWith max l r
