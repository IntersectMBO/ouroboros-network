{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Monoid.MSet
  ( MSet (..)
  , mact
  , macts
  ) where

-- | Right action of a monoid @m@ on a type @a@.
--
-- > prop a <| m1 <> m2 = a <| m1 <| m2
-- > prop a <| mempty = a
--
class Monoid m => MSet a m where
  (<|) :: a -> m -> a

infixl 5 <|

mact :: MSet a m => a -> m -> a
mact = (<|)

macts :: MSet a m => a -> [m] -> a
macts a []       = a
macts a (m : ms) = (a <| m) `macts` ms

instance Monoid m => MSet m m where
  (<|) = mappend
