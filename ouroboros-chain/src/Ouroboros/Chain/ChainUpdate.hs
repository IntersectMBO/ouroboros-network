{-# LANGUAGE DeriveTraversable #-}
module Ouroboros.Chain.ChainUpdate (
    ChainUpdate (..)
  ) where

import           Ouroboros.Chain.Point

-- | A representation of two actions to update a chain: add a block or roll
-- back to a previous point.
--
-- The type parameter @a@ is there to allow a 'Functor' instance. Typically,
-- it will be instantiated with @block@ itself.
data ChainUpdate block a = AddBlock a
                         | RollBack (Point block)
  deriving (Eq, Show, Functor, Foldable, Traversable)
