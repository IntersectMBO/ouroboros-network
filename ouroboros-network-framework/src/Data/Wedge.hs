{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}

-- | This module is a simplified version of
-- <https://hackage.haskell.org/package/smash/docs/Data-Wedge.html#t:Wedge>,
-- which is copyrighted by Emily Pillmore and originally pulished using
-- BSD-3-Clause license.
--
-- copyright: Emily Pillmore 2020-2021, iohk 2021
--
module Data.Wedge where

import           Control.Monad (ap)

import           Data.Bifunctor
import           Data.Bifoldable
import           Data.Bitraversable

-- | A wedge product
-- <https://hackage.haskell.org/package/smash/docs/Data-Wedge.html#t:Wedge>
--
data Wedge a b =
    Nowhere
  | Here a
  | There b
  deriving (Eq, Ord, Foldable, Functor, Show)

instance Bifunctor Wedge where
    bimap _ _ Nowhere   = Nowhere
    bimap f _ (Here a)  = Here  (f a)
    bimap _ g (There b) = There (g b)

instance Bifoldable Wedge where
    bifoldMap _ _ Nowhere   = mempty
    bifoldMap f _ (Here a)  = f a
    bifoldMap _ g (There b) = g b

instance Bitraversable Wedge where
    bitraverse _ _ Nowhere   = pure Nowhere
    bitraverse f _ (Here a)  = Here <$> f a
    bitraverse _ g (There b) = There <$> g b

instance Applicative (Wedge a) where
    pure  = return
    (<*>) = ap

instance Monad (Wedge a) where
    return = There

    Nowhere >>= _ = Nowhere
    Here a  >>= _ = Here a
    There a >>= f = f a
