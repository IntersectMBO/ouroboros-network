{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}

module Data.SOP.Lenses (
    Lens (..)
  , lenses_NP
  ) where

import           Data.SOP.Strict

-- | Simple lens to access an element of an n-ary product.
data Lens f xs a = Lens {
      getter :: NP f xs -> f a
    , setter :: f a -> NP f xs -> NP f xs
    }

-- | Generate all lenses to access the element of an n-ary product.
lenses_NP :: forall f xs. SListI xs => NP (Lens f xs) xs
lenses_NP = go sList
  where
    go :: SList xs' -> NP (Lens f xs') xs'
    go SNil  = Nil
    go SCons = lensFirst :* hmap shiftLens (go sList)

    lensFirst :: Lens f (x ': xs') x
    lensFirst = Lens {
          getter = hd
        , setter = \a' (_ :* as) -> a' :* as
        }

    shiftLens :: Lens f xs' a -> Lens f (x ': xs') a
    shiftLens l = Lens {
          getter = getter l . tl
        , setter = \a' (a :* as) -> a :* setter l a' as
        }
