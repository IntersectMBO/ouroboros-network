{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Util.SOP (
    -- * Minor variations on standard SOP operators
    sequence_NS'
  , map_NP'
  , partition_NS
  , Lens(..)
  , lenses_NP
    -- * Type-level non-empty lists
  , IsNonEmpty(..)
  , ProofNonEmpty(..)
  ) where

import           Data.SOP.Strict

{-------------------------------------------------------------------------------
  Minor variations on standard SOP operators
-------------------------------------------------------------------------------}

-- | Version of 'sequence_NS' that requires only 'Functor'
--
-- The version in the library requires 'Applicative', which is unnecessary.
sequence_NS' :: forall xs f g. Functor f
             => NS (f :.: g) xs -> f (NS g xs)
sequence_NS' = go
  where
    go :: NS (f :.: g) xs' -> f (NS g xs')
    go (Z (Comp fx)) = Z <$> fx
    go (S r)         = S <$> go r

-- | Version of 'map_NP' that does not require a singleton
map_NP' :: forall f g xs. (forall a. f a -> g a) -> NP f xs -> NP g xs
map_NP' f = go
  where
    go :: NP f xs' -> NP g xs'
    go Nil       = Nil
    go (x :* xs) = f x :* go xs

partition_NS :: forall xs f. SListI xs => [NS f xs] -> NP ([] :.: f) xs
partition_NS =
      foldr (hzipWith append) (hpure nil)
    . map (hexpand nil . hmap singleton)
  where
    nil :: ([] :.: f) a
    nil = Comp []

    singleton :: f a -> ([] :.: f) a
    singleton = Comp . (:[])

    append :: ([] :.: f) a -> ([] :.: f) a -> ([] :.: f) a
    append (Comp as) (Comp as') = Comp (as ++ as')

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

{-------------------------------------------------------------------------------
  Type-level non-empty lists
-------------------------------------------------------------------------------}

data ProofNonEmpty :: [*] -> * where
  ProofNonEmpty :: Proxy x -> ProofNonEmpty (x ': xs)

class IsNonEmpty xs where
  isNonEmpty :: proxy xs -> ProofNonEmpty xs

instance IsNonEmpty (x ': xs) where
  isNonEmpty _ = ProofNonEmpty (Proxy @x)
