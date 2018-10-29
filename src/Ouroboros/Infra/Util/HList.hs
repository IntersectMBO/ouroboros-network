{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Heterogeneous lists
--
-- Intended for qualified import
module Ouroboros.Infra.Util.HList (
    -- * Basic definitions
    HList(..)
  , All
    -- * Folding
  , foldl
  , foldr
  , repeatedly
  , collapse
    -- * Singletons
  , SList
  , IsList(..)
    -- * n-ary functions
  , Fn
  , applyFn
  , afterFn
  ) where

import Prelude hiding (foldl, foldr)
import Data.Kind (Constraint)
import Data.Proxy

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data HList :: [*] -> * where
  Nil  :: HList '[]
  (:*) :: a -> HList as -> HList (a ': as)

infixr :*

type family All c as :: Constraint where
  All c '[]       = ()
  All c (a ': as) = (c a, All c as)

instance All Show as => Show (HList as) where
  show = show . collapse (Proxy @Show) show

{-------------------------------------------------------------------------------
  Folding
-------------------------------------------------------------------------------}

foldl :: forall c as b proxy. All c as
      => proxy c
      -> (forall a. c a => b -> a -> b) -> b -> HList as -> b
foldl _ f = go
  where
    go :: All c as' => b -> HList as' -> b
    go !acc Nil       = acc
    go !acc (a :* as) = go (f acc a) as

foldr :: forall c as b proxy. All c as
      => proxy c
      -> (forall a. c a => a -> b -> b) -> b -> HList as -> b
foldr _ f e = go
  where
    go :: All c as' => HList as' -> b
    go Nil       = e
    go (a :* as) = f a (go as)

-- | Apply function repeatedly for all elements of the list
--
-- > repeatedly p = flip . foldl p . flip
repeatedly :: forall c as b proxy. All c as
           => proxy c
           -> (forall a. c a => a -> b -> b) -> (HList as -> b -> b)
repeatedly p f as e = foldl p (\b a -> f a b) e as

collapse :: forall c as b proxy. All c as
         => proxy c
         -> (forall a. c a => a -> b) -> HList as -> [b]
collapse _ f = go
  where
    go :: All c as' => HList as' -> [b]
    go Nil       = []
    go (a :* as) = f a : go as

{-------------------------------------------------------------------------------
  Singleton for HList
-------------------------------------------------------------------------------}

data SList :: [*] -> * where
  SNil :: SList '[]
  SCons :: SList as -> SList (a ': as)

class IsList (xs :: [*]) where
  isList :: SList xs

instance              IsList '[]       where isList = SNil
instance IsList as => IsList (a ': as) where isList = SCons isList

{-------------------------------------------------------------------------------
  n-ary functions
-------------------------------------------------------------------------------}

type family Fn as b where
  Fn '[]       b = b
  Fn (a ': as) b = a -> Fn as b

withArgs :: HList as -> Fn as b -> b
withArgs Nil       b = b
withArgs (a :* as) f = withArgs as (f a)

applyFn :: Fn as b -> HList as -> b
applyFn = flip withArgs

afterFn :: SList as -> (b -> c) -> Fn as b -> Fn as c
afterFn SNil       g b = g b
afterFn (SCons ss) g f = afterFn ss g . f
