{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Heterogeneous lists
--
-- Intended for qualified import
module Ouroboros.Consensus.Util.HList (
    -- * Basic definitions
    All
  , HList (..)
    -- * Folding
  , collapse
  , foldMap
  , foldl
  , foldlM
  , foldr
  , repeatedly
  , repeatedlyM
    -- * Singletons
  , IsList (..)
  , SList
    -- * n-ary functions
  , Fn
  , afterFn
  , applyFn
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Proxy
import           Prelude hiding (foldMap, foldl, foldr)

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

data HList :: [Type] -> Type where
  Nil  :: HList '[]
  (:*) :: a -> HList as -> HList (a ': as)

infixr :*

type family All c as :: Constraint where
  All c '[]       = ()
  All c (a ': as) = (c a, All c as)

instance All Show as => Show (HList as) where
  show = show . collapse (Proxy @Show) show

instance (IsList as, All Eq as) => Eq (HList as) where
    (==) = eq isList
      where
        eq :: All Eq bs => SList bs -> HList bs -> HList bs -> Bool
        eq SNil      _         _         = True
        eq (SCons s) (x :* xs) (y :* ys) = x == y && eq s xs ys

instance (IsList as, All Eq as, All Ord as) => Ord (HList as) where
    compare = cmp isList
      where
        cmp :: All Ord bs => SList bs -> HList bs -> HList bs -> Ordering
        cmp SNil      _         _         = EQ
        cmp (SCons s) (x :* xs) (y :* ys) = compare x y <> cmp s xs ys

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

foldlM :: forall c as m b proxy. (All c as, Monad m)
       => proxy c
       -> (forall a. c a => b -> a -> m b) -> b -> HList as -> m b
foldlM _ f = go
  where
    go :: All c as' => b -> HList as' -> m b
    go !acc Nil       = return acc
    go !acc (a :* as) = f acc a >>= \acc' -> go acc' as

foldr :: forall c as b proxy. All c as
      => proxy c
      -> (forall a. c a => a -> b -> b) -> b -> HList as -> b
foldr _ f e = go
  where
    go :: All c as' => HList as' -> b
    go Nil       = e
    go (a :* as) = f a (go as)

foldMap :: forall c as b proxy. (All c as, Monoid b)
        => proxy c
        -> (forall a. c a => a -> b)
        -> HList as
        -> b
foldMap p f = foldl p (\b a -> b <> f a) mempty

-- | Apply function repeatedly for all elements of the list
--
-- > repeatedly p = flip . foldl p . flip
repeatedly :: forall c as b proxy. All c as
           => proxy c
           -> (forall a. c a => a -> b -> b) -> (HList as -> b -> b)
repeatedly p f as e = foldl p (\b a -> f a b) e as

repeatedlyM :: forall c as b proxy m. (Monad m, All c as)
            => proxy c
            -> (forall a. c a => a -> b -> m b) -> (HList as -> b -> m b)
repeatedlyM p f as e = foldlM p (\b a -> f a b) e as

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

data SList :: [Type] -> Type where
  SNil :: SList '[]
  SCons :: SList as -> SList (a ': as)

class IsList (xs :: [Type]) where
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
