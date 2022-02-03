{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Variations on n-ary products over type-level lists of kind `[(*,*)]`.
module Ouroboros.Consensus.Util.NP2 (
    -- * Tuple projections
    Fst
  , Snd
    -- * NP2
  , NP2 (..)
  , cpure_NP2
  , type (-..->) (..)
  , type (:..:) (..)
    -- * OptNP2
  , OptNP2 (..)
  , fromNP2
  , fromNonEmptyNP2
    -- * Constraints
  , All2 (..)
  , And2
  , SList2 (..)
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Proxy
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util.SOP (IsNonEmpty (..),
                     ProofNonEmpty (ProofNonEmpty))

type family Fst (x :: (a, b)) :: a where
  Fst '(a, b) = a

type family Snd (x :: (a, b)) :: b where
  Snd '(a, b) = b

-- | Variation on '(:.:)' for an second argument of kind (* -> * -> *).
newtype (:..:) (f :: l -> Type) (g :: j -> k -> l) (j1 :: j) (k1 :: k)
  = Comp2 (f (g j1 k1))
  deriving (Generic)

newtype (-..->) f g a b = Fn2 {apFn2 :: f a b -> g a b}

infixr 1 -..->

-- | Variation on 'Data.SOP.NP.NP' applied to tupled lists and arguments of kind
-- (* -> * -> *).
data NP2 :: (k -> l -> Type) -> [(k, l)] -> Type where
  Nil2 :: NP2 f '[]
  Cons2 :: f x y -> NP2 f xs -> NP2 f ('(x, y) ': xs)

data SList2 :: [(k, l)] -> Type where
  SNil2 :: SList2 '[]
  SCons2 :: SList2I xs => SList2 ('(x, y) ': xs)

-- | Always satisfiable 2-param class
class Top2 x y

instance Top2 x y

type SList2I = All2 Top2

sList2 :: SList2I xs => SList2 xs
sList2 = cpara_SList2 (Proxy @Top2) SNil2 (const SCons2)

cpure_NP2 ::
  forall c xs proxy f.
  All2 c xs =>
  proxy c ->
  (forall a b. c a b => f a b) ->
  NP2 f xs
cpure_NP2 p f = case sList2 :: SList2 xs of
  SNil2  -> Nil2
  SCons2 -> f `Cons2` cpure_NP2 p f

-- | Like an 'NP2', but with optional values
data OptNP2 (empty :: Bool) (f :: k -> l -> Type) (xs :: [(k, l)]) where
  OptNil2 :: OptNP2 'True f '[]
  OptCons2 :: !(f x y) -> !(OptNP2 empty f xs) -> OptNP2 'False f ('(x, y) ': xs)
  OptSkip2 :: !(OptNP2 empty f xs) -> OptNP2 empty f (x ': xs)

fromNonEmptyNP2 :: forall f xs. IsNonEmpty xs => NP2 f xs -> OptNP2 'False f xs
fromNonEmptyNP2 xs = case isNonEmpty (Proxy @xs) of
  ProofNonEmpty {} ->
    case xs of
      x `Cons2` xs' -> fromNP2 (OptCons2 x) xs'

fromNP2 :: (forall empty. OptNP2 empty f xs -> r) -> NP2 f xs -> r
fromNP2 k Nil2            = k OptNil2
fromNP2 k (x `Cons2` xs') = fromNP2 (k . OptCons2 x) xs'

{-------------------------------------------------------------------------------
  Constraints
-------------------------------------------------------------------------------}

-- | Pairing of constraints of two arguments
class (f x y, g x y) => (f `And2` g) x y

instance (f x y, g x y) => (f `And2` g) x y

infixl 7 `And2`

type family AllF2 (c :: k -> l -> Constraint) (xs :: [(k, l)]) :: Constraint where
  AllF2 _c '[] = ()
  AllF2 c ('(x, y) ': xs) = (c x y, All2 c xs)

-- | Witness that a constraint on two arguments applies (curried) to every
-- argument of a list of 2-tuples.
class (AllF2 c xs, SList2I xs) => All2 (c :: k -> l -> Constraint) (xs :: [(k, l)]) where
  cpara_SList2 ::
    proxy c ->
    r '[] ->
    (forall y z ys. (c y z, All2 c ys) => r ys -> r ('(y, z) ': ys)) ->
    r xs

instance All2 c '[] where
  cpara_SList2 _p nil _cons = nil

instance (c x y, All2 c xs) => All2 c ('(x, y) ': xs) where
  cpara_SList2 p nil cons = cons (cpara_SList2 p nil cons)
