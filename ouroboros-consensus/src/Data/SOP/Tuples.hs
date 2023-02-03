{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Data.SOP.Tuples (
    -- * Tuples
    ApOnlySnd (..)
  , ApOnlySnd2 (..)
  , Fst
  , MapSnd
  , NsMapSnd (..)
  , Snd
  , Uncurry
  , UncurryComp (..)
  , castSndIdx
  ) where

import           Data.Kind (Type)
import           Data.SOP.Index
import           Data.SOP.Strict

{-------------------------------------------------------------------------------
  Tuples
-------------------------------------------------------------------------------}

type Fst :: (a, b) -> a
type family Fst t where
  Fst '(a,b) = a

type Snd :: (a, b) -> b
type family Snd t where
    Snd '(a,b) = b

type MapSnd :: [(Type, Type)] -> [Type]
type family MapSnd xs where
  MapSnd '[]       = '[]
  MapSnd (x ': xs) = Snd x ': MapSnd xs

type UncurryComp ::
     (Type -> Type)
  -> (Type -> Type -> Type)
  -> (Type, Type)
  -> Type
newtype UncurryComp f g ab = UncurryComp (f (Uncurry g ab))

type Uncurry :: (a -> b -> k) -> (a, b) -> k
type family Uncurry f ab where
  Uncurry f ab = (f (Fst ab) (Snd ab))

type ApOnlySnd :: (a -> Type) -> (b, a) -> Type
newtype ApOnlySnd f ba = ApOnlySnd { unApOnlySnd :: f (Snd ba) }

type ApOnlySnd2 :: (a -> a -> Type) -> (b, a) -> (b, a) -> Type
newtype ApOnlySnd2 f ab ab' = ApOnlySnd2 (f (Snd ab) (Snd ab'))

class NsMapSnd xs where
  nsMapSnd :: NS f (MapSnd xs) -> NS (ApOnlySnd f) xs

instance NsMapSnd '[] where
  nsMapSnd = \case {}

instance NsMapSnd xs => NsMapSnd (x ': xs) where
  nsMapSnd = \case
    Z fx -> Z (ApOnlySnd fx)
    S ns -> S (nsMapSnd ns)

castSndIdx :: Index xs x -> Index (MapSnd xs) (Snd x)
castSndIdx = \case
  IZ   -> IZ
  IS n -> IS (castSndIdx n)
