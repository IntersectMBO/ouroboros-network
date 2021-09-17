{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module LedgerOnDisk.SOP.OnDiskMappings where

import Data.Kind
import Data.SOP
import Control.Lens
import Data.SOP.NP
import Data.Coerce
import Data.SOP.Dict
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Control.Applicative


-- |


type MapKind k = k -> Type -> Type -> Type
type StateKind k = MapKind k -> Type

type family FstOf3 (tlm :: (k, l, m)) :: k where
  FstOf3 '(k, _, _) = k

type family SndOf3 (tlm :: (k, l, m)) :: l where
  SndOf3 '(_, l, _) = l

type family ThirdOf3 (tlm :: (k, l, m)) :: m where
  ThirdOf3 '(_, _, m) = m

data NullMap t k v = NullMap

instance Semigroup (NullMap t k v) where
  _ <> _ = NullMap

newtype ConstMap a t k v = ConstMap a

data SopMap (map :: MapKind ki) (tkv :: (ki, Type, Type)) where
  SopMap :: map t k v -> SopMap map '(t, k, v)

-- newtype SopMapM f map t k v = SopMapM (SopMap map '(t, k, f v))


class (Top tkv, tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))
instance (Top tkv, tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))

class (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: ki -> Type -> Type -> Constraint) (tkv :: (ki, Type, Type)) where
instance (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: ki -> Type -> Type -> Constraint) (tkv :: (ki, Type, Type)) where


class (All IsOnDiskTuple (OnDiskMappingsTypes state)) => HasOnDiskMappings (state :: StateKind ki) where

  type OnDiskMappingsTypes state :: [ (ki, Type, Type) ]

  onDiskMappingsLens :: Lens (state map1) (state map2) (OnDiskMappings state map1) (OnDiskMappings state map2)

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  projectOnDiskMappings = view onDiskMappingsLens

  injectOnDiskMappings :: OnDiskMappings state map -> state anymap -> state map
  injectOnDiskMappings = set onDiskMappingsLens

newtype ApMap map1 map2 t k v where
  ApMap :: (map1 t k v -> map2 t k v) -> ApMap map1 map2 t k v

newtype ComposeMap f map t k v where
  ComposeMap :: f (map t k v) -> ComposeMap f map t k v

newtype OnDiskMappings state map = OnDiskMappings (NP (SopMap map) (OnDiskMappingsTypes state))

class All (CurriedConstraint c) (OnDiskMappingsTypes state) => AllMap (c :: ki -> Type -> Type -> Constraint) state
instance All (CurriedConstraint c) (OnDiskMappingsTypes state) => AllMap (c :: ki -> Type -> Type -> Constraint) state

class EmptyConstraint t k v
instance EmptyConstraint t k v

pureOnDiskMappings :: forall ki (c :: ki -> Type -> Type -> Constraint) (map :: MapKind ki) (state :: StateKind ki) proxy.
  (AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map t k v)
  -> OnDiskMappings state map

type ToTuple tkv = '(FstOf3 tkv, SndOf3 tkv, ThirdOf3 tkv)

pureOnDiskMappings _ mk_map = OnDiskMappings $ hcpure (Proxy :: Proxy (CurriedConstraint c)) mk_map'
  where
    mk_map' :: forall (tkv :: (ki, Type, Type)).  (CurriedConstraint c tkv) => SopMap map tkv
    mk_map' = SopMap mk_map

apOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: MapKind ki) (map2 :: MapKind ki).
  (HasOnDiskMappings state)
  => OnDiskMappings state (map1 `ApMap` map2) -> OnDiskMappings state map1 -> OnDiskMappings state map2
apOnDiskMappings (OnDiskMappings x) (OnDiskMappings y) = OnDiskMappings $ ap_NP (map_NP go x) y
  where
    go :: forall (tkv :: (ki, Type, Type)). SopMap (map1 `ApMap` map2) tkv -> (SopMap map1 -.-> SopMap map2) tkv
    go (SopMap (ApMap f)) = Fn $ \(SopMap m) -> SopMap (f m)

collapseOnDiskMappings :: forall ki (state :: StateKind ki) a. HasOnDiskMappings state => OnDiskMappings state (ConstMap a) -> [a]
collapseOnDiskMappings (OnDiskMappings np) = hcollapse (map_NP to_k np)
  where
    to_k :: SopMap (ConstMap a) tkv -> K a tkv
    to_k (SopMap (ConstMap a)) = K a

traverseOnDiskMappings :: forall ki proxy state f c map1 map2. (Applicative f, AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> f (map2 t k v))
  -> OnDiskMappings state map1 -> f (OnDiskMappings state map2)
traverseOnDiskMappings _ f (OnDiskMappings np) = OnDiskMappings <$> hctraverse' (Proxy :: Proxy (CurriedConstraint c)) go np
  where
    go :: CurriedConstraint c tkv => SopMap map1 tkv -> f (SopMap map2 tkv)
    go (SopMap m) = SopMap <$> f m

foldMapOnDiskMappings :: forall ki proxy state c map a. (Monoid a, AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map t k v -> a)
  -> OnDiskMappings state map -> a
foldMapOnDiskMappings _ f (OnDiskMappings np)= hcfoldMap (Proxy :: Proxy (CurriedConstraint c)) go np
  where
    go :: CurriedConstraint c tkv => SopMap map tkv -> a
    go (SopMap m) = f m

mapOnDiskMappings :: forall ki proxy (state :: StateKind ki) c map1 map2. AllMap c state
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2
mapOnDiskMappings p f x = runIdentity $ traverseOnDiskMappings p (pure . f) x

zipOnDiskMappings :: forall ki proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki). (AllMap c state, HasOnDiskMappings state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3
zipOnDiskMappings p f x y = go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 map3)
    go = mapOnDiskMappings p (coerce . f ) x

zipAOnDiskMappings :: forall ki proxy (state :: StateKind ki) c map1 map2 map3 f. (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => f (map1 t k v) -> f (map2 t k v) -> f (map3 t k v))
  -> f (OnDiskMappings state map1) -> f (OnDiskMappings state map2) -> f (OnDiskMappings state map3)
zipAOnDiskMappings p f x y =  liftA2 apOnDiskMappings go y
  where
    go :: OnDiskMappings state (ComposeMap f (ApMap map2 map3))
    go = mapOnDiskMappings p (coerce . f ) x

-- data WrappedTuple tkv where
--   WrappedTuple :: tkv ~ (t,k,v) => WrappedTuple '(t, k, v)

-- data MapDict c t k v where
--   MapDict :: c t k v => MapDict c t k v

-- pureAllMap :: forall ki proxy state. HasOnDiskMappings state => proxy state -> Dict (All Top) (OnDiskMappingsTypes state)
-- pureAllMap _ = pureAll

allOnDiskTuplesMap :: forall ki proxy (state :: StateKind ki). HasOnDiskMappings state => proxy state -> Dict (All (IsOnDiskTuple )) (OnDiskMappingsTypes state)
allOnDiskTuplesMap _ = Dict

pureEmptyConstraintMap :: forall ki proxy (state :: StateKind ki). HasOnDiskMappings state => proxy state -> Dict (All (CurriedConstraint EmptyConstraint)) (OnDiskMappingsTypes state)
pureEmptyConstraintMap p = mapAll go $ allOnDiskTuplesMap p
  where
    go :: forall a. (EmptyConstraint (FstOf3 a) (SndOf3 a) (ThirdOf3 a)) => Dict (IsOnDiskTuple) a -> Dict (CurriedConstraint EmptyConstraint) a
    go Dict = Dict

class Semigroup (map t k v)  => SemigroupMap map t k v
instance Semigroup (map t k v)  => SemigroupMap map t k v

instance (HasOnDiskMappings state, AllMap (SemigroupMap map) state)
  => Semigroup (OnDiskMappings (state :: StateKind ki) (map :: MapKind ki)) where
  x <> y =  zipOnDiskMappings (Proxy :: Proxy (SemigroupMap map)) (<>) x y

newtype DataMap t k v = DataMap (Map k v)
  deriving stock (Show, Eq)
  deriving newtype (Functor)

deriving via MonoidalMap k v instance (Ord k, Semigroup v) => Semigroup (DataMap t k v)
deriving via MonoidalMap k v instance (Ord k, Semigroup v) => Monoid (DataMap t k v)

class (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v
instance (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v

