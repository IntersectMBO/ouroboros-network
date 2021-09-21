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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module LedgerOnDisk.V2.OnDiskMappings where

import Data.Kind
import Data.SOP
import Control.Lens
import Data.SOP.NP
import Data.Coerce
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.GADT.Compare
import Type.Reflection
import Data.Type.Equality
import Data.Functor
import Control.Monad
import Data.Monoid hiding (All)

-- data MapKind = MapRO | MapRW


type MapKind ki = ki -> Type -> Type -> Type
type StateKind ki = MapKind ki -> Type

type family FstOf3 (tlm :: (k, l, m)) :: k where
  FstOf3 '(k, _, _) = k

type family SndOf3 (tlm :: (k, l, m)) :: l where
  SndOf3 '(_, l, _) = l

type family ThirdOf3 (tlm :: (k, l, m)) :: m where
  ThirdOf3 '(_, _, m) = m

data CurriedMap (map :: MapKind ki) (tkv :: (ki, Type, Type)) where
  CurriedMap :: map t k v -> CurriedMap map '(t, k, v)

type ToTuple tkv = '(FstOf3 tkv, SndOf3 tkv, ThirdOf3 tkv)

-- Try delete Top
class (tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))
instance (tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))

class (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: ki -> Type -> Type -> Constraint) (tkv :: (ki, Type, Type)) where

instance (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: ki -> Type -> Type -> Constraint) (tkv :: (ki, Type, Type)) where

class Top3 (t :: ki) k v
instance Top3 (t :: ki) k v

class (All IsOnDiskTuple (OnDiskMappingsTypes state)) => HasOnDiskMappings (state :: StateKind ki) where

  type OnDiskMappingsTypes state :: [ (ki, Type, Type) ]

  onDiskMappingsLens :: Lens (state map1) (state map2) (OnDiskMappings state map1) (OnDiskMappings state map2)

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  projectOnDiskMappings = view onDiskMappingsLens

  injectOnDiskMappings :: OnDiskMappings state map -> state anymap -> state map
  injectOnDiskMappings = set onDiskMappingsLens

newtype ApMap map1 map2 t k v where
  ApMap :: (map1 t k v -> map2 t k v) -> ApMap map1 map2 t k v

newtype ComposeMap f map (t :: ki) k v where
  ComposeMap :: f (map t k v) -> ComposeMap f map t k v

newtype OnDiskMappings (state :: StateKind ki) map = OnDiskMappings (NP (CurriedMap map) (OnDiskMappingsTypes state))

-- Probs doesn't need to be a class, but what's the downside?
class (HasOnDiskMappings state, All (CurriedConstraint c) (OnDiskMappingsTypes state)) => AllMap (c :: ki -> Type -> Type -> Constraint) state
instance (HasOnDiskMappings state, All (CurriedConstraint c) (OnDiskMappingsTypes state)) => AllMap (c :: ki -> Type -> Type -> Constraint) state

class EmptyConstraint t k v
instance EmptyConstraint t k v

pureOnDiskMappings :: forall ki (map :: MapKind ki) (state :: StateKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map t k v)
  -> OnDiskMappings state map
pureOnDiskMappings mk_map = OnDiskMappings $ hcpure (Proxy :: Proxy IsOnDiskTuple) (CurriedMap mk_map)

purecOnDiskMappings :: forall ki (c :: ki -> Type -> Type -> Constraint) (map :: MapKind ki) (state :: StateKind ki) proxy.
  (AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map t k v)
  -> OnDiskMappings state map
purecOnDiskMappings _ mk_map = OnDiskMappings $ hcpure (Proxy :: Proxy (CurriedConstraint c)) (CurriedMap mk_map)

apOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: MapKind ki) (map2 :: MapKind ki).
  (HasOnDiskMappings state)
  => OnDiskMappings state (map1 `ApMap` map2) -> OnDiskMappings state map1 -> OnDiskMappings state map2
apOnDiskMappings (OnDiskMappings x) (OnDiskMappings y) = OnDiskMappings $ ap_NP (map_NP go x) y
  where
    go :: forall (tkv :: (ki, Type, Type)). CurriedMap (map1 `ApMap` map2) tkv -> (CurriedMap map1 -.-> CurriedMap map2) tkv
    go (CurriedMap (ApMap f)) = Fn $ \(CurriedMap  m) -> CurriedMap (f m)

collapseOnDiskMappings :: forall ki (state :: StateKind ki) a. HasOnDiskMappings state => OnDiskMappings state (ConstMap a) -> [a]
collapseOnDiskMappings odm = appEndo (foldMapOnDiskMappings (\(ConstMap x) -> Endo (x :)) odm) []

traversecOnDiskMappings :: forall ki proxy state f c map1 map2. (Applicative f, AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> f (map2 t k v))
  -> OnDiskMappings state map1 -> f (OnDiskMappings state map2)
traversecOnDiskMappings _ f (OnDiskMappings np) = OnDiskMappings <$> hctraverse' (Proxy :: Proxy (CurriedConstraint c)) go np
  where
    go :: CurriedConstraint c tkv => CurriedMap map1 tkv -> f (CurriedMap map2 tkv)
    go (CurriedMap m) = CurriedMap <$> f m

traverseOnDiskMappings :: forall ki state f map1 map2. (Applicative f, HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> f (map2 t k v))
  -> OnDiskMappings state map1 -> f (OnDiskMappings state map2)
traverseOnDiskMappings f (OnDiskMappings np) = OnDiskMappings <$> hctraverse' (Proxy :: Proxy IsOnDiskTuple) go np
  where
    go :: CurriedMap map1 tkv -> f (CurriedMap map2 tkv)
    go (CurriedMap m) = CurriedMap <$> f m

traverseOnDiskMappings_ :: forall ki state f map1. (Applicative f, HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> f ())
  -> OnDiskMappings state map1 -> f ()
traverseOnDiskMappings_ f (OnDiskMappings np) = hctraverse_ (Proxy :: Proxy IsOnDiskTuple) go np
  where
    go :: CurriedMap map1 tkv -> f ()
    go (CurriedMap m) = f m

sequenceOnDiskMappings :: forall ki (state :: StateKind ki) f (map :: MapKind ki).
  (Applicative f, HasOnDiskMappings state)
  => OnDiskMappings state (ComposeMap f map) -> f (OnDiskMappings state map)
sequenceOnDiskMappings (OnDiskMappings x) = OnDiskMappings <$> hsequence' (map_NP go x)
  where
    go :: CurriedMap (ComposeMap f map) tkv -> (f :.: CurriedMap map) tkv
    go (CurriedMap (ComposeMap f_map)) = Comp (CurriedMap <$> f_map)

foldMapOnDiskMappings :: forall ki state map a. (Monoid a, HasOnDiskMappings state)
  => (forall (t :: ki) k v. map t k v -> a)
  -> OnDiskMappings state map -> a
foldMapOnDiskMappings f m = getConst $ traverseOnDiskMappings_ (Const . f) m

foldMapcOnDiskMappings :: forall ki proxy state c map a. (Monoid a, AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map t k v -> a)
  -> OnDiskMappings state map -> a
foldMapcOnDiskMappings _ f (OnDiskMappings np)= hcfoldMap (Proxy :: Proxy (CurriedConstraint c)) go np
  where
    go :: CurriedConstraint c tkv => CurriedMap map tkv -> a
    go (CurriedMap m) = f m

mapOnDiskMappings :: forall ki (state :: StateKind ki) map1 map2.
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2
mapOnDiskMappings f x = runIdentity $ traverseOnDiskMappings (pure . f) x

mapcOnDiskMappings :: forall ki proxy (state :: StateKind ki) c map1 map2. AllMap c state
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2
mapcOnDiskMappings p f x = runIdentity $ traversecOnDiskMappings p (pure . f) x

zipOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3
zipOnDiskMappings f x y = go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 map3)
    go = mapOnDiskMappings (coerce . f ) x

zipcOnDiskMappings :: forall ki proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki). (AllMap c state, HasOnDiskMappings state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3
zipcOnDiskMappings p f x y = go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 map3)
    go = mapcOnDiskMappings p (coerce . f ) x

zipAOnDiskMappings :: forall ki (state :: StateKind ki) map1 map2 map3 f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> f (map3 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)
zipAOnDiskMappings f x y = sequenceOnDiskMappings $ go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 (ComposeMap f map3))
    go = mapOnDiskMappings (coerce . f ) x

zipAcOnDiskMappings :: forall ki f proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> f (map3 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)
zipAcOnDiskMappings p f x y = sequenceOnDiskMappings $ go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 (ComposeMap f map3))
    go = mapcOnDiskMappings p (coerce . f ) x

zip4AcOnDiskMappings :: forall ki f proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki) (map4 :: MapKind ki) (map5 :: MapKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> f (map5 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4 -> f (OnDiskMappings state map5)
zip4AcOnDiskMappings p f a b c d = sequenceOnDiskMappings $  a' `apOnDiskMappings` b `apOnDiskMappings` c `apOnDiskMappings` d
  where
    a' = mapcOnDiskMappings p (coerce . f)  a

-- zip4AcOnDiskMappings_ :: forall ki f proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki) (map4 :: MapKind ki) (map5 :: MapKind ki).
--   (AllMap c state, Applicative f)
--   => proxy c
--   -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> f ())
--   -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4 -> f ()
-- zip4AcOnDiskMappings_ p f a b c d = void $ zip4AcOnDiskMappings p f' a b c d
--   where
--     f' :: forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> f (NullMap t k v)
--     f' a' b' c' d' = f a' b' c' d' $> NullMap
--     -- a'' = mapcOnDiskMappings p (coerce . f') a

zipAcOnDiskMappings_:: forall ki f proxy (state :: StateKind ki) c (map1 :: MapKind ki) (map2 :: MapKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> f ())
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f ()
zipAcOnDiskMappings_ p f x y = void $ sequenceOnDiskMappings $  go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 (ComposeMap f NullMap))
    go = mapcOnDiskMappings p (\v -> ApMap (\z -> ComposeMap ( f v z $> NullMap))) x





-- data WrappedTuple tkv where
--   WrappedTuple :: tkv ~ (t,k,v) => WrappedTuple '(t, k, v)

-- data MapDict c t k v where
--   MapDict :: c t k v => MapDict c t k v

-- pureAllMap :: forall ki proxy state. HasOnDiskMappings state => proxy state -> Dict (All Top) (OnDiskMappingsTypes state)
-- pureAllMap _ = pureAll

-- allOnDiskTuplesMap :: forall ki proxy (state :: StateKind ki). HasOnDiskMappings state => proxy state -> Dict (All (IsOnDiskTuple )) (OnDiskMappingsTypes state)
-- allOnDiskTuplesMap _ = Dict

-- pureEmptyConstraintMap :: forall ki proxy (state :: StateKind ki). HasOnDiskMappings state => proxy state -> Dict (All (CurriedConstraint EmptyConstraint)) (OnDiskMappingsTypes state)
-- pureEmptyConstraintMap p = mapAll go $ allOnDiskTuplesMap p
--   where
--     go :: forall a. (EmptyConstraint (FstOf3 a) (SndOf3 a) (ThirdOf3 a)) => Dict (IsOnDiskTuple) a -> Dict (CurriedConstraint EmptyConstraint) a
--     go Dict = Dict

data NullMap t k v = NullMap
  deriving stock (Eq, Show)

instance Semigroup (NullMap t k v) where
  _ <> _ = NullMap

instance Monoid (NullMap t k v) where
  mempty = NullMap

newtype ConstMap a t k v = ConstMap a
  deriving stock (Eq, Show)
  deriving (Functor, Applicative) via (Const a)

deriving via (Const a v) instance Semigroup a => Semigroup (ConstMap a t k v)
deriving via (Const a v) instance Monoid a => Monoid (ConstMap a t k v)


class Semigroup (map t k v)  => SemigroupMap (map :: MapKind ki) (t :: ki) k v
instance Semigroup (map t k v)  => SemigroupMap map t k v

class (Monoid (map t k v), SemigroupMap map t k v)  => MonoidMap (map :: MapKind ki) (t :: ki) k v
instance (SemigroupMap map t k v, Monoid (map t k v))  => MonoidMap map t k v

instance (HasOnDiskMappings state, AllMap (SemigroupMap map) state)
  => Semigroup (OnDiskMappings (state :: StateKind ki) (map :: MapKind ki)) where
  x <> y =  zipcOnDiskMappings (Proxy :: Proxy (SemigroupMap map)) (<>) x y

instance
  ( HasOnDiskMappings state
  , AllMap (MonoidMap map) state
  , Semigroup (OnDiskMappings state map)
  ) => Monoid (OnDiskMappings (state :: StateKind ki) (map :: MapKind ki)) where
  mempty :: OnDiskMappings state map
  mempty = purecOnDiskMappings (Proxy :: Proxy (MonoidMap map)) mempty

newtype DataMap t k v = DataMap (Map k v)
  deriving stock (Show, Eq)
  deriving newtype (Functor)
  deriving (Semigroup, Monoid) via MonoidalMap k v

class (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v
instance (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v

class Ord k => KeysOrd t k v
instance Ord k => KeysOrd t k v

nullMappings :: forall ki (state :: StateKind ki). HasOnDiskMappings state => OnDiskMappings state NullMap
nullMappings = pureOnDiskMappings (NullMap :: forall (t :: ki) k v. NullMap t k v)

data ProductMap (map1 :: MapKind ki) (map2 :: MapKind ki) (t :: ki) k v = ProductMap (map1 t k v) (map2 t k v)

liftProductMapRight :: forall ki (state ::StateKind ki) (map1 :: MapKind ki) (map2 :: MapKind ki) (map3 :: MapKind ki).
  HasOnDiskMappings state
  => (OnDiskMappings state map2 -> OnDiskMappings state map3)
  -> OnDiskMappings state (map1 `ProductMap` map2) -> OnDiskMappings state (map1 `ProductMap` map3)
liftProductMapRight f x = zipOnDiskMappings (\(ProductMap l _) res -> ProductMap l res) x (f justRightMaps)
  where
    justRightMaps = mapOnDiskMappings (\(ProductMap _ r) -> r) x

lowerProductMapLeft :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> OnDiskMappings state map1
lowerProductMapLeft = mapOnDiskMappings $ \(ProductMap x _) -> x

lowerProductMapRight :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> OnDiskMappings state map2
lowerProductMapRight = mapOnDiskMappings $ \(ProductMap _ y) -> y
newtype MaybeMap map t k v = MaybeMap (Maybe (map t k v))

odmProductMapToTuple :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> (OnDiskMappings state map1, OnDiskMappings state map2)
odmProductMapToTuple odm = (lowerProductMapLeft odm, lowerProductMapRight odm)

data TableId state t k v = TableId (NS (K Int) (OnDiskMappingsTypes state))

-- indexedOnDiskMappings :: OnDiskMappings state (TableId state)
-- indexedOnDiskMappings
curriedMapEquality :: forall map x y. (Typeable x, Typeable y) => CurriedMap map x -> CurriedMap map y -> Maybe (x :~: y)

curriedMapEquality _ _ = (typeRep :: TypeRep x) `testEquality` (typeRep :: TypeRep y)

-- instance GEq (CurriedMap TableId) where
--   geq x@(CurriedMap (TableId i1 :: TableId (t1 :: ki) k1 v1)) y@(CurriedMap (TableId i2 :: TableId (t2 :: ki) k2 v2)) = do
--     guard (i1 == i2)
--     curriedMapEquality x y
