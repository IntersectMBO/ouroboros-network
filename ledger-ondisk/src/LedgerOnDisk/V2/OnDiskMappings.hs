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
{-# LANGUAGE DeriveGeneric #-}

module LedgerOnDisk.V2.OnDiskMappings where

import Data.Kind
import Data.SOP
-- import Control.Lens
import Data.SOP.NP
import Data.Coerce
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Functor
import Data.Monoid hiding (All)
import qualified Data.Monoid (All(..))
import GHC.Generics hiding ((:.:))
import Test.QuickCheck hiding (Fn)
import Data.Functor.Const
import Data.Functor.Identity
import Data.List (intercalate)


-------------------------------------------------------------------
{-
Some preliminaries.

At a top level we are concerned with types of the form `T (map :: ki -> Type -> Type -> Type)`

for example

type ki = 'Bool
data PetStore map = PetStore
  { inventory :: map 'True String Int
  , orders :: map 'False String Double
  , balance :: Int
  }

-}

type TableKind ki = ki -> Type -> Type -> Type
type StateKind ki = TableKind ki -> Type
type TableConstraintKind ki = ki -> Type -> Type -> Constraint

type family FstOf3 (tlm :: (k, l, m)) :: k where
  FstOf3 '(k, _, _) = k

type family SndOf3 (tlm :: (k, l, m)) :: l where
  SndOf3 '(_, l, _) = l

type family ThirdOf3 (tlm :: (k, l, m)) :: m where
  ThirdOf3 '(_, _, m) = m

data CurriedMap (map :: TableKind ki) (tkv :: (ki, Type, Type)) where
  CurryMap :: { uncurryMap :: map t k v} -> CurriedMap map '(t, k, v)

deriving stock instance (IsOnDiskTuple tkv, Eq (map (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv))) => Eq (CurriedMap map tkv)
deriving stock instance (IsOnDiskTuple tkv, Show (map (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv))) => Show (CurriedMap map tkv)

type ToTuple tkv = '(FstOf3 tkv, SndOf3 tkv, ThirdOf3 tkv)

class (tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))
instance (tkv ~ ToTuple tkv) => IsOnDiskTuple (tkv :: (ki, Type, Type))

class (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: TableConstraintKind ki) (tkv :: (ki, Type, Type)) where

instance (c (FstOf3 tkv) (SndOf3 tkv) (ThirdOf3 tkv), IsOnDiskTuple tkv)
  => CurriedConstraint (c :: TableConstraintKind ki) (tkv :: (ki, Type, Type)) where

{-| OnDiskMappingsTypes

Witnesses that (state map) is isomorphic to (a, NP map xs), where a is existensial and xs is `OnDiskMappingsTypes state`, and so fixed
by the instance.

Instances of `HasOnDiskMappings` are recommended to define a pattern synonym, for `PetStore`:

pattern OdmPetStore ::
  forall map. ()
  => ()
  => map 'True String Int
  -> map 'False String Double
  -> OnDiskMappings PetStore map
pattern OdmPetStore { inventory, orders, balance } = OnDiskMappings $ CurriedMap inventory :* CurriedMap orders :* balance :* Nil

-- TODO Revisit ^^^ when you have a bit more experience
-}

class (All IsOnDiskTuple (OnDiskMappingsTypes state)) => HasOnDiskMappings (state :: StateKind ki) where
  {-# MINIMAL tableTags, (onDiskMappingsLens | projectOnDiskMappings, injectOnDiskMappings) #-}

  -- TODO should this be data TableTag ki ?
  type TableTypeTag state :: ki -> Type -> Type -> Type
  tableTags :: OnDiskMappings state (TableTypeTag state)

  type OnDiskMappingsTypes state :: [ (ki, Type, Type) ]
  -- This is a lens, but we don't depend on lens here
  onDiskMappingsLens :: forall f (map1 :: TableKind ki) (map2 :: TableKind ki). Functor f
    => (OnDiskMappings state map1 -> f (OnDiskMappings state map2))
    -> state map1 -> f (state map2)
  onDiskMappingsLens f s = flip injectOnDiskMappings s <$> f (projectOnDiskMappings s)

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  projectOnDiskMappings = getConst . onDiskMappingsLens Const

  injectOnDiskMappings :: OnDiskMappings state map -> state anymap -> state map
  injectOnDiskMappings a s = runIdentity $ onDiskMappingsLens (const $ pure a) s

newtype OnDiskMappings (state :: StateKind ki) map = OnDiskMappings { unOnDiskMappings :: NP (CurriedMap map) (OnDiskMappingsTypes state) }

newtype ApMap map1 map2 t k v where
  ApMap :: (map1 t k v -> map2 t k v) -> ApMap map1 map2 t k v

newtype ComposeMap f map (t :: ki) k v where
  ComposeMap :: f (map t k v) -> ComposeMap f map t k v


-- Probs doesn't need to be a class, but what's the downside?
class (HasOnDiskMappings state, All (CurriedConstraint c) (OnDiskMappingsTypes state)) => AllMap (c :: TableConstraintKind ki) (state :: StateKind ki)
instance (HasOnDiskMappings state, All (CurriedConstraint c) (OnDiskMappingsTypes state)) => AllMap (c :: TableConstraintKind ki) (state :: StateKind ki)

pureOnDiskMappings :: forall ki (map :: TableKind ki) (state :: StateKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map t k v)
  -> OnDiskMappings state map
pureOnDiskMappings mk_map = OnDiskMappings $ hcpure (Proxy :: Proxy IsOnDiskTuple) (CurryMap mk_map)

pureAOnDiskMappings :: forall ki (map :: TableKind ki) (state :: StateKind ki) f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. f (map t k v))
  -> f (OnDiskMappings state map)
pureAOnDiskMappings mk_map = sequenceOnDiskMappings $ OnDiskMappings $ hcpure (Proxy :: Proxy IsOnDiskTuple) (CurryMap . ComposeMap $ mk_map)

purecOnDiskMappings :: forall ki (c :: TableConstraintKind ki) (map :: TableKind ki) (state :: StateKind ki) proxy.
  (AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map t k v)
  -> OnDiskMappings state map
purecOnDiskMappings _ mk_map = OnDiskMappings $ hcpure (Proxy :: Proxy (CurriedConstraint c)) (CurryMap mk_map)

pureAcOnDiskMappings :: forall ki (c :: TableConstraintKind ki) (map :: TableKind ki) (state :: StateKind ki) proxy f.
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => f (map t k v))
  -> f (OnDiskMappings state map)
pureAcOnDiskMappings p mk_map = sequenceOnDiskMappings $ purecOnDiskMappings p $ ComposeMap mk_map

apOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki).
  (HasOnDiskMappings state)
  => OnDiskMappings state (map1 `ApMap` map2) -> OnDiskMappings state map1 -> OnDiskMappings state map2
apOnDiskMappings (OnDiskMappings x) (OnDiskMappings y) = OnDiskMappings $ ap_NP (map_NP go x) y
  where
    go :: forall (tkv :: (ki, Type, Type)). CurriedMap (map1 `ApMap` map2) tkv -> (CurriedMap map1 -.-> CurriedMap map2) tkv
    go (CurryMap ap_map) = Fn $ \(CurryMap m) -> CurryMap $ coerce ap_map m

collapseOnDiskMappings :: forall ki (state :: StateKind ki) a.
  HasOnDiskMappings state =>
  OnDiskMappings state (ConstMap a) -> [a]
collapseOnDiskMappings odm = appEndo (coerce $ foldMapOnDiskMappings (Dual . Endo . (:) . getConstMap) odm) []

traverseOnDiskMappings :: forall ki state f map1 map2. (Applicative f, HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> f (map2 t k v))
  -> OnDiskMappings state map1 -> f (OnDiskMappings state map2)
traverseOnDiskMappings f (OnDiskMappings np) = OnDiskMappings <$> hctraverse' (Proxy :: Proxy IsOnDiskTuple) go np
  where
    go :: CurriedMap map1 tkv -> f (CurriedMap map2 tkv)
    go (CurryMap m) = fmap CurryMap . f $ m

traversecOnDiskMappings :: forall ki proxy (state :: StateKind ki) (c :: TableConstraintKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki) f.
  (Applicative f, AllMap c state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> f (map2 t k v))
  -> OnDiskMappings state map1 -> f (OnDiskMappings state map2)
traversecOnDiskMappings _ f (OnDiskMappings np) = OnDiskMappings <$> hctraverse'
  (Proxy :: Proxy (CurriedConstraint c))
  (fmap CurryMap . f . uncurryMap)
  np

traverseOnDiskMappings_ :: forall ki state f map1. (Applicative f, HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> f ())
  -> OnDiskMappings state map1 -> f ()
traverseOnDiskMappings_ f (OnDiskMappings np) = hctraverse_ (Proxy :: Proxy IsOnDiskTuple) (f . uncurryMap) np

sequenceOnDiskMappings :: forall ki (state :: StateKind ki) f (map :: TableKind ki).
  (Applicative f, HasOnDiskMappings state)
  => OnDiskMappings state (ComposeMap f map) -> f (OnDiskMappings state map)
sequenceOnDiskMappings (OnDiskMappings x) = OnDiskMappings <$> hsequence' (map_NP go x)
  where
    go :: CurriedMap (ComposeMap f map) tkv -> (f :.: CurriedMap map) tkv
    go (CurryMap (ComposeMap f_map)) = Comp (CurryMap <$> f_map)

sequenceOnDiskMappings_ :: forall ki (state :: StateKind ki) f.
  (Applicative f, HasOnDiskMappings state)
  => OnDiskMappings state (ComposeMap f NullMap) -> f ()
sequenceOnDiskMappings_ x = void $ sequenceOnDiskMappings x

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
    go (CurryMap m) = f m

mapOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2
mapOnDiskMappings f x = runIdentity $ traverseOnDiskMappings (pure . f) x

mapcOnDiskMappings :: forall ki proxy (state :: StateKind ki) c map1 map2. AllMap c state
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2
mapcOnDiskMappings p f x = runIdentity $ traversecOnDiskMappings p (pure . f) x

zipOnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3
zipOnDiskMappings f x y = mapOnDiskMappings (coerce . f ) x `apOnDiskMappings` y

zip3OnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki) (map4 :: TableKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4
zip3OnDiskMappings f x y z = mapOnDiskMappings (coerce . f ) x `apOnDiskMappings` y `apOnDiskMappings` z

zip4OnDiskMappings :: forall ki (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki)
  (map3 :: TableKind ki) (map4 :: TableKind ki) (map5 :: TableKind ki).
  (HasOnDiskMappings state)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> map5 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4 -> OnDiskMappings state map5
zip4OnDiskMappings f a b c d =  mapOnDiskMappings (coerce . f) a `apOnDiskMappings` b `apOnDiskMappings` c `apOnDiskMappings` d

zipcOnDiskMappings :: forall ki proxy (state :: StateKind ki) c (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki). (AllMap c state, HasOnDiskMappings state)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v)
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3
zipcOnDiskMappings p f x y = mapcOnDiskMappings p (coerce . f ) x `apOnDiskMappings` y

zipAOnDiskMappings :: forall ki (state :: StateKind ki) map1 map2 map3 f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> f (map3 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)
zipAOnDiskMappings f x y = sequenceOnDiskMappings $ mapOnDiskMappings (coerce . f ) x `apOnDiskMappings` y

zip3AOnDiskMappings :: forall ki (state :: StateKind ki) map1 map2 map3 map4 f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v -> f (map4 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> f (OnDiskMappings state map4)
zip3AOnDiskMappings f x y z = sequenceOnDiskMappings $ mapOnDiskMappings (coerce . f ) x `apOnDiskMappings` y `apOnDiskMappings` z

zip4AOnDiskMappings :: forall ki (state :: StateKind ki) map1 map2 map3 map4 map5 f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> f (map5 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4 -> f (OnDiskMappings state map5)
zip4AOnDiskMappings f x1 x2 x3 x4 = sequenceOnDiskMappings $ mapOnDiskMappings (coerce . f ) x1 `apOnDiskMappings` x2 `apOnDiskMappings` x3 `apOnDiskMappings` x4

zipAcOnDiskMappings :: forall ki f proxy (state :: StateKind ki) c (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> f (map3 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)
zipAcOnDiskMappings p f x y = sequenceOnDiskMappings $ mapcOnDiskMappings p (coerce . f ) x `apOnDiskMappings` y

zip3AcOnDiskMappings :: forall ki f proxy (state :: StateKind ki) c (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki) (map4 :: TableKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> f (map4 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> f (OnDiskMappings state map4)
zip3AcOnDiskMappings p f a b c = sequenceOnDiskMappings $  a' `apOnDiskMappings` b `apOnDiskMappings` c
  where
    a' = mapcOnDiskMappings p (coerce . f)  a

zip4AcOnDiskMappings :: forall ki f proxy (state :: StateKind ki) c (map1 :: TableKind ki) (map2 :: TableKind ki)
  (map3 :: TableKind ki) (map4 :: TableKind ki) (map5 :: TableKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> map4 t k v -> f (map5 t k v))
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> OnDiskMappings state map4 -> f (OnDiskMappings state map5)
zip4AcOnDiskMappings p f a b c d = sequenceOnDiskMappings $ mapcOnDiskMappings p (coerce . f) a `apOnDiskMappings` b `apOnDiskMappings` c `apOnDiskMappings` d

zip3AOnDiskMappings_ :: forall ki (state :: StateKind ki) map1 map2 map3 f.
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> map3 t k v -> f ())
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> f ()
zip3AOnDiskMappings_ f x y z = sequenceOnDiskMappings_ $ go `apOnDiskMappings` y `apOnDiskMappings` z
  where
    go = mapOnDiskMappings (\a0 -> ApMap (\a1 -> ApMap (\a2 -> ComposeMap ( f a0 a1 a2 $> NullMap)))) x

zip3AcOnDiskMappings_ :: forall ki (state :: StateKind ki) map1 map2 map3 f proxy c.
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> map3 t k v -> f ())
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> OnDiskMappings state map3 -> f ()
zip3AcOnDiskMappings_ p f x y z = sequenceOnDiskMappings_ $ go `apOnDiskMappings` y `apOnDiskMappings` z
  where
    go = mapcOnDiskMappings p (\a0 -> ApMap (\a1 -> ApMap (\a2 -> ComposeMap ( f a0 a1 a2 $> NullMap)))) x

zipAOnDiskMappings_:: forall ki f (state :: StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki).
  (HasOnDiskMappings state, Applicative f)
  => (forall (t :: ki) k v. map1 t k v -> map2 t k v -> f ())
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f ()
zipAOnDiskMappings_  f x y = void $ sequenceOnDiskMappings $  go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 (ComposeMap f NullMap))
    go = mapOnDiskMappings (\v -> ApMap (\z -> ComposeMap ( f v z $> NullMap))) x

zipAcOnDiskMappings_:: forall ki f proxy (state :: StateKind ki) c (map1 :: TableKind ki) (map2 :: TableKind ki).
  (AllMap c state, Applicative f)
  => proxy c
  -> (forall (t :: ki) k v. c t k v => map1 t k v -> map2 t k v -> f ())
  -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f ()
zipAcOnDiskMappings_ p f x y = void $ sequenceOnDiskMappings $  go `apOnDiskMappings` y
  where
    go :: OnDiskMappings state (ApMap map2 (ComposeMap f NullMap))
    go = mapcOnDiskMappings p (\v -> ApMap (\z -> ComposeMap ( f v z $> NullMap))) x


data NullMap t k v = NullMap
  deriving stock (Eq, Show, Functor)

instance Semigroup (NullMap t k v) where
  _ <> _ = NullMap

instance Monoid (NullMap t k v) where
  mempty = NullMap

newtype ConstMap a t k v = ConstMap { getConstMap :: a }
  deriving stock (Eq, Show)
  deriving newtype Arbitrary
  deriving (Functor, Applicative) via (Const a)

deriving via (Const a v) instance Semigroup a => Semigroup (ConstMap a t k v)
deriving via (Const a v) instance Monoid a => Monoid (ConstMap a t k v)

class c (map t k v) => MapIs c map t k v
instance c (map t k v) => MapIs c map t k v

-- class Semigroup (map t k v)  => SemigroupMap (map :: TableKind ki) (t :: ki) k v
-- instance Semigroup (map t k v)  => SemigroupMap map t k v

-- class (Monoid (map t k v), SemigroupMap map t k v)  => MonoidMap (map :: TableKind ki) (t :: ki) k v
-- instance (SemigroupMap map t k v, Monoid (map t k v))  => MonoidMap map t k v

instance (HasOnDiskMappings state, AllMap (MapIs Semigroup map) state)
  => Semigroup (OnDiskMappings (state :: StateKind ki) (map :: TableKind ki)) where
  x <> y =  zipcOnDiskMappings (Proxy :: Proxy (MapIs Semigroup map)) (<>) x y

instance
  ( HasOnDiskMappings state
  , AllMap (MapIs Semigroup map) state
  , AllMap (MapIs Monoid map) state
  -- , Semigroup (OnDiskMappings state map)
  ) => Monoid (OnDiskMappings (state :: StateKind ki) (map :: TableKind ki)) where
  mempty :: OnDiskMappings state map
  mempty = purecOnDiskMappings (Proxy :: Proxy (MapIs Monoid map)) mempty

-- allComposeDict :: forall c map state proxy.
--   (AllMap (MapIs c map) state, HasOnDiskMappings state)
--   => proxy '(state, map, c)
--   -> Dict (All (Compose c (CurriedMap map))) (OnDiskMappingsTypes state)
-- allComposeDict _ = all_NP go where
--   d1 :: Dict (All (CurriedConstraint (MapIs c map))) (OnDiskMappingsTypes state)
--   d1 = Dict
--   d2 :: NP (Dict (CurriedConstraint (MapIs c map))) (OnDiskMappingsTypes state)
--   d2 = unAll_NP d1
--   go :: NP (Dict (Compose c (CurriedMap map))) (OnDiskMappingsTypes state)
--   go = hmap (\Dict -> Dict) d2

-- deriving newtype instance (HasOnDiskMappings state, AllMap (MapIs Eq map) state) => Eq (OnDiskMappings state map)
instance (HasOnDiskMappings state, AllMap (MapIs Eq map) state) => Eq (OnDiskMappings state map) where
  x == y = Data.Monoid.getAll . foldMapOnDiskMappings getConstMap $ zipcOnDiskMappings (Proxy :: Proxy (MapIs Eq map)) go  x y where
    go :: Eq (map t k v) => map t k v -> map t k v -> ConstMap Data.Monoid.All t k v
    go z w = coerce $ z == w

    -- allComposeDict (Proxy :: Proxy '(state, map, Eq)) `withDict` go where
    -- go :: All (Compose Eq (CurriedMap map)) (OnDiskMappingsTypes state) => Bool
    -- go = x == y

-- class (Show (map1 t k v), Show (map2 t k v)) => ShowMapAux map1 map2 t k v
-- instance (Show (map1 t k v), Show (map2 t k v)) => ShowMapAux map1 map2 t k v

instance (HasOnDiskMappings state, AllMap (MapIs Show map) state) => Show (OnDiskMappings state map) where
  showsPrec d odm = showParen (d >= 10) $ showString $ "OnDiskMappings: {" <> showmappings <> "}" where
    showmappings = intercalate "," $ foldMapOnDiskMappings getConstMap $ mapcOnDiskMappings (Proxy :: Proxy (MapIs Show map)) go odm where
      go :: (Show (map t k v)) => map t k v -> ConstMap [String] t k v
      go m = ConstMap [show m]

instance (HasOnDiskMappings state, AllMap (MapIs Arbitrary map) state) => Arbitrary (OnDiskMappings (state :: StateKind ki) (map :: TableKind ki)) where
  arbitrary = pureAcOnDiskMappings (Proxy :: Proxy (MapIs Arbitrary map)) arbitrary
  shrink = fmap OnDiskMappings . unComp . go . unOnDiskMappings where
    go ::  forall ys. All (CurriedConstraint (MapIs Arbitrary map)) ys => NP (CurriedMap map) ys -> ([] :.: NP (CurriedMap map)) ys
    go Nil                 = Comp []
    go (CurryMap x :* Nil) = Comp $ fmap (\x' -> CurryMap x' :* Nil) $ shrink x
    go (CurryMap x :* xs)  = Comp $
        [ CurryMap x' :* xs  | x'  <- shrink x ] ++
        [ CurryMap x  :* xs' | xs' <- unComp $ go xs ]



-- instance (HasOnDiskMappings state, AllMap (MapIs Arbitrary map) state) => Eq (OnDiskMappings state map) where
--   arbitrary = getAll . foldMapOnDiskMappings getConstMap $ zipOnDiskMappings (\z w -> ConstMap . All $ z == w) x y

newtype DataMap (t :: ki) k v = DataMap (Map k v)
  deriving stock (Show, Eq, Generic)
  deriving newtype (Functor, Arbitrary)
  deriving (Semigroup, Monoid) via MonoidalMap k v

class (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v
instance (Ord k, Semigroup v) => KeysOrdValuesSemigroup t k v

class Ord k => KeysOrd t k v
instance Ord k => KeysOrd t k v

nullMappings :: forall ki (state :: StateKind ki). HasOnDiskMappings state => OnDiskMappings state NullMap
nullMappings = pureOnDiskMappings (NullMap :: forall (t :: ki) k v. NullMap t k v)

data ProductMap (map1 :: TableKind ki) (map2 :: TableKind ki) (t :: ki) k v = ProductMap (map1 t k v) (map2 t k v)

liftProductMapRight :: forall ki (state ::StateKind ki) (map1 :: TableKind ki) (map2 :: TableKind ki) (map3 :: TableKind ki).
  HasOnDiskMappings state
  => (OnDiskMappings state map2 -> OnDiskMappings state map3)
  -> OnDiskMappings state (map1 `ProductMap` map2) -> OnDiskMappings state (map1 `ProductMap` map3)
liftProductMapRight f x = zipOnDiskMappings (\(ProductMap l _) res -> ProductMap l res) x (f justRightMaps)
  where
    justRightMaps = mapOnDiskMappings (\(ProductMap _ r) -> r) x

liftOnDiskMappingsToComposeMap :: (Applicative f, HasOnDiskMappings state) => OnDiskMappings state map -> OnDiskMappings state (ComposeMap f map)
liftOnDiskMappingsToComposeMap = mapOnDiskMappings (ComposeMap . pure)

lowerProductMapLeft :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> OnDiskMappings state map1
lowerProductMapLeft = mapOnDiskMappings $ \(ProductMap x _) -> x

lowerProductMapRight :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> OnDiskMappings state map2
lowerProductMapRight = mapOnDiskMappings $ \(ProductMap _ y) -> y
newtype MaybeMap map t k v = MaybeMap (Maybe (map t k v))

odmProductMapToTuple :: HasOnDiskMappings state => OnDiskMappings state (ProductMap map1 map2) -> (OnDiskMappings state map1, OnDiskMappings state map2)
odmProductMapToTuple odm = (lowerProductMapLeft odm, lowerProductMapRight odm)

