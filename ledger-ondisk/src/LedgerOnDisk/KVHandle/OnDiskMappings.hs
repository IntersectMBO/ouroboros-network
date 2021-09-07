{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
module LedgerOnDisk.KVHandle.OnDiskMappings where


import Data.Kind
import Control.Lens
import Data.Hashable
import LedgerOnDisk.Mapping.PTMap
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import Data.Proxy
import Data.Set (Set)
import Test.QuickCheck

-- | An instance 'HasOnDiskMappings state' witnesses a type of the shape `(Type -> Type -> Type) -> Type` embeds
-- a collection of maps 'OnDiskMappings state'. We can think of 'state map' as being isomorphic to '(t, OnDiskMappings state map)'
-- where t is some 'other' data, and the 'OnDiskMappings state' is a collection of maps
class HasOnDiskMappings state where
  data OnDiskMappings state :: (Type -> Type -> Type) -> Type

  onDiskMappingsLens ::  Lens (state map1) (state map2)
    (OnDiskMappings state map1) (OnDiskMappings state map2)

  projectOnDiskMappings :: state map -> OnDiskMappings state map
  projectOnDiskMappings = view onDiskMappingsLens
  injectOnDiskMappings :: OnDiskMappings state map -> state any -> state map
  injectOnDiskMappings a s = s & onDiskMappingsLens .~ a

  nullMap :: OnDiskMappings state NullMap

type role NullMap nominal nominal
data NullMap k a = NullMap
  deriving stock (Eq, Show)

instance Semigroup (NullMap k a) where
  _ <> _ = NullMap

instance Monoid (NullMap k a) where
  mempty = NullMap

type role Keys nominal nominal
newtype Keys k a = Keys (Set k)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, Arbitrary)

instance forall (state :: (Type -> Type -> Type) -> Type) (map :: Type -> Type -> Type) (c :: Type -> Type -> Constraint).
  ( SemigroupMap map ~ c
  , HasConstrainedOnDiskMappings c state
  , forall k v. c k v => Semigroup (map k v)
  )
  => Semigroup (OnDiskMappings state map) where
  (<>) :: OnDiskMappings state map -> OnDiskMappings state map -> OnDiskMappings state map
  x <> y = let
    proxy :: Proxy c
    proxy = Proxy
    in runIdentity $ zipMappings proxy (\l r -> pure $ l <> r) x y

-- | A type family to compute the constraints on the keys and values of maps to grant a semigroup instance on (map k v)
-- i.e. morally `SemigroupMap map k v => Semigroup (Map k v)`
type family SemigroupMap (map :: (Type -> Type -> Type)) :: (Type -> Type -> Constraint)

type instance SemigroupMap Map = KeysOrdValuesSemigroups
type instance SemigroupMap HashMap = KeysHashableValuesSemigroups
type instance SemigroupMap NullMap = UnrestrictedKeysAndValues
type instance SemigroupMap Keys = KeysAreOrd
type instance SemigroupMap DiffMap = KeysAreOrd
-- type instance SemigroupMap PTMap = KeysAreOrd

class (Ord k) => KeysAreOrd k v
instance (Ord k) => KeysAreOrd k v

class UnrestrictedKeysAndValues k v
instance UnrestrictedKeysAndValues k v

class (Eq k, Hashable k) => KeysAreHashable k v
instance (Eq k, Hashable k) => KeysAreHashable k v

class (Monoid v) => ValuesAreSemigroups k v
instance ( Monoid v) => ValuesAreSemigroups k v

class (KeysAreOrd k v, ValuesAreSemigroups k v) => KeysOrdValuesSemigroups k v
instance (KeysAreOrd k v, ValuesAreSemigroups k v) => KeysOrdValuesSemigroups k v

class (KeysAreHashable k v, ValuesAreSemigroups k v) => KeysHashableValuesSemigroups k v
instance (KeysAreHashable k v, ValuesAreSemigroups k v) => KeysHashableValuesSemigroups k v

-- | An instance 'HasConstrainedOnDiskMappings c state' gives a collection of methods for working with OnDiskMappings.
-- 'c :: Type -> Type -> Constraint' is a constraint operations will assume on all k, v , for each 'map k v' in the OnDiskMappings
--
-- I think this is some kind of higher-order 'Zip' from 'semialign'
class HasOnDiskMappings state => HasConstrainedOnDiskMappings (c :: Type -> Type -> Constraint) state where
  {-# MINIMAL zipMappings #-}
  -- | Only required method. Example:
  -- >>> class Ord k => KeyOrd k v
  -- >>> instance Ord k => KeyOrd k v
  -- >>> zipMappings (Proxy @ KeyOrd) Map.union (map1 :: OnDiskMappings state Map) (map2 :: OnDiskMappings state Map)
  zipMappings :: forall f (proxy :: (Type -> Type -> Constraint) -> Type) map1 map2 map3. Applicative f
    => proxy c
    -> (forall k v. (c k v) => map1 k v -> map2 k v -> f (map3 k v))
    -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)

  mapMappings :: forall proxy map1 map2. ()
    => proxy c
    -> (forall k v. c k v => map1 k v -> map2 k v)
    -> OnDiskMappings state map1 -> OnDiskMappings state map2
  mapMappings p f = runIdentity . traverseMappings p (pure . f)

  traverseMappings :: forall f proxy map1 map2. (Applicative f)
    => proxy c
    -> (forall k v. (c k v) => map1 k v -> f (map2 k v))
    -> OnDiskMappings state map1
    -> f (OnDiskMappings state map2)
  traverseMappings p f m = zipMappings p (\_ x -> f x) nullMap m

