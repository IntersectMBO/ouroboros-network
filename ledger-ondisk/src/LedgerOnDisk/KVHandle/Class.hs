{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module LedgerOnDisk.KVHandle.Class where

import Control.Lens
-- import LedgerOnDisk.Mapping.Class
import LedgerOnDisk.Mapping.PTMap
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Kind

type role Keys nominal nominal
newtype Keys k a = Keys (Set k)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

type role NullMap nominal nominal
data NullMap k a = NullMap

instance Semigroup (NullMap k a) where
  _ <> _ = NullMap

instance Monoid (NullMap k a) where
  mempty = NullMap

-- | An instance 'HasOnDiskMappings state' witnesses a type of the shape `(Type -> Type -> Type) -> Type` embeds
-- a collection of maps 'OnDiskMappings state'. We can think of 'state map' as being isomorphic to '(t, OnDiskMappings state map)'
-- where t is some 'other' data, and the 'OnDiskMappings state' is a collection of maps
class HasOnDiskMappings state where
  data OnDiskMappings state :: (Type -> Type -> Type) -> Type
  onDiskMappingsLens ::  Lens (state map1) (state map2) (OnDiskMappings state map1) (OnDiskMappings state map2)
  nullMap :: OnDiskMappings state NullMap

-- | An instance 'HasConstrainedOnDiskMappings c state' gives a collection of methods for working with OnDiskMappings.
-- 'c :: Type -> Type -> Constraint' is a constraint operations will assume on all k, v , for each 'map k v' in the OnDiskMappings
--
-- I think this is some kind of higher-order 'Zip' from 'semialign'
class HasOnDiskMappings state => HasConstrainedOnDiskMappings c state where
  -- | Only required method. Example:
  -- >>> class Ord k => KeyOrd k v
  -- >>> instance Ord k => KeyOrd k v
  -- >>> zipMappings (Proxy @ KeyOrd) Map.union (map1 :: OnDiskMappings state Map) (map2 :: OnDiskMappings state Map)
  zipMappings :: forall f map1 map2 map3 proxy. Applicative f
    => proxy c
    -> (forall k v. (c k v) => map1 k v -> map2 k v -> f (map3 k v))
    -> OnDiskMappings state map1 -> OnDiskMappings state map2 -> f (OnDiskMappings state map3)

  mapMappings :: forall proxy map1 map2. ()
    => proxy c
    -> (forall k v. c k v => map1 k v -> map2 k v)
    -> OnDiskMappings state map1 -> OnDiskMappings state map2
  mapMappings p f = runIdentity . traverseMappings p (pure . f)

  traverseMappings :: forall f map1 map2 proxy. (Applicative f)
    => proxy c
    -> (forall k v. (c k v) => map1 k v -> f (map2 k v))
    -> OnDiskMappings state map1
    -> f (OnDiskMappings state map2)
  traverseMappings p f m = zipMappings p (\_ x -> f x) nullMap m


class (HasConstrainedOnDiskMappings (DBKVConstraint state) state)
  => DB state dbhandle | dbhandle -> state where
  type DBKVConstraint state :: Type -> Type -> Constraint

  type ReadSet state dbhandle
  prepare :: dbhandle
          -> OnDiskMappings state Keys
          -> IO (ReadSet state dbhandle)

  submit :: dbhandle
    -> ReadSet state dbhandle
    -> (OnDiskMappings state PTMap -> (a, OnDiskMappings state DiffMap))
    -> IO a
