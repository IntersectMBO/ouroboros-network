{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}

module LedgerOnDisk.KVHandle.ClassWithSnapshots where

import Data.Kind

import Data.Map.Monoidal (MonoidalMap(MonoidalMap))
import Data.Map (Map)
import Control.Comonad

import LedgerOnDisk.Mapping.PTMap
import LedgerOnDisk.KVHandle.OnDiskMappings
import LedgerOnDisk.KVHandle.RangeQuery


newtype DBRequest dbhandle = DBRequest
  { getDBRequest :: Map (Maybe (Snapshot dbhandle)) (DBOnDiskMappings dbhandle Query) }

deriving stock instance
  ( Eq (DBOnDiskMappings dbhandle Query)
  , Eq (Snapshot dbhandle)
  ) => Eq (DBRequest dbhandle)

deriving stock instance
  ( Show (DBOnDiskMappings dbhandle Query)
  , Show (Snapshot dbhandle)
  ) => Show (DBRequest dbhandle)

deriving via MonoidalMap (Maybe (Snapshot dbhandle)) (DBOnDiskMappings dbhandle Query)
  instance (Ord (Snapshot dbhandle), Semigroup  (DBOnDiskMappings dbhandle Query))
    => Semigroup (DBRequest dbhandle)

deriving via MonoidalMap (Maybe (Snapshot dbhandle)) (DBOnDiskMappings dbhandle Query)
  instance (Monoid (DBOnDiskMappings dbhandle Query), Ord (Snapshot dbhandle))
    => Monoid (DBRequest dbhandle)

-- | The return value for 'submit'
data DBResponse dbhandle a = DBResponse
  { payload :: a -- ^ An arbitrary return value. Easy access via 'extract'. Passed directly from 'DBOpResult'
  , snapshot :: !(Maybe (Snapshot dbhandle)) -- ^ The snapshot, if it was requested in 'DbOpResult'
  }
  deriving stock (Functor)

instance Comonad (DBResponse dbhandle) where
  extract = payload
  duplicate x = x <$ x

data DBOpArgs dbhandle = DBOpArgs
  { pointQueryResults :: !(DBOnDiskMappings dbhandle PTMap)
  , rangeQueryResults :: !(DBOnDiskMappings dbhandle Map)
  }

deriving stock instance
  ( Show (DBOnDiskMappings dbhandle Map)
  , Show (DBOnDiskMappings dbhandle PTMap)
  , Show (Snapshot dbhandle)
  ) => Show (DBOpArgs dbhandle)

deriving stock instance
  ( Eq (DBOnDiskMappings dbhandle Map)
  , Eq (DBOnDiskMappings dbhandle PTMap)
  , Eq (Snapshot dbhandle)
  ) => Eq (DBOpArgs dbhandle)

-- | The return value for a DB operation, see 'submit'.
-- This doesn't depend on a dbhandle, but rather state, which one should think
-- of as (T dbhandle) for some dbhandle
data DBOpResult state a = DBOpResult
  { payload :: a -- ^ An arbitrary return value. Easy access via 'extract'
  , changes :: !(OnDiskMappings state DiffMap) -- ^ Mutations to apply.
  , snapshot :: !Bool -- ^ Should a snapshot be taken?
  } deriving stock (Functor)

deriving stock instance (Eq a, Eq (OnDiskMappings state DiffMap))
  => Eq (DBOpResult state a)

deriving stock instance (Show a, Show (OnDiskMappings state DiffMap))
  => Show (DBOpResult state a)

instance Comonad (DBOpResult state) where
  extract = payload
  duplicate x = x <$ x

type DBOnDiskMappings dbhandle = OnDiskMappings (T dbhandle)

-- | An instance of 'DB state dbhandle' witnesses that we can perform db
-- operations on a dbhandle pertaining to type state.
class
  ( HasConstrainedOnDiskMappings (DBKVConstraint dbhandle) (T dbhandle)
  , Ord (Snapshot dbhandle)
  , HasConstrainedOnDiskMappings (SemigroupMap Query) (T dbhandle)
  ) => DB dbhandle where

  -- | Databases can put constraints on the keys and values they can store.
  -- For example, we may need a Binary Instance on keys and values and an Ord instance on keys
  --
  -- >>> class (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- >>> instance (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- ...
  -- type DBKVConstraint Foo = OrdKeysBinaryAll
  --
  -- The superclass constraint guarantees  state can only contain appropriate keys and values
  type DBKVConstraint dbhandle :: Type -> Type -> Constraint

  -- | The type the DB stores.
  -- TODO better name plz
  type T dbhandle :: (Type -> Type -> Type) -> Type

  -- | The abstract type representing prepared - but not submitted - queries
  data ReadHandle dbhandle

  -- | The abstract type naming snapshots of history
  -- This type is expected to be small, just an Id and maybe some flags
  data Snapshot dbhandle

  -- | Initiate an operation by preparing it. The 'DBRequest' holds keys and cursors, each associated with a timeline.
  -- | The consistent view of this data will be provided to the operation when it is submitted
  prepare :: dbhandle
          -> DBRequest dbhandle
          -> IO (ReadHandle dbhandle)

  -- | Submit a prepared operation
  submit :: ReadHandle dbhandle
    -- ^ The ReadSet returned from an earlier call to prepare on this handle
    -> (DBOpArgs dbhandle -> DBOpResult (T dbhandle) a)
    -- ^ The operation to perform. This function will be supplied with the data that was requested by 'prepare'.
    -- It should return a 'DbOpResult', containing mutations to apply,
    -- instructions on snapshotting, as well as an arbitrary 'a' payload.
    -> IO (DBResponse dbhandle a)

  rollback :: dbhandle -> Snapshot dbhandle -> IO ()
