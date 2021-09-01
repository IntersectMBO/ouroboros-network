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
module LedgerOnDisk.KVHandle.Class
  ( module LedgerOnDisk.KVHandle.Class
  , module LedgerOnDisk.KVHandle.OnDiskMappings
  ) where

import Data.Kind

import LedgerOnDisk.Mapping.PTMap
import LedgerOnDisk.KVHandle.OnDiskMappings

-- | An instance of 'DB state dbhandle' witnesses that we can perform db
-- operations on a dbhandle pertaining to type state.
class (HasConstrainedOnDiskMappings (DBKVConstraint state) state)
  => DB state dbhandle | dbhandle -> state where
  -- | Databases can put constraints on the keys and values they can store.
  -- For example, we may need a Binary Instance on keys and values and an Ord instance on keys
  --
  -- >>> class (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- >>> instance (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- ...
  -- type DBKVConstraint Foo = OrdKeysBinaryAll
  --
  -- The superclass constraint guarantees  state can only contain appropriate keys and values
  type DBKVConstraint state :: Type -> Type -> Constraint

  -- | The abstract type representing prepared - but not submitted - queries
  -- TODO rename to ReadHandle
  type ReadSet state dbhandle

  -- | Initiate an operation by preparing it. The 'DBRequest' holds keys
  -- | The consistent view of this data will be provided to the operation when it is submitted
  prepare :: dbhandle
          -> OnDiskMappings state Keys
          -> IO (ReadSet state dbhandle)

  -- | Submit a prepared operation
  submit :: dbhandle
    -> ReadSet state dbhandle
    -- ^ The ReadSet returned from an earlier call to prepare on this handle
    -> (OnDiskMappings state PTMap -> (a, OnDiskMappings state DiffMap))
    -- ^ The operation to perform. This function will be supplied with the data that was requested by 'prepare'.
    -- It should return the changes to apply
    -> IO a
