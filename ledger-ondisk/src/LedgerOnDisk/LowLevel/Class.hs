{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.LowLevel.Class where


import Data.Kind
import LedgerOnDisk.KVHandle.OnDiskMappings
import LedgerOnDisk.KVHandle.RangeQuery
import Data.Int
import LedgerOnDisk.Mapping.PTMap (DiffMap)
import Control.Concurrent.STM

type SeqId = Int64

class
  ( HasConstrainedOnDiskMappings (DBKVConstraint rohandle) (ROLLT rohandle)
  ) =>
  DBROLowLevel rohandle where
  -- | Databases can put constraints on the keys and values they can store.
  -- For example, we may need a Binary Instance on keys and values and an Ord instance on keys
  --
  -- >>> class (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- >>> instance (Ord k, Binary k, Binary v) => OrdKeysBinaryAll k v
  -- ...
  -- type DBKVConstraint Foo = OrdKeysBinaryAll
  --
  -- The superclass constraint guarantees  state can only contain appropriate keys and values
  type DBKVConstraint rohandle :: Type -> Type -> Constraint
  type ROLLT rohandle :: (Type -> Type -> Type) -> Type
  readSnapshot :: rohandle -> OnDiskMappings (ROLLT rohandle) Query -> IO (OnDiskMappings (ROLLT rohandle) QueryResult)
  closeHandle :: rohandle -> IO ()

class
  ( DBROLowLevel (ROHandle dbhandle)
  , ROLLT (ROHandle dbhandle) ~ LLT dbhandle
  )
  => DBLowLevel dbhandle where
  type LLT dbhandle :: (Type -> Type -> Type) -> Type


  -- TODO clients would need to be able to serialise this type
  -- perhaps (Binary (ROHandle dbhandle)) as a superclass constraint is adequate?
  type ROHandle dbhandle :: Type

  read :: dbhandle -> OnDiskMappings (LLT dbhandle) Query -> IO (SeqId, OnDiskMappings (LLT dbhandle) QueryResult)
  write :: dbhandle ->OnDiskMappings (LLT dbhandle) DiffMap -> IO SeqId

  getSeqId :: dbhandle -> STM SeqId

  snapshot :: dbhandle -> IO (ROHandle dbhandle)
