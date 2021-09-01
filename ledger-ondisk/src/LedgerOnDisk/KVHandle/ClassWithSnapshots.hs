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

import LedgerOnDisk.Mapping.PTMap
import LedgerOnDisk.KVHandle.OnDiskMappings

import Data.Kind
import Data.Int

import Data.Map.Monoidal (MonoidalMap(MonoidalMap))
import Data.Map (Map)
import qualified Data.Semigroup as Semi
import Control.Comonad

type CursorId = Int

type instance SemigroupMap (CursorRequest h) = UnrestrictedKeysAndValues

data CursorRequest0 h k v
  = NewCursor
    { snapshot :: !(Snapshot h)
    , initialNumRows :: !Int64
    }
  | ReadCursor
    { resumeFrom :: !(Cursor h k v)
    , numRows :: !Int64
    }
  | DisposeCursor !(Cursor h k v)

deriving stock instance (Eq (Cursor h k v), Eq (Snapshot h))
  => Eq (CursorRequest0 h k v)

deriving stock instance (Show (Cursor h k v), Show (Snapshot h))
  => Show (CursorRequest0 h k v)

newtype CursorRequest s k v = CursorRequest
  { getCursorRequest :: Map CursorId (CursorRequest0 s k v) }
  deriving (Semigroup, Monoid) via MonoidalMap CursorId (Semi.Last (CursorRequest0 s k v))

deriving stock instance (Eq (Cursor h k v), Eq (Snapshot h)) => Eq (CursorRequest h k v)
deriving stock instance (Show (Cursor h k v), Show (Snapshot h)) => Show (CursorRequest h k v)

data CursorResponse0 h k v
  = CursorFinished
  | CursorRows
    { remainingRows :: !Int64
    , returnedRows :: !Int64
    , nextCursor :: !(Cursor h k v)
    , rows :: ![(k, v)]
    }

deriving stock instance (Eq (Cursor h k v), Eq k, Eq v) => Eq (CursorResponse0 h k v)
deriving stock instance (Show (Cursor h k v), Show k, Show v) => Show (CursorResponse0 h k v)

newtype CursorResponse h k v = CursorResponse
  { getCursorResponse :: Map CursorId (CursorResponse0 h k v) }
  deriving (Semigroup, Monoid) via MonoidalMap CursorId (Semi.Last (CursorResponse0 h k v))

deriving stock instance (Eq (Cursor h k v), Eq k, Eq v) => Eq (CursorResponse h k v)
deriving stock instance (Show (Cursor h k v), Show k, Show v) => Show (CursorResponse h k v)

data DBRequest dbhandle = DBRequest
  { cursorRequests :: !(Map CursorId (DBOnDiskMappings dbhandle (CursorRequest dbhandle)))
  , kvRequests :: !(Map (Maybe (Snapshot dbhandle)) (DBOnDiskMappings dbhandle Keys))
  }

deriving stock instance
  ( Eq (DBOnDiskMappings dbhandle (CursorRequest dbhandle))
  , Eq (DBOnDiskMappings dbhandle Keys)
  , Eq (Snapshot dbhandle)
  ) => Eq (DBRequest dbhandle)

deriving stock instance
  ( Show (DBOnDiskMappings dbhandle (CursorRequest dbhandle))
  , Show (DBOnDiskMappings dbhandle Keys)
  , Show (Snapshot dbhandle)
  ) => Show (DBRequest dbhandle)

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
  { cursorResults :: !(Map CursorId (DBOnDiskMappings dbhandle (CursorResponse dbhandle)))
  , kvResults :: !(Map (Maybe (Snapshot dbhandle)) (DBOnDiskMappings dbhandle PTMap))
  }

-- | The return value for a DB operation, see 'submit'.
-- This doesn't depend on a dbhandle, but rather state, which one should think
-- of as (T dbhandle) for some dbhandle
data DBOpResult state a = DBOpResult
  { payload :: a -- ^ An arbitrary return value. Easy access via 'extract'
  , changes :: !(OnDiskMappings state DiffMap) -- ^ Mutations to apply.
  , snapshot :: !Bool -- ^ Should a snapshot be taken?
  } deriving stock (Functor)

-- -- | Describes how we should apply mutations in a DB operation.
-- -- These applications are relative to a "current" timeline, see 'submit'
-- data BranchTimelines
--   = NewTimeline     -- ^ Do not apply mutations to the current timeline, but fork a new timeline with the changes
--   | CurrentTimelineAndSnapshot -- ^ Apply mutations to the current timeline, and return a new timeline snapshotting the result
--   | CurrentTimeline -- ^ Apply mutations to the current timeline, do not snapshot the result
--   | DiscardChanges -- ^ Do not apply the mutations
--   deriving stock (Eq, Show, Enum, Ord)


instance Comonad (DBOpResult state) where
  extract = payload
  duplicate x = x <$ x


type DBOnDiskMappings dbhandle = OnDiskMappings (T dbhandle)

-- | An instance of 'DB state dbhandle' witnesses that we can perform db
-- operations on a dbhandle pertaining to type state.
class
  ( HasConstrainedOnDiskMappings (DBKVConstraint dbhandle) (T dbhandle)
  , Ord (Snapshot dbhandle)
  , Monoid (DBRequest dbhandle)
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

  type T dbhandle :: (Type -> Type -> Type) -> Type

  data Cursor dbhandle k v

  -- | The abstract type representing prepared - but not submitted - queries
  data ReadHandle dbhandle

  -- | The abstract type naming branches of history.
  -- In general Timelines are mutable. Akin to a git branch rather than a git tag.
  -- Immutable snapshots can be implemented with this either by discipline, or
  -- distinguishing between mutable and immutable timelines in their
  -- implemtation of `Timeline`
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
