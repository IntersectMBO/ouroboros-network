{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DuncanAndDouglas.DbChangelog where

import           Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

data DbChangelog (state :: (TableType -> Type -> Type -> Type) -> Type) =  DbChangelog

data TableType = TableTypeRO  -- ^ Read only
               | TableTypeRW  -- ^ Read and write
               | TableTypeRWU -- ^ Read, write and monoidal update

extendDbChangelog :: forall state.
                     HasOnDiskTables state
                  => SeqNo state
                  -> state TableDiff
                  -> Maybe (TableSnapshots state)
                  -> DbChangelog state
                  -> DbChangelog state
extendDbChangelog !seqno !state Nothing = undefined

class (HasTables state, HasOnlyTables (Tables state))
   => HasOnDiskTables state where

  data Tables state :: StateKind

  projectTables :: state table -> Tables state table
  injectTables  :: Tables state table -> state any -> state table

type TableKind = TableType -> Type -> Type -> Type
type StateKind = TableKind -> Type

newtype SeqNo (state :: StateKind) = SeqNo { seqnoToWord :: Word64 }
  deriving (Eq, Ord, Show)

data PairTable tablea tableb (t :: TableType) k v =
       PairTable (tablea t k v) (tableb t k v)
  deriving Show

data TableDiff (t :: TableType) k v where
     TableDiffRO  ::                   TableDiff TableTypeRO  k v
     TableDiffRW  :: DiffMapRW  k v -> TableDiff TableTypeRW  k v
     -- we don't need this for the sake of the present exercise
     -- TableDiffRWU :: DiffMapRWU k v -> TableDiff TableTypeRWU k v

newtype DiffMapRW k v = DiffMapRW (Map.Map k (DiffMapRWElem v))
  deriving (Eq, Show)

data DiffMapRWElem a = MapRWElemDelete
                     | MapRWElemInsert !a
  deriving (Eq, Show)

data TableSnapshots state where

     TableSnapshots :: (forall table. Tables state table
                                   -> Tables state (SnapshotOfTable table))
                    -> TableSnapshots state

data SnapshotOfTable (table :: TableType -> Type -> Type -> Type) (t :: TableType) k v where
       KeepTable       ::                SnapshotOfTable table t           k v
       SnapshotOfTable :: table t k v -> SnapshotOfTable table TableTypeRO k v

class HasTables (state :: StateKind) where
  type StateTableKeyConstraint   state :: Type -> Constraint
  type StateTableValueConstraint state :: Type -> Constraint

  traverseTables :: Applicative f
                   => (forall (t :: TableType) k v.
                              Ord k
                           => StateTableKeyConstraint   state k
                           => StateTableValueConstraint state v
                           => TableTag t v -> table t k v -> f (table' t k v))
                   -> state table -> f (state table')

  traverseTables_ :: Applicative f
                    => (forall (t :: TableType) k v.
                               Ord k
                            => StateTableKeyConstraint   state k
                            => StateTableValueConstraint state v
                            => TableTag t v -> table t k v -> f ())
                    -> state table -> f ()

  foldMapTables :: Monoid a
                => (forall (t :: TableType) k v.
                   Ord k
                => StateTableKeyConstraint   state k
                => StateTableValueConstraint state v
                => TableTag t v -> table t k v -> a)
                -> state table -> a

  -- The common pure case, not needing Applicative
  mapTables :: (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v)
            -> state table -> state table'
  mapTables f = undefined

data TableTag (t :: TableType) v where
       TableTagRO  ::                TableTag TableTypeRO  v
       TableTagRW  ::                TableTag TableTypeRW  v
       TableTagRWU :: Semigroup v => TableTag TableTypeRWU v

class HasTables state => HasOnlyTables state where
  traverse0Tables :: Applicative f
                  => (forall (t :: TableType) k v.
                             Ord k
                          => StateTableKeyConstraint   state k
                          => StateTableValueConstraint state v
                          => TableTag t v -> f (table t k v))
                  -> f (state table)

  traverse2Tables :: Applicative f
                  => (forall (t :: TableType) k v.
                             Ord k
                          => StateTableKeyConstraint   state k
                          => StateTableValueConstraint state v
                          => TableTag t v -> table t k v -> table' t k v -> f (table'' t k v))
                  -> state table -> state table' -> f (state table'')

  traverse2Tables_ :: Applicative f
                   => (forall (t :: TableType) k v.
                              Ord k
                           => StateTableKeyConstraint   state k
                           => StateTableValueConstraint state v
                           => TableTag t v -> table t k v -> table' t k v -> f ())
                   -> state table -> state table' -> f ()

  -- The common pure case, not needing Applicative
  constTables :: (forall (t :: TableType) k v.
                         Ord k
                      => StateTableKeyConstraint   state k
                      => StateTableValueConstraint state v
                      => TableTag t v -> table t k v)
              -> state table
  constTables f = undefined

  zipTables  :: forall table table' table''.
                (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v -> table'' t k v)
             -> state table -> state table' -> state table''
  zipTables f a b = undefined

  zip3Tables  :: forall table table' table'' table'''.
                 (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v -> table'' t k v -> table''' t k v)
             -> state table -> state table' -> state table'' -> state table'''
  zip3Tables f a b c = undefined
