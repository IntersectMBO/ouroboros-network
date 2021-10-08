{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Snapshots where

import           Prelude hiding (lookup)

import           Data.Kind
import           Data.Functor.Identity

--import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.FingerTree as FT
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Word (Word64)

--import           Control.Monad
import qualified Control.Monad.ST.Strict as ST
import           Control.Monad.ST.Strict (ST)
--import           Control.Monad.Trans.State as State
--import           Control.Concurrent.STM (STM, TVar)
--import qualified Control.Concurrent.STM as STM
import           Control.Exception


--
-- Diffs
--------

class Monoid (Diff a) => Changes a where
  data Diff a

  applyDiff :: a -> Diff a -> a


--
-- Mappings
-----------

class MappingR map where
  lookup :: Ord k => k      -> map k v -> Maybe v

class MappingW map where
  insert :: Ord k => k -> v -> map k v -> map k v
  delete :: Ord k => k      -> map k v -> map k v

class MappingW map => MappingU map where
  update :: Semigroup v
         => Ord k => k -> v -> map k v -> map k v

instance MappingR Map.Map where
  lookup = Map.lookup

instance MappingW Map.Map where
  insert = Map.insert
  delete = Map.delete

instance MappingU Map.Map where
  update = Map.insertWith (<>)


newtype MapRO  k v = MapRO  (Map.Map k v)
  deriving (Eq, Show, MappingR)

newtype MapRW  k v = MapRW  (Map.Map k v)
  deriving (Eq, Show, MappingR, MappingW)

newtype MapRWU k v = MapRWU (Map.Map k v)
  deriving (Eq, Show, MappingR, MappingW, MappingU)


--
-- Partial Mappings
-------------------

data PMapRep k v = PMapRep !(Map.Map k v) !(Set.Set k)
  deriving (Eq, Show)

mkPMapRep :: Ord k => Map.Map k v -> Set.Set k -> PMapRep k v
mkPMapRep m ks =
    PMapRep present absent
  where
    present = Map.restrictKeys m ks
    absent  = ks Set.\\ Map.keysSet present


instance MappingR PMapRep where
  lookup k (PMapRep present absent) =
    case Map.lookup k present of
      Just v        -> Just v
      Nothing
        | Set.member k absent
                    -> Nothing
        | otherwise -> error "PMap.lookup: used a key not fetched from disk"

instance MappingW PMapRep where
  insert k v (PMapRep present absent) = PMapRep (Map.insert k v present) absent
  delete k   (PMapRep present absent) = PMapRep (Map.delete k   present) absent

instance MappingU PMapRep where
  update k v (PMapRep present absent) = PMapRep (Map.insertWith (<>) k v present) absent


newtype PMapRO  k v = PMapRO  (PMapRep k v)
  deriving (Eq, Show, MappingR)

newtype PMapRW  k v = PMapRW  (PMapRep k v)
  deriving (Eq, Show, MappingR, MappingW)

newtype PMapRWU k v = PMapRWU (PMapRep k v)
  deriving (Eq, Show, MappingR, MappingW, MappingU)


--
-- Diffs on mappings
--------------------

data MapRWDiffElem a = MapRWElemDelete
                     | MapRWElemInsert !a
  deriving (Eq, Show)

instance Semigroup (MapRWDiffElem a) where
  _ <> y = y --TODO: check if this should be left or right biased

instance Ord k => Changes (MapRW k v) where
  newtype Diff (MapRW k v) = MapRWDiff (Map.Map k (MapRWDiffElem v))
    deriving (Eq, Show)

  applyDiff :: MapRW k v -> Diff (MapRW k v) -> MapRW k v
  applyDiff (MapRW m) (MapRWDiff md) =
      MapRW (Map.merge
               Map.preserveMissing
               (Map.mapMaybeMissing      insertDiffElem)
               (Map.zipWithMaybeMatched  applyDiffElem)
               m
               md)
    where
      insertDiffElem :: k -> MapRWDiffElem a -> Maybe a
      insertDiffElem _    MapRWElemDelete     = Nothing
      insertDiffElem _ (  MapRWElemInsert x)  = Just x

      applyDiffElem :: k -> a -> MapRWDiffElem a -> Maybe a
      applyDiffElem _ _    MapRWElemDelete     = Nothing
      applyDiffElem _ _ (  MapRWElemInsert x)  = Just x

instance Ord k => Monoid (Diff (MapRW k v)) where
  mempty = MapRWDiff Map.empty

instance Ord k => Semigroup (Diff (MapRW k v)) where
  MapRWDiff a <> MapRWDiff b = MapRWDiff (Map.unionWith (<>) a b)


newtype DiffMapRW k v = DiffMapRW (Diff (MapRW k v))
  deriving (Eq, Show, Semigroup, Monoid)

instance MappingW DiffMapRW where
  insert k v (DiffMapRW (MapRWDiff m)) =
    DiffMapRW (MapRWDiff (Map.insert k (MapRWElemInsert v) m))

  delete k (DiffMapRW (MapRWDiff m)) =
    DiffMapRW (MapRWDiff (Map.insert k  MapRWElemDelete m))


data MapRWUDiffElem a = MapRWUElemDelete
                      | MapRWUElemInsert !a
                      | MapRWUElemUpdate !a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (MapRWUDiffElem a) where
  _                  <> MapRWUElemDelete   = MapRWUElemDelete
  _                  <> MapRWUElemInsert y = MapRWUElemInsert  y

  MapRWUElemDelete   <> MapRWUElemUpdate y = MapRWUElemInsert  y
  MapRWUElemInsert x <> MapRWUElemUpdate y = MapRWUElemInsert (x <> y)
  MapRWUElemUpdate x <> MapRWUElemUpdate y = MapRWUElemUpdate (x <> y)

instance (Ord k, Semigroup v) => Changes (MapRWU k v) where
  newtype Diff (MapRWU k v) = MapRWUDiff (Map.Map k (MapRWUDiffElem v))
    deriving (Eq, Show)

  applyDiff :: MapRWU k v -> Diff (MapRWU k v) -> MapRWU k v
  applyDiff (MapRWU m) (MapRWUDiff md) =
      MapRWU (Map.merge
                Map.preserveMissing
                (Map.mapMaybeMissing      insertDiffElem)
                (Map.zipWithMaybeMatched  applyDiffElem)
                m
                md)
    where
      insertDiffElem :: k -> MapRWUDiffElem v -> Maybe v
      insertDiffElem _    MapRWUElemDelete     = Nothing
      insertDiffElem _ (  MapRWUElemInsert x)  = Just x
      insertDiffElem _ (  MapRWUElemUpdate x)  = Just x

      applyDiffElem :: k -> v -> MapRWUDiffElem v -> Maybe v
      applyDiffElem _ _    MapRWUElemDelete     = Nothing
      applyDiffElem _ _ (  MapRWUElemInsert y)  = Just y
      applyDiffElem _ x (  MapRWUElemUpdate y)  = Just (x<>y)

instance (Ord k, Semigroup v) => Monoid (Diff (MapRWU k v)) where
  mempty = MapRWUDiff Map.empty

instance (Ord k, Semigroup v) => Semigroup (Diff (MapRWU k v)) where
  MapRWUDiff a <> MapRWUDiff b = MapRWUDiff (Map.unionWith (<>) a b)


newtype DiffMapRWU k v = DiffMapRWU (Diff (MapRWU k v))
  deriving (Eq, Show, Semigroup, Monoid)

instance MappingW DiffMapRWU where
  insert k v (DiffMapRWU (MapRWUDiff m)) =
    DiffMapRWU (MapRWUDiff (Map.insert k (MapRWUElemInsert v) m))

  delete k (DiffMapRWU (MapRWUDiff m)) =
    DiffMapRWU (MapRWUDiff (Map.insert k  MapRWUElemDelete m))

instance MappingU DiffMapRWU where
  update k v (DiffMapRWU (MapRWUDiff m)) =
    DiffMapRWU (MapRWUDiff (Map.insertWith (<>) k (MapRWUElemUpdate v) m))


--
-- Mappings that track diffs
----------------------------


data PTMapRW  k v = PTMapRW !(PMapRW k v) !(DiffMapRW k v)
  deriving (Eq, Show)

instance MappingR PTMapRW where
  lookup k (PTMapRW m _d) = lookup k m

instance MappingW PTMapRW where
  insert k v (PTMapRW m d) = PTMapRW (insert k v m) (insert k v d)
  delete k   (PTMapRW m d) = PTMapRW (delete k   m) (delete k   d)

mkPTMapRW :: PMapRW k v -> PTMapRW k v
mkPTMapRW pm = PTMapRW pm (DiffMapRW (MapRWDiff Map.empty))

getDiffMapRW :: PTMapRW k v -> DiffMapRW k v
getDiffMapRW (PTMapRW _ d) = d


data PTMapRWU k v = PTMapRWU !(PMapRWU k v) !(DiffMapRWU k v)
  deriving (Eq, Show)

instance MappingR PTMapRWU where
  lookup k (PTMapRWU m _d) = lookup k m

instance MappingW PTMapRWU where
  insert k v (PTMapRWU m d) = PTMapRWU (insert k v m) (insert k v d)
  delete k   (PTMapRWU m d) = PTMapRWU (delete k   m) (delete k   d)

instance MappingU PTMapRWU where
  update k v (PTMapRWU m d) = PTMapRWU (update k v m) (update k v d)

mkPTMapRWU :: PMapRWU k v -> PTMapRWU k v
mkPTMapRWU pm = PTMapRWU pm (DiffMapRWU (MapRWUDiff Map.empty))

getDiffMapRWU :: PTMapRWU k v -> DiffMapRWU k v
getDiffMapRWU (PTMapRWU _ d) = d


--
-- Tables
---------

data TableType = TableTypeRO  -- ^ Read only
               | TableTypeRW  -- ^ Read and write
               | TableTypeRWU -- ^ Read, write and monoidal update

data TableTag (t :: TableType) v where
       TableTagRO  ::                TableTag TableTypeRO  v
       TableTagRW  ::                TableTag TableTypeRW  v
       TableTagRWU :: Semigroup v => TableTag TableTypeRWU v

deriving instance Show (TableTag k v)

class HasTableTag t v where
    tableTag :: TableTag t v

withTableTag :: TableTag t v -> (HasTableTag t v => a) -> a
withTableTag TableTagRO  x = x
withTableTag TableTagRW  x = x
withTableTag TableTagRWU x = x

instance HasTableTag TableTypeRO v where
    tableTag = TableTagRO

instance HasTableTag TableTypeRW v where
    tableTag = TableTagRW

instance Semigroup v => HasTableTag TableTypeRWU v where
    tableTag = TableTagRWU

data EmptyTable (t :: TableType) k v where
       EmptyTable :: EmptyTable t k v
  deriving Show

newtype ConstTable a (t :: TableType) k v where
       ConstTable :: a -> ConstTable a t k v
  deriving Show


data TableKeySet (t :: TableType) k v where
       TableKeySet :: Set.Set k
                   {- range queries go here -}
                   -> TableKeySet t k v
  deriving Show


-- | A table with an annotation: some extra data carried with it.
--
data AnnTable table a (t :: TableType) k v =
       AnnTable !(table t k v) !a


data TableReadSet (t :: TableType) k v where
     TableRO  :: PMapRO  k v -> TableReadSet TableTypeRO  k v
     TableRW  :: PMapRW  k v -> TableReadSet TableTypeRW  k v
     TableRWU :: PMapRWU k v -> TableReadSet TableTypeRWU k v


data TrackingTable (t :: TableType) k v where
     TrackingTableRO  :: PMapRO   k v -> TrackingTable TableTypeRO  k v
     TrackingTableRW  :: PTMapRW  k v -> TrackingTable TableTypeRW  k v
     TrackingTableRWU :: PTMapRWU k v -> TrackingTable TableTypeRWU k v


data TableDiff (t :: TableType) k v where
     TableDiffRO  ::                   TableDiff TableTypeRO  k v
     TableDiffRW  :: DiffMapRW  k v -> TableDiff TableTypeRW  k v
     TableDiffRWU :: DiffMapRWU k v -> TableDiff TableTypeRWU k v

mkTableReadSet :: Ord k => TableTag t v -> TableKeySet t k v -> Map.Map k v -> TableReadSet t k v
mkTableReadSet TableTagRO  (TableKeySet ks) m = TableRO  (PMapRO  (mkPMapRep m ks))
mkTableReadSet TableTagRW  (TableKeySet ks) m = TableRW  (PMapRW  (mkPMapRep m ks))
mkTableReadSet TableTagRWU (TableKeySet ks) m = TableRWU (PMapRWU (mkPMapRep m ks))

mkTrackingTable :: TableReadSet t k v -> TrackingTable t k v
mkTrackingTable (TableRO  m) = TrackingTableRO  m
mkTrackingTable (TableRW  m) = TrackingTableRW  (mkPTMapRW  m)
mkTrackingTable (TableRWU m) = TrackingTableRWU (mkPTMapRWU m)

getTableDiff :: TrackingTable t k v -> TableDiff t k v
getTableDiff (TrackingTableRO _)  = TableDiffRO
getTableDiff (TrackingTableRW  m) = TableDiffRW  (getDiffMapRW  m)
getTableDiff (TrackingTableRWU m) = TableDiffRWU (getDiffMapRWU m)



type TableKind = TableType -> * -> * -> *
type StateKind = TableKind -> *

class HasTables (state :: StateKind) where
  type StateTableKeyConstraint   state :: * -> Constraint
  type StateTableValueConstraint state :: * -> Constraint

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

  -- The common pure case, not needing Applicative
  mapTables :: (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v)
            -> state table -> state table'
  mapTables f = runIdentity . traverseTables (\t m -> pure (f t m))

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
  constTables f = runIdentity (traverse0Tables (pure . f))

  zipTables  :: (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v -> table'' t k v)
             -> state table -> state table' -> state table''
  zipTables f a b = runIdentity (traverse2Tables (\t x y -> pure (f t x y)) a b)


class (HasTables state, HasOnlyTables (Tables state), HasSeqNo state)
   => HasOnDiskTables state where

  data Tables state :: StateKind

  projectTables :: state table -> Tables state table
  injectTables  :: Tables state table -> state any -> state table

class HasSeqNo (state :: StateKind) where

  stateSeqNo :: state table -> SeqNo state

newtype SeqNo (state :: StateKind) = SeqNo { seqnoToWord :: Word64 }
  deriving (Eq, Ord, Bounded, Show)


class MonoidalTable table where
    emptyTable   :: Ord k => TableTag t v -> table t k v
    appendTables :: Ord k => TableTag t v -> table t k v -> table t k v -> table t k v

instance MonoidalTable TableKeySet where
    emptyTable   _ = TableKeySet Set.empty
    appendTables _ (TableKeySet a) (TableKeySet b) = TableKeySet (Set.union a b)

instance MonoidalTable TableDiff where
    emptyTable   TableTagRO  = TableDiffRO
    emptyTable   TableTagRW  = TableDiffRW  mempty
    emptyTable   TableTagRWU = TableDiffRWU mempty

    appendTables TableTagRO   TableDiffRO       TableDiffRO      = TableDiffRO
    appendTables TableTagRW  (TableDiffRW  m1) (TableDiffRW  m2) = TableDiffRW  (m1 <> m2)
    appendTables TableTagRWU (TableDiffRWU m1) (TableDiffRWU m2) = TableDiffRWU (m1 <> m2)

instance (HasOnlyTables (Tables state), MonoidalTable table)
      => Semigroup (Tables state table) where
   (<>) = zipTables appendTables

instance (HasOnlyTables (Tables state), MonoidalTable table)
      => Monoid (Tables state table) where
   mempty = constTables emptyTable


-- Common type aliases
----------------------

type TableKeySets   state = Tables state TableKeySet
type TableReadSets  state = Tables state TableReadSet

type AnnTableKeySets  state a = Tables state (AnnTable TableKeySet a)
type AnnTableReadSets state a = Tables state (AnnTable TableReadSet a)

type TrackingTables state = Tables state TrackingTable
type TableDiffs     state = Tables state TableDiff


-- Table snapshots
------------------

data TableSnapshots state where

     TableSnapshots :: (forall table. Tables state table
                                   -> Tables state (SnapshotOfTable table))
                    -> TableSnapshots state

data SnapshotOfTable (table :: TableType -> * -> * -> *) (t :: TableType) k v where
       KeepTable       ::                SnapshotOfTable table t           k v
       SnapshotOfTable :: table t k v -> SnapshotOfTable table TableTypeRO k v

-- | An variant of 'SnapshotOfTable' where we keep the original table too.
-- This is more expressive than 'SnapshotOfTable' because in principle it
-- would allow renaming tables of RW and RWU types.
--
data SnapshotOfTable' (table :: TableType -> * -> * -> *) (t :: TableType) k v where
       KeepTable'       :: table t k v -> SnapshotOfTable' table t         k v
       SnapshotOfTable' :: table t k v -> SnapshotOfTable' table TableTypeRO k v

instance HasOnDiskTables state => Semigroup (TableSnapshots state) where
  TableSnapshots g <> TableSnapshots f =
      TableSnapshots h
    where
      h :: forall table.
           Tables state table
        -> Tables state (SnapshotOfTable table)
      h ts = zipTables fixup_g (g ts') ts'
        where
          ts' :: Tables state (SnapshotOfTable' table)
          ts' = zipTables fixup_f (f ts) ts

      fixup_f :: TableTag t v
              -> SnapshotOfTable table t k v
              -> table t k v
              -> SnapshotOfTable' table t k v
      fixup_f _  KeepTable          t = KeepTable'       t
      fixup_f _ (SnapshotOfTable t) _ = SnapshotOfTable' t

      fixup_g :: TableTag t v
              -> SnapshotOfTable (SnapshotOfTable' table) t k v
              -> SnapshotOfTable' table t k v
              -> SnapshotOfTable  table t k v
      fixup_g _ KeepTable (KeepTable' _)       = KeepTable
      fixup_g _ KeepTable (SnapshotOfTable' t) = SnapshotOfTable t
      fixup_g _ (SnapshotOfTable (KeepTable'       t)) _ = SnapshotOfTable t
      fixup_g _ (SnapshotOfTable (SnapshotOfTable' t)) _ = SnapshotOfTable t

{-
data TableId (t :: TableType) k v where
     TableId :: TableTag t v -> Int -> TableId t k v
  deriving Show

data AnyTableId where
     AnyTableId :: TableId t k v -> AnyTableId

deriving instance Show AnyTableId


enumerateTables :: forall state. HasOnlyTables state => state TableId
enumerateTables =
    evalState enumerate 0
  where
    enumerate :: State Int (state TableId)
    enumerate =
      traverse0Tables $ \t -> do
        i <- State.get
        State.modify' (+1)
        pure $! TableId t i

listTables :: forall state. HasOnlyTables state => state TableId -> [AnyTableId]
listTables s =
    reverse (execState (collect s) [])
  where
    collect :: state TableId -> State [AnyTableId] ()
    collect =
      traverseTables_ $ \_t i -> State.modify (AnyTableId i :)

takeSnapshotTableId :: TableId t k v -> TableId TableTypeRO k v
takeSnapshotTableId (TableId _t i) = TableId TableTagRO i
-}

applyTableSnapshots
  :: forall state map.
     TableSnapshots state
  -> Tables state map
  -> Tables state (SnapshotOfTable map)
applyTableSnapshots (TableSnapshots f) s = f s


applyTableSnapshots'
  :: forall state map.
     HasOnlyTables (Tables state)
  => TableSnapshots state
  -> Tables state map
  -> Tables state (SnapshotOfTable' map)
applyTableSnapshots' (TableSnapshots f) s =
    zipTables combine (f s) s
  where
    combine :: TableTag t v
            -> SnapshotOfTable map t k v
            -> map t k v
            -> SnapshotOfTable' map t k v
    combine _  KeepTable          t = KeepTable'       t
    combine _ (SnapshotOfTable t) _ = SnapshotOfTable' t


applyTableSnapshotsInverse :: forall state.
                              HasOnlyTables (Tables state)
                           => TableSnapshots state
                           -> TableKeySets state
                           -> TableKeySets state
applyTableSnapshotsInverse (TableSnapshots f) ts =
    ST.runST $ do
      -- Initialise a tuple of STRefs with empty key sets
      refs <- newRefs

      -- Write into the swizzled STRefs, merging key sets if needed
      writeRefs (swizzleRefs refs)

      -- Read from the original STRefs
      readRefs refs
  where
    newRefs :: ST s (Tables state (STRefKeySet s))
    newRefs = traverse0Tables (\_t -> STRefKeySet <$> ST.newSTRef Set.empty)

    swizzleRefs :: Tables state (STRefKeySet s)
                -> Tables state (STRefKeySet s)
    swizzleRefs refs = zipTables selectRef (f refs) refs
      where
        selectRef :: forall s t k v.
                     TableTag t v
                  -> SnapshotOfTable (STRefKeySet s) t k v
                  -> STRefKeySet s t k v
                  -> STRefKeySet s t k v
        selectRef _t  KeepTable       ref    = ref
        selectRef _t (SnapshotOfTable ref) _ = snapshotSTRefKeySet ref

    snapshotSTRefKeySet :: STRefKeySet s t k v -> STRefKeySet s TableTypeRO k v
    snapshotSTRefKeySet (STRefKeySet r) = STRefKeySet r

    writeRefs :: Tables state (STRefKeySet s)
              -> ST s ()
    writeRefs = traverse2Tables_
                  (\_t (TableKeySet s) (STRefKeySet r) ->
                    ST.modifySTRef' r (Set.union s))
                  ts

    readRefs :: Tables state (STRefKeySet s) -> ST s (TableKeySets state)
    readRefs = traverseTables
                 (\_t (STRefKeySet r) -> TableKeySet <$> ST.readSTRef r)

-- Used only by applyTableSnapshotsInverse
newtype STRefKeySet s (t :: TableType) k v = STRefKeySet (STRef s (Set.Set k))


-- Disk state and disk operations
---------------------------------


-- The disk db has a single available value at once. To help keep track of the
-- changing value of the database it records a sequence number.
--
-- It is permitted for reads and writes to be raced against each other.
-- The read returns the SeqNo that the read was against. This can be used
-- to resolve write/read races.
--
-- Writes cannot race other writes.
--
class DiskDb dbhandle state {- | dbhandle -> state -} where
  readDb  :: dbhandle
          -> AnnTableKeySets state a
          -> IO (AnnTableReadSets state (a, SeqNo state))

  writeDb :: dbhandle
          -> [Either (TableDiffs state) (TableSnapshots state)]
          -> SeqNo state -- ^ The old sequence number, as a sanity check
          -> SeqNo state -- ^ The new sequence number, must be strictly greater
          -> IO ()

--  snapshotDB :: dbhandle
--             -> SeqNo state -- ^ The old sequence number, as a sanity check
--             -> IO ()


-- In-memory changes
--------------------

-- | A sequence of changes to a data store.
--
-- It is intended to be used to /extend in-memory/ beyond the state of an
-- on-disk data store. It allows new changes to be added in memory, and old
-- changes to be removed, typically to flush to disk. In this sense it behaves
-- like a FIFO queue with new changes added and one end and old changes removed
-- at the other.
--
-- It also allows rewinding to an earlier point in the sequence.
--
-- Most of the operations rely on the db state being in the
-- 'HasOnDiskTables' class.
--
-- It is also designed to be used with a disk DB from the 'DiskDb' class.
--

data DbChangelog (state :: (TableType -> * -> * -> *) -> *) =
       DbChangelog {
         -- | The point that the change log applies from. This must match
         -- the point at which reads are done from the disk Db.
         dbChangelogAnchorSeqNo :: SeqNo state,

-- Awkward: if the Tables are degenerate and have no tables at all then
-- we have no sequences and cannot split etc.
--         dbChangelogLastSeqNo :: Maybe (SeqNo state)

         -- | The changes to the content of the tables
         dbChangelogContent :: !(Tables state (SeqTableDiff state)),

         -- The changes involving table snapshots
         -- this is sparse, often empty, in practice never more than one element
         dbChangelogSnapshots :: !(SSeq state (TableSnapshots state)),

         -- The derived changes to apply to reads for snapshotted tables.
         dbChangelogForward :: !(Tables state (SeqSnapshotDiff state))
       }


emptyDbChangelog :: HasOnDiskTables state => SeqNo state -> DbChangelog state
emptyDbChangelog anchor =
    DbChangelog anchor mempty mempty mempty

{-
instance HasOnDiskTables state => Semigroup (DbChangelog state) where
    DbChangelog a1 b1 c1 <> DbChangelog a2 b2 c2 =
      DbChangelog (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance HasOnDiskTables state => Monoid (DbChangelog state) where
    mempty = emptyDbChangelog
-}

-- | The interpretation here is the table content changes followed by the
-- optional table snapshots.
data DbChange state = DbChange
                        !(SeqNo state)
                        !(Tables state TableDiff)
                        !(Maybe (TableSnapshots state))


-- |
--
extendDbChangelog :: forall state.
                     HasOnDiskTables state
                  => DbChange state -> DbChangelog state -> DbChangelog state
extendDbChangelog (DbChange seqno diffs msnapshots)
                  dbchangelog@DbChangelog{..} =
    let dbChangelogContent' = zipTables snoc dbChangelogContent diffs
     in case msnapshots of
          Nothing -> 
            dbchangelog {
              dbChangelogContent = dbChangelogContent'
            }

          Just snapshots ->
            dbchangelog {
              dbChangelogContent = dbChangelogContent',

              dbChangelogSnapshots =
                dbChangelogSnapshots FT.|> SSeqElem seqno snapshots,

              dbChangelogForward =
                extendSnapshotDiffs
                  snapshots
                  dbChangelogContent'
                  dbChangelogForward
            }
  where
    snoc :: Ord k
         => TableTag           t   v
         -> SeqTableDiff state t k v
         -> TableDiff          t k v
         -> SeqTableDiff state t k v
    snoc _ ds d = snocSeqTableDiff ds d seqno

extendSnapshotDiffs :: HasOnlyTables (Tables state)
                    => TableSnapshots state
                    -> Tables state (SeqTableDiff    state)
                    -> Tables state (SeqSnapshotDiff state)
                    -> Tables state (SeqSnapshotDiff state)
extendSnapshotDiffs snapshots tdiffs sdiffs =
    zipTables combine (applyTableSnapshots  snapshots tdiffs)
                      (applyTableSnapshots' snapshots sdiffs)
  where
    combine :: Ord k
            => TableTag t v
            -> SnapshotOfTable  (SeqTableDiff    state) t k v
            -> SnapshotOfTable' (SeqSnapshotDiff state) t k v
            -> SeqSnapshotDiff state t k v

    -- The snapshot diffs are only for the RO tables. It is always a
    -- nullary representation for the RW and RWU tables.
    combine TableTagRW  _ _ = emptyTable TableTagRW
    combine TableTagRWU _ _ = emptyTable TableTagRWU

    -- This case is where we keep existing RO snapshots.
    combine TableTagRO KeepTable (KeepTable' t) = t

    -- This case is where we re-arrange existing RO snapshots.
    combine TableTagRO (SnapshotOfTable SeqTableDiffRO)
                       (SnapshotOfTable' t) =
      -- Yes, this is a fancy identify function
      case t of
        SeqSnapshotDiffEmpty   -> SeqSnapshotDiffEmpty
        SeqSnapshotOfDiffRW{}  -> t
        SeqSnapshotOfDiffRWU{} -> t

    -- These two cases are where we _actually_ take a snapshot of a RW or RWU
    -- table, not just re-arranging existing RO snapshots. What we do in these
    -- cases is just grab the whole current sequence of diffs for that table.
    -- These _snapshots of the current diffs_ will be the diffs we apply to
    -- fast-forward reads to this snapshot point.
    combine TableTagRO (SnapshotOfTable (SeqTableDiffRW ds))
                       (SnapshotOfTable' SeqSnapshotDiffEmpty) =
            SeqSnapshotOfDiffRW ds

    combine TableTagRO (SnapshotOfTable (SeqTableDiffRWU ds))
                       (SnapshotOfTable' SeqSnapshotDiffEmpty) =
            SeqSnapshotOfDiffRWU ds

    -- These cases are impossible because both arguments are the result of
    -- applying the same snapshotting rearrangement function.
    combine TableTagRO (SnapshotOfTable SeqTableDiffRW{})
                        SnapshotOfTable'{}            = impossible
    combine TableTagRO (SnapshotOfTable SeqTableDiffRWU{})
                        SnapshotOfTable'{}            = impossible
    combine TableTagRO KeepTable SnapshotOfTable'{}   = impossible
    combine TableTagRO SnapshotOfTable{} KeepTable'{} = impossible

    impossible = error "extendSnapshotDiffs: impossible"


-- | Get the first and last 'SeqNo' in the sequence of changes.
-- TODO: or maybe we only need the anchor?
--
boundsDbChangelog :: DbChangelog state -> (SeqNo state, Maybe (SeqNo state))
boundsDbChangelog = undefined
--boundsDbChangelog DbChangelog {dbChangelogAnchorSeqNo, dbChangelogContent} =
--    (dbChangelogAnchorSeqNo, ...)

rewindDbChangelog :: SeqNo state -> DbChangelog state -> Maybe (DbChangelog state)
rewindDbChangelog = undefined
{-
cacheSnapshotDiffs :: Tables state (SSeq state TableDiff)
                   -> SSeq state (TableSnapshots state)
                   -> Tables state (SSeq state SnapshotDiff)
cacheSnapshotDiffs = undefined
-}

-- TODO: document that we must pass the last block for the LHS
splitDbChangelog :: SeqNo state -> DbChangelog state -> Maybe (DbChangelog state, DbChangelog state)
splitDbChangelog = undefined

summariseDbChangelog :: DbChangelog state
                     -> [Either (TableDiffs state) (TableSnapshots state)]
summariseDbChangelog = undefined


-- | For sanity checking we want to track when each table was last snapshotted,
-- which may be never. The reason is that we cannot read from snapshots in a
-- pipelined way.
--
type KeySetSanityInfo state = Maybe (SeqNo state)

-- | For sanity checking we will use the 'KeySetSanityInfo' that we
-- captured with the keys to read, and we will also use the seqno of the
-- DB we read to obtain the read set. We will check this matches the
-- DbChangelog when we forward the read sets.
--
type ReadSetSanityInfo state = (KeySetSanityInfo state, SeqNo state)

rewindTableKeySets :: HasOnDiskTables state
                   => DbChangelog state
                   -> TableKeySets state
                   -> AnnTableKeySets state (KeySetSanityInfo state)
rewindTableKeySets (DbChangelog _ _ snapshots _) tks =
    case FT.measure snapshots of
      SSeqSummaryEmpty ->
        mapTables (\_ x -> AnnTable x Nothing) tks

      SSeqSummary _ maxseqno sn ->
        mapTables (\_ x -> AnnTable x (Just maxseqno))
                  (applyTableSnapshotsInverse sn tks)


forwardTableReadSets :: DbChangelog state
                     -> AnnTableReadSets state (ReadSetSanityInfo state)
                     -> Maybe (TableReadSets state)
forwardTableReadSets = undefined


-- | A 'SeqTableDiff' is a sequence of 'TableDiff's.
--
-- It is represented slightly differently than literally a sequence of
-- 'TableDiff'. We take advantage of the fact that within each sequence we know
-- the whole lot are RO, RW or RWU. In particular the RO case is always empty
-- so we don't need to represent it as a sequence of empties.
--
data SeqTableDiff state t k v where
       SeqTableDiffRO  :: SeqTableDiff state TableTypeRO k v

       SeqTableDiffRW  :: SSeq state (DiffMapRW k v)
                       -> SeqTableDiff state TableTypeRW k v

       SeqTableDiffRWU :: Semigroup v
                       => SSeq state (DiffMapRWU k v)
                       -> SeqTableDiff state TableTypeRWU k v

instance MonoidalTable (SeqTableDiff state) where
    emptyTable TableTagRO  = SeqTableDiffRO
    emptyTable TableTagRW  = SeqTableDiffRW  FT.empty
    emptyTable TableTagRWU = SeqTableDiffRWU FT.empty

    appendTables TableTagRO  _ _ = SeqTableDiffRO
    appendTables TableTagRW  (SeqTableDiffRW a) (SeqTableDiffRW b) =
      SeqTableDiffRW (a <> b)

    appendTables TableTagRWU (SeqTableDiffRWU a) (SeqTableDiffRWU b) =
      SeqTableDiffRWU (a <> b)

snocSeqTableDiff :: Ord k
                 => SeqTableDiff state t k v
                 -> TableDiff t k v
                 -> SeqNo state
                 -> SeqTableDiff state t k v
snocSeqTableDiff  SeqTableDiffRO      TableDiffRO    _     = SeqTableDiffRO
snocSeqTableDiff (SeqTableDiffRW ds) (TableDiffRW d) seqno =
    SeqTableDiffRW (ds FT.|> SSeqElem seqno d)
snocSeqTableDiff (SeqTableDiffRWU ds) (TableDiffRWU d) seqno =
    SeqTableDiffRWU (ds FT.|> SSeqElem seqno d)


-- | The 'SeqSnapshotDiff' is used to forward reads through snapshots.
--
-- This is sort-of the dual of 'SeqTableDiff'. The 'SeqTableDiff' keeps
-- differences for the updatable R\/W and R\/W\/U tables, and nothing for
-- read-only tables.
--
-- In the presence of snapshots we have to do reads against updatable tables
-- and then forward those reads through a prefix of the 'SeqTableDiff' up to
-- the point at which the snapshot was taken.
--
-- So therefore the 'SeqSnapshotDiff' only keeps the copies of the prefixes of
-- diffs that were originally from updatable tables and are now used for
-- logical reads on read-only snapshots.
--
data SeqSnapshotDiff state t k v where
       SeqSnapshotDiffEmpty  :: SeqSnapshotDiff state t k v

       SeqSnapshotOfDiffRW   :: SSeq state (DiffMapRW k v)
                             -> SeqSnapshotDiff state TableTypeRO k v

       SeqSnapshotOfDiffRWU  :: Semigroup v
                             => SSeq state (DiffMapRWU k v)
                             -> SeqSnapshotDiff state TableTypeRO k v

instance MonoidalTable (SeqSnapshotDiff state) where
    emptyTable _ = SeqSnapshotDiffEmpty

    appendTables TableTagRW  _ _ = SeqSnapshotDiffEmpty
    appendTables TableTagRWU _ _ = SeqSnapshotDiffEmpty

    appendTables TableTagRO  SeqSnapshotDiffEmpty m = m
    appendTables TableTagRO  m SeqSnapshotDiffEmpty = m

    appendTables TableTagRO  (SeqSnapshotOfDiffRW m1) (SeqSnapshotOfDiffRW m2) =
                              SeqSnapshotOfDiffRW (m1 <> m2)

    appendTables TableTagRO  (SeqSnapshotOfDiffRWU m1) (SeqSnapshotOfDiffRWU m2) =
                              SeqSnapshotOfDiffRWU (m1 <> m2)

    -- These two cases would be expensive to implement properly (by promoting
    -- the RW to RWU) but in practice they should never happen. We don't
    -- conditionally take snapshots of RW and RWU tables into the same RO slot.
    --
    appendTables TableTagRO  (SeqSnapshotOfDiffRW _) (SeqSnapshotOfDiffRWU _) =
      error "SnapshotDiff.appendTables: you probably did not want to do this"

    appendTables TableTagRO  (SeqSnapshotOfDiffRWU _) (SeqSnapshotOfDiffRW _) =
      error "SnapshotDiff.appendTables: you probably did not want to do this"


-- | A summarisable sequence. That is a sequence of elements (with sequence
-- numbers) that can be summarised in a monoidal way.
--
type SSeq state a =
       FT.FingerTree (SSeqSummary state a)
                     (SSeqElem    state a)

data SSeqElem state a =
       SSeqElem
         !(SeqNo state) -- seq no
         !a

data SSeqSummary state a =
       SSeqSummaryEmpty
     | SSeqSummary
         !(SeqNo state)  -- min seq no
         !(SeqNo state)  -- max seq no
         !a

instance Semigroup a => Semigroup (SSeqSummary state a) where
  SSeqSummaryEmpty <> b = b
  a <> SSeqSummaryEmpty = a
  SSeqSummary smina smaxa a <> SSeqSummary sminb smaxb b =
    SSeqSummary
      (min smina sminb)
      (max smaxa smaxb)
      (a <> b)

instance Semigroup a => Monoid (SSeqSummary state a) where
  mempty = SSeqSummaryEmpty

instance Semigroup a
      => FT.Measured (SSeqSummary state a) (SSeqElem state a) where
  measure (SSeqElem seqno snapshot) = SSeqSummary seqno seqno snapshot


-- In-mem mock of disk operations
---------------------------------
{-
data TVarDb state = TVarDb !(Tables state TVarDbTable)
                           !(TVar (SeqNo state))

newtype TVarDbTable (t :: TableType) k v = TVarDbTable (TVar (Map.Map k v))

instance HasOnDiskTables state => DiskDb (TVarDb state) state where
  readDb (TVarDb tables seqnovar) keysets =
      STM.atomically $
        (,) <$> traverse2Tables readTVarDbTable tables keysets
            <*> STM.readTVar seqnovar
    where
      readTVarDbTable :: Ord k
                      => TableTag t v
                      -> TVarDbTable t k v
                      -> TableKeySet t k v
                      -> STM (TableReadSet t k v)
      readTVarDbTable t (TVarDbTable tv) ks = do
          tbl <- STM.readTVar tv
          let !pmap = mkTableReadSet t ks tbl
          return pmap

  writeDb (TVarDb tables seqnovar) diffs oldseqno newseqno =
      STM.atomically $ do
        traverse2Tables_ writeTVarDbTable tables diffs
        oldseqno' <- STM.readTVar seqnovar
        unless (oldseqno' == oldseqno) $
          STM.throwSTM (DbSeqNoIncorrect (fromEnum oldseqno') (fromEnum oldseqno))
        unless (newseqno > oldseqno) $
          STM.throwSTM (DbSeqNoNotMonotonic (fromEnum oldseqno) (fromEnum newseqno))
        STM.writeTVar seqnovar newseqno
    where
      writeTVarDbTable :: Ord k
                       => TableTag t v
                       -> TVarDbTable t k v
                       -> TableDiff   t k v
                       -> STM ()
      writeTVarDbTable TableTagRO (TVarDbTable _tv) TableDiffRO =
        return ()

      writeTVarDbTable TableTagRW (TVarDbTable tv) (TableDiffRW (DiffMapRW d)) = do
        m <- STM.readTVar tv
        --TODO: we should use a Map not PMap here
        let (MapRW m') = applyDiff (MapRW m) d
        STM.writeTVar tv m'

      writeTVarDbTable TableTagRWU (TVarDbTable tv) (TableDiffRWU (DiffMapRWU d)) = do
        m <- STM.readTVar tv
        let (MapRWU m') = applyDiff (MapRWU m) d
        STM.writeTVar tv m'
-}
data DbSeqNoException =
       DbSeqNoIncorrect    Int Int -- ^ Expected and given sequence number
     | DbSeqNoNotMonotonic Int Int -- ^ Current and given sequence number
  deriving Show

instance Exception DbSeqNoException





-- Example
------------

-- | An example of a top level state consisting of a few parts. This
-- demonstrates a top level state type with multiple tables, and other
-- in-memory state that are not tables, and is not kept on disk.
--
data LedgerState map =
     LedgerState {

       -- | An example of some nested state
       utxos   :: UTxOState map,

       -- | More nested state for snapshots
       snapshots :: UTxOSnapshots map,

       -- | Something standing in for other state, like protocol parameters.
       pparams :: PParams,

       -- | The current slot number, used to identify states within evolving
       -- sequences of states.
       --
       curslot :: SlotNo
     }

-- | The content of this doesn't actually matter. It's just a place-holder.
--
data PParams = PParams
  deriving Show

-- | An example sub-part of the state, to demonstrate \"vertical\" composition:
-- that the state and operations can work over components of the overall state.
--
-- In particular this one demonstrates two tables. The second of the two
-- tables can benefit from monoidal updates, since it incrementally maintains
-- an aggregation of the first table.
--
data UTxOState map =
     UTxOState {
       -- | A simple UTxO structure.
       utxo    :: map TableTypeRW TxIn TxOut,

       -- | An aggregation of the UTxO's coins by address.
       utxoagg :: map TableTypeRWU Addr Coin
     }


-- | A demonstration of snapshoting functionality: we keep three recent copies
-- of the UTxO aggregation. These are 
--
data UTxOSnapshots map =
     UTxOSnapshots {
       utxoagg1 :: map TableTypeRO Addr Coin,
       utxoagg2 :: map TableTypeRO Addr Coin,
       utxoagg3 :: map TableTypeRO Addr Coin
     }

data    Tx     = Tx [TxIn] [TxOut]  deriving (Eq, Show)
data    TxIn   = TxIn !TxId !TxIx   deriving (Eq, Ord, Show)
newtype TxId   = TxId Int           deriving (Eq, Ord, Show)
newtype TxIx   = TxIx Int           deriving (Eq, Ord, Show)
data    TxOut  = TxOut !Addr !Coin  deriving (Eq, Show)
newtype Addr   = Addr Int           deriving (Eq, Ord, Show)
newtype Coin   = Coin Int           deriving (Eq, Num, Show)

instance Semigroup Coin  where a <> b = a + b

data    Block  = Block !SlotNo [Tx] deriving (Eq, Show)
newtype SlotNo = SlotNo Int         deriving (Eq, Ord, Bounded, Enum, Show)


-- | This is a place-holder for serialisation constraints
class Example a
instance Example TxIn
instance Example TxOut
instance Example Addr
instance Example Coin


instance HasTables LedgerState where
  type StateTableKeyConstraint   LedgerState = Example
  type StateTableValueConstraint LedgerState = Example

  traverseTables f LedgerState { utxos, snapshots, pparams, curslot } =
    LedgerState <$> traverseTables f utxos
                <*> traverseTables f snapshots
                <*> pure pparams
                <*> pure curslot

  traverseTables_ f LedgerState { utxos, snapshots } =
    () <$ traverseTables_ f utxos
       <* traverseTables_ f snapshots

instance HasTables UTxOState where
  type StateTableKeyConstraint   UTxOState = Example
  type StateTableValueConstraint UTxOState = Example

  traverseTables f UTxOState { utxo, utxoagg } =
    UTxOState <$> f TableTagRW  utxo
              <*> f TableTagRWU utxoagg

  traverseTables_ f UTxOState { utxo, utxoagg } =
    () <$ f TableTagRW  utxo
       <* f TableTagRWU utxoagg

instance HasTables UTxOSnapshots where
  type StateTableKeyConstraint   UTxOSnapshots = Example
  type StateTableValueConstraint UTxOSnapshots = Example

  traverseTables f UTxOSnapshots { utxoagg1, utxoagg2, utxoagg3 } =
    UTxOSnapshots <$> f TableTagRO utxoagg1
                  <*> f TableTagRO utxoagg2
                  <*> f TableTagRO utxoagg3

  traverseTables_ f UTxOSnapshots { utxoagg1, utxoagg2, utxoagg3 } =
    () <$ f TableTagRO utxoagg1
       <* f TableTagRO utxoagg2
       <* f TableTagRO utxoagg3


instance HasTables (Tables LedgerState) where
  type StateTableKeyConstraint   (Tables LedgerState) = Example
  type StateTableValueConstraint (Tables LedgerState) = Example

  traverseTables f LedgerStateTables {
                       utxoTable,
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table,
                       utxoagg3Table
                     } =
    LedgerStateTables <$> f TableTagRW  utxoTable
                        <*> f TableTagRWU utxoaggTable
                        <*> f TableTagRO  utxoagg1Table
                        <*> f TableTagRO  utxoagg2Table
                        <*> f TableTagRO  utxoagg3Table

  traverseTables_ f LedgerStateTables {
                       utxoTable,
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table,
                       utxoagg3Table
                     } =
    () <$ f TableTagRW  utxoTable
       <* f TableTagRWU utxoaggTable
       <* f TableTagRO  utxoagg1Table
       <* f TableTagRO  utxoagg2Table
       <* f TableTagRO  utxoagg3Table


instance HasOnlyTables (Tables LedgerState) where
  traverse0Tables f =
    LedgerStateTables <$> f TableTagRW
                        <*> f TableTagRWU
                        <*> f TableTagRO
                        <*> f TableTagRO
                        <*> f TableTagRO

  traverse2Tables f st1 st2 =
    LedgerStateTables
      <$> f TableTagRW  (utxoTable st1) (utxoTable st2)
      <*> f TableTagRWU (utxoaggTable st1) (utxoaggTable st2)
      <*> f TableTagRO  (utxoagg1Table st1) (utxoagg1Table st2)
      <*> f TableTagRO  (utxoagg2Table st1) (utxoagg2Table st2)
      <*> f TableTagRO  (utxoagg3Table st1) (utxoagg3Table st2)

  traverse2Tables_ f st1 st2 =
     () <$ f TableTagRW  (utxoTable    st1) (utxoTable    st2)
        <* f TableTagRWU (utxoaggTable st1) (utxoaggTable st2)
        <* f TableTagRO  (utxoagg1Table st1) (utxoagg1Table st2)
        <* f TableTagRO  (utxoagg2Table st1) (utxoagg2Table st2)
        <* f TableTagRO  (utxoagg3Table st1) (utxoagg3Table st2)


instance HasOnDiskTables LedgerState where

  data Tables LedgerState table =
         LedgerStateTables {
           utxoTable     :: table TableTypeRW  TxIn TxOut,
           utxoaggTable  :: table TableTypeRWU Addr Coin,
           utxoagg1Table :: table TableTypeRO  Addr Coin,
           utxoagg2Table :: table TableTypeRO  Addr Coin,
           utxoagg3Table :: table TableTypeRO  Addr Coin
         }

  projectTables LedgerState {
                  utxos     = UTxOState {
                                utxo,
                                utxoagg
                              },
                  snapshots = UTxOSnapshots {
                                utxoagg1,
                                utxoagg2,
                                utxoagg3
                              }
                } =
    LedgerStateTables {
      utxoTable     = utxo,
      utxoaggTable  = utxoagg,
      utxoagg1Table = utxoagg1,
      utxoagg2Table = utxoagg2,
      utxoagg3Table = utxoagg3
    }

  injectTables
    LedgerStateTables {
      utxoTable,
      utxoaggTable,
      utxoagg1Table,
      utxoagg2Table,
      utxoagg3Table
    }
    LedgerState { pparams, curslot } =

    LedgerState {
      utxos     = UTxOState {
                    utxo    = utxoTable,
                    utxoagg = utxoaggTable
                  },
      snapshots = UTxOSnapshots {
                    utxoagg1 = utxoagg1Table,
                    utxoagg2 = utxoagg2Table,
                    utxoagg3 = utxoagg3Table
                  },
      pparams,
      curslot
    }

instance HasSeqNo LedgerState where
  stateSeqNo LedgerState {curslot = SlotNo s} = SeqNo (fromIntegral s)

class ShowTable (table :: TableType -> * -> * -> *) where
    showsTable :: (Show k, Show v) => table t k v -> ShowS

instance ShowTable EmptyTable where
    showsTable = shows

instance ShowTable TableKeySet where
    showsTable = shows

--instance ShowTable TableId where
--    showsTable = shows

instance ShowTable table => Show (Tables LedgerState table) where
  showsPrec _ LedgerStateTables{..} =
      showString "LedgerStateTables {\n"
    . showString "  utxoTable     = " . showsTable utxoTable     . showString ",\n"
    . showString "  utxoaggTable  = " . showsTable utxoaggTable  . showString ",\n"
    . showString "  utxoagg1Table = " . showsTable utxoagg1Table . showString ",\n"
    . showString "  utxoagg2Table = " . showsTable utxoagg2Table . showString ",\n"
    . showString "  utxoagg3Table = " . showsTable utxoagg3Table . showString ",\n"
    . showString "}"

-- | Simulate taking snapshots at epoch boundaries
--
epochSnapshotSwizzle :: Tables LedgerState table
                     -> Tables LedgerState (SnapshotOfTable table)
epochSnapshotSwizzle LedgerStateTables {
                       utxoaggTable,
                       utxoagg1Table,
                       utxoagg2Table
                     } =
    -- Take a snapshot of the current utxo aggregate.
    -- Shuffle all the older snapshots down by one.
    LedgerStateTables {
      utxoTable     = KeepTable,
      utxoaggTable  = KeepTable,
      utxoagg1Table = SnapshotOfTable utxoaggTable,
      utxoagg2Table = SnapshotOfTable utxoagg1Table,
      utxoagg3Table = SnapshotOfTable utxoagg2Table
    }

exampleKeySets :: TableKeySets LedgerState
exampleKeySets =
    LedgerStateTables {
      utxoTable     = TableKeySet (Set.singleton (TxIn (TxId 0) (TxIx 0))),
      utxoaggTable  = TableKeySet (Set.singleton (Addr 1)),
      utxoagg1Table = TableKeySet (Set.singleton (Addr 2)),
      utxoagg2Table = TableKeySet (Set.singleton (Addr 3)),
      utxoagg3Table = TableKeySet (Set.singleton (Addr 4))
    }

