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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Snapshots where

import           Prelude hiding (lookup)

import           Data.Kind
import           Data.Functor.Identity
import           Data.Foldable

--import qualified Data.Hashable as H
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.FingerTree as FT
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Word (Word64)

import           Control.Monad
import qualified Control.Monad.ST.Strict as ST
import           Control.Monad.ST.Strict (ST)
import           Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM
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

  -- TODO check whether this should insert the element. Tim Sheard argues not.
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

data RangeQuery k = RangeQuery
  { rqStartKey :: !(Maybe k)
  , rqOffset :: !Int
  , rqLimit :: !Int
  } deriving (Eq, Show)

normaliseRangeQueries :: (Foldable f, Ord k) => f (RangeQuery k) -> [RangeQuery k]
normaliseRangeQueries rqs =
  [ RangeQuery{rqStartKey, rqOffset, rqLimit}
  | (rqStartKey, offset_limit_map) <- Map.toList $ m
  , (rqOffset, rqLimit) <- Map.toList offset_limit_map
  ] where
  m = foldl' do_one Map.empty rqs where
    do_one !acc RangeQuery{rqStartKey, rqOffset, rqLimit} =
      Map.insertWith (Map.unionWith max) rqStartKey (Map.singleton rqOffset rqLimit) acc

data TableKeySet (t :: TableType) k v where
       TableKeySet :: Set.Set k -> TableKeySet t k v
       TableQuery ::
         { tqKeySet :: !(Set.Set k)
         , tqCountRows :: !Bool
         , tqRangeQueries :: [RangeQuery k]
         } -> TableKeySet 'TableTypeRO k v

deriving stock instance Eq k => Eq (TableKeySet t k v)
deriving stock instance Show k => Show (TableKeySet t k v)

-- | A table with an annotation: some extra data carried with it.
--
data AnnTable table a (t :: TableType) k v =
       AnnTable !(table t k v) !a


data TableReadSet (t :: TableType) k v where
     TableRO  ::
       { troPTMap :: PMapRO k v
       , troRowCount ::  Maybe Int
       , troRangeQueryResults :: Map.Map (Maybe k) (Map.Map Int [(k, v)])
       } -> TableReadSet TableTypeRO  k v
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

-- TODO integrate range queries here
mkTableReadSet :: Ord k => TableTag t v -> TableKeySet t k v -> Map.Map k v -> TableReadSet t k v
mkTableReadSet TableTagRO  (TableKeySet ks) m = TableRO
  { troPTMap = PMapRO  (mkPMapRep m ks), troRowCount = Nothing, troRangeQueryResults = Map.empty
  }
mkTableReadSet TableTagRW  (TableKeySet ks) m = TableRW  (PMapRW  (mkPMapRep m ks))
mkTableReadSet TableTagRWU (TableKeySet ks) m = TableRWU (PMapRWU (mkPMapRep m ks))
mkTableReadSet TableTagRO  (TableQuery {tqKeySet, tqCountRows, tqRangeQueries}) m = TableRO
  { troPTMap = PMapRO  (mkPMapRep m tqKeySet)
  , troRowCount = if tqCountRows then Just (length m) else Nothing
  , troRangeQueryResults = range_query_results
  } where
  range_query_results = fmap (fmap Map.toAscList) . foldl' one_query Map.empty $ tqRangeQueries where
    one_query !acc RangeQuery{rqStartKey, rqOffset, rqLimit} =
      Map.insertWith combine rqStartKey (Map.singleton rqOffset (Map.take rqLimit . Map.drop rqOffset $ m' )) acc where
        m' = case rqStartKey of
                Nothing -> m
                Just k -> let (_, r) = Map.split k m in r
        combine = Map.unionWith $ \x y -> if length x > length y then x else y

-- TODO integrate range queries here
mkTrackingTable :: TableReadSet t k v -> TrackingTable t k v
mkTrackingTable (TableRO  {troPTMap}) = TrackingTableRO troPTMap
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
  deriving (Eq, Ord, Enum, Bounded, Show)


class MonoidalTable table where
    emptyTable   :: Ord k => TableTag t v -> table t k v
    appendTables :: Ord k => TableTag t v -> table t k v -> table t k v -> table t k v

instance MonoidalTable TableKeySet where
    emptyTable   _ = TableKeySet Set.empty
    appendTables _ (TableKeySet a) (TableKeySet b) = TableKeySet (Set.union a b)
    appendTables _ (TableKeySet a) (tq@TableQuery{tqKeySet} ) = tq { tqKeySet = Set.union a tqKeySet }
    appendTables _ (tq@TableQuery{tqKeySet} ) (TableKeySet b) = tq { tqKeySet = Set.union tqKeySet b}
    appendTables _ (a@TableQuery{} ) (b@TableQuery{}) = TableQuery
      { tqKeySet = Set.union (tqKeySet a) (tqKeySet b)
      , tqCountRows = tqCountRows a || tqCountRows b
      , tqRangeQueries = normaliseRangeQueries $ tqRangeQueries a ++ tqRangeQueries b
      }

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


mkTableSnapshots :: (forall table1 table2. Tables state table1 -> Tables state table2) -> TableSnapshots state
mkTableSnapshots f = TableSnapshots f

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
      fixup_f _  KeepTable          t = KeepTable'     t
      fixup_f _ (SnapshotOfTable t) _ = SnapshotOfTable' t

      fixup_g :: TableTag t v
              -> SnapshotOfTable (SnapshotOfTable' table) t k v
              -> SnapshotOfTable' table t k v
              -> SnapshotOfTable  table t k v
      fixup_g _ KeepTable (KeepTable' _)       = KeepTable
      fixup_g _ KeepTable (SnapshotOfTable' t) = SnapshotOfTable t
      fixup_g _ (SnapshotOfTable (KeepTable'       t)) _ = SnapshotOfTable t
      fixup_g _ (SnapshotOfTable (SnapshotOfTable' t)) _ = SnapshotOfTable t


interpretTableSnapshots
  :: forall state map1 map2 m.
     (HasOnlyTables (Tables state), Applicative m)
  => (forall t k v. map1 t k v -> m (map2 TableTypeRO k v))
  -> (forall t k v. m (map2 t k v))
  -> TableSnapshots state
  -> Tables state map1
  -> m (Tables state map2)
interpretTableSnapshots snapshot keep (TableSnapshots f) s =
    traverseTables combine (f s)
  where
    combine :: TableTag t v -> SnapshotOfTable map1 t k v -> m (map2 t k v)
    combine _  KeepTable          = keep
    combine _ (SnapshotOfTable m) = snapshot m


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
         -- The changes to the content of the tables
         dbChangelogContent :: !(Tables state (SeqTable TableDiff)),

         -- The changes involving table snapshots
         -- this is sparse, often empty, in practice never more than one element
         dbChangelogSnapshots :: !(SeqTableSnapshots state),

         -- The derived changes to apply to reads
         dbChangelogForward :: () -- !(Tables state (SumSeqTable TableDiff))
       }


emptyDbChangelog :: HasOnDiskTables state => DbChangelog state
emptyDbChangelog =
    DbChangelog mempty mempty mempty

instance HasOnDiskTables state => Semigroup (DbChangelog state) where
    DbChangelog a1 b1 c1 <> DbChangelog a2 b2 c2 =
      DbChangelog (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance HasOnDiskTables state => Monoid (DbChangelog state) where
    mempty = emptyDbChangelog

-- |
--
extendDbChangelog :: HasOnDiskTables state
                  => {- DbChange state -> -} DbChangelog state -> DbChangelog state
extendDbChangelog = undefined

-- | Get the first and last 'SeqNo' in the sequence of changes.
--
boundsDbChangelog :: DbChangelog state -> Maybe (SeqNo state, SeqNo state)
boundsDbChangelog = undefined

rewindDbChangelog :: SeqNo state -> DbChangelog state -> Maybe (DbChangelog state)
rewindDbChangelog = undefined

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
rewindTableKeySets (DbChangelog _ snapshots _) tks = undefined
    -- case FT.measure snapshots of
    --   SumSeqMeasure _ _ _     Nothing   ->
    --     mapTables (\_ x -> AnnTable x Nothing) tks

    --   SumSeqMeasure _ _ seqno (Just sn) ->
    --     mapTables (\_ x -> AnnTable x (Just (toEnum seqno)))
    --               (applyTableSnapshotsInverse sn tks)


forwardTableReadSets :: DbChangelog state
                     -> AnnTableReadSets state (ReadSetSanityInfo state)
                     -> TableReadSets state
forwardTableReadSets = undefined



data SeqTable table (t :: TableType) k v =
       SeqTable
         !(FT.FingerTree (SeqTableMeasure table t k v)
                         (SeqTableElem    table t k v))

data SeqTableElem table (t :: TableType) k v =
       SeqTableElem
         !Int -- seq no
         !(table t k v)

data SeqTableMeasure table (t :: TableType) k v =
       SeqTableMeasure
         !Int  -- min seq no
         !Int  -- max seq no
         !(table t k v)

instance (MonoidalTable table, HasTableTag t v, Ord k)
      => Semigroup (SeqTableMeasure table t k v) where
  SeqTableMeasure smin1 smax1 a1 <> SeqTableMeasure smin2 smax2 a2 =
    SeqTableMeasure
      (min smin1 smin2)
      (max smax1 smax2)
      (appendTables tableTag a1 a2)

instance (MonoidalTable table, HasTableTag t v, Ord k)
      => Monoid (SeqTableMeasure table t k v) where
  mempty = SeqTableMeasure maxBound minBound (emptyTable tableTag)

instance (MonoidalTable table, HasTableTag t v, Ord k)
      => FT.Measured (SeqTableMeasure table t k v)
                     (SeqTableElem    table t k v) where
  measure (SeqTableElem seqno a) = SeqTableMeasure seqno seqno a

instance MonoidalTable table => MonoidalTable (SeqTable table) where
  emptyTable   t = withTableTag t (SeqTable FT.empty)
  appendTables t (SeqTable a) (SeqTable b) =
    withTableTag t (SeqTable (a <> b))


type SeqTableSnapshots state =
       FT.FingerTree (SeqTableSnapshotsMeasure state)
                     (SeqTableSnapshotsElem state)

data SeqTableSnapshotsElem state =
       SeqTableSnapshotsElem
         !(SeqNo state) -- seq no
         !(TableSnapshots state)

data SeqTableSnapshotsMeasure state =
       SeqTableSnapshotsMeasure
         !(SeqNo state)  -- min seq no
         !(SeqNo state)  -- max seq no
         !(Maybe (TableSnapshots state))

instance HasOnDiskTables state => Semigroup (SeqTableSnapshotsMeasure state) where
  SeqTableSnapshotsMeasure smin1 smax1 a1
   <> SeqTableSnapshotsMeasure smin2 smax2 a2 =

    SeqTableSnapshotsMeasure (min smin1 smin2)
                             (max smax1 smax2)
                             (a1 <> a2)

instance HasOnDiskTables state => Monoid (SeqTableSnapshotsMeasure state) where
  mempty = SeqTableSnapshotsMeasure maxBound minBound Nothing

instance HasOnDiskTables state
     => FT.Measured (SeqTableSnapshotsMeasure state)
                    (SeqTableSnapshotsElem    state) where
  measure (SeqTableSnapshotsElem seqno snapshot) =
    SeqTableSnapshotsMeasure seqno seqno (Just snapshot)


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


-- In-mem mock of disk operations
---------------------------------
data TVarDb state = TVarDb !(Tables state TVarDbTable)
                           !(TVar (SeqNo state))

newtype TVarDbTable (t :: TableType) k v = TVarDbTable (TVar (Map.Map k v))

-- data ProductTable table1 table2 (t :: TableType) k v = ProductTable (table1 t k v) (table2 t k v)

newtype ComposeTable f table (t :: TableType) k v = ComposeTable { runComposeTable :: f (table t k v) }

newtype WrappedMap (t :: TableType) k v = WrappedMap (Map.Map k v)

instance (HasOnDiskTables state, HasSeqNo state) => DiskDb (TVarDb state) state where
  readDb (TVarDb tables seqnovar) keysets = STM.atomically $ do
    seqno <- STM.readTVar seqnovar
    traverse2Tables (readTVarDbTable seqno) tables keysets
    where
      readTVarDbTable :: Ord k
                      => SeqNo state
                      -> TableTag t v
                      -> TVarDbTable t k v
                      -> AnnTable TableKeySet a t k v
                      -> STM (AnnTable TableReadSet (a, SeqNo state) t k v)
      readTVarDbTable seqno t (TVarDbTable tv) (AnnTable ks a) = do
          tbl <- STM.readTVar tv
          let !pmap = mkTableReadSet t ks tbl
          return $ AnnTable pmap (a, seqno)

  writeDb (TVarDb tables seqnovar) diffs_and_snapshots oldseqno newseqno =
      STM.atomically $ do
        oldseqno' <- STM.readTVar seqnovar
        unless (oldseqno' == oldseqno) $
          STM.throwSTM (DbSeqNoIncorrect (fromEnum oldseqno') (fromEnum oldseqno))
        unless (newseqno > oldseqno) $
          STM.throwSTM (DbSeqNoNotMonotonic (fromEnum oldseqno) (fromEnum newseqno))
        STM.writeTVar seqnovar newseqno
        for_ diffs_and_snapshots $ \case
          Left diffs -> traverse2Tables_ writeTVarDbTable tables diffs
          Right ss -> do
            swizzled_mb_tables <- interpretTableSnapshots take_snapshot (pure $ ComposeTable Nothing) ss tables
            traverse2Tables_ combine_swizzle swizzled_mb_tables tables
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
        let (MapRW m') = applyDiff (MapRW m) d
        STM.writeTVar tv m'

      writeTVarDbTable TableTagRWU (TVarDbTable tv) (TableDiffRWU (DiffMapRWU d)) = do
        m <- STM.readTVar tv
        let (MapRWU m') = applyDiff (MapRWU m) d
        STM.writeTVar tv m'

      take_snapshot (TVarDbTable tv) = (ComposeTable . Just . WrappedMap) <$> STM.readTVar tv

      combine_swizzle _ (ComposeTable mb_m) (TVarDbTable tv) = case mb_m of
        Nothing -> pure ()
        Just (WrappedMap m) -> STM.writeTVar tv m
        
data DbSeqNoException =
       DbSeqNoIncorrect    Int Int -- ^ Expected and given sequence number
     | DbSeqNoNotMonotonic Int Int -- ^ Current and given sequence number
  deriving Show

instance Exception DbSeqNoException


