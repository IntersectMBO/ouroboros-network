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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module UTxODb.Snapshots where

import           Prelude hiding (lookup, seq)

import           Data.Kind
import           Data.Functor.Identity
import           Data.Foldable

--import qualified Data.Hashable as H
import           Data.Monoid
import qualified Data.List as List
import           Data.Maybe (isJust, fromMaybe)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set
import qualified Data.FingerTree as FT
import qualified Data.STRef as ST
import           Data.STRef (STRef)
import           Data.Int (Int64)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (TVar, STM)

import           Control.Monad
import qualified Control.Monad.ST.Strict as ST
import           Control.Monad.ST.Strict (ST)
import           Control.Exception
import Data.Functor.Const
import Data.Functor
import Data.Binary (Binary)


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

emptyPMapRO :: PMapRO k v
emptyPMapRO = PMapRO (PMapRep Map.empty Set.empty)

newtype PMapRW  k v = PMapRW  (PMapRep k v)
  deriving (Eq, Show, MappingR, MappingW)

newtype PMapRWU k v = PMapRWU (PMapRep k v)
  deriving (Eq, Show, MappingR, MappingW, MappingU)


castPMapRWToRO :: PMapRW  k v -> PMapRO k v
castPMapRWToRO (PMapRW m) = PMapRO m

castPMapRWUToRO :: PMapRWU k v -> PMapRO k v
castPMapRWUToRO (PMapRWU m) = PMapRO m


--
-- Diffs
--------

class Monoid (Diff a) => Changes a where
  type Diff a

  applyDiff :: a -> Diff a -> a


--
-- Diffs on mappings
--------------------

newtype DiffMapRW k v = DiffMapRW (Map.Map k (DiffMapRWElem v))
  deriving (Eq, Show)

data DiffMapRWElem a = MapRWElemDelete
                     | MapRWElemInsert !a
  deriving (Eq, Show)

instance Semigroup (DiffMapRWElem a) where
  _ <> y = y --TODO: check if this should be left or right biased

instance Ord k => Monoid (DiffMapRW k v) where
  mempty = DiffMapRW Map.empty

instance Ord k => Semigroup (DiffMapRW k v) where
  DiffMapRW a <> DiffMapRW b = DiffMapRW (Map.unionWith (<>) a b)

instance MappingW DiffMapRW where
  insert k v (DiffMapRW m) =
    DiffMapRW (Map.insert k (MapRWElemInsert v) m)

  delete k (DiffMapRW m) =
    DiffMapRW (Map.insert k  MapRWElemDelete m)

applyDiffMapRW :: Ord k => MapRW k v -> DiffMapRW k v -> MapRW k v
applyDiffMapRW (MapRW m) (DiffMapRW md) =
    MapRW (Map.merge
             Map.preserveMissing
             (Map.mapMaybeMissing      insertDiffElem)
             (Map.zipWithMaybeMatched  applyDiffElem)
             m
             md)
  where
    insertDiffElem :: k -> DiffMapRWElem a -> Maybe a
    insertDiffElem _    MapRWElemDelete     = Nothing
    insertDiffElem _ (  MapRWElemInsert x)  = Just x

    applyDiffElem :: k -> a -> DiffMapRWElem a -> Maybe a
    applyDiffElem _ _    MapRWElemDelete     = Nothing
    applyDiffElem _ _ (  MapRWElemInsert x)  = Just x


instance Ord k => Changes (MapRW k v) where
  type Diff (MapRW k v) = DiffMapRW k v
  applyDiff = applyDiffMapRW

instance Ord k => Changes (PMapRW k v) where
  type Diff (PMapRW k v) = DiffMapRW k v

  applyDiff (PMapRW (PMapRep m s)) dm =
      PMapRW (PMapRep m' s)
    where
      MapRW m' = applyDiffMapRW (MapRW m) dm


newtype DiffMapRWU k v = DiffMapRWU (Map.Map k (DiffMapRWUElem v))
    deriving (Eq, Show)

data DiffMapRWUElem a = MapRWUElemDelete
                      | MapRWUElemInsert !a
                      | MapRWUElemUpdate !a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (DiffMapRWUElem a) where
  _                  <> MapRWUElemDelete   = MapRWUElemDelete
  _                  <> MapRWUElemInsert y = MapRWUElemInsert  y

  -- TODO check whether this should insert the element. Tim Sheard argues not.
  MapRWUElemDelete   <> MapRWUElemUpdate y = MapRWUElemInsert  y
  MapRWUElemInsert x <> MapRWUElemUpdate y = MapRWUElemInsert (x <> y)
  MapRWUElemUpdate x <> MapRWUElemUpdate y = MapRWUElemUpdate (x <> y)

instance (Ord k, Semigroup v) => Monoid (DiffMapRWU k v) where
  mempty = DiffMapRWU Map.empty

instance (Ord k, Semigroup v) => Semigroup (DiffMapRWU k v) where
  DiffMapRWU a <> DiffMapRWU b = DiffMapRWU (Map.unionWith (<>) a b)

instance MappingW DiffMapRWU where
  insert k v (DiffMapRWU m) =
    DiffMapRWU (Map.insert k (MapRWUElemInsert v) m)

  delete k (DiffMapRWU m) =
    DiffMapRWU (Map.insert k  MapRWUElemDelete m)

instance MappingU DiffMapRWU where
  update k v (DiffMapRWU m) =
    DiffMapRWU (Map.insertWith (<>) k (MapRWUElemUpdate v) m)

applyDiffMapRWU :: forall k v.
                   (Ord k, Semigroup v)
                => MapRWU k v -> Diff (MapRWU k v) -> MapRWU k v
applyDiffMapRWU (MapRWU m) (DiffMapRWU md) =
    MapRWU (Map.merge
              Map.preserveMissing
              (Map.mapMaybeMissing      insertDiffElem)
              (Map.zipWithMaybeMatched  applyDiffElem)
              m
              md)
  where
    insertDiffElem :: k -> DiffMapRWUElem v -> Maybe v
    insertDiffElem _    MapRWUElemDelete     = Nothing
    insertDiffElem _ (  MapRWUElemInsert x)  = Just x
    insertDiffElem _ (  MapRWUElemUpdate x)  = Just x

    applyDiffElem :: k -> v -> DiffMapRWUElem v -> Maybe v
    applyDiffElem _ _    MapRWUElemDelete     = Nothing
    applyDiffElem _ _ (  MapRWUElemInsert y)  = Just y
    applyDiffElem _ x (  MapRWUElemUpdate y)  = Just (x<>y)

instance (Ord k, Semigroup v) => Changes (MapRWU k v) where
  type Diff (MapRWU k v) = DiffMapRWU k v
  applyDiff = applyDiffMapRWU

instance (Ord k, Semigroup v) => Changes (PMapRWU k v) where
  type Diff (PMapRWU k v) = DiffMapRWU k v

  applyDiff (PMapRWU (PMapRep m s)) dm =
      PMapRWU (PMapRep m' s)
    where
      MapRWU m' = applyDiffMapRWU (MapRWU m) dm


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
mkPTMapRW pm = PTMapRW pm (DiffMapRW Map.empty)

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
mkPTMapRWU pm = PTMapRWU pm (DiffMapRWU Map.empty)

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

data PairTable tablea tableb (t :: TableType) k v =
       PairTable (tablea t k v) (tableb t k v)
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

instance (ShowTable table, Show a) => ShowTable (AnnTable table a) where
  showsPrecTable tag _ (AnnTable t a) = showString "AnnTable { ann=" . shows a . showString ", table=" . showsPrecTable tag 0 t . showString " }"

instance (MonoidalTable table, Monoid a) => MonoidalTable (AnnTable table a) where
  emptyTable tag = AnnTable (emptyTable tag)  mempty
  appendTables tag (AnnTable a1 t1) (AnnTable a2 t2) = AnnTable (appendTables tag a1 a2) (t1 <> t2)

data TableReadSet (t :: TableType) k v where
     TableReadSetRO  ::
       { troPTMap :: PMapRO k v
       , troRowCount ::  Maybe Int
       , troRangeQueryResults :: Map.Map (Maybe k) (Map.Map Int [(k, v)])
       } -> TableReadSet TableTypeRO  k v
     TableReadSetRW  :: PMapRW  k v -> TableReadSet TableTypeRW  k v
     TableReadSetRWU :: PMapRWU k v -> TableReadSet TableTypeRWU k v

instance ShowTable TableReadSet where
  showsPrecTable _ d (TableReadSetRW (PMapRW (PMapRep m s))) =
    showString "TableReadSetRW { result=" . shows m . shows ", keys=" . shows s . showString " }"
  showsPrecTable _ d (TableReadSetRWU (PMapRWU (PMapRep m s))) =
    showString "TableReadSetRWU { result=" . shows m . shows ", keys=" . shows s . showString " }"
  showsPrecTable _ d TableReadSetRO {troPTMap = PMapRO (PMapRep m s), ..} =
    showString "TableReadSetRO { result=" . shows m . shows ", keys=" . shows s . shows ", rowCount=" .
    shows troRowCount . shows ", rangeQueryResults=unimplemented" . showString " }"


emptyTableReadSetRO :: TableReadSet TableTypeRO k v
emptyTableReadSetRO   = TableReadSetRO { troPTMap = emptyPMapRO, troRowCount = Nothing, troRangeQueryResults = Map.empty }

data TrackingTable (t :: TableType) k v where
     TrackingTableRO  :: TableReadSet TableTypeRO k v   -> TrackingTable TableTypeRO  k v
     TrackingTableRW  :: PTMapRW  k v -> TrackingTable TableTypeRW  k v
     TrackingTableRWU :: PTMapRWU k v -> TrackingTable TableTypeRWU k v

instance MappingR (TrackingTable TableTypeRO) where
  lookup k (TrackingTableRO TableReadSetRO{troPTMap}) = lookup k troPTMap

instance MappingR (TrackingTable TableTypeRW) where
  lookup k (TrackingTableRW pt) = lookup k pt

instance MappingR (TrackingTable TableTypeRWU) where
  lookup k (TrackingTableRWU pt) = lookup k pt

instance MappingW (TrackingTable TableTypeRW) where
  insert k v (TrackingTableRW pt) = TrackingTableRW $ insert k v pt
  delete k (TrackingTableRW pt) = TrackingTableRW $ delete k pt

instance MappingW (TrackingTable TableTypeRWU) where
  insert k v (TrackingTableRWU pt) = TrackingTableRWU $ insert k v pt
  delete k (TrackingTableRWU pt) = TrackingTableRWU $ delete k pt

instance MappingU (TrackingTable TableTypeRWU) where
  update k v (TrackingTableRWU pt) = TrackingTableRWU (update k v pt)

data TableDiff (t :: TableType) k v where
     TableDiffRO  ::                   TableDiff TableTypeRO  k v
     TableDiffRW  :: DiffMapRW  k v -> TableDiff TableTypeRW  k v
     TableDiffRWU :: DiffMapRWU k v -> TableDiff TableTypeRWU k v

-- TODO integrate range queries here
mkTableReadSet :: Ord k => TableTag t v -> TableKeySet t k v -> Map.Map k v -> TableReadSet t k v
mkTableReadSet TableTagRW  (TableKeySet ks) m = TableReadSetRW  (PMapRW  (mkPMapRep m ks))
mkTableReadSet TableTagRWU (TableKeySet ks) m = TableReadSetRWU (PMapRWU (mkPMapRep m ks))
mkTableReadSet TableTagRO  TableQuery {tqKeySet, tqCountRows, tqRangeQueries} m = TableReadSetRO
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

mkTrackingTable :: TableReadSet t k v -> TrackingTable t k v
mkTrackingTable m@TableReadSetRO  {} = TrackingTableRO  m
mkTrackingTable (TableReadSetRW  m) = TrackingTableRW  (mkPTMapRW  m)
mkTrackingTable (TableReadSetRWU m) = TrackingTableRWU (mkPTMapRWU m)

getTableDiff :: TrackingTable t k v -> TableDiff t k v
getTableDiff (TrackingTableRO _)  = TableDiffRO
getTableDiff (TrackingTableRW  m) = TableDiffRW  (getDiffMapRW  m)
getTableDiff (TrackingTableRWU m) = TableDiffRWU (getDiffMapRWU m)


trackingTablesToTableDiffs :: forall state. HasTables state => state TrackingTable -> state TableDiff
trackingTablesToTableDiffs = mapTables (const getTableDiff)

annotatedReadsetToTrackingTables :: forall a state. HasTables state => state (AnnTable TableReadSet a) -> state TrackingTable
annotatedReadsetToTrackingTables = mapTables go where
  go :: forall t k v. TableTag t v -> AnnTable TableReadSet a t k v -> TrackingTable t k v
  go tag (AnnTable t _) = mkTrackingTable t

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

  traverseTables_ :: forall f table. Applicative f
                    => (forall (t :: TableType) k v.
                               Ord k
                            => StateTableKeyConstraint   state k
                            => StateTableValueConstraint state v
                            => TableTag t v -> table t k v -> f ())
                    -> state table -> f ()
  traverseTables_ g s = let
    g' :: forall t k v. (Ord k, StateTableKeyConstraint state k, StateTableValueConstraint state v) => TableTag t v -> table t k v -> f (EmptyTable t k v)
    g' tag tab = (g tag tab :: f ()) $> EmptyTable
    nearly :: f (state EmptyTable)
    nearly = traverseTables g' s
    in void nearly

  foldMapTables :: Monoid a
                => (forall (t :: TableType) k v.
                   Ord k
                => StateTableKeyConstraint   state k
                => StateTableValueConstraint state v
                => TableTag t v -> table t k v -> a)
                -> state table -> a
  foldMapTables f ts = getConst $ traverseTables (\t tb -> Const (f t tb)) ts

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

  traverse2Tables_ :: forall f table table'. Applicative f
                   => (forall (t :: TableType) k v.
                              Ord k
                           => StateTableKeyConstraint   state k
                           => StateTableValueConstraint state v
                           => TableTag t v -> table t k v -> table' t k v -> f ())
                   -> state table -> state table' -> f ()
  traverse2Tables_ f s1 s2 = let
    nearly :: f (state EmptyTable)
    nearly = traverse2Tables (\t t1 t2 -> f t t1 t2 $> EmptyTable) s1 s2
    in void nearly


  -- The common pure case, not needing Applicative
  constTables :: (forall (t :: TableType) k v.
                         Ord k
                      => StateTableKeyConstraint   state k
                      => StateTableValueConstraint state v
                      => TableTag t v -> table t k v)
              -> state table
  constTables f = runIdentity (traverse0Tables (pure . f))

  zipTables  :: forall table table' table''.
                (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v -> table'' t k v)
             -> state table -> state table' -> state table''
  zipTables f a b = runIdentity (traverse2Tables (\t x y -> pure (f t x y)) a b)

  zip3Tables  :: forall table table' table'' table'''.
                 (forall (t :: TableType) k v.
                        Ord k
                     => StateTableKeyConstraint   state k
                     => StateTableValueConstraint state v
                     => TableTag t v -> table t k v -> table' t k v -> table'' t k v -> table''' t k v)
             -> state table -> state table' -> state table'' -> state table'''
  zip3Tables f a b c = zip3rd (zippair a b) c
    where
      zippair :: state table -> state table' -> state (PairTable table table')
      zippair = zipTables (\_ -> PairTable)

      zip3rd :: state (PairTable table table') -> state table'' -> state table'''
      zip3rd = zipTables (\t (PairTable x y) z -> f t x y z)


class (HasTables state, HasOnlyTables (Tables state))
   => HasOnDiskTables state where

  data Tables state :: StateKind

  projectTables :: state table -> Tables state table
  injectTables  :: Tables state table -> state any -> state table

class HasSeqNo (state :: StateKind) where
  stateSeqNo :: state table -> SeqNo state

newtype SeqNo (state :: StateKind) = SeqNo { seqnoToWord :: Int64 }
  deriving stock (Eq, Ord, Bounded, Show)
  deriving newtype (Enum, Binary)



class MonoidalTable table where
    emptyTable   :: Ord k => TableTag t v -> table t k v
    appendTables :: Ord k => TableTag t v -> table t k v -> table t k v -> table t k v

instance MonoidalTable TableKeySet where
    emptyTable   _ = TableKeySet Set.empty
    appendTables _ (TableKeySet a) (TableKeySet b) = TableKeySet (Set.union a b)
    appendTables _ (TableKeySet a) tq@TableQuery{tqKeySet}  = tq { tqKeySet = Set.union a tqKeySet }
    appendTables _ tq@TableQuery{tqKeySet} (TableKeySet b) = tq { tqKeySet = Set.union tqKeySet b}
    appendTables _ a@TableQuery{} b@TableQuery{} = TableQuery
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


class ShowTable table where
  showsPrecTable :: (Show k, Show v) => TableTag t v -> Int -> table t k v -> ShowS



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


emptyTables :: HasOnlyTables state => state EmptyTable
emptyTables = runIdentity $ traverse0Tables $ \_ -> pure EmptyTable


-- In-memory changes
--------------------

-- | A sequence of states of a partitioned in-memory \/ on-disk data store. It
-- includes both partitions: the in-memory and on-disk parts.
--
-- It is intended to be used to /extend in-memory/ beyond the state of an
-- on-disk data store. It allows new changes to be added in memory, and old
-- changes to be removed, typically to flush to disk. In this sense it behaves
-- like a FIFO queue with new changes added and one end and old changes removed
-- at the other.
--
-- Most of the operations rely on the db state being in the 'HasOnDiskTables'
-- class. It is also designed to be used with a disk DB from the 'DiskDb' class.
--
-- The in-memory part is represented as a sequence of values and relies on
-- persistent data structures to achieve sharing between the many copies of the
-- value.
--
-- The on-disk part is represented (in-memory) as a sequence of /changes/ to the
-- on-disk tables.
--
-- These two sequences match up, in the sense of the sequence numbers of one
-- being a suffix of the other. The sequence of on-disk changes can extend
-- somewhat further back. This is to enable flushing changes to disk in bigger
-- batches.
--
-- There are two \"anchor points\":
-- 1. the anchor of the logical sequence of states; and
-- 2. the anchor of the on-disk data store.
--
-- The first is the limit of how far back we can rewind. This corresponds to
-- the sequence number /preceding/ the sequence of in-memory changes. The
-- second must correspond to the sequence number for the on-disk store. This
-- second anchor point ensures that operations are consistent with the on-disk
-- store.
--
data DbChangelog (state :: (TableType -> * -> * -> *) -> *) =
       DbChangelog {

         -- | The \"anchor\" point for the logical sequence of states.
         -- This gives us the rollback limit.
         dbChangelogStateAnchor :: SeqNo state,

         -- | The \"anchor\" point for the on-disk data store. This must match
         -- the sequence number for the data store so that reads and writes
         -- are consistent.
         dbChangelogDiskAnchor  :: SeqNo state,

         -- | The sequence of in-memory-only states. This /includes/ the anchor
         -- point as its first (oldest) element, and is therefore non-empty.
         dbChangelogStates      :: Seq state (state EmptyTable),

         --TODO: upon integration the representation of dbChangelogStates and
         -- dbChangelogStateAnchor could be changed to use
         -- AnchoredSeq (SeqNo state) (SeqNo state) (state EmptyTable)

         -- | The changes to the content of the on-disk tables. The sequence
         -- numbers for these changes match the in-memory-only sequence:
         -- the in-memory changes are a suffix. This /excludes/ the anchor
         -- point and so the sequence can be empty.
         dbChangelogTableDiffs :: !(Tables state (SeqTableDiff state)),

         -- The changes involving table snapshots
         -- this is sparse, often empty, in practice never more than one element
         dbChangelogSnapshots :: !(SSeq state (TableSnapshots state)),

         -- The derived changes to apply to reads for snapshotted tables.
         dbChangelogSnapshotDiffs :: !(Tables state (SeqSnapshotDiff state))
       }

invariantDbChangelog :: forall state.
                        HasOnDiskTables state => DbChangelog state -> Bool
invariantDbChangelog DbChangelog{..} =

    -- The state anchor must be the first element in the states seq
    (case FT.viewl dbChangelogStates of
       FT.EmptyL               -> False  -- must be non-empty
       SeqElem seqno _ FT.:< _ -> seqno == dbChangelogStateAnchor)

    -- The state sequence numbers must be increasing
 && increasing [ s | SeqElem  s _ <- Foldable.toList dbChangelogStates ]

    -- The disk anchor can be earlier than or the same as the main state anchor
 && dbChangelogDiskAnchor <= dbChangelogStateAnchor

    -- The disk diff sequence numbers must match the state sequence numbers.
    -- Depending on whether dbChangelogDiskAnchor == dbChangelogStateAnchor or
    -- not, then one is a suffix of the other, or the other way around.
 && (let isSuffix :: TableTag t v -> SeqTableDiff state t k v -> All
         isSuffix _  SeqTableDiffRO = All True
         isSuffix _ (SeqTableDiffRW  seq)
           | dbChangelogDiskAnchor == dbChangelogStateAnchor
           = All $ sseqSeqNos seq `List.isSuffixOf` seqSeqNos dbChangelogStates

           | otherwise
           = All $ seqSeqNos dbChangelogStates `List.isSuffixOf` sseqSeqNos seq

         isSuffix _ (SeqTableDiffRWU seq)
           | dbChangelogDiskAnchor == dbChangelogStateAnchor
           = All $ sseqSeqNos seq `List.isSuffixOf` seqSeqNos dbChangelogStates

           | otherwise
           = All $ seqSeqNos dbChangelogStates `List.isSuffixOf` sseqSeqNos seq

      in getAll (foldMapTables isSuffix dbChangelogTableDiffs))

    -- The disk anchor must be earlier than the disk diff sequence numbers and
    -- the disk diff sequence numbers must be increasing. (This is not already
    -- implied by the previous conditions because the disk diff sequence can be
    -- longer than the state sequence numbers.)
 && (let increasingSeqNos :: TableTag t v -> SeqTableDiff state t k v -> All
         increasingSeqNos _ SeqTableDiffRO       = All True
         increasingSeqNos _ (SeqTableDiffRW  seq) = All $
           increasing (dbChangelogDiskAnchor : sseqSeqNos seq)
         increasingSeqNos _ (SeqTableDiffRWU seq) = All $
           increasing (dbChangelogDiskAnchor : sseqSeqNos seq)
      in getAll (foldMapTables increasingSeqNos dbChangelogTableDiffs))

    -- The snapshots (if any) must be at sequence numbers from the disk diff
    -- sequence(s).
 && (let isSeqMember :: Ord k => SeqNo state
                     -> TableTag t v -> SeqTableDiff state t k v -> All
         isSeqMember _     _  SeqTableDiffRO       = All True
         isSeqMember seqno _ (SeqTableDiffRW  seq) = All $
           isJust (splitAtAfterSSeq seqno seq)
         isSeqMember seqno _ (SeqTableDiffRWU seq) = All $
           isJust (splitAtAfterSSeq seqno seq)
      in and [ getAll (foldMapTables (isSeqMember seqno) dbChangelogTableDiffs)
             | seqno <- sseqSeqNos dbChangelogSnapshots ])

-- TODO: check
-- &&    dbChangelogSnapshotDiffs
--    == cacheSnapshotDiffs dbChangelogTableDiffs dbChangelogSnapshots
  where
    -- TODO: increasing or strictly increasing: does this need to be relaxed to
    -- use (<=) ? What about EBBs? Do they appear in the ledger state seq?
    -- TODO: how about the split operations guaranteeing that we never split
    -- between equal slot nos. Then we can keep the invariant that the disk
    -- anchor point is /strictly/ before the diffs.
    increasing :: Ord a => [a] -> Bool
    increasing xs = and (zipWith (<) xs (drop 1 xs))

    seqSeqNos :: Seq state a -> [SeqNo state]
    seqSeqNos  seq = [ seqno | SeqElem  seqno _ <- Foldable.toList seq ]

    sseqSeqNos :: SSeq state a -> [SeqNo state]
    sseqSeqNos seq = [ seqno | SSeqElem seqno _ <- Foldable.toList seq ]


initialDbChangelog :: HasOnDiskTables state
                   => SeqNo state
                   -> state EmptyTable
                   -> DbChangelog state
initialDbChangelog anchorSeqNo anchorState =
    DbChangelog anchorSeqNo anchorSeqNo
                (FT.singleton (SeqElem anchorSeqNo anchorState))
                mempty mempty mempty

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
                        !(state TableDiff)
                        !(Maybe (TableSnapshots state))

-- |
--
extendDbChangelog :: forall state.
                     HasOnDiskTables state
                  => SeqNo state
                  -> state TableDiff
                  -> Maybe (TableSnapshots state)
                  -> DbChangelog state
                  -> DbChangelog state
extendDbChangelog !seqno !state Nothing =
    extendDbChangelogDiffs seqno state

extendDbChangelog !seqno !state (Just snapshots) =
    extendDbChangelogSnapshots seqno snapshots
  . extendDbChangelogDiffs     seqno state

extendDbChangelogDiffs :: forall state.
                          HasOnDiskTables state
                       => SeqNo state
                       -> state TableDiff
                       -> DbChangelog state
                       -> DbChangelog state
extendDbChangelogDiffs !seqno !state dbchangelog@DbChangelog{..} =
    let !diffs  = projectTables state
        !state' = mapTables (\_ _ -> EmptyTable) state
     in dbchangelog {
          dbChangelogStates     = dbChangelogStates FT.|> SeqElem seqno state',
          dbChangelogTableDiffs = zipTables snoc dbChangelogTableDiffs diffs
        }
  where
    snoc :: Ord k
         => TableTag           t   v
         -> SeqTableDiff state t k v
         -> TableDiff          t k v
         -> SeqTableDiff state t k v
    snoc _ ds d = snocSeqTableDiff ds d seqno

extendDbChangelogSnapshots :: HasOnDiskTables state
                           => SeqNo state
                           -> TableSnapshots state
                           -> DbChangelog state
                           -> DbChangelog state
extendDbChangelogSnapshots !seqno snapshots dbchangelog@DbChangelog{..} =
    dbchangelog {
      dbChangelogSnapshots =
        dbChangelogSnapshots FT.|> SSeqElem seqno snapshots,

      dbChangelogSnapshotDiffs =
        extendSnapshotDiffs
          snapshots
          dbChangelogTableDiffs
          dbChangelogSnapshotDiffs
    }

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

cacheSnapshotDiffs :: HasOnlyTables (Tables state)
                   => Tables state (SeqTableDiff state)
                   -> SSeq state (TableSnapshots state)
                   -> Tables state (SeqSnapshotDiff state)
cacheSnapshotDiffs diffs snapshots =
   -- The algorithm starts with the sequence of snapshot operations and
   -- taking prefixes of diffs at the corresponding seqnos. Then working
   -- from oldest to newest it extends the snapshot diffs for each snapshot
   -- and the prefix of the diffs from the corresponding seqno.
   List.foldl'
     (\sdiffs (snapshot, diffsPrefix) ->
         extendSnapshotDiffs snapshot diffsPrefix sdiffs)
     mempty
     [ (snapshot, diffsPrefix)
     | SSeqElem seqno snapshot <- Foldable.toList snapshots
     , let diffsPrefix = mapTables (\_ -> fst . splitAfterSeqTableDiff seqno) diffs ]


-- | Get the first and last 'SeqNo' in the sequence of changes.
-- TODO: or maybe we only need the anchor?
--
stateAnchorDbChangelog :: DbChangelog state -> SeqNo state
stateAnchorDbChangelog DbChangelog {dbChangelogStateAnchor} = dbChangelogStateAnchor

diskAnchorDbChangelog :: DbChangelog state -> SeqNo state
diskAnchorDbChangelog DbChangelog {dbChangelogDiskAnchor} = dbChangelogDiskAnchor

endOfDbChangelog :: DbChangelog state -> SeqNo state
endOfDbChangelog DbChangelog {dbChangelogStates}
  | _ FT.:> SeqElem seqno _ <- FT.viewr dbChangelogStates
  = seqno

  | otherwise
  = error "endOfDbChangelog: invariant violation: empty state seq"

currentStateDbChangeLog :: DbChangelog state -> state EmptyTable
currentStateDbChangeLog DbChangelog { dbChangelogStates = (FT.viewr -> _ FT.:> (SeqElem _ x))} = x
currentStateDbChangeLog _ = error "impossible"

-- | Reset the \"leading edge\" of the sequence of states to the given 'SeqNo'.
--
-- The given 'SeqNo' must be an element of the sequence (or the result will be
-- @Nothing@). The given 'SeqNo' will be the new last 'SeqNo' for the result.
--
rewindDbChangelog :: HasOnDiskTables state
                  => SeqNo state
                  -> DbChangelog state
                  -> Maybe (DbChangelog state)
rewindDbChangelog seqno dbchangelog@DbChangelog{..} = do
    -- Check that the state sequence has the seqno and we reset it back to
    -- that point, so the seqno is the last element of the new sequence.
    (dbChangelogStates', _) <- splitAtAfterSeq seqno dbChangelogStates

    -- Do the same for the table diffs, but these should not fail
    -- (since the invariant requires they match).
    let dbChangelogTableDiffs' =
          mapTables (\_ -> fst . splitAfterSeqTableDiff seqno)
                    dbChangelogTableDiffs

        -- The "no change" case is very common and the alternative has some
        -- cost, so we special case it:
        snapshotNoChanges = 
          case FT.viewr dbChangelogSnapshots of
            FT.EmptyR                                   -> True
            _ FT.:> SSeqElem seqno' _ | seqno >= seqno' -> True
            _                                           -> False
        dbChangelogSnapshots'
          | snapshotNoChanges = dbChangelogSnapshots 
          | otherwise         = fst (splitAfterSSeq seqno dbChangelogSnapshots)

        dbChangelogSnapshotDiffs'
          | snapshotNoChanges = dbChangelogSnapshotDiffs
          | otherwise         = cacheSnapshotDiffs dbChangelogTableDiffs
                                                   dbChangelogSnapshots

    Just dbchangelog {
      dbChangelogStates        = dbChangelogStates',
      dbChangelogTableDiffs    = dbChangelogTableDiffs',
      dbChangelogSnapshots     = dbChangelogSnapshots',
      dbChangelogSnapshotDiffs = dbChangelogSnapshotDiffs'
    }


-- | Advance the \"trailing edge\" to the given 'SeqNo'. This does not affect
-- the disk diffs which must be advanced separately using 'flushDbChangelog'.
--
-- The given 'SeqNo' must be an element of the sequence (or the result will be
-- @Nothing@). This 'SeqNo' will be the new anchor element.
--
advanceDbChangelog :: SeqNo state -> DbChangelog state -> Maybe (DbChangelog state)
advanceDbChangelog seqno dbchangelog@DbChangelog{..} = do
    -- Check that the state sequence has the seqno, and keep everything at and
    -- after the seqno. The seqno becomes the new anchor.
    -- Note that we make no changes to the disk parts.
    (_, dbChangelogStates') <- splitAtBeforeSeq seqno dbChangelogStates
    Just dbchangelog {
      dbChangelogStates      = dbChangelogStates',
      dbChangelogStateAnchor = seqno
    }

-- | Advance the disk diffs to the same 'SeqNo' as the states. This always
-- succeeds but will be a no-op if the two anchor points are already the same.
--
-- The disk diffs are returned in the format required by 'writeDb'.
--
flushDbChangelog :: HasOnDiskTables state
                 => DbChangelog state
                 -> ([Either (TableDiffs state) (TableSnapshots state)],
                     DbChangelog state)
flushDbChangelog dbchangelog@DbChangelog{..} =
    (toDiskDbWrites diffsToFlush snapshotsToFlush, dbchangelog')
  where
    !dbchangelog' =
      dbchangelog {
        dbChangelogDiskAnchor = dbChangelogStateAnchor,

        dbChangelogTableDiffs =
          mapTables (\_ -> snd . splitAfterSeqTableDiff dbChangelogStateAnchor)
                    dbChangelogTableDiffs,

        dbChangelogSnapshots  = dbChangelogSnapshots',

        dbChangelogSnapshotDiffs =
          mapTables (\_ -> snd . splitAfterSeqSnapshotDiff dbChangelogStateAnchor)
                    dbChangelogSnapshotDiffs
      }

    !diffsToFlush =
      mapTables (\_ -> fst . splitAfterSeqTableDiff dbChangelogStateAnchor)
                dbChangelogTableDiffs

    !(snapshotsToFlush, dbChangelogSnapshots') =
      splitAfterSSeq dbChangelogStateAnchor dbChangelogSnapshots

-- | Helper function to squash the DbChangelog's sequences of changes to the
-- table contents and sequence of snapshots into the summary form needed by
-- 'writeDb' to write to disk.
--
-- We interleave the snapshots with the table diffs according to the seqnos.
-- For any seqno with a snapshot, the snapshot comes /after/ the diffs at the
-- same seqno. Contiguous sequences of diffs are merged together into a single
-- diff for efficiency for writing to disk.
--
toDiskDbWrites :: HasOnDiskTables state
               => Tables state (SeqTableDiff state)
               -> SSeq state (TableSnapshots state)
               -> [Either (TableDiffs state) (TableSnapshots state)]
toDiskDbWrites !diffs !snapshots =
    -- Split on the slot number of the next snapshot, if any.
    case FT.viewl snapshots of
      FT.EmptyL ->
          Left (mapTables (\_ -> summarySeqTableDiff) diffs)
        : []

      SSeqElem seqno snapshot FT.:< snapshots' ->
          Left (mapTables (\_ -> summarySeqTableDiff
                               . fst . splitAfterSeqTableDiff seqno)
                          diffs)
        : Right snapshot
        : toDiskDbWrites diffs' snapshots'
        where
          diffs' = mapTables (\_ -> snd . splitAfterSeqTableDiff seqno) diffs


  --TODO: split the table diffs at the state anchor point
  -- return the summary of the bit split off


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
rewindTableKeySets (DbChangelog _ _ _ _ snapshots _) tks =
    -- Push the key sets through the inverse of the prevailing composition
    -- of snapshot transformations (if any). Also annotate the key sets
    -- with the seqno of the /last/ snapshot (if any) for later sanity checks.
    case FT.measure snapshots of
      SSeqSummaryEmpty ->
        mapTables (\_ x -> AnnTable x Nothing) tks

      SSeqSummary _ maxseqno sn ->
        mapTables (\_ x -> AnnTable x (Just maxseqno))
                  (applyTableSnapshotsInverse sn tks)


forwardTableReadSets :: forall state.
                        HasOnDiskTables state
                     => DbChangelog state
                     -> AnnTableReadSets state (ReadSetSanityInfo state)
                     -> Maybe (TableReadSets state)
forwardTableReadSets DbChangelog {
                       dbChangelogSnapshots,
                       dbChangelogSnapshotDiffs,
                       dbChangelogTableDiffs
                     } =
    -- The benefit of the complicated snapshot diffs now finally pays off:
    -- we apply all the snapshot transformations first, and then apply all of
    -- the content diffs.
    -- This is in contrast to the naive approach where we would have to
    -- interleave applying diffs and snapshots. We have been able to
    -- disentangle and distribute the diffs and snapshots so that we can
    -- bring all of the snapshots up front, and keep all of the diffs together.
    -- This is more complex but gives us a much more efficient algorithm for
    -- the very common operation of forwarding read sets.
    Just --TODO: check the sanity info
  . bar
  . foo
  . mapTables (\_ (AnnTable t _) -> t)
  where
    bar :: Tables state (SnapshotOfTable' TableReadSet)
        -> Tables state TableReadSet
    bar = zip3Tables applyReadSetDiffs
                     dbChangelogSnapshotDiffs
                     dbChangelogTableDiffs

    foo :: TableReadSets state
        -> Tables state (SnapshotOfTable' TableReadSet)
    foo = case summarySSeq dbChangelogSnapshots of
            Nothing -> mapTables (\_ -> KeepTable')
            Just sn -> applyTableSnapshots' sn

    applyReadSetDiffs :: Ord k
                      => TableTag t v
                      -> SeqSnapshotDiff state t k v
                      -> SeqTableDiff    state t k v
                      -> SnapshotOfTable' TableReadSet t k v
                      -> TableReadSet t k v

    applyReadSetDiffs  TableTagRW
                       SeqSnapshotDiffEmpty
                      (SeqTableDiffRW seqdiffs)
                      (KeepTable' (TableReadSetRW pm)) =
      TableReadSetRW (maybe pm (applyDiff pm) (summarySSeq seqdiffs))

    applyReadSetDiffs  TableTagRWU
                       SeqSnapshotDiffEmpty
                      (SeqTableDiffRWU seqdiffs)
                      (KeepTable' (TableReadSetRWU pm)) =
      TableReadSetRWU (maybe pm (applyDiff pm) (summarySSeq seqdiffs))

    applyReadSetDiffs  TableTagRO SeqSnapshotDiffEmpty SeqTableDiffRO
                      (KeepTable' t) = t

    applyReadSetDiffs  TableTagRO _ SeqTableDiffRO
                      (KeepTable' _) = inconsistency

    applyReadSetDiffs  TableTagRO seqdiffs SeqTableDiffRO
                      (SnapshotOfTable' readset) =
      applySnapshotDiffs readset seqdiffs


    applySnapshotDiffs :: Ord k
                       => TableReadSet          t           k v
                       -> SeqSnapshotDiff state TableTypeRO k v
                       -> TableReadSet          TableTypeRO k v
    applySnapshotDiffs (TableReadSetRW pm) (SeqSnapshotOfDiffRW seqdiffs) =
      emptyTableReadSetRO { troPTMap = castPMapRWToRO $ maybe pm (applyDiff pm) (summarySSeq seqdiffs) }

    applySnapshotDiffs (TableReadSetRWU pm) (SeqSnapshotOfDiffRWU seqdiffs) =
      emptyTableReadSetRO { troPTMap = castPMapRWUToRO $ maybe pm (applyDiff pm) (summarySSeq seqdiffs) }

    applySnapshotDiffs pm@TableReadSetRO {} SeqSnapshotDiffEmpty = pm

    -- By the invariant the swizzle and the snapshot diffs match each other.
    -- And when the read set is passed through the swizzle inverse then the
    -- read set will also match. So all other combinations are inconsistent.
    applySnapshotDiffs TableReadSetRW{}  SeqSnapshotDiffEmpty   = inconsistency
    applySnapshotDiffs TableReadSetRWU{} SeqSnapshotDiffEmpty   = inconsistency

    applySnapshotDiffs TableReadSetRO{}  SeqSnapshotOfDiffRW{}  = inconsistency
    applySnapshotDiffs TableReadSetRO{}  SeqSnapshotOfDiffRWU{} = inconsistency

    applySnapshotDiffs TableReadSetRW{}  SeqSnapshotOfDiffRWU{} = inconsistency
    applySnapshotDiffs TableReadSetRWU{} SeqSnapshotOfDiffRW{}  = inconsistency

    inconsistency = error "forwardTableReadSets: inconsistency between the snapshots and snapshot diffs"


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


splitAfterSeqTableDiff :: Ord k
                       => SeqNo state
                       -> SeqTableDiff state t k v
                       -> (SeqTableDiff state t k v, SeqTableDiff state t k v)
splitAfterSeqTableDiff _ SeqTableDiffRO = (SeqTableDiffRO, SeqTableDiffRO)
splitAfterSeqTableDiff seqno (SeqTableDiffRW ds) =
  case splitAfterSSeq seqno ds of
    (before, after) -> (SeqTableDiffRW before, SeqTableDiffRW after)
splitAfterSeqTableDiff seqno (SeqTableDiffRWU ds) =
  case splitAfterSSeq seqno ds of
    (before, after) -> (SeqTableDiffRWU before, SeqTableDiffRWU after)


-- | Merge a sequence of 'TableDiff' (as a 'SeqTableDiff') down to a single
-- 'TableDiff'. This uses the usual 'Semigroup' on diffs.
--
summarySeqTableDiff :: Ord k => SeqTableDiff state t k v -> TableDiff t k v
summarySeqTableDiff  SeqTableDiffRO       = TableDiffRO
summarySeqTableDiff (SeqTableDiffRW  seq) =
    TableDiffRW (fromMaybe mempty (summarySSeq seq))
summarySeqTableDiff (SeqTableDiffRWU seq) =
    TableDiffRWU (fromMaybe mempty (summarySSeq seq))


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

splitAfterSeqSnapshotDiff :: Ord k
                          => SeqNo state
                          -> SeqSnapshotDiff state t k v
                          -> (SeqSnapshotDiff state t k v,
                              SeqSnapshotDiff state t k v)
splitAfterSeqSnapshotDiff _ SeqSnapshotDiffEmpty =
    (SeqSnapshotDiffEmpty, SeqSnapshotDiffEmpty)

splitAfterSeqSnapshotDiff seqno (SeqSnapshotOfDiffRW ds) =
  case splitAfterSSeq seqno ds of
    (before, after) -> (SeqSnapshotOfDiffRW before, SeqSnapshotOfDiffRW after)

splitAfterSeqSnapshotDiff seqno (SeqSnapshotOfDiffRWU ds) =
  case splitAfterSSeq seqno ds of
    (before, after) -> (SeqSnapshotOfDiffRWU before, SeqSnapshotOfDiffRWU after)


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
  measure (SSeqElem seqno a) = SSeqSummary seqno seqno a

boundsSSeq :: Semigroup a => SSeq state a -> Maybe (SeqNo state, SeqNo state)
boundsSSeq seq =
    case FT.measure seq of
      SSeqSummaryEmpty    -> Nothing
      SSeqSummary lb ub _ -> Just (lb, ub)

summarySSeq :: Semigroup a => SSeq state a -> Maybe a
summarySSeq seq =
    case FT.measure seq of
      SSeqSummaryEmpty  -> Nothing
      SSeqSummary _ _ s -> Just s

splitBeforeSSeq :: Semigroup a => SeqNo state -> SSeq state a -> (SSeq state a, SSeq state a)
splitBeforeSSeq seqno seq =
    FT.split (splitPredicateBeforeSSeq seqno) seq

splitAtBeforeSSeq :: Semigroup a => SeqNo state -> SSeq state a -> Maybe (SSeq state a, SSeq state a)
splitAtBeforeSSeq seqno seq
  | let result@(_before, after) = FT.split (splitPredicateBeforeSSeq seqno) seq
  , SSeqElem seqno' _ FT.:< _ <- FT.viewl after
  , seqno == seqno'
  = Just result

  | otherwise
  = Nothing

splitAfterSSeq :: Semigroup a => SeqNo state -> SSeq state a -> (SSeq state a, SSeq state a)
splitAfterSSeq seqno seq =
    FT.split (splitPredicateAfterSSeq seqno) seq

splitAtAfterSSeq :: Semigroup a => SeqNo state -> SSeq state a -> Maybe (SSeq state a, SSeq state a)
splitAtAfterSSeq seqno seq
  | let result@(before, _after) = FT.split (splitPredicateAfterSSeq seqno) seq
  , _ FT.:> SSeqElem seqno' _ <- FT.viewr before
  , seqno == seqno'
  = Just result

  | otherwise
  = Nothing

splitPredicateBeforeSSeq :: SeqNo state -> SSeqSummary state a -> Bool
splitPredicateBeforeSSeq seqno (SSeqSummary _ smax _) = smax >= seqno
splitPredicateBeforeSSeq _      SSeqSummaryEmpty      = True -- vacuously true

splitPredicateAfterSSeq :: SeqNo state -> SSeqSummary state a -> Bool
splitPredicateAfterSSeq seqno (SSeqSummary _ smax _) = smax > seqno
splitPredicateAfterSSeq _      SSeqSummaryEmpty      = True -- vacuously true

-- In the empty case, smax > seqno is vacuously true as the "max" for empty
-- is the top element of the order.


-- | A simple sequence, with sequence numbers.
--
type Seq state a =
       FT.FingerTree (SeqSummary state)
                     (SeqElem    state a)

data SeqElem state a =
       SeqElem
         !(SeqNo state)
         !a

data SeqSummary state =
       SeqSummaryEmpty
     | SeqSummary
         !(SeqNo state)  -- min seq no
         !(SeqNo state)  -- max seq no

instance Semigroup (SeqSummary state) where
  SeqSummaryEmpty <> b = b
  a <> SeqSummaryEmpty = a
  SeqSummary smina smaxa <> SeqSummary sminb smaxb =
    SeqSummary
      (min smina sminb)
      (max smaxa smaxb)

instance Monoid (SeqSummary state) where
  mempty = SeqSummaryEmpty

instance FT.Measured (SeqSummary state) (SeqElem state a) where
  measure (SeqElem seqno _) = SeqSummary seqno seqno

boundsSeq :: Seq state a -> Maybe (SeqNo state, SeqNo state)
boundsSeq seq =
    case FT.measure seq of
      SeqSummaryEmpty  -> Nothing
      SeqSummary lb ub -> Just (lb, ub)

splitBeforeSeq :: SeqNo state -> Seq state a -> (Seq state a, Seq state a)
splitBeforeSeq seqno seq =
    FT.split (splitPredicateBeforeSeq seqno) seq

splitAtBeforeSeq :: SeqNo state -> Seq state a -> Maybe (Seq state a, Seq state a)
splitAtBeforeSeq seqno seq
  | let result@(_before, after) = FT.split (splitPredicateBeforeSeq seqno) seq
  , SeqElem seqno' _ FT.:< _ <- FT.viewl after
  , seqno == seqno'
  = Just result

  | otherwise
  = Nothing

splitAfterSeq :: SeqNo state -> Seq state a -> (Seq state a, Seq state a)
splitAfterSeq seqno seq =
    FT.split (splitPredicateAfterSeq seqno) seq

splitAtAfterSeq :: SeqNo state -> Seq state a -> Maybe (Seq state a, Seq state a)
splitAtAfterSeq seqno seq
  | let result@(before, _after) = FT.split (splitPredicateAfterSeq seqno) seq
  , _ FT.:> SeqElem seqno' _ <- FT.viewr before
  , seqno == seqno'
  = Just result

  | otherwise
  = Nothing

splitPredicateBeforeSeq :: SeqNo state -> SeqSummary state -> Bool
splitPredicateBeforeSeq seqno (SeqSummary _ smax) = smax >= seqno
splitPredicateBeforeSeq _      SeqSummaryEmpty    = True -- vacuously true

splitPredicateAfterSeq :: SeqNo state -> SeqSummary state -> Bool
splitPredicateAfterSeq seqno (SeqSummary _ smax) = smax > seqno
splitPredicateAfterSeq _      SeqSummaryEmpty    = True -- vacuously true

-- In the empty case, smax > seqno is vacuously true as the "max" for empty
-- is the top element of the order.

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
class DiskDb dbhandle state | dbhandle -> state where
  readDb  :: dbhandle
          -> AnnTableKeySets state a
          -> IO (AnnTableReadSets state (a, SeqNo state))

  writeDb :: dbhandle
          -> [Either (TableDiffs state) (TableSnapshots state)]
          -> SeqNo state -- ^ The old sequence number, as a sanity check
          -> SeqNo state -- ^ The new sequence number, must be strictly greater
          -> IO ()
