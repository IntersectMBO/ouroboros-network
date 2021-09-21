{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module LedgerOnDisk.V2.Fingertree
  
where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree)

import LedgerOnDisk.V2.Diff (DiffMap, MapFlavour (MK_RW), applyDiffMapToMap, restrictDiffMap)
import LedgerOnDisk.V2.OnDiskMappings
import Data.Map (Map)
import Data.Monoid
import Data.Semigroup(Min(..))
import qualified Data.Map.Strict as Map
import Data.Coerce
import Control.Lens
import Data.Map.Monoidal (MonoidalMap(..))
import LedgerOnDisk.V2.Query
import Test.QuickCheck
import Data.Set (Set)

type FtId = Int
data DbEvent (t :: MapFlavour) k v where
  DbEventDiff :: Min Int -> Last Int -> DiffMap t k v -> DbEvent t k v
  DbEventSnapshot :: FtId -> DbEvent t k v
  deriving stock (Eq, Show)

-- newtype DbEvents (state :: StateKind MapFlavour) = DbEvents { getDbEvents :: OnDiskMappings state (DbEvent state) }

data SnapshotData (t :: MapFlavour) k v = SnapshotData
  deriving stock (Eq, Show)

instance Semigroup (SnapshotData t k v) where
  _ <> _ = SnapshotData

instance Monoid (SnapshotData t k v) where
  mempty = SnapshotData

newtype Snapshot t k v where
  Snapshot :: Map FtId (SnapshotData t k v) -> Snapshot t k v
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via MonoidalMap FtId (SnapshotData t k v)



emptySnapshotMappings :: HasOnDiskMappings state => OnDiskMappings state Snapshot
emptySnapshotMappings = pureOnDiskMappings $ Snapshot Map.empty

data DbSummary t k v = DbSummary
 { dbsDiffs :: !(Dual (DiffMap t k v))
 , dbsSnapshots :: !(Snapshot t k v)
 , dbsQueryId :: !(Maybe (Min Int))
 , dbsSeqId :: !(Last Int)
 , dbsNumQueries :: !(Sum Int)
 } deriving stock (Eq, Show)

instance Ord k => Semigroup (DbSummary t k v) where
  x <> y = DbSummary
    { dbsDiffs = dbsDiffs x <> dbsDiffs y
    , dbsSnapshots = dbsSnapshots x <> dbsSnapshots y
    , dbsNumQueries = dbsNumQueries x <> dbsNumQueries y
    , dbsSeqId = dbsSeqId x <> dbsSeqId y
    , dbsQueryId = dbsQueryId x <> dbsQueryId y
    }

instance Ord k => Monoid (DbSummary t k v) where
  mempty = DbSummary
    { dbsDiffs = mempty
    , dbsSnapshots = mempty
    , dbsNumQueries = mempty
    , dbsSeqId = mempty
    , dbsQueryId = mempty
    }

instance Ord k => FT.Measured (DbSummary t k v) (DbEvent t k v ) where
  measure = \case
    DbEventDiff qid dbsSeqId dm -> mempty
      { dbsDiffs = coerce dm
      , dbsNumQueries = pure 1
      , dbsSeqId
      , dbsQueryId = Just qid
      }
    DbEventSnapshot i -> mempty
      { dbsSnapshots = Snapshot $ Map.singleton i SnapshotData
      , dbsSeqId = pure i
      }

type DbHistory t k v = FingerTree (DbSummary t k v) (DbEvent t k v)
-- instance (AllMap (SemigroupMap DiffMap) state, HasOnDiskMappings state) => Semigroup (DbSummary state) where
--   x <> y = DbSummary
--     { pureLedgerState = getLast $ (coerce . pureLedgerState $ x) <> (coerce . pureLedgerState $ y)
--     , diffs = diffs x <> diffs y
--     , snapshots = let
--         go (Snapshot u) (Snapshot v) = Snapshot $ Map.union u v
--         in zipOnDiskMappings go (snapshots x) (snapshots y)
--     , size = size x + size y
--     }

-- instance (AllMap (SemigroupMap DiffMap) state, AllMap (MonoidMap DiffMap) state, HasOnDiskMappings state, Semigroup (DbSummary state)) => Monoid (DbSummary state) where
--   mempty = DbSummary
--     { pureLedgerState = Nothing
--     -- TODO we need "AllMap (SemigroupMap DiffMap) state" for this to work, don't understand why, this should defo work without
--     , diffs = mempty

--     , snapshots = emptySnapshotMappings
--     , size = 0
--     }

-- instance (Monoid (DbSummary state), HasOnDiskMappings state) => FT.Measured (DbSummary (state :: StateKind MapFlavour)) (DbEvents state) where
--   measure = foldMapOnDiskMappings go . getDbEvents
--     where
--       go :: forall (t :: MapFlavour) k v.
--         (HasOnDiskMappings state, Monoid (DbSummary state))
--         => DbEvent state t k v -> DbSummary state
--       go = \case
--         DbEventDiff ls -> DbSummary
--                 { pureLedgerState = Just $ ls & onDiskMappingsLens .~ nullMappings
--                 , diffs = ls ^. onDiskMappingsLens
--                 , snapshots = emptySnapshotMappings
--                 , size = 1
--                 }
--         DbEventSnapshot _ -> mempty

-- type DbHistory (state :: StateKind MapFlavour) = FingerTree (DbSummary state) (DbEvents state)

reconcileQueryResult :: Ord k => DbHistory t k v -> Int -> Query t k v -> QueryResult t k v -> QueryResult t k v
reconcileQueryResult sdtHistory sq q QueryResult{qrPointQueries, qrRangeQueries, qrCountRows} = QueryResult
  { qrPointQueries = applyDiffMapToMap correcting_diff qrPointQueries
  , qrRangeQueries
  , qrCountRows
  }
  where
    correcting_diff = restrictDiffMap (qPointQueries q) . coerce . dbsDiffs . FT.measure . FT.dropUntil p $ sdtHistory
      where
        p DbSummary{dbsSeqId} = maybe False (> sq) (getLast dbsSeqId)

pushDbEvent :: Ord k => DbEvent t k v -> DbHistory t k v -> DbHistory t k v
pushDbEvent = flip (FT.|>)

updateHistory :: Ord k => Int -> Int -> DiffMap t k v -> DbHistory t k v -> DbHistory t k v
updateHistory rsQueryId sqid dm = pushDbEvent (DbEventDiff (pure rsQueryId) (pure sqid) dm)

prop_dbhistory_applies_sensibly :: Int -> Int -> DiffMap 'MK_RW Int Int -> QueryTestCase Int Int -> Property
prop_dbhistory_applies_sensibly qid sq dm qtc = let
  (the_map, the_query) = runQueryTestCase (qtc { qtcCountRows = False, qtcRangeQueries = []})
  changed_map = applyDiffMapToMap dm the_map
  query_changed_map = queryMap the_query changed_map
  query_orig_and_reconcile = reconcileQueryResult (updateHistory qid (sq + 1)  dm mempty) sq the_query (queryMap the_query the_map)
  in counterexample ("changed map: " <> show changed_map) $
      -- counterexample ("changed map: " <> show query_changed_map) $
  query_changed_map === query_orig_and_reconcile

{-
prop> prop_dbhistory_applies_sensibly
+++ OK, passed 100 tests.
-}
