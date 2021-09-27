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
{-# LANGUAGE RecordWildCards #-}

module LedgerOnDisk.V2.Fingertree

where

import qualified Data.FingerTree as FT
import Data.FingerTree (FingerTree)

import LedgerOnDisk.V2.Diff
import LedgerOnDisk.V2.Db
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
import Data.Time (TimeOfDay(todHour))
import LedgerOnDisk.V2.Snapshot

import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import qualified Data.STRef as ST

type FtId = Int
data DbEvent (t :: MapFlavour) k v where
  DbEventDiff :: Min Int -> Last Int -> DiffMap t k v -> DbEvent t k v
  -- DbEventSnapshot :: FtId -> DbEvent t k v
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

-- TODO
-- this should have different constructors for t
data DbTableDiff t k v where
  DbRWWrite :: Dual (DiffMap 'MK_RW k v) -> DbTableDiff 'MK_RW k v
  DbRWUWrite :: Dual (DiffMap 'MK_RWU k v) -> DbTableDiff 'MK_RWU k v
  DbSnapshotMove :: Dual (DiffMap 'MK_RO k v) -> DbTableDiff 'MK_RO k v

instance Semigroup (DbTableDiff t k v) where
  DbRWWrite x <> DbRWWrite y = DbRWWrite $ x <> y
  DbRWUWrite x <> DbRWUWrite y = DbRWUWrite $ x <> y
  DbSnapshotMove x <> DbSnapshotMove y = DbSnapshotMove  $ x <> y

data DbTableSummary t k v = DbTableSummary
 { dbsDiffs :: !(Dual (DiffMap t k v))
 -- , dbsSnapshots :: !(Snapshot t k v)
 , dbsQueryId :: !(Maybe (Min Int)) -- ^ We store the oldest query id. This lets us split a prefix consisting only of old queries
 , dbsSeqId :: !(Last Int) -- ^ We store the newest seq id
 , dbsNumQueries :: !(Sum Int)
 } deriving stock (Eq, Show)

instance Semigroup (DiffMap t k v) => Semigroup (DbTableSummary t k v) where
  x <> y = DbTableSummary
    { dbsDiffs = dbsDiffs x <> dbsDiffs y
    -- , dbsSnapshots = dbsSnapshots x <> dbsSnapshots y
    , dbsNumQueries = dbsNumQueries x <> dbsNumQueries y
    , dbsSeqId = dbsSeqId x <> dbsSeqId y
    , dbsQueryId = dbsQueryId x <> dbsQueryId y
    }

instance Monoid (DiffMap t k v) => Monoid (DbTableSummary t k v) where
  mempty = DbTableSummary
    { dbsDiffs = mempty
    -- , dbsSnapshots = mempty
    , dbsNumQueries = mempty
    , dbsSeqId = mempty
    , dbsQueryId = mempty
    }

instance Monoid (DbTableSummary t k v) => FT.Measured (DbTableSummary t k v) (DbEvent t k v ) where
  measure = \case
    DbEventDiff qid dbsSeqId dm -> mempty
      { dbsDiffs = coerce dm
      , dbsNumQueries = pure 1
      , dbsSeqId
      , dbsQueryId = Just qid
      }
    -- DbEventSnapshot i -> mempty
    --   { dbsSnapshots = Snapshot $ Map.singleton i SnapshotData
    --   , dbsSeqId = pure i
    --   }

type DbHistory t k v = FingerTree (DbTableSummary t k v) (DbEvent t k v)
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

-- 1. Extract the snapshot instructions for the suffix not including sq
-- 2. Apply the snapshot instructions to the query result
--
-- A point query

{-
ops:
 - adjustQuery :: DbHistory -> DbQuery -> DbQuery
 - reconcileQueryResult :: DbQuery -> DbHistory -> DbQueryResult -> Maybe DbQueryResult
 - pushDiff :: SeqId -> DbDiff -> DbHistory -> DbHistory

 Suppose Q_1 is a DbQuery, H_1 is a DbHistory, D_1 is a DbDiff, R_1 is a DbQueryResult, such that:

 reconcileQueryResult (adjustQuery H_1 Q_1)  (pushDiff sid D_1 H_1) = reconcileQueryResult (adjustQuery (pushDif sid H_1) Q_1) (pushDiff sid D_1 H_1)


-}

-- data ReconciliationError
--   = StaleQuery
--   | NonRORangeQuery
--   | NonROCountRows

reconcileQueryResult :: Monoid (DbTableSummary t k v)
  => MapTag t
  -> DbHistory t k v -- ^ The up-to-date history
  -> Int -- ^ The SeqId returned from readDb
  -> DbQuery t k v -- ^ The query issued to the Db
  -> DbQueryResult t k v
  -> Either ReconciliationError (DbQueryResult t k v)
reconcileQueryResult mt sdtHistory sq (DbQuery q) (DbQueryResult qr) = Just $ DbQueryResult QueryResult{..} where
  qrPointQueries = _
  qrRangeQueries = emptyRangeQueryResults
  qrCountRows = mempty
  -- QueryResult {qrPointQueries} -> pure $ QueryResult{ qrPointQueries = applyDiffMapToMap correcting_diff qrPointQueries } where
  --   correcting_diff = restrictDiffMap (pointQueries q) . getDual . dbsDiffs . FT.measure . FT.dropUntil p $ sdtHistory where
  --     p DbTableSummary{dbsSeqId} = maybe False (> sq) (getLast dbsSeqId)
  -- ROQueryResult {} -> _



  -- QueryResult{qrPointQueries, qrRangeQueries, qrCountRows} = QueryResult
  -- { qrPointQueries = applyDiffMapToMap correcting_diff qrPointQueries
  -- , qrRangeQueries
  -- , qrCountRows
  -- }
  -- where
  --   correcting_diff = restrictDiffMap (qPointQueries q) . coerce . dbsDiffs . FT.measure . FT.dropUntil p $ sdtHistory
  --     where
  --       p DbTableSummary{dbsSeqId} = maybe False (> sq) (getLast dbsSeqId)

pushDbEvent :: Monoid (DiffMap t k v) => DbEvent t k v -> DbHistory t k v -> DbHistory t k v
pushDbEvent = flip (FT.|>)

updateHistory :: Monoid (DiffMap t k v) => Int -> Int -> DiffMap t k v -> DbHistory t k v -> DbHistory t k v
updateHistory rsQueryId sqid dm = pushDbEvent (DbEventDiff (pure rsQueryId) (pure sqid) dm)

-- prop_dbhistory_applies_sensibly :: Int -> Int -> DiffMap 'MK_RW Int Int -> QueryTestCase Int Int -> Property
-- prop_dbhistory_applies_sensibly qid sq dm qtc = let
--   (the_map, the_query) = runQueryTestCase (qtc { qtcCountRows = False, qtcRangeQueries = []})
--   changed_map = applyDiffMapToMap dm the_map
--   query_changed_map = queryMap the_query changed_map
--   query_orig_and_reconcile = reconcileQueryResult (updateHistory qid (sq + 1)  dm mempty) sq the_query (queryMap the_query the_map)
--   in counterexample ("changed map: " <> show changed_map) $
--       -- counterexample ("changed map: " <> show query_changed_map) $
--   query_changed_map === query_orig_and_reconcile

{-
prop> prop_dbhistory_applies_sensibly
+++ OK, passed 100 tests.
-}

data QueryProvenance (t :: MapFlavour) k v where
  QPInit :: QueryProvenance t k v
  -- QPQ :: Query t k v -> QueryProvenance t k v
  QPSnapshoted :: Query k v -> QueryProvenance t k v

data STRefMap s (map :: TableKind MapFlavour) (t :: MapFlavour) k v where
  STRefMap :: ST.STRef s (map t' k v) -> STRefMap s map t k v

applySnapshotInstructionsInverse :: forall (state :: StateKind MapFlavour).
                                    HasOnDiskMappings state
                                 => SnapshotInstructions state
                                 -> OnDiskMappings state DbQuery
                                 -> OnDiskMappings state DbQuery
applySnapshotInstructionsInverse si s =
    ST.runST go
  where
    go :: forall s. ST s (OnDiskMappings state DbQuery)
    go = do
      -- let uninitialised :: forall t k v. QueryProvenance t k v.
      --     uninitialised = QPInit

      -- Set up a tuple of STRefs
      originalRefs <- pureAOnDiskMappings $ STRefMap <$> ST.newSTRef QPInit

      -- let originalRefs' :: OnDiskMappings state (STRefKeySet s)
      --     originalRefs' = originalRefs

      -- Now swizzle the STRefs
      let
          swizzledRefs :: OnDiskMappings state (STRefMap s QueryProvenance)
          swizzledRefs = zipOnDiskMappings selectRef (runSnapshotInstructions SnapshotOfTable (const KeepTable) si originalRefs) originalRefs

          selectRef :: forall (t :: MapFlavour) k v.
                     SnapshotOfTable (STRefMap s QueryProvenance) t k v
                    -> STRefMap s QueryProvenance t k v
                    -> STRefMap s QueryProvenance t k v
          selectRef KeepTable ref = ref
          selectRef (SnapshotOfTable (STRefMap ref)) _ = STRefMap ref


      -- Write into the swizzled STRefs
      -- _ < letzipAOnDiskMappings
      _ <- let
        go :: forall t k v. STRefMap s QueryProvenance t k v -> DbQuery t k v -> ST.ST s ()
        go (STRefMap ref) (DbQuery q) = ST.writeSTRef ref (QPSnapshoted q)
        in zipAOnDiskMappings_ go swizzledRefs s

      _
      -- Read from the original STRefs
      -- traverseMappings (\t r -> readSTRefKeySet r)
