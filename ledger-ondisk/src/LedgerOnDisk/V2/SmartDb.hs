{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
-- |

module LedgerOnDisk.V2.SmartDb where


-- import Data.Map (Map)
-- import Data.IORef
import qualified Data.FingerTree as FT
import LedgerOnDisk.V2.Fingertree
import Control.Concurrent.STM
import LedgerOnDisk.V2.Db
import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Diff
import LedgerOnDisk.V2.Query
import LedgerOnDisk.V2.DumbDb
import Control.Concurrent.Async
import Data.Proxy
import Data.SOP (All)
import Control.Exception
import Data.Functor
import Data.Monoid hiding (All)
import Data.Maybe
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup (getMin, Min (Min))
import Data.Coerce
import Test.QuickCheck
import Data.Map (Map)
import qualified Test.QuickCheck.Monadic as QC
import Data.IORef
import LedgerOnDisk.V2.Snapshot
import Data.Traversable
import Control.Monad.State.Strict

data SmartDbTable t k v = SmartDbTable
  { sdtBackingStore :: BackingStore t k v
  , sdtHistory :: DbHistory t k v
  , sdtLastSeqId :: !(Maybe Int)
  -- , sdtSeqId :: Int
  }
type OdmSmartDbTable state = OnDiskMappings state SmartDbTable

newtype SmartDbTableTMVar t k v = SmartDbTableTMVar (TMVar (SmartDbTable t k v))
type OdmSmartDbTableTMVar state = OnDiskMappings state SmartDbTableTMVar

newtype FlushPolicy t k v = FlushPolicy (DbHistory t k v -> DbTableSummary t k v -> Bool)
type OdmFlushPolicy state = OnDiskMappings state FlushPolicy

newtype SmartDbDiff (state  :: StateKind MapFlavour) = SmartDbDiff (Endo [Either (OdmDiffMap state) (SnapshotInstructions state)])
  deriving newtype (Semigroup, Monoid)

data ReadSet t k v = ReadSet
  { rsAsync :: !(Async (ProductMap QueryResult (ConstMap Int) t k v))
  , rsQuery :: !(Query t k v)
  }
type OdmReadSet state = OnDiskMappings state ReadSet

newtype ResolvedReadSet (t :: MapFlavour) k v = ResolvedReadSet (ProductMap (ProductMap QueryResult (ConstMap Int)) Query t k v)
type OdmResolvedReadSet state = OnDiskMappings state ResolvedReadSet

data SmartDb state = SmartDb
  { sdTables :: !(OdmSmartDbTableTMVar state)
  , sdNextQueryIdTV :: !(TVar Int)
  , sdActiveQueriesTV :: !(TVar (Set Int))
  , sdNextSeqIdTV :: !(TVar Int)
  , sdMaxPrepares :: !Int
  -- , handle :: DumbDb state
  }

data SmartReadSet state = SmartReadSet
  { dbHandle :: !(SmartDb state)
  , odmReadSet :: !(OdmReadSet state)
  , rsQueryId :: !Int
  }


{- queryId invariant
we track active queries

When prepare returns a SmartReadSet with rsQueryId == x

we guarantee that no query older than (x - sdMaxPrepares) is outstanding

-}

prepare :: forall (state :: StateKind MapFlavour). (AllMap KeysOrd state)
  => SmartDb state -> OdmQuery state -> IO (SmartReadSet state)
prepare dbHandle@SmartDb{sdTables, sdNextQueryIdTV, sdActiveQueriesTV, sdMaxPrepares} odm_q = bracketOnError init' finalize $ \rsQueryId -> do
  odmReadSet <- zipAcOnDiskMappings (Proxy :: Proxy KeysOrd) readTable sdTables odm_q
  pure SmartReadSet {..}
  where
    init' = atomically $ do
      myId <- stateTVar sdNextQueryIdTV $ \x -> (x, x +1)
      active_qs <- stateTVar sdActiveQueriesTV $ \s -> let s' = Set.insert myId s in (s', s')
      guard $ maybe True (\x -> x + sdMaxPrepares > myId) $ Set.lookupMin active_qs
      pure myId
    finalize i = atomically $ modifyTVar' sdActiveQueriesTV $ Set.delete i


submit :: forall (state :: StateKind MapFlavour) a. (AllMap KeysOrd state) => SmartReadSet state -> (OdmQueryResult state -> (a, SmartDbDiff state)) -> IO a
submit SmartReadSet{odmReadSet, rsQueryId, dbHandle = SmartDb{sdTables, sdNextSeqIdTV, sdActiveQueriesTV, sdMaxPrepares}} op = do
  odm_rs <- waitSmartReadSet odmReadSet
  sqid <- atomically $ stateTVar sdNextSeqIdTV $ \x -> (x, x+1)
  flip withSdbTables sdTables $ \odm_sdt -> do
    x <- performOperation rsQueryId sqid (queriesActiveFlushPolicy rsQueryId sdMaxPrepares) odm_rs op odm_sdt
    atomically $ modifyTVar' sdActiveQueriesTV $ Set.delete rsQueryId
    pure x


-- IMPLEMENTATION


waitSmartReadSet :: HasOnDiskMappings  state => OdmReadSet state -> IO (OdmResolvedReadSet state)
waitSmartReadSet odmReadSet =let
    -- go :: forall (t :: MapFlavour) k v. ReadSet t k v -> IO (ProductMap (ProductMap QueryResult (ConstMap Int)) (ConstMap (Query t k v)) t k v)
    go ReadSet{rsAsync, rsQuery} = do
      r <- wait rsAsync
      pure . coerce $ ProductMap r rsQuery
    in traverseOnDiskMappings go odmReadSet

performOperation :: AllMap KeysOrd state =>
   Int -> Int
  -> OdmFlushPolicy state
  -> OdmResolvedReadSet state
  -> (OdmQueryResult state -> (a, SmartDbDiff state))
  -> OdmSmartDbTable state
  -> IO (a, OnDiskMappings state (ComposeMap IO SmartDbTable))
performOperation qid sqid odm_flush_policy odm_rs op odm_sdt0 = do
    let
      reconciled_qr = let
        go SmartDbTable{sdtHistory} (ResolvedReadSet (ProductMap (ProductMap qr (ConstMap q_sq)) q)) = reconcileQueryResult sdtHistory q_sq q qr
        in zipcOnDiskMappings (Proxy  :: Proxy KeysOrd) go odm_sdt0 odm_rs
      (a, sdd) = op reconciled_qr

    -- TODO we should coalese the elements (i.e. into strings of Lefts and Rights)
    -- but this requires coalescing snapshots, a bit complicated
    new_odm_sdt <- flip execStateT odm_sdt0 $ for (appEndo (coerce sdd) []) $ \diff_or_snapshot -> do
        odm_sdt <- get
        new_odm_sdt <- lift $ case diff_or_snapshot of
          Left odm_dm -> let
            go :: forall (t :: MapFlavour) k v. Ord k => FlushPolicy t k v -> DiffMap t k v -> SmartDbTable t k v -> IO (ComposeMap IO SmartDbTable t k v)
            go fp dm sdt = applyDiffsToSmartDbTable fp sqid (DbEventDiff (pure qid) (pure sqid) dm) sdt
            in zip3AcOnDiskMappings (Proxy :: Proxy KeysOrd)
              go odm_flush_policy odm_dm odm_sdt >>= sequenceOnDiskMappings

          Right si -> do
            let
              (odmProductMapToTuple -> (odm_old_sq, odm_new_sq), odm_bs) = odmProductMapToTuple $ mapOnDiskMappings go odm_sdt where
                go :: forall (t :: MapFlavour) k v. SmartDbTable t k v -> ProductMap (ProductMap (ConstMap Int) (ConstMap Int)) BackingStore t k v
                go SmartDbTable{sdtBackingStore,sdtLastSeqId} =
                  ProductMap (ProductMap (ConstMap (fromMaybe 0 sdtLastSeqId)) (ConstMap sqid)) sdtBackingStore
            applySwizzle odm_old_sq odm_new_sq si odm_bs
            pure $ mapOnDiskMappings (\sdt -> sdt
                                      { -- sdtHistory = ? we need to add information that the swizzle happened
                                        sdtLastSeqId = Just sqid
                                      }) odm_sdt
        put new_odm_sdt
    pure (a, liftOnDiskMappingsToComposeMap new_odm_sdt)



-- The operation take the old SeqId and the value in odm_m1, it should return a new SeqId as well as
-- an operation to commit the changes. The commit operation should be suitable
-- for calling inside a "mask", i.e. short running and ideally not interruptible
-- holdingAllSeqIdLocks :: HasOnDiskMappings state
--   => OdmBackingStore state
--   -> (forall t k v. Int -> map1 t k v -> IO ( IO (map2 t k v), Int))
--   -> OnDiskMappings state map1 -> IO (OnDiskMappings state map2)
-- holdingAllSeqIdLocks odm_bs op odm_m1 = bracketOnError init' finalize inner
--   where
--     init' = atomically $ traverseOnDiskMappings go odm_bs
--       where
--         go BackingStore {..} = ConstMap <$> takeTMVar bsId


newSmartDbTable :: Ord k => BackingStore t k v -> SmartDbTable t k v
newSmartDbTable sdtBackingStore = SmartDbTable{sdtBackingStore, sdtHistory = mempty, sdtLastSeqId = Nothing}

withSdbTables :: HasOnDiskMappings state
  => (OnDiskMappings state SmartDbTable -> IO (a, OnDiskMappings state (ComposeMap IO SmartDbTable)))
  -> OdmSmartDbTableTMVar state -> IO a
withSdbTables op sdTables = bracketOnError take_sd_tables write_sd_tables $ \odm_sdt -> mask $ \restore -> do
  (r, new_sdt) <- restore $ op odm_sdt
  commited <- sequenceOnDiskMappings new_sdt
  void $ write_sd_tables commited
  pure r
  where
    take_sd_tables = atomically $ traverseOnDiskMappings (\(SmartDbTableTMVar tmv) -> takeTMVar tmv) sdTables
    write_sd_tables = atomically . zipAOnDiskMappings (\(SmartDbTableTMVar tmv) sdt -> putTMVar tmv sdt $> NullMap) sdTables


readTable :: Ord k => SmartDbTableTMVar t k v -> Query t k v -> IO (ReadSet t k v)
readTable (SmartDbTableTMVar tmv) rsQuery = do
  SmartDbTable{sdtBackingStore} <- atomically $ readTMVar tmv
  rsAsync <- async $ do
    ((qr, _), sq) <- readBackingStore sdtBackingStore $ queryBackingStore rsQuery
    pure $ ProductMap qr (ConstMap sq)
  pure $ ReadSet {rsAsync, rsQuery}


applyDiffsToSmartDbTable :: forall (t :: MapFlavour) k v. Ord k => FlushPolicy t k v -> Int -> DbEvent t k v -> SmartDbTable t k v -> IO (ComposeMap IO SmartDbTable t k v)
applyDiffsToSmartDbTable (FlushPolicy fp) sqid dbe sdt@SmartDbTable{sdtHistory, sdtLastSeqId, sdtBackingStore} = do
  let
    new_hist = sdtHistory FT.|> dbe
    (to_flush, to_store) = FT.split (fp new_hist) new_hist
  writeBackingStore_ sdtLastSeqId sqid sdtBackingStore $
    pure . Just . applyDiffMapToBStore (coerce . dbsDiffs . FT.measure $ to_flush)
  pure $ ComposeMap $ pure $ sdt { sdtHistory = to_store, sdtLastSeqId = Just sqid }

queriesActiveFlushPolicy :: HasOnDiskMappings state => Int -> Int -> OdmFlushPolicy state
queriesActiveFlushPolicy known_query_id sdMaxPrepares = pureOnDiskMappings $ let
  go _ = check_policy where
    check_policy DbTableSummary{dbsQueryId} = maybe False (\x -> getMin x + sdMaxPrepares > known_query_id) dbsQueryId
  in FlushPolicy go

alwaysFlushPolicy :: FlushPolicy t k v
alwaysFlushPolicy = FlushPolicy $ \_ _ -> False

neverFlushPolicy :: FlushPolicy t k v
neverFlushPolicy = FlushPolicy $ \_ _ -> True

smartDbHarness :: Map Int Int -> [(FlushPolicy 'MK_RW Int Int, DbEvent 'MK_RW Int Int)] -> Property
smartDbHarness the_map diffs = QC.monadicIO $ do
  bs <- QC.run $ newBackingStoreRW the_map
  let
    sdt0 = newSmartDbTable bs
    go_diff (all_diffs, sdt@SmartDbTable{}) (i, (flush_policy, dbevent))  = do
      (new_sdt, backing_store_map) <- QC.run $ do
        ComposeMap m_new_sdt <- applyDiffsToSmartDbTable flush_policy i dbevent sdt
        bsm <- readIORef (bsRef bs) <&> \case
          RWStore m -> m
        (,bsm) <$> m_new_sdt
      let new_all_diffs = case dbevent of
            DbEventDiff _ _ d -> d <> all_diffs
      QC.assert $ applyDiffMapToMap new_all_diffs the_map == applyDiffMapToMap (coerce . dbsDiffs . FT.measure $ sdtHistory new_sdt) backing_store_map
      pure (new_all_diffs, new_sdt)

  foldM_ go_diff (mempty, sdt0) (zip [1..] diffs)


prop_always_flush_works :: Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_always_flush_works m diffs = smartDbHarness m
  [ (alwaysFlushPolicy, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]

prop_never_flush_works :: Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_never_flush_works m diffs = smartDbHarness m
  [ (neverFlushPolicy, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]

prop_const_flush_works :: Positive Int -> Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_const_flush_works (Positive limit) m diffs = smartDbHarness m
  [ (FlushPolicy should_keep, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]
  where
  should_keep dbh = let
    Sum total_entries = dbsNumQueries . FT.measure $ dbh
    in \DbTableSummary{dbsNumQueries} -> total_entries - coerce dbsNumQueries < limit
{-

prop> prop_always_flush_works
+++ OK, passed 100 tests.

prop> prop_never_flush_works
+++ OK, passed 100 tests.

prop> prop_const_flush_works
+++ OK, passed 100 tests.

-}
