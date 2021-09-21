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

data SmartDbTable t k v = SmartDbTable
  { sdtBackingStore :: BackingStore t k v
  , sdtHistory :: DbHistory t k v
  , sdtLastSeqId :: !(Maybe Int)
  -- , sdtSeqId :: Int
  }

newtype SmartDbTableTMVar t k v = SmartDbTableTMVar (TMVar (SmartDbTable t k v))

type OdmSmartDbTableTMVar state = OnDiskMappings state SmartDbTableTMVar
data SmartDb state = SmartDb
  { sdTables :: !(OdmSmartDbTableTMVar state)
  , sdNextQueryIdTV :: !(TVar Int)
  , sdActiveQueriesTV :: !(TVar (Set Int))
  , sdNextSeqIdTV :: !(TVar Int)
  , sdMaxPrepares :: !Int
  -- , handle :: DumbDb state
  }

data ReadSet t k v = ReadSet
  { rsAsync :: !(Async (ProductMap QueryResult (ConstMap Int) t k v))
  , rsQuery :: !(Query t k v)
  }

type OdmReadSet state = OnDiskMappings state ReadSet

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



-- type OdmSDbSeqId state = OnDiskMappings state (ConstMap (SeqId (DumbDb state))

-- TODO I get a "-Wsimplifiable-class-constraints warning"  when using "AllMAp KeysOrd state" here
--- PUBLIC API
prepare :: forall (state :: StateKind MapFlavour). (HasOnDiskMappings state, All (CurriedConstraint KeysOrd) (OnDiskMappingsTypes state))
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


submit :: forall (state :: StateKind MapFlavour) a. (AllMap KeysOrd state) => SmartReadSet state -> (OdmQueryResult state -> (a, OdmDiffMap state)) -> IO a
submit SmartReadSet{odmReadSet, rsQueryId, dbHandle = SmartDb{sdTables, sdNextSeqIdTV, sdActiveQueriesTV, sdMaxPrepares}} op = do
  odm_rs <- waitSmartReadSet odmReadSet
  sqid <- atomically $ stateTVar sdNextSeqIdTV $ \x -> (x, x+1)
  flip withSdbTables sdTables $ \odm_sdt -> do
    let
      reconciled_qr = let
        go SmartDbTable{sdtHistory} (ProductMap (ProductMap qr (ConstMap q_sq)) q) = reconcileQueryResult sdtHistory q_sq q qr
        in zipcOnDiskMappings (Proxy  :: Proxy KeysOrd) go odm_sdt odm_rs
      (a, odm_dm) = op reconciled_qr
    flush <- let
      go :: forall (t :: MapFlavour) k v. Ord k => DiffMap t k v -> SmartDbTable t k v -> IO (ComposeMap IO SmartDbTable t k v)
      go dm sdt = applyDiffsToSmartDbTable (queriesActiveFlushPolicy rsQueryId sdMaxPrepares) sqid (DbEventDiff (pure rsQueryId) (pure sqid) dm) sdt
      in zipAcOnDiskMappings (Proxy :: Proxy KeysOrd) go odm_dm odm_sdt
    atomically $ modifyTVar' sdActiveQueriesTV $ Set.delete rsQueryId
    pure (a, flush)








    


  -- let
  --   go old_sq (ProductMap (DbHistoryTMVar dbh_tmv) (ProductMap qr (ConstMap rs_sq))) =
  --     bracketOnError (atomically $ takeTMVar dbh_tmv) (atomically . putTMVar dbh_tmv) $ \dbh ->
  --       reconcileQueryResult dbh rs_sq qr

  --     reconcileQueryResult
  -- odm_maps <- holdingAllSeqIdLocks (odmBackingStoreFromOdmReadSet srs) go odm_p



-- IMPLEMENTATION

waitSmartReadSet :: HasOnDiskMappings  state => OdmReadSet state -> IO (OnDiskMappings state (ProductMap (ProductMap QueryResult (ConstMap Int)) Query))
waitSmartReadSet odmReadSet =let
    -- go :: forall (t :: MapFlavour) k v. ReadSet t k v -> IO (ProductMap (ProductMap QueryResult (ConstMap Int)) (ConstMap (Query t k v)) t k v)
    go ReadSet{rsAsync, rsQuery} = do
      r <- wait rsAsync
      pure $ ProductMap r rsQuery
    in traverseOnDiskMappings go odmReadSet

-- odmBackingStoreFromOdmReadSet :: SmartReadSet state -> OdmBackingStore state
-- odmBackingStoreFromOdmReadSet SmartReadSet { dbHandle = SmartDb { handle = DumbDb { backingStore } } } = backingStore

-- data VersionedMap map t k v = VersionedMap !Int !(map t k v)

-- type OdmVersionedMap state map = OnDiskMappings state (VersionedMap map)


-- | The operation take the old SeqId and the value in odm_m1, it should return a new SeqId as well as
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

--     finalize odm_old_sq = atomically $ zipAOnDiskMappings go odm_bs odm_old_sq
--       where
--         go BackingStore {..} (ConstMap old_sq) = putTMVar bsId old_sq $> NullMap
--     inner odm_old_sq = mask $ \restore -> do
--       let
--         go1 (ConstMap old_sq) m1 = op old_sq m1 <&> \(m2, new_sq) -> ProductMap (ComposeMap m2) (ConstMap new_sq)
--       odm_r <- restore $ zipAOnDiskMappings go1 odm_old_sq odm_m1

--       let
--         go2 BackingStore {bsId} (ProductMap (ComposeMap m2) (ConstMap new_sq)) = _

--       zipAOnDiskMappings go2 odm_bs odm_r

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
  rsAsync <- async $ readBackingStore sdtBackingStore rsQuery <&> \(qr, sq) -> ProductMap qr (ConstMap sq)
  pure $ ReadSet {rsAsync, rsQuery}

type FlushPolicy t k v = DbHistory t k v -> DbSummary t k v -> Bool

applyDiffsToSmartDbTable :: forall (t :: MapFlavour) k v. Ord k => FlushPolicy t k v -> Int -> DbEvent t k v -> SmartDbTable t k v -> IO (ComposeMap IO SmartDbTable t k v)
applyDiffsToSmartDbTable flush_policy sqid dbe sdt@SmartDbTable{sdtHistory, sdtLastSeqId, sdtBackingStore} = do
  let
    new_hist = sdtHistory FT.|> dbe
    (to_flush, to_store) = FT.split (flush_policy new_hist) $ new_hist
  writeBackingStore sdtLastSeqId sqid sdtBackingStore . coerce . dbsDiffs . FT.measure $ to_flush
  pure $ ComposeMap $ pure $ sdt { sdtHistory = to_store, sdtLastSeqId = Just sqid }


queriesActiveFlushPolicy :: Int -> Int -> FlushPolicy t k v
queriesActiveFlushPolicy known_query_id sdMaxPrepares _ = \DbSummary{dbsQueryId} -> maybe False (\x -> getMin x + sdMaxPrepares > known_query_id) dbsQueryId

smartDbHarness :: Map Int Int -> [(FlushPolicy 'MK_RW Int Int, DbEvent 'MK_RW Int Int)] -> Property
smartDbHarness the_map diffs = QC.monadicIO $ do
  bs <- QC.run $ newBackingStore the_map
  let
    sdt0 = newSmartDbTable bs
    go_diff (all_diffs, sdt@SmartDbTable{}) (i, (flush_policy, dbevent))  = do
      (new_sdt, backing_store_map) <- QC.run $ do
        ComposeMap m_new_sdt <- applyDiffsToSmartDbTable flush_policy i dbevent sdt
        bsm <- readIORef (bsRef bs)
        (,bsm) <$> m_new_sdt
      let new_all_diffs = case dbevent of
            DbEventDiff _ _ d -> d <> all_diffs
            DbEventSnapshot  _ -> all_diffs
      QC.assert $ applyDiffMapToMap new_all_diffs the_map == applyDiffMapToMap (coerce . dbsDiffs . FT.measure $ sdtHistory new_sdt) backing_store_map
      pure (new_all_diffs, new_sdt)

  foldM_ go_diff (mempty, sdt0) (zip [0..] diffs)

prop_always_flush_works :: Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_always_flush_works m diffs = smartDbHarness m
  [ (\_ _ -> False, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]

prop_never_flush_works :: Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_never_flush_works m diffs = smartDbHarness m
  [ (\_ _ -> True, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]

prop_const_flush_works :: Positive Int -> Map Int Int -> [DiffMap 'MK_RW Int Int] -> Property
prop_const_flush_works (Positive limit) m diffs = smartDbHarness m
  [ (should_keep, DbEventDiff (pure i) (pure i) d) | (i, d) <- zip [0..] diffs ]
  where
  should_keep dbh = let
    Sum total_entries = dbsNumQueries . FT.measure $ dbh
    in \DbSummary{dbsNumQueries} -> total_entries - coerce dbsNumQueries < limit
{-

prop> prop_always_flush_works
+++ OK, passed 100 tests.

prop> prop_never_flush_works
+++ OK, passed 100 tests.

prop> prop_const_flush_works
+++ OK, passed 100 tests.

-}
