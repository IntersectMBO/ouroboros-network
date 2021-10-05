{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module LedgerOnDisk.V2.Query where

-- import LedgerOnDisk.KVHandle.OnDiskMappings
import Data.Map (Map)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Coerce
import Control.Applicative
-- import Test.QuickCheck
import Data.Functor.Identity
import Data.IntervalMap.FingerTree as IntervalMap
import qualified Data.Set as Set
import Data.Set (Set)
import Test.QuickCheck
import Data.Functor
import Data.List (unfoldr)
import Data.Maybe
import GHC.Generics ( Generic )

pointQuery :: Ord k => k -> Query k v
pointQuery k = mempty { qPointQueries =  Set.singleton k }

rowCountQuery :: Ord k => Query k v
rowCountQuery = mempty { qCountRows = Any True }

rangeQuery :: Ord k => Maybe k -> Int64 -> Int64 -> Query k v
rangeQuery mb_k offset limit = mempty { qRangeQueries = RangeQuery $ Map.singleton mb_k $ IntervalMap.singleton (Interval offset (offset + limit)) () }

{-

example:

data LedgerState map =
  LS
  { utxo :: map TxIn TxOut
  , utxoAgg :: map Addr Coun
  }

then

queryLedgerState :: LedgerState Query -> IO (LedgerState QueryResult)

-}

-- | a map from
-- keys to the number of rows requested. Nothing means to begin at the smallest existing key.
-- The key requested is not included in the results
newtype RangeQuery k = RangeQuery (Map (Maybe k) (IntervalMap Int64 ()))
  deriving stock (Eq, Show)

instance Ord k => Semigroup (RangeQuery k) where
  RangeQuery x <> RangeQuery y = RangeQuery $ Map.unionWith (<>) x y

instance Ord k => Monoid (RangeQuery k) where
  mempty = RangeQuery Map.empty

data Query k v = Query
  { qPointQueries :: !(Set k) -- ^ This is a 'Set k' of keys to retrieve
  , qCountRows :: !Any -- ^ Request an estimate of the number of rows. Returns an upper bound
  , qRangeQueries :: !(RangeQuery k)
    -- ^ a map from
    -- keys to the number of rows requested. Nothing means to begin at the smallest existing key.
    -- The key requested is not included in the results
  }
  deriving stock (Eq, Show)

newtype ArbitraryRangeQuery k = ArbitraryRangeQuery [(Maybe k, [(NonNegative Int64, NonNegative Int64)])]
  deriving stock (Eq, Show, Generic)

instance (Ord k, Arbitrary k) => Arbitrary (ArbitraryRangeQuery k) where
  arbitrary = ArbitraryRangeQuery  <$> arbitrary
  shrink = genericShrink

intervalMapToList :: Ord v => IntervalMap v a -> [Interval v]
intervalMapToList = unfoldr (\x -> IntervalMap.leastView x <&> \((i,_), x') -> (i, x'))

toArbitraryRangeQuery :: RangeQuery k -> ArbitraryRangeQuery k
toArbitraryRangeQuery (RangeQuery m ) = ArbitraryRangeQuery $
  [ (mb_k, [(NonNegative lb, NonNegative $ ub - lb + 1) | Interval lb ub <- ls])
  | (mb_k, ls) <- Map.toList . fmap intervalMapToList $  m
  ]

fromArbitraryRangeQuery :: Ord k => ArbitraryRangeQuery k -> RangeQuery k
fromArbitraryRangeQuery (ArbitraryRangeQuery xs) = RangeQuery $ Map.fromList
  [ (mb_k, foldr (\(NonNegative offset, NonNegative limit) -> IntervalMap.insert (Interval offset (offset + limit - 1)) ()) IntervalMap.empty ys)
  | (mb_k, ys) <- xs
  ]

instance (Ord k, Arbitrary k) => Arbitrary (RangeQuery k) where
  arbitrary = fromArbitraryRangeQuery <$> arbitrary
  shrink = fmap fromArbitraryRangeQuery . shrink . toArbitraryRangeQuery

instance Ord k => Semigroup (Query k v) where
  x <> y = Query
    { qPointQueries = qPointQueries x  <> qPointQueries y
    , qCountRows = qCountRows x  <> qCountRows y
    , qRangeQueries = qRangeQueries x  <> qRangeQueries y
    }

instance Ord k => Monoid (Query k v) where
  mempty = Query
    { qPointQueries = mempty
    , qCountRows = mempty
    , qRangeQueries = mempty
    }

newtype RangeQueryResults k v where
  RangeQueryResults :: { getRangeQueryResults :: Map (Maybe k) (Map Int64 [(k, v)]) } -> RangeQueryResults k v
  deriving (Eq, Show)

emptyRangeQueryResults :: Ord k => RangeQueryResults k v
emptyRangeQueryResults = RangeQueryResults Map.empty

data QueryResult k v where
  QueryResult ::
    { qrPointQueries :: !(Map k v)
    , qrRangeQueries :: !(RangeQueryResults k v)
    , qrCountRows :: !(Maybe Int64)
    }  -> QueryResult k v
  deriving stock (Eq, Show)

-- newtype RowCountResult k v = RowCountResult (Maybe Int64)
--   deriving stock (Eq, Show)

-- type instance SemigroupMap Query = KeysAreOrd



-- runRangeQuery :: forall m k v. (Applicative m, Ord k)
--   => (Maybe k -> Int64 -> Int64 -> m [(k, v)])
--   -> Maybe k
--   -> Int64
--   -> Int64
--   -> m (Endo (RangeQueryResults k v))
-- runRangeQuery performRangeQuery mb_k offset limit = go <$> performRangeQuery mb_k offset limit

  -- Map.union should never have to deal with matching keys, runQuery makes sure of that
  -- where go kvs = coerce . Map.insertWith Map.union mb_k $ Map.singleton offset kvs

runRangeQuery :: forall m k v. (Ord k, Applicative  m) => (Maybe k -> Int64 -> Int64 -> m [(k, v)]) -> RangeQuery k -> m (Endo (RangeQueryResults k v))
runRangeQuery perform_range_query (RangeQuery range_queries) = getAp $ Map.foldMapWithKey go_rq range_queries where
  go_rq :: Maybe k -> IntervalMap Int64 () -> Ap m (Endo (RangeQueryResults k v))
  go_rq mb_k qs_im = (reconcile_results_endo <>) <$> nonoverlapping_results where
    interval_map_to_list im = unfoldr (\x -> IntervalMap.leastView x <&> \((i,_), x') -> (i, x')) im

    reconcile_results_endo = coerce $ Map.adjust reconcile_results mb_k
    reconcile_results :: Map Int64 [(k, v)] -> Map Int64 [(k, v)]
    reconcile_results nonoverlapping_r = foldr go Map.empty $ interval_map_to_list qs_im  where
      go (Interval lb ub) acc = case Map.lookupLE lb nonoverlapping_r of
        Nothing -> Map.insert lb [] acc
        Just (k, v) -> Map.insert lb (take (fromIntegral $ ub - lb + 1) . drop (fromIntegral $ lb - k) $ v) acc
    nonoverlapping_results = foldMap go (non_overlapping_intervals qs_im) where
      go (Interval lb ub) = Ap $ go' <$> perform_range_query mb_k lb ub where
        go' kvs = coerce $ Map.insertWith Map.union mb_k $ Map.singleton lb kvs

    non_overlapping_intervals im = case IntervalMap.leastView im of
      Nothing -> []
      Just ((i1,()), rest) -> non_overlapping_intervals' [] i1 rest where
        non_overlapping_intervals' acc i@(IntervalMap.Interval lb1 ub1) rest' = case IntervalMap.leastView rest' of
          Nothing -> i : acc
          Just ((i2@(IntervalMap.Interval lb2 ub2),()), next)
            | lb2 <= ub1 -> non_overlapping_intervals' acc (IntervalMap.Interval lb1 (ub1 `max` ub2)) next
            | otherwise -> non_overlapping_intervals' (i : acc) i2 next

runQuery :: forall m k v. (Applicative m, Ord k)
  => (k -> m (Maybe v))
  -> (Maybe k -> Int64 -> Int64 -> m [(k, v)])
  -> m Int64
  -> Query k v
  -> m (QueryResult k v)
runQuery perform_lookup perform_range_query perform_count_rows Query{..} =
  getAp $ liftA3 combineResults doCountRows doPointQueries doRangeQueries where
    combineResults :: Maybe Int64 -> Endo (Map k v) -> Endo (RangeQueryResults k v) -> QueryResult k v
    combineResults qrCountRows pqs rqs = let
      qrPointQueries = appEndo pqs Map.empty
      qrRangeQueries = appEndo rqs emptyRangeQueryResults
      in QueryResult{..}

    doCountRows
      | getAny qCountRows = coerce $ Just <$> perform_count_rows
      | otherwise = pure Nothing

    doPointQueries  = foldMap go qPointQueries where
      go :: k -> Ap m (Endo (Map k v))
      go k = Ap $ coerce . maybe id (Map.insert k) <$> perform_lookup k

    doRangeQueries :: Ap m (Endo (RangeQueryResults k v))
    doRangeQueries = coerce $ runRangeQuery perform_range_query qRangeQueries

-- runQuery :: forall t m k v. (Applicative m, Ord k)
--   => (k -> m (Maybe v))
--   -> (Maybe k -> Int64 -> Int64 -> m [(k, v)])
--   -> m Int64
--   -> Query k v
--   -> m (QueryResult t k v)
-- runQuery perform_lookup perform_range_query perform_rowcount = \case
--   NoQuery -> pure NoQueryResult
--   RWQuery pq -> RWQueryResult <$> do_point_queries pq
--   RWUQuery pq -> RWUQueryResult <$> do_point_queries pq
--   roq@ROQuery {} -> runROQuery perform_lookup perform_range_query perform_rowcount roq
--   where
--     do_point_queries (pq :: Set k) = fmap (`appEndo` Map.empty) . getAp . foldMap go $ pq where
--       go :: k -> Ap m (Endo (Map k v))
--       go k = Ap $ coerce . maybe id (Map.insert k) <$> perform_lookup k

queryMap :: Ord k => Query k v -> Map k v -> QueryResult k v
queryMap q m0 = runIdentity $ runQuery doLookup doRangeQuery (pure . fromIntegral . length $ m0) q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k offset limit = coerce $ let
      m1
        | Just k <- mb_k = let
            in Map.dropWhileAntitone (<= k) m0
        | otherwise = m0
      in Map.toAscList . Map.take (fromIntegral limit) . Map.drop (fromIntegral offset) $ m1

-- queryMap :: Ord k => Query t k v -> Map k v -> QueryResult t k v
-- queryMap q m0 = runIdentity $ runQuery do_lookup do_range_query do_rowcount  q
--   where
--     do_lookup k = coerce $ Map.lookup k m0
--     do_range_query mb_k offset limit = coerce $ let
--       m1
--         | Just k <- mb_k = let
--             in Map.dropWhileAntitone (<= k) m0
--         | otherwise = m0
--       in Map.toAscList . Map.take (fromIntegral limit) . Map.drop (fromIntegral offset) $ m1
--     do_rowcount  = pure . fromIntegral . length $ m0

-- instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Query k v) where
--   arbitrary = oneof [RWQuery <$> arbitrary, pure NoQuery]
--   shrink (RWQuery x) = NoQuery : (RWQuery <$> shrink x)
--   shrink NoQuery = []

-- instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (Query 'MK_RWU k v) where
--   arbitrary = oneof [ RWUQuery  <$> arbitrary, pure NoQuery]
--   shrink (RWUQuery x) = NoQuery : (RWUQuery <$> shrink x)
--   shrink NoQuery = []

instance (Arbitrary k, Ord k) => Arbitrary (Query k v) where
  arbitrary = Query <$> arbitrary <*> arbitrary <*> arbitrary
  shrink Query{..} = mempty
    : mempty { qCountRows    = qCountRows }
    : mempty { qPointQueries = qPointQueries }
    : mempty { qRangeQueries = qRangeQueries }
    : [ Query
        { qCountRows = cr
        , qPointQueries = pq
        , qRangeQueries = rq
        }
      | (cr, pq, rq) <- shrink (qCountRows, qPointQueries, qRangeQueries)
      ]

data QueryTestCase k v where
  QueryTestCase ::
    { qtcMap :: !(Map k v)
    , qtcPointQueries :: !(Set k)
    , qtcRangeQueries :: ![(Maybe k, NonNegative Int64, Positive Int64)]
    , qtcCountRows :: !Any
    } -> QueryTestCase k v
    deriving stock (Eq, Show)

emptyQueryTestCase :: Ord k => QueryTestCase k v
emptyQueryTestCase = QueryTestCase
  { qtcMap = Map.empty
  , qtcPointQueries = mempty
  , qtcRangeQueries = mempty
  , qtcCountRows = mempty
  }

instance (Num k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (QueryTestCase k v) where
  arbitrary = do
    (qtcMap, qtcPointQueries, qtcRangeQueries) <- scale (\x -> (x - 1) `max` 0) $ do
      (m, ks, rqs) <- do
        m <- scale (`div` 2) arbitrary
        pq_1 <- scale (`div` 4) $ sublistOf (Map.keys m)
        pq_2 <- fmap (fmap (+1)) $ scale (`div` 8) $ sublistOf (Map.keys m) -- add one, these will likely miss
        rqs <- scale (`div` 8) arbitrary
        pure (m, Set.fromList $ pq_1 <> pq_2, rqs)
      pure (m, ks, rqs)
    qtcCountRows <- resize 1 arbitrary
    pure QueryTestCase{..}
  shrink QueryTestCase{..} = emptyQueryTestCase
    : emptyQueryTestCase { qtcMap }
    : emptyQueryTestCase { qtcMap, qtcCountRows }
    : emptyQueryTestCase { qtcMap, qtcPointQueries }
    : emptyQueryTestCase { qtcMap, qtcRangeQueries }
    : [ QueryTestCase
        { qtcMap = m
        , qtcCountRows = cr
        , qtcPointQueries = pq
        , qtcRangeQueries = rq
        }
      | (m, pq, rq, cr) <- shrink
        ( qtcMap
        , qtcPointQueries
        , qtcRangeQueries
        , qtcCountRows
        )
      ]

-- instance (Ord k, Arbitrary k, Arbitrary v, Num k) => Arbitrary (QueryTestCase 'MK_RW k v) where
--   arbitrary = do
--     (rwqtcMap, rwqtcPointQueries) <- arbitraryPointQueryTestCase
--     pure RWQueryTestCase{..}

--   shrink x = [ RWQueryTestCase{..} | (rwqtcMap, rwqtcPointQueries) <- shrinkPointQueryTestCase (rwqtcMap x) (rwqtcPointQueries x)]

-- instance (Ord k, Arbitrary k, Arbitrary v, Num k) => Arbitrary (QueryTestCase 'MK_RWU k v) where
--   arbitrary = do
--     (rwuqtcMap, rwuqtcPointQueries) <- arbitraryPointQueryTestCase
--     pure RWUQueryTestCase{..}

--   shrink x = [ RWUQueryTestCase{..} | (rwuqtcMap, rwuqtcPointQueries) <- shrinkPointQueryTestCase (rwuqtcMap x) (rwuqtcPointQueries x)]


runQueryTestCase :: Ord k => QueryTestCase k v -> (Map k v, Query k v)
runQueryTestCase QueryTestCase{..} = (qtcMap , foldMap pointQuery qtcPointQueries <> cr <> foldMap do_rq qtcRangeQueries) where
  cr
    | getAny qtcCountRows = rowCountQuery
    | otherwise = mempty
  do_rq (mb_k, NonNegative o, Positive l) = rangeQuery mb_k o l

-- pattern RWQueryResultish :: Ord k => Map k v -> QueryResult 'MK_RW k v
-- pattern RWQueryResultish  { rwqriPointQueries } <- (
--   \case
--       RWQueryResult{rwqrPointQueries} -> rwqrPointQueries
--       NoQueryResult -> mempty -> rwqriPointQueries
--   )
--   where
--     RWQueryResultish rwqrPointQueries  = RWQueryResult{..}

prop_simple_validation :: QueryTestCase Int Int -> Property
prop_simple_validation qtc = let
  (the_map, q) = runQueryTestCase qtc
  qr@QueryResult{..} = queryMap q the_map
  in counterexample ("Query: " <> show q) . counterexample ("QueryResult: " <> show qr) . conjoin $
    [ counterexample "point queries don't match:" $  Map.restrictKeys the_map (qPointQueries q) === qrPointQueries
    , counterexample "row counts don't match:" $ qrCountRows === if getAny (qCountRows q) then Just (fromIntegral $ length the_map) else Nothing
    ] <>
    let
      go rq@(mb_k, (fromIntegral . getNonNegative ) -> offset :: Int, (fromIntegral . getPositive) -> limit :: Int) =
        counterexample ("range query doesn't match: " <> show rq) $ result === expected
        where
          result = fromMaybe Map.empty (Map.lookup mb_k (getRangeQueryResults qrRangeQueries) >>=  Map.lookup (fromIntegral offset) <&> Map.fromList . take (fromIntegral limit))
          expected = Map.take limit . Map.drop offset  . maybe id (\k -> snd . Map.split k) mb_k $ the_map
    in go <$> qtcRangeQueries qtc

-- pattern ROQueryResultish :: Ord k => Map k v -> RangeQueryResults k v -> Maybe Int64 -> QueryResult 'MK_RO k v
-- pattern ROQueryResultish { roqriPointQueries, roqriRangeQueryResults, roqriCountRows } <- (
--   \case
--       ROQueryResult{..} -> (roqrPointQueries, roqrRangeQueries, roqrCountRows)
--       NoQueryResult -> (mempty, emptyRangeQueryResults, Nothing) -> (roqriPointQueries, roqriRangeQueryResults, roqriCountRows )
--   )
--   where
--     ROQueryResultish roqrPointQueries roqrRangeQueries roqrCountRows  = ROQueryResult{..}

-- pattern ROQueryish :: Ord k =>  Set k -> RangeQuery k -> Any -> Query 'MK_RO k v
-- pattern ROQueryish { roqiPointQueries, roqiRangeQueries, roqiCountRows } <- (
--   \case
--       ROQuery{..} -> (roqPointQueries, roqRangeQueries, roqCountRows)
--       NoQuery -> (mempty, mempty, mempty) -> (roqiPointQueries, roqiRangeQueries, roqiCountRows ))
--   where

prop_lots_of_range_queries :: Map Int Int -> Property
prop_lots_of_range_queries m = list_from_range_queries === Map.toList m
  where
    list_from_range_queries =
      [ (k, v)
      | (_mb_k, m') <- Map.toList . getRangeQueryResults . qrRangeQueries $ query_result
      , (k, v) <- [ kv | (_, kvs) <- Map.toList m', kv <- take 1 kvs ]
      ]
    query_result = queryMap the_query m
    the_query = foldMap (\(mb_k, o, l) -> rangeQuery mb_k o l) . take (length m) $ (Nothing, 0, 1) : [ (Just k, 0, 1) | k <- Map.keys m]


prop_query_is_monoid_associative :: Query Int Int -> Query Int Int -> Query Int Int -> Property
prop_query_is_monoid_associative x y z = (x <> y) <> z === x <> y <> z

prop_query_is_monoid_identity :: Query Int Int -> Property
prop_query_is_monoid_identity x = x === x <> mempty .&&. x === mempty <> x

{-
prop> prop_query_is_monoid_associative
+++ OK, passed 100 tests.
prop> prop_query_is_monoid_identity
+++ OK, passed 100 tests.
prop> prop_simple_validation
+++ OK, passed 100 tests.
prop> prop_lots_of_range_queries
+++ OK, passed 100 tests.
-}
