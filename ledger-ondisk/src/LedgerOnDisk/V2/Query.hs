{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}

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
-- import Data.IntervalMap.FingerTree (IntervalMap)
-- import Data.Traversable
import qualified Data.Set as Set
import Data.Set (Set)
import Test.QuickCheck
import Data.Functor
import Data.Maybe
import Data.Foldable
import Control.Monad
import Data.List (mapAccumR, unfoldr)
type RangeQueryId = Int64

pointQuery :: Ord k => k -> Query t k v
pointQuery k = nullQuery { qPointQueries =  Set.singleton k }

rowCountQuery :: Ord k => Query t k v
rowCountQuery = nullQuery { qCountRows = True }

rangeQuery :: Ord k => Maybe k -> Int64 -> Int64 -> Query t k v
rangeQuery mb_k offset limit = nullQuery { qRangeQueries = Map.singleton mb_k $ IntervalMap.singleton (Interval offset (offset + limit)) () }

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



data Query (t :: ki) k v = Query
  { qPointQueries :: !(Set k) -- ^ This is a 'Set k' of keys to retrieve
  , qCountRows :: !Bool -- ^ Request an estimate of the number of rows. Returns an upper bound
  , qRangeQueries :: !(Map (Maybe k) (IntervalMap Int64 ()))
    -- ^ a map from
    -- keys to the number of rows requested. Nothing means to begin at the smallest existing key.
    -- The key requested is not included in the results
  }
  deriving stock (Eq, Show)

nullQuery :: Ord k => Query t k v
nullQuery = Query { qPointQueries = mempty, qCountRows = False, qRangeQueries = Map.empty }

newtype RangeQueryResults k v = RangeQueryResults
  { getRangeQueryResults :: Map (Maybe k) (Map Int64 [(k, v)]) }
  deriving (Eq, Show)

emptyRangeQueryResults :: Ord k => RangeQueryResults k v
emptyRangeQueryResults = RangeQueryResults Map.empty

data QueryResult (t :: ki) k v = QueryResult
  { qrPointQueries :: !(Map k v) -- ^ does not include range queries
  , qrRangeQueries :: !(RangeQueryResults k v)
  , qrCountRows :: !(Maybe Int64)
  } deriving stock (Eq, Show)

-- newtype RowCountResult k v = RowCountResult (Maybe Int64)
--   deriving stock (Eq, Show)

-- type instance SemigroupMap Query = KeysAreOrd

instance Ord k => Semigroup (Query t k v) where
  x <> y = Query
    { qPointQueries = qPointQueries x <> qPointQueries y
    , qRangeQueries = Map.unionWith IntervalMap.union (qRangeQueries x) (qRangeQueries y)
    , qCountRows = coerce $ Any (qCountRows x) <> Any (qCountRows y)
    }

instance Ord k => Monoid (Query t k v) where
  mempty = nullQuery



-- runRangeQuery :: forall m k v. (Applicative m, Ord k)
--   => (Maybe k -> Int64 -> Int64 -> m [(k, v)])
--   -> Maybe k
--   -> Int64
--   -> Int64
--   -> m (Endo (RangeQueryResults k v))
-- runRangeQuery performRangeQuery mb_k offset limit = go <$> performRangeQuery mb_k offset limit

  -- Map.union should never have to deal with matching keys, runQuery makes sure of that
  -- where go kvs = coerce . Map.insertWith Map.union mb_k $ Map.singleton offset kvs

runQuery :: forall t m k v. (Applicative m, Ord k)
  => (k -> m (Maybe v))
  -> (Maybe k -> Int64 -> Int64 -> m [(k, v)])
  -> m Int64
  -> Query t k v
  -> m (QueryResult t k v)
runQuery performLookup performRangeQuery performCountRows Query{qPointQueries = query_keys, qRangeQueries, qCountRows} =
  coerce $ liftA3 combineResults doCountRows doPointQueries doRangeQueries
  where
    combineResults :: Maybe Int64 -> Endo (Map k v) -> Endo (RangeQueryResults k v) -> QueryResult t k v
    combineResults qrCountRows pqs rqs = let
      qrPointQueries = appEndo pqs Map.empty
      qrRangeQueries = appEndo rqs emptyRangeQueryResults
      in QueryResult{..}

    doCountRows
      | qCountRows = coerce $ Just <$> performCountRows
      | otherwise = pure Nothing
      
    doPointQueries  = foldMap go query_keys
      where
        go :: k -> Ap m (Endo (Map k v))
        go k = Ap $ coerce . maybe id (Map.insert k) <$> performLookup k

    doRangeQueries :: Ap m (Endo (RangeQueryResults k v))
    doRangeQueries  = Map.foldMapWithKey go_rq qRangeQueries where

      go_rq :: Maybe k -> IntervalMap Int64 () -> Ap m (Endo (RangeQueryResults k v))
      go_rq mb_k qs_im = (reconcile_results_endo <>) <$> nonoverlapping_results where
        interval_map_to_list im = unfoldr (\x -> IntervalMap.leastView x <&> \((i,_), x') -> (i, x')) im

        reconcile_results_endo = Endo . coerce $ Map.adjust reconcile_results mb_k
        reconcile_results :: Map Int64 [(k, v)] -> Map Int64 [(k, v)]
        reconcile_results nonoverlapping_r = foldr go Map.empty $ interval_map_to_list qs_im  where
          go (Interval lb ub) acc = case Map.lookupLE lb nonoverlapping_r of
            Nothing -> Map.insert lb [] acc
            Just (k, v) -> Map.insert lb (take (fromIntegral $ ub - lb + 1) . drop (fromIntegral $ lb - k) $ v) acc
        nonoverlapping_results = foldMap go (non_overlapping_intervals qs_im) where
          go (Interval lb ub) = Ap $ go' <$> performRangeQuery mb_k lb ub where
            go' kvs = coerce $ Map.insertWith Map.union mb_k $ Map.singleton lb kvs

        non_overlapping_intervals im = case IntervalMap.leastView im of
          Nothing -> []
          Just ((i1,()), rest) -> non_overlapping_intervals' [] i1 rest
        non_overlapping_intervals' acc i@(IntervalMap.Interval lb1 ub1) rest = case IntervalMap.leastView rest of
          Nothing -> i : acc
          Just ((i2@(IntervalMap.Interval lb2 ub2),()), next)
            | lb2 <= ub1 -> non_overlapping_intervals' acc (IntervalMap.Interval lb1 (ub1 `max` ub2)) next
            | otherwise -> non_overlapping_intervals' (i : acc) i2 next


queryMap :: Ord k => Query t k v -> Map k v -> QueryResult t k v
queryMap q m0 = runIdentity $ runQuery doLookup doRangeQuery (pure . fromIntegral . length $ m0) q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k offset limit = coerce $ let
      m1
        | Just k <- mb_k = let
            in Map.dropWhileAntitone (<= k) m0
        | otherwise = m0
      in Map.toAscList . Map.take (fromIntegral limit) . Map.drop (fromIntegral offset) $ m1

data QueryTestCase k v = QueryTestCase
  { qtcMap :: !(Map k v)
  , qtcPointQueries :: !(Set k)
  , qtcRangeQueries :: ![(Maybe k, NonNegative Int64, Positive Int64)]
  , qtcCountRows :: !Bool
  } deriving (Show)

instance (Num k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (QueryTestCase k v) where
  arbitrary = do
    (qtcMap, qtcPointQueries, qtcRangeQueries) <- scale (\x -> (x - 1) `max` 0) $ do
      qtcMap <- scale (`div` 2) arbitrary
      pq_1 <- scale (`div` 6) $ sublistOf (Map.keys qtcMap)
      pq_2 <- fmap (fmap (+1)) $ scale (`div` 6) $ sublistOf (Map.keys qtcMap) -- add one, these will likely miss
      rqs <- scale (`div` 6) arbitrary
      pure (qtcMap, Set.fromList $ pq_1 <> pq_2, rqs)
      -- let qtcPointQueries = Set.fromList $ pq_1 <> pq_2
    qtcCountRows <- resize 1 arbitrary
    pure QueryTestCase{..}
  shrink qtc =
      [ QueryTestCase{..}
      | (qtcMap, qtcPointQueries, qtcRangeQueries, qtcCountRows) <- shrink (qtcMap qtc, qtcPointQueries qtc, qtcRangeQueries qtc, qtcCountRows qtc)
      ]

emptyQueryTestCase :: Ord k => QueryTestCase k v
emptyQueryTestCase = QueryTestCase
  { qtcMap = Map.empty
  , qtcPointQueries = mempty
  , qtcRangeQueries = mempty
  , qtcCountRows = False
  }

runQueryTestCase :: Ord k => QueryTestCase k v -> (Map k v, Query t k v)
runQueryTestCase QueryTestCase{..} =
  ( qtcMap
  , Query
    { qPointQueries = qtcPointQueries
    , qCountRows = qtcCountRows
    , qRangeQueries = let
        go (mb_k, NonNegative offset, Positive limit) = Map.insertWith (<>) mb_k (IntervalMap.singleton (Interval offset (offset + limit)) ())
        in foldr go Map.empty qtcRangeQueries
    }
  )

prop_simple_validation :: QueryTestCase Int Int -> Property
prop_simple_validation qtc = let
  (the_map, q@Query{..}) = runQueryTestCase qtc
  qr@QueryResult{..} = queryMap q the_map
  in counterexample ("Query: " <> show q) . counterexample ("QueryResult: " <> show qr) . conjoin $
    [ counterexample "point queries don't match:" $  Map.restrictKeys the_map qPointQueries === qrPointQueries
    , counterexample "row counts don't match:" $ qrCountRows === if qCountRows then Just (fromIntegral $ length the_map) else Nothing
    ] <>
    let
      go rq@(mb_k, (fromIntegral . getNonNegative ) -> offset :: Int, (fromIntegral . getPositive) -> limit :: Int) =
        counterexample ("range query doesn't match: " <> show rq) $ result === expected
        where
          result = fromMaybe Map.empty (Map.lookup mb_k (getRangeQueryResults qrRangeQueries) >>=  Map.lookup (fromIntegral offset) <&> Map.fromList . take (fromIntegral limit))
          expected = Map.take limit . Map.drop offset  . maybe id (\k -> snd . Map.split k) mb_k . qtcMap $ qtc
    in go <$> qtcRangeQueries qtc

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

{-

prop> prop_simple_validation
+++ OK, passed 100 tests.
prop> prop_lots_of_range_queries
+++ OK, passed 100 tests.

-}
