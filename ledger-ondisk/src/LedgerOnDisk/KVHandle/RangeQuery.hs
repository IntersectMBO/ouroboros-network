{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LedgerOnDisk.KVHandle.RangeQuery where

import LedgerOnDisk.KVHandle.OnDiskMappings
import Data.Map (Map)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Monoid
import qualified Data.Vector as Vector
import Data.Coerce
import Control.Applicative
import Data.Functor
import Test.QuickCheck
import Data.Functor.Identity

type RangeQueryId = Int64

data Query k v = Query
  { pointQueries :: !(Keys k v) -- | This is a 'Set k'
  , rangeQueries :: !(Map (Maybe k) (Map RangeQueryId Int64)) -- | a map from
  -- keys to the the range queries (and num rows requested). Nothing means use the
  -- first key
  }
  deriving stock (Eq, Show)

type instance SemigroupMap Query = KeysAreOrd

instance Ord k => Semigroup (Query k v) where
  x <> y = Query
    { pointQueries = pointQueries x <> pointQueries y
    , rangeQueries = let
        go = Map.unionWith max
        in Map.unionWith go (rangeQueries x) (rangeQueries y)
    }

instance Ord k => Monoid (Query k v) where
  mempty = Query
    { pointQueries = mempty
    , rangeQueries = Map.empty
    }

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Query k v) where
  arbitrary = Query <$> arbitrary <*> arbitrary

data RangeQueryMetadata k v = RangeQueryMetadata
    { returnedRows :: [(k,v)]
    , nextKey :: !(Maybe (k, Int64)) -- ^ if there are remaining rows, the next k that would be returned (i.e. query this next time if you are trying to query everything), and how many rows remain (guaranteed > 0)
    } deriving stock (Eq, Show)

data QueryResult k v = QueryResult
  { queryResults :: !(Map k v)
  , rangeQueryMetadata :: !(Map (Maybe k) (RangeQueryMetadata k v))
  }


runRangeQuery :: forall m k v. (Applicative m, Ord k)
  => (Maybe k -> Int64 -> m (RangeQueryMetadata k v, [(k, v)]))
  -> Maybe k
  -> Map RangeQueryId Int64
  -> m (Endo (Map k v), Endo (Map RangeQueryId (RangeQueryMetadata k v)))
runRangeQuery performRangeQuery mbk rqs = performRangeQuery mbk needed_rows <&> \(md, kvs) -> let
  (kv_map, rq_map) = handle_results md kvs
  in (Endo (`Map.union` kv_map), Endo (`Map.union` rq_map))
  where
    needed_rows = maximum rqs
    handle_results :: RangeQueryMetadata k v -> [(k, v)] -> (Map k v, Map RangeQueryId (RangeQueryMetadata k v))
    handle_results RangeQueryMetadata{..} kvs = (Map.fromAscList kvs, Map.map compute_rqm rqs)
      where
        result_vector = Vector.fromList kvs
        compute_rqm :: Int64 -> RangeQueryMetadata k v
        compute_rqm n
          | n >= returnedRows = RangeQueryMetadata {returnedRows, nextKey = Nothing}
          | otherwise = RangeQueryMetadata
            { returnedRows = n
            , nextKey = let
                num_rows_declined = returnedRows - n
                in Just
                   ( fst $ result_vector Vector.! fromIntegral n
                   , maybe 0 snd nextKey + num_rows_declined
                   )
            }


runQuery :: forall m k v. (Applicative m, Ord k)
  => (k -> m (Maybe v))
  -> (Maybe k -> Int64 -> m (RangeQueryMetadata k v, [(k, v)]))
  -> Query k v
  -> m (QueryResult k v)
runQuery performLookup performRangeQuery Query{pointQueries = Keys query_keys, rangeQueries} =
  coerce $ liftA2 combineResults pointQueryResults rangeQueryResults
  where
    combineResults :: Endo (Map k v) -> (Endo (Map k v), Endo (Map RangeQueryId (RangeQueryMetadata k v))) -> QueryResult k v
    combineResults kv1 (kv2, rqs) = let
      queryResults = appEndo (kv1 <> kv2) Map.empty
      rangeQueryMetadata = appEndo rqs Map.empty
      in QueryResult{..}
    pointQueryResults :: Ap m (Endo (Map k v))
    pointQueryResults = foldMap go query_keys
      where
        go :: k -> Ap m (Endo (Map k v))
        go k = Ap $ coerce . maybe id (Map.insert k) <$> performLookup k

    rangeQueryResults :: Ap m (Endo (Map k v), Endo (Map RangeQueryId (RangeQueryMetadata k v)))
    rangeQueryResults = Map.foldMapWithKey (coerce $ runRangeQuery performRangeQuery) rangeQueries

queryMap :: Ord k => Query k v -> Map k v -> QueryResult k v
queryMap q m0 = runIdentity $ runQuery doLookup doRangeQuery q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k n = coerce $ let
      m1
        | Just k <- mb_k = let
            (_, mb_r, r0) = Map.splitLookup k m0
            in maybe id (Map.insert k) mb_r r0
        | otherwise = m0
      (result, rest) = Map.splitAt (fromIntegral n) m1
      returnedRows = fromIntegral $ length result
      remaining_rows = fromIntegral $ length rest
      nextKey = (\(k, _) -> (k, remaining_rows)) <$> Map.lookupMin rest
      in (RangeQueryMetadata { returnedRows, nextKey }, Map.toList result)
