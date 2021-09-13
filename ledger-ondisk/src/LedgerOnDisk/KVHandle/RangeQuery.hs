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
import Data.Coerce
import Control.Applicative
import Test.QuickCheck
import Data.Functor.Identity
import Data.IntervalMap.FingerTree as IntervalMap
import Data.IntervalMap.FingerTree (IntervalMap)
import Data.Traversable
import qualified Data.Set as Set
type RangeQueryId = Int64

pointQuery :: Ord k => k -> Query k v
pointQuery k = nullQuery { qPointQueries = Keys . Set.singleton $ k }

rowCountQuery :: Ord k => Query k v
rowCountQuery = nullQuery { qCountRows = True }

rangeQuery :: Ord k => Maybe k -> Int64 -> Int64 -> Query k v
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

data Query k v = Query
  { qPointQueries :: !(Keys k v) -- ^ This is a 'Set k' of keys to retrieve
  , qCountRows :: !Bool -- ^ Request an estimate of the number of rows. Returns an upper bound
  , qRangeQueries :: !(Map (Maybe k) (IntervalMap Int64 ()))
    -- ^ a map from
    -- keys to the number of rows requested. Nothing means to begin at the smallest existing key.
    -- The key requested is not included in the results
  }
  deriving stock (Eq, Show)

nullQuery :: Ord k => Query k v
nullQuery = Query { qPointQueries = mempty, qCountRows = False, qRangeQueries = Map.empty }

newtype RangeQueryResults k v = RangeQueryResults
  { getRangeQueryResults :: Map (Maybe k) (Map Int64 [(k, v)]) }
  deriving (Eq, Show)

emptyRangeQueryResults :: Ord k => RangeQueryResults k v
emptyRangeQueryResults = RangeQueryResults Map.empty

data QueryResult k v = QueryResult
  { queryResults :: !(Map k v) -- ^ does not include range queries
  , rangeQueryResults :: !(RangeQueryResults k v)
  , rowCount :: !(Maybe Int64)
  }

-- newtype RowCountResult k v = RowCountResult (Maybe Int64)
--   deriving stock (Eq, Show)

type instance SemigroupMap Query = KeysAreOrd

instance Ord k => Semigroup (Query k v) where
  x <> y = Query
    { qPointQueries = qPointQueries x <> qPointQueries y
    , qRangeQueries = Map.unionWith IntervalMap.union (qRangeQueries x) (qRangeQueries y)
    , qCountRows = coerce $ Any (qCountRows x) <> Any (qCountRows y)
    }

instance Ord k => Monoid (Query k v) where
  mempty = nullQuery

-- instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Query k v) where
--   arbitrary = Query <$> arbitrary <*> arbitrary <*> arbitrary


-- runRangeQuery :: forall m k v. (Applicative m, Ord k)
--   => (Maybe k -> Int64 -> Int64 -> m [(k, v)])
--   -> Maybe k
--   -> Int64
--   -> Int64
--   -> m (Endo (RangeQueryResults k v))
-- runRangeQuery performRangeQuery mb_k offset limit = go <$> performRangeQuery mb_k offset limit

  -- Map.union should never have to deal with matching keys, runQuery makes sure of that
  -- where go kvs = coerce . Map.insertWith Map.union mb_k $ Map.singleton offset kvs

runQuery :: forall m k v. (Applicative m, Ord k)
  => (k -> m (Maybe v))
  -> (Maybe k -> Int64 -> Int64 -> m [(k, v)])
  -> Query k v
  -> m (QueryResult k v)
runQuery performLookup performRangeQuery Query{qPointQueries = Keys query_keys, qRangeQueries} =
  coerce $ liftA2 combineResults doPointQueries doRangeQueries
  where
    combineResults :: Endo (Map k v) -> Endo (RangeQueryResults k v) -> QueryResult k v
    combineResults pqs rqs = let
      queryResults = appEndo pqs Map.empty
      rangeQueryResults = appEndo rqs emptyRangeQueryResults
      -- TODO implement me
      rowCount = pure 0
      in QueryResult{..}
    doPointQueries  = foldMap go query_keys
      where
        go :: k -> Ap m (Endo (Map k v))
        go k = Ap $ coerce . maybe id (Map.insert k) <$> performLookup k

    doRangeQueries :: Ap m (Endo (RangeQueryResults k v))
    doRangeQueries  = Map.foldMapWithKey go qRangeQueries
      where
        go :: Maybe k -> IntervalMap Int64 () -> Ap m (Endo (RangeQueryResults k v))
        go mb_k interval_map = foldMap go_interval (non_overlapping_intervals interval_map)
          where
            go_interval (Interval lb ub) = Ap $ go' <$> performRangeQuery mb_k lb ub
              where
                go' kvs = coerce $ Map.insertWith Map.union mb_k $ Map.singleton lb kvs

        non_overlapping_intervals im = case IntervalMap.leastView im of
          Nothing -> []
          Just ((i1,()), rest) -> non_overlapping_intervals' [] i1 rest
        non_overlapping_intervals' acc i@(IntervalMap.Interval lb1 ub1) rest = case IntervalMap.leastView rest of
          Nothing -> i : acc
          Just ((i2@(IntervalMap.Interval lb2 ub2),()), next)
            | lb2 <= ub1 -> non_overlapping_intervals' acc (IntervalMap.Interval lb1 (ub1 `max` ub2)) next
            | otherwise -> non_overlapping_intervals' (i : acc) i2 next
          

queryMap :: Ord k => Query k v -> Map k v -> QueryResult k v
queryMap q m0 = runIdentity $ runQuery doLookup doRangeQuery q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k offset limit = coerce $ let
      m1
        | Just k <- mb_k = let
            in Map.dropWhileAntitone (<= k) m0
        | otherwise = m0
      in Map.toAscList . Map.take (fromIntegral limit) . Map.drop (fromIntegral offset) $ m1
