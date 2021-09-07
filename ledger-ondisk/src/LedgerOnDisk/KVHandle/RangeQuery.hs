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

type RangeQueryId = Int64

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
  { pointQueries :: !(Keys k v) -- ^ This is a 'Set k' of keys to retrieve
  , countRows :: !Bool -- ^ Request an estimate of the number of rows. Returns an upper bound
  , rangeQueries :: !(Map (Maybe k) Int64)
    -- ^ a map from
    -- keys to the number of rows requested. Nothing means to begin at the smallest existing key.
    -- The key requested is not included in the results
  }
  deriving stock (Eq, Show)

newtype RangeQueryResults k v = RangeQueryResults
  { getRangeQueryResults :: Map (Maybe k) [(k, v)] }
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
    { pointQueries = pointQueries x <> pointQueries y
    , rangeQueries = Map.unionWith max (rangeQueries x) (rangeQueries y)
    , countRows = coerce $ Any (countRows x) <> Any (countRows y)
    }

instance Ord k => Monoid (Query k v) where
  mempty = Query
    { pointQueries = mempty
    , rangeQueries = Map.empty
    , countRows = getAny mempty
    }

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Query k v) where
  arbitrary = Query <$> arbitrary <*> arbitrary <*> arbitrary


runRangeQuery :: forall m k v. (Applicative m, Ord k)
  => (Maybe k -> Int64 -> m [(k, v)])
  -> Maybe k
  -> Int64
  -> m (Endo (RangeQueryResults k v))
runRangeQuery performRangeQuery mb_k n_rows = go <$> performRangeQuery mb_k n_rows
  where go kvs = coerce $ Map.insert mb_k kvs

runQuery :: forall m k v. (Applicative m, Ord k)
  => (k -> m (Maybe v))
  -> (Maybe k -> Int64 -> m [(k, v)])
  -> Query k v
  -> m (QueryResult k v)
runQuery performLookup performRangeQuery Query{pointQueries = Keys query_keys, rangeQueries} =
  coerce $ liftA2 combineResults doPointQuery doRangeQuery
  where
    combineResults :: Endo (Map k v) -> Endo (RangeQueryResults k v) -> QueryResult k v
    combineResults pqs rqs = let
      queryResults = appEndo pqs Map.empty
      rangeQueryResults = appEndo rqs emptyRangeQueryResults
      -- TODO implement me
      rowCount = pure 0
      in QueryResult{..}
    doPointQuery :: Ap m (Endo (Map k v))
    doPointQuery  = foldMap go query_keys
      where
        go :: k -> Ap m (Endo (Map k v))
        go k = Ap $ coerce . maybe id (Map.insert k) <$> performLookup k

    doRangeQuery :: Ap m (Endo (RangeQueryResults k v))
    doRangeQuery  = Map.foldMapWithKey (coerce $ runRangeQuery performRangeQuery) rangeQueries

queryMap :: Ord k => Query k v -> Map k v -> QueryResult k v
queryMap q m0 = runIdentity $ runQuery doLookup doRangeQuery q
  where
    doLookup k = coerce $ Map.lookup k m0
    doRangeQuery mb_k n = coerce $ let
      m1
        | Just k <- mb_k = let
            in Map.dropWhileAntitone (<= k) m0
        | otherwise = m0
      in Map.toAscList $ Map.take (fromIntegral n) m1
