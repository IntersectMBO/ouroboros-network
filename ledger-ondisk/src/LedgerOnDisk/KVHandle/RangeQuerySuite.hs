-- |

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
module LedgerOnDisk.KVHandle.RangeQuerySuite where

import LedgerOnDisk.KVHandle.RangeQuery
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.QuickCheck ( (===), classify, Property )
import qualified Data.Set as Set
import LedgerOnDisk.KVHandle.Class
import Test.Tasty
import Test.Tasty.QuickCheckLabels
import Data.Coerce
import Data.Int

suite :: TestTree
suite = testGroup "RangeQuery"
  [ testProperty "prop_one_point_query" prop_one_point_query
  , testProperty "prop_many_point_queries" prop_many_point_queries
  ]

newtype PointQueryOverlays = PointQueryOverlaps Bool
  deriving stock (Show)

{-
>>> "hello"

-}

pointQueryOverlaps :: Map Int Int -> Query Int Int -> PointQueryOverlays
pointQueryOverlaps m Query{qPointQueries = Keys ks} = coerce . not . Map.null . Map.restrictKeys m $ ks

queryProperty :: Map Int Int -> Query Int Int -> (QueryResult Int Int -> Property) -> Property
queryProperty m q f = let
  pqo = pointQueryOverlaps m q

  in classify (coerce pqo) "pointqueryoverlaps" . f $ queryMap q m
  -- where
  --   labels = concat
  --     [ [ "pointQueryOverlaps" | pointQueryOverlaps m q]
  --     , [ "pointQueryNoOverlaps" | not $ pointQueryOverlaps m q]
  --     ]

prop_one_point_query :: Int -> Map Int Int -> Property
prop_one_point_query k m = queryProperty m (pointQuery k) $ \QueryResult{..} ->
  Map.restrictKeys m (Set.singleton k) === queryResults

prop_many_point_queries :: [Int] -> Map Int Int -> Property
prop_many_point_queries ks m = queryProperty m (foldMap pointQuery ks) $ \QueryResult{..} ->
  Map.restrictKeys m (Set.fromList ks) === queryResults

-- prop_range_query_equals_many_point_queries :: RangeQueryId -> Maybe Int -> Int64 -> Map Int Int -> Property
-- prop_range_query_equals_many_point_queries rid mb_k n_rows m = property $ let
--   QueryResult{queryResults} = queryMap mempty {rangeQueries = Map.singleton mb_k (Map.singleton rid n_rows)} m
--   map_k = Map.dropWhileAntitone (\x -> maybe False (x <) mb_k) m
--   (expected_lookups, rest) = Map.splitAt n map_k
--   in expected_lookups === queryResults .&&. rangeQueryMetadata ===
