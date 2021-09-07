-- |

{-# LANGUAGE NamedFieldPuns #-}
module LedgerOnDisk.KVHandle.RangeQuerySuite where

import LedgerOnDisk.KVHandle.RangeQuery
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.QuickCheck
import qualified Data.Set as Set
import LedgerOnDisk.KVHandle.Class
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Coerce
import Data.Int

suite :: TestTree
suite = testGroup "RangeQuery"
  [ testProperty "prop_one_point_query" prop_one_point_query
  , testProperty "prop_many_point_queries" prop_many_point_queries
  ]

prop_one_point_query :: Int -> Map Int Int -> Property
prop_one_point_query k m = property $ let
  QueryResult{queryResults} = queryMap mempty {pointQueries = Keys $ Set.singleton k} m
  in Map.restrictKeys m (Set.singleton k) === queryResults

prop_many_point_queries :: Keys Int Int -> Map Int Int -> Property
prop_many_point_queries ks m = property $ let
  QueryResult{queryResults} = queryMap mempty {pointQueries = ks} m
  in Map.restrictKeys m (coerce ks) === queryResults

prop_range_query_equals_many_point_queries :: RangeQueryId -> Maybe Int -> Int64 -> Map Int Int -> Property
prop_range_query_equals_many_point_queries rid mb_k n_rows m = property $ let
  QueryResult{queryResults} = queryMap mempty {rangeQueries = Map.singleton mb_k (Map.singleton rid n_rows)} m
  map_k = Map.dropWhileAntitone (\x -> maybe False (x <) mb_k) m
  (expected_lookups, rest) = Map.splitAt n map_k
  in expected_lookups === queryResults .&&. rangeQueryMetadata ===
