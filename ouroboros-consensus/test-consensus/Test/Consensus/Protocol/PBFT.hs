{-# LANGUAGE OverloadedLists #-}
module Test.Consensus.Protocol.PBFT where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Protocol.PBFT (pruneChainState, chainStateSize)

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "PBFT"
    [ testGroup "pruneChainState"
      [ testProperty "pruneChainState/chainStateSize" prop_pruneChainState_chainStateSize
      , testProperty "chainStateSize/pruneChainState" prop_chainStateSize_pruneChainState
      , testCase     "pruneChainState 2"              test_pruneChainState
      ]
    ]

{-------------------------------------------------------------------------------
  Test setup
-------------------------------------------------------------------------------}

newtype TestChainState = TestChainState (Map Int (Seq Int))
  deriving (Show)

instance Arbitrary TestChainState where
  arbitrary = do
    -- The weights and ranges have been tweaked such that
    -- 'prop_chainStateSize_pruneChainState' can do some pruning.
    nbKs <- frequency
      -- We want to test with no keys, but not too often
      [ (1, return 0)
      , (9, choose (1, 5))
      ]
    nbVs <- frequency
      -- We want to test with no values, but not too often
      [ (1, return 0)
      -- We want to test with fewer values than keys, but not too often
      , (1, choose (1, nbKs))
      , (8, choose (nbKs, 200))
      ]
    return $ TestChainState $ Map.fromList
      [ (k, Seq.fromList vs)
      | k <- [0..nbKs-1]
      , let vs = [v | v <- [0..nbVs-1], v `mod` nbKs == k]
      ]

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

prop_pruneChainState_chainStateSize :: TestChainState -> Property
prop_pruneChainState_chainStateSize (TestChainState cs) =
    pruneChainState (chainStateSize cs) cs === cs

prop_chainStateSize_pruneChainState :: Positive Int -> TestChainState -> Property
prop_chainStateSize_pruneChainState (Positive n) (TestChainState cs)
    | chainStateSize cs >= n
    = label "pruned"
    $ chainStateSize (pruneChainState n cs) === n
    | otherwise
    = label "not pruned"
    $ chainStateSize (pruneChainState n cs) === chainStateSize cs

test_pruneChainState :: Assertion
test_pruneChainState =
    pruneChainState 2 cs @?= Map.fromList [('a', [5]), ('b', [6])]
  where
    cs :: Map Char (Seq Int)
    cs = [('a', [1,2,5]), ('b', [3, 4, 6])]
