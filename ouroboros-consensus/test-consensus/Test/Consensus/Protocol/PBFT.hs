{-# LANGUAGE OverloadedLists #-}
module Test.Consensus.Protocol.PBFT where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Protocol.PBFT.ChainState (PBftChainState)
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS
import           Ouroboros.Consensus.Protocol.PBFT.Crypto

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

newtype TestChainState = TestChainState (PBftChainState PBftMockCrypto)
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
    return $ TestChainState . testState $ Map.fromList
      [ (k, Seq.fromList vs)
      | k <- [0..nbKs-1]
      , let vs = [v | v <- [0..nbVs-1], v `mod` nbKs == k]
      ]

testState :: Map Int (Seq Int) -> PBftChainState PBftMockCrypto
testState = CS.fromMap
          . Map.mapKeys VerKeyMockDSIGN
          . Map.map (fmap fromIntegral)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

prop_pruneChainState_chainStateSize :: TestChainState -> Property
prop_pruneChainState_chainStateSize (TestChainState cs) =
    CS.prune (CS.size cs) cs === cs

prop_chainStateSize_pruneChainState :: Positive Int -> TestChainState -> Property
prop_chainStateSize_pruneChainState (Positive n) (TestChainState cs)
    | CS.size cs >= n
    = label "pruned"
    $ CS.size (CS.prune n cs) === n
    | otherwise
    = label "not pruned"
    $ CS.size (CS.prune n cs) === CS.size cs

test_pruneChainState :: Assertion
test_pruneChainState =
    CS.prune 2 cs @?= testState (Map.fromList [(100, [5]), (101, [6])])
  where
    cs :: PBftChainState PBftMockCrypto
    cs = testState $ Map.fromList [(100, [1,2,5]), (101, [3, 4, 6])]
