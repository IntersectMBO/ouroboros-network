{-# LANGUAGE NamedFieldPuns #-}

module Test.Dynamic.BFT (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" $
        prop_simple_bft_convergence k
    ]
  where
    k = SecurityParam 5

prop_simple_bft_convergence :: SecurityParam
                            -> TestConfig
                            -> Seed
                            -> Property
prop_simple_bft_convergence k
  testConfig@TestConfig{numCoreNodes, numSlots} seed =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    testOutput =
        runTestNetwork
            (\nid -> protocolInfo (ProtocolMockBFT numCoreNodes nid k))
            testConfig seed
