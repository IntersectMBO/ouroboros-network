module Test.Dynamic.BFT (
    tests
  ) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple BFT convergence" $
        \numCoreNodes numSlots seed ->
        forAllShrink
            (genNodeJoinPlan numCoreNodes numSlots)
            shrinkNodeJoinPlan $
        \nodeJoinPlan ->
            prop_simple_bft_convergence
                k numCoreNodes numSlots nodeJoinPlan seed
    ]
  where
    k = defaultSecurityParam

prop_simple_bft_convergence :: SecurityParam
                            -> NumCoreNodes
                            -> NumSlots
                            -> NodeJoinPlan
                            -> Seed
                            -> Property
prop_simple_bft_convergence k numCoreNodes numSlots nodeJoinPlan seed =
    prop_general k
        numSlots nodeJoinPlan
        (roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockBFT k))
            numCoreNodes numSlots nodeJoinPlan seed
