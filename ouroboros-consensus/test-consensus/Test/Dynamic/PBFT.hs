module Test.Dynamic.PBFT (
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
      testProperty "simple PBFT convergence" $
        \numCoreNodes numSlots seed ->
        forAllShrink
            (genNodeJoinPlan numCoreNodes numSlots)
            shrinkNodeJoinPlan $
        \nodeJoinPlan ->
            prop_simple_pbft_convergence
                k numCoreNodes numSlots nodeJoinPlan seed
    ]
  where
    k = defaultSecurityParam

prop_simple_pbft_convergence :: SecurityParam
                             -> NumCoreNodes
                             -> NumSlots
                             -> NodeJoinPlan
                             -> Seed
                             -> Property
prop_simple_pbft_convergence
  k numCoreNodes@(NumCoreNodes nn) numSlots nodeJoinPlan seed =
    prop_general k
        numSlots nodeJoinPlan
        (roundRobinLeaderSchedule numCoreNodes numSlots)
        testOutput
  where
    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k (fromIntegral nn) sigThd

    testOutput =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid (ProtocolMockPBFT params))
            numCoreNodes numSlots nodeJoinPlan seed
