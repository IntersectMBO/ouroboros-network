{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Dynamic.Praos (
    tests
  ) where

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ testProperty "simple Praos convergence - special case (issue #131)" $
        testPraos $ Seed ( 49644418094676
                         , 40315957626756
                         , 42668365444963
                         , 9796082466547
                         , 32684299622558
                         )
    , testProperty "simple Praos convergence - special crowded case" $
        testPraos $ Seed ( 8871923881324151440
                         , 881094692332313449
                         , 3091285302407489889
                         , 6410351877547894330
                         , 14676014321459888687
                         )
    , testProperty "simple Praos convergence" $
        \seed ->
        forAllShrink
            (genNodeJoinPlan numCoreNodes numSlots)
            shrinkNodeJoinPlan $
        \nodeJoinPlan ->
        forAllShrink
            (genNodeTopology numCoreNodes)
            shrinkNodeTopology $
        \nodeTopology ->
            testPraos' TestConfig
              { numCoreNodes
              , numSlots
              , nodeJoinPlan
              , nodeTopology
              , slotLengths = singletonSlotLengths praosSlotLength
              }
                seed
    ]
  where
    testPraos :: Seed -> Property
    testPraos = testPraos' TestConfig
      { numCoreNodes
      , numSlots
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeTopology = meshNodeTopology numCoreNodes
      , slotLengths = singletonSlotLengths praosSlotLength
      }

    testPraos' :: TestConfig -> Seed -> Property
    testPraos' =
        prop_simple_praos_convergence
            params

    numCoreNodes = NumCoreNodes 3
    numEpochs = 3
    numSlots = NumSlots $ fromIntegral $
        maxRollbacks k * praosSlotsPerEpoch * numEpochs

    params@PraosParams{praosSecurityParam = k, ..} = PraosParams {
        praosSecurityParam = SecurityParam 5
      , praosSlotsPerEpoch = 3
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      , praosSlotLength    = slotLengthFromSec 2
      }

prop_simple_praos_convergence :: PraosParams
                              -> TestConfig
                              -> Seed
                              -> Property
prop_simple_praos_convergence
  params@PraosParams{praosSecurityParam = k}
  testConfig@TestConfig{numCoreNodes} seed =
    counterexample (tracesToDot testOutputNodes) $
    prop_general k testConfig Nothing testOutput
  where
    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork
            (\nid -> protocolInfo (ProtocolMockPraos numCoreNodes nid params))
            testConfig seed
