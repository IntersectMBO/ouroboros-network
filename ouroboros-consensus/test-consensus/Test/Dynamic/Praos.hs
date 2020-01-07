{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Dynamic.Praos (
    tests
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Random.SplitMix
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Network
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Dynamic chain generation" $
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
    , testProperty "simple Praos convergence - Issue #1147" $
            testPraos' TestConfig
              { numCoreNodes
              , numSlots
              , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 24}),(CoreNodeId 1,SlotNo {unSlotNo = 29}),(CoreNodeId 2,SlotNo {unSlotNo = 29})])
              , nodeTopology = NodeTopology (Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 1])])
              , latencySeed = InjectLatencies (seedSMGen 9511500888644978603 10563634114029886209)
                -- ^ That seed caused a failure during work on PR 1131, which
                -- lead to Issue 1147. Seeds are unfortunately fragile, so if
                -- the test infrastructure has changed, the same seed might not
                -- reproduce the failure's necessary conditions.
              }
                Seed {getSeed = (10580735904357354200,1771287816820322801,4449307331544544775,3207572460394768516,9310143438266696584)}
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
        forAllShrink genLatencySeed shrinkLatencySeed $
        \latencySeed ->
            testPraos' TestConfig
              { numCoreNodes
              , numSlots
              , nodeJoinPlan
              , nodeTopology
              , latencySeed
              }
                seed
    ]
    `seq`
    [ testProperty "simple Praos convergence - Issue #1147" $
            testPraos' TestConfig
              { numCoreNodes
              , numSlots
              , nodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 24}),(CoreNodeId 1,SlotNo {unSlotNo = 29}),(CoreNodeId 2,SlotNo {unSlotNo = 29})])
              , nodeTopology = NodeTopology (Map.fromList [(CoreNodeId 0,Set.fromList []),(CoreNodeId 1,Set.fromList [CoreNodeId 0]),(CoreNodeId 2,Set.fromList [CoreNodeId 1])])
              , latencySeed = InjectLatencies (seedSMGen 35141719043 134466674058)
              }
                Seed {getSeed = (10580735904357354200,1771287816820322801,4449307331544544775,3207572460394768516,9310143438266696584)}
    ]
  where
    testPraos :: Seed -> Property
    testPraos = testPraos' TestConfig
      { numCoreNodes
      , numSlots
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeTopology = meshNodeTopology numCoreNodes
      , latencySeed  = noLatencySeed
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
            (\nid -> protocolInfo numCoreNodes (ProtocolMockPraos nid params))
            testConfig seed
