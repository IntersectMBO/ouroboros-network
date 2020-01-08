{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ThreadNet.Praos (
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

import           Test.ThreadNet.General
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Praos"
    [ testProperty "simple convergence - special case (issue #131)" $
          testPraos $ Seed (49644418094676, 40315957626756, 42668365444963, 9796082466547, 32684299622558)
    , testProperty "simple convergence - special crowded case" $
          testPraos $ Seed (8871923881324151440, 881094692332313449, 3091285302407489889, 6410351877547894330, 14676014321459888687)
    , testProperty "simple convergence"
        $ \initSeed ->
          forAllShrink
              (genNodeJoinPlan numCoreNodes numSlots)
              shrinkNodeJoinPlan
            $ \nodeJoinPlan ->
          forAllShrink
              (genNodeTopology numCoreNodes)
              shrinkNodeTopology
            $ \nodeTopology ->
          testPraos' TestConfig
            { numCoreNodes
            , numSlots
            , nodeJoinPlan
            , nodeRestarts = noRestarts
            , nodeTopology
            , slotLengths  = singletonSlotLengths praosSlotLength
            , initSeed
            }
    ]
  where
    testPraos :: Seed -> Property
    testPraos seed = testPraos' TestConfig
      { numCoreNodes
      , numSlots
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , nodeTopology = meshNodeTopology numCoreNodes
      , slotLengths  = singletonSlotLengths praosSlotLength
      , initSeed     = seed
      }

    testPraos' :: TestConfig -> Property
    testPraos' = prop_simple_praos_convergence params

    numCoreNodes = NumCoreNodes 3
    numEpochs    = 3
    numSlots     = NumSlots $ fromIntegral $
      maxRollbacks k * praosSlotsPerEpoch * numEpochs

    params@PraosParams{praosSecurityParam = k, ..} = PraosParams
      { praosSecurityParam = SecurityParam 5
      , praosSlotsPerEpoch = 3
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      , praosSlotLength    = slotLengthFromSec 2
      }

prop_simple_praos_convergence :: PraosParams
                              -> TestConfig
                              -> Property
prop_simple_praos_convergence
  params@PraosParams{praosSecurityParam}
  testConfig@TestConfig{numCoreNodes} =
    counterexample (tracesToDot testOutputNodes) $
    prop_general
      praosSecurityParam
      testConfig
      Nothing
      (const False)
      testOutput
  where
    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolMockPraos numCoreNodes nid params
            }
