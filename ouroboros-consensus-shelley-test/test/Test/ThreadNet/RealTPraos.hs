{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.RealTPraos (tests) where

import           Data.List ((!!))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.NodeId

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley
import           Test.ThreadNet.Rekeying

import           Test.Util.Orphans.Arbitrary ()

import           Ouroboros.Consensus.Shelley.Node

import           Test.ThreadNet.TxGen.Shelley
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)

tests :: TestTree
tests = testGroup "RealTPraos"
    [ testProperty "simple convergence" $ withMaxSuccess 20 $
          forAllShrink
              (genShelleyTestConfig testTuning)
              shrinkShelleyTestConfig
            $ \testConfig ->
          prop_simple_real_tpraos_convergence testConfig
    , testProperty "ocert replacement" $ withMaxSuccess 20 $
          forAllShrink
              (genShelleyTestConfig testTuning {rotateKESKeys = True})
              shrinkShelleyTestConfig
            $ \testConfig ->
          prop_simple_real_tpraos_convergence testConfig
    ]

prop_simple_real_tpraos_convergence
  :: ShelleyTestConfig
  -> Property
prop_simple_real_tpraos_convergence
  ShelleyTestConfig
    {stcTestConfig, stcGenesis, stcCoreNodes, stcNodeRestarts} =
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = fromIntegral . length . extractTxs
      , pgaExpectedCannotLead     = \_slot _nid -> const True
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          = Nothing
      , pgaSecurityParam          = k
      , pgaTestConfig             = stcTestConfig
      , pgaTestConfigB            = testConfigB
      }
      testOutput

  where
    TestConfig {initSeed, numCoreNodes} = stcTestConfig

    testConfigB = TestConfigB
      { epochSize = sgEpochLength stcGenesis
      , forgeEbbEnv  = Nothing
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = stcNodeRestarts
      , slotLength   = tpraosSlotLength
      , txGenExtra   = ShelleyTxGenExtra $ mkGenEnv stcCoreNodes
      }

    testOutput =
        runTestNetwork stcTestConfig testConfigB
            TestConfigMB
            { nodeInfo    = \(CoreNodeId nid) ->
              plainTestNodeInitialization $
                mkProtocolRealTPraos
                  stcGenesis
                  (stcCoreNodes !! fromIntegral nid)
            , mkRekeyM    = Just . fromRekeyingToRekeyM
               $ ocertRekeying initSeed stcGenesis stcCoreNodes
            }

    k = sgSecurityParam stcGenesis
