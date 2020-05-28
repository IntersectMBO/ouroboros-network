{-# LANGUAGE NamedFieldPuns #-}
module Test.ThreadNet.RealTPraos (tests) where

import           Data.List ((!!))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot (EpochSize (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (extractTxs)
import           Ouroboros.Consensus.NodeId

import           Test.ThreadNet.General
import           Test.ThreadNet.Infra.Shelley

import           Test.Util.Orphans.Arbitrary ()

import           Ouroboros.Consensus.Shelley.Node

import           Test.ThreadNet.TxGen.Shelley

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
  ShelleyTestConfig{stcTestConfig, stcGenesis, stcCoreNodes} =
    prop_general PropGeneralArgs
      { pgaBlockProperty          = const $ property True
      , pgaCountTxs               = fromIntegral . length . extractTxs
      , pgaExpectedBlockRejection = const False
      , pgaFirstBlockNo           = 0
      , pgaFixedMaxForkLength     = Nothing
      , pgaFixedSchedule          = Nothing
      , pgaSecurityParam          = k
      , pgaTestConfig             = stcTestConfig
      }
      testOutput

  where
    TestConfig {initSeed} = stcTestConfig
    testOutput =
        runTestNetwork stcTestConfig epochSize TestConfigBlock
            { forgeEbbEnv = Nothing
            , nodeInfo    = \(CoreNodeId nid) ->
              plainTestNodeInitialization $
                mkProtocolRealTPraos
                  stcGenesis
                  (stcCoreNodes !! fromIntegral nid)
            , rekeying    = Just $ ocertRekeying initSeed stcGenesis stcCoreNodes
            , txGenExtra  = ShelleyTxGenExtra $ mkGenEnv stcCoreNodes
            }

    epochSize :: EpochSize
    epochSize = sgEpochLength stcGenesis

    k = sgSecurityParam stcGenesis
