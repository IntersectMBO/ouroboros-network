{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Praos (
    tests
  ) where

import           Data.Proxy (Proxy (..))

import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.Praos (MockPraosBlock,
                     protocolInfoPraos)
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random
import           Test.Util.Slots (NumSlots (..))

data TestSetup = TestSetup
  { setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupEpochSize    :: EpochSize
  , setupNodeJoinPlan :: NodeJoinPlan
  , setupSlotLength   :: SlotLength
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      -- TODO k > 1 as a workaround for Issue #1511.
      k          <- SecurityParam <$> choose (2, 10)
      epochSize  <- EpochSize     <$> choose (1, 10)
      slotLength <- arbitrary

      testConfig <- arbitrary
      let TestConfig{numCoreNodes, numSlots} = testConfig

      nodeJoinPlan   <- genNodeJoinPlan numCoreNodes numSlots

      pure $ TestSetup
        k
        testConfig
        epochSize
        nodeJoinPlan
        slotLength

  -- TODO shrink

tests :: TestTree
tests = testGroup "Praos"
    [ testProperty "simple convergence - special case (issue #131)" $
          testPraos $ Seed (49644418094676, 40315957626756, 42668365444963, 9796082466547, 32684299622558)
    , testProperty "simple convergence - special crowded case" $
          testPraos $ Seed (8871923881324151440, 881094692332313449, 3091285302407489889, 6410351877547894330, 14676014321459888687)
    , testProperty "simple convergence" $ \setup ->
        prop_simple_praos_convergence setup
    ]
  where
    testPraos :: Seed -> Property
    testPraos seed =
        prop_simple_praos_convergence TestSetup
        { setupK            = k
        , setupTestConfig   = TestConfig
          { initSeed     = seed
          , nodeTopology = meshNodeTopology numCoreNodes
          , numCoreNodes
          , numSlots     =
              NumSlots $ maxRollbacks k * unEpochSize epochSize * numEpochs
          }
        , setupEpochSize    = epochSize
        , setupNodeJoinPlan = trivialNodeJoinPlan numCoreNodes
        , setupSlotLength   = slotLengthFromSec 2
        }
      where
        numCoreNodes = NumCoreNodes 3
        k            = SecurityParam 5
        epochSize    = EpochSize 3
        numEpochs    = 3

prop_simple_praos_convergence :: TestSetup -> Property
prop_simple_praos_convergence TestSetup
  { setupK            = k
  , setupTestConfig   = testConfig
  , setupEpochSize    = epochSize
  , setupNodeJoinPlan = nodeJoinPlan
  , setupSlotLength   = slotLength
  } =
    counterexample (tracesToDot (SimpleCodecConfig k) testOutputNodes) $
    prop_general PropGeneralArgs
      { pgaBlockProperty      = prop_validSimpleBlock
      , pgaCountTxs           = countSimpleGenTxs
      , pgaExpectedCannotLead = noExpectedCannotLeads
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule      = Nothing
      , pgaSecurityParam      = k
      , pgaTestConfig         = testConfig
      , pgaTestConfigB        = testConfigB
      }
      testOutput
  where
    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture slotLength epochSize
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      , version      = newestVersion (Proxy @MockPraosBlock)
      }

    params = PraosParams
      { praosSecurityParam = k
      , praosSlotsPerEpoch = unEpochSize epochSize
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      }

    TestConfig{numCoreNodes} = testConfig

    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = \nid -> plainTestNodeInitialization $
                                    protocolInfoPraos
                                      numCoreNodes
                                      nid
                                      params
                                      (HardFork.defaultEraParams
                                        k
                                        slotLength)
            , mkRekeyM = Nothing
            }
