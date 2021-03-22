{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Praos (tests) where

import           Data.Word (Word64)
import           Numeric.Natural (Natural)

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

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.HasCreator.Mock ()
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()

data TestSetup = TestSetup
  { setupEpochSize    :: EpochSize
  , setupInitialNonce :: Natural
    -- ^ the initial Shelley 'praosInitialEta'
    --
    -- This test varies it too ensure it explores different leader schedules.
  , setupK            :: SecurityParam
  , setupNodeJoinPlan :: NodeJoinPlan
  , setupSlotLength   :: SlotLength
  , setupTestConfig   :: TestConfig
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      -- TODO k > 1 as a workaround for Issue #1511.
      k          <- SecurityParam <$> choose (2, 10)
      epochSize  <- EpochSize     <$> choose (1, 10)
      slotLength <- arbitrary

      initialNonce <- fromIntegral <$> choose (0, maxBound :: Word64)

      testConfig <- arbitrary
      let TestConfig{numCoreNodes, numSlots} = testConfig

      nodeJoinPlan   <- genNodeJoinPlan numCoreNodes numSlots

      pure $ TestSetup
        epochSize
        initialNonce
        k
        nodeJoinPlan
        slotLength
        testConfig

  -- TODO shrink

tests :: TestTree
tests = testGroup "Praos"
    [ testProperty "simple convergence" $ \setup ->
        prop_simple_praos_convergence setup
    ]

prop_simple_praos_convergence :: TestSetup -> Property
prop_simple_praos_convergence TestSetup
  { setupEpochSize    = epochSize
  , setupK            = k
  , setupInitialNonce
  , setupNodeJoinPlan = nodeJoinPlan
  , setupSlotLength   = slotLength
  , setupTestConfig   = testConfig
  } =
    counterexample (tracesToDot testOutputNodes) $
    prop_general PropGeneralArgs
      { pgaBlockProperty       = prop_validSimpleBlock
      , pgaCountTxs            = countSimpleGenTxs
      , pgaExpectedCannotForge = noExpectedCannotForges
      , pgaFirstBlockNo        = 0
      , pgaFixedMaxForkLength  = Nothing
      , pgaFixedSchedule       = Nothing
      , pgaSecurityParam       = k
      , pgaTestConfig          = testConfig
      , pgaTestConfigB         = testConfigB
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
                                      setupInitialNonce
            , mkRekeyM = Nothing
            }
