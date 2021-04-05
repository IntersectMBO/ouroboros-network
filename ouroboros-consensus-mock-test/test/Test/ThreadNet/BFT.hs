{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.ThreadNet.BFT (tests) where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.BFT
import           Ouroboros.Consensus.Mock.Node.Serialisation
import           Ouroboros.Consensus.Util (Dict (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Consensus.Ledger.Mock.Generators ()

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

data TestSetup = TestSetup
  { setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupNodeJoinPlan :: NodeJoinPlan
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      -- TODO k > 1 as a workaround for Issue #1511.
      k <- SecurityParam <$> choose (2, 10)

      testConfig <- arbitrary
      let TestConfig{numCoreNodes, numSlots} = testConfig

      nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots

      pure $ TestSetup k testConfig nodeJoinPlan

  -- TODO shrink

tests :: TestTree
tests = testGroup "BFT" $
    [ roundtrip_all SimpleCodecConfig dictNestedHdr
    , testProperty "simple convergence" $ \setup ->
        prop_simple_bft_convergence setup
    ]
  where
    dictNestedHdr :: forall a. NestedCtxt_ MockBftBlock Header a -> Dict (Eq a, Show a)
    dictNestedHdr CtxtMock = Dict

prop_simple_bft_convergence :: TestSetup -> Property
prop_simple_bft_convergence TestSetup
  { setupK            = k
  , setupTestConfig   = testConfig
  , setupNodeJoinPlan = nodeJoinPlan
  } =
    prop_general PropGeneralArgs
      { pgaBlockProperty       = prop_validSimpleBlock
      , pgaCountTxs            = countSimpleGenTxs
      , pgaExpectedCannotForge = noExpectedCannotForges
      , pgaFirstBlockNo        = 0
      , pgaFixedMaxForkLength  = Nothing
      , pgaFixedSchedule       =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam       = k
      , pgaTestConfig          = testConfig
      , pgaTestConfigB         = testConfigB
      }
      testOutput
  where
    TestConfig{numCoreNodes, numSlots} = testConfig
    slotLength = slotLengthFromSec 20
    testConfigB = TestConfigB
      { forgeEbbEnv = Nothing
      , future      = singleEraFuture
          slotLength
          (EpochSize $ maxRollbacks k * 10)
          -- The mock ledger doesn't really care, and neither does BFT. We
          -- stick with the common @k * 10@ size for now.
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      , version      = newestVersion (Proxy @MockBftBlock)
      }

    testOutput =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = \nid ->
                plainTestNodeInitialization $
                  protocolInfoBft
                    numCoreNodes
                    nid
                    k
                    (HardFork.defaultEraParams k slotLength)
            , mkRekeyM = Nothing
            }
