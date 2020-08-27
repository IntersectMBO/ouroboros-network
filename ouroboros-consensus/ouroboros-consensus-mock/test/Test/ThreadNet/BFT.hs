{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wwarn    #-}
module Test.ThreadNet.BFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util (Dict (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.TxGen.Mock ()
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeToNodeVersion
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.Seed
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Consensus.Ledger.Mock.Generators ()

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip
import           Test.Util.Slots

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
    [ roundtrip_all testCodecCfg dictNestedHdr
    , testProperty "simple convergence" $ \setup ->
        prop_simple_bft_convergence setup

    , testProperty "BUG" $ once $
        prop_simple_bft_convergence $
          let ncn = NumCoreNodes 2 in
          TestSetup {
              setupK = SecurityParam 2
            , setupTestConfig = TestConfig {
                  initSeed = Seed 0
                , nodeTopology = meshNodeTopology ncn
                , numCoreNodes = ncn
                , numSlots = NumSlots 6
                }
            , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo 0),(CoreNodeId 1,SlotNo 4)])
            }
    ]
  where
    -- We pick a single value for @k@ for the serialisation tests
    testCodecCfg :: CodecConfig MockBftBlock
    testCodecCfg = SimpleCodecConfig (SecurityParam 4)

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
