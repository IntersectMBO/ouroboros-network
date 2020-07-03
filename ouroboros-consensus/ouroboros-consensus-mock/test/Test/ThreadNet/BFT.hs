{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.ThreadNet.BFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
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
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Consensus.Ledger.Mock.Generators ()

import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random
import           Test.Util.Serialisation
import           Test.Util.Slots (NumSlots (..))

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

    , testProperty "delayed message corner case" $
        once $
        let ncn = NumCoreNodes 2 in
        prop_simple_bft_convergence TestSetup
          { setupK = SecurityParam 3
          , setupTestConfig = TestConfig
            { numCoreNodes = ncn
            , numSlots = NumSlots 3
            , nodeTopology = meshNodeTopology ncn
            , initSeed = Seed {getSeed = (12659702313441544615,9326820694273232011,15820857683988100572,2201554969601311572,4716411940989238571)}
            }
          , setupNodeJoinPlan = NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 1})])
          }
    , testProperty "Mock.applyChainTick is not a no-op" $
        -- This repro failed on a wip branch that included a fix for Issue 1489
        -- and but not for Issue 1559. PR 1562 fixed it. We're retaining this
        -- as a regression test.
        once $
        let ncn = NumCoreNodes 3 in
        prop_simple_bft_convergence TestSetup
          { setupK = SecurityParam 5
          , setupTestConfig = TestConfig
            { numCoreNodes = ncn
            , numSlots     = NumSlots 7
            , nodeTopology = meshNodeTopology ncn
            , initSeed     = Seed (6358650144370660550,17563794202468751585,17692838336641672274,12649320068211251815,18441126729279419067)
            }
          , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0, SlotNo 0),(CoreNodeId 1, SlotNo 2),(CoreNodeId 2, SlotNo 2)]
          }
    , testProperty "simple convergence" $ \setup ->
        prop_simple_bft_convergence setup
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
      { pgaBlockProperty      = prop_validSimpleBlock
      , pgaCountTxs           = countSimpleGenTxs
      , pgaExpectedCannotLead = noExpectedCannotLeads
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength = Nothing
      , pgaFixedSchedule      =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam      = k
      , pgaTestConfig         = testConfig
      , pgaTestConfigB        = testConfigB
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
