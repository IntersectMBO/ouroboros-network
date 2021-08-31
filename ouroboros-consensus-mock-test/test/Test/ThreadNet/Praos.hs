{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.Praos (tests) where

import           Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
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

import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes), enumCoreNodes)
import           Test.Util.HardFork.Future (singleEraFuture)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (unNumSlots))

data TestSetup = TestSetup
  { setupEpochSize     :: EpochSize
  , setupInitialNonce  :: Natural
    -- ^ the initial Shelley 'praosInitialEta'
    --
    -- This test varies it too ensure it explores different leader schedules.
  , setupK             :: SecurityParam
  , setupNodeJoinPlan  :: NodeJoinPlan
  , setupSlotLength    :: SlotLength
  , setupTestConfig    :: TestConfig
  , setupEvolvingStake :: PraosEvolvingStake
  }
  deriving (Show)

genEvolvingStake :: EpochSize -> TestConfig -> Gen PraosEvolvingStake
genEvolvingStake epochSize TestConfig {numSlots, numCoreNodes} = do
    chosenEpochs <- sublistOf [0..EpochNo $ max 1 maxEpochs - 1]
    let l = fromIntegral maxEpochs
    stakeDists <- replicateM l genStakeDist
    return . PraosEvolvingStake . Map.fromList $ zip chosenEpochs stakeDists
  where
    maxEpochs = unNumSlots numSlots `div` unEpochSize epochSize
    relativeStake ts nid stk = (nid, fromIntegral stk / ts)
    genStakeDist = do
      stakes <- vector (fromIntegral x) `suchThat` any (> 0) :: Gen [Amount]
      let totalStake = fromIntegral $ sum stakes
      return
        . StakeDist
        . Map.fromList
        $ zipWith (relativeStake totalStake) (enumCoreNodes numCoreNodes) stakes
    NumCoreNodes x = numCoreNodes

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
      evolvingStake  <- genEvolvingStake epochSize testConfig

      pure $ TestSetup
        epochSize
        initialNonce
        k
        nodeJoinPlan
        slotLength
        testConfig
        evolvingStake

  -- TODO shrink

tests :: TestTree
tests = testGroup "Praos"
    [ testProperty "simple convergence" $ \setup ->
        prop_simple_praos_convergence setup
    ]

prop_simple_praos_convergence :: TestSetup -> Property
prop_simple_praos_convergence TestSetup
  { setupEpochSize     = epochSize
  , setupK             = k
  , setupInitialNonce
  , setupNodeJoinPlan  = nodeJoinPlan
  , setupSlotLength    = slotLength
  , setupTestConfig    = testConfig
  , setupEvolvingStake = evolvingStake
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
                                      evolvingStake
            , mkRekeyM = Nothing
            }
