{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.PBFT (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.MockChain.Chain (foldChain)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.PBFT (MockPBftBlock,
                     protocolInfoMockPBFT)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.PBFT
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense (condense)

import           Test.ThreadNet.General
import           Test.ThreadNet.Network
import qualified Test.ThreadNet.Ref.PBFT as Ref
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
  { setupK            :: SecurityParam
  , setupTestConfig   :: TestConfig
  , setupNodeJoinPlan :: NodeJoinPlan
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
      k <- SecurityParam <$> choose (1, 10)

      testConfig <- arbitrary
      let TestConfig{numCoreNodes, numSlots} = testConfig

      nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
      pure $ TestSetup k testConfig nodeJoinPlan

  -- TODO shrink

tests :: TestTree
tests = testGroup "PBFT" $
    [ testProperty "simple convergence" $ \setup ->
        prop_simple_pbft_convergence setup
    ]

prop_simple_pbft_convergence :: TestSetup -> Property
prop_simple_pbft_convergence TestSetup
  { setupK            = k
  , setupTestConfig   = testConfig
  , setupNodeJoinPlan = nodeJoinPlan
  } =
    tabulate "Ref.PBFT result" [Ref.resultConstrName refResult] $
    prop_asSimulated .&&.
    prop_general PropGeneralArgs
      { pgaBlockProperty       = prop_validSimpleBlock
      , pgaCountTxs            = countSimpleGenTxs
      , pgaExpectedCannotForge = expectedCannotForge numCoreNodes
      , pgaFirstBlockNo        = 0
      , pgaFixedMaxForkLength  =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule       =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam       = k
      , pgaTestConfig          = testConfig
      , pgaTestConfigB         = testConfigB
      }
      testOutput
  where
    TestConfig{numCoreNodes, numSlots} = testConfig
    slotLength = slotLengthFromSec 1
    testConfigB = TestConfigB
      { forgeEbbEnv = Nothing
      , future      = singleEraFuture
          slotLength
          (EpochSize $ maxRollbacks k * 10)
          -- The mock ledger doesn't really care, and neither does PBFT. We
          -- stick with the common @k * 10@ size for now.
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      , version      = newestVersion (Proxy @MockPBftBlock)
      }

    NumCoreNodes nn = numCoreNodes

    sigThd = PBftSignatureThreshold $ (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k numCoreNodes sigThd

    testOutput =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = plainTestNodeInitialization .
                            protocolInfoMockPBFT
                              params
                              (HardFork.defaultEraParams k slotLength)
            , mkRekeyM = Nothing
            }

    refResult :: Ref.Result
    refResult = Ref.simulate params nodeJoinPlan numSlots

    prop_asSimulated :: Property
    prop_asSimulated =
        counterexample ("Unexpected Nominal slots:") $
        conjoin $
        [ counterexample ("In final chain of " <> condense nid) $
          counterexample ("actual:   " <> condense actualSlots) $
          case refResult of
            Ref.Forked _ m        ->
              let expectedSlotss =
                    case Map.lookup cid m of
                      Nothing -> error "node missing from Ref.Forked"
                      Just ss -> map (:[]) $ Set.toList ss
              in
              counterexample
                ("expected: one of " <> condense expectedSlotss) $
              actualSlots `elem` expectedSlotss
            Ref.Nondeterministic  -> property True   -- TODO improve?
            Ref.Outcomes outcomes ->
              let expectedSlots =
                    [ s | (Ref.Nominal, s) <- zip outcomes [0..] ]
              in
              counterexample ("expected: " <> condense expectedSlots) $
              actualSlots == expectedSlots
        | (nid@(CoreId cid), no) <- Map.toList testOutputNodes
        , let actualSlots = actualSlotsOf no
        ]
      where
        TestOutput{testOutputNodes} = testOutput

        actualSlotsOf NodeOutput{nodeOutputFinalChain} =
            foldChain snoc id nodeOutputFinalChain [] :: [SlotNo]
          where
            snoc acc blk = acc . (blockSlot blk :)

type Blk = SimpleBlock SimpleMockCrypto
             (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

expectedCannotForge ::
     NumCoreNodes
  -> SlotNo
  -> NodeId
  -> WrapCannotForge Blk
  -> Bool
expectedCannotForge _ _ _ = \case
    WrapCannotForge PBftCannotForgeThresholdExceeded{} -> True
    _                                                  -> False
