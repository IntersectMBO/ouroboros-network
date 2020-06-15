{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.PBFT (
    tests
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Slotting.Slot

import           Ouroboros.Network.Block (SlotNo (..), blockSlot)
import           Ouroboros.Network.MockChain.Chain (foldChain)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger.Block
import           Ouroboros.Consensus.Mock.Ledger.Block.PBFT
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.PBFT (protocolInfoMockPBFT)
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
import           Test.ThreadNet.Util.NodeTopology
import           Test.ThreadNet.Util.SimpleBlock

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Random (Seed (..))
import           Test.Util.WrappedClock (NumSlots (..))

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
    [ testProperty "Issue 1505: removeTxs must not use fast path" $
      -- See (the comments of) Issue 1505.
      let ncn5 = NumCoreNodes 5
          k    = SecurityParam 5
      in
      prop_simple_pbft_convergence TestSetup
        { setupK = k
        , setupTestConfig = TestConfig
          { initSeed     = Seed (9550173506264790139,4734409083700350196,9697926137031612922,16476814117921936461,9569412668768792610)
          , nodeTopology = meshNodeTopology ncn5
          , numCoreNodes = ncn5
          , numSlots     = NumSlots 100
          }
        , setupNodeJoinPlan = NodeJoinPlan $ Map.fromList
          [ (CoreNodeId 0, SlotNo 0)   -- 0 only leads this slot
          , (CoreNodeId 1, SlotNo 6)   -- 1 only leads this slot
          , (CoreNodeId 2, SlotNo 22)  -- 2 only leads this slot
          , (CoreNodeId 3, SlotNo 24)
          , (CoreNodeId 4, SlotNo 99)  -- irrelevant, beyond affecting pbftThreshold via numCoreNodes
          ]
        }
    , testProperty "simple convergence" $ \setup ->
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
      { pgaBlockProperty      = prop_validSimpleBlock
      , pgaCountTxs           = countSimpleGenTxs
      , pgaExpectedCannotLead = expectedCannotLead numCoreNodes
      , pgaFirstBlockNo       = 0
      , pgaFixedMaxForkLength =
          Just $ NumBlocks $ case refResult of
            Ref.Forked{} -> 1
            _            -> 0
      , pgaFixedSchedule      =
          Just $ roundRobinLeaderSchedule numCoreNodes numSlots
      , pgaSecurityParam      = k
      , pgaTestConfig         = testConfig
      , pgaTestConfigB        = testConfigB
      }
      testOutput
  where
    TestConfig{numCoreNodes, numSlots} = testConfig
    slotLength = slotLengthFromSec 1
    testConfigB = TestConfigB
      { epochSize = EpochSize $ maxRollbacks k * 10
          -- The mock ledger doesn't really care, and neither does PBFT. We
          -- stick with the common @k * 10@ size for now.
      , forgeEbbEnv = Nothing
      , nodeJoinPlan
      , slotLength
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      }

    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
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

expectedCannotLead :: NumCoreNodes
                   -> SlotNo
                   -> NodeId
                   -> WrapCannotLead Blk
                   -> Bool
expectedCannotLead _ _ _ = \case
    WrapCannotLead PBftCannotLeadThresholdExceeded{} -> True
    _                                                -> False
