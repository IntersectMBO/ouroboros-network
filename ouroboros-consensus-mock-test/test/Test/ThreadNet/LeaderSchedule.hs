{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Test.ThreadNet.LeaderSchedule (tests) where

import           Control.Monad (replicateM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.PraosRule
import           Ouroboros.Consensus.Mock.Protocol.Praos
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol.LeaderSchedule

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
import           Test.Util.Slots (NumSlots (..))

data TestSetup = TestSetup
  { setupK              :: SecurityParam
  , setupTestConfig     :: TestConfig
  , setupEpochSize      :: EpochSize
    -- ^ Note: we don't think this value actually matters, since this test
    -- overrides the leader schedule.
  , setupNodeJoinPlan   :: NodeJoinPlan
  , setupLeaderSchedule :: LeaderSchedule
  , setupSlotLength     :: SlotLength
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
      leaderSchedule <- genLeaderSchedule k numSlots numCoreNodes nodeJoinPlan

      pure $ TestSetup
        k
        testConfig
        epochSize
        nodeJoinPlan
        leaderSchedule
        slotLength

  -- TODO shrink

tests :: TestTree
tests = testGroup "LeaderSchedule"
    [ testProperty "simple convergence" $ \setup ->
        prop_simple_leader_schedule_convergence setup
    ]

prop_simple_leader_schedule_convergence :: TestSetup -> Property
prop_simple_leader_schedule_convergence TestSetup
  { setupK              = k
  , setupTestConfig     = testConfig
  , setupEpochSize      = epochSize
  , setupNodeJoinPlan   = nodeJoinPlan
  , setupLeaderSchedule = schedule
  , setupSlotLength     = slotLength
  } =
    counterexample (tracesToDot testOutputNodes) $
    prop_general PropGeneralArgs
      { pgaBlockProperty       = prop_validSimpleBlock
      , pgaCountTxs            = countSimpleGenTxs
      , pgaExpectedCannotForge = noExpectedCannotForges
      , pgaFirstBlockNo        = 0
      , pgaFixedMaxForkLength  = Nothing
      , pgaFixedSchedule       = Just schedule
      , pgaSecurityParam       = k
      , pgaTestConfig          = testConfig
      , pgaTestConfigB         = testConfigB
      }
      testOutput
  where
    TestConfig{numCoreNodes} = testConfig

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       = singleEraFuture slotLength epochSize
      , messageDelay = noCalcMessageDelay
      , nodeJoinPlan
      , nodeRestarts = noRestarts
      , txGenExtra   = ()
      , version      = newestVersion (Proxy @MockPraosRuleBlock)
      }

    -- this is entirely ignored because of the 'WithLeaderSchedule' combinator
    dummyF = 0.5

    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork testConfig testConfigB TestConfigMB
            { nodeInfo = \nid ->
                plainTestNodeInitialization $
                protocolInfoPraosRule
                  numCoreNodes
                  nid
                  PraosParams
                  { praosSecurityParam = k
                  , praosSlotsPerEpoch = unEpochSize epochSize
                  , praosLeaderF       = dummyF
                  }
                  (HardFork.defaultEraParams k slotLength)
                  schedule
                  emptyPraosEvolvingStake
            , mkRekeyM = Nothing
            }

{-------------------------------------------------------------------------------
  Dependent generation and shrinking of leader schedules
-------------------------------------------------------------------------------}

genLeaderSchedule :: SecurityParam
                  -> NumSlots
                  -> NumCoreNodes
                  -> NodeJoinPlan
                  -> Gen LeaderSchedule
genLeaderSchedule k (NumSlots numSlots) numCoreNodes nodeJoinPlan =
    flip suchThat (consensusExpected k nodeJoinPlan) $ do
        leaders <- replicateM (fromIntegral numSlots) $ frequency
            [ ( 4, pick 0)
            , ( 2, pick 1)
            , ( 1, pick 2)
            , ( 1, pick 3)
            ]
        return $ LeaderSchedule $ Map.fromList $ zip [0..] leaders
  where
    pick :: Int -> Gen [CoreNodeId]
    pick = go (enumCoreNodes numCoreNodes)
      where
        go :: [CoreNodeId] -> Int -> Gen [CoreNodeId]
        go []   _ = return []
        go _    0 = return []
        go nids n = do
            nid <- elements nids
            xs  <- go (filter (/= nid) nids) (n - 1)
            return $ nid : xs

_shrinkLeaderSchedule :: NumSlots -> LeaderSchedule -> [LeaderSchedule]
_shrinkLeaderSchedule (NumSlots numSlots) (LeaderSchedule m) =
    [ LeaderSchedule m'
    | slot <- [0 .. fromIntegral numSlots - 1]
    , m'   <- reduceSlot slot m
    ]
  where
    reduceSlot :: SlotNo -> Map SlotNo [CoreNodeId] -> [Map SlotNo [CoreNodeId]]
    reduceSlot s m' = [Map.insert s xs m' | xs <- reduceList $ m' Map.! s]

    reduceList :: [a] -> [[a]]
    reduceList []       = []
    reduceList [_]      = []
    reduceList (x : xs) = xs : map (x :) (reduceList xs)
