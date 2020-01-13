{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.ThreadNet.LeaderSchedule (
    tests
  ) where

import           Control.Monad (replicateM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol

import           Test.ThreadNet.General
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "LeaderSchedule"
    [ testProperty "simple convergence" $
          forAllShrink
              (genNodeJoinPlan numCoreNodes numSlots)
              shrinkNodeJoinPlan
            $ \nodeJoinPlan ->
          forAllShrink
              (genNodeTopology numCoreNodes)
              shrinkNodeTopology
            $ \nodeTopology ->
          forAllShrink
              (genLeaderSchedule k numSlots numCoreNodes nodeJoinPlan)
              (shrinkLeaderSchedule numSlots)
            $ \schedule ->
          forAll arbitrary
            $ \initSeed ->
          prop_simple_leader_schedule_convergence params TestConfig
            { numCoreNodes
            , numSlots
            , nodeJoinPlan
            , nodeRestarts = noRestarts
            , nodeTopology
            , slotLengths
            , initSeed
            }
              schedule
    ]
  where
    params@PraosParams{praosSecurityParam = k, ..} = PraosParams
      { praosSecurityParam = SecurityParam 5
      , praosSlotsPerEpoch = 3
      , praosLeaderF       = 0.5
      , praosLifetimeKES   = 1000000
      , praosSlotLength    = slotLengthFromSec 2
      }

    numCoreNodes = NumCoreNodes 3
    numSlots     = NumSlots $ maxRollbacks k * praosSlotsPerEpoch * numEpochs
    numEpochs    = 3
    slotLengths  = singletonSlotLengths praosSlotLength

prop_simple_leader_schedule_convergence :: PraosParams
                                        -> TestConfig
                                        -> LeaderSchedule
                                        -> Property
prop_simple_leader_schedule_convergence
  params@PraosParams{praosSecurityParam}
  testConfig@TestConfig{numCoreNodes} schedule =
    counterexample (tracesToDot testOutputNodes) $
    prop_general
      praosSecurityParam
      testConfig
      (Just schedule)
      (const False)
      testOutput
  where
    testOutput@TestOutput{testOutputNodes} =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolLeaderSchedule numCoreNodes nid params schedule
            , rekeying = Nothing
            }

{-------------------------------------------------------------------------------
  Dependent generation and shrinking of leader schedules
-------------------------------------------------------------------------------}

genLeaderSchedule :: SecurityParam
                  -> NumSlots
                  -> NumCoreNodes
                  -> NodeJoinPlan
                  -> Gen LeaderSchedule
genLeaderSchedule k (NumSlots numSlots) (NumCoreNodes numCoreNodes) nodeJoinPlan =
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
    pick = go [0 .. numCoreNodes - 1]
      where
        go :: [Int] -> Int -> Gen [CoreNodeId]
        go []   _ = return []
        go _    0 = return []
        go nids n = do
            nid <- elements nids
            xs  <- go (filter (/= nid) nids) (n - 1)
            return $ CoreNodeId nid : xs

shrinkLeaderSchedule :: NumSlots -> LeaderSchedule -> [LeaderSchedule]
shrinkLeaderSchedule (NumSlots numSlots) (LeaderSchedule m) =
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
