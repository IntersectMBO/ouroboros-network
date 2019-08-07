{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.LeaderSchedule (
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
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Praos (prop_all_common_prefix)
import           Test.Dynamic.Util

import           Test.Util.Range

tests :: TestTree
tests = testGroup "Dynamic chain generation"
    [ testProperty
        "simple leader schedule convergence" $
            prop
    ]
  where
    params@PraosParams{..} = defaultDemoPraosParams
    numCoreNodes = NumCoreNodes 3
    numSlots  = NumSlots $ fromEnum $
        maxRollbacks praosSecurityParam * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

    prop seed =
        forAllShrink
            (genLeaderSchedule numSlots numCoreNodes params)
            (shrinkLeaderSchedule numSlots)
            $ \schedule ->
                prop_simple_leader_schedule_convergence
                    params
                    numCoreNodes numSlots schedule seed

prop_simple_leader_schedule_convergence :: PraosParams
                                        -> NumCoreNodes
                                        -> NumSlots
                                        -> LeaderSchedule
                                        -> Seed
                                        -> Property
prop_simple_leader_schedule_convergence
  params@PraosParams{praosSecurityParam = k}
  numCoreNodes numSlots schedule seed =
    counterexample ("schedule: " <> condense schedule <> "\n" <> show longest) $
    label ("longest crowded run " <> show (crowdedRunLength longest)) $
    counterexample (tracesToDot final) $
    tabulate "shortestLength" [show (rangeK k (shortestLength final'))] $
    prop_all_common_prefix
        (maxRollbacks k)
        (Map.elems final')
  where
    longest = longestCrowdedRun schedule

    TestOutput{testOutputNodes = final} =
        runTestNetwork
            (\nid -> protocolInfo numCoreNodes nid
                 (ProtocolLeaderSchedule params schedule))
            numCoreNodes numSlots seed

    -- Without the 'NodeConfig's
    final' = snd <$> final

{-------------------------------------------------------------------------------
  Dependent generation and shrinking of leader schedules
-------------------------------------------------------------------------------}

genLeaderSchedule :: NumSlots
                  -> NumCoreNodes
                  -> PraosParams
                  -> Gen LeaderSchedule
genLeaderSchedule (NumSlots numSlots) (NumCoreNodes numCoreNodes) PraosParams{..} =
    flip suchThat notTooCrowded $ do
        leaders <- replicateM numSlots $ frequency
            [ ( 4, pick 0)
            , ( 2, pick 1)
            , ( 1, pick 2)
            , ( 1, pick 3)
            ]
        return $ LeaderSchedule $ Map.fromList $ zip [1..] leaders
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

        notTooCrowded :: LeaderSchedule -> Bool
        notTooCrowded schedule =
            crowdedRunLength (longestCrowdedRun schedule) <= maxRollbacks praosSecurityParam

shrinkLeaderSchedule :: NumSlots -> LeaderSchedule -> [LeaderSchedule]
shrinkLeaderSchedule (NumSlots numSlots) (LeaderSchedule m) =
    [ LeaderSchedule m'
    | slot <- [1 .. fromIntegral numSlots]
    , m'   <- reduceSlot slot m
    ]
  where
    reduceSlot :: SlotNo -> Map SlotNo [CoreNodeId] -> [Map SlotNo [CoreNodeId]]
    reduceSlot s m' = [Map.insert s xs m' | xs <- reduceList $ m' Map.! s]

    reduceList :: [a] -> [[a]]
    reduceList []       = []
    reduceList [_]      = []
    reduceList (x : xs) = xs : map (x :) (reduceList xs)
