{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
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
import           Ouroboros.Network.Chain (Chain)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos
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
            prop_simple_leader_schedule_convergence
                (NumSlots (fromIntegral numSlots))
                (NumCoreNodes 3)
                params
    ]
  where
    params@PraosParams{..} = defaultDemoPraosParams
    numSlots  = maxRollbacks praosSecurityParam * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

prop_simple_leader_schedule_convergence :: NumSlots
                                        -> NumCoreNodes
                                        -> PraosParams
                                        -> Seed
                                        -> Property
prop_simple_leader_schedule_convergence numSlots numCoreNodes params seed =
    forAllShrink
        (genLeaderSchedule numSlots numCoreNodes params)
        (shrinkLeaderSchedule numSlots)
        $ \schedule ->
            let longest = longestCrowdedRun schedule
            in    counterexample ("schedule: " <> condense schedule <> "\n" <> show longest)
                $ label ("longest crowded run " <> show (crowdedRunLength longest))
                $ prop_simple_protocol_convergence
                    (protocolInfo (DemoLeaderSchedule schedule params) numCoreNodes)
                    isValid
                    numCoreNodes
                    numSlots
                    seed
  where
    isValid :: [NodeId]
            -> Map NodeId ( NodeConfig DemoLeaderSchedule
                          , Chain (SimpleBlock DemoLeaderSchedule SimpleBlockMockCrypto))
            -> Property
    isValid nodeIds final =
            counterexample (tracesToDot final)
       $    tabulate "shortestLength"
            [show (rangeK (praosSecurityParam params) (shortestLength final'))]
       $    Map.keys final === nodeIds
       .&&. prop_all_common_prefix
              (maxRollbacks $ praosSecurityParam params)
              (Map.elems final')
      where
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
