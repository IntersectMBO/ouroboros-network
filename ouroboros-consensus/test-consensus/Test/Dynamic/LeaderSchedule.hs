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
import           Data.Function (on)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (Slot (..))
import           Ouroboros.Network.Chain (Chain)
import           Ouroboros.Network.Node

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Praos (prop_all_common_prefix)
import           Test.Dynamic.Util

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
    numSlots  = praosK * praosSlotsPerEpoch * numEpochs
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
                    numSlots
                    numCoreNodes
                    seed
  where
    isValid :: [NodeId]
            -> [(VTime, Map NodeId (Chain (Block DemoLeaderSchedule)))]
            -> Property
    isValid nodeIds trace =
      case trace of
        [(_, final)] ->   counterexample (tracesToDot' final)
                     $    collect (shortestLength final)
                     $    Map.keys final === nodeIds
                     .&&. prop_all_common_prefix
                            (fromIntegral $ praosK params)
                            (Map.elems final)
        _otherwise   -> property False

    creator :: Block DemoLeaderSchedule -> NodeId
    creator = CoreId
            . getWLSPayload
            . headerOuroboros
            . simpleHeader

    tracesToDot' :: Map NodeId (Chain (Block DemoLeaderSchedule)) -> String
    tracesToDot' = tracesToDot creator

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
        pick :: Int -> Gen [Int]
        pick = go [0 .. numCoreNodes - 1]
          where
            go :: [Int] -> Int -> Gen [Int]
            go []   _ = return []
            go _    0 = return []
            go nids n = do
                nid <- elements nids
                xs  <- go (filter (/= nid) nids) (n - 1)
                return $ nid : xs

        notTooCrowded :: LeaderSchedule -> Bool
        notTooCrowded schedule =
            crowdedRunLength (longestCrowdedRun schedule) < fromIntegral praosK

shrinkLeaderSchedule :: NumSlots -> LeaderSchedule -> [LeaderSchedule]
shrinkLeaderSchedule (NumSlots numSlots) (LeaderSchedule m) =
    [ LeaderSchedule m'
    | slot <- [1 .. fromIntegral numSlots]
    , m'   <- reduceSlot slot m
    ]
  where
    reduceSlot :: Slot -> Map Slot [Int] -> [Map Slot [Int]]
    reduceSlot s m' = [Map.insert s xs m' | xs <- reduceList $ m' Map.! s]

    reduceList :: [a] -> [[a]]
    reduceList []       = []
    reduceList [_]      = []
    reduceList (x : xs) = xs : map (x :) (reduceList xs)

{-------------------------------------------------------------------------------
  Crowded Run - longest multi-leader section of a leader schedule
-------------------------------------------------------------------------------}

-- | Describes a sequence of slots in a leader schedule with slots with
-- more than one leader, possibly interrupted by slots without leader.
-- There can be no such sequence, but if there is, first slot and number of
-- multi-leader slots are given.
newtype CrowdedRun = CrowdedRun (Maybe (Slot, Int))
    deriving (Show, Eq)

crowdedRunLength :: CrowdedRun -> Int
crowdedRunLength (CrowdedRun m) = maybe 0 snd m

instance Ord CrowdedRun where
    compare = compare `on` crowdedRunLength

noRun :: CrowdedRun
noRun = CrowdedRun Nothing

incCrowdedRun :: Slot -> CrowdedRun -> CrowdedRun
incCrowdedRun slot (CrowdedRun Nothing)          = CrowdedRun (Just (slot, 1))
incCrowdedRun _    (CrowdedRun (Just (slot, n))) = CrowdedRun (Just (slot, n + 1))

longestCrowdedRun :: LeaderSchedule -> CrowdedRun
longestCrowdedRun (LeaderSchedule m) = fst
                                     $ foldl' go (noRun, noRun)
                                     $ Map.toList
                                     $ fmap length m
  where
    go :: (CrowdedRun, CrowdedRun) -> (Slot, Int) -> (CrowdedRun, CrowdedRun)
    go (x, y) (slot, n)
        | n == 0    = (x, y)
        | n == 1    = (x, noRun)
        | otherwise = let y' = incCrowdedRun slot y in (max x y', y')
