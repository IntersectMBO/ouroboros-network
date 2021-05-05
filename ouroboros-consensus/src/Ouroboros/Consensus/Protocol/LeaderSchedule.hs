{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , leaderScheduleFor
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), fromCoreNodeId)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Leader schedule

  The leader schedule allows us to define, in tests, precisely when each node
  is meant to lead. Unlike in, say, Praos, where this is determined by a single
  random seed, this gives us the ability to construct test cases in an
  inspectable and shrinkable manner.
-------------------------------------------------------------------------------}

newtype LeaderSchedule = LeaderSchedule {
        getLeaderSchedule :: Map SlotNo [CoreNodeId]
      }
    deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NoThunks)

-- | The 'Slots' a given node is supposed to lead in
leaderScheduleFor :: CoreNodeId -> LeaderSchedule -> Set SlotNo
leaderScheduleFor nid =
      Map.keysSet
    . Map.filter (elem nid)
    . getLeaderSchedule

instance Semigroup LeaderSchedule where
    LeaderSchedule l <> LeaderSchedule r =
        LeaderSchedule $
        Map.unionWith comb l r
      where
        comb ls rs = ls ++ filter (`notElem` ls) rs

instance Condense LeaderSchedule where
    condense (LeaderSchedule m) = condense
                                $ map (\(s, ls) -> (s, map fromCoreNodeId ls))
                                $ Map.toList m
