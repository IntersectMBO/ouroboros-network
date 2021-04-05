{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    ConsensusConfig (..)
  , LeaderSchedule (..)
  , WithLeaderSchedule
  , leaderScheduleFor
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), fromCoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
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

{-------------------------------------------------------------------------------
  ConsensusProtocol instance
-------------------------------------------------------------------------------}

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

data instance ConsensusConfig (WithLeaderSchedule p) = WLSConfig {
      wlsConfigSchedule :: !LeaderSchedule
    , wlsConfigP        :: !(ConsensusConfig p)
    , wlsConfigNodeId   :: !CoreNodeId
    }
  deriving (Generic)

instance ConsensusProtocol p => ConsensusProtocol (WithLeaderSchedule p) where
  type SelectView    (WithLeaderSchedule p) = SelectView p

  type ChainDepState (WithLeaderSchedule p) = ()
  type LedgerView    (WithLeaderSchedule p) = ()
  type ValidationErr (WithLeaderSchedule p) = ()
  type IsLeader      (WithLeaderSchedule p) = ()
  type ValidateView  (WithLeaderSchedule p) = ()
  type CanBeLeader   (WithLeaderSchedule p) = ()

  protocolSecurityParam = protocolSecurityParam . wlsConfigP

  checkIsLeader WLSConfig{..} () slot _ =
    case Map.lookup slot $ getLeaderSchedule wlsConfigSchedule of
        Nothing -> error $ "WithLeaderSchedule: missing slot " ++ show slot
        Just nids
            | wlsConfigNodeId `elem` nids -> Just ()
            | otherwise                   -> Nothing

  tickChainDepState     _ _ _ _ = TickedTrivial
  updateChainDepState   _ _ _ _ = return ()
  reupdateChainDepState _ _ _ _ = ()

instance ConsensusProtocol p
      => NoThunks (ConsensusConfig (WithLeaderSchedule p))
