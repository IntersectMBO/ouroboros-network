{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , WithLeaderSchedule
  , NodeConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (Empty)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

newtype LeaderSchedule = LeaderSchedule {getLeaderSchedule :: Map SlotNo [CoreNodeId]}
    deriving (Show, Eq, Ord)

instance Condense LeaderSchedule where
    condense (LeaderSchedule m) = show
                                $ map (\(s, ls) ->
                                    (unSlotNo s, map (\(CoreNodeId nid) -> nid) ls))
                                $ Map.toList m

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

instance OuroborosTag p => OuroborosTag (WithLeaderSchedule p) where

  type ChainState      (WithLeaderSchedule p) = ()
  type NodeState       (WithLeaderSchedule p) = ()
  type LedgerView      (WithLeaderSchedule p) = LedgerView p
  type ValidationErr   (WithLeaderSchedule p) = ()
  type IsLeader        (WithLeaderSchedule p) = ()
  type SupportedHeader (WithLeaderSchedule p) = Empty

  data NodeConfig (WithLeaderSchedule p) = WLSNodeConfig
    { lsNodeConfigSchedule :: LeaderSchedule
    , lsNodeConfigP        :: NodeConfig p
    , lsNodeConfigNodeId   :: CoreNodeId
    }

  preferCandidate       WLSNodeConfig{..} = preferCandidate       lsNodeConfigP
  compareCandidates     WLSNodeConfig{..} = compareCandidates     lsNodeConfigP
  protocolSecurityParam WLSNodeConfig{..} = protocolSecurityParam lsNodeConfigP
  protocolSlotLength    WLSNodeConfig{..} = protocolSlotLength    lsNodeConfigP

  checkIsLeader WLSNodeConfig{..} slot _ _ = return $
    case Map.lookup slot $ getLeaderSchedule lsNodeConfigSchedule of
        Nothing                              -> Nothing
        Just nids
            | lsNodeConfigNodeId `elem` nids -> Just ()
            | otherwise                      -> Nothing

  applyChainState _ _ _ _ = return ()
  rewindChainState _ _ _  = Just ()
