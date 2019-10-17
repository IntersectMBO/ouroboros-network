{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , WithLeaderSchedule
  , NodeConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), fromCoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (Empty)
import           Ouroboros.Consensus.Util.Condense (Condense (..))

newtype LeaderSchedule = LeaderSchedule {getLeaderSchedule :: Map SlotNo [CoreNodeId]}
    deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NoUnexpectedThunks)

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

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

instance OuroborosTag p => OuroborosTag (WithLeaderSchedule p) where

  type ChainState      (WithLeaderSchedule p) = ()
  type NodeState       (WithLeaderSchedule p) = ()
  type LedgerView      (WithLeaderSchedule p) = ()
  type ValidationErr   (WithLeaderSchedule p) = ()
  type IsLeader        (WithLeaderSchedule p) = ()
  type SupportedHeader (WithLeaderSchedule p) = Empty

  data NodeConfig (WithLeaderSchedule p) = WLSNodeConfig
    { lsNodeConfigSchedule :: !LeaderSchedule
    , lsNodeConfigP        :: !(NodeConfig p)
    , lsNodeConfigNodeId   :: !CoreNodeId
    }
    deriving (Generic)

  preferCandidate       WLSNodeConfig{..} = preferCandidate       lsNodeConfigP
  compareCandidates     WLSNodeConfig{..} = compareCandidates     lsNodeConfigP
  protocolSecurityParam WLSNodeConfig{..} = protocolSecurityParam lsNodeConfigP
  protocolNetworkMagic  WLSNodeConfig{..} = protocolNetworkMagic  lsNodeConfigP

  checkIsLeader WLSNodeConfig{..} slot _ _ = return $
    case Map.lookup slot $ getLeaderSchedule lsNodeConfigSchedule of
        Nothing                              -> Nothing
        Just nids
            | lsNodeConfigNodeId `elem` nids -> Just ()
            | otherwise                      -> Nothing

  applyChainState _ _ _ _ = return ()
  rewindChainState _ _ _  = Just ()

instance OuroborosTag p => NoUnexpectedThunks (NodeConfig (WithLeaderSchedule p))
