{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.LeaderSchedule (
    LeaderSchedule (..)
  , WithLeaderSchedule
  , Payload (..)
  , NodeConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (Slot (..))
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Node (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense (Condense (..))

newtype LeaderSchedule = LeaderSchedule {getLeaderSchedule :: Map Slot [CoreNodeId]}
    deriving (Show, Eq, Ord)

instance Condense LeaderSchedule where
    condense (LeaderSchedule m) = show
                                $ map (\(s, ls) ->
                                    (getSlot s, map (\(CoreNodeId nid) -> nid) ls))
                                $ Map.toList m

-- | Extension of protocol @p@ by a static leader schedule.
data WithLeaderSchedule p

class NoConstraint a
instance NoConstraint a

instance OuroborosTag p => OuroborosTag (WithLeaderSchedule p) where

  -- | The payload is just the id of the block creator (just for testing).
  newtype Payload (WithLeaderSchedule p) ph = WLSPayload {getWLSPayload :: CoreNodeId}
    deriving (Generic, Condense)

  type ChainState     (WithLeaderSchedule p) = ()
  type NodeState      (WithLeaderSchedule p) = ()
  type LedgerView     (WithLeaderSchedule p) = ()
  type ValidationErr  (WithLeaderSchedule p) = ()
  type IsLeader       (WithLeaderSchedule p) = ()
  type SupportedBlock (WithLeaderSchedule p) = NoConstraint

  data NodeConfig (WithLeaderSchedule p) = WLSNodeConfig
    { lsNodeConfigSchedule :: LeaderSchedule
    , lsNodeConfigP        :: NodeConfig p
    , lsNodeConfigNodeId   :: CoreNodeId
    }

  mkPayload cfg () _ph = return $ WLSPayload $ lsNodeConfigNodeId cfg

  preferCandidate       WLSNodeConfig{..} = preferCandidate       lsNodeConfigP
  compareCandidates     WLSNodeConfig{..} = compareCandidates     lsNodeConfigP
  protocolSecurityParam WLSNodeConfig{..} = protocolSecurityParam lsNodeConfigP

  checkIsLeader WLSNodeConfig{..} slot _ _ = return $
    case Map.lookup slot $ getLeaderSchedule lsNodeConfigSchedule of
        Nothing                              -> Nothing
        Just nids
            | lsNodeConfigNodeId `elem` nids -> Just ()
            | otherwise                      -> Nothing

  applyChainState _ _ _ _ = return ()

deriving instance Eq   (Payload (WithLeaderSchedule p) ph)
deriving instance Ord  (Payload (WithLeaderSchedule p) ph)
deriving instance Show (Payload (WithLeaderSchedule p) ph)

instance Serialise (Payload (WithLeaderSchedule p) ph) where
  -- use Generic instance
