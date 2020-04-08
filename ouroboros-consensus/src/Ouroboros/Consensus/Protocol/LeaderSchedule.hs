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
  , ConsensusConfig (..)
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.NodeId (CoreNodeId (..), fromCoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
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

data instance ConsensusConfig (WithLeaderSchedule p) = WLSConfig
  { wlsConfigSchedule :: !LeaderSchedule
  , wlsConfigP        :: !(ConsensusConfig p)
  , wlsConfigNodeId   :: !CoreNodeId
  }
  deriving (Generic)

instance ConsensusProtocol p => ConsensusProtocol (WithLeaderSchedule p) where
  type ConsensusState (WithLeaderSchedule p) = ()
  type LedgerView     (WithLeaderSchedule p) = ()
  type ValidationErr  (WithLeaderSchedule p) = ()
  type IsLeader       (WithLeaderSchedule p) = ()
  type ValidateView   (WithLeaderSchedule p) = ()
  type SelectView     (WithLeaderSchedule p) = SelectView p

  preferCandidate       WLSConfig{..} = preferCandidate       wlsConfigP
  compareCandidates     WLSConfig{..} = compareCandidates     wlsConfigP
  protocolSecurityParam WLSConfig{..} = protocolSecurityParam wlsConfigP

  checkIfCanBeLeader _ = True -- Conservative approximation

  checkIsLeader WLSConfig{..} slot _ _ = return $
    case Map.lookup slot $ getLeaderSchedule wlsConfigSchedule of
        Nothing                           -> Nothing
        Just nids
            | wlsConfigNodeId `elem` nids -> Just ()
            | otherwise                   -> Nothing

  updateConsensusState _ _ _ _ = return ()
  rewindConsensusState _ _ _  = Just ()

instance ConsensusProtocol p
      => NoUnexpectedThunks (ConsensusConfig (WithLeaderSchedule p))
