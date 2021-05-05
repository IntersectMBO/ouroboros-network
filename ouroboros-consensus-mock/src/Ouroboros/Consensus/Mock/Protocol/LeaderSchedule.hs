{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Mock.Protocol.LeaderSchedule (
    ConsensusConfig (..)
  , LeaderSchedule (..)
  , WithLeaderSchedule
  , leaderScheduleFor
  ) where

import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  ConsensusProtocol instance that overrides leader selection

  Using a provided LeaderSchedule, this instance will override the computation
  for checking leadership and just take the leader from the provided schedule.
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
