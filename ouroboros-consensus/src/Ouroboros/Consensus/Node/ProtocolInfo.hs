{-# LANGUAGE GADTs #-}
-- | ProtocolInfo
module Ouroboros.Consensus.Node.ProtocolInfo (
    -- * ProtocolInfo
    module X
    -- * Data required to run a protocol
  , protocolInfo
  ) where

import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Byron as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.BFT as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos as X
import           Ouroboros.Consensus.Node.ProtocolInfo.Mock.PraosRule as X
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: NumCoreNodes
             -> CoreNodeId
             -> Protocol blk
             -> ProtocolInfo blk
protocolInfo nodes nid demoProtocol = case demoProtocol of
    ProtocolMockBFT        params          -> protocolInfoBft       nodes nid params
    ProtocolMockPraos      params          -> protocolInfoPraos     nodes nid params
    ProtocolLeaderSchedule params schedule -> protocolInfoPraosRule nodes nid params schedule
    ProtocolMockPBFT       params          -> protocolInfoMockPBFT  nodes nid params
    ProtocolRealPBFT       params gc       -> protocolInfoByron     nodes nid params gc
