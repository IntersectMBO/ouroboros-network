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
import           Ouroboros.Consensus.Protocol

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: NumCoreNodes
             -> Protocol blk
             -> ProtocolInfo blk
protocolInfo nodes demoProtocol = case demoProtocol of
    ProtocolMockBFT        nid params           -> protocolInfoBft       nodes nid params
    ProtocolMockPraos      nid params           -> protocolInfoPraos     nodes nid params
    ProtocolLeaderSchedule nid params schedule  -> protocolInfoPraosRule nodes nid params schedule
    ProtocolMockPBFT       nid params           -> protocolInfoMockPBFT  nodes nid params
    ProtocolRealPBFT       gc mthr prv swv mplc -> protocolInfoByron     gc mthr prv swv mplc
