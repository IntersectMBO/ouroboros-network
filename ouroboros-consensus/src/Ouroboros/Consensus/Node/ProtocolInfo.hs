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
protocolInfo :: Protocol blk -> ProtocolInfo blk
protocolInfo (ProtocolMockBFT nodes nid params) =
    protocolInfoBft nodes nid params

protocolInfo (ProtocolMockPraos nodes nid params) =
    protocolInfoPraos nodes nid params

protocolInfo (ProtocolLeaderSchedule nodes nid params schedule) =
    protocolInfoPraosRule nodes nid params schedule

protocolInfo (ProtocolMockPBFT nodes nid params) =
    protocolInfoMockPBFT nodes nid params

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc
