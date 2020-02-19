{-# LANGUAGE GADTs #-}
module Ouroboros.Consensus.Cardano (
    -- * Supported protocols
    ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
    -- * Abstract over the various protocols
  , Protocol(..)
    -- * Data required to run a protocol
  , protocolInfo
    -- * Evidence that we can run all the supported protocols
  , runProtocol
  , module X
  ) where

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X
import           Ouroboros.Consensus.Byron.Protocol (PBftByronCrypto)
import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.BFT as X
import           Ouroboros.Consensus.Mock.Node.PBFT as X
import           Ouroboros.Consensus.Mock.Node.Praos as X
import           Ouroboros.Consensus.Mock.Node.PraosRule as X
import           Ouroboros.Consensus.Mock.Protocol.Praos as X
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.BFT as X
import           Ouroboros.Consensus.Protocol.ExtConfig as X
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Supported protocols
-------------------------------------------------------------------------------}

type ProtocolMockBFT        = Bft BftMockCrypto
type ProtocolMockPraos      = ExtConfig PraosMockCrypto AddrDist
type ProtocolLeaderSchedule = WithLeaderSchedule (Praos PraosCryptoUnused)
type ProtocolMockPBFT       = ExtConfig (PBft PBftMockCrypto) (PBftLedgerView PBftMockCrypto)
type ProtocolRealPBFT       = ExtConfig (PBft PBftByronCrypto) ByronConfig

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol blk where
  -- | Run BFT against the mock ledger
  ProtocolMockBFT
    :: NumCoreNodes
    -> CoreNodeId
    -> SecurityParam
    -> SlotLengths
    -> Protocol (SimpleBftBlock SimpleMockCrypto BftMockCrypto)

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> Protocol (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> LeaderSchedule
    -> Protocol (SimplePraosRuleBlock SimpleMockCrypto)

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: PBftParams
    -> CoreNodeId
    -> Protocol (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)

  -- | Run PBFT against the real ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol ByronBlock

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: Protocol blk -> ProtocolInfo blk
protocolInfo (ProtocolMockBFT nodes nid params slotLengths) =
    protocolInfoBft nodes nid params slotLengths

protocolInfo (ProtocolMockPraos nodes nid params) =
    protocolInfoPraos nodes nid params

protocolInfo (ProtocolLeaderSchedule nodes nid params schedule) =
    protocolInfoPraosRule nodes nid params schedule

protocolInfo (ProtocolMockPBFT params nid) =
    protocolInfoMockPBFT params nid

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol blk -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol ProtocolRealPBFT{}       = Dict
