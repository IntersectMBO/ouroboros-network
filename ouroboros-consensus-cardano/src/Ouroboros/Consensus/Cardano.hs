{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Cardano (
    -- * Supported protocols
    ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
  , ProtocolRealTPraos
    -- * Abstract over the various protocols
  , Protocol(..)
  , verifyProtocol
    -- * Data required to run a protocol
  , protocolInfo
    -- * Evidence that we can run all the supported protocols
  , runProtocol
  , module X
  ) where

import           Data.Type.Equality

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update

import           Ouroboros.Consensus.Block
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
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Node as X
import           Ouroboros.Consensus.Shelley.Protocol (TPraos,
                     TPraosStandardCrypto)
import           Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolMockBFT        = Bft BftMockCrypto
type ProtocolMockPraos      = Praos PraosMockCrypto
type ProtocolLeaderSchedule = WithLeaderSchedule (Praos PraosCryptoUnused)
type ProtocolMockPBFT       = PBft PBftMockCrypto
type ProtocolRealPBFT       = PBft PBftByronCrypto
type ProtocolRealTPraos     = TPraos TPraosStandardCrypto

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol blk p where
  -- | Run BFT against the mock ledger
  ProtocolMockBFT
    :: NumCoreNodes
    -> CoreNodeId
    -> SecurityParam
    -> BlockConfig MockBftBlock
    -> Protocol MockBftBlock ProtocolMockBFT

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> BlockConfig MockPraosBlock
    -> Protocol MockPraosBlock ProtocolMockPraos

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> BlockConfig MockPraosRuleBlock
    -> LeaderSchedule
    -> Protocol MockPraosRuleBlock ProtocolLeaderSchedule

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: PBftParams
    -> BlockConfig MockPBftBlock
    -> CoreNodeId
    -> Protocol MockPBftBlock ProtocolMockPBFT

  -- | Run PBFT against the real Byron ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol
         ByronBlock
         ProtocolRealPBFT

  -- | Run TPraos against the real Shelley ledger
  ProtocolRealTPraos
    :: ShelleyGenesis TPraosStandardCrypto
    -> ProtVer
    -> Maybe (TPraosLeaderCredentials TPraosStandardCrypto)
    -> Protocol
         (ShelleyBlock TPraosStandardCrypto)
         ProtocolRealTPraos

verifyProtocol :: Protocol blk p -> (p :~: BlockProtocol blk)
verifyProtocol ProtocolMockBFT{}        = Refl
verifyProtocol ProtocolMockPraos{}      = Refl
verifyProtocol ProtocolLeaderSchedule{} = Refl
verifyProtocol ProtocolMockPBFT{}       = Refl
verifyProtocol ProtocolRealPBFT{}       = Refl
verifyProtocol ProtocolRealTPraos{}     = Refl

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: Protocol blk p -> ProtocolInfo blk
protocolInfo (ProtocolMockBFT nodes nid params cfg) =
    protocolInfoBft nodes nid params cfg

protocolInfo (ProtocolMockPraos nodes nid params cfg) =
    protocolInfoPraos nodes nid params cfg

protocolInfo (ProtocolLeaderSchedule nodes nid params cfg schedule) =
    protocolInfoPraosRule nodes nid params cfg schedule

protocolInfo (ProtocolMockPBFT params cfg nid) =
    protocolInfoMockPBFT params cfg nid

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc

protocolInfo (ProtocolRealTPraos genesis protVer mbLeaderCredentials) =
    protocolInfoShelley genesis protVer mbLeaderCredentials

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol blk p -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol ProtocolRealPBFT{}       = Dict
runProtocol ProtocolRealTPraos{}     = Dict
