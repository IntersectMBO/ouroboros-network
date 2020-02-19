{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Cardano (
    -- * Supported protocols
    ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
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
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
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
    -> SlotLengths
    -> Protocol
         (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
         ProtocolMockBFT

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> SlotLengths
    -> Protocol
         (SimplePraosBlock SimpleMockCrypto PraosMockCrypto)
         ProtocolMockPraos

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> SlotLengths
    -> LeaderSchedule
    -> Protocol
         (SimplePraosRuleBlock SimpleMockCrypto)
         ProtocolLeaderSchedule

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: PBftParams
    -> SlotLengths
    -> CoreNodeId
    -> Protocol
         (SimplePBftBlock SimpleMockCrypto PBftMockCrypto)
         ProtocolMockPBFT

  -- | Run PBFT against the real ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol
         ByronBlock
         ProtocolRealPBFT

verifyProtocol :: Protocol blk p -> (p :~: BlockProtocol blk)
verifyProtocol ProtocolMockBFT{}        = Refl
verifyProtocol ProtocolMockPraos{}      = Refl
verifyProtocol ProtocolLeaderSchedule{} = Refl
verifyProtocol ProtocolMockPBFT{}       = Refl
verifyProtocol ProtocolRealPBFT{}       = Refl

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: Protocol blk p -> ProtocolInfo blk
protocolInfo (ProtocolMockBFT nodes nid params slotLengths) =
    protocolInfoBft nodes nid params slotLengths

protocolInfo (ProtocolMockPraos nodes nid params slotLengths) =
    protocolInfoPraos nodes nid params slotLengths

protocolInfo (ProtocolLeaderSchedule nodes nid params slotLengths schedule) =
    protocolInfoPraosRule nodes nid params slotLengths schedule

protocolInfo (ProtocolMockPBFT params slotLengths nid) =
    protocolInfoMockPBFT params slotLengths nid

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol blk p -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol ProtocolRealPBFT{}       = Dict
