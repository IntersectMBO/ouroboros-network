{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolMockBFT
  , ProtocolMockPraos
  , ProtocolLeaderSchedule
  , ProtocolMockPBFT
  , ProtocolRealPBFT
  , ProtocolRealTPraos
  , ProtocolCardano
    -- * Abstract over the various protocols
  , Protocol(..)
  , verifyProtocol
    -- * Data required to run a protocol
  , protocolInfo
    -- * Evidence that we can run all the supported protocols
  , runProtocol
  , module X

    -- * Client support for nodes running a protocol
  , ProtocolClient(..)
  , protocolClientInfo
  , runProtocolClient
  , verifyProtocolClient
  ) where

import           Data.Type.Equality

import           Cardano.Prelude (Natural)

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Chain.Update as Update
import           Cardano.Slotting.Slot (EpochNo)

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId (CoreNodeId)
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.BFT as X
import           Ouroboros.Consensus.Protocol.LeaderSchedule as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator

import           Ouroboros.Consensus.Mock.Ledger
import           Ouroboros.Consensus.Mock.Node ()
import           Ouroboros.Consensus.Mock.Node.BFT as X
import           Ouroboros.Consensus.Mock.Node.PBFT as X
import           Ouroboros.Consensus.Mock.Node.Praos as X
import           Ouroboros.Consensus.Mock.Node.PraosRule as X
import           Ouroboros.Consensus.Mock.Protocol.Praos as X

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X
import           Ouroboros.Consensus.Byron.Protocol (PBftByronCrypto)

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X
import           Ouroboros.Consensus.Shelley.Protocol (TPraos,
                     TPraosStandardCrypto)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Node

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
type ProtocolCardano        = HardForkProtocol '[ByronBlock, ShelleyBlock TPraosStandardCrypto]

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol (m :: * -> *) blk p where
  -- | Run BFT against the mock ledger
  ProtocolMockBFT
    :: NumCoreNodes
    -> CoreNodeId
    -> SecurityParam
    -> HardFork.EraParams
    -> Protocol m MockBftBlock ProtocolMockBFT

  -- | Run Praos against the mock ledger
  ProtocolMockPraos
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> HardFork.EraParams
    -> Protocol m MockPraosBlock ProtocolMockPraos

  -- | Run Praos against the mock ledger but with an explicit leader schedule
  ProtocolLeaderSchedule
    :: NumCoreNodes
    -> CoreNodeId
    -> PraosParams
    -> HardFork.EraParams
    -> LeaderSchedule
    -> Protocol m MockPraosRuleBlock ProtocolLeaderSchedule

  -- | Run PBFT against the mock ledger
  ProtocolMockPBFT
    :: PBftParams
    -> HardFork.EraParams
    -> CoreNodeId
    -> Protocol m MockPBftBlock ProtocolMockPBFT

  -- | Run PBFT against the real Byron ledger
  ProtocolRealPBFT
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
    -> Protocol m ByronBlock ProtocolRealPBFT

  -- | Run TPraos against the real Shelley ledger
  ProtocolRealTPraos
    :: ShelleyGenesis TPraosStandardCrypto
    -> ProtVer
    -> Natural -- ^ Max major protocol version
    -> Maybe (TPraosLeaderCredentials TPraosStandardCrypto)
    -> Protocol m (ShelleyBlock TPraosStandardCrypto) ProtocolRealTPraos

  -- | Run the protocols of /the/ Cardano block
  ProtocolCardano
       -- Byron
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe PBftLeaderCredentials
       -- Shelley
    -> ShelleyGenesis TPraosStandardCrypto
    -> ProtVer -- TODO unify with 'Update.ProtocolVersion' (2 vs 3 numbers)
    -> Natural -- ^ Max major protocol version
    -> Maybe (TPraosLeaderCredentials TPraosStandardCrypto)
       -- Hard fork
    -> Maybe EpochNo
       -- ^ maybe lower bound on first Shelley epoch
       --
       -- Setting this to @Just@ when a true lower bound is known may
       -- particularly improve performance of bulk syncing. For example, @Just
       -- 180@ would be sound for the Cardano mainnet, since the Byron era's
       -- immutable prefix now includes that era. We can update it over time,
       -- and set it to the precise value once the transition has actually
       -- taken place.
       --
       -- The @Nothing@ case is useful for test and possible alternative nets.
    -> HardCodedTransition
    -> Protocol m (CardanoBlock TPraosStandardCrypto) ProtocolCardano

verifyProtocol :: Protocol m blk p -> (p :~: BlockProtocol blk)
verifyProtocol ProtocolMockBFT{}        = Refl
verifyProtocol ProtocolMockPraos{}      = Refl
verifyProtocol ProtocolLeaderSchedule{} = Refl
verifyProtocol ProtocolMockPBFT{}       = Refl
verifyProtocol ProtocolRealPBFT{}       = Refl
verifyProtocol ProtocolRealTPraos{}     = Refl
verifyProtocol ProtocolCardano{}        = Refl

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: forall m blk p. IOLike m
             => Protocol m blk p -> ProtocolInfo m blk
protocolInfo (ProtocolMockBFT nodes nid k paramsEra) =
    protocolInfoBft nodes nid k paramsEra

protocolInfo (ProtocolMockPraos nodes nid paramsPraos paramsEra) =
    protocolInfoPraos nodes nid paramsPraos paramsEra

protocolInfo (ProtocolLeaderSchedule nodes nid paramsPraos paramsEra schedule) =
    protocolInfoPraosRule nodes nid paramsPraos paramsEra schedule

protocolInfo (ProtocolMockPBFT paramsPBft paramsEra nid) =
    protocolInfoMockPBFT paramsPBft paramsEra nid

protocolInfo (ProtocolRealPBFT gc mthr prv swv mplc) =
    protocolInfoByron gc mthr prv swv mplc

protocolInfo (ProtocolRealTPraos genesis protVer maxMajorPV mbLeaderCredentials) =
    protocolInfoShelley genesis maxMajorPV protVer mbLeaderCredentials

protocolInfo (ProtocolCardano
               genesisByron mthr prv swv mbLeaderCredentialsByron
               genesisShelley protVer maxMajorPV mbLeaderCredentialsShelley
               mbLowerBound hardCodedTransition) =
    protocolInfoCardano
      genesisByron mthr prv swv mbLeaderCredentialsByron
      genesisShelley protVer maxMajorPV mbLeaderCredentialsShelley
      mbLowerBound hardCodedTransition

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol m blk p -> Dict (RunNode blk)
runProtocol ProtocolMockBFT{}        = Dict
runProtocol ProtocolMockPraos{}      = Dict
runProtocol ProtocolLeaderSchedule{} = Dict
runProtocol ProtocolMockPBFT{}       = Dict
runProtocol ProtocolRealPBFT{}       = Dict
runProtocol ProtocolRealTPraos{}     = Dict
runProtocol ProtocolCardano{}        = Dict

{-------------------------------------------------------------------------------
  Client support for the protocols: what you need as a client of the node
-------------------------------------------------------------------------------}

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
data ProtocolClient blk p where
  --TODO: the mock protocols

  ProtocolClientRealPBFT
    :: EpochSlots
    -> SecurityParam
    -> ProtocolClient
         ByronBlock
         ProtocolRealPBFT

  ProtocolClientRealTPraos
    :: ProtocolClient
         (ShelleyBlock TPraosStandardCrypto)
         ProtocolRealTPraos

  ProtocolClientCardano
    :: EpochSlots
    -> SecurityParam
    -> ProtocolClient
         (CardanoBlock TPraosStandardCrypto)
         ProtocolCardano

-- | Sanity check that we have the right type combinations
verifyProtocolClient :: ProtocolClient blk p -> (p :~: BlockProtocol blk)
verifyProtocolClient ProtocolClientRealPBFT{}   = Refl
verifyProtocolClient ProtocolClientRealTPraos{} = Refl
verifyProtocolClient ProtocolClientCardano{}    = Refl

-- | Sanity check that we have the right class instances available
runProtocolClient :: ProtocolClient blk p -> Dict (RunNode blk)
runProtocolClient ProtocolClientRealPBFT{}   = Dict
runProtocolClient ProtocolClientRealTPraos{} = Dict
runProtocolClient ProtocolClientCardano{}    = Dict

-- | Data required by clients of a node running the specified protocol.
protocolClientInfo :: ProtocolClient blk p -> ProtocolClientInfo blk
protocolClientInfo (ProtocolClientRealPBFT epochSlots secParam) =
    protocolClientInfoByron epochSlots secParam

protocolClientInfo ProtocolClientRealTPraos =
    protocolClientInfoShelley

protocolClientInfo (ProtocolClientCardano epochSlots secParam) =
    protocolClientInfoCardano epochSlots secParam
