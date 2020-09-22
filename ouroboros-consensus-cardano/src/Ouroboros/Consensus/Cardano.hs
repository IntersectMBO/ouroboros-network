{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Cardano (
    -- * The block type of the Cardano block chain
    CardanoBlock
    -- * Supported protocols
  , ProtocolByron
  , ProtocolShelley
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

import           Data.Kind (Type)
import           Data.Type.Equality

import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochSlots)
import qualified Cardano.Chain.Update as Update

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Unary

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto,
                     StandardShelley)

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC
import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Cardano.ShelleyHFC

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron          = HardForkProtocol '[ByronBlock]
type ProtocolShelley        = HardForkProtocol '[ShelleyBlock StandardShelley]
type ProtocolCardano        = HardForkProtocol '[ByronBlock, ShelleyBlock StandardShelley]

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol (m :: Type -> Type) blk p where
  -- | Run PBFT against the real Byron ledger
  ProtocolByron
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe ByronLeaderCredentials
    -> Protocol m ByronBlockHFC ProtocolByron

  -- | Run TPraos against the real Shelley ledger
  ProtocolShelley
    :: ShelleyGenesis StandardShelley
    -> Nonce
       -- ^ The initial nonce, typically derived from the hash of Genesis
       -- config JSON file.
       --
       -- WARNING: chains using different values of this parameter will be
       -- mutually incompatible.
    -> ProtVer
    -> MaxMajorProtVer
    -> Maybe (TPraosLeaderCredentials StandardShelley)
    -> Protocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley

  -- | Run the protocols of /the/ Cardano block
  ProtocolCardano
       -- Byron
    :: Genesis.Config
    -> Maybe PBftSignatureThreshold
    -> Update.ProtocolVersion
    -> Update.SoftwareVersion
    -> Maybe ByronLeaderCredentials
       -- Shelley
    -> ShelleyGenesis StandardShelley
    -> Nonce
       -- ^ The initial nonce for the Shelley era, typically derived from the
       -- hash of Shelley Genesis config JSON file.
       --
       -- WARNING: chains using different values of this parameter will be
       -- mutually incompatible.
    -> ProtVer -- TODO unify with 'Update.ProtocolVersion' (2 vs 3 numbers)
    -> MaxMajorProtVer
    -> Maybe (TPraosLeaderCredentials StandardShelley)
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
    -> TriggerHardFork -- ^ Transition from Byron to Shelley
    -> TriggerHardFork -- ^ Transition from Shelley to ShelleyMA
    -> Protocol m (CardanoBlock StandardCrypto) ProtocolCardano

verifyProtocol :: Protocol m blk p -> (p :~: BlockProtocol blk)
verifyProtocol ProtocolByron{}   = Refl
verifyProtocol ProtocolShelley{} = Refl
verifyProtocol ProtocolCardano{} = Refl

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: forall m blk p. IOLike m
             => Protocol m blk p -> ProtocolInfo m blk
protocolInfo (ProtocolByron gc mthr prv swv mplc) =
    inject $ protocolInfoByron gc mthr prv swv mplc

protocolInfo (ProtocolShelley genesis initialNonce protVer maxMajorPV mbLeaderCredentials) =
    inject $ protocolInfoShelley genesis initialNonce maxMajorPV protVer mbLeaderCredentials

protocolInfo (ProtocolCardano
               genesisByron mthr prv swv mbLeaderCredentialsByron
               genesisShelley initialNonce protVer maxMajorPV mbLeaderCredentialsShelley
               mbLowerBound byronTransition shelleyTransition) =
    protocolInfoCardano
      genesisByron mthr prv swv mbLeaderCredentialsByron
      genesisShelley initialNonce protVer maxMajorPV mbLeaderCredentialsShelley
      mbLowerBound byronTransition shelleyTransition

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol m blk p -> Dict (RunNode blk)
runProtocol ProtocolByron{}   = Dict
runProtocol ProtocolShelley{} = Dict
runProtocol ProtocolCardano{} = Dict

{-------------------------------------------------------------------------------
  Client support for the protocols: what you need as a client of the node
-------------------------------------------------------------------------------}

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
data ProtocolClient blk p where
  ProtocolClientByron
    :: EpochSlots
    -> ProtocolClient
         ByronBlockHFC
         ProtocolByron

  ProtocolClientShelley
    :: ProtocolClient
         (ShelleyBlockHFC StandardShelley)
         ProtocolShelley

  ProtocolClientCardano
    :: EpochSlots
    -> ProtocolClient
         (CardanoBlock StandardCrypto)
         ProtocolCardano

-- | Sanity check that we have the right type combinations
verifyProtocolClient :: ProtocolClient blk p -> (p :~: BlockProtocol blk)
verifyProtocolClient ProtocolClientByron{}   = Refl
verifyProtocolClient ProtocolClientShelley{} = Refl
verifyProtocolClient ProtocolClientCardano{} = Refl

-- | Sanity check that we have the right class instances available
runProtocolClient :: ProtocolClient blk p -> Dict (RunNode blk)
runProtocolClient ProtocolClientByron{}   = Dict
runProtocolClient ProtocolClientShelley{} = Dict
runProtocolClient ProtocolClientCardano{} = Dict

-- | Data required by clients of a node running the specified protocol.
protocolClientInfo :: ProtocolClient blk p -> ProtocolClientInfo blk
protocolClientInfo (ProtocolClientByron epochSlots) =
    inject $ protocolClientInfoByron epochSlots

protocolClientInfo ProtocolClientShelley =
    inject $ protocolClientInfoShelley

protocolClientInfo (ProtocolClientCardano epochSlots) =
    protocolClientInfoCardano epochSlots
