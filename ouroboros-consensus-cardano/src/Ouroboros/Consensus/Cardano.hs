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
  , ProtocolParamsByron(..)
  , ProtocolParamsShelley(..)
  , ProtocolParamsAllegra(..)
  , ProtocolParamsMary(..)
  , ProtocolParamsTransition(..)
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

import           Cardano.Chain.Slotting (EpochSlots)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Protocol.Abstract as X
import           Ouroboros.Consensus.Protocol.PBFT as X
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary

import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Byron.Node as X

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X

import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.Cardano.ByronHFC
import           Ouroboros.Consensus.Cardano.Node
import           Ouroboros.Consensus.Cardano.ShelleyHFC

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolByron   = HardForkProtocol '[ ByronBlock ]
type ProtocolShelley = HardForkProtocol '[ ShelleyBlock StandardShelley ]
type ProtocolCardano = HardForkProtocol '[ ByronBlock
                                         , ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardAllegra
                                         , ShelleyBlock StandardMary
                                         , ShelleyBlock StandardAlonzo
                                         ]

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol (m :: Type -> Type) blk p where
  -- | Run PBFT against the Byron ledger
  ProtocolByron
    :: ProtocolParamsByron
    -> Protocol m ByronBlockHFC ProtocolByron

  -- | Run TPraos against the Shelley ledger
  ProtocolShelley
    :: ProtocolParamsShelleyBased StandardShelley
    -> ProtocolParamsShelley
    -> Protocol m (ShelleyBlockHFC StandardShelley) ProtocolShelley

  -- | Run the protocols of /the/ Cardano block
  --
  -- WARNING: only a single set of Shelley credentials is allowed when used for
  -- mainnet. Testnets allow multiple Shelley credentials.
  ProtocolCardano ::
       ProtocolParamsByron
    -> ProtocolParamsShelleyBased StandardShelley
    -> ProtocolParamsShelley
    -> ProtocolParamsAllegra
    -> ProtocolParamsMary
    -> ProtocolParamsAlonzo
    -> ProtocolParamsTransition
         ByronBlock
         (ShelleyBlock StandardShelley)
    -> ProtocolParamsTransition
         (ShelleyBlock StandardShelley)
         (ShelleyBlock StandardAllegra)
    -> ProtocolParamsTransition
         (ShelleyBlock StandardAllegra)
         (ShelleyBlock StandardMary)
    -> ProtocolParamsTransition
         (ShelleyBlock StandardMary)
         (ShelleyBlock StandardAlonzo)
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
protocolInfo (ProtocolByron params) =
    inject $ protocolInfoByron params

protocolInfo (ProtocolShelley paramsShelleyBased paramsShelley) =
    inject $ protocolInfoShelley paramsShelleyBased paramsShelley

protocolInfo (ProtocolCardano
               paramsByron
               paramsShelleyBased
               paramsShelley
               paramsAllegra
               paramsMary
               paramsAlonzo
               paramsByronShelley
               paramsShelleyAllegra
               paramsAllegraMary
               paramsMaryAlonzo) =
    protocolInfoCardano
      paramsByron
      paramsShelleyBased
      paramsShelley
      paramsAllegra
      paramsMary
      paramsAlonzo
      paramsByronShelley
      paramsShelleyAllegra
      paramsAllegraMary
      paramsMaryAlonzo

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
