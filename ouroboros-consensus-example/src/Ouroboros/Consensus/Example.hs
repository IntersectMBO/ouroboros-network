{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Ouroboros.Consensus.Example (
    -- * The block type of the Cardano block chain
    ExampleBlock
    -- * Supported protocols
  , ProtocolExample
    -- * Abstract over the various protocols
  , ProtocolParamsShelley(..)
  , ProtocolParamsExample(..)
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

import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.Shelley.Node as X

import           Ouroboros.Consensus.Example.Block
import           Ouroboros.Consensus.Example.Node

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @cardano-node@.
-------------------------------------------------------------------------------}

type ProtocolExample = HardForkProtocol '[ ShelleyBlock StandardShelley
                                         , ShelleyBlock StandardExample
                                         ]

{-------------------------------------------------------------------------------
  Abstract over the various protocols
-------------------------------------------------------------------------------}

-- | Consensus protocol to use
data Protocol (m :: Type -> Type) blk p where
  -- | Run the protocols of /the/ Example block
  --
  -- WARNING: only a single set of Shelley credentials is allowed when used for
  -- mainnet. Testnets allow multiple Shelley credentials.
  ProtocolExample
    :: ProtocolParamsShelleyBased StandardShelley
    -> ProtocolParamsShelley
    -> ProtocolParamsExample
    -> ProtocolParamsTransition
         (ShelleyBlock StandardShelley)
         (ShelleyBlock StandardExample)
    -> Protocol m (ExampleBlock StandardCrypto) ProtocolExample

verifyProtocol :: Protocol m blk p -> (p :~: BlockProtocol blk)
verifyProtocol ProtocolExample{} = Refl

{-------------------------------------------------------------------------------
  Data required to run a protocol
-------------------------------------------------------------------------------}

-- | Data required to run the selected protocol
protocolInfo :: forall m blk p. IOLike m
             => Protocol m blk p -> ProtocolInfo m blk
protocolInfo (ProtocolExample
               paramsShelleyBased
               paramsShelley
               paramsExample
               paramsShelleyExample) =
    protocolInfoExample
      paramsShelleyBased
      paramsShelley
      paramsExample
      paramsShelleyExample

{-------------------------------------------------------------------------------
  Evidence that we can run all the supported protocols
-------------------------------------------------------------------------------}

runProtocol :: Protocol m blk p -> Dict (RunNode blk)
runProtocol ProtocolExample{} = Dict

{-------------------------------------------------------------------------------
  Client support for the protocols: what you need as a client of the node
-------------------------------------------------------------------------------}

-- | Node client support for each consensus protocol.
--
-- This is like 'Protocol' but for clients of the node, so with less onerous
-- requirements than to run a node.
--
data ProtocolClient blk p where
  ProtocolClientExample
    :: EpochSlots
    -> ProtocolClient
         (ExampleBlock StandardCrypto)
         ProtocolExample

-- | Sanity check that we have the right type combinations
verifyProtocolClient :: ProtocolClient blk p -> (p :~: BlockProtocol blk)
verifyProtocolClient ProtocolClientExample{} = Refl

-- | Sanity check that we have the right class instances available
runProtocolClient :: ProtocolClient blk p -> Dict (RunNode blk)
runProtocolClient ProtocolClientExample{} = Dict

-- | Data required by clients of a node running the specified protocol.
protocolClientInfo :: ProtocolClient blk p -> ProtocolClientInfo blk
protocolClientInfo (ProtocolClientExample epochSlots) =
    protocolClientInfoExample epochSlots
