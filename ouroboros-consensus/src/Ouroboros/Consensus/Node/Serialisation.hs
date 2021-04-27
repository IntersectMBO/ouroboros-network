{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Serialisation for sending things across the network.
--
-- We separate @NodeToNode@ from @NodeToClient@ to be very explicit about what
-- gets sent where.
--
-- Unlike in "Ouroboros.Consensus.Storage.Serialisation", we don't separate the
-- encoder from the decoder, because the reasons don't apply: we always need
-- both directions and we don't have access to the bytestrings that could be
-- used for the annotations (we use CBOR-in-CBOR in those cases).
module Ouroboros.Consensus.Node.Serialisation (
    SerialiseNodeToClient (..)
  , SerialiseNodeToNode (..)
  , SerialiseResult (..)
    -- * Defaults
  , defaultDecodeCBORinCBOR
  , defaultEncodeCBORinCBOR
    -- * Re-exported for convenience
  , Some (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (decode, encode))
import           Data.SOP.BasicFunctors

import           Ouroboros.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Ledger.Basics (LedgerConfig)

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

-- | Serialise a type @a@ so that it can be sent across network via a
-- node-to-node protocol.
class SerialiseNodeToNode blk a where
  encodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  decodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default, unversioned
  -- implementation using 'Serialise'

  default encodeNodeToNode
    :: Serialise a
    => CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  encodeNodeToNode _ccfg _version = encode

  default decodeNodeToNode
    :: Serialise a
    => CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a
  decodeNodeToNode _ccfg _version = decode

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

-- | Serialise a type @a@ so that it can be sent across the network via
-- node-to-client protocol.
class SerialiseNodeToClient blk a where
  encodeNodeToClient :: CodecConfig blk -> BlockNodeToClientVersion blk -> a -> Encoding
  decodeNodeToClient :: CodecConfig blk -> BlockNodeToClientVersion blk -> forall s. Decoder s a

  -- When the config is not needed, we provide a default, unversioned
  -- implementation using 'Serialise'

  default encodeNodeToClient
    :: Serialise a
    => CodecConfig blk -> BlockNodeToClientVersion blk -> a -> Encoding
  encodeNodeToClient _ccfg _version = encode

  default decodeNodeToClient
    :: Serialise a
    => CodecConfig blk -> BlockNodeToClientVersion blk -> forall s. Decoder s a
  decodeNodeToClient _ccfg _version = decode

{-------------------------------------------------------------------------------
  NodeToClient - SerialiseResult
-------------------------------------------------------------------------------}

-- | How to serialise the result of the @result@ of a query.
--
-- The @LocalStateQuery@ protocol is a node-to-client protocol, hence the
-- 'NodeToClientVersion' argument.
class SerialiseResult blk query where
  encodeResult
    :: forall result.
       CodecConfig blk
    -> BlockNodeToClientVersion blk
    -> query result
    -> result -> Encoding
  decodeResult
    :: forall result.
       CodecConfig blk
    -> BlockNodeToClientVersion blk
    -> query result
    -> forall s. Decoder s result

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

-- | Uses the 'Serialise' instance, but wraps it in CBOR-in-CBOR.
--
-- Use this for the 'SerialiseNodeToNode' and/or 'SerialiseNodeToClient'
-- instance of @blk@ and/or @'Header' blk@, which require CBOR-in-CBOR to be
-- compatible with the corresponding 'Serialised' instance.
defaultEncodeCBORinCBOR :: Serialise a => a -> Encoding
defaultEncodeCBORinCBOR = wrapCBORinCBOR encode

-- | Inverse of 'defaultEncodeCBORinCBOR'
defaultDecodeCBORinCBOR :: Serialise a => Decoder s a
defaultDecodeCBORinCBOR = unwrapCBORinCBOR (const <$> decode)

{-------------------------------------------------------------------------------
  Forwarding instances
-------------------------------------------------------------------------------}

instance SerialiseNodeToNode blk blk
      => SerialiseNodeToNode blk (I blk) where
  encodeNodeToNode cfg version (I h) =
      encodeNodeToNode cfg version h
  decodeNodeToNode cfg version =
      I <$> decodeNodeToNode cfg version

instance SerialiseNodeToClient blk blk
      => SerialiseNodeToClient blk (I blk) where
  encodeNodeToClient cfg version (I h) =
      encodeNodeToClient cfg version h
  decodeNodeToClient cfg version =
      I <$> decodeNodeToClient cfg version

instance SerialiseNodeToNode blk (GenTxId     blk)
      => SerialiseNodeToNode blk (WrapGenTxId blk) where
  encodeNodeToNode cfg version (WrapGenTxId h) =
      encodeNodeToNode cfg version h
  decodeNodeToNode cfg version =
      WrapGenTxId <$> decodeNodeToNode cfg version

instance SerialiseNodeToClient blk (ApplyTxErr     blk)
      => SerialiseNodeToClient blk (WrapApplyTxErr blk) where
  encodeNodeToClient cfg version (WrapApplyTxErr h) =
      encodeNodeToClient cfg version h
  decodeNodeToClient cfg version =
      WrapApplyTxErr <$> decodeNodeToClient cfg version

instance SerialiseNodeToClient blk (LedgerConfig     blk)
      => SerialiseNodeToClient blk (WrapLedgerConfig blk) where
  encodeNodeToClient cfg version (WrapLedgerConfig h) =
      encodeNodeToClient cfg version h
  decodeNodeToClient cfg version =
      WrapLedgerConfig <$> decodeNodeToClient cfg version
