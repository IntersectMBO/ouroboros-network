module Cardano.Network.Protocol.Handshake.Codec
  ( nodeToNodeHandshakeCodec
  , nodeToClientHandshakeCodec
  , module Ouroboros.Network.Protocol.Handshake.Codec
  ) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy qualified as BL

import Network.TypedProtocol.Codec

import Ouroboros.Network.Protocol.Handshake.Codec hiding (codecHandshake)
import Ouroboros.Network.Protocol.Handshake.Codec qualified as Handshake
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)

import Cardano.Network.NodeToClient.Version (NodeToClientVersion,
           nodeToClientVersionCodec)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion,
           nodeToNodeVersionCodec)


-- | 'Handshake' codec for the @node-to-node@ protocol suite.
--
nodeToNodeHandshakeCodec :: MonadST m
                         => Codec (Handshake NodeToNodeVersion CBOR.Term)
                                  CBOR.DeserialiseFailure m BL.ByteString
nodeToNodeHandshakeCodec = Handshake.codecHandshake nodeToNodeVersionCodec


-- | 'Handshake' codec for the @node-to-client@ protocol suite.
--
nodeToClientHandshakeCodec :: MonadST m
                           => Codec (Handshake NodeToClientVersion CBOR.Term)
                                    CBOR.DeserialiseFailure m BL.ByteString
nodeToClientHandshakeCodec = Handshake.codecHandshake nodeToClientVersionCodec
