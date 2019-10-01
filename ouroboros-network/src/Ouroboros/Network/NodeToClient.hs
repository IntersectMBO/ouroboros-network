{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

-- | This is the starting point for a module that will bring together the
-- overall node to client protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToClient (
    NodeToClientProtocols(..)
  , NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , DictVersion (..)
  , nodeToClientCodecCBORTerm

  , connectTo_V1
  , connectTo

  , withServer_V1
  , withServer

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  ) where

import           Control.Concurrent.Async (Async)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm

import           Network.Mux.Types (ProtocolEnum(..), MiniProtocolLimits (..))
import           Network.Mux.Interface

import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Control.Tracer (Tracer)


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-client protocol.
--
data NodeToClientProtocols = ChainSyncWithBlocksPtcl
                           | LocalTxSubmissionPtcl
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | These are the actual wire format protocol numbers.
--
-- These are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToClientProtocols where

  fromProtocolEnum ChainSyncWithBlocksPtcl = 5
  fromProtocolEnum LocalTxSubmissionPtcl   = 6

  toProtocolEnum 5 = Just ChainSyncWithBlocksPtcl
  toProtocolEnum 6 = Just LocalTxSubmissionPtcl
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits NodeToClientProtocols where
  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMessageSize  _ = 0xffffffff
  maximumIngressQueue _ = 0xffffffff

-- | Enumeration of node to client protocol versions.
--
data NodeToClientVersion = NodeToClientV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToClientVersion where
    encode NodeToClientV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToClientV_1
        _ -> fail "decode NodeToClientVersion: unknown tag"

-- | Version data for NodeToClient protocol v1
--
newtype NodeToClientVersionData = NodeToClientVersionData
  { networkMagic :: Word32 }
  deriving (Eq, Show, Typeable)

nodeToClientCodecCBORTerm :: CodecCBORTerm Text NodeToClientVersionData
nodeToClientCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToClientVersionData -> CBOR.Term
      encodeTerm NodeToClientVersionData { networkMagic } =
        CBOR.TInt (fromIntegral networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToClientVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToClientVersionData $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t

-- | A specialised version of 'Ouroboros.Network.Socket.connectToNode'.  It is
-- a general purpose function which can connect using any version of the
-- protocol.  This is mostly useful for future enhancements.
--
connectTo
  :: IO channel
  -> Snocket channel addr NodeToClientProtocols
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> Versions NodeToClientVersion
              DictVersion
              (OuroborosApplication InitiatorApp peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectTo createChannel sn =
  connectToNode
    createChannel sn
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)

-- | A version of 'Ouroboros.Network.Socket.connectToNode' which connects using
-- the 'NodeToClientV_1' version of the protocol.
--
connectTo_V1
  :: IO channel
  -> Snocket channel addr NodeToClientProtocols
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication InitiatorApp peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ 'OuroborosInitiatorApplication' which is run on an established connection
  -- using a multiplexer after the initial handshake protocol suceeds.
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectTo_V1 createChannel sn handshakeTracer peeridFn versionData application =
  connectTo
    createChannel sn handshakeTracer peeridFn
    (simpleSingletonVersions
      NodeToClientV_1
      versionData
      (DictVersion nodeToClientCodecCBORTerm)
      application)


-- | A specialised version of 'Ouroboros.Network.Socket.withServerNode'; Use
-- 'withServer_V1' instead of you would like to use 'NodeToCLientV_1' version of
-- the protocols.
--
withServer
  :: ( HasResponder appType ~ True
     , Ord addr
     )
  => Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO addr
  -> IO channel
  -> Snocket channel addr NodeToClientProtocols
  -> addr
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication appType peerid NodeToClientProtocols IO BL.ByteString a b)
  -> (Async () -> IO t)
  -> IO t
withServer handshakeTracer tbl createChannel sn addr peeridFn acceptVersion versions k =
  withServerNode
    handshakeTracer
    tbl
    createChannel
    sn
    addr
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)
    peeridFn
    acceptVersion
    versions
    (\_ -> k)

-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' version of the protocol.
--
-- TODO: do not leak 'Snocket' abstraction, specialise it to 'Socket's and pipes.
--
withServer_V1
  :: ( HasResponder appType ~ True
     , Ord addr
     )
  => Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO addr
  -> IO channel
  -> Snocket channel addr NodeToClientProtocols
  -> addr
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication appType peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ applications which has the reponder side, i.e.
  -- 'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> (Async () -> IO t)
  -> IO t
withServer_V1 handshakeTracer tbl createChannel sn addr peeridFn versionData application =
    withServer
      handshakeTracer tbl createChannel sn addr peeridFn 
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions
        NodeToClientV_1
        versionData
        (DictVersion nodeToClientCodecCBORTerm)
        application)
