{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
    NodeToNodeProtocols(..)
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DictVersion (..)
  , nodeToNodeCodecCBORTerm

  , connectTo
  , withServer

  -- * Re-exports
  , AnyResponderApp (..)
  , ErrorPolicies (..)
  , nullErrorPolicies
  , ErrorPolicy (..)
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
import qualified Network.Socket as Socket

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer)

import           Network.Mux.Types (ProtocolEnum(..), MiniProtocolLimits (..))
import           Network.Mux.Interface
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv)

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-node protocol.
--
data NodeToNodeProtocols = ChainSyncWithHeadersPtcl
                         | BlockFetchPtcl
                         | TxSubmissionPtcl
  deriving (Eq, Ord, Enum, Bounded, Show)

-- These protocol numbers end up in the wire format so it is vital that they
-- are stable, even as they are upgraded. So we use custom Enum instances here.
-- This allows us to retire old versions and add new, which may leave some
-- holes in the numbering space.

-- | These are the actual wire format protocol numbers.
--
-- The application specific protocol numbers start from 2 because of the two
-- mux built-in protocols.
--
-- These are chosen to not overlap with the node to client protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToNodeProtocols where

  fromProtocolEnum ChainSyncWithHeadersPtcl = 2
  fromProtocolEnum BlockFetchPtcl           = 3
  fromProtocolEnum TxSubmissionPtcl         = 4

  toProtocolEnum 2 = Just ChainSyncWithHeadersPtcl
  toProtocolEnum 3 = Just BlockFetchPtcl
  toProtocolEnum 4 = Just TxSubmissionPtcl
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits NodeToNodeProtocols where
  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMessageSize  _ = 0xffffffff
  maximumIngressQueue _ = 0xffffffff

-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion = NodeToNodeV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToNodeVersion where
    encode NodeToNodeV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToNodeV_1
        _ -> fail "decode NodeToNodeVersion: unknown tag"

-- | Version data for NodeToNode protocol v1
--
newtype NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic :: Word32 }
  deriving (Eq, Show, Typeable)

nodeToNodeCodecCBORTerm :: CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToNodeVersionData -> CBOR.Term
      encodeTerm NodeToNodeVersionData { networkMagic } =
        CBOR.TInt (fromIntegral networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffff = Right (NodeToNodeVersionData $ fromIntegral x)
                               | otherwise             = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t


-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> Versions NodeToNodeVersion
              DictVersion
              (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo =
  connectToNode
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)

-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@
--
withServer
  :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  -> ConnectionTable IO Socket.SockAddr
  -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToNodeVersion DictVersion (AnyResponderApp peerid NodeToNodeProtocols IO BL.ByteString)
  -> ErrorPolicies IO Socket.SockAddr ()
  -> (Async () -> IO t)
  -> IO t
withServer errTracer tbl stVar addr peeridFn acceptVersion versions errPolicies k =
  withServerNode
    errTracer
    tbl
    stVar
    addr
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)
    peeridFn
    acceptVersion
    versions
    errPolicies
    (\_ -> k)
