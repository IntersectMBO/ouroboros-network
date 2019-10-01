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
  , connectTo_V1

  , withServer
  , withServer_V1

  -- * Subscription Workers
  -- ** IP subscriptin worker
  , IPSubscriptionTarget (..)
  , ipSubscriptionWorker
  , ipSubscriptionWorker_V1
  , SubscriptionTrace (..)
  , WithIPList (..)
  -- ** DNS subscription worker
  , DnsSubscriptionTarget (..)
  , dnsSubscriptionWorker
  , dnsSubscriptionWorker_V1
  , DnsTrace (..)
  , WithDomainName (..)

  -- * Re-exports
  , ConnectionTable
  , newConnectionTable
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  ) where

import           Control.Concurrent.Async (Async)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (DiffTime)
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

import           Control.Monad.Class.MonadSTM

import           Network.Mux.Types (ProtocolEnum(..), MiniProtocolLimits (..))
import           Network.Mux.Interface

import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Server.ConnectionTable ( ConnectionTable
                                                          , newConnectionTable
                                                          )
import qualified Ouroboros.Network.Subscription.Ip as Subscription
import           Ouroboros.Network.Subscription.Ip ( IPSubscriptionTarget (..)
                                                   , WithIPList (..)
                                                   , SubscriptionTrace (..)
                                                   )
import qualified Ouroboros.Network.Subscription.Dns as Subscription
import           Ouroboros.Network.Subscription.Dns ( DnsSubscriptionTarget (..)
                                                    , DnsTrace (..)
                                                    , WithDomainName (..)
                                                    )
import           Ouroboros.Network.Snocket
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Control.Tracer (Tracer)


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
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToNodeVersionData $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t


-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: IO channel
  -> Snocket channel addr NodeToNodeProtocols
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> Versions NodeToNodeVersion
              DictVersion
              (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe addr
  -> addr
  -> IO ()
connectTo createChannel sn =
  connectToNode
    createChannel sn
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)


-- | Like 'connectTo' but specific to 'NodeToNodeV_1'.
--
-- TODO: do not leak 'Snocket' abstraction
--
connectTo_V1
  :: IO channel
  -> Snocket channel addr NodeToNodeProtocols
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe addr
  -> addr
  -> IO ()
connectTo_V1 createChannel sn handshakeTracer peeridFn versionData application localAddr remoteAddr =
    connectTo
      createChannel sn handshakeTracer peeridFn
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      localAddr
      remoteAddr
        

-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@
--
-- TODO: specialise to use `Socket`, node-to-node protocol is only used over
-- TCP sockets.
--
withServer
  :: ( HasResponder appType ~ True
     , Ord addr
     )
  => Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO addr
  -> IO channel
  -> Snocket channel addr NodeToNodeProtocols
  -> addr
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToNodeVersion DictVersion (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString a b)
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


-- | Like 'withServer' but specific to 'NodeToNodeV_1'.
--
-- TODO: do not leak 'Snocket' abstraction, specialise this over `Socket`.
--
withServer_V1
  :: ( HasResponder appType ~ True
     , Ord addr
     )
  => Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> ConnectionTable IO addr
  -> IO channel
  -> Snocket channel addr NodeToNodeProtocols
  -> addr
  -> (addr -> addr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString x y)
  -> (Async () -> IO t)
  -> IO t
withServer_V1 handshakeTracer tbl createChannel sn addr peeridFn versionData application k =
    withServer
      handshakeTracer tbl createChannel sn addr peeridFn
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      k


-- | 'ipSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ipSubscriptionWorker
    :: forall appType peerid void x y.
       (HasInitiator appType ~ True)
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
ipSubscriptionWorker
  subscriptionTracer
  handshakeTracer
  peeridFn
  tbl
  localIPv4 localIPv6
  connectionAttemptDelay
  ips
  versions
    = Subscription.ipSubscriptionWorker
        subscriptionTracer
        tbl
        localIPv4 localIPv6
        connectionAttemptDelay
        ips
        (\_ -> retry)
        (connectToNode'
          undefined -- TODO
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          handshakeTracer
          peeridFn
          versions)


-- | Like 'ipSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
ipSubscriptionWorker_V1
    :: forall appType peerid void x y.
       (HasInitiator appType ~ True)
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> Maybe Socket.SockAddr
    -- ^ Local IPv4 address to use, Nothing indicates don't use IPv4
    -> Maybe Socket.SockAddr
    -- ^ Local IPv6 address to use, Nothing indicates don't use IPv6
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> IPSubscriptionTarget
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
ipSubscriptionWorker_V1
  subscriptionTracer
  handshakeTracer
  peeridFn
  tbl
  localIPv4 localIPv6
  connectionAttemptDelay
  ips
  versionData
  application
    = ipSubscriptionWorker
        subscriptionTracer
        handshakeTracer
        peeridFn
        tbl
        localIPv4 localIPv6
        connectionAttemptDelay
        ips
        (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)


-- | 'dnsSubscriptionWorker' which starts given application versions on each
-- established connection.
--
dnsSubscriptionWorker
    :: forall appType peerid x y void.
       HasInitiator appType ~ True
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> DnsSubscriptionTarget
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
dnsSubscriptionWorker
  subscriptionTracer
  dnsTracer
  handshakeTracer
  peeridFn
  tbl
  localIPv4 localIPv6
  connectionAttemptDelay
  dst
  versions =
    Subscription.dnsSubscriptionWorker
      subscriptionTracer
      dnsTracer
      tbl
      localIPv4 localIPv6
      connectionAttemptDelay
      dst
      (\_ -> retry)
      (connectToNode'
        undefined -- TODO
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        handshakeTracer
        peeridFn
        versions)


-- | Like 'dnsSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
dnsSubscriptionWorker_V1
    :: forall appType peerid x y void.
       HasInitiator appType ~ True
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> Maybe Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> DnsSubscriptionTarget
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          peerid
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO void
dnsSubscriptionWorker_V1
  subscriptionTracer
  dnsTracer
  handshakeTracer
  peeridFn
  tbl
  localIPv4 localIPv6
  connectionAttemptDelay
  dst
  versionData
  application =
     dnsSubscriptionWorker 
      subscriptionTracer
      dnsTracer
      handshakeTracer
      peeridFn
      tbl
      localIPv4 localIPv6
      connectionAttemptDelay
      dst
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
