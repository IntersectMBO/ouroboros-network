{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

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

  -- ** DNS subscription worker
  , DnsSubscriptionTarget (..)
  , dnsSubscriptionWorker
  , dnsSubscriptionWorker_V1

  -- * Re-exports
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)

  -- ** Connection table
  , ConnectionTable
  , newConnectionTable

  -- ** Error Policies and Peer state
  , ErrorPolicies (..)
  , remoteNetworkErrorPolicy
  , localNetworkErrorPolicy
  , nullErrorPolicies
  , ErrorPolicy (..)
  , PeerStates (..)
  , newPeerStatesVar
  , cleanPeerStates
  , PeerState (..)
  , SuspendDecision (..)

  -- ** Traces
  , TraceSendRecv (..)
  , SubscriptionTrace (..)
  , DnsTrace (..)
  , ErrorPolicyTrace (..)
  , WithIPList (..)
  , WithDomainName (..)
  , WithAddr (..)
  ) where

import           Control.Concurrent.Async (Async)
import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (DiffTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm
import qualified Network.Socket as Socket

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)

import           Network.Mux.Types
import           Network.Mux.Interface
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))

import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.BlockFetch.Client (BlockFetchProtocolFailure)
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound
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
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))


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
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

nodeToNodeCodecCBORTerm :: CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToNodeVersionData -> CBOR.Term
      encodeTerm NodeToNodeVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToNodeVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t


-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
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


-- | Like 'connectTo' but specific to 'NodeToNodeV_1'.
--
connectTo_V1
  :: Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication InitiatorApp peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo_V1 muxTracer handshakeTracer peeridFn versionData application localAddr remoteAddr =
    connectTo
      muxTracer handshakeTracer
      peeridFn
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      localAddr
      remoteAddr

-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@
--
withServer
  :: ( HasResponder appType ~ True)
  => Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  -> ConnectionTable IO Socket.SockAddr
  -> StrictTVar IO (PeerStates IO Socket.SockAddr)
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToNodeVersion DictVersion (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString a b)
  -> ErrorPolicies Socket.SockAddr ()
  -> (Async () -> IO t)
  -> IO t
withServer muxTracer handshakeTracer errorPolicyTracer tbl stVar addr peeridFn acceptVersion versions errPolicies k =
  withServerNode
    muxTracer
    handshakeTracer
    errorPolicyTracer
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


-- | Like 'withServer' but specific to 'NodeToNodeV_1'.
--
withServer_V1
  :: ( HasResponder appType ~ True )
  => Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  -> ConnectionTable IO Socket.SockAddr
  -> StrictTVar IO (PeerStates IO Socket.SockAddr)
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToNodeVersionData
  -> (OuroborosApplication appType peerid NodeToNodeProtocols IO BL.ByteString x y)
  -> ErrorPolicies Socket.SockAddr ()
  -> (Async () -> IO t)
  -> IO t
withServer_V1 muxTracer handshakeTracer errorPolicyTracer tbl stVar addr peeridFn versionData application =
    withServer
      muxTracer handshakeTracer errorPolicyTracer tbl stVar addr peeridFn
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)


-- | 'ipSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ipSubscriptionWorker
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr)
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> ErrorPolicies Socket.SockAddr ()
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
  muxTracer
  handshakeTracer
  errorPolicyTracer
  peeridFn
  tbl
  peerStatesVar
  localAddr
  connectionAttemptDelay
  errPolicies
  ips
  versions
    = Subscription.ipSubscriptionWorker
        subscriptionTracer
        errorPolicyTracer
        tbl
        peerStatesVar
        localAddr
        connectionAttemptDelay
        errPolicies
        ips
        (connectToNode'
          (\(DictVersion codec) -> encodeTerm codec)
          (\(DictVersion codec) -> decodeTerm codec)
          muxTracer
          handshakeTracer
          peeridFn
          versions)


-- | Like 'ipSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
ipSubscriptionWorker_V1
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr)
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> ErrorPolicies Socket.SockAddr ()
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
  muxTracer
  handshakeTracer
  errorPolicyTracer
  peeridFn
  tbl
  peerStatesVar
  localAddresses
  connectionAttemptDelay
  errPolicies
  ips
  versionData
  application
    = ipSubscriptionWorker
        subscriptionTracer
        muxTracer
        handshakeTracer
        errorPolicyTracer
        peeridFn
        tbl
        peerStatesVar
        localAddresses
        connectionAttemptDelay
        errPolicies
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
       ( HasInitiator appType ~ True )
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToNodeVersion CBOR.Term)
          peerid
          (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr)
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> ErrorPolicies Socket.SockAddr ()
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
  muxTracer
  handshakeTracer
  errorPolicyTracer
  peeridFn
  tbl
  peerStatesVar
  localAddresses
  connectionAttemptDelay
  errPolicies
  dst
  versions =
    Subscription.dnsSubscriptionWorker
      subscriptionTracer
      dnsTracer
      errorPolicyTracer
      tbl
      peerStatesVar
      localAddresses
      connectionAttemptDelay
      errPolicies
      dst
      (connectToNode'
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        muxTracer
        handshakeTracer
        peeridFn
        versions)


-- | Like 'dnsSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
dnsSubscriptionWorker_V1
    :: forall appType peerid x y void.
       ( HasInitiator appType ~ True )
    => Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToNodeProtocols))
    -> Tracer IO (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr)
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> ErrorPolicies Socket.SockAddr ()
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
  muxTracer
  handshakeTracer
  errorPolicyTracer
  peeridFn
  tbl
  peerStatesVar
  localAddresses
  connectionAttemptDelay
  errPolicies
  dst
  versionData
  application =
     dnsSubscriptionWorker
      subscriptionTracer
      dnsTracer
      muxTracer
      handshakeTracer
      errorPolicyTracer
      peeridFn
      tbl
      peerStatesVar
      localAddresses
      connectionAttemptDelay
      errPolicies
      dst
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)

-- | A minimal error policy for remote peers, which only handles exceptions
-- raised by `ouroboros-network`.
--
remoteNetworkErrorPolicy :: ErrorPolicies Socket.SockAddr a
remoteNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- Handshake client protocol error: we either did not recognise received
          -- version or we refused it.  This is only for outbound connections,
          -- thus we suspend the consumer.
          ErrorPolicy
            $ \(_ :: HandshakeClientProtocolError NodeToNodeVersion)
                  -> Just misconfiguredPeer
        
          -- exception thrown by `runDecoderWithByteLimit`
        , ErrorPolicy
            $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                   -> Just theyBuggyOrEvil

          -- deserialisation failure; this means that the remote peer is either
          -- buggy, adversarial, or the connection return garbage.  In the last
          -- case it's also good to shutdown both the consumer and the
          -- producer, as it's likely that the other side of the connection
          -- will return grabage as well.
        , ErrorPolicy
            $ \(_ :: DeserialiseFailure)
                  -> Just theyBuggyOrEvil

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
            $ \(e :: MuxError)
                  -> case errorType e of
                        MuxUnknownMiniProtocol  -> Just theyBuggyOrEvil
                        MuxDecodeError          -> Just theyBuggyOrEvil
                        MuxIngressQueueOverRun  -> Just theyBuggyOrEvil
                        MuxControlProtocolError -> Just theyBuggyOrEvil
                        MuxTooLargeMessage      -> Just theyBuggyOrEvil

                        -- MuxBearerClosed && IOExcepctions (during read
                        -- & write operations) can happen in two cases:
                        --   network error
                        --   the remote peer closed the connection
                        -- In the first case:
                        --   It would be nice to implement exponential backoff,
                        --   and this could be done in subscription worker.
                        -- In the second case: there are two cases:
                        --   the error was thrown in a responder thread:
                        --     It might be that the peer is not interested in our
                        --     chain, in which case the local node should not
                        --     penalise the remote peer. 'SuspendConsumer' (and
                        --     thus 'SuspendPeer') will actually hurt the local
                        --     node - it will not be able to download chain from
                        --     the remote peer.
                        --   the error was thrown in an initiator thread:
                        --    it is likely that the local node misbehaved
                        --    / presented invalid data.  The remote peer
                        --    penalised the local node for that, no action is
                        --    needed.
                        MuxBearerClosed         -> Nothing
                        MuxIOException{}        -> Nothing

          -- Error policy for TxSubmission protocol: outbound side (client role)
        , ErrorPolicy
            $ \(_ :: TxOutbound.TxSubmissionProtocolError)
                  -> Just theyBuggyOrEvil

          -- Error policy for TxSubmission protocol: inbound side (server role)
        , ErrorPolicy
            $ \(_ :: TxInbound.TxSubmissionProtocolError)
                  -> Just theyBuggyOrEvil

          -- Error policy for BlockFetch protocol: consumer side (client role)
        , ErrorPolicy
            $ \(_ :: BlockFetchProtocolFailure)
                  -> Just theyBuggyOrEvil
        ],

      -- Exception raised during connect; suspend connecting to that peer for
      -- a 'shortDelay'
      epConErrorPolicies = [
          ErrorPolicy $ \(_ :: IOException) -> Just $
            SuspendConsumer shortDelay
        ],

      epReturnCallback = \_ _ _ -> ourBug
    }
  where
    theyBuggyOrEvil :: SuspendDecision DiffTime
    theyBuggyOrEvil = SuspendPeer defaultDelay defaultDelay

    misconfiguredPeer :: SuspendDecision DiffTime
    misconfiguredPeer = SuspendConsumer defaultDelay

    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    defaultDelay :: DiffTime
    defaultDelay = 200 -- seconds

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

-- | Error policy for local clients.  This is equivalent to
-- 'nullErrorPolicies', but explicit in the errors which can be catched.
--
-- We are very permissive here, and very strict in the
-- `NodeToClient.networkErrorPolicy`.  After any failure the client will be
-- killed and not penalised by this policy.  This allows to restart the local
-- client without a delay.
--
localNetworkErrorPolicy :: ErrorPolicies Socket.SockAddr a
localNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- exception thrown by `runDecoderWithByteLimit`
          ErrorPolicy
            $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                  -> Nothing

          -- deserialisation failure
        , ErrorPolicy
            $ \(_ :: DeserialiseFailure) -> Nothing

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
          $ \(_ :: MuxError) -> Nothing
        ],

      -- The node never connects to a local client
      epConErrorPolicies = [],

      epReturnCallback = \_ _ _ -> ourBug
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw
