{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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

  , SubscriptionWorkerV2 (..)
  , ncSubscriptionWorker
  , ncSubscriptionWorker_V1
  , ncSubscriptionWorker_V2

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull

  -- * Re-exports
  , ErrorPolicies (..)
  , networkErrorPolicies
  , nullErrorPolicies
  , ErrorPolicy (..)
  , ErrorPolicyTrace (..)
  , WithAddr (..)
  , PeerStates (..)
  , newPeerStatesVar
  , cleanPeerStates
  , PeerState (..)
  , SuspendDecision (..)
  , newConnectionTable
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)
  , IPSubscriptionTarget (..)
  , SubscriptionTrace (..)
  , WithIPList (..)
  ) where

import           Control.Concurrent.Async (Async)
import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
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
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket
import qualified Ouroboros.Network.Subscription.Ip as Subscription
import           Ouroboros.Network.Subscription.Ip ( IPSubscriptionTarget (..)
                                                   , WithIPList (..)
                                                   , SubscriptionTrace (..)
                                                   )
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))

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
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

nodeToClientCodecCBORTerm :: CodecCBORTerm Text NodeToClientVersionData
nodeToClientCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToClientVersionData -> CBOR.Term
      encodeTerm NodeToClientVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToClientVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToClientVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t

-- | A specialised version of 'Ouroboros.Network.Socket.connectToNode'.  It is
-- a general purpose function which can connect using any version of the
-- protocol.  This is mostly useful for future enhancements.
--
connectTo
  :: Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> Versions NodeToClientVersion
              DictVersion
              (OuroborosApplication InitiatorApp peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectTo =
  connectToNode
    (\(DictVersion codec) -> encodeTerm codec)
    (\(DictVersion codec) -> decodeTerm codec)

-- | A version of 'Ouroboros.Network.Socket.connectToNode' which connects using
-- the 'NodeToClientV_1' version of the protocol.
--
connectTo_V1
  :: Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication InitiatorApp peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ 'OuroborosInitiatorApplication' which is run on an established connection
  -- using a multiplexer after the initial handshake protocol suceeds.
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectTo_V1 muxTracer handshakeTracer peeridFn versionData application =
  connectTo
    muxTracer
    handshakeTracer
    peeridFn
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
  :: ( HasResponder appType ~ True )
  => Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  -> ConnectionTable IO Socket.SockAddr
  -> StrictTVar IO (PeerStates IO Socket.SockAddr)
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> (forall vData. DictVersion vData -> vData -> vData -> Accept)
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication appType peerid NodeToClientProtocols IO BL.ByteString a b)
  -> ErrorPolicies Socket.SockAddr ()
  -> (Async () -> IO t)
  -> IO t
withServer muxTracer handshakeTracer  errorPolicyTracer tbl stVar addr peeridFn acceptVersion versions errPolicies k =
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

-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' version of the protocol.
--
withServer_V1
  :: ( HasResponder appType ~ True )
  => Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
  -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  -> ConnectionTable IO Socket.SockAddr
  -> StrictTVar IO (PeerStates IO Socket.SockAddr)
  -> Socket.AddrInfo
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -- ^ create peerid from local address and remote address
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication appType peerid NodeToClientProtocols IO BL.ByteString a b)
  -- ^ applications which has the reponder side, i.e.
  -- 'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> ErrorPolicies Socket.SockAddr ()
  -> (Async () -> IO t)
  -> IO t
withServer_V1 muxTracer handshakeTracer errorPolicyTracer tbl stVar addr peeridFn versionData application =
    withServer
      muxTracer handshakeTracer errorPolicyTracer tbl stVar addr peeridFn
      (\(DictVersion _) -> acceptEq)
      (simpleSingletonVersions
        NodeToClientV_1
        versionData
        (DictVersion nodeToClientCodecCBORTerm)
        application)

-- | 'ncSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ncSubscriptionWorker
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
    -> Tracer IO
        (TraceSendRecv
          (Handshake NodeToClientVersion CBOR.Term)
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
        NodeToClientVersion
        DictVersion
        (OuroborosApplication
          appType
          peerid
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO void
ncSubscriptionWorker
  subscriptionTracer
  muxTracer
  handshakeTracer
  errTracer
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
        errTracer
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


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_1'.
--
ncSubscriptionWorker_V1
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True )
    => Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
    -> Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr)
    -> LocalAddresses Socket.SockAddr
    -> (Socket.SockAddr -> Maybe DiffTime)
    -- ^ Lookup function, should return expected delay for the given address
    -> ErrorPolicies Socket.SockAddr ()
    -> IPSubscriptionTarget
    -> NodeToClientVersionData
    -> (OuroborosApplication
          appType
          peerid
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO void
ncSubscriptionWorker_V1
  subscriptionTracer
  muxTracer
  handshakeTracer
  errTracer
  peeridFn
  tbl
  peerStatesVar
  localAddresses
  connectionAttemptDelay
  errPolicies
  ips
  versionData
  application
    = ncSubscriptionWorker
        subscriptionTracer
        muxTracer
        handshakeTracer
        errTracer
        peeridFn
        tbl
        peerStatesVar
        localAddresses
        connectionAttemptDelay
        errPolicies
        ips
        (simpleSingletonVersions
          NodeToClientV_1
          versionData
          (DictVersion nodeToClientCodecCBORTerm)
          application)


data SubscriptionWorkerV2 appType peerid void x y = SubscriptionWorkerV2
  { sw2SubscriptionTrace :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
  , sw2MuxBearerTrace :: Tracer IO (WithMuxBearer peerid (MuxTrace NodeToClientProtocols))
  , sw2SendRecvTrace :: Tracer IO (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  , sw2AddrTrace :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
  , sw2PeerFunc :: (Socket.SockAddr -> Socket.SockAddr -> peerid)
  , sw2ConnTable :: ConnectionTable IO Socket.SockAddr
  , sw2PeerStates :: StrictTVar IO (PeerStates IO Socket.SockAddr)
  , sw2LocalAddress :: LocalAddresses Socket.SockAddr
  , sw2ConnAttemptDelay :: Socket.SockAddr -> Maybe DiffTime
  , sw2ErrorPolicies :: ErrorPolicies Socket.SockAddr ()
  , sw2SunscriptionTarget :: IPSubscriptionTarget
  , sw2VersionData :: NodeToClientVersionData
  , sw2Application :: OuroborosApplication appType peerid NodeToClientProtocols IO BL.ByteString x y
  }

ncSubscriptionWorker_V2
    :: forall appType peerid void x y.
       ( HasInitiator appType ~ True )
    => SubscriptionWorkerV2 appType peerid void x y -> IO void
ncSubscriptionWorker_V2 sw2 =
  ncSubscriptionWorker_V1
    (sw2SubscriptionTrace sw2)
    (sw2MuxBearerTrace sw2)
    (sw2SendRecvTrace sw2)
    (sw2AddrTrace sw2)
    (sw2PeerFunc sw2)
    (sw2ConnTable sw2)
    (sw2PeerStates sw2)
    (sw2LocalAddress sw2)
    (sw2ConnAttemptDelay sw2)
    (sw2ErrorPolicies sw2)
    (sw2SunscriptionTarget sw2)
    (sw2VersionData sw2)
    (sw2Application sw2)


-- | 'ErrorPolicies' for client application.  Additional rules can be added by
-- means of a 'Semigroup' instance of 'ErrorPolicies'.
--
-- This error policies will try to preserve `subscriptionWorker`, e.g. if the
-- connect function throws an `IOException` we will suspend it for
-- a 'shortDelay', and try to re-connect.
--
-- This allows to recover from a situation where a node temporarily shutsdown,
-- or running a client application which is subscribed two more than one node
-- (possibly over network).
--
-- If a trusted node sends us a wrong data or
--
networkErrorPolicies :: ErrorPolicies Socket.SockAddr a
networkErrorPolicies = ErrorPolicies
    { epAppErrorPolicies = [
        -- Handshake client protocol error: we either did not recognise received
        -- version or we refused it.  This is only for outbound connections to
        -- a local node, thus we throw the exception.
        ErrorPolicy
          $ \(_ :: HandshakeClientProtocolError NodeToClientVersion)
                -> Just ourBug

        -- exception thrown by `runDecoderWithByteLimit`
        -- trusted node send too much input
      , ErrorPolicy
          $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                -> Just ourBug

        -- deserialisation failure of a message from a trusted node
      , ErrorPolicy
          $ \(_ :: DeserialiseFailure)
                -> Just ourBug

      , ErrorPolicy
          $ \(e :: MuxError)
                -> case errorType e of
                      MuxUnknownMiniProtocol  -> Just ourBug
                      MuxDecodeError          -> Just ourBug
                      MuxIngressQueueOverRun  -> Just ourBug
                      MuxControlProtocolError -> Just ourBug
                      MuxTooLargeMessage      -> Just ourBug

                      -- in case of bearer closed / or IOException we suspend
                      -- the peer for a short time
                      --
                      -- TODO: the same notes apply as to
                      -- 'NodeToNode.networkErrorPolicies'
                      MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                      MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)
      ]
    , epConErrorPolicies = [
        -- If an 'IOException' is thrown by the 'connect' call we suspend the
        -- peer for 'shortDelay' and we will try to re-connect to it after that
        -- period.
        ErrorPolicy $ \(_ :: IOException) -> Just $
          SuspendPeer shortDelay shortDelay
      ]
    , epReturnCallback = \_ _ _ -> ourBug
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds
