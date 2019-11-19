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

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo_V1
  , connectTo

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , withServer_V1
  , withServer

  , NetworkIPSubscriptionTracers (..)
  , IPSubscriptionParams
  , SubscriptionParams (..)
  , ncSubscriptionWorker
  , ncSubscriptionWorker_V1

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull

  -- * Re-exports
  , ConnectionId (..)
  , ErrorPolicies (..)
  , networkErrorPolicies
  , nullErrorPolicies
  , ErrorPolicy (..)
  , ErrorPolicyTrace (..)
  , WithAddr (..)
  , SuspendDecision (..)
  , TraceSendRecv (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)
  , IPSubscriptionTarget (..)
  , SubscriptionTrace (..)
  , WithIPList (..)
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm
import qualified Network.Socket as Socket

import           Network.Mux hiding (MiniProtocolLimits(..))
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))

import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Tracers
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionParams, SubscriptionParams (..))
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
  fromProtocolEnum ChainSyncWithBlocksPtcl = MiniProtocolNum 5
  fromProtocolEnum LocalTxSubmissionPtcl   = MiniProtocolNum 6

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
  :: NetworkConnectTracers NodeToClientProtocols NodeToClientVersion
  -> Versions NodeToClientVersion
              DictVersion
              (OuroborosApplication InitiatorApp ConnectionId NodeToClientProtocols IO BL.ByteString a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectTo = connectToNode cborTermVersionDataCodec

-- | A version of 'Ouroboros.Network.Socket.connectToNode' which connects using
-- the 'NodeToClientV_1' version of the protocol.
--
connectTo_V1
  :: NetworkConnectTracers NodeToClientProtocols NodeToClientVersion
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication InitiatorApp ConnectionId NodeToClientProtocols IO BL.ByteString a b)
  -- ^ 'OuroborosInitiatorApplication' which is run on an established connection
  -- using a multiplexer after the initial handshake protocol suceeds.
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectTo_V1 tracers versionData application =
  connectTo
    tracers
    (simpleSingletonVersions
      NodeToClientV_1
      versionData
      (DictVersion nodeToClientCodecCBORTerm)
      application)

-- | A specialised version of 'Ouroboros.Network.Socket.withServerNode'; Use
-- 'withServer_V1' instead of you would like to use 'NodeToCLientV_1' version of
-- the protocols.
--
-- Comments to 'Ouroboros.Network.NodeToNode.withServer' apply here as well.
--
withServer
  :: ( HasResponder appType ~ True )
  => NetworkServerTracers NodeToClientProtocols NodeToClientVersion
  -> NetworkMutableState
  -> Socket.AddrInfo
  -> Versions NodeToClientVersion DictVersion
              (OuroborosApplication appType ConnectionId NodeToClientProtocols IO BL.ByteString a b)
  -> ErrorPolicies Socket.SockAddr ()
  -> IO Void
withServer tracers networkState addr versions errPolicies =
  withServerNode
    tracers
    networkState
    addr
    cborTermVersionDataCodec
    (\(DictVersion _) -> acceptEq)
    versions
    errPolicies
    (\_ async -> Async.wait async)

-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' version of the protocol.
--
withServer_V1
  :: ( HasResponder appType ~ True )
  => NetworkServerTracers NodeToClientProtocols NodeToClientVersion
  -> NetworkMutableState
  -> Socket.AddrInfo
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (OuroborosApplication appType ConnectionId NodeToClientProtocols IO BL.ByteString a b)
  -- ^ applications which has the reponder side, i.e.
  -- 'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> ErrorPolicies Socket.SockAddr ()
  -> IO Void
withServer_V1 tracers networkState addr versionData application =
    withServer
      tracers networkState addr
      (simpleSingletonVersions
        NodeToClientV_1
        versionData
        (DictVersion nodeToClientCodecCBORTerm)
        application)

-- | 'ncSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ncSubscriptionWorker
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToClientProtocols NodeToClientVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> Versions
        NodeToClientVersion
        DictVersion
        (OuroborosApplication
          appType
          ConnectionId
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO Void
ncSubscriptionWorker
  NetworkIPSubscriptionTracers
    { nistSubscriptionTracer
    , nistMuxTracer
    , nistHandshakeTracer
    , nistErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions
    = Subscription.ipSubscriptionWorker
        nistSubscriptionTracer
        nistErrorPolicyTracer
        networkState
        subscriptionParams
        (connectToNode'
          cborTermVersionDataCodec
          (NetworkConnectTracers nistMuxTracer nistHandshakeTracer)
          versions)


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_1'.
--
ncSubscriptionWorker_V1
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToClientProtocols NodeToClientVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> NodeToClientVersionData
    -> (OuroborosApplication
          appType
          ConnectionId
          NodeToClientProtocols
          IO BL.ByteString x y)
    -> IO Void
ncSubscriptionWorker_V1
  tracers
  networkState
  subscriptionParams
  versionData
  application
    = ncSubscriptionWorker
        tracers
        networkState
        subscriptionParams
        (simpleSingletonVersions
          NodeToClientV_1
          versionData
          (DictVersion nodeToClientCodecCBORTerm)
          application)

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
