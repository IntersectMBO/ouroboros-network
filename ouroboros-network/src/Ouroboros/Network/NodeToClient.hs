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
    nodeToClientProtocols
  , NodeToClientProtocols (..)
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


  , NetworkClientSubcriptionTracers
  , NetworkSubscriptionTracers (..)
  , ClientSubscriptionParams (..)
  , ncSubscriptionWorker
  , ncSubscriptionWorker_V1

  -- * Re-exported clients
  , chainSyncClientNull
  , localTxSubmissionClientNull

  -- * Re-exported network interface
  , IOManager (..)
  , AssociateWithIOCP
  , withIOManager
  , LocalSnocket
  , localSnocket
  , LocalAddress

  -- * Re-exports
  , ConnectionId (..)
  , LocalConnectionId
  , ErrorPolicies (..)
  , networkErrorPolicies
  , nullErrorPolicies
  , ErrorPolicy (..)
  , ErrorPolicyTrace (..)
  , WithAddr (..)
  , SuspendDecision (..)
  , TraceSendRecv (..)
  , ProtocolLimitFailure
  , Handshake
  , LocalAddresses (..)
  , SubscriptionTrace (..)
  , HandshakeTr
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (Identity (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Network.Mux (WithMuxBearer (..))

import           Ouroboros.Network.Driver (TraceSendRecv(..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Tracers
import           Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientNull)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client (localTxSubmissionClientNull)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
import qualified Ouroboros.Network.Protocol.Handshake.Version as V
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Client ( ClientSubscriptionParams (..) )
import qualified Ouroboros.Network.Subscription.Client as Subscription
import           Ouroboros.Network.Subscription.Ip (SubscriptionTrace (..))
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import           Ouroboros.Network.IOManager

-- The Handshake tracer types are simply terrible.
type HandshakeTr = WithMuxBearer (ConnectionId LocalAddress)
    (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))


-- | Recorod of node-to-client mini protocols.
--
data NodeToClientProtocols appType bytes m a b = NodeToClientProtocols {
    -- | local chain-sync mini-protocol
    --
    localChainSyncProtocol    :: RunMiniProtocol appType bytes m a b,

    -- | local tx-submission mini-protocol
    --
    localTxSubmissionProtocol :: RunMiniProtocol appType bytes m a b
  }


-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-client protocol.
--
-- This function specifies the wire format protocol numbers.
--
-- They are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
nodeToClientProtocols
  :: NodeToClientProtocols appType bytes m a b
  -> OuroborosApplication appType bytes m a b
nodeToClientProtocols NodeToClientProtocols {
                          localChainSyncProtocol,
                          localTxSubmissionProtocol
                        } =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 5,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localChainSyncProtocol
      }
    , MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 6,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localTxSubmissionProtocol
      }
    ]

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 0xffffffff
    }

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

instance Acceptable NodeToClientVersionData where
    acceptableVersion local remote | local == remote = V.Accept
                                   | otherwise =  Refuse $ T.pack $ "version data mismatch: " ++ show local
                                                    ++ " /= " ++ show remote

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
  :: LocalSnocket
  -- ^ callback constructed by 'Ouroboros.Network.IOManager.withIOManager'
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> Versions NodeToClientVersion
              DictVersion
              (ConnectionId LocalAddress ->
                 OuroborosApplication InitiatorApp BL.ByteString IO a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> FilePath
  -- ^ path of the unix socket or named pipe
  -> IO ()
connectTo snocket tracers versions path =
    connectToNode snocket
                  cborTermVersionDataCodec
                  tracers
                  versions
                  Nothing
                  (localAddressFromPath path)

-- | A version of 'Ouroboros.Network.Socket.connectToNode' which connects using
-- the 'NodeToClientV_1' version of the protocol.
--
connectTo_V1
  :: LocalSnocket
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (ConnectionId LocalAddress ->
        OuroborosApplication InitiatorApp BL.ByteString IO a b)
  -- ^ 'OuroborosInitiatorApplication' which is run on an established connection
  -- using a multiplexer after the initial handshake protocol suceeds.
  -> FilePath
  -- ^ path to unix socket or named pipe
  -> IO ()
connectTo_V1 snocket tracers versionData application =
  connectTo
    snocket
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
  :: ( HasResponder appType ~ True
     )
  => LocalSnocket
  -> NetworkServerTracers LocalAddress NodeToClientVersion
  -> NetworkMutableState LocalAddress
  -> LocalAddress
  -> Versions NodeToClientVersion DictVersion
              (ConnectionId LocalAddress ->
                 OuroborosApplication appType BL.ByteString IO a b)
  -> ErrorPolicies
  -> IO Void
withServer sn tracers networkState addr versions errPolicies =
  withServerNode
    sn
    tracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    addr
    cborTermVersionDataCodec
    (\(DictVersion _) -> acceptableVersion)
    (fmap (SomeResponderApplication .) versions)
    errPolicies
    (\_ async -> Async.wait async)

-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' version of the protocol.
--
-- TODO: do not leak 'Snocket' abstraction, specialise it to 'Socket's and pipes.
--
withServer_V1
  :: ( HasResponder appType ~ True
     )
  => LocalSnocket
  -> NetworkServerTracers LocalAddress NodeToClientVersion
  -> NetworkMutableState LocalAddress
  -> LocalAddress
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> (ConnectionId LocalAddress ->
        OuroborosApplication appType BL.ByteString IO a b)
  -- ^ applications which has the reponder side, i.e.
  -- 'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> ErrorPolicies
  -> IO Void
withServer_V1 sn tracers networkState addr versionData application =
    withServer
      sn tracers networkState addr
      (simpleSingletonVersions
        NodeToClientV_1
        versionData
        (DictVersion nodeToClientCodecCBORTerm)
        application)

type NetworkClientSubcriptionTracers
    = NetworkSubscriptionTracers Identity LocalAddress NodeToClientVersion


-- | 'ncSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ncSubscriptionWorker
    :: forall appType x y.
       ( HasInitiator appType ~ True
       )
    => LocalSnocket
    -> NetworkClientSubcriptionTracers
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams ()
    -> Versions
        NodeToClientVersion
        DictVersion
        (ConnectionId LocalAddress ->
           OuroborosApplication
             appType
             BL.ByteString IO x y)
    -> IO Void
ncSubscriptionWorker
  sn
  NetworkSubscriptionTracers
    { nsSubscriptionTracer
    , nsMuxTracer
    , nsHandshakeTracer
    , nsErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions
    = Subscription.clientSubscriptionWorker
        sn
        (Identity `contramap` nsSubscriptionTracer)
        nsErrorPolicyTracer
        networkState
        subscriptionParams
        (connectToNode'
          sn
          cborTermVersionDataCodec
          (NetworkConnectTracers nsMuxTracer nsHandshakeTracer)
          versions)


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_1'.
--
ncSubscriptionWorker_V1
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => LocalSnocket
    -> NetworkClientSubcriptionTracers
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams ()
    -> NodeToClientVersionData
    -> (ConnectionId LocalAddress ->
          OuroborosApplication
            appType
            BL.ByteString IO x y)
    -> IO Void
ncSubscriptionWorker_V1
  sn
  tracers
  networkState
  subscriptionParams
  versionData
  application
    = ncSubscriptionWorker
        sn
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
networkErrorPolicies :: ErrorPolicies
networkErrorPolicies = ErrorPolicies
    { epAppErrorPolicies = [
        -- Handshake client protocol error: we either did not recognise received
        -- version or we refused it.  This is only for outbound connections to
        -- a local node, thus we throw the exception.
        ErrorPolicy
          $ \(_ :: HandshakeClientProtocolError NodeToClientVersion)
                -> Just ourBug

        -- exception thrown by `runPeerWithLimits`
        -- trusted node send too much input
      , ErrorPolicy
          $ \(_ :: ProtocolLimitFailure)
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
                      MuxInitiatorOnly        -> Just ourBug

                      -- in case of bearer closed / or IOException we suspend
                      -- the peer for a short time
                      --
                      -- TODO: the same notes apply as to
                      -- 'NodeToNode.networkErrorPolicies'
                      MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                      MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)
                      MuxSDUReadTimeout       -> Just (SuspendPeer shortDelay shortDelay)
      ]
    , epConErrorPolicies = [
        -- If an 'IOException' is thrown by the 'connect' call we suspend the
        -- peer for 'shortDelay' and we will try to re-connect to it after that
        -- period.
        ErrorPolicy $ \(_ :: IOException) -> Just $
          SuspendPeer shortDelay shortDelay
      ]
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

type LocalConnectionId = ConnectionId LocalAddress
