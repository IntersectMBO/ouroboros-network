{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
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

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo_V1
  , connectTo_V2
  , connectTo

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , withServer_V1
  , withServer_V2
  , withServer


  , NetworkClientSubcriptionTracers
  , NetworkSubscriptionTracers (..)
  , ClientSubscriptionParams (..)
  , ncSubscriptionWorker
  , ncSubscriptionWorker_V1
  , ncSubscriptionWorker_V2

  -- * Null Protocol Peers
  , chainSyncPeerNull
  , localStateQueryPeerNull
  , localTxSubmissionPeerNull

  -- * Re-exported network interface
  , IOManager (..)
  , AssociateWithIOCP
  , withIOManager
  , LocalSnocket
  , localSnocket
  , LocalSocket
  , LocalAddress (..)

    -- * Versions
  , Versions (..)
  , versionedNodeToClientProtocols
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
    -- ** Codecs
  , nodeToClientHandshakeCodec
  , nodeToClientVersionCodec
  , nodeToClientCodecCBORTerm

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

import           Cardano.Prelude (FatalError)

import           Control.Exception (ErrorCall, IOException)
import qualified Control.Concurrent.Async as Async
import           Control.Monad (forever)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer

import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity (Identity (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Kind (Type)
import           Data.Void (Void)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import           Network.TypedProtocol (Peer)
import           Network.Mux (WithMuxBearer (..))

import           Ouroboros.Network.Codec
import           Ouroboros.Network.Driver (TraceSendRecv(..))
import           Ouroboros.Network.Driver.Simple (DecoderFailure)
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient.Version
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Tracers
import qualified Ouroboros.Network.Protocol.ChainSync.Type   as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type   as LocalTxSubmission
import           Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSubmission
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type   as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalStateQuery.Client as LocalStateQuery
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
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
    localTxSubmissionProtocol :: RunMiniProtocol appType bytes m a b,

    -- | local state-query mini-protocol
    --
    localStateQueryProtocol   :: RunMiniProtocol appType bytes m a b
  }


-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-client protocol.
--
-- This function specifies the wire format protocol numbers as well as the
-- protocols that run for each 'NodeToClientVersion'.
--
-- They are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
nodeToClientProtocols
  :: (ConnectionId addr -> STM m ControlMessage -> NodeToClientProtocols appType bytes m a b)
  -> NodeToClientVersion
  -> OuroborosApplication appType addr bytes m a b
nodeToClientProtocols protocols version =
    OuroborosApplication $ \connectionId controlMessageSTM ->
      case protocols connectionId controlMessageSTM of
        NodeToClientProtocols {
            localChainSyncProtocol,
            localTxSubmissionProtocol,
            localStateQueryProtocol
          } ->
          [ localChainSyncMiniProtocol localChainSyncProtocol
          , localTxSubmissionMiniProtocol localTxSubmissionProtocol
          ] <>
          [ localStateQueryMiniProtocol localStateQueryProtocol
          | case version of
              NodeToClientV_1 -> False
              _               -> True
          ]
  where
    localChainSyncMiniProtocol localChainSyncProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 5,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localChainSyncProtocol
      }
    localTxSubmissionMiniProtocol localTxSubmissionProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 6,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localTxSubmissionProtocol
      }
    localStateQueryMiniProtocol localStateQueryProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 7,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localStateQueryProtocol
      }

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 0xffffffff
    }


nodeToClientHandshakeCodec :: MonadST m
                           => Codec (Handshake NodeToClientVersion CBOR.Term)
                                    CBOR.DeserialiseFailure m BL.ByteString
nodeToClientHandshakeCodec = codecHandshake nodeToClientVersionCodec


-- | 'Versions' containing a single version of 'nodeToClientProtocols'.
--
versionedNodeToClientProtocols
    :: NodeToClientVersion
    -> NodeToClientVersionData
    -> (ConnectionId LocalAddress -> STM m ControlMessage -> NodeToClientProtocols appType bytes m a b)
    -> Versions NodeToClientVersion
                NodeToClientVersionData
                (OuroborosApplication appType LocalAddress bytes m a b)
versionedNodeToClientProtocols versionNumber versionData protocols =
    simpleSingletonVersions
      versionNumber
      versionData
      (nodeToClientProtocols protocols versionNumber)

-- | A specialised version of 'Ouroboros.Network.Socket.connectToNode'.  It is
-- a general purpose function which can connect using any version of the
-- protocol.  This is mostly useful for future enhancements.
--
connectTo
  :: LocalSnocket
  -- ^ callback constructed by 'Ouroboros.Network.IOManager.withIOManager'
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> Versions NodeToClientVersion
              NodeToClientVersionData
              (OuroborosApplication InitiatorMode LocalAddress BL.ByteString IO a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> FilePath
  -- ^ path of the unix socket or named pipe
  -> IO ()
connectTo snocket tracers versions path =
    connectToNode snocket
                  nodeToClientHandshakeCodec
                  (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                  tracers
                  acceptableVersion
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
  -> OuroborosApplication InitiatorMode LocalAddress BL.ByteString IO a b
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
      application)

{-# DEPRECATED connectTo_V1 "Use connectTo_V2" #-}


-- | A version of 'Ouroboros.Network.Socket.connectToNode' which connects using
-- the 'NodeToClientV_1' version of the protocol.
--
connectTo_V2
  :: LocalSnocket
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> OuroborosApplication InitiatorMode LocalAddress BL.ByteString IO a b
  -- ^ 'NodeToClientV_1' version of 'OuroborosInitiatorApplication' which is
  -- run on an established connection using a multiplexer after the initial
  -- handshake protocol suceeds.
  -> OuroborosApplication InitiatorMode LocalAddress BL.ByteString IO a b
  -- ^ 'NodeToClientV_2' version of 'OuroborosInitiatorApplication' which is
  -- run on an established connection using a multiplexer after the initial
  -- handshake protocol suceeds. 'NodeToClientV_2' supports 'LocalStateQuery'
  -- mini-protocol.
  -> FilePath
  -- ^ path to unix socket or named pipe
  -> IO ()
connectTo_V2 snocket tracers versionData application_V1 application_V2 =
    connectTo
      snocket
      tracers
      (
          simpleSingletonVersions
            NodeToClientV_1
            versionData
            application_V1
        <>
          simpleSingletonVersions
            NodeToClientV_2
            versionData
            application_V2
      )

-- | A specialised version of 'Ouroboros.Network.Socket.withServerNode'; Use
-- 'withServer_V1' instead of you would like to use 'NodeToCLientV_1' version of
-- the protocols.
--
-- Comments to 'Ouroboros.Network.NodeToNode.withServer' apply here as well.
--
withServer
  :: LocalSnocket
  -> NetworkServerTracers LocalAddress NodeToClientVersion
  -> NetworkMutableState LocalAddress
  -> LocalSocket
  -> Versions NodeToClientVersion
              NodeToClientVersionData
              (OuroborosApplication ResponderMode LocalAddress BL.ByteString IO a b)
  -> ErrorPolicies
  -> IO Void
withServer sn tracers networkState sd versions errPolicies =
  withServerNode'
    sn
    tracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    sd
    nodeToClientHandshakeCodec
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    acceptableVersion
    (SomeResponderApplication <$> versions)
    errPolicies
    (\_ async -> Async.wait async)

-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' version of the protocol.
--
withServer_V1
  :: LocalSnocket
  -> NetworkServerTracers LocalAddress NodeToClientVersion
  -> NetworkMutableState LocalAddress
  -> LocalSocket
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> OuroborosApplication ResponderMode LocalAddress BL.ByteString IO a b
  -- ^ applications which has the reponder side, i.e.
  -- 'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> ErrorPolicies
  -> IO Void
withServer_V1 sn tracers networkState sd versionData application =
    withServer
      sn tracers networkState sd
      (simpleSingletonVersions
        NodeToClientV_1
        versionData
        application)

{-# DEPRECATED withServer_V1 "Use withServer_V2" #-}


-- | A specialised version of 'withServer' which can only communicate using
-- 'NodeToClientV_1' or 'NodeToClientV_2' version of the protocol.
--
withServer_V2
  :: LocalSnocket
  -> NetworkServerTracers LocalAddress NodeToClientVersion
  -> NetworkMutableState LocalAddress
  -> LocalSocket
  -> NodeToClientVersionData
  -- ^ Client version data sent during initial handshake protocol.  Client and
  -- server must agree on it.
  -> OuroborosApplication ResponderMode LocalAddress BL.ByteString IO a b
  -- ^ 'NodeToClientV_1' version of applications which has the reponder side,
  -- i.e.  'OuroborosResponderApplication' or
  -- 'OuroborosInitiatorAndResponderApplication'.
  -> OuroborosApplication ResponderMode LocalAddress BL.ByteString IO a b
  -- ^ 'NodeToClientV_2' version of 'OuroborosApplication', which supports
  -- 'LocalStateQuery' mini-protocol.
  -> ErrorPolicies
  -> IO Void
withServer_V2 sn tracers networkState sd versionData application_V1 application_V2 =
    withServer
      sn tracers networkState sd
      (
          simpleSingletonVersions
            NodeToClientV_1
            versionData
            application_V1
        <>
          simpleSingletonVersions
            NodeToClientV_2
            versionData
            application_V2
      )

type NetworkClientSubcriptionTracers
    = NetworkSubscriptionTracers Identity LocalAddress NodeToClientVersion


-- | 'ncSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ncSubscriptionWorker
    :: forall mode x y.
       ( HasInitiator mode ~ True
       )
    => LocalSnocket
    -> NetworkClientSubcriptionTracers
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams ()
    -> Versions
        NodeToClientVersion
        NodeToClientVersionData
        (OuroborosApplication mode LocalAddress BL.ByteString IO x y)
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
          nodeToClientHandshakeCodec
          (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
          (NetworkConnectTracers nsMuxTracer nsHandshakeTracer)
          acceptableVersion
          versions)


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_1'.
--
ncSubscriptionWorker_V1
    :: forall mode x y.
       ( HasInitiator mode ~ True )
    => LocalSnocket
    -> NetworkClientSubcriptionTracers
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams ()
    -> NodeToClientVersionData
    -> OuroborosApplication mode LocalAddress BL.ByteString IO x y
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
          application)

{-# DEPRECATED ncSubscriptionWorker_V1 "Use ncSubscriptionWorker_V2" #-}


-- | Like 'ncSubscriptionWorker' but specific to 'NodeToClientV_2'.
--
ncSubscriptionWorker_V2
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => LocalSnocket
    -> NetworkClientSubcriptionTracers
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams ()
    -> NodeToClientVersionData
    -> OuroborosApplication appType LocalAddress BL.ByteString IO x y
    -- ^ 'NodeToClientV_1' version of 'OuroborosApplication'
    -> OuroborosApplication appType LocalAddress BL.ByteString IO x y
    -- ^ 'NodeToClientV_2' version of 'OuroboorsApplication', which supports
    -- 'LocalStateQuery' mini-protocol.
    -> IO Void
ncSubscriptionWorker_V2
  sn
  tracers
  networkState
  subscriptionParams
  versionData
  application_V1
  application_V2
    = ncSubscriptionWorker
        sn
        tracers
        networkState
        subscriptionParams
        (
            simpleSingletonVersions
              NodeToClientV_1
              versionData
              application_V1
          <>
            simpleSingletonVersions
              NodeToClientV_2
              versionData
              application_V2
        )

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
         $ \(_ :: DecoderFailure)
               -> Just ourBug

      , ErrorPolicy
          $ \(e :: MuxError)
                -> case errorType e of
                      MuxUnknownMiniProtocol       -> Just ourBug
                      MuxDecodeError               -> Just ourBug
                      MuxIngressQueueOverRun       -> Just ourBug
                      MuxInitiatorOnly             -> Just ourBug
                      MuxShutdown {}               -> Just ourBug
                      MuxCleanShutdown             -> Just ourBug
                      MuxBlockedOnCompletionVar {} -> Just ourBug

                      -- in case of bearer closed / or IOException we suspend
                      -- the peer for a short time
                      --
                      -- TODO: the same notes apply as to
                      -- 'NodeToNode.networkErrorPolicies'
                      MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                      MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)
                      MuxSDUReadTimeout       -> Just (SuspendPeer shortDelay shortDelay)
                      MuxSDUWriteTimeout      -> Just (SuspendPeer shortDelay shortDelay)

        -- Error thrown by 'IOManager', this is fatal on Windows, and it will
        -- never fire on other platofrms.
      , ErrorPolicy
          $ \(_ :: IOManagerError)
                -> Just Throw

        -- Using 'error' throws.
      , ErrorPolicy
          $ \(_ :: ErrorCall)
                -> Just Throw

        -- Using 'panic' throws.
      , ErrorPolicy
          $ \(_ :: FatalError)
                -> Just Throw
      ]
    , epConErrorPolicies = [
        -- If an 'IOException' is thrown by the 'connect' call we suspend the
        -- peer for 'shortDelay' and we will try to re-connect to it after that
        -- period.
        ErrorPolicy $ \(_ :: IOException) -> Just $
          SuspendPeer shortDelay shortDelay

      , ErrorPolicy
          $ \(_ :: IOManagerError)
                -> Just Throw
      ]
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

type LocalConnectionId = ConnectionId LocalAddress

--
-- Null Protocol Peers
--

chainSyncPeerNull
    :: forall (header :: Type) (point :: Type) (tip :: Type) m a. MonadTimer m
    => Peer (ChainSync.ChainSync header point tip)
            AsClient ChainSync.StIdle m a
chainSyncPeerNull =
    ChainSync.chainSyncClientPeer
      (ChainSync.ChainSyncClient untilTheCowsComeHome )

localStateQueryPeerNull
    :: forall (block :: Type) (point :: Type) (query :: Type -> Type) m a.
       MonadTimer m
    => Peer (LocalStateQuery.LocalStateQuery block point query)
            AsClient LocalStateQuery.StIdle m a
localStateQueryPeerNull =
    LocalStateQuery.localStateQueryClientPeer
      (LocalStateQuery.LocalStateQueryClient untilTheCowsComeHome)

localTxSubmissionPeerNull
    :: forall (tx :: Type) (reject :: Type) m a. MonadTimer m
    => Peer (LocalTxSubmission.LocalTxSubmission tx reject)
            AsClient LocalTxSubmission.StIdle m a
localTxSubmissionPeerNull =
    LocalTxSubmission.localTxSubmissionClientPeer
      (LocalTxSubmission.LocalTxSubmissionClient untilTheCowsComeHome)

-- ;)
untilTheCowsComeHome :: MonadTimer m => m a
untilTheCowsComeHome = forever $ threadDelay 43200 {- day in seconds -}
