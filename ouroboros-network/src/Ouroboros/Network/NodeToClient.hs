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
  , connectTo

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , withServer


  , NetworkClientSubcriptionTracers
  , NetworkSubscriptionTracers (..)
  , ClientSubscriptionParams (..)
  , ncSubscriptionWorker

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
import           Network.TypedProtocol.Codec
import           Network.Mux (WithMuxBearer (..))
import           Network.Mux.Types (MuxRuntimeError (..))

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
                  noTimeLimitsHandshake
                  (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
                  tracers
                  acceptableVersion
                  versions
                  Nothing
                  (localAddressFromPath path)


-- | A specialised version of 'Ouroboros.Network.Socket.withServerNode'.
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
    noTimeLimitsHandshake
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    acceptableVersion
    (SomeResponderApplication <$> versions)
    errPolicies
    (\_ async -> Async.wait async)

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
          noTimeLimitsHandshake
          (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
          (NetworkConnectTracers nsMuxTracer nsHandshakeTracer)
          acceptableVersion
          versions)

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
          $ \(_ :: HandshakeProtocolError NodeToClientVersion)
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

                      -- in case of bearer closed / or IOException we suspend
                      -- the peer for a short time
                      --
                      -- TODO: the same notes apply as to
                      -- 'NodeToNode.networkErrorPolicies'
                      MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                      MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)
                      MuxSDUReadTimeout       -> Just (SuspendPeer shortDelay shortDelay)
                      MuxSDUWriteTimeout      -> Just (SuspendPeer shortDelay shortDelay)

      , ErrorPolicy
          $ \(e :: MuxRuntimeError)
                -> case e of
                     ProtocolAlreadyRunning       {} -> Just ourBug
                     UnknownProtocolInternalError {} -> Just ourBug
                     MuxBlockedOnCompletionVar    {} -> Just ourBug

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
