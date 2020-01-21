{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures      #-}

-- |
-- Module exports interface for running a node over a socket over TCP \/ IP.
--
module Ouroboros.Network.Socket (
    -- * High level socket interface
      NetworkMutableState (..)
    , newNetworkMutableState
    , newNetworkMutableStateSTM
    , cleanNetworkMutableState
    , ConnectionId (..)
    , withServerNode
    , connectToNode
    , connectToNode'

    -- * What to do with a connection to a peer
    -- Slightly complicated by the fact that we have initiator and responder
    -- sides to mux, some of which we may wish to disallow (note to client
    -- server doesn't allow outgoing connections, for example).
    -- TODO rename
    , ConnectionData (..)
    , SomeVersionedApplication (..)
    , RejectConnection (..)
    , withConnections
    , connection
    , incomingConnection
    , outgoingConnection

    -- * Traces
    , NetworkConnectTracers (..)
    , nullNetworkConnectTracers
    , NetworkServerTracers (..)
    , nullNetworkServerTracers

    -- * Re-export of PeerStates
    , PeerStates

    ) where

import           Control.Concurrent.Async
import           Control.Exception (IOException)
import           Control.Monad (when)
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import qualified Control.Monad.STM as STM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import           Control.Exception (throwIO)
import qualified Codec.CBOR.Term     as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Codec.Serialise (Serialise)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (traverse_)
import           Data.Int
import           Data.Void (Void)

import qualified Network.Socket as Socket

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..))

import           Control.Tracer

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
import Network.Mux.DeltaQ.TraceTransformer
import qualified Network.Mux.Types as Mx
import           Network.Mux.Types (MuxBearer)
import           Network.Mux.Interface
import qualified Network.Mux.Bearer.Socket as Mx

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.Server.ConnectionTable as CT

import           Ouroboros.Network.Connections.Concurrent hiding (Accept, Reject)
import qualified Ouroboros.Network.Connections.Concurrent as Connection
                   (Accept, Reject, Decision(Accept, Reject), concurrent)
import           Ouroboros.Network.Connections.Socket.Server (acceptLoopOn)
import qualified Ouroboros.Network.Connections.Socket.Types as Connections
import           Ouroboros.Network.Connections.Types hiding (Decision(..))

-- | Tracer used by 'connectToNode' (and derivatives, like
-- 'Ouroboros.Network.NodeToNode.connectTo' or
-- 'Ouroboros.Network.NodeToClient.connectTo).
--
data NetworkConnectTracers ptcl vNumber = NetworkConnectTracers {
      nctMuxTracer         :: Tracer IO (Mx.WithMuxBearer ConnectionId (Mx.MuxTrace ptcl)),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nctHandshakeTracer   :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                       ConnectionId
                                                       (DecoderFailureOrTooMuchInput DeserialiseFailure))
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
    }

nullNetworkConnectTracers :: NetworkConnectTracers ptcl vNumber
nullNetworkConnectTracers = NetworkConnectTracers {
      nctMuxTracer       = nullTracer,
      nctHandshakeTracer = nullTracer
    }


-- | Connection is identified by local and remote address.
--
-- TODO: the type variable which this data type fills in is called `peerid`.  We
-- should renamed to `connectionId`.
--
data ConnectionId = ConnectionId {
    localAddress  :: !Socket.SockAddr,
    remoteAddress :: !Socket.SockAddr
  }
  deriving (Eq, Ord, Show)

instance NoUnexpectedThunks ConnectionId where
    showTypeOf _ = "Peer"
    whnfNoUnexpectedThunks _ctxt _act = return NoUnexpectedThunks

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.  If the limit is exceeded, then @'TooMuchInput'@ exception will
-- be thrown.
--
maxTransmissionUnit :: Int64
maxTransmissionUnit = 4 * 1440

-- |
-- Connect to a remote node.  It is using bracket to enclose the underlying
-- socket acquisition.  This implies that when the continuation exits the
-- underlying bearer will get closed.
--
-- The connection will start with handshake protocol sending @Versions@ to the
-- remote peer.  It must fit into @'maxTransmissionUnit'@ (~5k bytes).
--
-- Exceptions thrown by @'MuxApplication'@ are rethrown by @'connectTo'@.
connectToNode
  :: forall appType ptcl vNumber vDataT a b.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     , HasInitiator appType ~ True
     )
  => VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectToNode versionDataCodec tracers versions localAddr remoteAddr =
    bracket
      (Socket.socket (Socket.addrFamily remoteAddr) Socket.Stream Socket.defaultProtocol)
      Socket.close
      (\sd -> do
          when (Socket.addrFamily remoteAddr == Socket.AF_INET ||
                Socket.addrFamily remoteAddr == Socket.AF_INET6) $ do
              Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
              Socket.setSocketOption sd Socket.ReusePort 1
#endif
          case localAddr of
            Just addr -> do
              when (Socket.addrFamily remoteAddr == Socket.AF_INET6) $
                Socket.setSocketOption sd Socket.IPv6Only 1
              Socket.bind sd (Socket.addrAddress addr)
            Nothing   -> return ()
          Socket.connect sd (Socket.addrAddress remoteAddr)
          connectToNode' versionDataCodec tracers versions sd
      )

-- |
-- Connect to a remote node using an existing socket. It is up to to caller to
-- ensure that the socket is closed in case of an exception.
--
-- The connection will start with handshake protocol sending @Versions@ to the
-- remote peer.  It must fit into @'maxTransmissionUnit'@ (~5k bytes).
--
-- Exceptions thrown by @'MuxApplication'@ are rethrown by @'connectTo'@.
connectToNode'
  :: forall appType ptcl vNumber vDataT a b.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     , HasInitiator appType ~ True
     )
  => VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Socket.Socket
  -> IO ()
connectToNode' versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions sd = do
    connectionId <- ConnectionId <$> Socket.getSocketName sd <*> Socket.getPeerName sd
    muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
    bearer <- Mx.socketAsMuxBearer muxTracer sd
    Mx.muxBearerSetState muxTracer bearer Mx.Connected
    traceWith muxTracer $ Mx.MuxTraceHandshakeStart
    ts_start <- getMonotonicTime
    !mapp <- runPeerWithByteLimit
              maxTransmissionUnit
              BL.length
              nctHandshakeTracer
              codecHandshake
              connectionId
              (Mx.muxBearerAsControlChannel bearer Mx.ModeInitiator)
              (handshakeClientPeer versionDataCodec versions)
    ts_end <- getMonotonicTime
    case mapp of
         Left err -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientError err (diffTime ts_end ts_start)
             throwIO err
         Right app -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientEnd (diffTime ts_end ts_start)
             Mx.muxStart muxTracer connectionId (toApplication app) bearer


-- |
-- Accept or reject an incoming connection.  Each record contains the new state
-- after accepting / rejecting a connection.  When accepting a connection one
-- has to give a mux application which necessarily has the server side, and
-- optionally has the client side.
--
-- TODO:
-- If the other side will not allow us to run the client side on the incoming
-- connection, the whole connection will terminate.  We might want to be more
-- admissible in this scenario: leave the server thread running and let only
-- the client thread to die.
data AcceptConnection st peerid where

    AcceptConnection
      :: !st
      -> !peerid
      -> AcceptConnection st peerid

    RejectConnection
      :: !st
      -> !peerid
      -> AcceptConnection st peerid

-- | Tracers required by a server which handles inbound connections.
--
data NetworkServerTracers ptcl vNumber = NetworkServerTracers {
      nstMuxTracer         :: Tracer IO (Mx.WithMuxBearer ConnectionId (Mx.MuxTrace ptcl)),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nstHandshakeTracer   :: Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term)
                                                       ConnectionId
                                                       (DecoderFailureOrTooMuchInput DeserialiseFailure)),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nstErrorPolicyTracer :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
    }

nullNetworkServerTracers :: NetworkServerTracers ptcl vNumber
nullNetworkServerTracers = NetworkServerTracers {
      nstMuxTracer         = nullTracer,
      nstHandshakeTracer   = nullTracer,
      nstErrorPolicyTracer = nullTracer
    }


-- | Mutable state maintained by the network component.
--
data NetworkMutableState = NetworkMutableState {
    nmsConnectionTable :: CT.ConnectionTable IO Socket.SockAddr,
    -- ^ 'ConnectionTable' which maintains information about current upstream and
    -- downstream connections.
    nmsPeerStates      :: StrictTVar IO (PeerStates IO Socket.SockAddr)
    -- ^ 'PeerStates' which maintains state of each downstream / upstream peer
    -- that errored, misbehaved or was not interesting to us.
  }

newNetworkMutableStateSTM :: STM.STM NetworkMutableState
newNetworkMutableStateSTM =
    NetworkMutableState <$> CT.newConnectionTableSTM
                        <*> newPeerStatesVarSTM

newNetworkMutableState :: IO NetworkMutableState
newNetworkMutableState = atomically newNetworkMutableStateSTM

-- | Clean 'PeerStates' within 'NetworkMutableState' every 200s
--
cleanNetworkMutableState :: NetworkMutableState
                         -> IO ()
cleanNetworkMutableState NetworkMutableState {nmsPeerStates} =
    cleanPeerStates 200 nmsPeerStates

-- | Domain-specific rejection type. Only incoming connections can be rejected.
-- Outgoing connections can still fail, but there is no "normal"
-- (non-exceptional) reason to reject one.
--
-- TODO constructor should include an explanation
data RejectConnection (p :: Provenance) where
  Rejected :: RejectConnection Remote

data SomeVersionedApplication ptcl vNumber vDataT provenance where
  SomeVersionedResponderApp
    :: ( HasResponder appType ~ True )
    => NetworkServerTracers ptcl vNumber
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> SomeVersionedApplication ptcl vNumber vDataT Remote
  SomeVersionedInitiatorApp
    :: ( HasInitiator appType ~ True )
    => NetworkConnectTracers ptcl vNumber
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> SomeVersionedApplication ptcl vNumber vDataT Local

-- | Contains everything needed to run a mux'd versioned ouroboros application
-- as initiator or responder. See use in `withConnections`. The idea is that
-- the user will give a custom `request :: Provenance -> Type` type and must
-- create a `ConnectionData ptcl vNumber provenance` with the same provenance
-- as the request. In this way, the choice of `request` GADT can determine
-- whether initiator or responder are even allowed (node-to-client server, for
-- instance, does not allow initiation).
data ConnectionData ptcl vNumber provenance where
  -- | Locally-initiated connection data.
  ConnectionDataLocal
    :: ( HasInitiator appType ~ True )
    => NetworkConnectTracers ptcl vNumber
    -> NetworkMutableState
    -> VersionDataCodec vDataT CBOR.Term
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Local
  -- | Data for a remotely-initiated connection.
  ConnectionDataRemote
    :: ( HasResponder appType ~ True )
    => NetworkServerTracers ptcl vNumber
    -> NetworkMutableState
    -> ErrorPolicies Socket.SockAddr ()
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Remote


-- | Get a concurrent connections manager, running `connection` for each
-- connection (`Socket`) between two peers (`ConnectionId`).
--
--
withConnections
  :: forall ptcl vNumber request t.
     ( Mx.ProtocolEnum ptcl
     , Mx.MiniProtocolLimits ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     )
  => (forall provenance. request provenance -> ConnectionData ptcl vNumber provenance)
  -> (Connections Connections.ConnectionId Socket.Socket request (Connection.Reject RejectConnection) (Connection.Accept ()) IO -> IO t)
  -> IO t
withConnections mk = Connection.concurrent (connection mk)

-- | Handle any connection (remotely- or locally-initiated).
-- After filling in the first 7 parameters, you get a function that can be
-- used to create a `Connections.Concurrent.concurrent` `Connections` term.
--
-- From that, you can derive a socket accept loop by using `acceptLoopOn`.
-- Outgoing connections can be made using `runClientWith` on a pair of
-- socket addresses, and referencing that `Connections` term.
connection
  :: forall ptcl vNumber provenance request.
     ( Mx.ProtocolEnum ptcl
     , Mx.MiniProtocolLimits ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     )
  => (forall provenance'. request provenance' -> ConnectionData ptcl vNumber provenance')
  -> Initiated provenance
  -> Connections.ConnectionId
  -> Socket.Socket
  -> request provenance
  -> IO (Connection.Decision provenance RejectConnection ())
connection mk _ connid socket request = case mk request of

    ConnectionDataLocal tracers state vCodec versions ->
        bracket (addConnection state) (const (removeConnection state)) $ \_ ->
            outgoingConnection vCodec tracers versions connid socket

    ConnectionDataRemote tracers state errPolicies vCodec accept versions ->
        bracket (addConnection state) (const (removeConnection state)) $ \_ ->
            incomingConnection tracers state vCodec accept versions errPolicies
                connid socket

  where
    -- FIXME may be better to only add/remove from the connection table once
    -- the connection has been accepted. The way it is now, rejected connections
    -- will briefly appear in the table.
    addConnection mutableState =
        atomically $ CT.addConnection (nmsConnectionTable mutableState) remoteAddr localAddr Nothing
    removeConnection mutableState =
        CT.removeConnection (nmsConnectionTable mutableState) remoteAddr localAddr
    (localAddr, remoteAddr) = Connections.connectionIdPair connid
 

-- | What to do for outgoing (locally-initiated) connections.
-- Unlike `incomingConnection`, there is apparently no `NetworkMutableState`
-- to deal with, so this is comparatively simple.
-- FIXME _why_ though? Why would there be mutable state to update on the
-- incoming side, but not outgoing?
outgoingConnection
  :: forall ptcl vNumber vDataT appType a b.
     ( HasInitiator appType ~ True
     , Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     )
  => VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Connections.ConnectionId
  -> Socket.Socket       -- ^ Socket to peer; could have been established by us or them.
  -> IO (Connection.Decision Local RejectConnection ()) -- ^ TODO better reject and accept types?
outgoingConnection versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions connId sd =
    -- Always accept and run initiator mode mux on the socket.
    pure $ Connection.Accept $ \_connThread -> do
        let connectionHandle = ()
            action = do
                let (localAddr, remoteAddr) = Connections.connectionIdPair connId
                    -- Mux uses a different connection ID type (the one from Connections
                    -- guarantees consistent protocols).
                    connectionId = ConnectionId localAddr remoteAddr
                muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
                bearer <- Mx.socketAsMuxBearer muxTracer sd
                Mx.muxBearerSetState muxTracer bearer Mx.Connected
                traceWith muxTracer $ Mx.MuxTraceHandshakeStart
                ts_start <- getMonotonicTime
                !mapp <- runPeerWithByteLimit
                          maxTransmissionUnit
                          BL.length
                          nctHandshakeTracer
                          codecHandshake
                          connectionId
                          (Mx.muxBearerAsControlChannel bearer Mx.ModeInitiator)
                          (handshakeClientPeer versionDataCodec versions)
                ts_end <- getMonotonicTime
                case mapp of
                     Left err -> do
                         traceWith muxTracer $ Mx.MuxTraceHandshakeClientError err (diffTime ts_end ts_start)
                         -- FIXME is it right to throw an exception here? Or would it be
                         -- better to return Connection.Reject
                         throwIO err
                     Right app -> do
                         traceWith muxTracer $ Mx.MuxTraceHandshakeClientEnd (diffTime ts_end ts_start)
                         Mx.muxStart muxTracer connectionId (toApplication app) bearer
        pure $ Handler { handle = connectionHandle, action = action }

-- | What to do on an incoming connection: run the given versions, which is
-- known to have a responder side.
incomingConnection
    :: forall ptcl vNumber vDataT appType a b.
       ( HasResponder appType ~ True
       , Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => NetworkServerTracers ptcl vNumber
    -> NetworkMutableState
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ErrorPolicies Socket.SockAddr ()
    -> Connections.ConnectionId -- ^ Includes our address and remote address.
    -> Socket.Socket            -- ^ Established by the remote peer.
    -> IO (Connection.Decision Remote RejectConnection ()) -- ^ TODO give better types for reject and handle
incomingConnection NetworkServerTracers { nstMuxTracer
                                        , nstHandshakeTracer
                                        , nstErrorPolicyTracer }
                   NetworkMutableState { nmsPeerStates }
                   versionDataCodec
                   acceptVersion
                   versions
                   errorPolicies
                   connId
                   sd = do
    -- Make a decision about whether to accept this connection.
    -- We need the time, and the local and remote addresses, so that
    -- `acceptConnectionTx` can do its thing.
    time <- getMonotonicTime
    let (localAddr, remoteAddr) = Connections.connectionIdPair connId
    decision <- STM.atomically $ do
        st <- readTVar nmsPeerStates
        !handleConn <- acceptConnectionTx localAddr time remoteAddr st
        case handleConn of
            AcceptConnection !st' connId' -> do
              writeTVar nmsPeerStates st'
              pure $ Just connId'
            RejectConnection !st' _connId' -> do
              writeTVar nmsPeerStates st'
              pure Nothing
    case decision of
        -- TODO better type for the reason for rejection.
        Nothing -> pure $ Connection.Reject Rejected
        -- To accept, we get a reference to the thread running the handler's
        -- action, so that we can `registerProducer`, which needs it for
        -- some reason.
        Just peerid -> pure $ Connection.Accept $ \connThread ->
          let -- TODO better type for a handle.
              -- This is supposed to give a handle on the `action` defined
              -- blow, allowing its holder to observe and to some extent control
              -- what's going on in that action.
              connectionHandle = ()
              -- When a connection comes up, we must register a producer.
              -- When it goes down, we must call some error policies thing
              -- `completeApplicationTx` and update state to unregister the
              -- producer.
              --
              -- TODO this is rather complex. Should be bottled up somewhere.
              register = atomically $ do
                  -- It's a StrictTVar so we read take and write I guess (no
                  -- modifyTVar')
                  st <- readTVar nmsPeerStates
                  writeTVar nmsPeerStates (registerProducer remoteAddr connThread st)
              unregister _ exitCase = do
                  time' <- getMonotonicTime
                  (rthreads, rtrace) <- case exitCase of
                      ExitCaseSuccess a -> atomically $ do
                          st <- readTVar nmsPeerStates
                          !result <- completeApplicationTx errorPolicies (ApplicationResult time' remoteAddr a) st
                          let !st' = unregisterProducer remoteAddr connThread (carState result)
                          writeTVar nmsPeerStates st'
                          return (carThreads result, carTrace result)
                      ExitCaseException e -> atomically $ do
                          st <- readTVar nmsPeerStates
                          !result <- completeApplicationTx errorPolicies (ApplicationError time' remoteAddr e) st
                          let !st' = unregisterProducer remoteAddr connThread (carState result)
                          writeTVar nmsPeerStates st'
                          return (carThreads result, carTrace result)
                      ExitCaseAbort -> error "ExitCaseAbort should not exist!"
                  -- TODO what are these threads?
                  -- Do we need them? If the `action` defined below does proper
                  -- threading (doesn't leave dangling threads) then this
                  -- should not be needed.
                  traverse_ cancel rthreads
                  traverse_ (traceWith nstErrorPolicyTracer) rtrace

              -- This is the action to run for this connection.
              -- Does version negotiation, sets up mux, and starts it.
              --
              -- `fmap fst` because generalBracket gives the result of the
              -- releaser callback as well.
              action = fmap fst $ generalBracket register unregister $ \_ -> do
                  muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer peerid `contramap` nstMuxTracer
                  (bearer :: MuxBearer ptcl IO) <- Mx.socketAsMuxBearer muxTracer' sd
                  Mx.muxBearerSetState muxTracer' bearer Mx.Connected
                  traceWith muxTracer' $ Mx.MuxTraceHandshakeStart
                  mapp <- runPeerWithByteLimit
                          maxTransmissionUnit
                          BL.length
                          nstHandshakeTracer
                          codecHandshake
                          peerid
                          (Mx.muxBearerAsControlChannel bearer Mx.ModeResponder)
                          (handshakeServerPeer versionDataCodec acceptVersion versions)
                  case mapp of
                    Left err -> do
                      traceWith muxTracer' $ Mx.MuxTraceHandshakeServerError err
                      throwIO err
                    Right app -> do
                      traceWith muxTracer' $ Mx.MuxTraceHandshakeServerEnd
                      Mx.muxStart muxTracer' peerid (toApplication app) bearer
          in  pure $ Handler { handle = connectionHandle, action = action }
  where

    -- Determines whether we should accept a connection, by using the
    -- `PeerStates` from `NetworkMutableState`.
    acceptConnectionTx
      :: Socket.SockAddr
      -> Time
      -> Socket.SockAddr
      -> PeerStates IO Socket.SockAddr
      -> STM IO (AcceptConnection (PeerStates IO Socket.SockAddr) ConnectionId)
    acceptConnectionTx sockAddr t connAddr st = do
      d <- beforeConnectTx t connAddr st
      case d of
        AllowConnection st'    -> pure $ AcceptConnection st' (ConnectionId sockAddr connAddr)
        DisallowConnection st' -> pure $ RejectConnection st' (ConnectionId sockAddr connAddr)

-- | Connection request type for use by `withServerNode`. Only
-- remotely-initiated requests are allowed.
data WithServerNodeRequest (p :: Provenance) where
    WithServerNodeRequest :: WithServerNodeRequest Remote

-- |
-- Run a server application.  It will listen on the given address for incoming
-- connection.  The server thread runs using @withAsync@ function, which means
-- that it will terminate when the callback terminates or throws an exception.
--
-- TODO: we should track connections in the state and refuse connections from
-- peers we are already connected to.  This is also the right place to ban
-- connection from peers which missbehaved.
-- Counter TODO: how do we identify peers which are already connected? If they
-- have the same address, then TCP/IP wouldn't allow a duplicate connection, so
-- this point is irrelevant... unless we have some other way of identifying?
--
-- The server will run handshake protocol on each incoming connection.  We
-- assume that each versin negotiation message should fit into
-- @'maxTransmissionUnit'@ (~5k bytes).
--
-- Note: it will open a socket in the current thread and pass it to the spawned
-- thread which runs the server.  This makes it useful for testing, where we
-- need to guarantee that a socket is open before we try to connect to it.
--
-- This is intended to be a convenient way to get a server-only application
-- up and running. Not suitable if you want to do concurrent outgoing and
-- incoming connections, as in a peer-to-peer node.
withServerNode
    :: forall appType ptcl vNumber extra t a b.
       ( HasResponder appType ~ True
       , ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , MiniProtocolLimits ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => NetworkServerTracers ptcl vNumber
    -> NetworkMutableState
    -> Socket.AddrInfo
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> ErrorPolicies Socket.SockAddr ()
    -> (Socket.SockAddr -> Async Void -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode tracers networkState addr versionDataCodec acceptVersion versions errorPolicies k =
    Connections.withSockType (Socket.addrAddress addr) $ \sockAddr ->
        withAsync (makeServer sockAddr) (k (Socket.addrAddress addr))

  where

    makeServer :: Connections.SockAddr addrType -> IO Void
    makeServer sockAddr = Connection.concurrent handleConnection $ \connections ->
      acceptLoopOn sockAddr (const WithServerNodeRequest) acceptException connections

    handleConnection :: forall provenance .
           Initiated provenance
        -> Connections.ConnectionId
        -> Socket.Socket
        -> WithServerNodeRequest provenance
        -> IO (Decision provenance RejectConnection ())
    handleConnection _ connid socket WithServerNodeRequest = incomingConnection
        tracers
        networkState
        versionDataCodec
        acceptVersion
        versions
        errorPolicies
        connid
        socket

    acceptException :: IOException -> IO ()
    acceptException e = traceWith
        (WithAddr (Socket.addrAddress addr) `contramap` nstErrorPolicyTracer tracers)
        (ErrorPolicyAcceptException e)
