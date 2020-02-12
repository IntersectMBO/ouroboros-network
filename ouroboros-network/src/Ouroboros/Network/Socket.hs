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

    -- * What to do with a connection to a peer
    -- Slightly complicated by the fact that we have initiator and responder
    -- sides to mux, some of which we may wish to disallow (note to client
    -- server doesn't allow outgoing connections, for example).
    -- TODO rename
      ConnectionData (..)
    , ConnectionId (..)
    , SomeVersionedApplication (..)
    , RejectConnection (..)
    , withConnections
    , connection
    , incomingConnection
    , outgoingConnection
    , runInitiator

    , ConnectionHandle (..)
    , ConnectionStatus (..)
    , waitForConnection

    -- * Traces
    , NetworkConnectTracers (..)
    , nullNetworkConnectTracers
    , NetworkServerTracers (..)
    , nullNetworkServerTracers

    -- * Quick setup functions for demo'ing (and, at the moment, testing)
    , withServerNode
    , connectToNode


    ) where

import           Control.Concurrent.Async
import           Control.Exception (IOException)
import           Control.Monad (when)
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadThrow
import           Control.Exception (throwIO)
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BL
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
import qualified Network.Mux.Bearer.Socket as Mx

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel

import           Ouroboros.Network.Connections.Concurrent hiding (Accept, Reject)
import qualified Ouroboros.Network.Connections.Concurrent as Connection
                   (Accept, Reject, Decision(Accept), concurrent)
import           Ouroboros.Network.Connections.Socket.Server (acceptLoop, server)
import qualified Ouroboros.Network.Connections.Socket.Types as Connections
import           Ouroboros.Network.Connections.Socket.Types (Some (..))
import           Ouroboros.Network.Connections.Types hiding (Decision(..))

-- | Tracer for locally-initiated connections.
--
data NetworkConnectTracers ptcl vNumber = NetworkConnectTracers {
      nctMuxTracer         :: Tracer IO (Mx.WithMuxBearer ConnectionId Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nctHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer ConnectionId
                                          (TraceSendRecv (Handshake vNumber CBOR.Term)))
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
-- Exceptions thrown by @'MuxApplication'@ are rethrown by @'connectToNode'@.
--
-- This does not use a `Connections` term. It manually sets up a socket and
-- makes a connection and runs a given initiator-side protocol suite. For
-- production deployments of peer-to-peer nodes, use `withConnections`.
connectToNode
  :: forall appType ptcl vNumber extra sockType a b.
     ( ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , MiniProtocolLimits ptcl
     , Mx.HasInitiator appType ~ True
     )
  => VersionDataCodec extra CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber extra (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Connections.SockAddr sockType
  -- ^ local address; the created socket will bind to it
  -> Connections.SockAddr sockType
  -- ^ remote address
  -> IO ()
connectToNode versionDataCodec tracers versions localAddr remoteAddr =
    bracket
      (Socket.socket family Socket.Stream Socket.defaultProtocol)
      Socket.close
      (\sd -> do
          when isInet $ do
              Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
              Socket.setSocketOption sd Socket.ReusePort 1
#endif
          when isInet6 $ Socket.setSocketOption sd Socket.IPv6Only 1
          Socket.bind sd (Connections.forgetSockType localAddr)
          Socket.connect sd (Connections.forgetSockType remoteAddr)
          runInitiator
            versionDataCodec
            tracers
            versions
            (Connections.makeConnectionId localAddr remoteAddr)
            sd
      )

  where
  family :: Socket.Family
  isInet, isInet6 :: Bool
  (family, isInet, isInet6) = case remoteAddr of
    Connections.SockAddrIPv4 _ _     -> (Socket.AF_INET,  True,  False)
    Connections.SockAddrIPv6 _ _ _ _ -> (Socket.AF_INET6, True,  True)
    Connections.SockAddrUnix _       -> (Socket.AF_UNIX,  False, False)

-- | Tracers required by a server which handles inbound connections.
--
data NetworkServerTracers ptcl vNumber = NetworkServerTracers {
      nstMuxTracer         :: Tracer IO (Mx.WithMuxBearer ConnectionId Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nstHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer ConnectionId
                                          (TraceSendRecv (Handshake vNumber CBOR.Term))),
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

-- | Domain-specific rejection type. Only incoming connections can be rejected.
-- Outgoing connections can still fail, but there is no "normal"
-- (non-exceptional) reason to reject one.
--
-- TODO constructor should include an explanation
data RejectConnection (p :: Provenance) where
  Rejected :: RejectConnection Remote

data SomeVersionedApplication ptcl vNumber vDataT provenance where
  SomeVersionedResponderApp
    :: ( Mx.HasResponder appType ~ True )
    => NetworkServerTracers ptcl vNumber
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> SomeVersionedApplication ptcl vNumber vDataT Remote
  SomeVersionedInitiatorApp
    :: ( Mx.HasInitiator appType ~ True )
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
    :: ( Mx.HasInitiator appType ~ True )
    => NetworkConnectTracers ptcl vNumber
    -> VersionDataCodec vDataT CBOR.Term
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Local
  -- | Data for a remotely-initiated connection.
  ConnectionDataRemote
    :: ( Mx.HasResponder appType ~ True )
    => NetworkServerTracers ptcl vNumber
    -> ErrorPolicies Socket.SockAddr ()
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Remote

-- | Handle giving an interface to a connection.
data ConnectionHandle m = ConnectionHandle
  { status :: STM m ConnectionStatus }

data ConnectionStatus = Running | Finished (Maybe SomeException)

waitForConnection :: MonadSTM m => ConnectionHandle m -> m (Maybe SomeException)
waitForConnection ch = atomically $ do
  stat <- status ch
  case stat of
    Finished result -> pure result
    Running         -> retry

-- | Get a concurrent connections manager, running `connection` for each
-- connection (`Socket`) between two peers (`ConnectionId`).
--
--
withConnections
  :: forall ptcl vNumber reject request t.
     ( ProtocolEnum ptcl
     , MiniProtocolLimits ptcl
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
  -> (Connections Connections.ConnectionId Socket.Socket request (Connection.Reject reject) (Connection.Accept (ConnectionHandle IO)) IO -> IO t)
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
  :: forall ptcl vNumber provenance reject request.
     ( ProtocolEnum ptcl
     , MiniProtocolLimits ptcl
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
  -> IO (Connection.Decision IO provenance reject (ConnectionHandle IO))
connection mk _ connid socket request = case mk request of

    -- TODO should take an error policy. We'll use it for exception handling,
    -- to figure out when to blow everything up.
    ConnectionDataLocal tracers vCodec versions ->
        outgoingConnection vCodec tracers versions connid socket

    ConnectionDataRemote tracers errPolicies vCodec accept versions ->
        incomingConnection tracers vCodec accept versions errPolicies connid socket

-- | What to do for outgoing (locally-initiated) connections.
outgoingConnection
  :: forall ptcl vNumber vDataT appType reject a b.
     ( Mx.HasInitiator appType ~ True
     , ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , MiniProtocolLimits ptcl
     )
  => VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Connections.ConnectionId
  -> Socket.Socket       -- ^ Socket to peer; could have been established by us or them.
  -> IO (Connection.Decision IO Local reject (ConnectionHandle IO))
outgoingConnection versionDataCodec tracers versions connId sd =
    -- Always accept and run initiator mode mux on the socket.
    pure $ Connection.Accept $ \_connThread -> do
        statusVar <- atomically (newTVar Running)
        let connectionHandle = ConnectionHandle
              { status = readTVar statusVar }
            action = mask $ \restore -> do
              restore (runInitiator versionDataCodec tracers versions connId sd) `catch`
                (\e -> atomically (writeTVar statusVar (Finished (Just e))))
              atomically (writeTVar statusVar (Finished Nothing))
        pure $ Handler { handle = connectionHandle, action = action }

-- | Outgoing (locally-initiated) connection action. Runs the initiator-side
-- of some protocol suite.
runInitiator
  :: forall ptcl vNumber vDataT appType a b.
     ( Mx.HasInitiator appType ~ True
     , ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     , Show ptcl
     , MiniProtocolLimits ptcl
     )
  => VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Connections.ConnectionId
  -> Socket.Socket       -- ^ Socket to peer; could have been established by us or them.
  -> IO ()
runInitiator versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions connId sd = do
  let (localAddr, remoteAddr) = Connections.connectionIdPair connId
      -- Mux uses a different connection ID type (the one from Connections
      -- guarantees consistent protocols).
      connectionId = ConnectionId localAddr remoteAddr
  muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
  let bearer = Mx.socketAsMuxBearer muxTracer sd
  Mx.traceMuxBearerState muxTracer Mx.Connected
  traceWith muxTracer $ Mx.MuxTraceHandshakeStart
  ts_start <- getMonotonicTime
  !mapp <- runPeerWithByteLimit
            maxTransmissionUnit
            BL.length
            (contramap (Mx.WithMuxBearer connectionId) nctHandshakeTracer)
            codecHandshake
            (fromChannel (Mx.muxBearerAsControlChannel bearer Mx.ModeInitiator))
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
           Mx.muxStart muxTracer (toApplication app connectionId) bearer

-- | What to do on an incoming connection: run the given versions, which is
-- known to have a responder side.
incomingConnection
    :: forall ptcl vNumber vDataT appType reject a b.
       ( Mx.HasResponder appType ~ True
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
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ErrorPolicies Socket.SockAddr ()
    -> Connections.ConnectionId -- ^ Includes our address and remote address.
    -> Socket.Socket            -- ^ Established by the remote peer.
    -> IO (Connection.Decision IO Remote reject (ConnectionHandle IO))
incomingConnection NetworkServerTracers { nstMuxTracer
                                        , nstHandshakeTracer }
                                        -- , nstErrorPolicyTracer }
                   versionDataCodec
                   acceptVersion
                   versions
                   -- TODO use these for exception handling.
                   _errorPolicies
                   connId
                   sd = pure $ Connection.Accept $ \_ -> do
  statusVar <- atomically (newTVar Running)
  let connectionHandle = ConnectionHandle { status = readTVar statusVar }
      action = mask $ \restore -> do
        restore runResponder `catch`
          (\e -> atomically (writeTVar statusVar (Finished (Just e))) >> throwIO e)
        atomically (writeTVar statusVar (Finished Nothing))
      -- This is the action to run for this connection.
      -- Does version negotiation, sets up mux, and starts it.
      runResponder = do
          muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer peerid `contramap` nstMuxTracer
          let bearer = Mx.socketAsMuxBearer muxTracer' sd
          Mx.traceMuxBearerState muxTracer' Mx.Connected
          traceWith muxTracer' $ Mx.MuxTraceHandshakeStart
          mapp <- runPeerWithByteLimit
                  maxTransmissionUnit
                  BL.length
                  (contramap (Mx.WithMuxBearer peerid) nstHandshakeTracer)
                  codecHandshake
                  (fromChannel (Mx.muxBearerAsControlChannel bearer Mx.ModeResponder))
                  (handshakeServerPeer versionDataCodec acceptVersion versions)
          case mapp of
            Left err -> do
              traceWith muxTracer' $ Mx.MuxTraceHandshakeServerError err
              throwIO err
            Right app -> do
              traceWith muxTracer' $ Mx.MuxTraceHandshakeServerEnd
              Mx.muxStart muxTracer' (toApplication app peerid) bearer
  pure $ Handler { handle = connectionHandle, action = action }
  where
  -- Drop the family information from the ConnectionId.
  peerid :: ConnectionId
  peerid = uncurry ConnectionId (Connections.connectionIdPair connId)

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
       ( Mx.HasResponder appType ~ True
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
    -> Some Connections.SockAddr
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
withServerNode tracers (Some sockAddr) versionDataCodec acceptVersion versions errorPolicies k =
    Connection.concurrent handleConnection $ \connections ->
      server sockAddr (const WithServerNodeRequest) $ \realAddr serv ->
        -- At this point the socket is bound and listening.
        withAsync (acceptLoop acceptException connections serv)
                  (k (Connections.forgetSockType realAddr))

  where

    handleConnection :: forall provenance .
           Initiated provenance
        -> Connections.ConnectionId
        -> Socket.Socket
        -> WithServerNodeRequest provenance
        -> IO (Decision IO provenance CannotReject (ConnectionHandle IO))
    handleConnection _ connid socket WithServerNodeRequest = incomingConnection
        tracers
        versionDataCodec
        acceptVersion
        versions
        errorPolicies
        connid
        socket

    acceptException :: IOException -> IO ()
    acceptException e = traceWith
        (WithAddr (Connections.forgetSockType sockAddr) `contramap` nstErrorPolicyTracer tracers)
        (ErrorPolicyAcceptException e)
