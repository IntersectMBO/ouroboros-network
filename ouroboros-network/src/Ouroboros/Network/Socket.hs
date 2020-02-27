{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
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
    , SomeResponderApplication (..)
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
    , debuggingNetworkConnectTracers
    , NetworkServerTracers (..)
    , nullNetworkServerTracers
    , debuggingNetworkServerTracers

    -- * Quick setup functions for demo'ing (and, at the moment, testing)
    , withServerNode
    , connectToNode
    , connectToNodeSocket

    -- * Auxiliary functions
    , sockAddrFamily
    ) where

import           Control.Concurrent.Async
import           Control.Exception (IOException, SomeException (..))
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
import           Data.Void

import qualified Network.Socket as Socket


import           Control.Tracer

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
import Network.Mux.DeltaQ.TraceTransformer
import qualified Network.Mux.Types as Mx
import           Network.Mux.Types (MuxBearer)

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.IOManager (AssociateWithIOCP)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Connections.Concurrent hiding (Accept, Reject)
import qualified Ouroboros.Network.Connections.Concurrent as Connection
                   (Accept, Reject, Decision(Accept), concurrent)
import           Ouroboros.Network.Connections.Socket.Server (acceptLoop, withSocket)
import           Ouroboros.Network.Connections.Types hiding (Decision(..))

-- | Tracer for locally-initiated connections.
--
data NetworkConnectTracers addr ptcl vNumber = NetworkConnectTracers {
      nctMuxTracer         :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)  Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nctHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)
                                          (TraceSendRecv (Handshake vNumber CBOR.Term)))
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
    }

nullNetworkConnectTracers :: NetworkConnectTracers addr ptcl vNumber
nullNetworkConnectTracers = NetworkConnectTracers {
      nctMuxTracer       = nullTracer,
      nctHandshakeTracer = nullTracer
    }


debuggingNetworkConnectTracers :: (Show addr, Show ptcl, Show vNumber)
                               => NetworkConnectTracers addr ptcl vNumber
debuggingNetworkConnectTracers = NetworkConnectTracers {
      nctMuxTracer       = showTracing stdoutTracer, 
      nctHandshakeTracer = showTracing stdoutTracer
    }

sockAddrFamily
    :: Socket.SockAddr
    -> Socket.Family
sockAddrFamily (Socket.SockAddrInet  _ _    ) = Socket.AF_INET
sockAddrFamily (Socket.SockAddrInet6 _ _ _ _) = Socket.AF_INET6
sockAddrFamily (Socket.SockAddrUnix _       ) = Socket.AF_UNIX

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.  If the limit is exceeded, then @'TooMuchInput'@ exception will
-- be thrown.
--
maxTransmissionUnit :: Int64
maxTransmissionUnit = 4 * 1440

-- | The handshake protocol number.
--
handshakeProtocolNum :: MiniProtocolNum
handshakeProtocolNum = MiniProtocolNum 0

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
  :: forall appType ptcl vNumber extra fd addr a b.
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
  => Snocket IO fd addr
  -> VersionDataCodec extra CBOR.Term
  -> NetworkConnectTracers addr ptcl vNumber
  -> Versions vNumber extra (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectToNode sn versionDataCodec tracers versions localAddr remoteAddr =
    bracket
      (Snocket.openToConnect sn remoteAddr)
      (Snocket.close sn)
      (\sd -> do
          Snocket.bind sn sd localAddr
          Snocket.connect sn sd remoteAddr
          runInitiator
            sn
            versionDataCodec
            tracers
            versions
            (ConnectionId localAddr remoteAddr)
            sd
      )

-- Wraps a Socket inside a Snocket and calls connectToNode
connectToNodeSocket
  :: forall appType ptcl vNumber extra a b.
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
  => AssociateWithIOCP
  -> VersionDataCodec extra CBOR.Term
  -> NetworkConnectTracers Socket.SockAddr ptcl vNumber
  -> Versions vNumber extra (OuroborosApplication appType (ConnectionId Socket.SockAddr) ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Socket.SockAddr
  -> Socket.SockAddr
  -> IO ()
connectToNodeSocket iocp versionDataCodec tracers versions localAddr remoteAddr =
    connectToNode
      (Snocket.socketSnocket iocp)
      versionDataCodec
      tracers
      versions
      localAddr
      remoteAddr

-- |
-- Wrapper for OuroborosResponderApplication and OuroborosInitiatorAndResponderApplication.
--
data SomeResponderApplication peerid ptcl m bytes b where
     SomeResponderApplication
       :: forall appType peerid ptcl m bytes a b.
          Mx.HasResponder appType ~ True
       => (OuroborosApplication appType peerid ptcl m bytes a b)
       -> SomeResponderApplication peerid ptcl m bytes b

-- | Tracers required by a server which handles inbound connections.
--
data NetworkServerTracers addr ptcl vNumber = NetworkServerTracers {
      nstMuxTracer         :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr) Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nstHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)
                                          (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nstErrorPolicyTracer :: Tracer IO (WithAddr addr ErrorPolicyTrace)
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
    }

nullNetworkServerTracers :: NetworkServerTracers addr ptcl vNumber
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

debuggingNetworkServerTracers :: (Show addr, Show ptcl, Show vNumber)
                              =>  NetworkServerTracers addr ptcl vNumber
debuggingNetworkServerTracers = NetworkServerTracers {
      nstMuxTracer         = showTracing stdoutTracer,
      nstHandshakeTracer   = showTracing stdoutTracer,
      nstErrorPolicyTracer = showTracing stdoutTracer
    }

data SomeVersionedApplication ptcl vNumber vDataT addr provenance where
  SomeVersionedResponderApp
    :: ( Mx.HasResponder appType ~ True )
    => NetworkServerTracers addr ptcl vNumber
    -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> SomeVersionedApplication ptcl vNumber vDataT addr Remote
  SomeVersionedInitiatorApp
    :: ( Mx.HasInitiator appType ~ True )
    => NetworkConnectTracers addr ptcl vNumber
    -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> SomeVersionedApplication ptcl vNumber vDataT addr Local

-- | Contains everything needed to run a mux'd versioned ouroboros application
-- as initiator or responder. See use in `withConnections`. The idea is that
-- the user will give a custom `request :: Provenance -> Type` type and must
-- create a `ConnectionData ptcl vNumber provenance` with the same provenance
-- as the request. In this way, the choice of `request` GADT can determine
-- whether initiator or responder are even allowed (node-to-client server, for
-- instance, does not allow initiation).
data ConnectionData ptcl vNumber provenance addr where
  -- | Locally-initiated connection data.
  ConnectionDataLocal
    :: ( Mx.HasInitiator appType ~ True )
    => NetworkConnectTracers addr ptcl vNumber
    -> ErrorPolicies
    -> VersionDataCodec vDataT CBOR.Term
    -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Local addr
  -- | Data for a remotely-initiated connection.
  ConnectionDataRemote
    :: ( Mx.HasResponder appType ~ True )
    => NetworkServerTracers addr ptcl vNumber
    -> ErrorPolicies
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> ConnectionData ptcl vNumber Remote addr

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
  :: forall ptcl vNumber reject request fd addr t.
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
     , Ord addr
     )
  => Snocket IO fd addr
  -> (forall provenance. request provenance -> ConnectionData ptcl vNumber provenance addr)
  -> (Connections (ConnectionId addr) fd request
        (Connection.Reject reject)
        (Connection.Accept (ConnectionHandle IO))
        IO -> IO t)
  -> IO t
withConnections sn mk = Connection.concurrent (connection sn mk)

-- | Handle any connection (remotely- or locally-initiated).
-- After filling in the first 7 parameters, you get a function that can be
-- used to create a `Connections.Concurrent.concurrent` `Connections` term.
--
-- From that, you can derive a socket accept loop by using `acceptLoopOn`.
-- Outgoing connections can be made using `runClientWith` on a pair of
-- socket addresses, and referencing that `Connections` term.
connection
  :: forall ptcl vNumber provenance reject request fd addr.
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
     , Ord addr
     )
  => Snocket IO fd addr
  -> (forall provenance'. request provenance' -> ConnectionData ptcl vNumber provenance' addr)
  -> Initiated provenance
  -> ConnectionId addr
  -> fd
  -> request provenance
  -> IO (Connection.Decision IO provenance reject (ConnectionHandle IO))
connection sn mk _ connid socket request = case mk request of

    -- TODO should take an error policy. We'll use it for exception handling,
    -- to figure out when to blow everything up.
    ConnectionDataLocal tracers errPolicies vCodec versions ->
        outgoingConnection sn tracers vCodec versions errPolicies connid socket

    ConnectionDataRemote tracers errPolicies vCodec accept versions ->
        incomingConnection sn tracers vCodec accept versions errPolicies connid socket


-- | What to do for outgoing (locally-initiated) connections.
outgoingConnection
  :: forall ptcl vNumber vDataT appType reject fd addr a b.
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
  => Snocket IO fd addr
  -> NetworkConnectTracers addr ptcl vNumber
  -> VersionDataCodec vDataT CBOR.Term
  -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> ErrorPolicies
  -- ^ FIXME this includes "application" error policies as well as "connection"
  -- error policies, but the latter is not relevant here. Use a smaller type.
  -> ConnectionId addr
  -> fd -- ^ Socket to peer; could have been established by us or them.
  -> IO (Connection.Decision IO Local reject (ConnectionHandle IO))
outgoingConnection sn tracers versionDataCodec versions errorPolicies connId sd =
    -- Always accept and run initiator mode mux on the socket.
    pure $ Connection.Accept $ \_connThread -> do
        -- io-sim-classes STM interface thinks this is ambgiuous in the monad
        -- m... Shame shame
        -- statusVar <- atomically (newTVar Running)
        statusVar <- atomically (newTVar Running :: STM IO (StrictTVar IO ConnectionStatus))
        let connectionHandle = ConnectionHandle
              { status = readTVar statusVar }
            action = mask $ \restore -> do
              result <- try (restore (runInitiator sn versionDataCodec tracers versions connId sd))
              case result of
                Left (exception :: SomeException) -> do
                  atomically (writeTVar statusVar (Finished (Just exception)))
                  case evalErrorPolicies exception (epAppErrorPolicies errorPolicies) of
                    -- This will make the `Connections` term re-throw the
                    -- exception and bring down the application.
                    Just Throw -> throwIO exception
                    _ -> pure ()
                Right _ -> atomically (writeTVar statusVar (Finished Nothing))
        pure $ Handler { handle = connectionHandle, action = action }

-- | Outgoing (locally-initiated) connection action. Runs the initiator-side
-- of some protocol suite.
runInitiator
  :: forall ptcl vNumber vDataT appType fd addr a b.
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
  => Snocket IO fd addr
  -> VersionDataCodec vDataT CBOR.Term
  -> NetworkConnectTracers addr ptcl vNumber
  -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> ConnectionId addr
  -> fd -- ^ Socket to peer; could have been established by us or them.
  -> IO ()
runInitiator sn versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions connectionId sd = do
  muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
  let bearer :: MuxBearer IO
      bearer = Snocket.toBearer sn muxTracer sd
  Mx.traceMuxBearerState muxTracer Mx.Connected
  traceWith muxTracer $ Mx.MuxTraceHandshakeStart
  ts_start <- getMonotonicTime
  !mapp <- runPeerWithByteLimit
            maxTransmissionUnit
            BL.length
            (contramap (Mx.WithMuxBearer connectionId) nctHandshakeTracer)
            codecHandshake
            (fromChannel (Mx.muxBearerAsChannel bearer handshakeProtocolNum Mx.ModeInitiator))
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
    :: forall ptcl vNumber vDataT appType reject fd addr a b.
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
       , Ord addr
       )
    => Snocket IO fd addr
    -> NetworkServerTracers addr ptcl vNumber
    -> VersionDataCodec vDataT CBOR.Term
    -> (forall vData . vDataT vData -> vData -> vData -> Accept)
    -> Versions vNumber vDataT (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> ErrorPolicies
    -- ^ FIXME this includes "application" error policies as well as "connection"
    -- error policies, but the latter is not relevant here. Use a smaller type.
    -> ConnectionId addr -- ^ Includes our address and remote address.
    -> fd -- ^ Established by the remote peer.
    -> IO (Connection.Decision IO Remote reject (ConnectionHandle IO))
incomingConnection sn
                   NetworkServerTracers { nstMuxTracer
                                        , nstHandshakeTracer }
                                        -- , nstErrorPolicyTracer }
                   versionDataCodec
                   acceptVersion
                   versions
                   errorPolicies
                   connid
                   sd = pure $ Connection.Accept $ \_ -> do
  -- Sadly, the type signature _is_ needed. io-sim-classes is defined such
  -- that the `m` type is ambiguous without it.
  statusVar <- atomically (newTVar Running :: STM IO (StrictTVar IO ConnectionStatus))
  let connectionHandle = ConnectionHandle { status = readTVar statusVar }
      action = mask $ \restore -> do
        result <- try (restore runResponder)
        case result of
          Left (exception :: SomeException) -> do
            atomically (writeTVar statusVar (Finished (Just exception)))
            case evalErrorPolicies exception (epAppErrorPolicies errorPolicies) of
              Just Throw -> throwIO exception
              _ -> pure ()
          Right _ -> atomically (writeTVar statusVar (Finished Nothing))
      -- This is the action to run for this connection.
      -- Does version negotiation, sets up mux, and starts it.
      runResponder = do
          muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer connid `contramap` nstMuxTracer
          let bearer :: MuxBearer IO
              bearer = Snocket.toBearer sn muxTracer' sd
          Mx.traceMuxBearerState muxTracer' Mx.Connected
          traceWith muxTracer' $ Mx.MuxTraceHandshakeStart
          mapp <- runPeerWithByteLimit
                  maxTransmissionUnit
                  BL.length
                  (contramap (Mx.WithMuxBearer connid) nstHandshakeTracer)
                  codecHandshake
                  (fromChannel (Mx.muxBearerAsChannel bearer handshakeProtocolNum Mx.ModeResponder))
                  (handshakeServerPeer versionDataCodec acceptVersion versions)
          case mapp of
            Left err -> do
              traceWith muxTracer' $ Mx.MuxTraceHandshakeServerError err
              throwIO err
            Right app -> do
              traceWith muxTracer' $ Mx.MuxTraceHandshakeServerEnd
              Mx.muxStart muxTracer' (toApplication app connid) bearer
  pure $ Handler { handle = connectionHandle, action = action }

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
    :: forall ptcl vNumber extra t a b appType fd addr.
       ( ProtocolEnum ptcl
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
       , Ord addr
       , Mx.HasResponder appType ~ 'True
       )
    => Snocket IO fd addr
    -> NetworkServerTracers addr ptcl vNumber
    -> addr
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> ErrorPolicies
    -> (addr -> Async Void -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode sn tracers addr versionDataCodec acceptVersion versions errorPolicies k =
    Connection.concurrent handleConnection $ \connections ->
      -- When the continuation runs here, the socket is bound and listening.
      withSocket sn addr $ \boundAddr socket -> withAsync
        (acceptLoop sn connections boundAddr WithServerNodeRequest (acceptException boundAddr)
          (Snocket.accept sn socket))
        (k boundAddr)

  where

    handleConnection :: forall provenance .
           Initiated provenance
        -> ConnectionId addr
        -> fd
        -> WithServerNodeRequest provenance
        -> IO (Decision IO provenance CannotReject (ConnectionHandle IO))
    handleConnection _ connid socket WithServerNodeRequest = incomingConnection
        sn
        tracers
        versionDataCodec
        acceptVersion
        versions
        errorPolicies
        connid
        socket

    acceptException :: addr -> SomeException -> IO ()
    acceptException boundAddr e = case fromException e of
      Just (e' :: IOException) -> traceWith
        (WithAddr boundAddr `contramap` nstErrorPolicyTracer tracers)
        (ErrorPolicyAcceptException e')
      _ -> pure ()
