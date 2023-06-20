{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- it is useful to have 'HasInitiator' constraint on 'connectToNode' & friends.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- For Hashable SockAddr
{-# OPTIONS_GHC -Wno-orphans #-}


-- |
-- Module exports interface for running a node over a socket over TCP \/ IP.
--
module Ouroboros.Network.Socket
  ( -- * High level socket interface
    ConnectionTable
  , ConnectionTableRef (..)
  , ValencyCounter
  , NetworkMutableState (..)
  , SomeResponderApplication (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , AcceptedConnectionsLimit (..)
  , ConnectionId (..)
  , withServerNode
  , withServerNode'
  , connectToNode
  , connectToNodeSocket
  , connectToNode'
    -- * Socket configuration
  , configureSocket
  , configureSystemdSocket
  , SystemdSocketTracer (..)
  , configureOutboundSocket
    -- * Traces
  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , debuggingNetworkConnectTracers
  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , debuggingNetworkServerTracers
  , AcceptConnectionsPolicyTrace (..)
    -- * Helper function for creating servers
  , fromSnocket
  , beginConnection
    -- * Re-export of HandshakeCallbacks
  , HandshakeCallbacks (..)
    -- * Re-export of PeerStates
  , PeerStates
    -- * Re-export connection table functions
  , newConnectionTable
  , refConnection
  , addConnection
  , removeConnection
  , newValencyCounter
  , addValencyCounter
  , remValencyCounter
  , waitValencyCounter
  , readValencyCounter
    -- * Auxiliary functions
  , sockAddrFamily
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (SomeException (..))
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Control.Monad (unless, when)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import qualified Control.Monad.STM as STM
import qualified Data.ByteString.Lazy as BL
import           Data.Hashable
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           Data.Void
import           Data.Word (Word16)
import           GHC.IO.Exception
#if !defined(mingw32_HOST_OS)
import           Foreign.C.Error
#endif

import           Network.Socket (SockAddr, Socket, StructLinger (..))
import qualified Network.Socket as Socket

import           Control.Tracer

import qualified Network.Mux.Bearer as Mx
import qualified Network.Mux.Compat as Mx
import           Network.Mux.DeltaQ.TraceTransformer
import           Network.TypedProtocol.Codec hiding (decode, encode)

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ControlMessage
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Handshake (HandshakeCallbacks (..))
import           Ouroboros.Network.IOManager (IOManager)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Server.ConnectionTable
import           Ouroboros.Network.Server.Socket
                     (AcceptConnectionsPolicyTrace (..),
                     AcceptedConnectionsLimit (..))
import qualified Ouroboros.Network.Server.Socket as Server
import           Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Subscription.PeerState


-- | Tracer used by 'connectToNode' (and derivatives, like
-- 'Ouroboros.Network.NodeToNode.connectTo' or
-- 'Ouroboros.Network.NodeToClient.connectTo).
--
data NetworkConnectTracers addr vNumber = NetworkConnectTracers {
      nctMuxTracer         :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)  Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nctHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)
                                          (TraceSendRecv (Handshake vNumber CBOR.Term)))
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
    }

nullNetworkConnectTracers :: NetworkConnectTracers addr vNumber
nullNetworkConnectTracers = NetworkConnectTracers {
      nctMuxTracer       = nullTracer,
      nctHandshakeTracer = nullTracer
    }


debuggingNetworkConnectTracers :: (Show addr, Show vNumber)
                               => NetworkConnectTracers addr vNumber
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


-- | Configure a socket.  Either 'Socket.AF_INET' or 'Socket.AF_INET6' socket
-- is expected.
--
configureSocket :: Socket -> Maybe SockAddr -> IO ()
configureSocket sock addr = do
    let fml = sockAddrFamily <$> addr
    Socket.setSocketOption sock Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
    -- not supported on Windows 10
    Socket.setSocketOption sock Socket.ReusePort 1
#endif
    Socket.setSocketOption sock Socket.NoDelay 1
    -- it is safe to set 'SO_LINGER' option (which implicates that every
    -- close will reset the connection), since our protocols are robust.
    -- In particular if invalid data will arrive (which includes the rare
    -- case of a late packet from a previous connection), we will abandon
    -- (and close) the connection.
    Socket.setSockOpt sock Socket.Linger
                          (StructLinger { sl_onoff  = 1,
                                          sl_linger = 0 })
    when (fml == Just Socket.AF_INET6)
      -- An AF_INET6 socket can be used to talk to both IPv4 and IPv6 end points, and
      -- it is enabled by default on some systems. Disabled here since we run a separate
      -- IPv4 server instance if configured to use IPv4.
      $ Socket.setSocketOption sock Socket.IPv6Only 1


-- | Configure sockets passed through systemd socket activation.
-- Currently 'ReuseAddr' and 'Linger' options are not configurable with
-- 'systemd.socket', these options are set by this function.  For other socket
-- options we only trace if they are not set.
--
configureSystemdSocket :: Tracer IO SystemdSocketTracer -> Socket -> SockAddr -> IO ()
configureSystemdSocket tracer sock addr = do
   let fml = sockAddrFamily addr
   case fml of
     Socket.AF_INET ->
          Socket.setSocketOption sock Socket.ReuseAddr 1
     Socket.AF_INET6 ->
          Socket.setSocketOption sock Socket.ReuseAddr 1
     _ -> return ()
#if !defined(mingw32_HOST_OS)
   -- not supported on Windows 10
   reusePortOpt <- Socket.getSocketOption sock Socket.ReusePort
   unless (reusePortOpt /= 0) $
     traceWith tracer (SocketOptionNotSet Socket.ReusePort)
#endif
   noDelayOpt <- Socket.getSocketOption sock Socket.NoDelay
   unless (noDelayOpt /= 0) $
     traceWith tracer (SocketOptionNotSet Socket.NoDelay)

   Socket.setSockOpt sock Socket.Linger
                         (StructLinger { sl_onoff  = 1,
                                         sl_linger = 0 })
   when (fml == Socket.AF_INET6) $ do
     ipv6OnlyOpt <- Socket.getSocketOption sock Socket.IPv6Only
     unless (ipv6OnlyOpt /= 0) $
       traceWith tracer (SocketOptionNotSet Socket.IPv6Only)

data SystemdSocketTracer = SocketOptionNotSet Socket.SocketOption
  deriving Show


-- | Configure an connection socket.
--
configureOutboundSocket :: Socket -> IO ()
configureOutboundSocket sock = do
    Socket.setSockOpt sock Socket.Linger
                          (StructLinger { sl_onoff  = 1,
                                          sl_linger = 0 })


instance Hashable Socket.SockAddr where
  hashWithSalt s (Socket.SockAddrInet   p   a   ) = hashWithSalt s (fromIntegral p :: Word16, a)
  hashWithSalt s (Socket.SockAddrInet6  p _ a _ ) = hashWithSalt s (fromIntegral p :: Word16, a)
  hashWithSalt s (Socket.SockAddrUnix   p       ) = hashWithSalt s p

-- | We place an upper limit of `30s` on the time we wait on receiving an SDU.
-- There is no upper bound on the time we wait when waiting for a new SDU.
-- This makes it possible for miniprotocols to use timeouts that are larger
-- than 30s or wait forever.  `30s` for receiving an SDU corresponds to
-- a minimum speed limit of 17kbps.
--
-- ( 8      -- mux header length
-- + 0xffff -- maximum SDU payload
-- )
-- * 8
-- = 524_344 -- maximum bits in an SDU
--
--  524_344 / 30 / 1024 = 17kbps
--
sduTimeout :: DiffTime
sduTimeout = 30

-- | For handshake, we put a limit of `10s` for sending or receiving a single
-- `MuxSDU`.
--
sduHandshakeTimeout :: DiffTime
sduHandshakeTimeout = 10


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
  :: forall appType vNumber vData fd addr a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator appType ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> (fd -> IO ()) -- ^ configure a socket
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
  -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
  -> VersionDataCodec CBOR.Term vNumber vData
  -> NetworkConnectTracers addr vNumber
  -> HandshakeCallbacks vData
  -> Versions vNumber vData (OuroborosApplication appType addr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectToNode sn makeBearer configureSock handshakeCodec handshakeTimeLimits versionDataCodec tracers handshakeCallbacks versions localAddr remoteAddr =
    bracket
      (Snocket.openToConnect sn remoteAddr)
      (Snocket.close sn)
      (\sd -> do
          configureSock sd
          case localAddr of
            Just addr -> Snocket.bind sn sd addr
            Nothing   -> return ()
          Snocket.connect sn sd remoteAddr
          connectToNode' sn makeBearer handshakeCodec handshakeTimeLimits versionDataCodec tracers handshakeCallbacks versions sd
          print "DELAY"
          threadDelay 120
          print "DELAY: DONE"
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
  :: forall appType vNumber vData fd addr a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator appType ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
  -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
  -> VersionDataCodec CBOR.Term vNumber vData
  -> NetworkConnectTracers addr vNumber
  -> HandshakeCallbacks vData
  -> Versions vNumber vData (OuroborosApplication appType addr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -> fd
  -- ^ a configured socket to use to connect to a remote service provider
  -> IO ()
connectToNode' sn makeBearer handshakeCodec handshakeTimeLimits versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } handshakeCallbacks versions sd = do
    connectionId <- ConnectionId <$> Snocket.getLocalAddr sn sd <*> Snocket.getRemoteAddr sn sd
    muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
    ts_start <- getMonotonicTime

    handshakeBearer <- Mx.getBearer makeBearer sduHandshakeTimeout muxTracer sd
    app_e <-
      runHandshakeClient
        handshakeBearer
        connectionId
        -- TODO: push 'HandshakeArguments' up the call stack.
        HandshakeArguments {
          haHandshakeTracer  = nctHandshakeTracer,
          haHandshakeCodec   = handshakeCodec,
          haVersionDataCodec = versionDataCodec,
          haAcceptVersion    = acceptCb handshakeCallbacks,
          haQueryVersion     = queryCb handshakeCallbacks,
          haTimeLimits       = handshakeTimeLimits
        }
        versions
    ts_end <- getMonotonicTime
    case app_e of
         Left (HandshakeProtocolLimit err) -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientError err (diffTime ts_end ts_start)
             throwIO err

         Left (HandshakeProtocolError err) -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientError err (diffTime ts_end ts_start)
             throwIO err

         Right (HandshakeNegotiationResult app _versionNumber _agreedOptions) -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientEnd (diffTime ts_end ts_start)
             bearer <- Mx.getBearer makeBearer sduTimeout muxTracer sd
             Mx.muxStart
               muxTracer
               (toApplication connectionId (continueForever (Proxy :: Proxy IO)) app)
               bearer

         Right (HandshakeQueryResult _vMap) -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientEnd (diffTime ts_end ts_start)
             throwIO (QueryNotSupported @vNumber)


-- Wraps a Socket inside a Snocket and calls connectToNode'
connectToNodeSocket
  :: forall appType vNumber vData a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator appType ~ True
     )
  => IOManager
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
  -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
  -> VersionDataCodec CBOR.Term vNumber vData
  -> NetworkConnectTracers Socket.SockAddr vNumber
  -> HandshakeCallbacks vData
  -> Versions vNumber vData (OuroborosApplication appType Socket.SockAddr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -> Socket.Socket
  -> IO ()
connectToNodeSocket iocp handshakeCodec handshakeTimeLimits versionDataCodec tracers handshakeCallbacks versions sd =
    connectToNode'
      (Snocket.socketSnocket iocp)
      Mx.makeSocketBearer
      handshakeCodec
      handshakeTimeLimits
      versionDataCodec
      tracers
      handshakeCallbacks
      versions
      sd

-- |
-- Wrapper for OuroborosResponderApplication and OuroborosInitiatorAndResponderApplication.
--
data SomeResponderApplication addr bytes m b where
     SomeResponderApplication
       :: forall appType addr bytes m a b.
          Mx.HasResponder appType ~ True
       => (OuroborosApplication appType addr bytes m a b)
       -> SomeResponderApplication addr bytes m b

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
data AcceptConnection st vNumber vData peerid m bytes where

    AcceptConnection
      :: forall st vNumber vData peerid bytes m b.
         !st
      -> !(ConnectionId peerid)
      -> Versions vNumber vData (SomeResponderApplication peerid bytes m b)
      -> AcceptConnection st vNumber vData peerid m bytes

    RejectConnection
      :: !st
      -> !(ConnectionId peerid)
      -> AcceptConnection st vNumber vData peerid m bytes


-- |
-- Accept or reject incoming connection based on the current state and address
-- of the incoming connection.
--
beginConnection
    :: forall vNumber vData addr st fd.
       ( Ord vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => Mx.MakeBearer IO fd
    -> Tracer IO (Mx.WithMuxBearer (ConnectionId addr) Mx.MuxTrace)
    -> Tracer IO (Mx.WithMuxBearer (ConnectionId addr) (TraceSendRecv (Handshake vNumber CBOR.Term)))
    -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
    -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
    -> VersionDataCodec CBOR.Term vNumber vData
    -> HandshakeCallbacks vData
    -> (Time -> addr -> st -> STM.STM (AcceptConnection st vNumber vData addr IO BL.ByteString))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr fd st ()
beginConnection makeBearer muxTracer handshakeTracer handshakeCodec handshakeTimeLimits versionDataCodec handshakeCallbacks fn t addr st = do
    accept <- fn t addr st
    case accept of
      AcceptConnection st' connectionId versions -> pure $ Server.Accept st' $ \sd -> do
        muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` muxTracer

        traceWith muxTracer' $ Mx.MuxTraceHandshakeStart

        handshakeBearer <- Mx.getBearer makeBearer sduHandshakeTimeout muxTracer' sd
        app_e <-
          runHandshakeServer
            handshakeBearer
            connectionId
            HandshakeArguments {
              haHandshakeTracer  = handshakeTracer,
              haHandshakeCodec   = handshakeCodec,
              haVersionDataCodec = versionDataCodec,
              haAcceptVersion    = acceptCb handshakeCallbacks,
              haQueryVersion     = queryCb handshakeCallbacks,
              haTimeLimits       = handshakeTimeLimits
            }
           versions

        case app_e of
             Left (HandshakeProtocolLimit err) -> do
                 traceWith muxTracer' $ Mx.MuxTraceHandshakeServerError err
                 throwIO err

             Left (HandshakeProtocolError err) -> do
                 traceWith muxTracer' $ Mx.MuxTraceHandshakeServerError err
                 throwIO err

             Right (HandshakeNegotiationResult (SomeResponderApplication app) _versionNumber _agreedOptions) -> do
                 traceWith muxTracer' $ Mx.MuxTraceHandshakeServerEnd
                 bearer <- Mx.getBearer makeBearer sduTimeout muxTracer' sd
                 Mx.muxStart
                   muxTracer'
                   (toApplication connectionId (continueForever (Proxy :: Proxy IO)) app)
                   bearer

             Right (HandshakeQueryResult _vMap) -> do
                 traceWith muxTracer' Mx.MuxTraceHandshakeServerEnd
                 -- Wait 20s for client to receive response, who should close the connection.
                 threadDelay 20

      RejectConnection st' _peerid -> pure $ Server.Reject st'

mkListeningSocket
    :: Snocket IO fd addr
    -> (fd -> addr -> IO ())
    -> addr
    -> Snocket.AddressFamily addr
    -> IO fd
mkListeningSocket sn configureSock addr family_ = do
    sd <- Snocket.open sn family_
    configureSock sd addr
    Snocket.bind sn sd addr
    Snocket.listen sn sd
    pure sd

-- |
-- Make a server-compatible socket from a network socket.
--
fromSnocket
    :: forall fd addr. Ord addr
    => ConnectionTable IO addr
    -> Snocket IO fd addr
    -> fd -- ^ socket or handle
    -> IO (Server.Socket addr fd)
fromSnocket tblVar sn sd = go <$> Snocket.accept sn sd
  where
    go :: Snocket.Accept IO fd addr -> Server.Socket addr fd
    go (Snocket.Accept accept) = Server.Socket $ do
      (result, next) <- accept
      case result of
        Snocket.Accepted sd' remoteAddr -> do
          -- TOOD: we don't need to that on each accept
          localAddr <- Snocket.getLocalAddr sn sd'
          atomically $ addConnection tblVar remoteAddr localAddr ConnectionInbound Nothing
          pure (remoteAddr, sd', close remoteAddr localAddr sd', go next)
        Snocket.AcceptFailure err ->
          -- the is no way to construct 'Server.Socket'; This will be removed in a later commit!
          throwIO err

    close remoteAddr localAddr sd' = do
        removeConnection tblVar remoteAddr localAddr ConnectionInbound
        Snocket.close sn sd'


-- | Tracers required by a server which handles inbound connections.
--
data NetworkServerTracers addr vNumber = NetworkServerTracers {
      nstMuxTracer         :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr) Mx.MuxTrace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nstHandshakeTracer   :: Tracer IO (Mx.WithMuxBearer (ConnectionId addr)
                                          (TraceSendRecv (Handshake vNumber CBOR.Term))),
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotation mismatches.
      nstErrorPolicyTracer :: Tracer IO (WithAddr addr ErrorPolicyTrace),
      -- ^ error policy tracer; must not be 'nullTracer', otherwise all the
      -- exceptions which are not matched by any error policy will be caught
      -- and not logged or rethrown.
      nstAcceptPolicyTracer :: Tracer IO AcceptConnectionsPolicyTrace
      -- ^ tracing rate limiting of accepting connections.
    }

nullNetworkServerTracers :: NetworkServerTracers addr vNumber
nullNetworkServerTracers = NetworkServerTracers {
      nstMuxTracer          = nullTracer,
      nstHandshakeTracer    = nullTracer,
      nstErrorPolicyTracer  = nullTracer,
      nstAcceptPolicyTracer = nullTracer
    }

debuggingNetworkServerTracers :: (Show addr, Show vNumber)
                              =>  NetworkServerTracers addr vNumber
debuggingNetworkServerTracers = NetworkServerTracers {
      nstMuxTracer          = showTracing stdoutTracer,
      nstHandshakeTracer    = showTracing stdoutTracer,
      nstErrorPolicyTracer  = showTracing stdoutTracer,
      nstAcceptPolicyTracer = showTracing stdoutTracer
    }


-- | Mutable state maintained by the network component.
--
data NetworkMutableState addr = NetworkMutableState {
    nmsConnectionTable :: ConnectionTable IO addr,
    -- ^ 'ConnectionTable' which maintains information about current upstream and
    -- downstream connections.
    nmsPeerStates      :: StrictTVar IO (PeerStates IO addr)
    -- ^ 'PeerStates' which maintains state of each downstream / upstream peer
    -- that errored, misbehaved or was not interesting to us.
  }

newNetworkMutableStateSTM :: STM.STM (NetworkMutableState addr)
newNetworkMutableStateSTM =
    NetworkMutableState <$> newConnectionTableSTM
                        <*> newPeerStatesVarSTM

newNetworkMutableState :: IO (NetworkMutableState addr)
newNetworkMutableState = atomically newNetworkMutableStateSTM

-- | Clean 'PeerStates' within 'NetworkMutableState' every 200s
--
cleanNetworkMutableState :: NetworkMutableState addr
                         -> IO ()
cleanNetworkMutableState NetworkMutableState {nmsPeerStates} =
    cleanPeerStates 200 nmsPeerStates

-- |
-- Thin wrapper around @'Server.run'@.
--
runServerThread
    :: forall vNumber vData fd addr b.
       ( Ord vNumber
       , Typeable vNumber
       , Show vNumber
       , Ord addr
       )
    => NetworkServerTracers addr vNumber
    -> NetworkMutableState addr
    -> Snocket IO fd addr
    -> Mx.MakeBearer IO fd
    -> fd
    -> AcceptedConnectionsLimit
    -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
    -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
    -> VersionDataCodec CBOR.Term vNumber vData
    -> HandshakeCallbacks vData
    -> Versions vNumber vData (SomeResponderApplication addr BL.ByteString IO b)
    -> ErrorPolicies
    -> IO Void
runServerThread NetworkServerTracers { nstMuxTracer
                                     , nstHandshakeTracer
                                     , nstErrorPolicyTracer
                                     , nstAcceptPolicyTracer
                                     }
                NetworkMutableState { nmsConnectionTable
                                    , nmsPeerStates }
                sn
                makeBearer
                sd
                acceptedConnectionsLimit
                handshakeCodec
                handshakeTimeLimits
                versionDataCodec
                handshakeCallbacks
                versions
                errorPolicies = do
    sockAddr <- Snocket.getLocalAddr sn sd
    serverSocket <- fromSnocket nmsConnectionTable sn sd
    Server.run
        nstErrorPolicyTracer
        nstAcceptPolicyTracer
        serverSocket
        acceptedConnectionsLimit
        (acceptException sockAddr)
        (beginConnection makeBearer nstMuxTracer nstHandshakeTracer handshakeCodec handshakeTimeLimits versionDataCodec handshakeCallbacks (acceptConnectionTx sockAddr))
        -- register producer when application starts, it will be unregistered
        -- using 'CompleteConnection'
        (\remoteAddr thread st -> pure $ registerProducer remoteAddr thread
        st)
        completeTx mainTx (toLazyTVar nmsPeerStates)
  where
    mainTx :: Server.Main (PeerStates IO addr) Void
    mainTx (ThrowException e) = throwIO e
    mainTx PeerStates{}       = retry

    -- When a connection completes, we do nothing. State is ().
    -- Crucially: we don't re-throw exceptions, because doing so would
    -- bring down the server.
    completeTx :: Server.CompleteConnection
                    addr
                    (PeerStates IO addr)
                    (WithAddr addr ErrorPolicyTrace)
                    ()
    completeTx result st = case result of

      Server.Result thread remoteAddr t (Left (SomeException e)) ->
        fmap (unregisterProducer remoteAddr thread)
          <$> completeApplicationTx errorPolicies (ApplicationError t remoteAddr e) st

      Server.Result thread remoteAddr t (Right r) ->
        fmap (unregisterProducer remoteAddr thread)
          <$> completeApplicationTx errorPolicies (ApplicationResult t remoteAddr r) st

    iseCONNABORTED :: IOError -> Bool
#if defined(mingw32_HOST_OS)
    -- On Windows the network packet classifies all errors
    -- as OtherError. This means that we're forced to match
    -- on the error string. The text string comes from
    -- the network package's winSockErr.c, and if it ever
    -- changes we must update our text string too.
    iseCONNABORTED (IOError _ _ _ "Software caused connection abort (WSAECONNABORTED)" _ _) = True
    iseCONNABORTED _ = False
#else
    iseCONNABORTED (IOError _ _ _ _ (Just cerrno) _) = eCONNABORTED == Errno cerrno
#if defined(darwin_HOST_OS)
    -- There is a bug in accept for IPv6 sockets. Instead of returning -1
    -- and setting errno to ECONNABORTED an invalid (>= 0) file descriptor
    -- is returned, with the client address left unchanged. The uninitialized
    -- client address causes the network package to throw the user error below.
    iseCONNABORTED (IOError _ UserError _ "Network.Socket.Types.peekSockAddr: address family '0' not supported." _ _) = True
#endif
    iseCONNABORTED _ = False
#endif


    acceptException :: addr -> IOException -> IO ()
    acceptException a e = do
      traceWith (WithAddr a `contramap` nstErrorPolicyTracer) $ ErrorPolicyAcceptException e

      -- Try the determine if the connection was aborted by the remote end
      -- before we could process the accept, or if it was a resource exaustion
      -- problem.
      -- NB. This piece of code is fragile and depends on specific
      -- strings/mappings in the network and base libraries.
      if iseCONNABORTED e then return ()
                          else throwIO e

    acceptConnectionTx sockAddr t connAddr st = do
      d <- beforeConnectTx t connAddr st
      case d of
        AllowConnection st'    -> pure $ AcceptConnection st' (ConnectionId sockAddr connAddr) versions
        OnlyAccept st'         -> pure $ AcceptConnection st' (ConnectionId sockAddr connAddr) versions
        DisallowConnection st' -> pure $ RejectConnection st' (ConnectionId sockAddr connAddr)

-- | Run a server application. It will listen on the given address for incoming
-- connection, otherwise like withServerNode'.
withServerNode
    :: forall vNumber vData t fd addr b.
       ( Ord vNumber
       , Typeable vNumber
       , Show vNumber
       , Ord addr
       )
    => Snocket IO fd addr
    -> Mx.MakeBearer IO fd
    -> (fd -> addr -> IO ()) -- ^ callback to configure a socket
    -> NetworkServerTracers addr vNumber
    -> NetworkMutableState addr
    -> AcceptedConnectionsLimit
    -> addr
    -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
    -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
    -> VersionDataCodec CBOR.Term vNumber vData
    -> HandshakeCallbacks vData
    -> Versions vNumber vData (SomeResponderApplication addr BL.ByteString IO b)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> ErrorPolicies
    -> (addr -> Async Void -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode sn makeBearer
               configureSock
               tracers
               networkState
               acceptedConnectionsLimit
               addr
               handshakeCodec
               handshakeTimeLimits
               versionDataCodec
               handshakeCallbacks
               versions
               errorPolicies
               k =
    bracket (mkListeningSocket sn configureSock addr (Snocket.addrFamily sn addr)) (Snocket.close sn) $ \sd -> do
      withServerNode'
        sn
        makeBearer
        tracers
        networkState
        acceptedConnectionsLimit
        sd
        handshakeCodec
        handshakeTimeLimits
        versionDataCodec
        handshakeCallbacks
        versions
        errorPolicies
        k

-- |
-- Run a server application on the provided socket. The socket must be ready to accept connections.
-- The server thread runs using @withAsync@ function, which means
-- that it will terminate when the callback terminates or throws an exception.
--
-- TODO: we should track connections in the state and refuse connections from
-- peers we are already connected to.  This is also the right place to ban
-- connection from peers which missbehaved.
--
-- The server will run handshake protocol on each incoming connection.  We
-- assume that each versin negotiation message should fit into
-- @'maxTransmissionUnit'@ (~5k bytes).
--
-- Note: it will open a socket in the current thread and pass it to the spawned
-- thread which runs the server.  This makes it useful for testing, where we
-- need to guarantee that a socket is open before we try to connect to it.
withServerNode'
    :: forall vNumber vData t fd addr b.
       ( Ord vNumber
       , Typeable vNumber
       , Show vNumber
       , Ord addr
       )
    => Snocket IO fd addr
    -> Mx.MakeBearer IO fd
    -> NetworkServerTracers addr vNumber
    -> NetworkMutableState addr
    -> AcceptedConnectionsLimit
    -> fd
    -- ^ a configured socket to be used be the server.  The server will call
    -- `bind` and `listen` methods but it will not set any socket or tcp options
    -- on it.
    -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString
    -> ProtocolTimeLimits (Handshake vNumber CBOR.Term)
    -> VersionDataCodec CBOR.Term vNumber vData
    -> HandshakeCallbacks vData
    -> Versions vNumber vData (SomeResponderApplication addr BL.ByteString IO b)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> ErrorPolicies
    -> (addr -> Async Void -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode' sn makeBearer
                tracers
                networkState
                acceptedConnectionsLimit
                sd
                handshakeCodec
                handshakeTimeLimits
                versionDataCodec
                handshakeCallbacks
                versions
                errorPolicies
                k = do
      addr' <- Snocket.getLocalAddr sn sd
      withAsync
        (runServerThread
          tracers
          networkState
          sn
          makeBearer
          sd
          acceptedConnectionsLimit
          handshakeCodec
          handshakeTimeLimits
          versionDataCodec
          handshakeCallbacks
          versions
          errorPolicies)
        (k addr')
