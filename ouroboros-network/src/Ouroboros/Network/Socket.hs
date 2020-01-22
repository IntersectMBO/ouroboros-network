{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module exports interface for running a node over a socket over TCP \/ IP.
--
module Ouroboros.Network.Socket (
    -- * High level socket interface
      ConnectionTable
    , ConnectionTableRef (..)
    , ValencyCounter
    , NetworkMutableState (..)
    , newNetworkMutableState
    , newNetworkMutableStateSTM
    , cleanNetworkMutableState
    , ConnectionId (..)
    , withServerNode
    , connectToNode
    , connectToNode'

    -- * Traces
    , NetworkConnectTracers (..)
    , nullNetworkConnectTracers
    , NetworkServerTracers (..)
    , nullNetworkServerTracers

    -- * Helper function for creating servers
    , fromSocket
    , beginConnection

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
    ) where

import           Control.Concurrent.Async
import           Control.Exception (IOException, SomeException (..))
import           Control.Monad (when)
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import qualified Control.Monad.STM as STM
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

import qualified Network.Socket as Socket hiding (recv)

import           Cardano.Prelude (NoUnexpectedThunks (..), ThunkInfo (..))

import           Control.Tracer

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
import Network.Mux.DeltaQ.TraceTransformer
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Bearer.Socket as Mx

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel
import qualified Ouroboros.Network.Server.Socket as Server
import           Ouroboros.Network.Server.ConnectionTable


-- | Tracer used by 'connectToNode' (and derivatives, like
-- 'Ouroboros.Network.NodeToNode.connectTo' or
-- 'Ouroboros.Network.NodeToClient.connectTo).
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
-- Exceptions thrown by @'MuxApplication'@ are rethrown by @'connectTo'@.
connectToNode
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
  => VersionDataCodec extra CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber extra (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
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
  => VersionDataCodec extra CBOR.Term
  -> NetworkConnectTracers ptcl vNumber
  -> Versions vNumber extra (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Socket.Socket
  -> IO ()
connectToNode' versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions sd = do
    connectionId <- ConnectionId <$> Socket.getSocketName sd <*> Socket.getPeerName sd
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
             throwIO err
         Right app -> do
             traceWith muxTracer $ Mx.MuxTraceHandshakeClientEnd (diffTime ts_end ts_start)
             Mx.muxStart muxTracer (toApplication app connectionId) bearer


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
data AcceptConnection st vNumber extra peerid ptcl m bytes where

    AcceptConnection
      :: forall appType st vNumber extra peerid ptcl m bytes a b.
         Mx.HasResponder appType ~ True
      => !st
      -> !peerid
      -> Versions vNumber extra (OuroborosApplication appType peerid ptcl m bytes a b)
      -> AcceptConnection st vNumber extra peerid ptcl m bytes

    RejectConnection
      :: !st
      -> !peerid
      -> AcceptConnection st vNumber extra peerid ptcl m bytes


-- |
-- Accept or reject incoming connection based on the current state and address
-- of the incoming connection.
--
beginConnection
    :: forall peerid ptcl vNumber extra addr st.
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
       )
    => Tracer IO (Mx.WithMuxBearer peerid Mx.MuxTrace)
    -> Tracer IO (Mx.WithMuxBearer peerid (TraceSendRecv (Handshake vNumber CBOR.Term)))
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> (Time -> addr -> st -> STM.STM (AcceptConnection st vNumber extra peerid ptcl IO BL.ByteString))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr Socket.Socket st ()
beginConnection muxTracer handshakeTracer versionDataCodec acceptVersion fn t addr st = do
    accept <- fn t addr st
    case accept of
      AcceptConnection st' peerid versions -> pure $ Server.Accept st' $ \sd -> do
        muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer peerid `contramap` muxTracer
        let bearer = Mx.socketAsMuxBearer muxTracer' sd
        Mx.traceMuxBearerState muxTracer' Mx.Connected
        traceWith muxTracer' $ Mx.MuxTraceHandshakeStart
        mapp <- runPeerWithByteLimit
                maxTransmissionUnit
                BL.length
                (contramap (Mx.WithMuxBearer peerid) handshakeTracer)
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
      RejectConnection st' _peerid -> pure $ Server.Reject st'


-- Make the server listening socket
mkListeningSocket
    :: Socket.Family
    -> Maybe Socket.SockAddr
    -> IO Socket.Socket
mkListeningSocket addrFamily_ addr = do
    sd <- Socket.socket addrFamily_ Socket.Stream Socket.defaultProtocol
    when (addrFamily_ == Socket.AF_INET ||
          addrFamily_ == Socket.AF_INET6) $ do
        Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
        Socket.setSocketOption sd Socket.ReusePort 1
#endif
    case addr of
      Nothing -> pure ()
      Just addr_ -> do
        when (addrFamily_ == Socket.AF_INET6) $
           -- An AF_INET6 socket can be used to talk to both IPv4 and IPv6 end points, and
           -- it is enabled by default on some systems. Disabled here since we run a separate
           -- IPv4 server instance if configured to use IPv4.
           Socket.setSocketOption sd Socket.IPv6Only 1

        Socket.bind sd addr_
        Socket.listen sd 1
    pure sd


-- |
-- Make a server-compatible socket from a network socket.
--
fromSocket
    :: ConnectionTable IO Socket.SockAddr
    -> Socket.Socket
    -> Server.Socket Socket.SockAddr Socket.Socket
fromSocket tblVar sd = Server.Socket
    { Server.acceptConnection = do
        (sd', remoteAddr) <- Socket.accept sd
        localAddr <- Socket.getSocketName sd'
        atomically $ addConnection tblVar remoteAddr localAddr Nothing
        pure (remoteAddr, sd', close remoteAddr localAddr sd')
    }
  where
    close remoteAddr localAddr sd' = do
        removeConnection tblVar remoteAddr localAddr
        Socket.close sd'


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


-- | Mutable state maintained by the network component.
--
data NetworkMutableState = NetworkMutableState {
    nmsConnectionTable :: ConnectionTable IO Socket.SockAddr,
    -- ^ 'ConnectionTable' which maintains information about current upstream and
    -- downstream connections.
    nmsPeerStates      :: StrictTVar IO (PeerStates IO Socket.SockAddr)
    -- ^ 'PeerStates' which maintains state of each downstream / upstream peer
    -- that errored, misbehaved or was not interesting to us.
  }

newNetworkMutableStateSTM :: STM.STM NetworkMutableState
newNetworkMutableStateSTM =
    NetworkMutableState <$> newConnectionTableSTM
                        <*> newPeerStatesVarSTM

newNetworkMutableState :: IO NetworkMutableState
newNetworkMutableState = atomically newNetworkMutableStateSTM

-- | Clean 'PeerStates' within 'NetworkMutableState' every 200s
--
cleanNetworkMutableState :: NetworkMutableState
                         -> IO ()
cleanNetworkMutableState NetworkMutableState {nmsPeerStates} =
    cleanPeerStates 200 nmsPeerStates

-- |
-- Thin wrapper around @'Server.run'@.
--
runServerThread
    :: forall appType ptcl vNumber extra a b.
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
    -> NetworkMutableState
    -> Socket.Socket
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication appType ConnectionId ptcl IO BL.ByteString a b)
    -> ErrorPolicies Socket.SockAddr ()
    -> IO Void
runServerThread NetworkServerTracers { nstMuxTracer
                                     , nstHandshakeTracer
                                     , nstErrorPolicyTracer }
                NetworkMutableState { nmsConnectionTable
                                    , nmsPeerStates }
                sd
                versionDataCodec
                acceptVersion
                versions
                errorPolicies = do
    sockAddr <- Socket.getSocketName sd
    Server.run
        nstErrorPolicyTracer
        (fromSocket nmsConnectionTable sd)
        (acceptException sockAddr)
        (beginConnection nstMuxTracer nstHandshakeTracer versionDataCodec acceptVersion (acceptConnectionTx sockAddr))
        -- register producer when application starts, it will be unregistered
        -- using 'CompleteConnection'
        (\remoteAddr thread st -> pure $ registerProducer remoteAddr thread
        st)
        completeTx mainTx (toLazyTVar nmsPeerStates)
  where
    mainTx :: Server.Main (PeerStates IO Socket.SockAddr) Void
    mainTx (ThrowException e) = throwM e
    mainTx PeerStates{}       = retry

    -- When a connection completes, we do nothing. State is ().
    -- Crucially: we don't re-throw exceptions, because doing so would
    -- bring down the server.
    completeTx :: Server.CompleteConnection
                    Socket.SockAddr
                    (PeerStates IO Socket.SockAddr)
                    (WithAddr Socket.SockAddr ErrorPolicyTrace)
                    ()
    completeTx result st = case result of

      Server.Result thread remoteAddr t (Left (SomeException e)) ->
        fmap (unregisterProducer remoteAddr thread)
          <$> completeApplicationTx errorPolicies (ApplicationError t remoteAddr e) st

      Server.Result thread remoteAddr t (Right r) ->
        fmap (unregisterProducer remoteAddr thread)
          <$> completeApplicationTx errorPolicies (ApplicationResult t remoteAddr r) st

    acceptException :: Socket.SockAddr -> IOException -> IO ()
    acceptException a e = do
      traceWith (WithAddr a `contramap` nstErrorPolicyTracer) $ ErrorPolicyAcceptException e

    acceptConnectionTx sockAddr t connAddr st = do
      d <- beforeConnectTx t connAddr st
      case d of
        AllowConnection st'    -> pure $ AcceptConnection st' (ConnectionId sockAddr connAddr) versions
        DisallowConnection st' -> pure $ RejectConnection st' (ConnectionId sockAddr connAddr)


-- |
-- Run a server application.  It will listen on the given address for incoming
-- connection.  The server thread runs using @withAsync@ function, which means
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
    bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd -> do
      addr' <- Socket.getSocketName sd
      withAsync
        (runServerThread
          tracers
          networkState
          sd
          versionDataCodec
          acceptVersion
          versions
          errorPolicies)
        (k addr')
