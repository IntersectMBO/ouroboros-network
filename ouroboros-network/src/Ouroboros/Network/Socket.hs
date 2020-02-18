{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
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
    , connectToNodeSocket
    , connectToNode'

    -- * Traces
    , NetworkConnectTracers (..)
    , nullNetworkConnectTracers
    , debuggingNetworkConnectTracers
    , NetworkServerTracers (..)
    , nullNetworkServerTracers
    , debuggingNetworkServerTracers

    -- * Helper function for creating servers
    , fromSnocket
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

    -- * Auxiliary functions
    , sockAddrFamily
    ) where

import           Cardano.Prelude (UseIsNormalForm (..))

import           Control.Concurrent.Async
import           Control.Exception (IOException, SomeException (..))
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
import           GHC.Generics (Generic)

import qualified Network.Socket as Socket

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Control.Tracer

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
import Network.Mux.DeltaQ.TraceTransformer
import qualified Network.Mux.Types as Mx
import           Network.Mux.Types (MuxBearer)

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.PeerState
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.IOManager (AssociateWithIOCP)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket
import qualified Ouroboros.Network.Server.Socket as Server
import           Ouroboros.Network.Server.ConnectionTable


-- | Tracer used by 'connectToNode' (and derivatives, like
-- 'Ouroboros.Network.NodeToNode.connectTo' or
-- 'Ouroboros.Network.NodeToClient.connectTo).
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


-- | Connection is identified by local and remote address.
--
-- TODO: the type variable which this data type fills in is called `peerid`.  We
-- should renamed to `connectionId`.
--
data ConnectionId addr = ConnectionId {
    localAddress  :: !addr,
    remoteAddress :: !addr
  }
  deriving (Eq, Ord, Show, Generic)
  deriving NoUnexpectedThunks via (UseIsNormalForm (ConnectionId addr))

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
  :: forall appType ptcl vNumber extra a b fd addr.
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
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectToNode sn versionDataCodec tracers versions localAddr remoteAddr =
    bracket
      (Snocket.openToConnect sn remoteAddr)
      (Snocket.close sn)
      (\sd -> do
          case localAddr of
            Just addr -> Snocket.bind sn sd addr
            Nothing   -> return ()
          Snocket.connect sn sd remoteAddr
          connectToNode' sn versionDataCodec tracers versions sd
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
  :: forall appType ptcl vNumber extra a b fd addr.
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
  -> fd
  -> IO ()
connectToNode' sn versionDataCodec NetworkConnectTracers {nctMuxTracer, nctHandshakeTracer } versions sd = do
    connectionId <- ConnectionId <$> Snocket.getLocalAddr sn sd <*> Snocket.getRemoteAddr sn sd
    muxTracer <- initDeltaQTracer' $ Mx.WithMuxBearer connectionId `contramap` nctMuxTracer
    let bearer = Snocket.toBearer sn muxTracer sd
    ts_start <- getMonotonicTime
    mapp <- runPeerWithByteLimit
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


-- Wraps a Socket inside a Snocket and calls connectToNode'
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
  -> Socket.Socket
  -> IO ()
connectToNodeSocket iocp versionDataCodec tracers versions sd =
    connectToNode'
      (Snocket.socketSnocket iocp)
      versionDataCodec
      tracers
      versions
      sd

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
    :: forall peerid ptcl vNumber extra addr st fd.
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
    => Snocket IO fd addr
    -> Tracer IO (Mx.WithMuxBearer peerid Mx.MuxTrace)
    -> Tracer IO (Mx.WithMuxBearer peerid (TraceSendRecv (Handshake vNumber CBOR.Term)))
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> (Time -> addr -> st -> STM.STM (AcceptConnection st vNumber extra peerid ptcl IO BL.ByteString))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr fd st ()
beginConnection sn muxTracer handshakeTracer versionDataCodec acceptVersion fn t addr st = do
    accept <- fn t addr st
    case accept of
      AcceptConnection st' peerid versions -> pure $ Server.Accept st' $ \sd -> do
        muxTracer' <- initDeltaQTracer' $ Mx.WithMuxBearer peerid `contramap` muxTracer
        let bearer :: MuxBearer IO
            bearer = Snocket.toBearer sn muxTracer' sd
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

mkListeningSocket
    :: Snocket IO fd addr
    -> Maybe addr
    -> Snocket.AddressFamily addr
    -> IO fd
mkListeningSocket sn addr family_ = do
    sd <- Snocket.open sn family_

    case addr of
      Nothing -> pure ()
      Just addr_ -> do
        Snocket.bind sn sd addr_
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
    -> Server.Socket addr fd
fromSnocket tblVar sn sd = go (Snocket.accept sn sd)
  where
    go :: Snocket.Accept addr fd -> Server.Socket addr fd
    go (Snocket.Accept accept) = Server.Socket $ do
      (sd', remoteAddr, next) <- accept
      -- TOOD: we don't need to that on each accept
      localAddr <- Snocket.getLocalAddr sn sd'
      atomically $ addConnection tblVar remoteAddr localAddr Nothing
      pure (remoteAddr, sd', close remoteAddr localAddr sd', go next)

    close remoteAddr localAddr sd' = do
        removeConnection tblVar remoteAddr localAddr
        Snocket.close sn sd'


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

debuggingNetworkServerTracers :: (Show addr, Show ptcl, Show vNumber)
                              =>  NetworkServerTracers addr ptcl vNumber
debuggingNetworkServerTracers = NetworkServerTracers {
      nstMuxTracer         = showTracing stdoutTracer,
      nstHandshakeTracer   = showTracing stdoutTracer,
      nstErrorPolicyTracer = showTracing stdoutTracer
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
    :: forall appType ptcl vNumber extra fd addr a b.
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
    => NetworkServerTracers addr ptcl vNumber
    -> NetworkMutableState addr
    -> Snocket IO fd addr
    -> fd
    -> VersionDataCodec extra CBOR.Term
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication appType (ConnectionId addr) ptcl IO BL.ByteString a b)
    -> ErrorPolicies
    -> IO Void
runServerThread NetworkServerTracers { nstMuxTracer
                                     , nstHandshakeTracer
                                     , nstErrorPolicyTracer }
                NetworkMutableState { nmsConnectionTable
                                    , nmsPeerStates }
                sn
                sd
                versionDataCodec
                acceptVersion
                versions
                errorPolicies = do
    sockAddr <- Snocket.getLocalAddr sn sd
    Server.run
        nstErrorPolicyTracer
        (fromSnocket nmsConnectionTable sn sd)
        (acceptException sockAddr)
        (beginConnection sn nstMuxTracer nstHandshakeTracer versionDataCodec acceptVersion (acceptConnectionTx sockAddr))
        -- register producer when application starts, it will be unregistered
        -- using 'CompleteConnection'
        (\remoteAddr thread st -> pure $ registerProducer remoteAddr thread
        st)
        completeTx mainTx (toLazyTVar nmsPeerStates)
  where
    mainTx :: Server.Main (PeerStates IO addr) Void
    mainTx (ThrowException e) = throwM e
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

    acceptException :: addr -> IOException -> IO ()
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
    :: forall appType ptcl vNumber extra t fd addr a b.
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
    -> NetworkMutableState addr
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
withServerNode sn tracers networkState addr versionDataCodec acceptVersion versions errorPolicies k =
    bracket (mkListeningSocket sn (Just addr) (Snocket.addrFamily sn addr)) (Snocket.close sn) $ \sd -> do
      addr' <- Snocket.getLocalAddr sn sd
      withAsync
        (runServerThread
          tracers
          networkState
          sn
          sd
          versionDataCodec
          acceptVersion
          versions
          errorPolicies)
        (k addr')
