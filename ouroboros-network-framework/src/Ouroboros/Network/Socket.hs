{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
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
  , SomeResponderApplication (..)
  , ConnectionId (..)
  , ConnectToArgs (..)
  , connectToNode
  , connectToNodeWithMux
  , connectToNodeSocket
  , connectToNode'
  , connectToNodeWithMux'
    -- * Socket configuration
  , configureSocket
  , configureSystemdSocket
  , SystemdSocketTracer (..)
    -- * Traces
  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , debuggingNetworkConnectTracers
    -- * Re-export of HandshakeCallbacks
  , HandshakeCallbacks (..)
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
  , simpleMuxCallback
  ) where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Hashable
import Data.Monoid.Synchronisation (FirstToFinish (..))
import Data.Typeable (Typeable)
import Data.Word (Word16)

import Network.Socket (SockAddr, Socket, StructLinger (..))
import Network.Socket qualified as Socket

import Control.Tracer

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Mux.DeltaQ.TraceTransformer
import Network.TypedProtocol.Codec hiding (decode, encode)

import Ouroboros.Network.Context
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Handshake (HandshakeCallbacks (..))
import Ouroboros.Network.IOManager (IOManager)
import Ouroboros.Network.Mux
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Server.ConnectionTable
import Ouroboros.Network.Snocket (Snocket)
import Ouroboros.Network.Snocket qualified as Snocket


-- | Tracer used by 'connectToNode' (and derivatives, like
-- 'Ouroboros.Network.NodeToNode.connectTo' or
-- 'Ouroboros.Network.NodeToClient.connectTo).
--
data NetworkConnectTracers addr vNumber = NetworkConnectTracers {
      nctMuxTracer         :: Tracer IO (Mx.WithBearer (ConnectionId addr)  Mx.Trace),
      -- ^ low level mux-network tracer, which logs mux sdu (send and received)
      -- and other low level multiplexing events.
      nctHandshakeTracer   :: Tracer IO (Mx.WithBearer (ConnectionId addr)
                                          (TraceSendRecv (Handshake vNumber CBOR.Term)))
      -- ^ handshake protocol tracer; it is important for analysing version
      -- negotiation mismatches.
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
sockAddrFamily Socket.SockAddrInet {}  = Socket.AF_INET
sockAddrFamily Socket.SockAddrInet6 {} = Socket.AF_INET6
sockAddrFamily Socket.SockAddrUnix {}  = Socket.AF_UNIX


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

-- | Common arguments of various variants of `connectToNode`.
--
data ConnectToArgs fd addr vNumber vData = ConnectToArgs {
    ctaHandshakeCodec      :: Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure IO BL.ByteString,
    ctaHandshakeTimeLimits :: ProtocolTimeLimits (Handshake vNumber CBOR.Term),
    ctaVersionDataCodec    :: VersionDataCodec CBOR.Term vNumber vData,
    ctaConnectTracers      :: NetworkConnectTracers addr vNumber,
    ctaHandshakeCallbacks  :: HandshakeCallbacks vData
  }


-- | Connect to a remote node.  It is using bracket to enclose the underlying
-- socket acquisition.  This implies that when the continuation exits the
-- underlying bearer will get closed.
--
-- The connection will start with handshake protocol sending @Versions@ to the
-- remote peer.  It must fit into @'maxTransmissionUnit'@ (~5k bytes).
--
-- Exceptions thrown by 'MuxApplication' are rethrown by 'connectToNode'.
connectToNode
  :: forall muxMode vNumber vData fd addr a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator muxMode ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> ConnectToArgs fd addr vNumber vData
  -> (fd -> IO ()) -- ^ configure socket
  -> Versions vNumber vData (OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b)
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO (Either SomeException (Either a b))
connectToNode sn mkBearer args configureSock versions localAddr remoteAddr =
  connectToNodeWithMux sn mkBearer args configureSock versions localAddr remoteAddr simpleMuxCallback


-- | A version `connectToNode` which allows one to control which mini-protocols
-- to execute on a given connection.
connectToNodeWithMux
  :: forall muxMode vNumber vData fd addr a b x.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator muxMode ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> ConnectToArgs fd addr vNumber vData
  -> (fd -> IO ()) -- ^ configure socket
  -> Versions vNumber vData (OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -- ^ remote address
  -> Maybe addr
  -> addr
  -> (    ConnectionId addr
       -> vNumber
       -> vData
       -> OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b
       -> Mx.Mux muxMode IO
       -> Async IO ()
       -> IO x)
  -- ^ callback which has access to ConnectionId, negotiated protocols, mux
  -- handle created for that connection and an `Async` handle to the thread
  -- which runs `Mx.runMux`.  The `Mux` handle allows schedule mini-protocols.
  --
  -- NOTE: when the callback returns or errors, the mux thread will be killed.
  -> IO x
connectToNodeWithMux sn mkBearer args configureSock versions localAddr remoteAddr k
  =
  bracket
    (Snocket.openToConnect sn remoteAddr)
    (Snocket.close sn)
    (\sd -> do
      configureSock sd
      traverse_ (Snocket.bind sn sd) localAddr
      Snocket.connect sn sd remoteAddr
      connectToNodeWithMux' sn mkBearer args versions sd k
    )


-- | Connect to a remote node using an existing socket. It is up to to caller to
-- ensure that the socket is closed in case of an exception.
--
-- The connection will start with handshake protocol sending @Versions@ to the
-- remote peer.  It must fit into @'maxTransmissionUnit'@ (~5k bytes).
--
-- Exceptions thrown by @'MuxApplication'@ are rethrown by @'connectTo'@.
connectToNode'
  :: forall muxMode vNumber vData fd addr a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator muxMode ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> ConnectToArgs fd addr vNumber vData
  -- ^ a configured socket to use to connect to a remote service provider
  -> Versions vNumber vData (OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -> fd
  -> IO (Either SomeException (Either a b))
connectToNode' sn mkBearer args versions as =
  connectToNodeWithMux' sn mkBearer args versions as simpleMuxCallback


connectToNodeWithMux'
  :: forall muxMode vNumber vData fd addr a b x.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator muxMode ~ True
     )
  => Snocket IO fd addr
  -> Mx.MakeBearer IO fd
  -> ConnectToArgs fd addr vNumber vData
  -> Versions vNumber vData (OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -- ^ a configured socket to use to connect to a remote service provider
  -> fd
  -> (    ConnectionId addr
       -> vNumber
       -> vData
       -> OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString IO a b
       -> Mx.Mux muxMode IO
       -> Async IO ()
       -> IO x)
  -- ^ callback which has access to ConnectionId, negotiated protocols, mux
  -- handle created for that connection and an `Async` handle to the thread
  -- which runs `Mx.runMux`.  The `Mux` handle allows schedule mini-protocols.
  --
  -- NOTE: when the callback returns or errors, the mux thread will be killed.
  -> IO x
connectToNodeWithMux'
  sn makeBearer
  ConnectToArgs {
      ctaHandshakeCodec      = handshakeCodec,
      ctaHandshakeTimeLimits = handshakeTimeLimits,
      ctaVersionDataCodec    = versionDataCodec,
      ctaConnectTracers      =
        NetworkConnectTracers {
          nctMuxTracer,
          nctHandshakeTracer
        },
      ctaHandshakeCallbacks  = handshakeCallbacks
  }
  versions sd k = do
    connectionId <- (\localAddress remoteAddress -> ConnectionId { localAddress, remoteAddress })
                <$> Snocket.getLocalAddr sn sd <*> Snocket.getRemoteAddr sn sd
    muxTracer <- initDeltaQTracer' $ Mx.WithBearer connectionId `contramap` nctMuxTracer
    ts_start <- getMonotonicTime

    handshakeBearer <- Mx.getBearer makeBearer sduHandshakeTimeout muxTracer sd Nothing
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
         traceWith muxTracer $ Mx.TraceHandshakeClientError err (diffTime ts_end ts_start)
         throwIO err

       Left (HandshakeProtocolError err) -> do
         traceWith muxTracer $ Mx.TraceHandshakeClientError err (diffTime ts_end ts_start)
         throwIO err

       Right (HandshakeNegotiationResult app versionNumber agreedOptions) -> do
         traceWith muxTracer $ Mx.TraceHandshakeClientEnd (diffTime ts_end ts_start)
         Mx.withReadBufferIO (\buffer -> do
             bearer <- Mx.getBearer makeBearer sduTimeout muxTracer sd buffer
             mux <- Mx.new (toMiniProtocolInfos (runForkPolicy noBindForkPolicy remoteAddress) app)
             withAsync (Mx.run muxTracer mux bearer) $ \aid ->
               k connectionId versionNumber agreedOptions app mux aid
           )

       Right (HandshakeQueryResult _vMap) -> do
         traceWith muxTracer $ Mx.TraceHandshakeClientEnd (diffTime ts_end ts_start)
         throwIO (QueryNotSupported @vNumber)


-- | An internal mux callback which starts all mini-protocols and blocks
-- until the first one terminates.  It returns the result (or error) of the
-- first terminated mini-protocol.
--
simpleMuxCallback
  :: forall muxMode addr vNumber vData m a b.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadSTM   m
     , MonadThrow m
     , MonadThrow (STM m)
     )
  => ConnectionId addr
  -> vNumber
  -> vData
  -> OuroborosApplicationWithMinimalCtx muxMode addr BL.ByteString m a b
  -> Mx.Mux muxMode m
  -> Async m ()
  -> m (Either SomeException (Either a b))
simpleMuxCallback connectionId _ _ app mux aid = do
    let initCtx = MinimalInitiatorContext connectionId
        respCtx = ResponderContext connectionId

    resOps <- sequence
      [ Mx.runMiniProtocol
          mux
          miniProtocolNum
          miniProtocolDir
          Mx.StartEagerly
          action
      | MiniProtocol{miniProtocolNum, miniProtocolRun}
          <- getOuroborosApplication app
      , (miniProtocolDir, action) <-
          case miniProtocolRun of
            InitiatorProtocolOnly initiator ->
              [(Mx.InitiatorDirectionOnly, fmap (first Left) . runMiniProtocolCb initiator initCtx)]
            ResponderProtocolOnly responder ->
              [(Mx.ResponderDirectionOnly, fmap (first Right) . runMiniProtocolCb responder respCtx)]
            InitiatorAndResponderProtocol initiator responder ->
              [(Mx.InitiatorDirection, fmap (first Left) . runMiniProtocolCb initiator initCtx)
              ,(Mx.ResponderDirection, fmap (first Right) . runMiniProtocolCb responder respCtx)]
      ]

    -- Wait for the first MuxApplication to finish, then stop the mux.
    r <- waitOnAny resOps
    Mx.stop mux
    wait aid
    return r
  where
    waitOnAny :: [STM m (Either SomeException x)] -> m (Either SomeException x)
    waitOnAny = atomically . runFirstToFinish . foldMap FirstToFinish


-- Wraps a Socket inside a Snocket and calls connectToNode'
connectToNodeSocket
  :: forall muxMode vNumber vData a b.
     ( Ord vNumber
     , Typeable vNumber
     , Show vNumber
     , Mx.HasInitiator muxMode ~ True
     )
  => IOManager
  -> ConnectToArgs Socket.Socket Socket.SockAddr vNumber vData
  -> Versions vNumber vData (OuroborosApplicationWithMinimalCtx muxMode Socket.SockAddr BL.ByteString IO a b)
  -- ^ application to run over the connection
  -> Socket.Socket
  -> IO (Either SomeException (Either a b))
connectToNodeSocket iocp args versions sd =
    connectToNode'
      (Snocket.socketSnocket iocp)
      Mx.makeSocketBearer
      args
      versions
      sd

-- |
-- Wrapper for OuroborosResponderApplication and OuroborosInitiatorAndResponderApplication.
--
data SomeResponderApplication addr bytes m b where
     SomeResponderApplication
       :: forall muxMode addr bytes m a b.
          Mx.HasResponder muxMode ~ True
       => (OuroborosApplicationWithMinimalCtx muxMode addr bytes m a b)
       -> SomeResponderApplication addr bytes m b

