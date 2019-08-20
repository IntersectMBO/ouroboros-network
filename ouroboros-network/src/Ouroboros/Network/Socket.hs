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
    , withServerNode
    , connectToNode
    , connectToNode'
    , connectToNodeSocket

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

import           Control.Concurrent.Async
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import qualified Control.Monad.STM as STM
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Exception (throwIO)
import qualified Codec.CBOR.Term     as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Codec.Serialise (Serialise)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import qualified Network.Socket as Socket hiding (recv)

import           Control.Tracer (Tracer)

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
import qualified Network.Mux.Types as Mx
import           Network.Mux.Types (MuxBearer)
import           Network.Mux.Interface

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket
import qualified Ouroboros.Network.Server.Socket as Server
import           Ouroboros.Network.Server.ConnectionTable

sockAddrFamily
    :: Socket.SockAddr
    -> Socket.Family
sockAddrFamily (Socket.SockAddrInet  _ _    ) = Socket.AF_INET
sockAddrFamily (Socket.SockAddrInet6 _ _ _ _) = Socket.AF_INET6
sockAddrFamily (Socket.SockAddrUnix  _      ) = Socket.AF_UNIX

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
  :: forall appType peerid ptcl vNumber extra a b channel addr.
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
  => IO channel
  -> Snocket channel addr ptcl
  -> (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
  -> Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -> Versions vNumber extra (OuroborosApplication appType peerid ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Maybe addr
  -- ^ local address; the created socket will bind to it
  -> addr
  -- ^ remote address
  -> IO ()
connectToNode createChannel sn encodeData decodeData handshakeTracer peeridFn versions localAddr remoteAddr =
    bracket
      createChannel
      (Snocket.close sn)
      (\sd -> do
          case localAddr of
            Just addr -> Snocket.bind sn sd addr
            Nothing   -> return ()
          sd' <- case Snocket.connect sn of
            Left  connect -> connect remoteAddr
            Right connect -> connect sd remoteAddr
          connectToNode' sn encodeData decodeData handshakeTracer peeridFn versions sd'
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
  :: forall appType peerid ptcl vNumber extra a b channel addr.
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
  => Snocket channel addr ptcl
  -> (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
  -> Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (addr -> addr -> peerid)
  -> Versions vNumber extra (OuroborosApplication appType peerid ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> channel
  -> IO ()
connectToNode' sn encodeData decodeData handshakeTracer peeridFn versions sd = do
    peerid <- peeridFn <$> (Snocket.getLocalAddr sn sd) <*> (Snocket.getRemoteAddr sn sd)
    bearer <- Snocket.toBearer sn sd
    Mx.muxBearerSetState bearer Mx.Connected
    mapp <- runPeerWithByteLimit
              maxTransmissionUnit
              BL.length
              handshakeTracer
              codecHandshake
              peerid
              (Mx.muxBearerAsControlChannel bearer Mx.ModeInitiator)
              (handshakeClientPeer encodeData decodeData versions)
    case mapp of
         Left err -> throwIO err
         Right app -> do
             Mx.muxBearerSetState bearer Mx.Mature
             Mx.muxStart peerid (toApplication app) bearer


-- Wraps a Socket inside a Snocket and calls connectToNode'
connectToNodeSocket
  :: forall appType peerid ptcl vNumber extra a b.
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
  => (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
  -> Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
  -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
  -> Versions vNumber extra (OuroborosApplication appType peerid ptcl IO BL.ByteString a b)
  -- ^ application to run over the connection
  -> Socket.Socket
  -> IO ()
connectToNodeSocket encodeData decodeData handshakeTracer peeridFn versions sd = do
    addr <- Socket.getSocketName sd
    let family = sockAddrFamily addr
    connectToNode' (Snocket.socketSnocket family) encodeData decodeData handshakeTracer peeridFn
                   versions sd

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
         HasResponder appType ~ True
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
    :: forall peerid ptcl vNumber extra addr st channel.
       ( Mx.ProtocolEnum ptcl
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
    => Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Snocket channel addr ptcl
    -> (addr -> st -> STM.STM (AcceptConnection st vNumber extra peerid ptcl IO BL.ByteString))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr channel st ()
beginConnection handshakeTracer encodeData decodeData acceptVersion sn fn addr st = do
    accept <- fn addr st
    case accept of
      AcceptConnection st' peerid versions -> pure $ Server.Accept st' $ \sd -> do
        (bearer :: MuxBearer ptcl IO) <- Snocket.toBearer sn sd
        Mx.muxBearerSetState bearer Mx.Connected
        mapp <- runPeerWithByteLimit
                maxTransmissionUnit
                BL.length
                handshakeTracer
                codecHandshake
                peerid
                (Mx.muxBearerAsControlChannel bearer Mx.ModeResponder)
                (handshakeServerPeer encodeData decodeData acceptVersion versions)
        case mapp of
          Left err -> throwIO err
          Right app -> do
            Mx.muxBearerSetState bearer Mx.Mature
            Mx.muxStart peerid (toApplication app) bearer
      RejectConnection st' _peerid -> pure $ Server.Reject st'

mkListeningSocket
    :: IO channel
    -> Snocket channel addr ptcl
    -> Maybe addr
    -> IO channel
mkListeningSocket createChannel sn addr = do
    sd <- createChannel

    case addr of
      Nothing -> pure ()
      Just addr_ -> do
        Snocket.bind sn sd addr_
        Snocket.listen sn sd
    pure sd

-- |
-- Make a server-compatible socket from a network socket.
--
fromSocket
    :: Ord addr
    => ConnectionTable IO addr
    -> Snocket channel addr ptcl
    -> channel
    -> Server.Socket addr channel
fromSocket tblVar sn sd = Server.Socket
    { Server.acceptConnection = do
        (sd', remoteAddr) <- Snocket.accept sn sd
        localAddr <- Snocket.getLocalAddr sn sd'
        atomically $ addConnection tblVar remoteAddr localAddr Nothing
        pure (remoteAddr, sd', close remoteAddr localAddr sd')
    }
  where
    close remoteAddr localAddr sd' = do
        removeConnection tblVar remoteAddr localAddr
        Snocket.close sn sd'


-- |
-- Thin wrapper around @'Server.run'@.
--
runNetworkNode'
    :: forall peerid ptcl st vNumber extra t channel addr.
       ( Mx.ProtocolEnum ptcl
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
       , Ord addr
       )
    => Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> ConnectionTable IO addr
    -> Snocket channel addr ptcl
    -> channel
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -- -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> (SomeException -> IO ())
    -> (addr -> st -> STM.STM (AcceptConnection st vNumber extra peerid ptcl IO BL.ByteString))
    -> Server.CompleteConnection st ()
    -> Server.Main st t
    -> st
    -> IO t
runNetworkNode' handshakeTracer tbl sn sd encodeData decodeData acceptVersion acceptException acceptConn complete
    main st = Server.run (fromSocket tbl sn sd) acceptException (beginConnection handshakeTracer encodeData decodeData
        acceptVersion sn acceptConn) complete main st


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
    :: forall appType peerid ptcl vNumber extra t channel addr a b.
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
       , Ord addr
       )
    => Tracer IO (TraceSendRecv (Handshake vNumber CBOR.Term) peerid (DecoderFailureOrTooMuchInput DeserialiseFailure))
    -> ConnectionTable IO addr
    -> IO channel
    -> Snocket channel addr ptcl
    -> addr
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (addr -> addr -> peerid)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication appType peerid ptcl IO BL.ByteString a b)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> (addr -> Async () -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode handshakeTracer tbl createChannel sn addr encodeData decodeData peeridFn acceptVersion versions k =
    bracket (mkListeningSocket createChannel sn (Just addr)) (Snocket.close sn) $ \sd -> do
      addr' <- Snocket.getLocalAddr sn sd
      withAsync
        (runNetworkNode'
          handshakeTracer
          tbl
          sn
          sd
          encodeData
          decodeData
          acceptVersion
          throwIO
          (\connAddr st ->
            pure $ AcceptConnection
                    st
                    (peeridFn addr' connAddr)
                    versions)
          complete
          main
          ()) (k addr')

    where
      main :: Server.Main () ()
      main _ = retry

      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete outcome st = case outcome of
        Left _  -> pure st
        Right _ -> pure st
