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
      AnyResponderApp (..)
    , ConnectionTable
    , ConnectionTableRef (..)
    , ValencyCounter
    , withServerNode
    , withSimpleServerNode
    , connectToNode
    , connectToNode'

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
import           Control.Exception (SomeException (..))
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
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import qualified Network.Socket as Socket hiding (recv)

import           Control.Tracer (nullTracer, Tracer)

import           Network.TypedProtocol.Driver.ByteLimit
import           Network.TypedProtocol.Driver (TraceSendRecv)

import qualified Network.Mux as Mx
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
import qualified Ouroboros.Network.Server.Socket as Server
import           Ouroboros.Network.Server.ConnectionTable

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
  -> Maybe Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectToNode encodeData decodeData tracer peeridFn versions localAddr remoteAddr =
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
            Just addr -> Socket.bind sd (Socket.addrAddress addr)
            Nothing   -> return ()
          Socket.connect sd (Socket.addrAddress remoteAddr)
          connectToNode' encodeData decodeData tracer peeridFn versions sd
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
connectToNode' encodeData decodeData tracer peeridFn versions sd = do
    peerid <- peeridFn <$> Socket.getSocketName sd <*> Socket.getPeerName sd
    bearer <- Mx.socketAsMuxBearer sd
    Mx.muxBearerSetState bearer Mx.Connected
    mapp <- runPeerWithByteLimit
              maxTransmissionUnit
              BL.length
              tracer
              codecHandshake
              peerid
              (Mx.muxBearerAsControlChannel bearer Mx.ModeInitiator)
              (handshakeClientPeer encodeData decodeData versions)
    case mapp of
         Left err -> throwIO err
         Right app -> do
             Mx.muxBearerSetState bearer Mx.Mature
             Mx.muxStart peerid (toApplication app) bearer

-- |
-- A mux application which has a server component.
--
data AnyResponderApp peerid ptcl m bytes where
      AnyResponderApp
        :: forall appType peerid ptcl m bytes a b.
           HasResponder appType ~ True
        => OuroborosApplication appType peerid ptcl m bytes a b
        -> AnyResponderApp peerid ptcl m bytes


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
      :: !st
      -> !peerid
      -> Versions vNumber extra (AnyResponderApp peerid ptcl m bytes)
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
    => (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> (Time IO -> addr -> st -> STM.STM (AcceptConnection st vNumber extra peerid ptcl IO BL.ByteString))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr Socket.Socket st ()
beginConnection encodeData decodeData acceptVersion fn t addr st = do
    accept <- fn t addr st
    case accept of
      AcceptConnection st' peerid versions -> pure $ Server.Accept st' $ \sd -> do
        (bearer :: MuxBearer ptcl IO) <- Mx.socketAsMuxBearer sd
        Mx.muxBearerSetState bearer Mx.Connected
        mapp <- runPeerWithByteLimit
                maxTransmissionUnit
                BL.length
                nullTracer
                codecHandshake
                peerid
                (Mx.muxBearerAsControlChannel bearer Mx.ModeResponder)
                (handshakeServerPeer encodeData decodeData acceptVersion versions)
        case mapp of
          Left err -> throwIO err
          Right (AnyResponderApp app) -> do
            Mx.muxBearerSetState bearer Mx.Mature
            Mx.muxStart peerid (toApplication app) bearer
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


-- |
-- Thin wrapper around @'Server.run'@.
--
runNetworkNode'
    :: forall peerid ptcl vNumber extra t.
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
    => ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
    -> Socket.Socket
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -- -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> (SomeException -> IO ())
    -> (Time IO
          -> Socket.SockAddr
          -> PeerStates IO Socket.SockAddr (Time IO)
          -> STM.STM
              (AcceptConnection
                (PeerStates IO Socket.SockAddr (Time IO))
                vNumber extra peerid ptcl IO BL.ByteString))
    -> Server.ApplicationStart Socket.SockAddr (PeerStates IO Socket.SockAddr (Time IO))
    -> Server.CompleteConnection Socket.SockAddr (PeerStates IO Socket.SockAddr (Time IO)) ()
    -> Server.Main (PeerStates IO Socket.SockAddr (Time IO)) t
    -> IO t
runNetworkNode' tbl stVar sd encodeData decodeData acceptVersion acceptException acceptConn applicationStart complete
    main = Server.run
        (fromSocket tbl sd)
        acceptException
        (beginConnection encodeData decodeData acceptVersion acceptConn)
        applicationStart
        complete main (toLazyTVar stVar)


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
    :: forall peerid ptcl vNumber extra t.
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
    => Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
    -> Socket.AddrInfo
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (AnyResponderApp peerid ptcl IO BL.ByteString)
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> ErrorPolicies IO Socket.SockAddr ()
    -> (Socket.SockAddr -> Async () -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode errTracer tbl stVar addr encodeData decodeData peeridFn acceptVersion versions errPolicies k =
    bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd -> do
      addr' <- Socket.getSocketName sd
      withAsync
        (runNetworkNode'
          tbl
          stVar
          sd
          encodeData
          decodeData
          acceptVersion
          throwIO
          (\t connAddr st -> do
            d <- beforeConnectTx t connAddr st
            case d of
              AllowConnection st'    -> pure $ AcceptConnection st' (peeridFn addr' connAddr) versions
              DisallowConnection st' -> pure $ RejectConnection st' (peeridFn addr' connAddr))
              -- register producer when application starts, it will be
              -- unregistered using 'CompleteConnection'
          (\remoteAddr thread st -> pure $ registerProducer remoteAddr thread
          st)
          complete
          main) (k addr')

    where
      main :: Server.Main (PeerStates IO Socket.SockAddr (Time IO)) ()
      main (ThrowException e) = throwM e
      main PeerStates{}       = retry

      completeTx :: CompleteApplication IO
                      (PeerStates IO Socket.SockAddr (Time IO))
                      Socket.SockAddr
                      ()
      completeTx = completeApplicationTx errTracer errPolicies

      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete :: Server.CompleteConnection
                    Socket.SockAddr
                    (PeerStates IO Socket.SockAddr (Time IO))
                    ()
      complete result st = case result of
        Server.Result thread remoteAddr t (Left (SomeException e)) -> do
          (st', io) <- completeTx (ApplicationError t remoteAddr e) st
          pure $ (unregisterProducer remoteAddr thread st', io)
        Server.Result thread remoteAddr t (Right r) -> do
          (st', io) <- completeTx (ApplicationResult t remoteAddr r) st
          pure $ (unregisterProducer remoteAddr thread st', io)


-- |
-- Like @'withServerNode'@ but always runs only server side on an incoming
-- connection.
--
withSimpleServerNode
    :: forall peerid ptcl vNumber extra t a b.
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
    => Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> ConnectionTable IO Socket.SockAddr
    -> StrictTVar IO (PeerStates IO Socket.SockAddr (Time IO))
    -> Socket.AddrInfo
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (Socket.SockAddr -> Socket.SockAddr -> peerid)
    -- ^ create peerid from local address and remote address
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (OuroborosApplication ResponderApp peerid ptcl IO BL.ByteString a b)
    -- ^ The mux server application that will be run on each incoming
    -- connection.
    -> ErrorPolicies IO Socket.SockAddr ()
    -> (Socket.SockAddr -> Async () -> IO t)
    -- ^ callback which takes the local address that the server bound to along with the @Async@ of
    -- the thread that is running the server
    --
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withSimpleServerNode errTracer tbl stVar addr encodeData decodeData peeridFn acceptVersion versions errPolicies k = withServerNode errTracer tbl stVar addr encodeData decodeData peeridFn acceptVersion (AnyResponderApp <$> versions) errPolicies k
