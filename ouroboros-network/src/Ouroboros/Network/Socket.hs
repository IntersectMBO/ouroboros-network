{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module exports interface for running a node over a socket over TCP \/ IP.
--
module Ouroboros.Network.Socket (
    -- * High level socket interface
      withServerNode
    , withSimpleServerNode
    , connectTo

    -- * Helper function for creating servers
    , socketAsMuxBearer
    , fromSocket
    , beginConnection

    -- * Auxiliary functions
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad (when)
-- TODO: remove this, it will not be needed when `orElse` PR will be merged.
import qualified Control.Monad.STM as STM
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (throwIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import           Ouroboros.Network.Time

import qualified Ouroboros.Network.Server.Socket as Server
import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Types as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import           Ouroboros.Network.Mux.Interface

import           Text.Printf

-- |
-- Create @'MuxBearer'@ from a socket.
--
socketAsMuxBearer
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Socket.Socket
  -> IO (MuxBearer ptcl IO)
socketAsMuxBearer sd = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read    = readSocket,
          Mx.write   = writeSocket,
          Mx.close   = closeSocket,
          Mx.sduSize = sduSize,
          Mx.state   = mxState
        }
    where
      readSocket :: (HasCallStack) => IO (Mx.MuxSDU ptcl, Time IO)
      readSocket = do
          hbuf <- recvLen' 8 []
          --say "read"
          --hexDump hbuf ""
          case Mx.decodeMuxSDUHeader hbuf of
              Left  e      -> throwM e
              Right header -> do
                  -- say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  blob <- recvLen' (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return (BL.concat $ reverse bufs)
      recvLen' l bufs = do
          buf <- Socket.recv sd l
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed (show sd ++ " closed when reading data") callStack
              else recvLen' (l - fromIntegral (BL.length buf)) (buf : bufs)

      writeSocket :: Mx.MuxSDU ptcl -> IO (Time IO)
      writeSocket sdu = do
          --say "write"
          ts <- getMonotonicTime
          let ts32 = timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          --hexDump buf ""
          Socket.sendAll sd buf
          return ts

      closeSocket :: IO ()
      closeSocket = Socket.close sd

      sduSize :: IO Word16
#if defined(mingw32_HOST_OS)
      sduSize = return 13000 -- MaxSegment isn't supported on Windows
#else
      sduSize = do
          -- XXX it is really not acceptable to call getSocketOption for every SDU we want to send
          {- getSocketOption for MaxSegment is not supported for AF_UNIX sockets -}
          addr <- Socket.getSocketName sd
          case addr of
               Socket.SockAddrUnix _ -> return 0xffff
               _                     -> do
                    mss <- Socket.getSocketOption sd Socket.MaxSegment
                    -- 1260 = IPv6 min MTU minus TCP header, 8 = mux header size
                    return $ fromIntegral $ max (1260 - 8) (min 0xffff (15 * mss - 8))
#endif

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))


-- |
-- Connect to a remote node.  It is using bracket to enclose the underlying
-- socket acquisition.  This implies that when the continuation exits the
-- underlying bearer will get closed.
connectTo
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Show ptcl
     , Mx.MiniProtocolLimits ptcl
     )
  => MuxApplication InitiatorApp ptcl IO
  -- ^ application to run over the connection
  -> Socket.AddrInfo
  -- ^ local address; the created socket will bind to it
  -> Socket.AddrInfo
  -- ^ remote address
  -> IO ()
connectTo app localAddr remoteAddr =
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
          Socket.bind sd (Socket.addrAddress localAddr)
          Socket.connect sd (Socket.addrAddress remoteAddr)
          bearer <- socketAsMuxBearer sd
          Mx.muxBearerSetState bearer Mx.Connected
          Mx.muxStart app bearer
      )

-- |
-- A mux application which has a server component.
--
data AnyMuxResponderApp ptcl m where
      AnyMuxResponderApp :: forall appType ptcl m. HasResponder appType ~ True => MuxApplication appType ptcl m -> AnyMuxResponderApp ptcl m


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
data AcceptConnection st ptcl m where
    AcceptConnection :: !st -> AnyMuxResponderApp ptcl m -> AcceptConnection st ptcl m
    RejectConnection :: !st -> AcceptConnection st ptcl m


-- |
-- Accept or reject incoming connection based on the current state and address
-- of the incoming connection.
--
beginConnection
    :: forall ptcl addr st.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => (addr -> st -> STM.STM (AcceptConnection st ptcl IO))
    -- ^ either accept or reject a connection.
    -> Server.BeginConnection addr Socket.Socket st ()
beginConnection fn addr st = do
    accept <- fn addr st
    case accept of
      AcceptConnection st' anyApp -> pure $ Server.Accept st' $ \sd ->
        case anyApp of
          AnyMuxResponderApp app -> do
            bearer <- socketAsMuxBearer sd
            Mx.muxBearerSetState bearer Mx.Connected
            Mx.muxStart app bearer
      RejectConnection st' -> pure $ Server.Reject st'


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
    :: Socket.Socket
    -> Server.Socket Socket.SockAddr Socket.Socket
fromSocket sd = Server.Socket
    { Server.acceptConnection = do
        (sd', addr) <- Socket.accept sd
        pure (addr, sd', Socket.close sd')
    }


-- |
-- Thin wrapper around @'Server.run'@.
--
runNetworkNode'
    :: forall ptcl st t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => Socket.Socket
    -> (SomeException -> IO ())
    -> (Socket.SockAddr -> st -> STM.STM (AcceptConnection st ptcl IO))
    -> Server.CompleteConnection st ()
    -> Server.Main st t
    -> st
    -> IO t
runNetworkNode' sd acceptException acceptConn complete main st =
    Server.run (fromSocket sd) acceptException (beginConnection acceptConn) complete main st


-- |
-- Run a server application.  It will listen on the given address for incoming
-- connection.  The server thread runs using @withAsync@ function, which means
-- that it will terminate when the callback terminates or throws an exception.
--
-- TODO: we should track connections in the state and refuse connections from
-- peers we are already connected to.  This is also the right place to ban
-- connection from peers which missbehaved.
withServerNode
    :: forall ptcl t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => Socket.AddrInfo
    -> AnyMuxResponderApp ptcl IO
    -- ^ The mux application that will be run on each incoming connection from
    -- a given address.  Note that if @'MuxClientAndServerApplication'@ is
    -- returned, the connection will run a full duplex set of mini-protocols.
    -> (Async () -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode addr app k =
    bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd -> do

      let main :: Server.Main () ()
          main _ = retry

      withAsync
        (runNetworkNode'
          sd throwIO
          (\_connAddr st -> pure $ AcceptConnection st app)
          complete
          main
          ()) k

    where
      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete outcome st = case outcome of
        Left _  -> pure st
        Right _ -> pure st


-- |
-- Like @'withServerNode'@ but always runs only server side on an incoming
-- connection.
--
withSimpleServerNode
    :: forall ptcl t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => Socket.AddrInfo
    -> MuxApplication ResponderApp ptcl IO
    -- ^ The mux server application that will be run on each incoming
    -- connection.
    -> (Async () -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withSimpleServerNode addr app k = withServerNode addr (AnyMuxResponderApp app) k
