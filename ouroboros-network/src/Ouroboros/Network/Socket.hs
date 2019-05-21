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
      serverNode
    , withServerNode
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
import qualified Codec.CBOR.Term     as CBOR
import           Codec.Serialise (Serialise)
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.ByteChannel


import           Ouroboros.Network.Time
import           Ouroboros.Network.ByteChannel (socketAsByteChannel)
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec

import qualified Ouroboros.Network.Server.Socket as Server
import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Types as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import           Ouroboros.Network.Mux.Interface ( MuxApplication
                                                 , AppType (..)
                                                 )

import           Text.Printf

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.  If the limit is exceeded, then @'NotEnoughInput'@ exception will
-- be thrown.
--
maxTransmissionUnit :: Int
maxTransmissionUnit = 4 * 1440

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
-- socket aquisition.  This implies that when the continuation exits the
-- underlying bearer will get closed.
--
-- The connection will start with handshake protocol sending @Versions@ to the
-- remote peer.  It must fit into @'maxTransmissionUnit'@ (~5k bytes).
--
connectTo
  :: forall ptcl vNumber extra.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     , Ord vNumber
     , Enum vNumber
     , Serialise vNumber
     , Typeable vNumber
     , Show vNumber
     )
  => (forall vData. extra vData -> vData -> CBOR.Term)
  -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
  -> Versions vNumber extra (MuxApplication ClientApp ptcl IO)
  -- ^ application to run over the connection
  -> Socket.AddrInfo
  -- ^ address of the peer we want to connect to
  -> IO ()
connectTo encodeData decodeData versions remoteAddr =
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
          Socket.connect sd (Socket.addrAddress remoteAddr)
          mapp <- runPeerL maxTransmissionUnit
                           nullTracer
                           codecHandshake
                           (socketAsByteChannel sd)
                           (handshakeClientPeer encodeData decodeData versions)
          case mapp of
            Left err -> throwIO err
            Right app -> do
              bearer <- socketAsMuxBearer sd
              Mx.muxBearerSetState bearer Mx.Connected
              Mx.muxStart app bearer
      )


-- Accept every incoming connection and use the socket as a mux bearer
-- to run the mini protocols.
beginConnection
    :: ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> (st -> STM.STM (Either st st)) -- either accept or reject a connection.
    -> Server.BeginConnection addr Socket.Socket st ()
beginConnection encodeData decodeData acceptVersion versions acceptConn _sockAddr st = do
    x <- acceptConn st
    case x of
      Right st' -> pure $ Server.Accept st' $ \sd -> do
        mapp <- runPeerL maxTransmissionUnit
                         nullTracer
                         codecHandshake
                         (socketAsByteChannel sd)
                         (handshakeServerPeer encodeData decodeData acceptVersion versions)
        case mapp of
          Left err -> throwIO err
          Right app -> do
            bearer <- socketAsMuxBearer sd
            Mx.muxBearerSetState bearer Mx.Connected
            Mx.muxStart app bearer
      Left st' -> pure $ Server.Reject st'


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
-- Make a server-compatibile socket from a network socket.
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
    :: forall ptcl st vNumber extra t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => Socket.Socket
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> (SomeException -> IO ())
    -> (st -> STM.STM (Either st st))
    -> Server.CompleteConnection st ()
    -> Server.Main st t
    -> st
    -> IO t
runNetworkNode' sd encodeData decodeData acceptVersion versions acceptException acceptConn complete main st =
    Server.run (fromSocket sd) acceptException (beginConnection encodeData decodeData acceptVersion versions acceptConn) complete main st


-- |
-- Creates a socket and runs a server in the current thread.   It will accept
-- connections in a new thread using given @MuxApplication@.
--
serverNode
    :: forall ptcl vNumber extra.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => Socket.AddrInfo
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> IO ()
serverNode addr encodeData decodeData acceptVersion versions =
  bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd ->
    runNetworkNode' sd encodeData decodeData acceptVersion versions throwIO (pure . Right) complete  main ()
    where
      main :: Server.Main () ()
      main _ = retry

      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete outcome st = case outcome of
        Left _  -> pure st
        Right _ -> pure st


-- |
-- Run a server application.  It will listen on the given address for incoming
-- connection.  The server thread runs using @withAsync@ function, which means
-- that it will terminate when the callback terminates or throws an exception.
--
-- The server will run handshake protocol on each incoming connection.  We
-- assume that each versin negotiation message should fit into
-- @'maxTransmissionUnit'@ (~5k bytes).
--
-- Note: it will open a socket in the current thread and pass it to the spawned
-- thread which runs the server.  This makes it useful for testing, where we
-- need to guarantee that a socket is open before we try to connect to it.
withServerNode
    :: forall ptcl vNumber extra t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Ord vNumber
       , Enum vNumber
       , Serialise vNumber
       , Typeable vNumber
       , Show vNumber
       )
    => Socket.AddrInfo
    -> (forall vData. extra vData -> vData -> CBOR.Term)
    -> (forall vData. extra vData -> CBOR.Term -> Either Text vData)
    -> (forall vData. extra vData -> vData -> vData -> Accept)
    -> Versions vNumber extra (MuxApplication ServerApp ptcl IO)
    -> (Async () -> IO t)
    -- ^ callback which takes the @Async@ of the thread that is running the server.
    -- Note: the server thread will terminate when the callback returns or
    -- throws an exception.
    -> IO t
withServerNode addr encodeData decodeData acceptVersion versions k =
    bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd -> do

      let main :: Server.Main () ()
          main _ = retry

      withAsync
        (runNetworkNode' sd encodeData decodeData acceptVersion versions throwIO (pure . Right) complete main ()) k

    where
      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete outcome st = case outcome of
        Left _  -> pure st
        Right _ -> pure st
