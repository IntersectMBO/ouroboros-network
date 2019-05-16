{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module exports interface for running a node over a socket over TCP \/ IP.
--
module Ouroboros.Network.Socket (
      withNetworkNode
    , withConnection
    , socketAsMuxBearer

    -- * Auxiliary functions
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad (when)
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
import           Ouroboros.Network.Mux.Interface ( Connection (..)
                                                 , WithConnection
                                                 , NetworkInterface (..)
                                                 , MuxApplication
                                                 , NetworkNode (..)
                                                 , miniProtocolDescription
                                                 )

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
              -- @'Ouroboros.Network.Mux.Ingress.demux'@ will read even after
              -- receiving the terminal message.  In this case,
              -- indeterministically, this exception it thrown.  The
              -- indeterminism kicks since server might kill mux threads
              -- before it tries to read data.
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
withConnection
  :: forall ptcl r.
     Mx.ProtocolEnum ptcl
  => Ord ptcl
  => Enum ptcl
  => Bounded ptcl
  => MuxApplication ptcl IO
  -> WithConnection IO Socket.AddrInfo (Connection IO) r
withConnection app remoteAddr kConn =
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
          bearer <- socketAsMuxBearer sd
          Mx.muxBearerSetState bearer Mx.Connected
          kConn $ Connection {
              runConnection =
                Mx.muxStart mpds bearer
                `catch`
                handleMuxError
            }
      )
    where
      mpds :: Mx.MiniProtocolDescriptions ptcl IO
      mpds = miniProtocolDescription app

      -- catch @'MuxBearerClosed'@ exception; we should ignore it and let @kConn@
      -- finish; @connect@ will close the underlying socket.
      --
      -- Note: we do it only for initiated connections, not ones that the server
      -- accepted.  The assymetry comes simply from the fact that in initiated
      -- connections we might want to run a computation that is not interrupted
      -- by a normal shutdown (a received terminal message throws
      -- @'Mx.MuxBearerClosed'@ exception).
      handleMuxError :: Mx.MuxError -> IO ()
      handleMuxError Mx.MuxError { Mx.errorType = Mx.MuxBearerClosed } = return ()
      handleMuxError e                                                 = throwIO e

-- |
-- Run a node using @'NetworkInterface'@ using a socket.  It will start to
-- listen on incomming connections on the supplied @'nodeAddress'@, and returns
-- @'NetworkNode'@ which let one connect to other peers (by opening a new
-- TCP connection) or shut down the node.
--
-- When connecting to a remote node using @connectTo@ th
withNetworkNode
  :: forall ptcl t r.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => NetworkInterface ptcl Socket.AddrInfo IO
  -> (NetworkNode Socket.AddrInfo IO r -> IO t)
  -> IO t
withNetworkNode NetworkInterface {nodeAddress, nodeApplication} k =
    bracket mkSocket Socket.close $ \sd -> do

      killVar <- newEmptyTMVarM
      let main :: Server.Main () ()
          main _ = takeTMVar killVar

          killNode :: IO ()
          killNode = atomically $ putTMVar killVar ()

          node = NetworkNode { connect = withConnection nodeApplication, killNode }

      withAsync
        (Server.run (fromSocket sd) throwIO acceptConnection complete main ())
        (\_ -> k node)

  where

    mpds :: Mx.MiniProtocolDescriptions ptcl IO
    mpds = miniProtocolDescription nodeApplication

    -- Make the server listening socket
    mkSocket :: IO Socket.Socket
    mkSocket = do
      sd <- Socket.socket (Socket.addrFamily nodeAddress) Socket.Stream Socket.defaultProtocol
      when (Socket.addrFamily nodeAddress == Socket.AF_INET ||
            Socket.addrFamily nodeAddress == Socket.AF_INET6) $ do
          Socket.setSocketOption sd Socket.ReuseAddr 1
#if !defined(mingw32_HOST_OS)
          Socket.setSocketOption sd Socket.ReusePort 1
#endif
      Socket.bind sd (Socket.addrAddress nodeAddress)
      Socket.listen sd 1
      pure sd

    -- Accept every incoming connection and use the socket as a mux bearer
    -- to run the mini protocols.
    acceptConnection :: Server.BeginConnection addr Socket.Socket () ()
    acceptConnection _sockAddr st = pure $ Server.Accept st $ \sd -> do
      bearer <- socketAsMuxBearer sd
      Mx.muxBearerSetState bearer Mx.Connected
      Mx.muxStart mpds bearer

    -- When a connection completes, we do nothing. State is ().
    -- Crucially: we don't re-throw exceptions, because doing so would
    -- bring down the server.
    complete outcome st = case outcome of
      Left _  -> pure st
      Right _ -> pure st

    -- Make a server-compatibile socket from a network socket.
    fromSocket :: Socket.Socket -> Server.Socket Socket.SockAddr Socket.Socket
    fromSocket sd = Server.Socket
      { Server.acceptConnection = do
          (sd', addr) <- Socket.accept sd
          pure (addr, sd', Socket.close sd')
      }
