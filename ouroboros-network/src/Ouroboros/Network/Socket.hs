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
    , connectTo

    -- * Helper function for creating servers
    , socketAsMuxBearer
    , fromSocket
    , beginConnection

    -- * Auxiliary functions
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad (when, void)
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
  => MuxApplication ClientApp ptcl IO
  -- ^ application to run over the connection
  -> Socket.AddrInfo
  -- ^ address of the peer we want to connect to
  -> IO ()
connectTo app remoteAddr =
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
          Mx.muxStart mpds bearer
            `catch`
            handleMuxError
      )
    where
      mpds :: Mx.MiniProtocolDescriptions ptcl IO
      mpds = \ptcl -> Mx.MiniProtocolDescription {
          Mx.mpdInitiator = Just (fmap void $ clientApplication app ptcl),
          Mx.mpdResponder = Nothing
        }


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


-- Accept every incoming connection and use the socket as a mux bearer
-- to run the mini protocols.
beginConnection
    :: ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => MuxApplication appType ptcl IO
    -> (st -> STM.STM (Either st st)) -- either accept or reject a connection.
    -> Server.BeginConnection addr Socket.Socket st ()
beginConnection app acceptConn _sockAddr st = do
    x <- acceptConn st
    case x of
      Right st' -> pure $ Server.Accept st' $ \sd -> do
        bearer <- socketAsMuxBearer sd
        Mx.muxBearerSetState bearer Mx.Connected
        Mx.muxStart (miniProtocolDescription app) bearer
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
    :: forall ptcl appType st t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       )
    => Socket.Socket
    -> MuxApplication appType ptcl IO
    -> (SomeException -> IO ())
    -> (st -> STM.STM (Either st st))
    -> Server.CompleteConnection st ()
    -> Server.Main st t
    -> st
    -> IO t
runNetworkNode' sd app acceptException acceptConn complete main st =
    Server.run (fromSocket sd) acceptException (beginConnection app acceptConn) complete main st


-- |
-- Run a server application.  It will listen on the given address for incoming
-- connection.
--
withServerNode
    :: forall ptcl appType t.
       ( Mx.ProtocolEnum ptcl
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , Mx.MiniProtocolLimits ptcl
       , HasServer appType ~ True
       )
    => Socket.AddrInfo
    -> MuxApplication appType ptcl IO
    -> (IO () -> Async () -> IO t)
    -- ^ callback which takes @IO@ action which will terminate the server.
    -> IO t
withServerNode addr app k =
    bracket (mkListeningSocket (Socket.addrFamily addr) (Just $ Socket.addrAddress addr)) Socket.close $ \sd -> do

      killVar <- newEmptyTMVarM
      let main :: Server.Main () ()
          main _ = takeTMVar killVar

          killNode :: IO ()
          killNode = atomically $ putTMVar killVar ()

      withAsync
        (runNetworkNode' sd app throwIO (pure . Right) complete main ())
        (\aid -> k killNode aid)

    where
      -- When a connection completes, we do nothing. State is ().
      -- Crucially: we don't re-throw exceptions, because doing so would
      -- bring down the server.
      complete outcome st = case outcome of
        Left _  -> pure st
        Right _ -> pure st
