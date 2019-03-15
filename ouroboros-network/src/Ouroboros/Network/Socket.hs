{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Socket (
      runNetworkNodeWithSocket
    , runNetworkNodeWithSocket'
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import           Ouroboros.Network.Time

import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Types as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import           Ouroboros.Network.Mux.Interface ( NetworkInterface (..)
                                                 , NetworkNode (..)
                                                 , Connection (..)
                                                 , miniProtocolDescription
                                                 )

import           Text.Printf

-- |
-- Create @'MuxBearer'@ from a socket.
--
socketAsMuxBearer
  :: forall ptcl.
     Mx.ProtocolEnum ptcl
  => Socket
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
                  --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  blob <- recvLen' (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' l bufs = do
          buf <- Socket.recv sd l
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed "Socket closed when reading data" callStack
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
      closeSocket = close sd

      sduSize :: IO Word16
      sduSize = do
          -- XXX it is really not acceptable to call getSocketOption for every SDU we want to send
          mss <- getSocketOption sd MaxSegment
          -- 1260 = IPv6 min MTU minus TCP header, 8 = mux header size
          return $ fromIntegral $ max (1260 - 8) (min 0xffff (15 * mss - 8))


hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))

-- |
-- Like @'runNetworkNodeWithSocket'@ but it allows to run an action when an
-- exception is raised by the mux layer.  This is useful in tests.
--
runNetworkNodeWithSocket'
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => NetworkInterface ptcl AddrInfo IO
  -> Maybe (Maybe SomeException -> IO ())
  -> IO (NetworkNode AddrInfo IO)
runNetworkNodeWithSocket' NetworkInterface {nodeAddress, protocols} k = do
      (sd, hdl) <- startNode
      return $ NetworkNode {
          connectTo,
          killNode = closeConn sd hdl
        }
    where
      mpds :: Mx.MiniProtocolDescriptions ptcl IO
      mpds = miniProtocolDescription . protocols

      startNode :: IO (Socket, Async ())
      startNode =
          bracketOnError
            (socket (addrFamily nodeAddress) Stream defaultProtocol)
            close
            (\sd -> do
                setSocketOption sd ReuseAddr 1
                setSocketOption sd ReusePort 1
                bind sd (addrAddress nodeAddress)
                listen sd 2
                rh <- async (server sd)
                return (sd, rh)
            )
        where
          server sd = forever $ do
            (client, _) <- accept sd
            aid <- async $ larval client
            void $ async $ watcher client aid

          larval sd = do
            bearer <- socketAsMuxBearer sd
            Mx.muxBearerSetState bearer Mx.Connected
            Mx.muxStart mpds bearer k

          watcher sd aid = do
            res_e <- waitCatch aid
            case res_e of
              Left e -> do
                close sd
                sequence_ $ k <*> (Just (Just e))
              Right _ -> return ()

      connectTo :: AddrInfo -> IO (Connection IO)
      connectTo remote =
        bracketOnError
          (socket (addrFamily nodeAddress) Stream defaultProtocol)
          close
          (\sd -> do
              setSocketOption sd ReuseAddr 1
              setSocketOption sd ReusePort 1
              bind sd (addrAddress nodeAddress)
              connect sd (addrAddress remote)
              hdl <- async $ do
                bearer <- socketAsMuxBearer sd
                Mx.muxBearerSetState bearer Mx.Connected
                Mx.muxStart mpds bearer k
              return $ Connection {
                  terminate = closeConn sd hdl,
                  await     = either Just (const Nothing) <$> waitCatch hdl
                }
          )

      closeConn :: Socket -> Async () -> IO ()
      closeConn sd hdl = do
        cancel hdl
        close sd

-- |
-- Run a node using @'NetworkInterface'@ using a socket.  It will start to
-- listen on incomming connections on the supplied @'nodeAddress'@, and returns
-- @'NetworkNode'@ which let one connect to other peers (by opening a new
-- TCP connection) or shut down the node.
--
-- Note: the openned connection using @'connectTo'@ will be closed by the mux
-- layer when one of the mux threads terminates or throws an exception, see
-- @'startMux'@.
--
runNetworkNodeWithSocket
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => NetworkInterface ptcl AddrInfo IO
  -> IO (NetworkNode AddrInfo IO)
runNetworkNodeWithSocket ni = runNetworkNodeWithSocket' ni Nothing
