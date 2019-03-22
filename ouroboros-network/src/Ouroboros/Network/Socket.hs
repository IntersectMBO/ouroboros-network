{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Socket (
      killResponder
    , startInitiator
    , startInitiatorT
    , startResponder
    , startResponderT
    , hexDump
    ) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           GHC.Stack
import           Network.Socket hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import qualified Ouroboros.Network.Mux as Mx
import qualified Ouroboros.Network.Mux.Types as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import qualified Ouroboros.Network.Mux.Control as Mx

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
          let sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel $ fromIntegral $ ts .&. 0xffffffff }
              buf = Mx.encodeMuxSDU sdu'
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


startResponder :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
               => [Mx.SomeMuxVersion]
               -> (Mx.SomeMuxVersion -> Maybe (Mx.MiniProtocolDescriptions ptcl IO))
               -> AddrInfo
               -> IO (Socket, Async ())
startResponder versions mpds addr = startResponderT versions mpds addr Nothing

startResponderT :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
                => [Mx.SomeMuxVersion]
                -> (Mx.SomeMuxVersion -> Maybe (Mx.MiniProtocolDescriptions ptcl IO))
                -> AddrInfo
                -> Maybe (Maybe SomeException -> IO ())
                -> IO (Socket, Async ())
startResponderT versions mpds addr rescb_m =
    bracketOnError
        (socket (addrFamily addr) Stream defaultProtocol)
        close
        (\sd -> do
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress addr)
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
        Mx.muxStart versions mpds bearer Mx.StyleServer rescb_m

    watcher sd aid = do
        res_e <- waitCatch aid
        case rescb_m of
             Nothing ->
                 case res_e of
                      Left  e -> close sd >> say ("Version negotiation failed with " ++ show e)
                      Right _ -> return ()
             Just rescb ->
                 case res_e of
                      Left e  -> do
                          rescb $ Just e
                          close sd
                      Right _ -> rescb Nothing

killResponder :: (Socket, Async ()) -> IO ()
killResponder (sd, hdl) = do
    cancel hdl
    close sd

startInitiator :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
               => [Mx.SomeMuxVersion]
               -> (Mx.SomeMuxVersion -> Maybe (Mx.MiniProtocolDescriptions ptcl IO))
               -> AddrInfo
               -> AddrInfo
               -> IO ()
startInitiator versions mpds local remote = startInitiatorT versions mpds local remote Nothing

startInitiatorT :: (Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl)
                => [Mx.SomeMuxVersion]
                -> (Mx.SomeMuxVersion -> Maybe (Mx.MiniProtocolDescriptions ptcl IO))
                -> AddrInfo
                -> AddrInfo
                -> Maybe (Maybe SomeException -> IO ())
                -> IO ()
startInitiatorT versions mpds local remote rescb_m =
    bracketOnError
        (socket (addrFamily local) Stream defaultProtocol)
        close
        (\sd -> do
            bearer <- socketAsMuxBearer sd
            setSocketOption sd ReuseAddr 1
            setSocketOption sd ReusePort 1
            bind sd (addrAddress local)
            connect sd (addrAddress remote)
            Mx.muxBearerSetState bearer Mx.Connected

            void $ Mx.muxStart versions mpds bearer Mx.StyleClient rescb_m
        )

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))

