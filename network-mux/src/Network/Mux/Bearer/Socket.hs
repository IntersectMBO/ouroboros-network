{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket
  ( socketAsMuxBearer
  , hexDump
  ) where

import           Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           Text.Printf

import           GHC.Stack

import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx

hexDump :: BL.ByteString -> String -> IO ()
hexDump buf out | BL.empty == buf = say out
hexDump buf out = hexDump (BL.tail buf) (out ++ printf "0x%02x " (BL.head buf))


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
          hbuf <- recvLen' True 8 []
          --say "read"
          --hexDump hbuf ""
          case Mx.decodeMuxSDU hbuf of
              Left  e      -> throwM e
              Right header -> do
                  -- say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  blob <- recvLen' False (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Bool -> Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return (BL.concat $ reverse bufs)
      recvLen' waitingOnNxtHeader l bufs = do
          buf <- Socket.recv sd l
          if BL.null buf
              then do
                  when (waitingOnNxtHeader) $
                      {- This may not be an error, but could be an orderly shutdown.
                       - We wait 5 seconds to give the mux protocols time to perform
                       - a clean up and exit.
                       -}
                      threadDelay 5
                  throwM $ Mx.MuxError Mx.MuxBearerClosed (show sd ++
                      " closed when reading data, waiting on next header " ++
                      show waitingOnNxtHeader) callStack
              else recvLen' False (l - fromIntegral (BL.length buf)) (buf : bufs)

      writeSocket :: Mx.MuxSDU ptcl -> IO (Time IO)
      writeSocket sdu = do
          --say "write"
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
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
