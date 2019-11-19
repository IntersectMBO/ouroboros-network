{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket
  ( socketAsMuxBearer
  ) where

import           Control.Monad (when)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import           GHC.Stack

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer

import qualified Network.Socket as Socket hiding (recv)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Trace as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx


-- |
-- Create @'MuxBearer'@ from a socket.
--
-- Note: 'IOException's thrown by 'sendAll' and 'recv' are wrapped in
-- 'MuxError'.
--
socketAsMuxBearer
  :: Tracer IO Mx.MuxTrace
  -> Socket.Socket
  -> MuxBearer IO
socketAsMuxBearer tracer sd =
      Mx.MuxBearer {
        Mx.read    = readSocket,
        Mx.write   = writeSocket,
        Mx.sduSize = 12288
      }
    where
      readSocket :: HasCallStack => IO (Mx.MuxSDU, Time)
      readSocket = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          hbuf <- recvLen' True 8 []
          case Mx.decodeMuxSDU hbuf of
              Left  e      -> throwM e
              Right header -> do
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
                  blob <- recvLen' False (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  traceWith tracer (Mx.MuxTraceRecvDeltaQObservation header ts)
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Bool -> Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' _ 0 bufs = return (BL.concat $ reverse bufs)
      recvLen' waitingOnNxtHeader l bufs = do
          traceWith tracer $ Mx.MuxTraceRecvStart $ fromIntegral l
          buf <- Socket.recv sd l
                    `catch` Mx.handleIOException "recv errored"
          if BL.null buf
              then do
                  when (waitingOnNxtHeader) $
                      {- This may not be an error, but could be an orderly shutdown.
                       - We wait 1 seconds to give the mux protocols time to perform
                       - a clean up and exit.
                       -}
                      threadDelay 1
                  throwM $ Mx.MuxError Mx.MuxBearerClosed (show sd ++
                      " closed when reading data, waiting on next header " ++
                      show waitingOnNxtHeader) callStack
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd buf
                  recvLen' False (l - fromIntegral (BL.length buf)) (buf : bufs)

      writeSocket :: Mx.MuxSDU -> IO Time
      writeSocket sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          Socket.sendAll sd buf
            `catch` Mx.handleIOException "sendAll errored"
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

