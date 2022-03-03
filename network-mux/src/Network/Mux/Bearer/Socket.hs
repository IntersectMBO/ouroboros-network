{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket (socketAsMuxBearer) where

import           Control.Monad (when)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)

import qualified Network.Socket as Socket
#if !defined(mingw32_HOST_OS)
import qualified Network.Socket.ByteString.Lazy as Socket (recv, sendAll)
#else
import qualified System.Win32.Async.Socket.ByteString.Lazy as Win32.Async
#endif

import qualified Network.Mux as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Time as Mx
import qualified Network.Mux.Timeout as Mx
import qualified Network.Mux.Trace as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
#if defined(linux_HOST_OS) && defined(MUX_TRACE_TCPINFO)
import           Network.Mux.TCPInfo (SocketOption(TCPInfoSocketOption))
#endif

-- |
-- Create @'MuxBearer'@ from a socket.
--
-- On Windows 'System.Win32.Async` operations are used to read and write from
-- a socket.  This means that the socket must be associated with the I/O
-- completion port with
-- 'System.Win32.Async.IOManager.associateWithIOCompletionPort'.
--
-- Note: 'IOException's thrown by 'sendAll' and 'recv' are wrapped in
-- 'MuxError'.
--
socketAsMuxBearer
  :: DiffTime
  -> Tracer IO Mx.MuxTrace
  -> Socket.Socket
  -> MuxBearer IO
socketAsMuxBearer sduTimeout tracer sd =
      Mx.MuxBearer {
        Mx.read    = readSocket,
        Mx.write   = writeSocket,
        Mx.sduSize = Mx.SDUSize 12288
      }
    where
      hdrLenght = 8

      readSocket :: Mx.TimeoutFn IO -> IO (Mx.MuxSDU, Time)
      readSocket timeout = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart

          -- Wait for the first part of the header without any timeout
          h0 <- recvAtMost True hdrLenght

          -- Optionally wait at most sduTimeout seconds for the complete SDU.
          r_m <- timeout sduTimeout $ recvRem h0
          case r_m of
                Nothing -> do
                    traceWith tracer $ Mx.MuxTraceSDUReadTimeoutException
                    throwIO $ Mx.MuxError Mx.MuxSDUReadTimeout "Mux SDU Timeout"
                Just r -> return r

      recvRem :: BL.ByteString -> IO (Mx.MuxSDU, Time)
      recvRem !h0 = do
          hbuf <- recvLen' (hdrLenght - BL.length h0) [h0]
          case Mx.decodeMuxSDU hbuf of
               Left  e ->  throwIO e
               Right header@Mx.MuxSDU { Mx.msHeader } -> do
                   traceWith tracer $ Mx.MuxTraceRecvHeaderEnd msHeader
                   !blob <- recvLen' (fromIntegral $ Mx.mhLength msHeader) []

                   !ts <- getMonotonicTime
                   let !header' = header {Mx.msBlob = blob}
                   traceWith tracer (Mx.MuxTraceRecvDeltaQObservation msHeader ts)
                   return (header', ts)

      recvLen' ::  Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' l bufs = do
          buf <- recvAtMost False l
          recvLen' (l - BL.length buf) (buf : bufs)

      recvAtMost :: Bool -> Int64 -> IO BL.ByteString
      recvAtMost waitingOnNxtHeader l = do
          traceWith tracer $ Mx.MuxTraceRecvStart $ fromIntegral l
#if defined(mingw32_HOST_OS)
          buf <- Win32.Async.recv sd (fromIntegral l)
#else
          buf <- Socket.recv sd l
#endif
                    `catch` Mx.handleIOException "recv errored"
          if BL.null buf
              then do
                  when (waitingOnNxtHeader) $
                      {- This may not be an error, but could be an orderly shutdown.
                       - We wait 1 seconds to give the mux protocols time to perform
                       - a clean up and exit.
                       -}
                      threadDelay 1
                  throwIO $ Mx.MuxError Mx.MuxBearerClosed (show sd ++
                      " closed when reading data, waiting on next header " ++
                      show waitingOnNxtHeader)
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd (fromIntegral $ BL.length buf)
                  return buf

      writeSocket :: Mx.TimeoutFn IO -> Mx.MuxSDU -> IO Time
      writeSocket timeout sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart (Mx.msHeader sdu')
          r <- timeout sduTimeout $
#if defined(mingw32_HOST_OS)
              Win32.Async.sendAll sd buf
#else
              Socket.sendAll sd buf
#endif
              `catch` Mx.handleIOException "sendAll errored"
          case r of
               Nothing -> do
                    traceWith tracer $ Mx.MuxTraceSDUWriteTimeoutException
                    throwIO $ Mx.MuxError Mx.MuxSDUWriteTimeout "Mux SDU Timeout"
               Just _ -> do
                   traceWith tracer $ Mx.MuxTraceSendEnd
#if defined(linux_HOST_OS) && defined(MUX_TRACE_TCPINFO)
                   -- If it was possible to detect if the MuxTraceTCPInfo was
                   -- enable we woulnd't have to hide the getSockOpt
                   -- syscall in this ifdef. Instead we would only call it if
                   -- we knew that the information would be traced.
                   tcpi <- Socket.getSockOpt sd TCPInfoSocketOption
                   traceWith tracer $ Mx.MuxTraceTCPInfo tcpi (Mx.mhLength $ Mx.msHeader sdu)
#endif
                   return ts

