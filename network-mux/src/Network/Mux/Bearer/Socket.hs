{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket (socketAsBearer) where

import Control.Monad (when)
import Control.Tracer
import Data.ByteString.Lazy qualified as BL
import Data.Int

import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)

import Network.Socket qualified as Socket
#if !defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy qualified as Socket (recv, sendAll)
import Network.Socket.ByteString qualified as Socket (sendMany)
#else
import System.Win32.Async.Socket.ByteString.Lazy qualified as Win32.Async
#endif

import Network.Mux.Codec qualified as Mx
import Network.Mux.Time qualified as Mx
import Network.Mux.Timeout qualified as Mx
import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (Bearer)
import Network.Mux.Types qualified as Mx
#if defined(linux_HOST_OS) && defined(MUX_TRACE_TCPINFO)
import Network.Mux.TCPInfo (SocketOption (TCPInfoSocketOption))
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
socketAsBearer
  :: Mx.SDUSize
  -> Int
  -> DiffTime
  -> Tracer IO Mx.Trace
  -> Socket.Socket
  -> Bearer IO
socketAsBearer sduSize batchSize sduTimeout tracer sd =
      Mx.Bearer {
        Mx.read      = readSocket,
        Mx.write     = writeSocket,
        Mx.writeMany = writeSocketMany,
        Mx.sduSize   = sduSize,
        Mx.batchSize = batchSize,
        Mx.name      = "socket-bearer"
      }
    where
      readSocket :: Mx.TimeoutFn IO -> IO (Mx.SDU, Time)
      readSocket timeout = do
          traceWith tracer Mx.TraceRecvHeaderStart

          -- Wait for the first part of the header without any timeout
          h0 <- recvAtMost True Mx.msHeaderLength

          -- Optionally wait at most sduTimeout seconds for the complete SDU.
          r_m <- timeout sduTimeout $ recvRem h0
          case r_m of
                Nothing -> do
                    traceWith tracer Mx.TraceSDUReadTimeoutException
                    throwIO $ Mx.SDUReadTimeout
                Just r -> return r

      recvRem :: BL.ByteString -> IO (Mx.SDU, Time)
      recvRem !h0 = do
          hbuf <- recvLen' (Mx.msHeaderLength - BL.length h0) [h0]
          case Mx.decodeSDU hbuf of
               Left  e ->  throwIO e
               Right header@Mx.SDU { Mx.msHeader } -> do
                   traceWith tracer $ Mx.TraceRecvHeaderEnd msHeader
                   !blob <- recvLen' (fromIntegral $ Mx.mhLength msHeader) []

                   !ts <- getMonotonicTime
                   let !header' = header {Mx.msBlob = blob}
                   traceWith tracer (Mx.TraceRecvDeltaQObservation msHeader ts)
                   return (header', ts)

      recvLen' ::  Int64 -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' l bufs = do
          buf <- recvAtMost False l
          recvLen' (l - BL.length buf) (buf : bufs)

      recvAtMost :: Bool -> Int64 -> IO BL.ByteString
      recvAtMost waitingOnNxtHeader l = do
          traceWith tracer $ Mx.TraceRecvStart $ fromIntegral l
#if defined(mingw32_HOST_OS)
          buf <- Win32.Async.recv sd (fromIntegral l)
#else
          buf <- Socket.recv sd l
#endif
                    `catch` Mx.handleIOException "recv errored"
          if BL.null buf
              then do
                  when waitingOnNxtHeader $
                      {- This may not be an error, but could be an orderly shutdown.
                       - We wait 1 seconds to give the mux protocols time to perform
                       - a clean up and exit.
                       -}
                      threadDelay 1
                  throwIO $ Mx.BearerClosed (show sd ++
                      " closed when reading data, waiting on next header " ++
                      show waitingOnNxtHeader)
              else do
                  traceWith tracer $ Mx.TraceRecvEnd (fromIntegral $ BL.length buf)
                  return buf

      writeSocket :: Mx.TimeoutFn IO -> Mx.SDU -> IO Time
      writeSocket timeout sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = Mx.setTimestamp sdu (Mx.RemoteClockModel ts32)
              buf  = Mx.encodeSDU sdu'
          traceWith tracer $ Mx.TraceSendStart (Mx.msHeader sdu')
          r <- timeout sduTimeout $
#if defined(mingw32_HOST_OS)
              Win32.Async.sendAll sd buf
#else
              Socket.sendAll sd buf
#endif
              `catch` Mx.handleIOException "sendAll errored"
          case r of
               Nothing -> do
                    traceWith tracer Mx.TraceSDUWriteTimeoutException
                    throwIO Mx.SDUWriteTimeout
               Just _ -> do
                   traceWith tracer Mx.TraceSendEnd
#if defined(linux_HOST_OS) && defined(MUX_TRACE_TCPINFO)
                   -- If it was possible to detect if the TraceTCPInfo was
                   -- enable we wouldn't have to hide the getSockOpt
                   -- syscall in this ifdef. Instead we would only call it if
                   -- we knew that the information would be traced.
                   tcpi <- Socket.getSockOpt sd TCPInfoSocketOption
                   traceWith tracer $ Mx.TraceTCPInfo tcpi (Mx.mhLength $ Mx.msHeader sdu)
#endif
                   return ts

      writeSocketMany :: Mx.TimeoutFn IO -> [Mx.SDU] -> IO Time
#if defined(mingw32_HOST_OS)
      writeSocketMany timeout sdus = do
        ts <- getMonotonicTime
        mapM_ (writeSocket timeout) sdus
        return ts
#else
      writeSocketMany timeout sdus = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              buf  = map (Mx.encodeSDU .
                           (\sdu -> Mx.setTimestamp sdu (Mx.RemoteClockModel ts32))) sdus
          r <- timeout ((fromIntegral $ length sdus) * sduTimeout) $
              Socket.sendMany sd (concatMap BL.toChunks buf)
              `catch` Mx.handleIOException "sendAll errored"
          case r of
               Nothing -> do
                    traceWith tracer Mx.TraceSDUWriteTimeoutException
                    throwIO Mx.SDUWriteTimeout
               Just _ -> do
                   traceWith tracer Mx.TraceSendEnd
#if defined(linux_HOST_OS) && defined(MUX_TRACE_TCPINFO)
                   -- If it was possible to detect if the TraceTCPInfo was
                   -- enable we wouldn't have to hide the getSockOpt
                   -- syscall in this ifdef. Instead we would only call it if
                   -- we knew that the information would be traced.
                   tcpi <- Socket.getSockOpt sd TCPInfoSocketOption
                   traceWith tracer $ Mx.TraceTCPInfo tcpi (sum $ map (Mx.mhLength . Mx.msHeader) sdus)
#endif
                   return ts
#endif
