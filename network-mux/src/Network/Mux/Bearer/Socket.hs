{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Socket
  ( socketAsBearerBuffered
  , socketAsBearer) where

import Control.Exception hiding (catch, throwIO)
import Control.Monad (when)
import Control.Tracer
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as BS
import Data.Int
import Data.IORef
import Data.Word

-- import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)

import Network.Socket qualified as Socket
#if !defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy qualified as Socket (recv, sendAll)
import Network.Socket.ByteString qualified as Socket (sendMany)
import Data.ByteString.Internal (ByteString (..))
import Foreign.Marshal.Utils
#else
import System.Win32.Async.Socket.ByteString.Lazy qualified as Win32.Async
#endif

import Network.Mux.Codec qualified as Mx
import Network.Mux.Time qualified as Mx
import Network.Mux.Timeout qualified as Mx
import Network.Mux.Trace qualified as Mx
import Network.Mux.Types (Bearer, BearerIngressBuffer (..))
import Network.Mux.Types qualified as Mx
import Foreign.Ptr
import GHC.ForeignPtr
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
socketAsBearerBuffered
  :: Mx.SDUSize
  -> Int
  -> Mx.BearerIngressBuffer
  -> DiffTime
  -> Tracer IO Mx.Trace
  -> Socket.Socket
  -> Bearer IO Mx.Buffered
socketAsBearerBuffered sduSize batchSize
                       sb@BearerIngressBuffer { bibSize, bibInfoRef }
                       sduTimeout tracer sd =
      Mx.Bearer {
        Mx.read      = readSocket,
        Mx.write     = writeSocket,
        Mx.writeMany = writeSocketMany,
        Mx.sduSize   = sduSize,
        Mx.batchSize = batchSize,
        Mx.name      = "socket-bearer-buffrd"
      }
    where
      hdrLength :: Word16
      hdrLength = 8

      readSocket :: Mx.TimeoutFn IO -> IO (Mx.SDU, Time)
      readSocket timeout = do
          traceWith tracer Mx.TraceRecvHeaderStart

          buf <- Mx.peekBearerBuffer sb
          when (BS.null buf) do
            -- Wait for the first part of the header without any timeout
            recv True False
          -- Optionally wait at most sduTimeout seconds for the complete SDU.
          r_m <- timeout sduTimeout recvRem
          case r_m of
            Nothing -> do
                traceWith tracer Mx.TraceSDUReadTimeoutException
                throwIO Mx.SDUReadTimeout
            Just r -> return r

      recvRem :: IO (Mx.SDU, Time)
      recvRem = do
        while ((< fromIntegral hdrLength) . BS.length <$> Mx.peekBearerBuffer sb)
          (recv False True)
        bytes <- BL.fromStrict <$> Mx.peekBearerBuffer sb
        case Mx.decodeSDU bytes of
          Left  e -> throwIO e
          Right header@Mx.SDU { Mx.msHeader } -> do
              traceWith tracer $ Mx.TraceRecvHeaderEnd msHeader
              bytesBuffer <- Mx.newBearerIngressBuffer . Just . fromIntegral $ Mx.mhLength msHeader
              !blob <- recvLen bytesBuffer
              !ts <- getMonotonicTime
              let !header' = header {Mx.msBlob = blob}
              traceWith tracer (Mx.TraceRecvDeltaQObservation msHeader ts)
              return (header', ts)
        where
          while p m = do
            x <- p
            when x $ m >> while p m

      recvLen :: BearerIngressBuffer -> IO BL.ByteString
      recvLen BearerIngressBuffer { bibSize = destSize, bibInfoRef = destInfoRef } =
        modifyIORef' socketInfoRef skipHeader >> recvLen'
        where
          socketInfoRef = bibInfoRef

          skipHeader (!start, !len, basePtr) =
            let start' = start + fromIntegral hdrLength
                len'   = len - fromIntegral hdrLength
             in if len' == 0
                   then (0, 0, basePtr)
                   else (start', len', basePtr)

          recvLen' = do
            (_destStart, destLen, destPtr) <- readIORef destInfoRef
            (srcStart, srcLen, srcPtr)     <- readIORef socketInfoRef
            if srcLen > 0 -- ^ grab data from buffer if available
              then do
                let howMany = (destSize - destLen) `min` srcLen
                    srcInfo' = if srcLen == howMany
                                 then (0, 0, srcPtr)
                                 else (srcStart + howMany, srcLen - howMany, srcPtr)
                    destLen' = destLen + howMany
                    destInfo' = (0, destLen', destPtr)
                unsafeWithForeignPtr destPtr $ \destPtr' ->
                  unsafeWithForeignPtr srcPtr $ \srcPtr' -> do
                    let destPtr'' = plusPtr destPtr' destLen
                        srcPtr''  = plusPtr srcPtr' srcStart
                    copyBytes destPtr'' srcPtr'' howMany
                writeIORef socketInfoRef srcInfo'
                traceWith tracer $ Mx.TraceRecvEnd howMany
                if destLen' == destSize
                  then return . BL.fromStrict $ BS destPtr destSize
                  else writeIORef destInfoRef destInfo' >> recvLen'
              else do
                recv False False >> recvLen'

      recv :: Bool -> Bool -> IO ()
      recv waitingOnNxtHeader reservePermission = do
        (start, len, socketBufPtr) <- readIORef bibInfoRef
        let maxRead = if (   start + len + fromIntegral hdrLength - 1
                          >= bibSize - fromIntegral hdrLength) && reservePermission
                        then bibSize - (start + len)
                        else (bibSize - fromIntegral hdrLength) - (start + len)

        bytesRead <-
          assert (maxRead > 0) $
          withForeignPtr socketBufPtr $
            \ptr -> Socket.recvBuf sd (plusPtr ptr (start + len)) maxRead

        when (bytesRead == 0) do
          {- This may not be an error, but could be an orderly shutdown.
           - We wait 1 seconds to give the mux protocols time to perform
           - a clean up and exit.
           -}
          threadDelay 1
          throwIO $ Mx.BearerClosed (show sd ++
                  " closed when reading data, waiting on next header " ++
                  show waitingOnNxtHeader)
        traceWith tracer $ Mx.TraceRecvRaw bytesRead
        writeIORef bibInfoRef (start, len + bytesRead, socketBufPtr)
        -- where
#if !defined(mingw32_HOST_OS)
          -- Read at most `min rbSize maxLen` bytes from the socket
          -- into rbBuf.
          -- Creates and returns a Bytestring matching the exact size
          -- of the number of bytes read.
          -- recvBuf :: Mx.ReadBuffer IO -> Int64 -> IO (Maybe Builder)
          -- recvBuf Mx.ReadBuffer{..} maxLen = do
          --   len <- Socket.recvBuf sd rbBuf (min rbSize $ fromIntegral maxLen)
          --   traceWith tracer $ Mx.TraceRecvRaw len
          --   if len > 0
          --      then do
          --        Just . lazyByteString . BL.fromStrict <$> create len (\dest -> copyBytes dest rbBuf len)
          --        -- bs <- lazyByteString $ create len (\dest -> copyBytes dest rbBuf len)
          --        -- return $ BL.fromStrict bs
          --      else return Nothing --undefined --return $ BL.empty
#endif

          -- recvFromSocket :: Int64 -> IO Builder
          -- recvFromSocket len = do
-- #if defined(mingw32_HOST_OS)
--               buf <- Win32.Async.recv sd (fromIntegral len)
-- #else
--               mBuf <- (case readBuffer_m of
--                           Nothing         -> undefined --Socket.recv sd len
--                           Just readBuffer -> recvBuf readBuffer len
--                          )
-- #endif
--                       `catch` Mx.handleIOException "recv errored"
--               case mBuf of
--                 Nothing -> do
--                   when waitingOnNxtHeader $
--                       {- This may not be an error, but could be an orderly shutdown.
--                        - We wait 1 seconds to give the mux protocols time to perform
--                        - a clean up and exit.
--                        -}
--                       threadDelay 1
--                   throwIO $ Mx.BearerClosed (show sd ++
--                       " closed when reading data, waiting on next header " ++
--                       show waitingOnNxtHeader)
--                 Just buf -> return buf

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
  -> Bearer IO Mx.Unbuffered
socketAsBearer sduSize batchSize sduTimeout tracer sd =
      Mx.Bearer {
        Mx.read      = readSocket,
        Mx.write     = writeSocket,
        Mx.writeMany = writeSocketMany,
        Mx.sduSize   = sduSize,
        Mx.batchSize = batchSize,
        Mx.name      = "socket-bearer-buffrd"
      }
    where
      hdrLenght = 8

      readSocket :: Mx.TimeoutFn IO -> IO (Mx.SDU, Time)
      readSocket timeout = do
          traceWith tracer Mx.TraceRecvHeaderStart

          -- Wait for the first part of the header without any timeout
          h0 <- recvAtMost True hdrLenght

          -- Optionally wait at most sduTimeout seconds for the complete SDU.
          r_m <- timeout sduTimeout $ recvRem h0
          case r_m of
                Nothing -> do
                    traceWith tracer Mx.TraceSDUReadTimeoutException
                    throwIO $ Mx.SDUReadTimeout
                Just r -> return r

      recvRem :: BL.ByteString -> IO (Mx.SDU, Time)
      recvRem !h0 = do
          hbuf <- recvLen' (hdrLenght - BL.length h0) [h0]
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
      writeSocketMany timeout sdus =
        getMonotonicTime <* mapM_ (writeSocket timeout) sdus
