{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Async.Socket
  ( sendBuf
  , recvBuf
  , connect
  , accept
  ) where


import           Control.Concurrent
import           Control.Exception
import           Data.Word

import           Foreign.Ptr (Ptr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Storable (Storable (poke))

import           Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket

import           System.Win32.Types
import           System.Win32.Async.WSABuf
import           System.Win32.Async.IOData
import           System.Win32.Async.ErrCode
import           System.Win32.Async.Socket.Syscalls


sendBuf :: Socket
        -> Ptr Word8
        -> Int
        -> IO Int
sendBuf sock buf size = Socket.withFdSocket sock $ \fd ->
    -- on Windows sockets are Word32, GHC represents file descriptors with CInt
    -- which is Int32.
    alloca $ \bufsPtr ->
      withIOCPData "sendBuf" (FDSocket fd) $ \lpOverlapped waitVar -> do
        poke bufsPtr WSABuf {buf, len = fromIntegral size}
        sendResult <- c_WSASend fd bufsPtr 1 nullPtr 0 lpOverlapped nullPtr
        errorCode <- wsaGetLastError
        if sendResult == 0 || errorCode == wSA_IO_PENDING
          then do
            iocpResult <- takeMVar waitVar
            case iocpResult of
              Right numBytes -> return $ ResultAsync numBytes
              Left  e        -> return $ ErrorAsync  (ErrorCode e)
          else return $ ErrorSync (WsaErrorCode errorCode) False

-- | Unfortunatelly `connect` using interruptible ffi is not interruptible. 
-- Instead we run the `Socket.connect` in a dedicated thread and block on an
-- 'MVar'. 
--
connect :: Socket -> SockAddr -> IO ()
connect sock addr = do
    v <- newEmptyMVar
    _ <- mask_ $ forkIOWithUnmask $ \unmask -> 
        (unmask (Socket.connect sock addr) >> putMVar v Nothing)
        `catch` (\(e :: IOException) -> putMVar v (Just e))
    r <- takeMVar v
    case r of
      Just e  -> throwIO e
      Nothing -> return ()


-- | This is a thin wrapper around 'Network.Socket.accept'.  It's possible to
-- 'killThread' which runs the 'accept'.  It will leave a stranded thread, but
-- closing a socket terminates 'Network.Socket.accept' call, and thus there's
-- not resource leak.
--
-- TODO: other possible approaches:
--  * use 'WSAEventSelect' but it needs further investiation (unfortunatelly
--    `WSAAccept` is not part of  IOCP); or
--  * use `AcceptEx`.
--
accept :: Socket -> IO (Socket, SockAddr)
accept sock = do
    v <- newEmptyMVar
    _ <- mask_ $ forkIOWithUnmask $ \unmask -> 
          (unmask (Socket.accept sock) >>= putMVar v . Right)
          `catch` (\(e :: IOException) -> putMVar v (Left e))
    r <- takeMVar v
    case r of
      Left e  -> throwIO e
      Right x -> return x


recvBuf :: Socket -> Ptr Word8 -> Int -> IO Int
recvBuf sock buf size =
    Socket.withFdSocket sock $ \fd ->
      withIOCPData "recvBuf" (FDSocket fd) $ \lpOverlapped waitVar ->
        alloca $ \wsaBuf ->
          alloca $ \lpFlags -> do
            poke wsaBuf (WSABuf (fromIntegral size) buf)
            poke lpFlags 0
            recvResult <- c_WSARecv fd wsaBuf 1 nullPtr lpFlags lpOverlapped nullPtr
            errorCode <- wsaGetLastError
            if recvResult == 0 || errorCode == wSA_IO_PENDING
              then do
                iocpResult <- takeMVar waitVar
                case iocpResult of
                  Right numBytes -> return $ ResultAsync numBytes
                  Left e         -> return $ ErrorAsync  (ErrorCode e)
              else return $ ErrorSync (WsaErrorCode errorCode) False
