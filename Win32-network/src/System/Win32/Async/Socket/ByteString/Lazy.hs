{-# LANGUAGE BangPatterns #-}

module System.Win32.Async.Socket.ByteString.Lazy
  ( send
  , sendAll
  , recv
  ) where


import           Control.Concurrent (takeMVar)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL (ByteString (..))
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Int (Int64)
import           Foreign ( Storable (..)
                         , plusPtr
                         , castPtr
                         , nullPtr
                         )
import           Foreign.Marshal.Array (allocaArray)

import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import           System.Win32.Async.ErrCode
import           System.Win32.Async.IOData
import           System.Win32.Async.WSABuf
import qualified System.Win32.Async.Socket.ByteString as Socket.ByteString
import           System.Win32.Async.Socket.Syscalls


-- | Sending each chunk using vectored I/O.  In one system call one can
-- transmit at most 1024 chunks, it is safe to transmit less than 4194304
-- bytes.
--
-- The socket must be in a connected state, and must be associated with an IO
-- completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.
--
send :: Socket
     -> ByteString
     -> IO Int64
send sock bs =
    fmap fromIntegral $
      Socket.withFdSocket sock $ \fd ->
        withIOCPData "send" (FDSocket fd) $ \lpOverlapped waitVar ->
          let cs  = take maxNumChunks (BL.toChunks bs)
              size = length cs
          in allocaArray size $ \ptr ->
               withPokes cs ptr $ \nwsabuf -> do
                 sendResult <- c_WSASend fd ptr nwsabuf nullPtr 0 lpOverlapped nullPtr
                 errorCode <- wsaGetLastError
                 if sendResult == 0 || errorCode == wSA_IO_PENDING
                   then do
                     iocpResult <- takeMVar waitVar
                     case iocpResult of
                       Right numBytes -> return $ ResultAsync numBytes
                       Left  e        -> return $ ErrorAsync  (ErrorCode e)
                   else return $ ErrorSync (WsaErrorCode errorCode) False
  where
    withPokes ss p f = loop ss p 0 0
      where
        loop (c:cs) q k !nwsabuf
            | k < maxNumBytes = unsafeUseAsCStringLen c $ \(ptr, strlen) -> do
                poke q $ WSABuf (fromIntegral strlen) (castPtr ptr)
                loop cs
                     (q `plusPtr` sizeOf (undefined :: WSABuf))
                     (k + fromIntegral strlen)
                     (nwsabuf + 1)
            | otherwise = f nwsabuf
        loop _ _ _ nwsabuf = f nwsabuf

    maxNumBytes, maxNumChunks :: Int
    maxNumBytes  = 4194304 -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024    -- maximum number of chunks to transmit in one system call


sendAll :: Socket
        -> ByteString
        -> IO ()
sendAll _sock bs | BL.null bs = return ()
sendAll sock  bs = do
    sent <- send sock bs
    -- it is simpler than `Network.Socket.Lazy.sendAll` - waiting for sending
    -- all the chunks is already perfomed by 'send'.
    let bs' = BL.drop sent bs
    when (sent >= 0 && not (BL.null bs')) $ sendAll sock bs'

-- | Receive bytes from a socket, which must be in a connected state, and must
-- be associated with an IO completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.
-- It can return less bytes than requested.
--
recv :: Socket
     -> Int
     -> IO ByteString
recv sock size = toChunk <$> Socket.ByteString.recv sock size
  where
    toChunk bs | BS.null bs = BL.Empty
               | otherwise  = BL.Chunk bs BL.Empty
