{-# LANGUAGE BangPatterns #-}

module System.Win32.Async.Socket.ByteString.Lazy
  ( send
  , sendAll
  , recv
  ) where


import           Control.Monad (when)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL (ByteString (..))
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Int (Int64)
import           Foreign.C (CInt (..))
import           Foreign.Marshal.Array (allocaArray)
import           Foreign.Ptr
import           Foreign.StablePtr (StablePtr)
import           Foreign.Storable

import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import           System.Win32.Types (DWORD)

import           System.Win32.Async.WSABuf
import           System.Win32.Async.Internal
import qualified System.Win32.Async.Socket.ByteString as Socket.ByteString


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
send sock bs = do
    let cs  = take maxNumChunks (BL.toChunks bs)
        size = length cs
    siz <- Socket.withFdSocket sock $ \fd -> allocaArray size $ \ptr ->
             withPokes cs ptr $ \nwsabuf ->
               wsaWaitForCompletion "send" (c_sendBuf fd ptr nwsabuf)
    return $ fromIntegral siz
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


foreign import ccall safe "HsSendBuf"
    c_sendBuf :: SOCKET
              -> Ptr WSABuf  -- ^ lpBuffers
              -> DWORD       -- ^ dwBufferCount
              -> StablePtr b
              -> IO ()

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
