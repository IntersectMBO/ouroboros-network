{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Async.Socket
  ( sendBuf
  , connect
  , accept
  ) where


import           Control.Concurrent
import           Control.Exception
import           Data.Word

import           Foreign.C (CInt (..))
import           Foreign.Ptr (Ptr)
import           Foreign.StablePtr (StablePtr)

import           Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket

import           System.Win32.Async.Internal


type SOCKET = CInt


sendBuf :: Socket
        -> Ptr Word8
        -> Int
        -> IO Int
sendBuf sock buf size = Socket.withFdSocket sock $ \fd ->
    -- on Windows sockets are Word32, GHC represents file descriptors with CInt
    -- which is Int32.
    wsaWaitForCompletion (c_sendBuf fd buf (fromIntegral size))

foreign import ccall safe "HsSendBuf"
    c_sendBuf :: SOCKET
              -> Ptr Word8
              -> Word32
              -> StablePtr b
              -> IO ()

-- | Unfortunatelly `connect` using interruptible ffi is not interruptible. 
-- Instead we run the `Socket.connect` in a dedicated thread and block on an
-- 'MVar'. 
--
connect :: Socket -> SockAddr -> IO ()
connect sock addr = do
    v <- newEmptyMVar
    _ <- mask_ $ forkIOWithUnmask $ \unmask -> 
        unmask (Socket.connect sock addr) >> putMVar v Nothing
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
          unmask (Socket.accept sock >>= putMVar v . Right)
          `catch` (\(e :: IOException) -> putMVar v (Left e))
    r <- takeMVar v
    case r of
      Left e  -> throwIO e
      Right x -> return x
