{-# LANGUAGE InterruptibleFFI #-}

module System.Win32.Async.Socket
  ( sendBuf
  ) where

import           Data.Word

import           Foreign.C (CInt (..))
import           Foreign.Ptr (Ptr)
import           Foreign.StablePtr (StablePtr)

import           Network.Socket (Socket)
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
