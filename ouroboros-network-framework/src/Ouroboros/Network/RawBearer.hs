{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ouroboros.Network.RawBearer
where

import           Network.Socket (Socket)
import qualified Network.Socket as Socket
import           Foreign.Ptr (Ptr)
import           Data.Word (Word8)

#if defined(mingw32_HOST_OS)
import           Data.Bits
import           Foreign.Ptr (IntPtr (..), ptrToIntPtr)
import qualified System.Win32 as Win32
import qualified System.Win32.Async as Win32.Async
import qualified System.Win32.NamedPipes as Win32
#endif

-- | Generalized API for sending and receiving raw bytes over a file
-- descriptor, socket, or similar object.
data RawBearer m =
  RawBearer
    { send :: Ptr Word8 -> Int -> m Int
    , recv :: Ptr Word8 -> Int -> m Int
    }

class ToRawBearer m fd where
  toRawBearer :: fd -> m (RawBearer m)

instance ToRawBearer IO Socket where
  toRawBearer s =
    return RawBearer
      { send = Socket.sendBuf s
      , recv = Socket.recvBuf s
      }

#if defined(mingw32_HOST_OS)

-- | We cannot declare an @instance ToRawBearer Win32.HANDLE@, because
-- 'Win32.Handle' is just a type alias for @Ptr ()@. So instead, we provide
-- this function, which can be used to implement 'ToRawBearer' elsewhere (e.g.
-- over a newtype).
win32HandleToRawBearer :: Win32.HANDLE -> RawBearer IO
win32HandleToRawBearer s =
    RawBearer
      { send = \buf size -> fromIntegral <$> Win32.win32_WriteFile s (castPtr buf) (fromIntegral size)
      , recv = \buf size -> fromIntegral <$> Win32.win32_ReadFile s (castPtr buf) (fromIntegral size)
      }
#endif
