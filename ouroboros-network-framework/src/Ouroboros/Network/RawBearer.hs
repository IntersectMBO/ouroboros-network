{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ouroboros.Network.RawBearer where

import           Data.Word (Word8)
import           Foreign.Ptr (Ptr)
import           Network.Socket (Socket)
import qualified Network.Socket as Socket

#if defined(mingw32_HOST_OS)
import           Foreign.Ptr (castPtr)
import qualified System.Win32 as Win32
#endif

-- | Generalized API for sending and receiving raw bytes over a file
-- descriptor, socket, or similar object.
data RawBearer m =
  RawBearer
    { send :: Ptr Word8 -> Int -> m Int
    , recv :: Ptr Word8 -> Int -> m Int
    }

newtype MakeRawBearer m fd = MakeRawBearer {
  getRawBearer :: fd -> m (RawBearer m)
}

makeSocketRawBearer :: MakeRawBearer IO Socket
makeSocketRawBearer = MakeRawBearer (return . socketToRawBearer)

socketToRawBearer :: Socket -> RawBearer IO
socketToRawBearer s =
    RawBearer
      { send = Socket.sendBuf s
      , recv = Socket.recvBuf s
      }

#if defined(mingw32_HOST_OS)

win32MakeRawBearer :: MakeRawBearer IO Win32.HANDLE
win32MakeRawBearer = MakeRawBearer (return . win32HandleToRawBearer)

-- | We cannot declare an @instance ToRawBearer Win32.HANDLE@, because
-- 'Win32.Handle' is just a type alias for @Ptr ()@. So instead, we provide
-- this function, which can be used to implement 'ToRawBearer' elsewhere (e.g.
-- over a newtype).
win32HandleToRawBearer :: Win32.HANDLE -> RawBearer IO
win32HandleToRawBearer s =
    RawBearer
      { send = \buf size -> fromIntegral <$> Win32.win32_WriteFile s (castPtr buf) (fromIntegral size) Nothing
      , recv = \buf size -> fromIntegral <$> Win32.win32_ReadFile s (castPtr buf) (fromIntegral size) Nothing
      }
#endif
