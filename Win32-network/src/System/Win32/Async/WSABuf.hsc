{-# LANGUAGE NamedFieldPuns #-}

#include <winsock2.h>

module System.Win32.Async.WSABuf
  ( WSABuf (..)
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import System.Win32.Types (ULONG)


-- | 'WSABuf' is Haskell representation of 'WSABUF' struct.
--
data WSABuf = WSABuf {
    len :: ULONG,
    buf :: Ptr Word8
  }

instance Storable WSABuf where
    sizeOf _    = (#const sizeof(WSABUF))
    alignment _ = (#alignment WSABUF)

    peek p = do
      len <- (#peek WSABUF, len) p
      buf <- (#peek WSABUF, buf) p
      return $ WSABuf len buf

    poke p WSABuf {len, buf} = do
      (#poke WSABUF, len) p len
      (#poke WSABUF, buf) p buf
