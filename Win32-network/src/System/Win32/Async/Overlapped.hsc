{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

#include <winsock2.h>
#include <windows.h>

module System.Win32.Async.Overlapped
  ( -- * OVERLAPPED
    OVERLAPPED (..)
  , nullOVERLAPPED
  , LPOVERLAPPED

    -- * WSAOVERLAPPED (winsock2)
  , WSAOVERLAPPED (..)
  , nullWSAOVERLAPPED
  , LPWSAOVERLAPPED
  ) where

#if MIN_VERSION_Win32 (2, 7, 0)
import System.Win32.File ( LPOVERLAPPED, OVERLAPPED (..) )
#endif
import Foreign ( Ptr
               , nullPtr
               )
import Foreign.Storable (Storable (..))
import System.Win32.Types


#if !MIN_VERSION_Win32 (2, 7, 0)
data OVERLAPPED
  = OVERLAPPED { ovl_internal     :: ULONG_PTR
               , ovl_internalHigh :: ULONG_PTR
               , ovl_offset       :: DWORD
               , ovl_offsetHigh   :: DWORD
               , ovl_hEvent       :: HANDLE
               } deriving (Show)

instance Storable OVERLAPPED where
  sizeOf    _ = (#const sizeof(OVERLAPPED))
  alignment _ = (#alignment OVERLAPPED)

  poke buf ov@OVERLAPPED {ovl_hEvent} = do
    (#poke OVERLAPPED, Internal)     buf (ovl_internal ov)
    (#poke OVERLAPPED, InternalHigh) buf (ovl_internal ov)
    (#poke OVERLAPPED, Offset)       buf (ovl_offset ov)
    (#poke OVERLAPPED, OffsetHigh)   buf (ovl_offsetHigh ov)
    (#poke OVERLAPPED, hEvent)       buf ovl_hEvent

  peek buf = do
    ovl_internal     <- (#peek OVERLAPPED, Internal)     buf
    ovl_internalHigh <- (#peek OVERLAPPED, InternalHigh) buf
    ovl_offset       <- (#peek OVERLAPPED, Offset)       buf
    ovl_offsetHigh   <- (#peek OVERLAPPED, OffsetHigh)   buf
    ovl_hEvent       <- (#peek OVERLAPPED, hEvent)       buf
    return $ OVERLAPPED { ovl_internal
                        , ovl_internalHigh
                        , ovl_offset
                        , ovl_offsetHigh
                        , ovl_hEvent
                        }


type LPOVERLAPPED = Ptr OVERLAPPED
#endif

nullOVERLAPPED :: OVERLAPPED
nullOVERLAPPED = OVERLAPPED 0 0 0 0 nullPtr

data WSAOVERLAPPED
  = WSAOVERLAPPED { wsa_internal     :: DWORD
                  , wsa_internalHigh :: DWORD
                  , wsa_offset       :: DWORD
                  , wsa_offsetHigh   :: DWORD
                  , wsa_hEvent       :: HANDLE
                  }

instance Storable WSAOVERLAPPED where
  sizeOf    _ = (#const sizeof(WSAOVERLAPPED))
  alignment _ = (#alignment WSAOVERLAPPED)

  poke buf ov@WSAOVERLAPPED {wsa_hEvent} = do
    (#poke WSAOVERLAPPED, Internal)     buf (wsa_internal ov)
    (#poke WSAOVERLAPPED, InternalHigh) buf (wsa_internal ov)
    (#poke WSAOVERLAPPED, Offset)       buf (wsa_offset ov)
    (#poke WSAOVERLAPPED, OffsetHigh)   buf (wsa_offsetHigh ov)
    (#poke WSAOVERLAPPED, hEvent)       buf wsa_hEvent

  peek buf = do
    wsa_internal     <- (#peek WSAOVERLAPPED, Internal)     buf
    wsa_internalHigh <- (#peek WSAOVERLAPPED, InternalHigh) buf
    wsa_offset       <- (#peek WSAOVERLAPPED, Offset)       buf
    wsa_offsetHigh   <- (#peek WSAOVERLAPPED, OffsetHigh)   buf
    wsa_hEvent       <- (#peek WSAOVERLAPPED, hEvent)       buf
    return $ WSAOVERLAPPED { wsa_internal
                           , wsa_internalHigh
                           , wsa_offset
                           , wsa_offsetHigh
                           , wsa_hEvent
                           }

type LPWSAOVERLAPPED = Ptr WSAOVERLAPPED

nullWSAOVERLAPPED :: WSAOVERLAPPED
nullWSAOVERLAPPED = WSAOVERLAPPED 0 0 0 0 nullPtr
