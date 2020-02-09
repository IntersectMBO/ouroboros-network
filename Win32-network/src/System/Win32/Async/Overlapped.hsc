{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}

#include <windows.h>

module System.Win32.Async.Overlapped
  ( OVERLAPPED (..)
  , LPOVERLAPPED
  ) where

#if MIN_VERSION_Win32 (2, 7, 0)
import System.Win32.File ( LPOVERLAPPED, OVERLAPPED (..) )
#else
import Foreign ( Ptr
               , nullPtr
               )
import Foreign.Storable (Storable (..))
import System.Win32.Types
#endif


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
