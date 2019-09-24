{-# LANGUAGE CPP #-}

module System.Win32.Types.Overlapped
  ( OVERLAPPED (..)
  , LPOVERLAPPED
  ) where

#if MIN_VERSION_Win32 (2, 7, 0)
import System.Win32.File ( LPOVERLAPPED, OVERLAPPED )
#else
import Foreign
import Foreign.Storable (Storable (..))
import System.Win32.Types
#endif


#if MIN_VERSION_Win32 (2, 7, 0)
-- 'LPOVERLAPPED' and 'OVERLAPPED' data type is imported from 'System.Win32.File'
#else
data OVERLAPPED
  = OVERLAPPED { ovl_internal     :: ULONG_PTR
               , ovl_internalHigh :: ULONG_PTR
               , ovl_offset       :: DWORD
               , ovl_offsetHigh   :: DWORD
               , ovl_hEvent       :: HANDLE
               } deriving (Show)

instance Storable OVERLAPPED where
  sizeOf = const ((32))
  alignment _ = 8
  poke buf ad = do
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) buf (ovl_internal     ad)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) buf (ovl_internalHigh ad)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) buf (ovl_offset       ad)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) buf (ovl_offsetHigh   ad)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) buf (ovl_hEvent       ad)

  peek buf = do
      intnl      <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) buf
      intnl_high <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) buf
      off        <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) buf
      off_high   <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) buf
      hevnt      <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) buf
      return $ OVERLAPPED intnl intnl_high off off_high hevnt

type LPOVERLAPPED = Ptr OVERLAPPED
#endif
