{-# LANGUAGE Safe #-}

-- The code of this module is copied from https://github.com/haskell/win32.
-- setFilePointerEx was introduced in Win32 in version 2.6.2.0, but katip
-- does not allow us to use it.

module Ouroboros.Consensus.Storage.Seek
  ( setFilePointerEx
  ) where

#include <windows.h>

import           System.Win32.Types
import           System.Win32.File (FilePtrDirection)
import           Foreign (Ptr, alloca, peek)

setFilePointerEx :: HANDLE -> LARGE_INTEGER -> FilePtrDirection -> IO LARGE_INTEGER
setFilePointerEx h dist dir =
  alloca $ \p_pos -> do
  failIfFalse_ "SetFilePointerEx" $ c_SetFilePointerEx h dist p_pos dir
  peek p_pos
#if defined(i386_HOST_ARCH)
foreign import stdcall unsafe "windows.h SetFilePointerEx"
#elif defined(x86_64_HOST_ARCH)
foreign import ccall unsafe "windows.h SetFilePointerEx"
#else
# error Unknown mingw32 arch
#endif
  c_SetFilePointerEx :: HANDLE -> LARGE_INTEGER -> Ptr LARGE_INTEGER -> FilePtrDirection -> IO Bool
