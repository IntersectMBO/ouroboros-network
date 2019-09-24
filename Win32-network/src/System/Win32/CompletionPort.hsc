{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InterruptibleFFI    #-}

module System.Win32.CompletionPort
  ( createIoCompletionPort
  ) where

import Foreign.Ptr (Ptr, nullPtr)

import System.Win32.Types

#include <fcntl.h>
#include <windows.h>

-- | Windows documentation:
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport>
--
createIoCompletionPort
    :: HANDLE -- ^ file handle to associate with the completion port, can be
              -- 'iNVALID_HANDLE_VALUE'
    -> HANDLE -- ^ existing completion port, can be 'nullPtr'
    -> DWORD  -- ^ number of concurrent threads
    -> IO HANDLE
createIoCompletionPort fileHandle completionPort concurrentThreads
    = c_CreateIoCompletionPort fileHandle completionPort nullPtr concurrentThreads

foreign import ccall unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> HANDLE -> Ptr () -> DWORD -> IO HANDLE
