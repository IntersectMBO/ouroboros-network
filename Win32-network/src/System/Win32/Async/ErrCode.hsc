#include <fcntl.h>
#include <winsock2.h>

#include <windows.h>

-- | 'ErrCode's used by 'System.Win32.Async'
module System.Win32.Async.ErrCode where

import System.Win32.Types (ErrCode, failWith)

-- | This error is thrown by 'GetQueuedCompletionsStatus' when I/O completion
-- port is closed.
--
-- 735
eRROR_ABANDONED_WAIT_0 :: ErrCode
eRROR_ABANDONED_WAIT_0 = #const ERROR_ABANDONED_WAIT_0

-- | Reached the end of file.
--
-- 38
eRROR_HANDLE_EOF :: ErrCode
eRROR_HANDLE_EOF = #const ERROR_HANDLE_EOF

-- | The 'HANDLE' is invalid.
--
-- 6
eRROR_INVALID_HANDLE :: ErrCode
eRROR_INVALID_HANDLE = #const ERROR_INVALID_HANDLE

-- 997
eRROR_IO_PENDING :: ErrCode
eRROR_IO_PENDING = #const ERROR_IO_PENDING

-- 535
eRROR_PIPE_CONNECTED :: ErrCode
eRROR_PIPE_CONNECTED = #const ERROR_PIPE_CONNECTED

-- 536
eRROR_PIPE_LISTENING :: ErrCode
eRROR_PIPE_LISTENING = #const ERROR_PIPE_LISTENING

type WSAErrCode = Int

wSA_IO_PENDING :: WSAErrCode
wSA_IO_PENDING = #const WSA_IO_PENDING

foreign import ccall safe "winsock2.hs WSAGetLastError"
    wsaGetLastError :: IO WSAErrCode

-- Accordin to
-- <https://docs.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-wsagetlasterror>
-- @FormatMessage@ can be used to get 'WSAErrCode' description.
wsaFailWith :: String -> WSAErrCode -> IO a
wsaFailWith fn_name err_code = failWith fn_name (fromIntegral err_code)
