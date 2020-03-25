-- | FFI imports for winsock2 syscalls.
--
module System.Win32.Async.Socket.Syscalls
  ( SOCKET
  , c_WSASend
  , c_WSASendTo
  , c_WSARecv
  , c_WSARecvFrom
  ) where

import           Foreign (Ptr)
import           Foreign.C (CInt (..))

import           System.Win32.Types

import           System.Win32.Async.WSABuf
import           System.Win32.Async.Overlapped


type SOCKET = CInt


foreign import ccall unsafe "WSASend"
    c_WSASend :: SOCKET
              -> Ptr WSABuf      -- ^ lpBuffers
              -> DWORD           -- ^ dwBufferCount
              -> LPDWORD         -- ^ lpNumberOfBytesSent
              -> DWORD           -- ^ dwFlags
              -> LPWSAOVERLAPPED -- ^ lpOverlapped
              -> Ptr ()          -- ^ lpCompletionRouting
              -> IO CInt


foreign import ccall unsafe "WSASendTo"
    c_WSASendTo :: SOCKET
                -> Ptr WSABuf      -- ^ lpBuffers
                -> DWORD           -- ^ dwBufferCount
                -> LPDWORD         -- ^ lpNumberOfBytesSent
                -> DWORD           -- ^ dwFlags
                -> Ptr sa          -- ^ lpTo
                -> Int             -- ^ iToLen (size in bytes of `lpTo`)
                -> LPWSAOVERLAPPED -- ^ lpOverlapped
                -> Ptr ()          -- ^ lpCompletionRouting
                -> IO CInt

    
foreign import ccall unsafe "WSARecv"
    c_WSARecv :: SOCKET          -- ^ socket
              -> Ptr WSABuf      -- ^ lpBuffers
              -> DWORD           -- ^ dwBufferCount
              -> LPDWORD         -- ^ lpNumberOfBytesReceived
              -> LPDWORD         -- ^ lpFlags
              -> LPWSAOVERLAPPED -- ^ lpOverlapped
              -> Ptr ()          -- ^ lpCompletionRouting
              -> IO CInt


foreign import ccall unsafe "WSARecvFrom"
    c_WSARecvFrom :: SOCKET          -- ^ socket
                  -> Ptr WSABuf      -- ^ lpBuffers
                  -> DWORD           -- ^ dwBufferCount
                  -> LPDWORD         -- ^ lpNumberOfBytesReceived
                  -> LPDWORD         -- ^ lpFlags
                  -> Ptr sa          -- ^ lpFrom
                  -> Ptr Int         -- ^ iFromLen (size in bytes of `lpFrom`)
                  -> LPWSAOVERLAPPED -- ^ lpOverlapped
                  -> Ptr ()          -- ^ lpCompletionRouting
                  -> IO CInt
