-- | FFI imports for winsock2 syscalls.
--
module System.Win32.Async.Socket.Syscalls
  ( SOCKET
  , c_WSASend
  , c_WSARecv
  ) where

import           Foreign (Ptr)
import           Foreign.C (CInt (..))

import           System.Win32.Types

import           System.Win32.Async.WSABuf
import           System.Win32.Async.Overlapped


type SOCKET = CInt


foreign import ccall safe "WSASend"
    c_WSASend :: SOCKET
              -> Ptr WSABuf      -- ^ lpBuffers
              -> DWORD           -- ^ dwBufferCount
              -> LPDWORD         -- ^ lpNumberOfBytesSent
              -> DWORD           -- ^ dwFlags
              -> LPWSAOVERLAPPED -- ^ lpOverlapped
              -> Ptr ()          -- ^ lpCompletionRouting
              -> IO CInt

    
foreign import ccall safe "WSARecv"
    c_WSARecv :: SOCKET          -- ^ socket
              -> Ptr WSABuf      -- ^ lpBuffers
              -> DWORD           -- ^ dwBufferCount
              -> LPDWORD         -- ^ lpNumberOfBytesRecvd
              -> LPDWORD         -- ^ lpFlags
              -> LPWSAOVERLAPPED -- ^ lpOverlapped
              -> Ptr ()          -- ^ lpCompletionRouting
              -> IO CInt
