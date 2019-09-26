#include "windows_cconv.h"
#include <fcntl.h>
#include <windows.h>

{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CApiFFI          #-}
{-# LANGUAGE InterruptibleFFI #-}

-- | For full details on the Windows named pipes API see
-- <https://docs.microsoft.com/en-us/windows/desktop/ipc/named-pipes>
--
module System.Win32.NamedPipes (

    -- * Named pipe server APIs
    createNamedPipe,
    connectNamedPipe,
    pIPE_UNLIMITED_INSTANCES,

    -- ** Paramater types
    LPSECURITY_ATTRIBUTES,
    OpenMode,
    pIPE_ACCESS_DUPLEX,
    pIPE_ACCESS_INBOUND,
    pIPE_ACCESS_OUTBOUND,
    PipeMode,
    pIPE_TYPE_BYTE,
    pIPE_TYPE_MESSAGE,
    pIPE_READMODE_BYTE,
    pIPE_READMODE_MESSAGE,

    -- * Named pipe client APIs
    -- | This directly reuses other Win32 file APIs
    createFile,
    closePipe,
    readPipe,
    pGetLine,
    writePipe,

    -- * Client and server APIs
    pipeToHandle,
  ) where


import Control.Monad (unless)
import Data.Functor (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as BSC
import System.IO (Handle, IOMode, BufferMode(..), hSetBuffering)
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.FD     (mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)

import Foreign hiding (void)
import Foreign.C
import System.Win32.Types
import System.Win32.File hiding ( win32_ReadFile, c_ReadFile
                                , win32_WriteFile, c_WriteFile
                                )



-- | The named pipe open mode.
--
-- This must specify one of:
--
-- * 'pIPE_ACCESS_DUPLEX'
-- * 'pIPE_ACCESS_INBOUND'
-- * 'pIPE_ACCESS_OUTBOUND'
--
-- It may also specify:
--
-- * 'fILE_FLAG_WRITE_THROUGH'
-- * 'fILE_FLAG_OVERLAPPED'
--
-- It may also specify any combination of:
--
-- * 'wRITE_DAC'
-- * 'wRITE_OWNER'
-- * 'aCCESS_SYSTEM_SECURITY'
--
type OpenMode = UINT

#{enum OpenMode,
 , pIPE_ACCESS_DUPLEX            = PIPE_ACCESS_DUPLEX
 , pIPE_ACCESS_INBOUND           = PIPE_ACCESS_INBOUND
 , pIPE_ACCESS_OUTBOUND          = PIPE_ACCESS_OUTBOUND
 }

-- | The pipe mode.
--
-- One of the following type modes can be specified. The same type mode must be
-- specified for each instance of the pipe.
--
-- * 'pIPE_TYPE_BYTE'
-- * 'pIPE_TYPE_MESSAGE'
--
-- One of the following read modes can be specified. Different instances of the
-- same pipe can specify different read modes.
--
-- * 'pIPE_READMODE_BYTE'
-- * 'pIPE_READMODE_MESSAGE'
--
type PipeMode = UINT

#{enum PipeMode,
 , pIPE_TYPE_BYTE                = PIPE_TYPE_BYTE
 , pIPE_TYPE_MESSAGE             = PIPE_TYPE_MESSAGE
 , pIPE_READMODE_BYTE            = PIPE_READMODE_BYTE
 , pIPE_READMODE_MESSAGE         = PIPE_READMODE_MESSAGE
 }

-- | If the 'createNamedPipe' @nMaxInstances@ parameter is
-- 'pIPE_UNLIMITED_INSTANCES', the number of pipe instances that can be created
-- is limited only by the availability of system resources.
pIPE_UNLIMITED_INSTANCES :: DWORD
pIPE_UNLIMITED_INSTANCES = #const PIPE_UNLIMITED_INSTANCES

-- | Creates an instance of a named pipe and returns a handle for subsequent
-- pipe operations. A named pipe server process uses this function either to
-- create the first instance of a specific named pipe and establish its basic
-- attributes or to create a new instance of an existing named pipe.
--
-- For full details see
-- <https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-createnamedpipea>
--
createNamedPipe :: String   -- ^ pipe name of form @\.\pipe\{pipename}@
                -> OpenMode
                -> PipeMode
                -> DWORD    -- ^ nMaxInstances
                -> DWORD    -- ^ nOutBufferSize
                -> DWORD    -- ^ nInBufferSize
                -> DWORD    -- ^ nDefaultTimeOut
                -> Maybe LPSECURITY_ATTRIBUTES
                -> IO HANDLE
createNamedPipe name openMode pipeMode
                nMaxInstances nOutBufferSize nInBufferSize
                nDefaultTimeOut mb_attr =
  withTString name $ \ c_name ->
    failIf (==iNVALID_HANDLE_VALUE) "CreateNamedPipe" $
      c_CreateNamedPipe c_name openMode pipeMode
                        nMaxInstances nOutBufferSize nInBufferSize
                        nDefaultTimeOut (maybePtr mb_attr)

foreign import ccall unsafe "windows.h CreateNamedPipeW"
  c_CreateNamedPipe :: LPCTSTR
                    -> DWORD
                    -> DWORD
                    -> DWORD
                    -> DWORD
                    -> DWORD
                    -> DWORD
                    -> LPSECURITY_ATTRIBUTES
                    -> IO HANDLE

-- | Enables a named pipe server process to wait for a client process to
-- connect to an instance of a named pipe. A client process connects by
-- calling the 'createFile' function.
--
-- 'connectNamedPipe' can only be called on a fresh pipe, which is not
-- connected to other client, or after calling @DisconnectNamedPipe@ windows
-- API (which we don't expose yet).  This is quite different to Berkeley
-- socket `accept` call.
--
connectNamedPipe :: HANDLE -> Maybe LPOVERLAPPED -> IO ()
connectNamedPipe hpipe mb_over = do
    ok <- c_ConnectNamedPipe hpipe (maybePtr mb_over)
    if ok
       then return ()
       else do
         -- If a client connects before the function is called, the function
         -- returns zero and GetLastError returns ERROR_PIPE_CONNECTED.
         -- This can happen if a client connects in the interval between the
         -- call to CreateNamedPipe and the call to ConnectNamedPipe. In this
         -- situation, there is a good connection between client and server,
         -- even though the function returns zero.
         err_code <- getLastError
         unless (err_code == eRROR_PIPE_CONNECTED)
                (failWith "ConnectNamedPipe" err_code)

eRROR_PIPE_CONNECTED :: ErrCode
eRROR_PIPE_CONNECTED = #const ERROR_PIPE_CONNECTED

foreign import ccall interruptible "windows.h ConnectNamedPipe"
  c_ConnectNamedPipe :: HANDLE
                     -> LPOVERLAPPED
                     -> IO Bool

pipeToHandle :: HANDLE -> String -> IOMode -> IO Handle
pipeToHandle hpipe name iomode = do
  osfd   <- handleToFd hpipe
  (fd,_) <- mkFD osfd
                 iomode
                 (Just (Stream, 0, 0))
                 False  -- pipes are normal HANDLEs, not sockets
                 False  -- non-blocking is ignored on Windows
  hnd <- mkHandleFromFD fd
                        Stream
                        name
                        iomode
                        False   -- non-blocking mode is ignored on Windows
                        Nothing -- binary mode
  hSetBuffering hnd NoBuffering
  return hnd

handleToFd :: HANDLE -> IO CInt
handleToFd hnd =
  _open_osfhandle (fromIntegral (ptrToIntPtr hnd))
                  (#const _O_BINARY)


-- | Close underlaying 'HANDLE'; It is just 'closeHandle' renamed for
-- consistency sake.
--
closePipe :: HANDLE
          -> IO ()
closePipe = closeHandle

--
-- Read from a pipe
--

win32_ReadFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
win32_ReadFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "ReadFile" $ c_ReadFile h buf n p_n (maybePtr mb_over)
  peek p_n

foreign import ccall interruptible "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- | Interruptible read from a Windows 'HANDLE'.
--
readPipe :: HANDLE
         -> Int
         -> IO ByteString
readPipe h size
  = BS.createAndTrim size
      (\ptr ->
        fromIntegral <$>
          win32_ReadFile h ptr (fromIntegral size) Nothing)

-- | Get a single line from a 'HANDLE'.
--
pGetLine :: HANDLE
         -> IO String
pGetLine h = go ""
    where
      go :: String -> IO String
      go !s = do
        [x] <- BSC.unpack <$> readPipe h 1
        if x == '\n'
          then pure (reverse s)
          else go (x : s)


--
-- Write to a pipe
--

win32_WriteFile :: HANDLE
                -> Ptr a
                -> DWORD
                -> Maybe LPOVERLAPPED
                -> IO DWORD
win32_WriteFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_WriteFile h buf n p_n (maybePtr mb_over)
  peek p_n

foreign import ccall interruptible "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- | Write a 'ByteString' to a pipe.
--
writePipe :: HANDLE
          -> ByteString
          -> IO ()
writePipe h bs = BS.unsafeUseAsCStringLen bs $
    \(str, len) ->
      void $ win32_WriteFile h (castPtr str) (fromIntegral len) Nothing
