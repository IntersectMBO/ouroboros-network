#include <fcntl.h>
#include <windows.h>

{-# LANGUAGE CPP                #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NumericUnderscores #-}

-- | For full details on the Windows named pipes API see
-- <https://docs.microsoft.com/en-us/windows/desktop/ipc/named-pipes>
--
module System.Win32.NamedPipes (

    -- * Named pipe server APIs
    createNamedPipe,
    pIPE_UNLIMITED_INSTANCES,

    -- ** Paramater types
    LPSECURITY_ATTRIBUTES,
    OpenMode,
    pIPE_ACCESS_DUPLEX,
    pIPE_ACCESS_INBOUND,
    pIPE_ACCESS_OUTBOUND,
    fILE_FLAG_OVERLAPPED,
    PipeMode,
    pIPE_TYPE_BYTE,
    pIPE_TYPE_MESSAGE,
    pIPE_READMODE_BYTE,
    pIPE_READMODE_MESSAGE,

    -- * Named pipe client APIs
    -- ** connect to a named pipe
    connect,

    -- ** waiting for named pipe instances
    waitNamedPipe,

    TimeOut,
    nMPWAIT_USE_DEFAULT_WAIT,
    nMPWAIT_WAIT_FOREVER,
  ) where


import Control.Exception
import Control.Monad (when)
import Foreign.C.String (withCString)

import System.Win32.Types hiding (try)
import System.Win32.File

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
-- We're not exposing 'PIPE_NOWAIT' (and the default 'PIPE_WAIT'), as these
-- should not be used for async I/O (ref:
-- <https://docs.microsoft.com/en-us/windows/win32/ipc/named-pipe-type-read-and-wait-modes>).
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
-- To create a named pipe which can be associate with IO completion port on
-- needs to pass 'fILE_FLAG_OVERLAPPED' to 'OpenMode' argument,
-- e.g.
--
-- >  Win32.createNamedPipe pipeName
-- >                        (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
-- >                        (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
-- >                        pIPE_UNLIMITED_INSTANCES
-- >                        512
-- >                        512
-- >                        0
-- >                        Nothing
--
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


-- | Timeout in milliseconds.
--
-- * 'nMPWAIT_USE_DEFAULT_WAIT' indicates that the timeout value passed to
--   'createNamedPipe' should be used.
-- * 'nMPWAIT_WAIT_FOREVER' - 'waitNamedPipe' will block forever, until a named
--   pipe instance is available.
--
type TimeOut = DWORD
#{enum TimeOut,
 , nMPWAIT_USE_DEFAULT_WAIT = NMPWAIT_USE_DEFAULT_WAIT
 , nMPWAIT_WAIT_FOREVER     = NMPWAIT_WAIT_FOREVER
 }


-- | Wait until a named pipe instance is available.  If there is no instance at
-- hand before the timeout, it will error with 'ERROR_SEM_TIMEOUT', i.e.
-- @invalid argument (The semaphore timeout period has expired)@
--
-- It returns 'True' if there is an available instance, subusequent
-- 'createFile' might still fail, if another thread will take turn and connect
-- before, or if the other end shuts down the name pipe.
--
-- It returns 'False' if timeout fired.
--
waitNamedPipe :: String  -- ^ pipe name
              -> TimeOut -- ^ nTimeOut
              -> IO Bool
waitNamedPipe name timeout =
    withCString name $ \ c_name -> do
      r <- c_WaitNamedPipe c_name timeout
      e <- getLastError
      if | r                      -> pure r
         | e == eRROR_SEM_TIMEOUT -> pure False
         | otherwise              -> failWith "waitNamedPipe" e


-- 'c_WaitNamedPipe' is a blocking call, hence the _safe_ import.
foreign import ccall safe "windows.h WaitNamedPipeA"
  c_WaitNamedPipe :: LPCSTR -- lpNamedPipeName
                  -> DWORD  -- nTimeOut
                  -> IO BOOL

-- | A reliable connect call, as designed in
-- <https://docs.microsoft.com/en-us/windows/win32/ipc/named-pipe-client>
--
-- The arguments are passed directly to 'createFile'.
--
-- Note we pick the more familiar posix naming convention, do not confuse this
-- function with 'connectNamedPipe' (which corresponds to posix 'accept')
--
connect :: String                      -- ^ file name
        -> AccessMode                  -- ^ dwDesiredAccess
        -> ShareMode                   -- ^ dwSharedMode
        -> Maybe LPSECURITY_ATTRIBUTES -- ^ lpSecurityAttributes
        -> CreateMode                  -- ^ dwCreationDisposition
        -> FileAttributeOrFlag         -- ^ dwFlagsAndAttributes
        -> Maybe HANDLE                -- ^ hTemplateFile
        -> IO HANDLE
connect fileName dwDesiredAccess dwSharedMode lpSecurityAttributes dwCreationDisposition dwFlagsAndAttributes hTemplateFile = connectLoop
  where
    connectLoop = do
      -- `createFile` checks for `INVALID_HANDLE_VALUE` and retries if this is
      -- caused by `ERROR_SHARING_VIOLATION`.
      mh <- try $
              createFile fileName
                         dwDesiredAccess
                         dwSharedMode
                         lpSecurityAttributes
                         dwCreationDisposition
                         dwFlagsAndAttributes
                         hTemplateFile
      case mh :: Either IOException HANDLE of
        Left e -> do
          -- unfortuntally 'ERROR_PIPE_BUSY' is not part of 'errentry' in
          -- `libraries/cbits/Win32Utils.c`.
          errorCode <- getLastError
          when (errorCode /= eRROR_PIPE_BUSY)
            $ throwIO e
          -- all pipe instance were busy, wait 20s and retry; we ignore the
          -- result
          _ <- waitNamedPipe fileName 5_000
          connectLoop

        Right h -> pure h


-- | [ERROR_PIPE_BUSY](https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-#ERROR_PIPE_BUSY):
-- all pipe instances are busy.
--
eRROR_PIPE_BUSY :: ErrCode
eRROR_PIPE_BUSY = #const ERROR_PIPE_BUSY

eRROR_SEM_TIMEOUT :: ErrCode
eRROR_SEM_TIMEOUT = #const ERROR_SEM_TIMEOUT
