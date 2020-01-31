{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Async.File
    ( -- * HANDLE API
      readHandle
    , writeHandle

      -- * NamedPipe API
    , connectNamedPipe
    ) where

import Prelude hiding (read)

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import Data.Functor (void)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.StablePtr (StablePtr)

import System.Win32.Types (HANDLE)
import qualified System.Win32.Types as Win32
import System.Win32.Async.ErrCode
import System.Win32.Async.Internal

-- | Read a given number of bytes from a 'HANDLE'.  The 'HANDLE' must be
-- opened with 'System.Win32.fILE_FLAG_OVERLAPPED' and associated with an IO
-- completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.
--
readHandle :: HANDLE
           -> Int
           -> IO ByteString
readHandle h size = BS.createAndTrim size
      (\ptr ->
          fromIntegral <$>
            waitForCompletion "readHandle" (c_AsyncRead h ptr (fromIntegral size)))

foreign import ccall safe "HsAsyncRead"
    c_AsyncRead :: HANDLE
                -> Ptr a
                -> CInt
                -> StablePtr b
                -> IO ()


-- | Write a 'ByteString' to a 'HANDLE'.  The 'HANDLE' must be opened with
-- 'System.Win32.fILE_FLAG_OVERLAPPED' and associated with an IO completion port
-- via 'System.Win32.Async.IOManager.associateWithIOCompletionPort'.
--
writeHandle :: HANDLE
            -> ByteString
            -> IO ()
writeHandle h bs = BS.unsafeUseAsCStringLen bs $
    \(str, len) ->
        void $ waitForCompletion "writeHandle" (c_AsyncWrite h (castPtr str) (fromIntegral len))

foreign import ccall safe "HsAsyncWrite"
    c_AsyncWrite :: HANDLE
                 -> Ptr a
                 -> CInt
                 -> StablePtr b
                 -> IO ()


-- | Connect named pipe aka accept a connection.  The 'HANDLE' must be opened
-- with 'System.Win32.FILE_FLAG_OVERLLAPPED' and associated with IO completion
-- port via 'System.Win32.Async.IOManager.associateWithIOCompletionPort'.
--
-- [msdn documentation](https://docs.microsoft.com/en-us/windows/win32/api/namedpipeapi/nf-namedpipeapi-connectnamedpipe)
--
connectNamedPipe :: HANDLE -> IO ()
connectNamedPipe h = void $ waitForCompletion "connectNamedPipe" $ \ptr -> do
    c_ConnectNamedPipe h ptr
    errCode <- Win32.getLastError
    -- connectNamedPipe can error with:
    --
    -- 'ERROR_PIPE_LISTENING' - the pipe is listening, we need to wait more
    -- 'ERROR_PIPE_CONNECTED' - the pipe is already connected
    -- 'ERROR_NO_DATA'        - previous client has not disconnected, we
    --                          should error
    -- 'ERROR_IO_PENDING'     - IO is pending, 'waitForCompletion' should
    --                            resolve this
    unless (   errCode == eRROR_PIPE_LISTENING
            || errCode == eRROR_PIPE_CONNECTED
            || errCode == eRROR_IO_PENDING)
        $ Win32.failWith ("connectNamedPipe (" ++ show errCode ++ ")") errCode

foreign import ccall interruptible "HsConnectNamedPipe"
    c_ConnectNamedPipe :: HANDLE -> StablePtr a -> IO ()
