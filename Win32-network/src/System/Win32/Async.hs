{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Async
    ( -- * IOManager
      IOCompletionPort
    , withIOManager
    , associateWithIOCompletionPort

      -- * HANDLE API
    , readHandle
    , writeHandle

      -- * NamedPipe API
    , connectNamedPipe

      -- * Windows Error Codes
    , eRROR_ABANDONED_WAIT_0
    , eRROR_HANDLE_EOF
    , eRROR_INVALID_HANDLE
    , eRROR_IO_PENDING
    ) where

import Prelude hiding (read)

import Control.Concurrent
import Control.Exception (Exception (..), bracket, throwIO)
import Control.Monad (when, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import Data.Functor (void)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.Marshal (alloca)
import Foreign.Storable

import System.Win32.Types (HANDLE, ErrCode)
import qualified System.Win32.Types as Win32
import qualified System.Win32.File  as Win32 (closeHandle)
import qualified System.Win32.CompletionPort  as Win32 (createIoCompletionPort)

data IOCompletionException = NullPointerException
  deriving Show

instance Exception IOCompletionException

-- | This error is thrown by 'GetQueuedCompletionsStatus' when I/O completion
-- port is closed.
-- 
eRROR_ABANDONED_WAIT_0 :: ErrCode
eRROR_ABANDONED_WAIT_0 = 735

-- | Reached the end of file.
--
eRROR_HANDLE_EOF :: ErrCode
eRROR_HANDLE_EOF = 38

-- | The 'HANDLE' is invalid.
--
eRROR_INVALID_HANDLE :: ErrCode
eRROR_INVALID_HANDLE = 6

eRROR_IO_PENDING :: ErrCode
eRROR_IO_PENDING = 997

eRROR_PIPE_CONNECTED :: ErrCode
eRROR_PIPE_CONNECTED = 535

eRROR_PIPE_LISTENING :: ErrCode
eRROR_PIPE_LISTENING = 536

-- | New type wrapper which holds 'HANDLE' of the I/O completion port.
--
newtype IOCompletionPort = IOCompletionPort HANDLE

createIOCompletionPort :: IO IOCompletionPort
createIOCompletionPort =
    IOCompletionPort <$>
      Win32.createIoCompletionPort Win32.iNVALID_HANDLE_VALUE nullPtr maxBound

-- | Create an 'IOCompletionPort' and start a I/O manager thread (bound thread).
-- The IOManager thread will only run for the duration of the callback.
--
withIOManager :: (IOCompletionPort -> IO r) -> IO r
withIOManager k =
    bracket
        createIOCompletionPort
        (\(IOCompletionPort port) -> Win32.closeHandle port)
        $ \iocp -> do
          _ <- forkOS (handleCompletions iocp)
          k iocp

-- | I/O manager which handles completions of I/O operations.  It should run on
-- a bound thread.  It dereferences the stable pointed which was allocated by
-- `readAsync` or `writeAsync`, and puts the results of the operation in the
-- 'MVar' which it points to.  This wakes up the 'waitForCompletion' function.
--
-- To safely shutdown the I/O manager, it's enough to close the port's 'HANDLE'.
-- This should be done after all the associated handles are closed.
--
-- The underlaying 'c' code is using a fixed completion key which is not exposed
-- on the Haskell side.
--
handleCompletions :: IOCompletionPort
                  -- ^ handle of a completion port
                  -> IO ()
handleCompletions iocp@(IOCompletionPort port) = do
    cont <- alloca $ \num_bytes_ptr ->
      alloca $ \userdata_ptr -> do
        res <- c_GetQueuedCompletionStatus port num_bytes_ptr userdata_ptr maxBound
        when (userdata_ptr == nullPtr)
          -- Did we received wrong comletion key?
          $ throwIO NullPointerException
        mvar_ptr <- peek userdata_ptr
        errCode <-
          if not res
            then Win32.getLastError
            else return 0

        -- TODO: add a tracer which logs errors
        if |    errCode == eRROR_ABANDONED_WAIT_0
             || errCode == eRROR_INVALID_HANDLE -> do
             -- I/O completion port handle was closed during asynchronous
             -- operation
             -- https://docs.microsoft.com/en-gb/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus#remarks
             -- or the completion port handle is invalid, in both cases the
             -- event is  not triggered with an read / write operations, and
             -- thus the 'mvar_ptr' must not be freed.
             pure False
           | errCode /= 0 -> do
             mvar <- deRefStablePtr mvar_ptr
             freeStablePtr mvar_ptr
             success <- tryPutMVar mvar (Left errCode)
             when (not success) $ fail
                 "System.Win32.Async.handleCompletions: \
                 \MVar is not empty."
             pure True

           | otherwise -> do
             !(num_bytes :: Int) <-
                 fromIntegral <$> peek num_bytes_ptr
             mvar <- deRefStablePtr mvar_ptr
             freeStablePtr mvar_ptr
             success <- tryPutMVar mvar (Right num_bytes)
             when (not success) $ fail
                 "System.Win32.Async.handleCompletions: \
                   \MVar is not empty."
             pure True
    if cont
      then handleCompletions iocp
      else pure ()

foreign import ccall safe "HsGetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus
      :: HANDLE
      -> Ptr Word32
      -> Ptr a
      -> Word32
      -> IO Bool

waitForCompletion :: (StablePtr (MVar (Either ErrCode Int)) -> IO ())
                  -> IO Int
waitForCompletion asyncIO = do
    wait_var <- newEmptyMVar
    -- the pointer is freed in 'c_GetQueuedCompletionStatus'
    wait_var_ptr <- newStablePtr wait_var
    asyncIO wait_var_ptr
    errCode <- Win32.getLastError
    if errCode == eRROR_IO_PENDING
      then do
        -- The operation is initiated asynchronously.
        -- We wait for the completion of the operation.
        res' <- takeMVar wait_var
        case res' of
            Right num_bytes
                   -> return num_bytes
            Left e |  e == eRROR_HANDLE_EOF
                   || e == eRROR_PIPE_CONNECTED
                   -- we are treating ERROR_PIPE_LISTENING as an error at this point
                   -> pure 0
                   | otherwise
                   -> Win32.failWith ("waitForCompletion: " ++ show e) e
      else
        -- An error occurred.
        if   errCode == eRROR_HANDLE_EOF
          || errCode == eRROR_PIPE_CONNECTED
          then return 0
          else Win32.failWith ("waitForCompletion: " ++ show errCode) errCode

-- | Read a given number of bytes from a 'HANDLE'.
--
readHandle :: HANDLE
           -> Int
           -> IO ByteString
readHandle h size = BS.createAndTrim size
      (\ptr ->
          fromIntegral <$>
            waitForCompletion (c_AsyncRead h ptr (fromIntegral size)))

foreign import ccall safe "HsAsyncRead"
    c_AsyncRead :: HANDLE
                -> Ptr a
                -> CInt
                -> StablePtr b
                -> IO ()


-- | Write a 'ByteString' to a HANDLE.
--
writeHandle :: HANDLE
            -> ByteString
            -> IO ()
writeHandle h bs = BS.unsafeUseAsCStringLen bs $
    \(str, len) ->
        void $ waitForCompletion (c_AsyncWrite h (castPtr str) (fromIntegral len))

foreign import ccall safe "HsAsyncWrite"
    c_AsyncWrite :: HANDLE
                 -> Ptr a
                 -> CInt
                 -> StablePtr b
                 -> IO ()

-- | Associate a 'HANDLE' to a completion port.
foreign import ccall safe "HsAssociate"
    associateWithIOCompletionPort :: HANDLE           -- ^ handle
                                  -> IOCompletionPort -- ^ I/O completion port
                                  -> IO ()

connectNamedPipe :: HANDLE -> IO ()
connectNamedPipe h = void $ waitForCompletion $ \ptr -> do
    c_ConnectNamedPipe h ptr
    errCode <- Win32.getLastError
    -- connectNamedPipe can error with:
    -- * 'ERROR_PIPE_LISTENING' - the pipe is listening, we need to wait more
    -- * 'ERROR_PIPE_CONNECTED' - the pipe is already connected
    -- * 'ERROR_NO_DATA'        - previous client has not disconnected, we
    --                            should error
    -- * 'ERROR_IO_PENDING'     - IO is pending, 'waitForCompletion' should
    --                            resolve this
    unless (   errCode == eRROR_PIPE_LISTENING
            || errCode == eRROR_PIPE_CONNECTED
            || errCode == eRROR_IO_PENDING)
        $ Win32.failWith ("connectNamedPipe (" ++ show errCode ++ ")") errCode
           

foreign import ccall interruptible "HsConnectNamedPipe"
    c_ConnectNamedPipe :: HANDLE -> StablePtr a -> IO ()
