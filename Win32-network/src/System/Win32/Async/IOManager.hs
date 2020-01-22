{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Win32.Async.IOManager
    ( -- * IOManager
      IOCompletionPort
    , createIOCompletionPort
    , closeIOCompletionPort
    , associateWithIOCompletionPort
    , withIOManager
    ) where

import Control.Concurrent
import Control.Exception (Exception (..), bracket, throwIO)
import Control.Monad (when)
import Data.Word (Word32)
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr)
import Foreign.Marshal (alloca)
import Foreign.Storable (Storable (..))

import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import System.Win32.Types (BOOL, HANDLE, DWORD)
import qualified System.Win32.Types as Win32
import qualified System.Win32.File  as Win32 (closeHandle)
import System.Win32.Async.ErrCode

-- | New type wrapper which holds 'HANDLE' of the I/O completion port.
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport>
--
newtype IOCompletionPort = IOCompletionPort HANDLE

closeIOCompletionPort :: IOCompletionPort -> IO ()
closeIOCompletionPort (IOCompletionPort iocp) = Win32.closeHandle iocp

-- | Windows documentation:
--
createIOCompletionPort
    :: DWORD  -- ^ number of concurrent threads
    -> IO IOCompletionPort
createIOCompletionPort concurrentThreads
    = IOCompletionPort <$>
        Win32.failIfNull "createIoCompletionPort"
          (c_CreateIoCompletionPort
            Win32.iNVALID_HANDLE_VALUE
            nullPtr
            nullPtr
            concurrentThreads)

foreign import ccall unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE -> HANDLE -> Ptr () -> DWORD -> IO HANDLE

associateWithIOCompletionPort :: Either HANDLE Socket
                              -> IOCompletionPort
                              -> IO ()
associateWithIOCompletionPort (Left h) iocp = do
    Win32.failIfFalse_ "associateWithIOCompletionPort" (c_AssociateHandle h iocp)
associateWithIOCompletionPort (Right sock) iocp =
    Socket.withFdSocket sock $ \fd -> do
      Win32.failIfFalse_ "associateWithIOCompletionPort" (c_AssociateSocket fd iocp)

foreign import ccall safe "HsAssociateHandle"
    c_AssociateHandle :: HANDLE           -- ^ handle
                      -> IOCompletionPort -- ^ I/O completion port
                      -> IO BOOL

foreign import ccall safe "HsAssociateSocket"
    c_AssociateSocket :: CInt             -- ^ Socket descriptor
                      -> IOCompletionPort -- ^ I/O completion port
                      -> IO BOOL

-- | Create an 'IOCompletionPort' and start a I/O manager thread (bound thread).
-- The IOManager thread will only run for the duration of the callback.
--
withIOManager :: (IOCompletionPort -> IO r) -> IO r
withIOManager k =
    bracket
        (createIOCompletionPort maxBound)
        closeIOCompletionPort
        $ \iocp -> do
          _ <- forkOS (handleCompletions iocp)
          k iocp

data IOCompletionException = NullPointerException
  deriving Show

instance Exception IOCompletionException

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
             -- event is not triggered by a read or write, and thus the
             -- 'mvar_ptr' must not be freed.
             pure False

           | errCode /= 0 -> do
             mvar <- deRefStablePtr mvar_ptr
             freeStablePtr mvar_ptr
             success <- tryPutMVar mvar (Left errCode)
             when (not success)
               $ fail "System.Win32.Async.handleCompletions: MVar is not empty."
             pure True

           | otherwise -> do
             !(num_bytes :: Int) <-
                 fromIntegral <$> peek num_bytes_ptr
             mvar <- deRefStablePtr mvar_ptr
             freeStablePtr mvar_ptr
             success <- tryPutMVar mvar (Right num_bytes)
             when (not success)
               $ fail "System.Win32.Async.handleCompletions: MVar is not empty."
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
