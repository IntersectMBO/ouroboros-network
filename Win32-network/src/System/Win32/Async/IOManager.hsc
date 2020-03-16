{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

#include <Win32-network.h>

module System.Win32.Async.IOManager
    ( -- * IOManager
      IOCompletionPort
    , associateWithIOCompletionPort
    , withIOManager
    , IOManagerError (..)

      -- * Low level IOCP interface
    , dequeueCompletionPackets
    , createIOCompletionPort
    , closeIOCompletionPort
    ) where

import Control.Concurrent
import Control.Exception ( Exception (..)
                         , IOException
                         , bracket
                         , catch
                         , throwIO
                         , throwTo
                         )
import Control.Monad (when)
import Data.Word (Word32)
import Data.Functor (void)
import Foreign.C (CInt (..), CUIntPtr (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr)
import Foreign.Marshal (alloca, free)
import Foreign.Storable (Storable (..))

import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import System.Win32.Types (BOOL, HANDLE, DWORD, ULONG_PTR)
import qualified System.Win32.Types as Win32
import qualified System.Win32.File  as Win32 (closeHandle)
import System.Win32.Async.ErrCode
import System.Win32.Async.IOData
import System.Win32.Async.Overlapped

import Unsafe.Coerce (unsafeCoerce)

-- | New type wrapper which holds 'HANDLE' of the I/O completion port.
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport>
--
newtype IOCompletionPort = IOCompletionPort HANDLE
  deriving Show

closeIOCompletionPort :: IOCompletionPort -> IO ()
closeIOCompletionPort (IOCompletionPort iocp) = Win32.closeHandle iocp

-- | The completion key used by this library for all overllaped IO.
--
magicCompletionKey :: ULONG_PTR
magicCompletionKey = 696205568

-- | Windows documentation:
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport>
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
              0 -- it is ignored since we are passing 'iNVALID_HANDLE_VALUE':
                -- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport#parameters>
              concurrentThreads)

foreign import ccall unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort :: HANDLE    -- ^ FileHandle
                             -> HANDLE    -- ^ ExistingCompletionPort
                             -> ULONG_PTR -- ^ CompletionKey
                             -> DWORD     -- ^ NumberOfConcurrentConnections
                             -> IO HANDLE

-- | Associate with I/O completion port.  This can be used multiple times on
-- a file descriptor.
--
associateWithIOCompletionPort :: Either HANDLE Socket
                              -> IOCompletionPort
                              -> IO ()
associateWithIOCompletionPort (Left h) (IOCompletionPort iocp) = do
    Win32.failIfFalse_ "associateWithIOCompletionPort (HANDLE)" $
      (iocp ==) <$> c_CreateIoCompletionPort h iocp magicCompletionKey 0
associateWithIOCompletionPort (Right sock) (IOCompletionPort iocp) =
    Socket.withFdSocket sock $ \fd ->
      let h = coerceFdToHANDLE fd
      in Win32.failIfFalse_ "associateWithIOCompletionPort (SOCKET)" $
          (iocp ==) <$> c_CreateIoCompletionPort h iocp magicCompletionKey 0
  where
    -- this is actually safe
    coerceFdToHANDLE :: CInt -> HANDLE
    coerceFdToHANDLE = unsafeCoerce

-- | A newtype warpper for 'IOError's whihc are catched in the 'IOManager'
-- thread and are re-thrown in the main application thread.
--
newtype IOManagerError = IOManagerError IOException
  deriving (Show, Eq)

instance Exception IOManagerError


-- | Create an 'IOCompletionPort' and start a I/O manager thread (bound thread).
-- The IOManager thread will only run for the duration of the callback.
--
-- The 'IOManager' will throw 'IOException' back to the starting thread (which
-- should be the main application thread).  There's no point of running the
-- application if the 'dequeueCompletionPackets' died, which most likely
-- indicates a bug in itself.
--
-- TODO: add a tracer which logs when `dequeueCompletionPackets' errors
--
withIOManager :: (IOCompletionPort -> IO r) -> IO r
withIOManager k = do
    tid <- myThreadId
    bracket
        (createIOCompletionPort maxBound)
        closeIOCompletionPort
        $ \iocp -> do
          -- The 'c_GetQueuedCompletionStatus' is not interruptible so we
          -- cannot simply use `withAsync` pattern here (the main thread will
          -- deadlock when trying to kill the io-manager thread).
          -- But note that 'closeIOCopletionPort' will terminate the io-manager
          -- thread (we cover this scenario in the 'test_closeIOCP' test).
          _ <-
            forkOS
              $ void $ dequeueCompletionPackets iocp
                `catch`
                \(e :: IOException) -> do
                  -- throw IOExceptoin's back to the thread which started 'IOManager'
                  throwTo tid (IOManagerError e)
                  throwIO e
          k iocp


data IOCompletionException
    = NullPointerException
    | NullOverlappedPointer
    | IOCompoletionError !Win32.ErrCode
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
dequeueCompletionPackets :: IOCompletionPort
                         -- ^ handle of a completion port
                         -> IO ()
dequeueCompletionPackets iocp@(IOCompletionPort port) =
    alloca $ \numBytesPtr ->
      alloca $ \lpCompletionKey ->
        alloca $ \lpOverlappedPtr -> do
      -- make 'GetQueuedCompletionStatus' system call which dequeues
      -- a packet from io completion packet.  We use 'maxBound' as the timeouts
      -- for dequeueing completion packets from IOCP.  The thread that runs
      -- 'dequeueCompletionPackets' is ment to run for the entire execution time
      -- of an appliction.
      gqcsResult <- c_GetQueuedCompletionStatus port numBytesPtr lpCompletionKey lpOverlappedPtr maxBound
      errorCode <- Win32.getLastError
      lpOverlapped <- peek lpOverlappedPtr
      completionKey <- peek lpCompletionKey
      let gqcsOverlappedIsNull = lpOverlapped == nullPtr

      -- gqcsIODataPtr was allocated by 'withIODataPtr' or
      -- 'wsaWaitForCompletion', and we are responsible to deallocate it but
      -- only if 'gqcsCompletionKey' is 'True'.

      if | gqcsOverlappedIsNull && errorCode == eRROR_ABANDONED_WAIT_0 ->
           -- this is triggered by closing IO completion 'HANDLE' while there
           -- are outstanding requests (this is documented
           -- <https://docs.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus#remarks>)
              return ()
         | gqcsOverlappedIsNull && errorCode == eRROR_INVALID_HANDLE ->
           -- If one doesn't terminate the dequeueing thread, some of the tests
           -- will take 10x-100x more time to complete:
           --
           -- - 'Async.Handle.async reads and writes'
           -- - 'Async.Handle.PingPongPipelined test'
           -- - 'Async.Socket.PingPong test'
           -- - 'Async.Socket.PingPongPipelined test'
           --
           -- or not terminate at all within reasonable timeout:
           --
           -- - 'Async.Handle.PingPong test'
           --
           -- Note that in the test setup, we start io manager thread for each
           -- test case.  If we don't terminate it when the iocp port is closed
           -- we might end up with a busy loop which runs expensive system
           -- calls that fail.  This could explain the slowdown.
           --
           -- In every observed case both values:
           --
           -- - 'gqcsResult'
           -- - 'gqcsCompletionKey'
           --
           -- where false.
           --
               return ()
         | gqcsOverlappedIsNull ->
           -- 'GetLastCompletionStatus' has not dequeued any completion packet
           -- from the completion port.  Must be the first clause, since if
           -- this is not true we cannot trust other arguments.
              throwIO NullOverlappedPointer
         | completionKey /= magicCompletionKey ->
            dequeueCompletionPackets iocp
         | gqcsResult -> do
           -- 'GetQueuedCompletionStatus' system call returned without errors.
             !(numBytes :: Int) <-
                 fromIntegral <$> peek numBytesPtr
             let ioDataPtr :: Ptr AsyncIOCPData
                 ioDataPtr = castPtr lpOverlapped
             mvarPtr <- peek (iodDataPtr AsyncSing ioDataPtr)
             mvar <- deRefStablePtr mvarPtr
             freeStablePtr mvarPtr
             free ioDataPtr
             success <- tryPutMVar mvar (Right numBytes)
             when (not success)
               $ fail "System.Win32.Async.dequeueCompletionPackets: MVar is not empty."
             dequeueCompletionPackets iocp
         | otherwise -> do
           -- the async action returned with an error
             let ioDataPtr :: Ptr AsyncIOCPData
                 ioDataPtr = castPtr lpOverlapped
             mvarPtr <- peek (iodDataPtr AsyncSing ioDataPtr)
             mvar <- deRefStablePtr mvarPtr
             freeStablePtr mvarPtr
             free ioDataPtr
             success <- tryPutMVar mvar (Left (ErrorCode errorCode))
             when (not success)
               $ fail "System.Win32.Async.dequeueCompletionPackets: MVar is not empty."
             dequeueCompletionPackets iocp


foreign import ccall safe "GetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus
      :: HANDLE
      -- ^ completion port
      -> Ptr Word32
      -- ^ lpNumberOfBytesTransferred
      -> Ptr ULONG_PTR
      -- ^ lpCompletionKey
      -> Ptr LPOVERLAPPED
      -- ^ lpOverlapped
      -> Word32
      -- ^ dwMilliseconds
      -> IO BOOL
