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
import Foreign.C (CInt (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (deRefStablePtr, freeStablePtr)
import Foreign.Marshal (alloca)
import Foreign.Storable (Storable (..))

import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import System.Win32.Types (BOOL, HANDLE, DWORD)
import qualified System.Win32.Types as Win32
import qualified System.Win32.Mem   as Win32
import qualified System.Win32.File  as Win32 (closeHandle)
import System.Win32.Async.ErrCode
import System.Win32.Async.IOData

-- | New type wrapper which holds 'HANDLE' of the I/O completion port.
-- <https://docs.microsoft.com/en-us/windows/win32/fileio/createiocompletionport>
--
newtype IOCompletionPort = IOCompletionPort HANDLE
  deriving Show

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

-- | Associate with I/O completion port.  This can be used multiple times on
-- a file descriptor.
--
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
    alloca $ \numBytesPtr -> do
      -- make 'GetQueuedCompletionStatus' system call which dequeues
      -- a packet from io completion packet.  We use 'maxBound' as the timeouts
      -- for dequeueing completion packets from IOCP.  The thread that runs
      -- 'dequeueCompletionPackets' is ment to run for the entire execution time
      -- of an appliction.
      GQCSResult { gqcsResult
                 , gqcsOverlappedIsNull
                 , gqcsCompletionKey
                 , gqcsIODataPtr
                 }
          <- getQueuedCompletionStatus port numBytesPtr maxBound
      errorCode <- Win32.getLastError

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
         | not gqcsCompletionKey ->
           -- skip if the completion key did not match with our
           -- 'MAGIC_COMPLETION_KEY'
            dequeueCompletionPackets iocp
         | gqcsResult -> do
           -- 'GetQueuedCompletionStatus' system call returned without errors.
             !(numBytes :: Int) <-
                 fromIntegral <$> peek numBytesPtr
             mvarPtr <- peek (iodDataPtr AsyncSing gqcsIODataPtr)
             mvar <- deRefStablePtr mvarPtr
             freeStablePtr mvarPtr
             hp <- Win32.getProcessHeap
             Win32.heapFree hp 0 (castPtr gqcsIODataPtr)
             success <- tryPutMVar mvar (Right numBytes)
             when (not success)
               $ fail "System.Win32.Async.dequeueCompletionPackets: MVar is not empty."
             dequeueCompletionPackets iocp
         | otherwise -> do
           -- the async action returned with an error
             mvarPtr <- peek (iodDataPtr AsyncSing gqcsIODataPtr)
             mvar <- deRefStablePtr mvarPtr
             freeStablePtr mvarPtr
             hp <- Win32.getProcessHeap
             Win32.heapFree hp 0 (castPtr gqcsIODataPtr)
             success <- tryPutMVar mvar (Left (ErrorCode errorCode))
             when (not success)
               $ fail "System.Win32.Async.dequeueCompletionPackets: MVar is not empty."
             dequeueCompletionPackets iocp


-- | Return value of 'HsGetQueuedCompletionStatus'.
--
data GQCSResult a = GQCSResult {
      gqcsResult           :: Bool,
      -- ^ return valud of 'GetQueuedCompletionStatus'
      gqcsOverlappedIsNull :: Bool,
      -- ^ wheather 'OVERLAPPED' pointer was null.  According to MSDN:
      --
      -- lpOverlapped is NULL, the function did not dequeue a completion packet
      -- from the completion port. In this case, the function does not store
      -- information in the variables pointed to by the lpNumberOfBytes and
      -- lpCompletionKey parameters, and their values are indeterminate.
      --
      -- Source: <https://docs.microsoft.com/en-us/windows/win32/api/ioapiset/nf-ioapiset-getqueuedcompletionstatus#remarks>
      gqcsCompletionKey   :: Bool,
      -- ^ true iff 'completionKey' matches with 'MAGIC_COMPLETION_KEY' which
      -- we use in requests.
      gqcsIODataPtr       :: Ptr AsyncIOCPData
    -- ^ it is vital that 'gqcsIODataPtr' type is in sync with what we allocate
    -- in 'withIODataPtr', otherwise 'dequeueCompletionPackets' likely
    -- will crash.  For this reason we use a type alias.
    -- 'dequeueCompletionPackets' will never see @'WsaAsyncIOCPData' a@, but it
    -- is safe to use it (what we do in 'System.Win32.Async.Socket' module).
    }

instance Storable (GQCSResult a) where
    sizeOf    _ = (#const sizeof(GQCSRESULT))
    alignment _ = (#alignment GQCSRESULT)

    poke buf GQCSResult { gqcsResult
                        , gqcsOverlappedIsNull
                        , gqcsCompletionKey
                        , gqcsIODataPtr
                        } = do
      (#poke GQCSRESULT, gqcsResult)           buf gqcsResult
      (#poke GQCSRESULT, gqcsOverlappedIsNull) buf gqcsOverlappedIsNull
      (#poke GQCSRESULT, gqcsCompletionKey)    buf gqcsCompletionKey
      (#poke GQCSRESULT, gqcsIODataPtr)        buf gqcsIODataPtr

    peek buf =
      GQCSResult <$> (#peek GQCSRESULT, gqcsResult)           buf
                 <*> (#peek GQCSRESULT, gqcsOverlappedIsNull) buf
                 <*> (#peek GQCSRESULT, gqcsCompletionKey)    buf
                 <*> (#peek GQCSRESULT, gqcsIODataPtr)        buf


-- | A thin wrapper around 'HsGetQueuedCompletionStatus', this makes clear the
-- intention of 'GQCSResult' data type.
--
getQueuedCompletionStatus
    :: HANDLE
      -- ^ completion port's 'HANDLE'
    -> Ptr Word32
      -- ^ lpNumberOfBytesTransferred; For the stored value to make sense the
      -- 'gqcsOverlappedIsNull' must be 'True'.
    -> Word32
      -- ^ timeout in millisecons
    -> IO (GQCSResult a)
getQueuedCompletionStatus completionPort lpNumberOfBytesTransferred dwMilliseconds =
    alloca $ \gqcsResultPtr -> do
      c_GetQueuedCompletionStatus completionPort lpNumberOfBytesTransferred dwMilliseconds gqcsResultPtr
      peek gqcsResultPtr


foreign import ccall safe "HsGetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus
      :: HANDLE
      -- ^ completion port
      -> Ptr Word32
      -- ^ lpNumberOfBytesTransferred
      -> Word32
      -- ^ dwMilliseconds
      -> Ptr (GQCSResult a)
      -- ^ result of 'GetQueuedCompletinStatus' system call
      -> IO ()
