{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include <windows.h>
#include <Win32-network.h>

module System.Win32.Async.IOData
  ( IOData (..)
  , iodOverlappedPtr
  , iodDataPtr

  , Result (..)
  , withIODataPtr
  ) where

import Control.Concurrent ( MVar
                          , newEmptyMVar
                          )
import Control.Exception (mask)
import Control.Monad (when)
import Foreign ( Ptr
               , StablePtr
               , Storable (..)
               , castPtr
               , newStablePtr
               , nullPtr
               , plusPtr
               )

import           System.Win32.Types (ErrCode)
import qualified System.Win32.Types as Win32
import qualified System.Win32.Mem   as Win32

import System.Win32.Async.Overlapped


-- | Data associated with each async call.  This is represented as the 'IODATA'
-- struct.  The windows kernel macro 'CONTAINING_RECORD' allows to access the
-- 'IODATA' when the io action is signaled 'GetQueuedCompletionStatus' via the
-- 'iodOverlapped :: OVERLAPPED' member.
--
data IOData a = IOData {
      iodOverlapped :: OVERLAPPED
      -- ^ 'OVERLAPPED' structue passed through the iocp port.
    , iodData       :: StablePtr (MVar a)
      -- ^ stable pointer which hitchhikes through the iocp port.
    }

-- | Acces the 'OVERLAPPED' member of 'IODATA' struct, in c:
--
-- >  ioData->iodOverlapped
--
iodOverlappedPtr :: Ptr (IOData a) -> Ptr OVERLAPPED
iodOverlappedPtr = (#ptr IODATA, iodOverlapped)

iodDataPtr :: Ptr (IOData a) -> Ptr (StablePtr (MVar a))
iodDataPtr = (#ptr IODATA, iodData)


instance Storable (IOData a) where
    sizeOf    _ = (#const sizeof(IODATA))
    alignment _ = (#alignment IODATA)

    poke buf IOData {iodOverlapped, iodData} = do
      (#poke IODATA, iodOverlapped) buf iodOverlapped
      (#poke IODATA, iodData)       buf iodData

    peek buf = do
      iodOverlapped <- (#peek IODATA, iodOverlapped) buf
      iodData       <- (#peek IODATA, iodData)       buf
      return $ IOData { iodOverlapped
                      , iodData
                      }

-- | Result of an async i/o action.  It maybe either asynchronous or
-- synchronous result or error.
--
-- One has to be really careful with passing 'True' value to 'ResultSync' or
-- 'ErrorSync' - it can reslult in a race between two threads deallocating the
-- same memory, subsequently os killing the process (often silently).
--
data Result a
    = ResultAsync !a
    -- ^ Asynchronous result, as returned by the iocp thread.
    | ErrorAsync  !ErrCode
    -- ^ Asynchronous error, as returned by the iocp thread.
    | ResultSync  !a       !Bool
    -- ^ Synchronous result; the if the 'Bool' value is true, `ioDataPtr` will
    -- be deallocated.
    | ErrorSync   !ErrCode !Bool
    -- ^ Synchronous error; the if the 'Bool' value is true, `ioDataPtr` will
    -- be deallocated.
  deriving Show


-- | Allocate 'IOData' on process heap.  If the continuation does not succeed
-- ang 'getLastError' is not 'ERROR_IO_PENDING' it will deallocate 'IOData'
-- otherwise the resposibility of deallocation is in the
-- 'dequeueCompletionPackets' thread.
--
-- It is executed with async exception masked to ensure that 'ioDataPtr' is
-- filled in with 'IOData' before it is passed to the continuation.
--
-- If the continuation throws 'IOException' we assume that the completion
-- packed was not enqueued in the io completion port, and thus we free the
-- allocated 'IOData'.
--
withIODataPtr :: Show a
              => String
              -> (LPOVERLAPPED -> MVar (Either ErrCode a) -> IO (Result a))
              -- ^ continuation, executed with unmasked async exceptions.  It
              -- receives 'LPOVERLAPPED' allocated on process heap and an empty
              -- 'MVar' which will be filled by 'dequeueCompletionPackets'
              -- ('IOManager' thread), when the async IO terminates.
              -> IO a
withIODataPtr errorTag k = mask $ \unmask -> do
    ph <- Win32.getProcessHeap
    -- using malloc will not work, we have to allocate memory in process heap
    -- succeeded
    ioDataPtr <-
      castPtr <$>
        Win32.heapAlloc
          ph Win32.hEAP_ZERO_MEMORY
          (fromIntegral $ sizeOf (undefined :: IOData a))
    waitVar <- newEmptyMVar
    iodData <- newStablePtr waitVar
    let ioData = IOData { iodOverlapped = OVERLAPPED {
                              ovl_internal     = 0,
                              ovl_internalHigh = 0,
                              ovl_offset       = 0,
                              ovl_offsetHigh   = 0,
                              ovl_hEvent       = nullPtr
                            }
                        , iodData
                        }
    poke ioDataPtr ioData
    result <-
      (unmask $ k (iodOverlappedPtr ioDataPtr) waitVar)

    case result of
      ResultAsync b ->
        return b

      ErrorAsync  errorCode ->
        Win32.failWith ("withIODataPtr (" ++ errorTag ++ ")") errorCode

      ResultSync b deallocate -> do
        when deallocate
          $ Win32.heapFree ph 0 (castPtr ioDataPtr)
        return b

      ErrorSync errorCode deallocate -> do
        when deallocate
          $ Win32.heapFree ph 0 (castPtr ioDataPtr)
        Win32.failWith ("withIODataPtr (" ++ errorTag ++ ")") errorCode
