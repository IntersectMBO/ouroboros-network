{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

#include <Win32-network.h>
-- includes winsock2 and windows header files

-- |  This module is provides an internal mechanism of doing asyncronous IO.
--
module System.Win32.Async.IOData
  ( withIOCPData

  , AsyncType (..)
  , AsyncSing (..)
  , IOData
  , AsyncIOCPData
  , WsaAsyncIOCPData
  , iodOverlappedPtr
  , iodDataPtr
  , ErrorCode (..)
  , FD (..)
  , Result (..)
  ) where

import Control.Concurrent ( MVar
                          , newEmptyMVar
                          )
import Control.Exception ( SomeAsyncException
                         , catch
                         , mask
                         , throwIO
                         )
import Control.Monad ( when
                     , void
                     )
import Foreign ( Ptr
               , StablePtr
               , Storable (..)
               , castPtr
               , newStablePtr
               , plusPtr
               )

import           System.Win32.Types ( HANDLE
                                    , ErrCode
                                    )
import qualified System.Win32.Types as Win32
import qualified System.Win32.Mem   as Win32

import System.Win32.Async.Internal (SOCKET)
import System.Win32.Async.Overlapped
import System.Win32.Async.ErrCode

--  | A typelevel tag which describes wheather we use async operations for
--  a 'HANDLE' or 'SOCKET' (via winsock2).
--
data AsyncType = Async | WsaAsync


-- | A singleton for 'AsyncType'.
--
data AsyncSing (asyncType :: AsyncType) where
    AsyncSing    :: AsyncSing Async
    WsaAsyncSing :: AsyncSing WsaAsync


-- |  We use two overlapped structs: 'OVERLAPPED' or 'WSAOVERLAPPED'.  This is
-- a type safe wrapper for both.  The 'Storable' instances are derived
-- from storable instances of 'OVERLAPPED' or 'WSAOVERLAPPED' types.
--
data Overlapped (a :: AsyncType) where
    Overlapped :: OVERLAPPED
               -> Overlapped Async

    WsaOverlapped :: WSAOVERLAPPED
                  -> Overlapped WsaAsync

deriving via (Overlapped Async) instance Storable (Overlapped Async)

deriving via (Overlapped WsaAsync) instance Storable (Overlapped WsaAsync)


--
-- IOData
--


-- | Data associated with each async call.  It is represented as the 'IODATA'
-- struct.
--
--  When an async call is initialised, e.g. 'ReadFile', the windows api takes
--  a pointer to 'OVERLAPPED' or 'WSAOVERLAPPED' in case of 'winsock2'.  When
--  it completes 'GetQueuedCompletionStatus' returns the 'OVERLAPPED' (or
--  'WSAOVERLAPPED') in the same memory location.  The windows kernel macro
--  'CONTAINING_RECORD' allows to access the 'IODATA' via its member
--  'iodOverlapped'.
--
--  The 'dequeueCompletionPackets' (which calls 'GetQueuedCompletionStatus'
--  fills the 'MVar' with the result returned by 'GetQueuedCompletionStatus'
--  (or 'GetLastError').
--
--  Outside of this module the type synonim 'IOCPData', 'AsyncIOCPData' or
--  'WsaIOCPData' should be used.
--
data IOData (asyncType :: AsyncType) =
    IOData { iodOverlapped :: Overlapped asyncType
             -- ^ overlapped structue passed through the iocp port
           , iodData       :: StablePtr (MVar (Either ErrCode Int))
             -- ^ associated stable pointer.
           }

-- | 'AsyncIOCPData' type synonim guarantess that 'dequeueCompletionPackets' is
-- using the same dataa type as 'withIOCPData' in this module, i.e. submitted
-- overlapped requests matches with the notifications pulled by
-- 'dequeueCompletionPackets'.
--
type AsyncIOCPData      = IOData Async
type WsaAsyncIOCPData   = IOData WsaAsync

--
-- 'IOData' - construction
--

-- | Smart constructor of 'IOData'.  It allocates a stable pointer to a fresh
-- 'MVar' which is also returned.
--
newIOData :: AsyncSing asyncType -> IO (IOData asyncType, MVar (Either ErrCode Int))
newIOData AsyncSing    = do
    v <- newEmptyMVar
    p <- newStablePtr v
    return ( IOData (Overlapped nullOVERLAPPED) p
           , v
           )
newIOData WsaAsyncSing = do
    v <- newEmptyMVar
    p <- newStablePtr v
    return ( IOData (WsaOverlapped nullWSAOVERLAPPED) p
           , v
           )

--
-- Working with pointers to 'IOData'
--

-- | Acces the 'OVERLAPPED' member of 'IODATA' struct, in C:
--
-- >  ioData->iodOverlapped
--
iodOverlappedPtr' :: AsyncSing asyncType
                  -> Ptr (IOData asyncType)
                  -> Ptr (Overlapped asyncType)
iodOverlappedPtr' AsyncSing    = (#ptr IODATA,    iodOverlapped)
iodOverlappedPtr' WsaAsyncSing = (#ptr WSAIODATA, iodOverlapped)

-- | This closed type family is only to make 'castOverlappedPtr' type safe.
-- We could use 'castPtr' but it would leek outside of this module.  This is
-- more elegant.
--
type family OverlappedType (asyncType :: AsyncType) :: * where
  OverlappedType Async    = OVERLAPPED
  OverlappedType WsaAsync = WSAOVERLAPPED

-- | Cast 'Overlapped asyncType' to 'OverlappedType asyncType' which is either
-- 'OVERLAPPED' or 'WSAOVERLLAPPED'.  This is safe because the underlaying data
-- is layed out in the same way.
--
castOverlappedPtr :: Ptr (Overlapped asyncType) -> Ptr (OverlappedType asyncType)
castOverlappedPtr = castPtr

-- | Access the 'lpdOverlapped' member of the 'IODATA' or 'WSAIODATA' struct and
-- cast it the the correct overlapped type: either 'OVERLAPPED' or
-- 'WSAOVERLAPPED'.
--
iodOverlappedPtr :: AsyncSing asyncType
                 -> Ptr (IOData asyncType)
                 -> Ptr (OverlappedType asyncType)
iodOverlappedPtr asyncTag = castOverlappedPtr . iodOverlappedPtr' asyncTag

-- | Access 'iodData' member of 'IODATA' or 'WSAIODATA' struct.
--
iodDataPtr :: AsyncSing asyncType
           -> Ptr (IOData asyncType)
           -> Ptr (StablePtr (MVar (f Int)))
iodDataPtr AsyncSing    = (#ptr IODATA,    iodData)
iodDataPtr WsaAsyncSing = (#ptr WSAIODATA, iodData)

--
-- Storable instances
--

instance Storable (IOData Async) where
    sizeOf    _ = (#const sizeof(IODATA))
    alignment _ = (#alignment IODATA)

    poke buf IOData {iodOverlapped = Overlapped ovl, iodData} = do
      (#poke IODATA, iodOverlapped) buf ovl
      (#poke IODATA, iodData)       buf iodData

    peek buf = do
      ovl     <- (#peek IODATA, iodOverlapped) buf
      iodData <- (#peek IODATA, iodData)       buf
      return $ IOData { iodOverlapped = Overlapped ovl
                      , iodData
                      }

instance Storable (IOData WsaAsync) where
    sizeOf    _ = (#const sizeof(WSAIODATA))
    alignment _ = (#alignment WSAIODATA)

    poke buf IOData {iodOverlapped = WsaOverlapped ovl, iodData} = do
      (#poke WSAIODATA, iodOverlapped) buf ovl
      (#poke WSAIODATA, iodData)       buf iodData

    peek buf = do
      ovl     <- (#peek WSAIODATA, iodOverlapped) buf
      iodData <- (#peek WSAIODATA, iodData)       buf
      return $ IOData { iodOverlapped = WsaOverlapped ovl
                      , iodData
                      }


--
-- Error codes.
--


-- | Error coes: either 'ErrCode' or 'WsaErrCode'.  For 'asyncType ~ WsaAsync'
-- we allow to use both.  This is because the 'dequeueCompletionPackets'
-- rightfully only reports 'ErrCode's.
--
data ErrorCode asyncType where
    ErrorCode :: forall (any :: AsyncType).
                 !ErrCode
              -> ErrorCode any

    WsaErrorCode :: !WSAErrCode
                 -> ErrorCode WsaAsync

instance Show (ErrorCode asyncType) where
    show (ErrorCode e)    = "ErrorCode " ++ show e
    show (WsaErrorCode e) = "WsaErrorCode " ++ show e


failWithErrorCode :: String -> ErrorCode asyncType -> IO a
failWithErrorCode tag (ErrorCode errorCode)       = Win32.failWith tag errorCode
failWithErrorCode tag (WsaErrorCode wsaErrorCode) = wsaFailWith tag wsaErrorCode


-- | Result of an async i/o action.  It maybe either asynchronous or
-- synchronous result or error.
--
-- One has to be really careful with passing 'True' value to 'ResultSync' or
-- 'ErrorSync' - it can reslult in a race between two threads deallocating the
-- same memory, subsequently os killing the process (often silently).
--
data Result asyncType a
    = ResultAsync !a
    -- ^ Asynchronous result, as returned by the iocp thread.
    | ErrorAsync  !(ErrorCode asyncType)
    -- ^ Asynchronous error, as returned by the iocp thread.
    | ResultSync  !a                     !Bool
    -- ^ Synchronous result; the if the 'Bool' value is true, `ioDataPtr` will
    -- be deallocated.
    | ErrorSync   !(ErrorCode asyncType) !Bool
    -- ^ Synchronous error; the if the 'Bool' value is true, `ioDataPtr` will
    -- be deallocated.


-- | File descriptors.
--
data FD asyncType where
    FDHandle :: HANDLE -> FD Async
    FDSocket :: SOCKET -> FD WsaAsync


-- | Allocate 'IOCPData' on process heap.  If the continuation does not succeed
-- and 'getLastError' is not 'ERROR_IO_PENDING' it might deallocate 'IOCPData'
-- otherwise the resposibility of deallocation is in the
-- 'dequeueCompletionPackets' thread.
--
-- It is executed with async exception masked to ensure that 'ioDataPtr' is
-- filled in with 'IOCPData' before it is passed to the continuation.
--
withIOCPData :: forall a (asyncType :: AsyncType).
                Storable (IOData asyncType)
             => String
             -- ^ error tag
             -> FD asyncType
             -- ^ file descriptor used for the syscall
             -> (Ptr (OverlappedType asyncType)
                  -> MVar (Either ErrCode Int)
                  -> IO (Result asyncType a))
             -- ^ continuation, executed with unmasked async exceptions which
             -- executes a syscall.  It receives 'LPOVERLAPPED' allocated on
             -- process heap and an empty 'MVar' which will be filled by
             -- 'dequeueCompletionPackets' ('IOManager' thread), when the
             -- async IO terminates.
             --
             -- Note that the 'MVar' holds 'ErrorCode Async', even if
             -- 'asyncType ~ WsaAsync', this is because the
             -- 'GetQueuedCompletionStatus' and thus
             -- 'dequeueCompletionPackets' can handle both `windows.h` and
             -- `winsock2.h` apis.
             --
             -> IO a
withIOCPData errorTag fd k = mask $ \unmask -> do
    ph <- Win32.getProcessHeap
    -- Using malloc will not work, we have to allocate memory on the process heap.
    ioDataPtr <-
      castPtr <$>
        Win32.heapAlloc
          ph Win32.hEAP_ZERO_MEMORY
          (fromIntegral $ sizeOf (undefined :: IOData asyncType))
    -- allocate stable pointer
    (ioData, v) <- newIOData asyncTag
    poke ioDataPtr ioData
    result <-
      (unmask $ k (iodOverlappedPtr asyncTag ioDataPtr) v)
      `catch`
         (\(e :: SomeAsyncException) -> do
            -- CancelIoEx MSDN docs:
            -- <https://docs.microsoft.com/en-us/windows/win32/fileio/cancelioex-func#remarks>
            -- 
            -- An MSDN example:
            -- <https://docs.microsoft.com/en-us/windows/win32/fileio/canceling-pending-i-o-operations#canceling-asynchronous-io>
            --
            -- When we cancel an async operations an io completion packet the
            -- io completion thread ('dequeueCompletionPackets') is notified.
            -- 'GetQueuedCompletionStatus' sets error code to
            -- 'ERROR_OPERATION_ABORTED';  thus the  'ioDataPtr' will be
            -- freed by that thread.  There's no need for us to check the
            -- status of the cancelation with 'GetOverlappedResult'
            --
            case fd of
              FDHandle handle -> do
                -- TODO: trace cancelResult and the error code
                -- we need to cast 'Ptr (Overlapped Async)' to 'Ptr OVERLAPPED'.
                void $ c_CancelIoEx handle (iodOverlappedPtr AsyncSing ioDataPtr)
                throwIO e
              FDSocket _ -> throwIO e
                -- TODO: is there a way to cancel 'WSARecv'?
         )

    case result of
      ResultAsync b ->
        return b

      ErrorAsync  errorCode ->
        failWithErrorCode ("withIODataPtr (" ++ errorTag ++ ")") errorCode

      ResultSync b deallocate -> do
        when deallocate
          $ Win32.heapFree ph 0 (castPtr ioDataPtr)
        return b

      ErrorSync errorCode deallocate -> do
        when deallocate
          $ Win32.heapFree ph 0 (castPtr ioDataPtr)
        failWithErrorCode ("withIODataPtr (" ++ errorTag ++ ")") errorCode

  where
    asyncTag :: AsyncSing asyncType
    asyncTag = case fd of
      FDHandle _ -> AsyncSing
      FDSocket _ -> WsaAsyncSing

foreign import ccall unsafe "CancelIoEx"
    c_CancelIoEx :: HANDLE
                 -> LPOVERLAPPED
                 -> IO Win32.BOOL
