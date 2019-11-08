{-# LANGUAGE CApiFFI          #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE MultiWayIf       #-}

module System.Win32.Event
  ( LPSECURITY_ATTRIBUTES
  , createEvent
  , setEvent
  , resetEvent
  , WaitForSingleObjectStatus (..)
  , waitForSingleObject
  , interruptibleWaitForSingleObject
  ) where

import Control.Concurrent
import Control.Exception
import Foreign.C.String (withCAString)

import System.Win32.File (LPSECURITY_ATTRIBUTES)
import System.Win32.Types

#include <fcntl.h>
#include <windows.h>


{-
data SECURITY_ATTRIBUTES = SECURITY_ATTRIBUTES
  { saLength             :: DWORD
  , saSecurityDescriptor :: LPVOID
  , saInheritHandle      :: BOOL
  }

instance Storable SECURITY_ATTRIBUTES where
    sizeOf = const (#size SECURITY_ATTRIBUTES)
    alignment _ = #alignment SECURITY_ATTRIBUTES
    poke buf bhi = do
        (#poke SECURITY_ATTRIBUTES, nLength)              buf (saLength bhi)
        (#poke SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf (saSecurityDescriptor bhi)
        (#poke SECURITY_ATTRIBUTES, bInheritHandle)       buf (saInheritHandle bhi)
    peek buf = do
        len <- (#peek SECURITY_ATTRIBUTES, nLength)              buf
        sec <- (#peek SECURITY_ATTRIBUTES, lpSecurityDescriptor) buf
        inh <- (#peek SECURITY_ATTRIBUTES, bInheritHandle)       buf
        return $ SECURITY_ATTRIBUTES len sec inh
-}

createEvent :: Maybe LPSECURITY_ATTRIBUTES
            -> BOOL   -- ^ bManualReset
            -> BOOL   -- ^ bInitialState
            -> String -- ^ lpName
            -> IO HANDLE
createEvent mb_attr bManualReset bInitialState lpName =
  withCAString lpName $ \c_lpName -> 
    c_CreateEvent (maybePtr mb_attr) bManualReset bInitialState c_lpName

foreign import ccall unsafe "windows.h CreateEventA"
  c_CreateEvent :: LPSECURITY_ATTRIBUTES -- ^ lpEventAttributes
                -> BOOL                  -- ^ bManualReset
                -> BOOL                  -- ^ bInitialState
                -> LPCSTR                -- ^ lpName
                -> IO HANDLE

setEvent :: HANDLE -> IO ()
setEvent = failIfFalse_ "SetEvent" . c_SetEvent

foreign import ccall unsafe "windows.h SetEvent"
  c_SetEvent :: HANDLE -> IO BOOL

resetEvent :: HANDLE -> IO ()
resetEvent = failIfFalse_ "ResetEvent" . c_ResetEvent

foreign import ccall unsafe "windows.h ResetEvent"
  c_ResetEvent :: HANDLE -> IO BOOL

type WAIT_FOR_SINGLE_OBJECT_STATUS = UINT

#{enum WAIT_FOR_SINGLE_OBJECT_STATUS,
 , wAIT_ABANDONED = WAIT_ABANDONED
 , wAIT_OBJECT_0  = WAIT_OBJECT_0
 , wAIT_TIMEOUT   = WAIT_TIMEOUT
 , wAIT_FAILED    = WAIT_FAILED
 }

foreign import ccall interruptible "windows.h WaitForSingleObject"
  c_WaitForSingleObject :: HANDLE
                        -> DWORD -- ^ dwMilliseconds
                        -> IO WAIT_FOR_SINGLE_OBJECT_STATUS

data WaitForSingleObjectStatus
    = WaitAbandoned
    | WaitObject0
    | WaitTimeout

-- | 'waitForSingleObject' is not interruptible, use
-- 'interruptibleWaitForSingleObject' if this is desired.
--
waitForSingleObject :: HANDLE -> DWORD -> IO WaitForSingleObjectStatus
waitForSingleObject h dwMilliseconds = do
    res <- failIf ((==) wAIT_FAILED)
                  "WaitForSingleObject"
                  (c_WaitForSingleObject h dwMilliseconds)
    if | res == wAIT_ABANDONED -> pure WaitAbandoned
       | res == wAIT_OBJECT_0  -> pure WaitObject0
       | res == wAIT_TIMEOUT   -> pure WaitTimeout
       | otherwise             -> error $ "WaitForSingleObject: not recognised return code"
                                        ++ show res

interruptibleWaitForSingleObject
    :: HANDLE -> DWORD -> IO WaitForSingleObjectStatus
interruptibleWaitForSingleObject h dwMilliseconds =
    do
      v <- newEmptyMVar
      -- child thred which blocks on the event, if the parent thread receives
      -- an exception, unblock the thread.
      _ <- forkFinally
            (waitForSingleObject h dwMilliseconds)
            (putMVar v)
      res <- takeMVar v
      case res of
        Left e  -> throwIO e
        Right r -> pure r
  `finally` do
    -- `setEvent` is idempotent, in case of an (async) exception this will
    -- cancel the forked thread
    setEvent h
        
