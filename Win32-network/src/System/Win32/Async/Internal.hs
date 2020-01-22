module System.Win32.Async.Internal where

import Control.Concurrent

import Foreign.StablePtr (StablePtr, newStablePtr)
import System.Win32.Types (ErrCode)
import qualified System.Win32.Types as Win32
import System.Win32.Async.ErrCode

waitForCompletion :: String
                  -> (StablePtr (MVar (Either ErrCode Int)) -> IO ())
                  -> IO Int
waitForCompletion errorTag asyncIO = do
    wait_var <- newEmptyMVar
    -- the pointer is freed in 'handleCompletions'
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
                   -> Win32.failWith ("waitForCompletion: (" ++ errorTag ++ ") " ++ show e) e
      else
        -- An error occurred.
        if   errCode == eRROR_HANDLE_EOF
          || errCode == eRROR_PIPE_CONNECTED
          then return 0
          else Win32.failWith ("waitForCompletion: (" ++ errorTag ++ ") " ++ show errCode) errCode


wsaWaitForCompletion :: String -- error tag
                     -> (StablePtr (MVar (Either WSAErrCode Int)) -> IO ())
                     -> IO Int
wsaWaitForCompletion errorTag asyncIO = do
    wait_var <- newEmptyMVar
    -- the pointer is freed in 'c_GetQueuedCompletionStatus'
    wait_var_ptr <- newStablePtr wait_var
    asyncIO wait_var_ptr
    errCode <- wsaGetLastError
    if errCode == wSA_IO_PENDING
      then do
        -- The operation is initiated asynchronously.
        -- We wait for the completion of the operation.
        res' <- takeMVar wait_var
        case res' of
            Right num_bytes
                   -> return num_bytes
            Left e -> wsaFailWith ("wsaWaitForCompletion (" ++ errorTag ++ ") "  ++ show e) e
      else
        -- An error occurred.
        wsaFailWith ("wsaWaitForCompletion (" ++ errorTag ++ ") " ++ show errCode) errCode
