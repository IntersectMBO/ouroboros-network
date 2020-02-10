module System.Win32.Async.Internal
  ( SOCKET
  , CInt (..)
  , wsaWaitForCompletion
  ) where

import Control.Concurrent

import Foreign ( StablePtr
               , newStablePtr
               )
import Foreign.C (CInt (..))
import System.Win32.Async.ErrCode


type SOCKET = CInt


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
