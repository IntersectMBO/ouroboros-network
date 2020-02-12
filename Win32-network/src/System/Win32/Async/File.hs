{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InterruptibleFFI    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module System.Win32.Async.File
    ( -- * HANDLE API
      readHandle
    , writeHandle

      -- * NamedPipe API
    , connectNamedPipe
    ) where

import Prelude hiding (read)

import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS (unsafeUseAsCStringLen)
import Data.Functor (void)
import Foreign.Ptr ( Ptr
                   , castPtr
                   , nullPtr
                   )

import           System.Win32.Types ( HANDLE
                                    , BOOL
                                    , DWORD
                                    )
import qualified System.Win32.Types as Win32

import System.Win32.Async.ErrCode
import System.Win32.Async.IOData
import System.Win32.Async.Overlapped


-- | Read a given number of bytes from a 'HANDLE'.  The 'HANDLE' must be
-- opened with 'System.Win32.fILE_FLAG_OVERLAPPED' and associated with an IO
-- completion port via
-- 'System.Win32.Async.IOManager.associateWithIOCompletionProt'.
--
readHandle :: HANDLE
           -> Int
           -> IO ByteString
readHandle h size =
    BS.createAndTrim size $ \ptr ->
      withIOCPData "readHandle" (FDHandle h) $ \lpOverlapped waitVar -> do
        readResult <- c_ReadFile h ptr (fromIntegral size) nullPtr lpOverlapped
        errorCode <- Win32.getLastError
        if readResult || errorCode == eRROR_IO_PENDING
          then do
            iocpResult <- takeMVar waitVar
            case iocpResult of
              Right numBytes  -> return $ ResultAsync numBytes
              Left e          | e == eRROR_HANDLE_EOF
                              -> return $ ResultAsync 0
              Left errorAsync -> return $ ErrorAsync (ErrorCode errorAsync)
          else
            if errorCode == eRROR_HANDLE_EOF
              then return $ ResultSync 0 False
              else return $ ErrorSync (ErrorCode errorCode) False

foreign import ccall safe "ReadFile"
    c_ReadFile :: HANDLE
               -> Ptr a
               -- ^ lpBuffer
               -> DWORD
               -- ^ nNumberedOfBytesToRead
               -> Ptr DWORD
               -- ^ lpNumberOfBytesRead
               -> LPOVERLAPPED
               -- ^ lpOverlapped
               -> IO BOOL


-- | Write a 'ByteString' to a 'HANDLE'.  The 'HANDLE' must be opened with
-- 'System.Win32.fILE_FLAG_OVERLAPPED' and associated with an IO completion port
-- via 'System.Win32.Async.IOManager.associateWithIOCompletionPort'.
--
writeHandle :: HANDLE
            -> ByteString
            -> IO ()
writeHandle h bs =
    BS.unsafeUseAsCStringLen bs $ \(str, len) ->
      void $ withIOCPData @Int "writeHandle" (FDHandle h) $ \lpOverlapped waitVar -> do
        writeResult <- c_WriteFile h (castPtr str) (fromIntegral len) nullPtr lpOverlapped
        errorCode <- Win32.getLastError
        if writeResult || errorCode == eRROR_IO_PENDING
          then do
            iocpResult <- takeMVar waitVar
            case iocpResult of
              Right numBytes   -> return $ ResultAsync numBytes
              Left  errorAsync -> return $ ErrorAsync (ErrorCode errorAsync)
          else return $ ErrorSync (ErrorCode errorCode) False


foreign import ccall safe "WriteFile"
    c_WriteFile :: HANDLE
                -- ^ hFile
                -> Ptr a
                -- ^ lpBuffer
                -> DWORD
                -- ^ nNumberOfBytesToWrite
                -> Ptr DWORD
                -- ^ nNumberOfBytesWritten
                -> LPOVERLAPPED
                -- ^ lpOverlapped
                -> IO BOOL

-- | Connect named pipe aka accept a connection.  The 'HANDLE' must be opened
-- with 'System.Win32.FILE_FLAG_OVERLLAPPED' and associated with IO completion
-- port via 'System.Win32.Async.IOManager.associateWithIOCompletionPort'.
--
-- [msdn documentation](https://docs.microsoft.com/en-us/windows/win32/api/namedpipeapi/nf-namedpipeapi-connectnamedpipe)
--
connectNamedPipe :: HANDLE -> IO ()
connectNamedPipe h =
    void $ withIOCPData "connectNamedPipe" (FDHandle h) $ \lpOverlapped waitVar -> do
      connectResult <- c_ConnectNamedPipe h lpOverlapped
      errorCode <- Win32.getLastError
      if connectResult || errorCode == eRROR_IO_PENDING
        then do
          iocpResult <- takeMVar waitVar
          case iocpResult of
            Right _         -> return $ ResultAsync ()
            Left e          | e == eRROR_PIPE_CONNECTED
                            -> return $ ResultAsync ()
            Left errorAsync -> return $ ErrorAsync (ErrorCode errorAsync)
        else
          if | errorCode == eRROR_PIPE_CONNECTED ->
               return $ ResultSync () True
             | errorCode == eRROR_NO_DATA ->
               return $ ErrorSync (ErrorCode errorCode) True
             | otherwise ->
               return $ ErrorSync (ErrorCode errorCode) False

foreign import ccall interruptible "ConnectNamedPipe"
    c_ConnectNamedPipe :: HANDLE
                       -> LPOVERLAPPED
                       -> IO BOOL
