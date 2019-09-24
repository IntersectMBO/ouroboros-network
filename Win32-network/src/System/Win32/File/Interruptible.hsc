#include <fcntl.h>
#include <windows.h>

{-# LANGUAGE CPP              #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CApiFFI          #-}
{-# LANGUAGE InterruptibleFFI #-}

module System.Win32.File.Interruptible
    ( readHandle
    , hGetLine
    , writeHandle
    , flushHandle

    -- * Low lever system calls
    , win32_ReadFile
    , win32_WriteFile
    ) where

import           Control.Monad (when, unless)
import           Data.Functor (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import qualified Data.ByteString.Char8 as BSC

import Foreign hiding (void)
import System.Win32.Types
import System.Win32.Types.Overlapped
import System.Win32.Async (eRROR_IO_PENDING)

--
-- Read from a HANDLE
--

-- | Interruptible read from a Windows 'HANDLE'.
--
readHandle :: HANDLE
           -> Int
           -> Maybe OVERLAPPED
           -> IO ByteString
readHandle h size mb_ovl
  = BS.createAndTrim size
      (\ptr ->
        fromIntegral <$>
          win32_ReadFile h ptr (fromIntegral size) mb_ovl)

win32_ReadFile :: HANDLE -> Ptr a -> DWORD -> Maybe OVERLAPPED -> IO DWORD
win32_ReadFile h buf n mb_ovl =
    alloca $ \ p_n -> do
      maybeWith with mb_ovl
        $ \ovl_ptr -> do
          res <- c_ReadFile h buf n p_n ovl_ptr
          unless res $ do
            errCode <- getLastError
            when (errCode /= eRROR_IO_PENDING)
              $ failWith "win32_ReadFile" errCode
      peek p_n

foreign import ccall interruptible "windows.h ReadFile"
  c_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool


-- | Get a single line from a 'HANDLE'.
--
hGetLine :: HANDLE
         -> IO String
hGetLine h = go ""
    where
      go :: String -> IO String
      go !s = do
        [x] <- BSC.unpack <$> readHandle h 1 Nothing
        if x == '\n'
          then pure (reverse s)
          else go (x : s)

--
-- Write to a HANDLE
--

win32_WriteFile :: HANDLE
                -> Ptr a
                -> DWORD
                -> LPOVERLAPPED
                -> IO DWORD
win32_WriteFile h buf n over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_WriteFile h buf n p_n over
  peek p_n

foreign import ccall interruptible "windows.h WriteFile"
  c_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

-- | Write a 'ByteString' to a pipe.
--
writeHandle :: HANDLE
            -> ByteString
            -> Maybe OVERLAPPED
            -> IO ()
writeHandle h bs mb_ovl = BS.unsafeUseAsCStringLen bs $
    \(str, len) ->
        maybeWith with mb_ovl
          (void . win32_WriteFile h (castPtr str) (fromIntegral len))

flushHandle :: HANDLE -> IO ()
flushHandle = failIfFalse_ "FlushFileBuffers" . c_FlushFileBuffers

foreign import ccall interruptible "windows.h FlushFileBuffers"
    c_FlushFileBuffers :: HANDLE -> IO Bool
