-- | This is the main module of `Win32-network` package which should be used
-- with "System.Win32.Async.Socket.ByteString",
-- "System.Win32.Async.Socket.ByteString.Lazy" or "System.Win32.Async.File" for
-- sending and/or receiving.
--
module System.Win32.Async
  ( module System.Win32.Async.File
  , module System.Win32.Async.ErrCode
  , module System.Win32.Async.Socket
  ) where

import System.Win32.Async.File
import System.Win32.Async.ErrCode
import System.Win32.Async.Socket
