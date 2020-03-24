-- | This module exposed internals of 'System.Win32.Async.IOManager' for
-- testing purposes, use 'System.IOManager' instead.
--
module System.Win32.Async.Internal
  ( createIOCompletionPort
  , closeIOCompletionPort
  , dequeueCompletionPackets
  ) where

import System.Win32.Async.IOManager
