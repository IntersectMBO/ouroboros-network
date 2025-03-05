{-# LANGUAGE CPP #-}

-- | This module defines a priority type for trace logging.
module Cardano.KESAgent.Priority (
  Priority (..),
)
where

#if !defined(mingw32_HOST_OS)
-- On POSIX, we'll just use the 'Priority' type from the @hsyslog@ package.
import System.Posix.Syslog.Priority
#endif

#if defined(mingw32_HOST_OS)
-- On Windows, we'll define our own 'Priority' type as a drop-in replacement
-- for the syslog priority type from the @hsyslog@ package.
data Priority
  = Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Emergency
  deriving (Show, Eq, Ord, Enum, Bounded)
#endif
