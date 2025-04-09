{-# LANGUAGE CPP #-}

-- | This module defines a priority type for trace logging.
module Cardano.KESAgent.Priority (
  Priority (..),
  prioColor,
)
where

import Cardano.KESAgent.Util.ColoredOutput

#if !defined(mingw32_HOST_OS)
-- On POSIX, we'll just use the 'Priority' type from the @hsyslog@ package.
import System.Posix.Syslog.Priority
#endif

#if defined(mingw32_HOST_OS)
-- On Windows, we'll define our own 'Priority' type as a drop-in replacement
-- for the syslog priority type from the @hsyslog@ package.
data Priority
  = Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug
  deriving (Show, Eq, Ord, Enum, Bounded)
#endif

-- Assign colors to priority levels
prioColor :: Priority -> Color
prioColor Emergency = Color Bright Magenta
prioColor Alert = Color Dull Magenta
prioColor Critical = Color Bright Red
prioColor Error = Color Dull Red
prioColor Warning = Color Bright Yellow
prioColor Notice = Color Dull Black
prioColor Info = Color Dull Cyan
prioColor Debug = Color Dull White
