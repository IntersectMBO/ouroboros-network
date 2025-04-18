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
prioColor Emergency = bold $ bright magenta
prioColor Alert = bold magenta
prioColor Critical = bold $ bright red
prioColor Error = red
prioColor Warning = bright yellow
prioColor Notice = bold defaultColor
prioColor Info = defaultColor
prioColor Debug = white
