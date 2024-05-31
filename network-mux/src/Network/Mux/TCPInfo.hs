{-# LANGUAGE CPP #-}

module Network.Mux.TCPInfo
  ( StructTCPInfo (..)
#if linux_HOST_OS
  , SocketOption (TCPInfoSocketOption)
#endif
  ) where

#if linux_HOST_OS
import           Network.Mux.TCPInfo.Linux
#else
data StructTCPInfo = TCPInfoUnavailable
  deriving (Eq, Ord, Show)
#endif

