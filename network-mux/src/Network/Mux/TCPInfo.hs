{-# LANGUAGE CPP #-}

module Network.Mux.TCPInfo
  ( StructTCPInfo (..)
#if os_HOST_linux
  , SocketOption (TCPInfoSocketOption)
#endif
  ) where

#if os_HOST_linux
import Network.Mux.TCPInfo.Linux
#else
data StructTCPInfo = TCPInfoUnavailable
  deriving (Eq, Ord, Show)
#endif

