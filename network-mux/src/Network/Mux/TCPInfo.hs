{-# LANGUAGE CPP #-}

module Network.Mux.TCPInfo
  ( StructTCPInfo (..)
  , SocketOption (TCPInfoSocketOption)
  ) where

#if os_HOST_linux
import           Network.Mux.TCPInfo.Linux
#else
data StructTCPInfo = TCPInfoUnavailable
  deriving (Eq, Ord, Show)
#endif

