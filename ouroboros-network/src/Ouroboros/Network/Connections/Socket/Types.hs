{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Connections.Socket.Types
  ( SockType (..)
  , SockAddr (..)
  , forgetSockType
  ) where

import qualified Network.Socket as Socket

data SockType where
  Inet4 :: SockType
  Inet6 :: SockType
  Unix  :: SockType

-- | Like `Network.Socket.SockAddr` but with a type parameter indicating which
-- kind of address it is: IPv4, IPv6, or Unix domain.
data SockAddr (sockType :: SockType) where
  SockAddrInet  :: !Socket.PortNumber -> !Socket.HostAddress -> SockAddr Inet4
  SockAddrInet6 :: !Socket.PortNumber
                -> !Socket.FlowInfo
                -> !Socket.HostAddress6
                -> !Socket.ScopeID
                -> SockAddr Inet6
  SockAddrUnix  :: String -> SockAddr Unix

forgetSockType :: SockAddr sockType -> Socket.SockAddr
forgetSockType sockAddr = case sockAddr of
  SockAddrInet  pn ha       -> Socket.SockAddrInet  pn ha
  SockAddrInet6 pn fi ha si -> Socket.SockAddrInet6 pn fi ha si
  SockAddrUnix  st          -> Socket.SockAddrUnix  st
