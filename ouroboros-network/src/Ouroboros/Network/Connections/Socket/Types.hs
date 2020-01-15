{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Network.Connections.Socket.Types
  ( SockType (..)
  , SockAddr (..)
  , ConnectionId (..)
  , Some (..)
  , withSockType
  , forgetSockType
  ) where

import Data.Kind (Type)
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

-- | A connection is identified by a pair of addresses. For IPv* this is fine,
-- but not for Unix domain sockets: these can be unnamed, so that a connecting
-- socket which does not bind will get the address `SockAddrUnix ""`.
--
-- How to deal with this?
-- One obvious way is to simply make up an ephemeral identifier for any
-- `SockAddrUnix ""` term, maybe using its file descriptor.
--
data ConnectionId where
  ConnectionIdIPv6 :: !(SockAddr Inet6) -> !(SockAddr Inet6) -> ConnectionId
  ConnectionIdIPv4 :: !(SockAddr Inet4) -> !(SockAddr Inet4) -> ConnectionId
  ConnectionIdUnix :: !(SockAddr Unix)  -> !(SockAddr Unix)  -> ConnectionId

instance Eq ConnectionId where
  ConnectionIdIPv6 a b == ConnectionIdIPv6 c d =
    (forgetSockType a, forgetSockType b) == (forgetSockType c, forgetSockType d)
  ConnectionIdIPv4 a b == ConnectionIdIPv4 c d =
    (forgetSockType a, forgetSockType b) == (forgetSockType c, forgetSockType d)
  ConnectionIdUnix a b == ConnectionIdUnix c d =
    (forgetSockType a, forgetSockType b) == (forgetSockType c, forgetSockType d)
  _ == _ = False

instance Ord ConnectionId where
  ConnectionIdIPv6 a b `compare` ConnectionIdIPv6 c d =
    (forgetSockType a, forgetSockType b) `compare` (forgetSockType c, forgetSockType d)
  ConnectionIdIPv4 a b `compare` ConnectionIdIPv4 c d =
    (forgetSockType a, forgetSockType b) `compare` (forgetSockType c, forgetSockType d)
  ConnectionIdUnix a b `compare` ConnectionIdUnix c d =
    (forgetSockType a, forgetSockType b) `compare` (forgetSockType c, forgetSockType d)
  -- Put IPv6 above IPv4 and Unix; IPv4 above Unix, Unix at the bottom.
  ConnectionIdIPv6 _ _ `compare` _ = GT
  ConnectionIdIPv4 _ _ `compare` _ = GT
  ConnectionIdUnix _ _ `compare` _ = LT

data Some (ty :: l -> Type) where
  Some :: ty x -> Some ty

withSockType :: Socket.SockAddr -> Some SockAddr
withSockType sockAddr = case sockAddr of
  Socket.SockAddrInet  pn ha       -> Some (SockAddrInet  pn ha)
  Socket.SockAddrInet6 pn fi ha si -> Some (SockAddrInet6 pn fi ha si)
  Socket.SockAddrUnix  st          -> Some (SockAddrUnix  st)

forgetSockType :: SockAddr sockType -> Socket.SockAddr
forgetSockType sockAddr = case sockAddr of
  SockAddrInet  pn ha       -> Socket.SockAddrInet  pn ha
  SockAddrInet6 pn fi ha si -> Socket.SockAddrInet6 pn fi ha si
  SockAddrUnix  st          -> Socket.SockAddrUnix  st
