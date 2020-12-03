{-# LANGUAGE NamedFieldPuns #-}

-- | Transform addresses to 'IP' addresses.
--
module Ouroboros.Network.HasIPAddress
  ( HasIPAddress (..)
  , sockAddrHasIPAddress
  , idHasIPv4Address
  , idHasIPv6Address
  ) where


import           Data.IP (IP (..))
import qualified Data.IP as IP
import           Network.Socket (SockAddr (..))


-- |  Transform an address into 'IP' address.  Although the 'IP' address is
-- kept abstract, to be able to support testing environments.
--
-- This is not captured by a class, since there would be only one instance, in
-- production code.  This gives also some additional flexibility for test
-- environments.
--
data HasIPAddress addr ip = HasIPAddress {
    getIPAddress :: addr -> ip,
    isIPv6       :: ip   -> Bool
  }


-- | Only 'SockAddr' has an 'IPAddress'; 'LocalAddress'-es are not supported.
--
sockAddrHasIPAddress :: HasIPAddress SockAddr IP
sockAddrHasIPAddress = HasIPAddress {
    getIPAddress,
    isIPv6
  }
  where
    -- we are not using 'SockAddr' for local clients, so it's ok to error.
    getIPAddress  SockAddrUnix {}         = error "sockAddrHasIPAddress: SockUnix is not supported"
    getIPAddress (SockAddrInet _ ha)      = IPv4 (IP.toIPv4w ha)
    getIPAddress (SockAddrInet6 _ _ ha _) = IPv6 (IP.toIPv6w ha)

    isIPv6 IPv6 {} = True
    isIPv6 _       = False


-- | Useful for tests.
--
idHasIPv4Address :: HasIPAddress addr addr
idHasIPv4Address = HasIPAddress {
    getIPAddress = id,
    isIPv6 = const False
  }


-- | Useful for tests.
--
idHasIPv6Address :: HasIPAddress addr addr
idHasIPv6Address = HasIPAddress {
    getIPAddress = id,
    isIPv6 = const True
  }
