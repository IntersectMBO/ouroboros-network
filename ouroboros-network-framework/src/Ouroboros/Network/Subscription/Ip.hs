{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}

-- | IP subscription targets.
module Ouroboros.Network.Subscription.Ip
    ( SubscriptionParams (..)
    , IPSubscriptionParams
    , IPSubscriptionTarget (..)
    , LocalAddresses (..)
    , ipSubscriptionTargets
    , matchWithLocalAddress
    , ipRetryDelay

    --  * Traces
    , SubscriptionTrace (..)
    , ErrorPolicyTrace (..)
    , WithIPList (..)
    ) where


{- The parallel connection attemps implemented in this module is inspired by
 - RFC8305, https://tools.ietf.org/html/rfc8305 .
 -}

import           Data.Maybe (mapMaybe)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Clock (DiffTime)
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.Worker

-- | Minimum time to wait between ip reconnects
--
ipRetryDelay :: DiffTime
ipRetryDelay = 10 -- 10s delay

data IPSubscriptionTarget = IPSubscriptionTarget {
    -- | List of destinations to possibly connect to
      ispIps     :: ![Socket.SockAddr]
    -- | Number of parallel connections to keep actice.
    , ispValency :: !Word
    } deriving (Eq, Show)

-- | Pair socket addresses with corresponding local addresses to get a list of
-- connection identifiers.
--
-- Useful alongside the `ispIps` field of an `IPSubscriptionTarget`.
ipSubscriptionTargets
  :: [Socket.SockAddr]
  -> LocalAddresses
  -> Maybe (NonEmpty (ConnectionId Socket.SockAddr))
ipSubscriptionTargets addrs localAddrs = NE.nonEmpty connIds
  where
  connIds :: [ConnectionId Socket.SockAddr]
  connIds = mapMaybe (matchWithLocalAddress localAddrs) addrs

data LocalAddresses = LocalAddresses {
    -- | Local IPv4 address to use, Nothing indicates don't use IPv4
    laIpv4 :: Maybe Socket.SockAddr
    -- | Local IPv6 address to use, Nothing indicates don't use IPv6
  , laIpv6 :: Maybe Socket.SockAddr
    -- | Local Unix address to use, Nothing indicates don't use Unix sockets
  , laUnix :: Maybe Socket.SockAddr
  } deriving (Eq, Show)

-- | If there is a local address matching the socket family, give a ConnectionId
-- where the local address is the first component (bind address).
matchWithLocalAddress :: LocalAddresses -> Socket.SockAddr -> Maybe (ConnectionId Socket.SockAddr)
matchWithLocalAddress laddrs sockAddr = case sockAddr of
  Socket.SockAddrInet  _ _     -> fmap (`ConnectionId` sockAddr) (laIpv4 laddrs)
  Socket.SockAddrInet6 _ _ _ _ -> fmap (`ConnectionId` sockAddr) (laIpv6 laddrs)
  Socket.SockAddrUnix  _       -> fmap (`ConnectionId` sockAddr) (laUnix laddrs)

-- | 'ipSubscriptionWorker' and 'dnsSubscriptionWorker' parameters
--
data SubscriptionParams a target = SubscriptionParams
  { spLocalAddresses         :: LocalAddresses
    -- ^ FIXME weird that we would fix this to SockAddr bur leave the `target`
    -- type open.
  , spSubscriptionTarget     :: target
  }

type IPSubscriptionParams a = SubscriptionParams a IPSubscriptionTarget

data WithIPList a = WithIPList {
      wilSrc   :: !LocalAddresses
    , wilDsts  :: ![Socket.SockAddr]
    , wilEvent :: !a
    }

instance (Show a) => Show (WithIPList a) where
    show (WithIPList (LocalAddresses Nothing (Just ipv6) Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv6) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) Nothing Nothing) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show ipv4) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses Nothing Nothing (Just unix)) wilDsts wilEvent) =
        printf "IPs: %s %s %s" (show unix) (show wilDsts) (show wilEvent)
    show (WithIPList (LocalAddresses (Just ipv4) (Just ipv6) Nothing) wilDsts wilEvent) =
        printf  "IPs: %s %s %s %s" (show ipv4) (show ipv6)
                                   (show wilDsts) (show wilEvent)
    show WithIPList {wilSrc, wilDsts, wilEvent} =
        printf "IPs: %s %s %s" (show wilSrc) (show wilDsts) (show wilEvent)
