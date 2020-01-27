{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{- Partial implementation of RFC8305, https://tools.ietf.org/html/rfc8305 .
 - Prioritization of destination addresses doesn't implement longest prefix matching
 - and doesn't take address scope etc. into account.
 -}

module Ouroboros.Network.Subscription.Dns
    ( DnsSubscriptionTarget (..)
    , DnsSubscriptionParams

    , Resolver (..)
    , mkResolverIO
    , dnsResolve
    , resolutionDelay

      -- * Traces
    , SubscriptionTrace (..)
    , DnsTrace (..)
    , ErrorPolicyTrace (..)
    , WithDomainName (..)
    , WithAddr (..)
    ) where

import           Control.Exception (IOException)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import qualified Data.IP as IP
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.Ip


-- | Time to wait for an AAAA response after receiving an A response.
resolutionDelay :: DiffTime
resolutionDelay = 0.05 -- 50ms delay

-- | Description of one DNS subscription target: a name to resolve and the
-- number of addresses in that name to subscribe to.
data DnsSubscriptionTarget = DnsSubscriptionTarget {
      dstDomain  :: !DNS.Domain
    , dstPort    :: !Socket.PortNumber
    , dstValency :: !Word
    } deriving (Eq, Show)


data Resolver m = Resolver {
      lookupA    :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    , lookupAAAA :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    }

-- | Resolve a name. AAAA and A are done concurrently and waited on, then
-- the results concatenated, with AAAA going before A.
dnsResolve
  :: forall m .
     ( MonadAsync m
     , MonadSTM   m
     , MonadCatch m
     , MonadThrow m
     )
  => Tracer m DnsTrace
  -> Resolver m
  -> DNS.Domain
  -> m [Socket.SockAddr]
dnsResolve tracer resolver domain = do
  (resultsAAAA, resultsA) <- concurrently
    (resolveAAAA `catch` handleException)
    (resolveA `catch` handleException)
  pure (alternate resultsAAAA resultsA)

  where

  alternate :: [a] -> [a] -> [a]
  alternate [] ys = ys
  alternate xs [] = xs
  alternate (x:xs) (y:ys) = x : y : alternate xs ys

  resolveAAAA :: m [Socket.SockAddr]
  resolveAAAA = do
    r_e <- lookupAAAA resolver domain
    case r_e of
      Left e  -> do
        traceWith tracer $ DnsTraceLookupAAAAError e
        pure []
      Right r -> do
        traceWith tracer $ DnsTraceLookupAAAAResult r
        pure r

  resolveA :: m [Socket.SockAddr]
  resolveA = do
    r_e <- lookupA resolver domain
    case r_e of
      Left e -> do
        traceWith tracer $ DnsTraceLookupAError e
        pure []
      Right r -> do
        traceWith tracer $ DnsTraceLookupAResult r
        pure r

  handleException :: IOException -> m [a]
  handleException _ = pure []

type DnsSubscriptionParams a = SubscriptionParams a DnsSubscriptionTarget

mkResolverIO :: Socket.PortNumber -> (Resolver IO -> IO t) -> IO t
mkResolverIO port k = do
  rs <- DNS.makeResolvSeed DNS.defaultResolvConf
  DNS.withResolver rs $ \dnsResolver -> k $ Resolver
    (ipv4ToSockAddr dnsResolver)
    (ipv6ToSockAddr dnsResolver)

  where
  ipv4ToSockAddr dnsResolver d = do
    r <- DNS.lookupA dnsResolver d
    case r of
      Right ips -> return $ Right $ map
        (Socket.SockAddrInet (fromIntegral port) . IP.toHostAddress)
        ips
      Left e    -> return $ Left e

  ipv6ToSockAddr dnsResolver d = do
    r <- DNS.lookupAAAA dnsResolver d
    case r of
      Right ips -> return $ Right $ map
        (\ip -> Socket.SockAddrInet6 (fromIntegral port) 0 (IP.toHostAddress6 ip) 0)
        ips
      Left e    -> return $ Left e

data WithDomainName a = WithDomainName {
      wdnDomain :: !DNS.Domain
    , wdnEvent  :: !a
    }

instance Show a => Show (WithDomainName a) where
    show WithDomainName {wdnDomain, wdnEvent} = printf  "Domain: %s %s" (show wdnDomain) (show wdnEvent)

data DnsTrace =
      DnsTraceLookupException SomeException
    | DnsTraceLookupAError DNS.DNSError
    | DnsTraceLookupAAAAError DNS.DNSError
    | DnsTraceLookupIPv6First
    | DnsTraceLookupIPv4First
    | DnsTraceLookupAResult [Socket.SockAddr]
    | DnsTraceLookupAAAAResult [Socket.SockAddr]

instance Show DnsTrace where
    show (DnsTraceLookupException e)   = "lookup exception " ++ show e
    show (DnsTraceLookupAError e)      = "A lookup failed with " ++ show e
    show (DnsTraceLookupAAAAError e)   = "AAAA lookup failed with " ++ show e
    show DnsTraceLookupIPv4First       = "Returning IPv4 address first"
    show DnsTraceLookupIPv6First       = "Returning IPv6 address first"
    show (DnsTraceLookupAResult as)    = "Lookup A result: " ++ show as
    show (DnsTraceLookupAAAAResult as) = "Lookup AAAAA result: " ++ show as
