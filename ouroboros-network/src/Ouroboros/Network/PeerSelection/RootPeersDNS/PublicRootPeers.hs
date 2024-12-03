{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
  ( -- * DNS based provider for public root peers
    publicRootPeersProvider
  , TracePublicRootPeers (..)
  ) where

import Data.Functor ((<&>))
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import System.Random

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer (..), traceWith)

import Network.DNS (DNSError, TTL)
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket

import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore,
           withDNSSemaphore)

---------------------------------------------
-- Public root peer set provider using DNS
--

data TracePublicRootPeers =
       TracePublicRootRelayAccessPoint (Map RelayAccessPoint PeerAdvertise)
     | TracePublicRootDomains [DomainAccessPoint]
     | TracePublicRootResult  DNS.Domain [(IP, TTL)]
     | TracePublicRootResultVia DomainSRVAccessPoint DNS.Domain [(IP, TTL)]
     | TracePublicRootFailure DomainAccessPoint (Maybe DNSError)
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
publicRootPeersProvider
  :: forall peerAddr resolver exception a m.
     (MonadThrow m, MonadAsync m, Exception exception,
      Ord peerAddr)
  => Tracer m TracePublicRootPeers
  -> (IP -> Socket.PortNumber -> peerAddr)
  -> DNSSemaphore m
  -> DNS.ResolvConf
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -> DNSActions resolver exception m
  -> StdGen
  -> ((Int -> m (Map peerAddr PeerAdvertise, DiffTime)) -> m a)
  -> m a
publicRootPeersProvider tracer
                        toPeerAddr
                        dnsSemaphore
                        resolvConf
                        readServices
                        DNSActions {
                          dnsResolverResource,
                          dnsLookupWithTTL
                        }
                        rng0
                        action = do
    services <- atomically readServices
    traceWith tracer (TracePublicRootRelayAccessPoint services)
    rr <- dnsResolverResource resolvConf
    resourceVar <- newTVarIO rr
    action (requestPublicRootPeers resourceVar)
  where
    -- processResult :: (DNSLookupResult IP, PeerAdvertise)
    --               -> m ((Maybe PortNumber, PeerAdvertise), [(IP, TTL)])
    processResult (DNSLookup (dp@(DomainPlain domain port), errs, ipsttls)
                  , pa) = do
        mapM_ (traceWith tracer . TracePublicRootFailure dap . Just)
              errs
        when (not . null $ ipsttls) $
            traceWith tracer $ TracePublicRootResult domain ipsttls

        return ((Just port, pa), ipsttls)
        where
          dap = DomainAccessPoint dp

    processResult ( (DNSLookupSRV (srvDomain, errs, mResult))
                  , pa) = do
        mapM_ (traceWith tracer . TracePublicRootFailure (DomainSRVAccessPoint srvDomain)  . Just)
              errs

        case mResult of
          Nothing -> do
            traceWith tracer $ TracePublicRootFailure (DomainSRVAccessPoint srvDomain) Nothing
            return ((Nothing, pa), [])
          Just (domain, port, ipsttls) -> do
            when (not . null $ ipsttls) $
              traceWith tracer $ TracePublicRootResultVia srvDomain domain ipsttls
            return ((Just port, pa), ipsttls)

    requestPublicRootPeers
      :: StrictTVar m (Resource m (Either (DNSorIOError exception) resolver))
      -> Int
      -> m (Map peerAddr PeerAdvertise, DiffTime)
    requestPublicRootPeers resourceVar _numRequested = do
        services <- atomically readServices
        traceWith tracer (TracePublicRootRelayAccessPoint services)
        rr <- readTVarIO resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let (doms, relayAddrs) =
                  flip partition (Map.assocs services) $ \case
                    (RelayDomainAccessPoint {}, _) -> True
                    _otherwise -> False
                lookups =
                  [ (, pa)
                      <$> withDNSSemaphore dnsSemaphore
                            (dnsLookupWithTTL
                              dap
                              resolvConf
                              resolver
                              rng0)
                  | (RelayDomainAccessPoint dap, pa) <- doms]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results  <- withAsyncAll lookups (atomically . mapM waitSTM)
            results' <- mapM processResult results
            let successes = [ ( (toPeerAddr ip port, pa)
                              , ipttl)
                            | ( (Just port, pa)
                              , ipttls) <- results'
                            , (ip, ipttl) <- ipttls
                            ]
                !domainsIps = [(toPeerAddr ip port, pa)
                              | (RelayAccessAddress ip port, pa) <- relayAddrs ]
                !ips      = Map.fromList (map fst successes) `Map.union` Map.fromList domainsIps
                !ttl      = if null lookups
                               then -- Not having any peers with domains configured is not
                                    -- a DNS error.
                                    ttlForResults [60]
                               else ttlForResults (map snd successes)
            -- If all the lookups failed we'll return an empty set with a minimum
            -- TTL, and the governor will invoke its exponential backoff.
            return (ips, ttl)

-- Aux

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

-- | Policy for TTL for positive results
ttlForResults :: [DNS.TTL] -> DiffTime

-- This case says we have a successful reply but there is no answer.
-- This covers for example non-existent TLDs since there is no authority
-- to say that they should not exist.
ttlForResults []   = ttlForDnsError DNS.NameError 0
ttlForResults ttls = clipTTLBelow
                   . clipTTLAbove
                   . (fromIntegral :: Word32 -> DiffTime)
                   $ maximum ttls

-- | Limit insane TTL choices.
clipTTLAbove, clipTTLBelow :: DiffTime -> DiffTime
clipTTLBelow = max 60     -- between 1min
clipTTLAbove = min 86400  -- and 24hrs

-- | Policy for TTL for negative results
-- Cache negative response for 3hrs
-- Otherwise, use exponential backoff, up to a limit
ttlForDnsError :: DNSError -> DiffTime -> DiffTime
ttlForDnsError DNS.NameError _ = 10800
ttlForDnsError _           ttl = clipTTLAbove (ttl * 2 + 5)
