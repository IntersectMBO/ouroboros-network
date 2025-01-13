{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers (resolveLedgerPeers) where

-- import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Tracer (Tracer, traceWith)
import Data.List (foldl')
import Data.IP qualified as IP
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow

import Network.DNS qualified as DNS
import Network.Socket qualified as Socket
import System.Random

import Ouroboros.Network.PeerSelection.LedgerPeers.Common
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore,
           withDNSSemaphore)

-- | Provides DNS resolution functionality.
--
-- Concurrently resolve DNS names, respecting the 'maxDNSConcurrency' limit.
--
resolveLedgerPeers
  :: forall m peerAddr resolver exception.
     ( Ord peerAddr
     , MonadThrow m
     , MonadAsync m
     , Exception exception
     )
  => Tracer m TraceLedgerPeers
  -> (IP.IP -> Socket.PortNumber -> peerAddr)
  -> DNSSemaphore m
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> [RelayAccessPoint]
  -> StdGen
  -> m (Map DNS.Domain (Set peerAddr))
resolveLedgerPeers tracer
                   toPeerAddr
                   dnsSemaphore
                   resolvConf
                   DNSActions {
                      dnsResolverResource,
                      dnsLookupWithTTL
                    }
                   domains
                   rng
                   = do
    traceWith tracer (TraceLedgerPeersDomains domains)
    rr <- dnsResolverResource resolvConf
    resourceVar <- newTVarIO rr
    resolveDomains resourceVar
  where
    resolveDomains
      :: StrictTVar m (Resource m (Either (DNSorIOError exception) resolver))
      -> m (Map DNS.Domain (Set peerAddr))
    resolveDomains resourceVar = do
        rr <- readTVarIO resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (domain',) <$> withDNSSemaphore dnsSemaphore
                                     (dnsLookupWithTTL
                                       DnsLedgerPeers
                                       domain
                                       resolvConf
                                       resolver
                                       rng)
                  | domain <- domains
                  , Just domain' <-
                      case domain of
                        RelayAccessAddress {}  -> [Nothing]
                        RelayAccessDomain d _p -> [Just d]
                        RelayAccessSRVDomain d -> [Just d]
                  ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and
            -- defaults to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            return $ foldl' processResult Map.empty results

    processResult :: Map DNS.Domain (Set peerAddr)
                  -> (DNS.Domain, DNSLookupResult IP)
                  -> Map DNS.Domain (Set peerAddr)
    processResult mr (domain , ipsttls) = do
        Map.alter (addFn ipsttls) domain mr

    addFn :: DNSLookupResult IP
          -> Maybe (Set peerAddr)
          -> Maybe (Set peerAddr)
    addFn (Left _) Nothing = Just Set.empty
    addFn (Left _) addrs = addrs
    addFn (Right ipsttls) Nothing =
        let ips = map (\(ip, port, _ttl) -> (ip, port)) ipsttls
            !addrs = map (uncurry toPeerAddr)
                         ips
            !addrSet = Set.fromList addrs in
        Just addrSet
    addFn (Right ipsttls) (Just addrSet) =
        let ips = map (\(ip, port, _ttl) -> (ip, port)) ipsttls
            !addrs = map (uncurry toPeerAddr)
                         ips
            !addrSet' = Set.union addrSet (Set.fromList addrs) in
        Just addrSet'

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)
