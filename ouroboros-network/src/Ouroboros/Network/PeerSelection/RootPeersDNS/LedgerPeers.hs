{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers (resolveLedgerPeers) where

import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Tracer (Tracer, traceWith)
import Data.IP qualified as IP
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Foldable (foldlM)
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
  -> [DomainAccessPoint]
  -> StdGen
  -> m (Map DomainAccessPoint (Set peerAddr))
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
      -> m (Map DomainAccessPoint (Set peerAddr))
    resolveDomains resourceVar = do
        rr <- readTVarIO resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ withDNSSemaphore dnsSemaphore
                                     (dnsLookupWithTTL
                                       domain
                                       resolvConf
                                       resolver
                                       rng)
                  | domain <- domains ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and
            -- defaults to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            foldlM processResult Map.empty results

    processResult :: Map DomainAccessPoint (Set peerAddr)
                  -> DNSLookupResult IP
                  -> m (Map DomainAccessPoint (Set peerAddr))
    processResult mr (DNSLookup (domain@DomainPlain { dapDomain, dapPortNumber }
                     , errs
                     , ipsttls)) = do
        mapM_ (traceWith tracer . TraceLedgerPeersFailure dapDomain . Just)
              errs
        when (not $ null ipsttls) $
            traceWith tracer $ TraceLedgerPeersResult dapDomain ipsttls

        return $ Map.alter (addFn ipsttls dapPortNumber) (DomainAccessPoint domain) mr

    processResult mr (DNSLookupSRV (domain0, errs, mResult)) = do
        let domain0' = DomainSRVAccessPoint domain0
        case mResult of
          Nothing -> do
            if null errs
              then traceWith tracer $ TraceLedgerPeersFailure (srvDomain domain0) Nothing
              else
                mapM_ (traceWith tracer . TraceLedgerPeersFailure (srvDomain domain0) . Just)
                      errs
            return $
              -- TODO is this necessary?
              Map.insertWith (\_new old -> old) domain0' Set.empty mr
          Just (dFollow@(DomainPlain dPlain port), ipsttls) -> do
            mapM_ (traceWith tracer . TraceLedgerPeersFailure dPlain . Just)
                  errs
            when (not . null $ ipsttls) $
              traceWith tracer $ TraceLedgerPeersResultVia domain0 dFollow ipsttls
            return $ Map.alter (addFn ipsttls port) domain0' mr

    addFn :: [(IP, DNS.TTL)]
          -> PortNumber
          -> Maybe (Set peerAddr)
          -> Maybe (Set peerAddr)
    addFn ipsttls port Nothing =
        let ips = map fst ipsttls
            !addrs = map (\ip -> toPeerAddr ip port)
                         ips
            !addrSet = Set.fromList addrs in
        Just addrSet
    addFn ipsttls port (Just addrSet) =
        let ips = map fst ipsttls
            !addrs = map (\ip -> toPeerAddr ip port)
                         ips
            !addrSet' = Set.union addrSet (Set.fromList addrs) in
        Just addrSet'

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)
