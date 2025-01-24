{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers (resolveLedgerPeers) where

import Control.Monad.Class.MonadAsync
import Control.Tracer (Tracer, traceWith)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow

import Network.DNS qualified as DNS
import System.Random

import Ouroboros.Network.PeerSelection.LedgerPeers.Common
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
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
  -> DNSSemaphore m
  -> DNS.ResolvConf
  -> DNSActions peerAddr resolver exception m
  -> LedgerPeersKind
  -> [RelayAccessPoint]
  -> StdGen
  -> m (Map DNS.Domain (Set peerAddr))
resolveLedgerPeers tracer
                   dnsSemaphore
                   resolvConf
                   DNSActions {
                      dnsResolverResource,
                      dnsLookupWithTTL
                    }
                   peerKind
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
                                       (DnsLedgerPeer peerKind)
                                       domain
                                       resolvConf
                                       resolver
                                       rng)
                  | domain <- domains
                  , domain' <-
                      case domain of
                        RelayAccessAddress {}  -> []
                        RelayAccessDomain d _p -> [d]
                        RelayAccessSRVDomain d -> [d]
                  ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and
            -- defaults to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            return $ foldl' processResult Map.empty results

    processResult :: Map DNS.Domain (Set peerAddr)
                  -> (DNS.Domain, DNSLookupResult peerAddr)
                  -> Map DNS.Domain (Set peerAddr)
    processResult mr (domain , addrttls) = do
        Map.alter (addFn addrttls) domain mr

    addFn :: DNSLookupResult peerAddr
          -> Maybe (Set peerAddr)
          -> Maybe (Set peerAddr)
    addFn (Left _) Nothing = Just Set.empty
    addFn (Left _) addrs = addrs
    addFn (Right addrttls) Nothing =
      Just $ Set.fromList . map fst $ addrttls
    addFn (Right addrttls) (Just addrSet) =
      let !addrSet' = Set.union addrSet (Set.fromList . map fst $ addrttls)
       in Just addrSet'

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)
