{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.LedgerPeers (resolveLedgerPeers) where

import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Tracer (Tracer, traceWith)
import qualified Data.IP as IP
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Foldable (foldlM)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow


import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Ouroboros.Network.PeerSelection.LedgerPeers.Common
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                     (DNSActions (..), DNSorIOError (..), Resource (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
                     (DNSSemaphore, withDNSSemaphore)

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
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (,) domain
                      <$> withDNSSemaphore dnsSemaphore
                            (dnsLookupWithTTL
                              resolvConf
                              resolver
                              (dapDomain domain))
                  | domain <- domains ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and
            -- defaults to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            foldlM processResult Map.empty results

    processResult :: Map DomainAccessPoint (Set peerAddr)
                  -> (DomainAccessPoint, ([DNS.DNSError], [(IP, DNS.TTL)]))
                  -> m (Map DomainAccessPoint (Set peerAddr))
    processResult mr (domain, (errs, ipsttls)) = do
        mapM_ (traceWith tracer . TraceLedgerPeersFailure (dapDomain domain))
              errs
        when (not $ null ipsttls) $
            traceWith tracer $ TraceLedgerPeersResult (dapDomain domain) ipsttls

        return $ Map.alter addFn domain mr
      where
        addFn :: Maybe (Set peerAddr) -> Maybe (Set peerAddr)
        addFn Nothing =
            let ips = map fst ipsttls
                !addrs = map (\ip -> toPeerAddr ip (dapPortNumber domain))
                             ips
                !addrSet = Set.fromList addrs in
            Just addrSet
        addFn (Just addrSet) =
            let ips = map fst ipsttls
                !addrs = map (\ip -> toPeerAddr ip (dapPortNumber domain))
                             ips
                !addrSet' = Set.union addrSet (Set.fromList addrs) in
            Just addrSet'

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)
