{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
  ( -- * DNS based provider for local root peers
    localRootPeersProvider
  , TraceLocalRootPeers (..)
  ) where

import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void, absurd)
import Data.Word (Word32)
import System.Random

import Control.Applicative (Alternative, (<|>))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), contramap, traceWith)

import Network.DNS qualified as DNS

import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore (DNSSemaphore,
           newDNSLocalRootSemaphore, withDNSSemaphore)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency,
           LocalRootConfig, WarmValency)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

data TraceLocalRootPeers extraFlags peerAddr =
       TraceLocalRootDomains (LocalRootPeers.Config extraFlags RelayAccessPoint)
       -- ^ 'Int' is the configured valency for the local producer groups
     | TraceLocalRootWaiting RelayAccessPoint DiffTime
     | TraceLocalRootGroups  (LocalRootPeers.Config extraFlags peerAddr)
       -- ^ This traces the results of the local root peer provider
     | TraceLocalRootDNSMap  (Map RelayAccessPoint [peerAddr])
       -- ^ This traces the results of the domain name resolution
     | TraceLocalRootReconfigured (LocalRootPeers.Config extraFlags RelayAccessPoint) -- ^ Old value
                                  (LocalRootPeers.Config extraFlags RelayAccessPoint) -- ^ New value
     | TraceLocalRootFailure RelayAccessPoint DNSorIOError
       --TODO: classify DNS errors, config error vs transitory
     | TraceLocalRootError DNS.Domain SomeException
  deriving Show

-- | Resolve 'RelayAddress'-es of local root peers using dns if needed.  Local
-- roots are provided wrapped in a 'StrictTVar', which value might change
-- (re-read form a config file).  The resolved dns names are available through
-- the output 'StrictTVar'.
--
localRootPeersProvider
  :: forall m extraFlags peerAddr resolver.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , Ord peerAddr
     , Eq extraFlags
     )
  => Tracer m (TraceLocalRootPeers extraFlags peerAddr)
  -> PeerActionsDNS peerAddr resolver m
  -> DNS.ResolvConf
  -> StdGen
  -> STM m [( HotValency
            , WarmValency
            , Map RelayAccessPoint (LocalRootConfig extraFlags))]
  -- ^ input
  -> StrictTVar m [( HotValency
                   , WarmValency
                   , Map peerAddr (LocalRootConfig extraFlags))]
  -- ^ output 'TVar'
  -> m Void
localRootPeersProvider tracer
                       PeerActionsDNS {
                         paToPeerAddr = toPeerAddr,
                         paDnsActions = DNSActions {
                           dnsAsyncResolverResource,
                           dnsLookupWithTTL
                         }
                       }
                       resolvConf
                       rng0
                       readLocalRootPeers
                       rootPeersGroupVar =
      atomically do
        domainsGroups <- readLocalRootPeers
        writeTVar rootPeersGroupVar (getLocalRootPeersGroups Map.empty domainsGroups)
        (,,) <$> newTVar rng0 <*> newDNSLocalRootSemaphore <*> pure domainsGroups
  >>= loop'
  where
    loop' (varRng, sem, domGroups) = loop varRng sem domGroups
    -- | Loop function that monitors DNS Domain resolution threads and restarts
    -- if either these threads fail or detects the local configuration changed.
    --
    loop :: StrictTVar m StdGen
         -> DNSSemaphore m
         -> [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig extraFlags))]
         -> m Void
    loop varRng dnsSemaphore domainsGroups = do
      traceWith tracer (TraceLocalRootDomains domainsGroups)
      rr <- dnsAsyncResolverResource resolvConf
      let
          -- filter domains to monitor and perform DNS resolutions on
          domains :: [RelayAccessPoint]
          domains = [ rap
                    | (_, _, m) <- domainsGroups
                    , (rap, _) <- Map.toList m
                    , case rap of
                        RelayAccessAddress {} -> False
                        _otherwise            -> True
                    ]

          -- Initial DNS Domain Map has all domains entries empty
          initialDNSDomainMap :: Map RelayAccessPoint [peerAddr]
          initialDNSDomainMap =
            Map.fromList $ map (, []) domains

      -- Create TVar to store DNS lookup results
      dnsDomainMapVar <- newTVarIO initialDNSDomainMap

      traceWith tracer (TraceLocalRootDNSMap initialDNSDomainMap)

      -- Launch DomainAddress monitoring threads and wait for threads to error
      -- or for local configuration changes.
      --
      -- Each thread receives the DNS Domain Map TVar so it can update it with
      -- its current DNS lookup result. The way we build the resulting local
      -- root groups is:
      --
      -- After that each thread resolves its domain, it is going to read the
      -- static local root peers groups and for each domain it finds, it is
      -- going to lookup into the new DNS Domain Map and replace that entry
      -- with the lookup result.
      domainsGroups' <-
        withAsyncAllWithCtx (monitorDomain rr dnsSemaphore dnsDomainMapVar varRng `map` domains) $ \as -> do
          let tagErrWithDomain (domain, _, res) = either (Left . (domain,)) absurd res
          res <- atomically $
                  -- wait until any of the monitoring threads errors
                  (tagErrWithDomain <$> waitAnyCatchSTMWithCtx as)
              <|>
                  -- wait for configuration changes
                  (do a <- readLocalRootPeers
                      -- wait until the input domains groups changes
                      check (a /= domainsGroups)
                      return (Right a))
          case res of
            Left (domain, err)    -> traceWith tracer (TraceLocalRootError domain err)
                                  -- current domain groups haven't changed, we
                                  -- can return them
                                  >> return domainsGroups
            Right domainsGroups'  -> traceWith tracer (TraceLocalRootReconfigured domainsGroups domainsGroups')
                                  -- current domain groups changed, we should
                                  -- return them
                                  >> return domainsGroups'
      -- we continue the loop outside of 'withAsyncAll',  this makes sure that
      -- all the monitoring threads are killed.
      loop varRng dnsSemaphore domainsGroups'


    -- | Function that runs on a monitoring thread. This function will, every
    -- TTL, issue a DNS resolution request and collect the results for its
    -- particular domain in the DNS Domain Map TVar. After having the result it
    -- will construct the new view of the local root groups by replacing every
    -- domain name in the static configuration with the most up to date results
    -- from the DNS Domain Map.
    monitorDomain
      :: Resource m (Either DNSorIOError resolver)
      -> DNSSemaphore m
      -> StrictTVar m (Map RelayAccessPoint [peerAddr])
      -> StrictTVar m StdGen
      -> RelayAccessPoint
      -> (DNS.Domain, m Void)
    monitorDomain rr0 dnsSemaphore dnsDomainMapVar varRng domain =
        (domain', go 0
                     (retryResource (TraceLocalRootFailure domain `contramap` tracer)
                                    (1 :| [3, 6, 9, 12])
                                    rr0))
      where
        domain' = case domain of
          RelayAccessDomain d _p -> d
          RelayAccessSRVDomain d -> d
          _otherwise             -> error "LocalRootPeers.monitorDomain: impossible!"

        go :: DiffTime
           -> Resource m resolver
           -> m Void
        go !ttl !rr = do
          when (ttl > 0) $ do
            traceWith tracer (TraceLocalRootWaiting domain ttl)
            threadDelay ttl

          (resolver, rr') <- withResource rr

          --- Resolve 'domain'
          rng <- atomically . stateTVar varRng $ split
          reply <- withDNSSemaphore dnsSemaphore
                                    (dnsLookupWithTTL
                                      DNSLocalPeer
                                      domain
                                      resolvConf
                                      resolver
                                      rng)

          case reply of
            Left errs ->
              go (minimum $ map (ttlForDnsError ttl) errs)
                 rr'
            Right results -> do
              (newRootPeersGroups, newDNSDomainMap) <- atomically $ do
                -- Read current DNS Domain Map value
                dnsDomainMap <- readTVar dnsDomainMapVar

                let results' = map fst results
                    -- New DNS Resolution results, update the map
                    newDNSDomainMap =
                      Map.insert domain results' dnsDomainMap

                -- Only overwrite if it changed:
                when (results' /= dnsDomainMap Map.! domain) $
                  writeTVar dnsDomainMapVar newDNSDomainMap

                -- Read the static local roots configuration
                staticRootPeersGroups <- readLocalRootPeers

                -- Read current root peers groups value
                oldRootPeersGroups <- readTVar rootPeersGroupVar

                -- Get possibly new value for root peers groups
                let newRootPeersGroups =
                      getLocalRootPeersGroups newDNSDomainMap
                                              staticRootPeersGroups

                -- Only overwrite if it changed:
                when (oldRootPeersGroups /= newRootPeersGroups) $
                  writeTVar rootPeersGroupVar newRootPeersGroups

                return (newRootPeersGroups, newDNSDomainMap)

              traceWith tracer (TraceLocalRootGroups newRootPeersGroups)
              traceWith tracer (TraceLocalRootDNSMap newDNSDomainMap)

              go (ttlForResults (map snd results)) rr'

    -- | Returns local root peers without any domain names, only 'peerAddr'
    -- (IP + PortNumber).
    --
    -- It does so by reading a DNS Domain Map and replacing all instances of a
    -- DomainAccessPoint in the static configuration with the values from the
    -- map.
    getLocalRootPeersGroups :: Map RelayAccessPoint [peerAddr]
                            -> [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint (LocalRootConfig extraFlags))]
                            -> [( HotValency
                                , WarmValency
                                , Map peerAddr (LocalRootConfig extraFlags))]
    getLocalRootPeersGroups dnsMap =
      -- The idea is to traverse the static configuration. Enter each local
      -- group and check if any of the RelayAccessPoint has a Domain Name.
      --
      -- If it does we make a lookup in the DNS Domain Map and get the new
      -- entries.
      --
      -- So in a nutshell we are traversing the static configuration and
      -- replacing every domain name for its resolved result (if it exists).
      fmap (second (Map.foldlWithKey'
                      (\accMap rap pa
                         -> case rap of
                             RelayAccessAddress ip port ->
                               Map.insert (toPeerAddr ip port) pa accMap
                             dap ->
                               let newEntries =
                                     maybe Map.empty (Map.fromList . fmap (, pa))
                                           $ Map.lookup dap dnsMap
                                in accMap <> newEntries
                      )
                   Map.empty
                   )
           )

-- * Aux

-- | Policy for TTL for positive results
ttlForResults :: [DNS.TTL] -> DiffTime

-- This case says we have a successful reply but there is no answer.
-- This covers for example non-existent TLDs since there is no authority
-- to say that they should not exist.
ttlForResults []   = ttlForDnsError 0 DNS.NameError
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
ttlForDnsError :: DiffTime -> DNS.DNSError -> DiffTime
ttlForDnsError _   DNS.NameError = 10800
ttlForDnsError ttl _             = clipTTLAbove (ttl * 2 + 5)

-- | `withAsyncAll`, but the actions are tagged with a context
withAsyncAllWithCtx :: MonadAsync m => [(ctx, m a)] -> ([(ctx, Async m a)] -> m b) -> m b
withAsyncAllWithCtx contextualized action = go [] contextualized
  where
    go as []            = action (reverse as)
    go as ((ctx, x):xs) = withAsync x (\a -> go ((ctx, a):as) xs)

-- | `waitAnyCatchSTM`, but the asyncs are tagged with a context
waitAnyCatchSTMWithCtx :: MonadAsync m => [(ctx, Async m a)] -> STM m (ctx, Async m a, Either SomeException a)
waitAnyCatchSTMWithCtx = foldr (orElse . waitWithCtx) retry
  where
    waitWithCtx (ctx, a) =
      do
        r <- waitCatchSTM a
        pure (ctx, a, r)

---------------------------------------------
-- Examples
--
{-
exampleLocal :: [DomainAccessPoint] -> IO ()
exampleLocal domains = do
      rootPeersVar <- newTVarIO Map.empty
      withAsync (observer rootPeersVar Map.empty) $ \_ ->
        provider rootPeersVar
  where
    provider rootPeersVar =
      localRootPeersProvider
        (showTracing stdoutTracer)
        DNS.defaultResolvConf
        rootPeersVar
        (map (\d -> (d, DoAdvertisePeer)) domains)

    observer :: (Eq a, Show a) => StrictTVar IO a -> a -> IO ()
    observer var fingerprint = do
      x <- atomically $ do
        x <- readTVar var
        check (x /= fingerprint)
        return x
      traceWith (showTracing stdoutTracer) x
      observer var x

examplePublic :: [DomainAccessPoint] -> IO ()
examplePublic domains = do
    publicRootPeersProvider
      (showTracing stdoutTracer)
      DNS.defaultResolvConf
      domains $ \requestPublicRootPeers ->
        forever $ do
          (ips, ttl) <- requestPublicRootPeers 42
          traceWith (showTracing stdoutTracer) (ips, ttl)
          threadDelay ttl
-}
