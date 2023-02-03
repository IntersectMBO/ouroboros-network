{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS
  ( -- * DNS based actions for local and public root providers
    DNSActions (..)
    -- * DNS resolver IO auxiliary functions
  , constantResource
    -- ** DNSActions IO
  , ioDNSActions
  , LookupReqs (..)
    -- * DNS semaphore
  , DNSSemaphore
  , newLocalAndPublicRootDNSSemaphore
    -- * DNS based provider for local root peers
  , localRootPeersProvider
  , DomainAccessPoint (..)
  , RelayAccessPoint (..)
  , IP.IP (..)
  , TraceLocalRootPeers (..)
    -- * DNS based provider for public root peers
  , publicRootPeersProvider
  , TracePublicRootPeers (..)
    -- DNS lookup support
  , resolveDomainAccessPoint
    -- * DNS type re-exports
  , DNS.ResolvConf
  , DNS.Domain
  , DNS.TTL
    -- * Socket type re-exports
  , Socket.PortNumber
  ) where

import           Data.Foldable (foldlM)
import           Data.List (elemIndex)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void, absurd)
import           Data.Word (Word32)

import           Control.Applicative (Alternative, (<|>))
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Concurrent.Class.MonadSTM.TSem
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer (..), contramap, traceWith)


import qualified Data.IP as IP
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Data.Bifunctor (second)
import           Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                     (DNSActions (..), DNSorIOError (..), LookupReqs (..),
                     Resource (..), constantResource, ioDNSActions,
                     withResource')
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers
                     (HotValency, WarmValency)

-----------------------------------------------
-- local root peer set provider based on DNS
--

data TraceLocalRootPeers peerAddr exception =
       TraceLocalRootDomains [( HotValency
                              , WarmValency
                              , Map RelayAccessPoint PeerAdvertise)]
       -- ^ 'Int' is the configured valency for the local producer groups
     | TraceLocalRootWaiting DomainAccessPoint DiffTime
     | TraceLocalRootResult  DomainAccessPoint [(IP, DNS.TTL)]
     | TraceLocalRootGroups  [( HotValency
                              , WarmValency
                              , Map peerAddr PeerAdvertise)]
       -- ^ This traces the results of the local root peer provider
     | TraceLocalRootDNSMap  (Map DomainAccessPoint [peerAddr])
       -- ^ This traces the results of the domain name resolution
     | TraceLocalRootReconfigured [( HotValency
                                   , WarmValency
                                   , Map RelayAccessPoint PeerAdvertise)] -- ^ Old value
                                  [( HotValency
                                   , WarmValency
                                   , Map RelayAccessPoint PeerAdvertise)] -- ^ New value
     | TraceLocalRootFailure DomainAccessPoint (DNSorIOError exception)
       --TODO: classify DNS errors, config error vs transitory
     | TraceLocalRootError   DomainAccessPoint SomeException
  deriving Show


-- | Maximal concurrency when resolving DNS names of root and ledger peers.
--
maxDNSConcurrency :: Integer
maxDNSConcurrency = 8

-- | Maximal concurrency when resolving DNS names of local root peers.
--
maxDNSLocalRootConcurrency :: Integer
maxDNSLocalRootConcurrency = 2

-- | A semaphore used to limit concurrency of dns names resolution.
--
newtype DNSSemaphore m = DNSSemaphore (TSem m)

-- | Create a `DNSSemaphore` for root and ledger peers.
--
newLocalAndPublicRootDNSSemaphore :: MonadSTM m => m (DNSSemaphore m)
newLocalAndPublicRootDNSSemaphore = DNSSemaphore <$> atomically (newTSem maxDNSConcurrency)

-- | Create a `DNSSemaphore` for local root peers.
--
newDNSLocalRootSemaphore :: MonadSTM m => STM m (DNSSemaphore m)
newDNSLocalRootSemaphore = DNSSemaphore <$> newTSem maxDNSLocalRootConcurrency

withDNSSemaphore :: (MonadSTM m, MonadThrow m) => DNSSemaphore m -> m a -> m a
withDNSSemaphore (DNSSemaphore s) =
    bracket_ (atomically $ waitTSem s)
             (atomically $ signalTSem s)

-- | Resolve 'RelayAddress'-es of local root peers using dns if needed.  Local
-- roots are provided wrapped in a 'StrictTVar', which value might change
-- (re-read form a config file).  The resolved dns names are available through
-- the output 'StrictTVar'.
--
localRootPeersProvider
  :: forall m peerAddr resolver exception.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , Eq (Async m Void)
     , Ord peerAddr
     )
  => Tracer m (TraceLocalRootPeers peerAddr exception)
  -> (IP -> Socket.PortNumber -> peerAddr)
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> STM m [( HotValency
            , WarmValency
            , Map RelayAccessPoint PeerAdvertise)]
  -- ^ input
  -> StrictTVar m [( HotValency
                   , WarmValency
                   , Map peerAddr PeerAdvertise)]
  -- ^ output 'TVar'
  -> m Void
localRootPeersProvider tracer
                       toPeerAddr
                       resolvConf
                       DNSActions {
                         dnsAsyncResolverResource,
                         dnsLookupWithTTL
                       }
                       readLocalRootPeers
                       rootPeersGroupVar =
        atomically (do domainsGroups <- readLocalRootPeers
                       writeTVar rootPeersGroupVar (getLocalRootPeersGroups Map.empty domainsGroups)
                       dnsSemaphore <- newDNSLocalRootSemaphore
                       return (dnsSemaphore, domainsGroups))
    >>= uncurry loop
  where
    -- | Loop function that monitors DNS Domain resolution threads and restarts
    -- if either these threads fail or detects the local configuration changed.
    --
    loop :: DNSSemaphore m -> [(HotValency, WarmValency, Map RelayAccessPoint PeerAdvertise)] -> m Void
    loop dnsSemaphore domainsGroups = do
      traceWith tracer (TraceLocalRootDomains domainsGroups)
      rr <- dnsAsyncResolverResource resolvConf
      let
          -- Get only DomainAccessPoint to monitor and perform DNS resolution
          -- on them.
          domains :: [DomainAccessPoint]
          domains = [ domain
                    | (_, _, m) <- domainsGroups
                    , (RelayDomainAccessPoint domain, _) <- Map.toList m ]

          -- Initial DNS Domain Map has all domains entries empty
          initialDNSDomainMap :: Map DomainAccessPoint [peerAddr]
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
        withAsyncAll (monitorDomain rr dnsSemaphore dnsDomainMapVar `map` domains) $ \as -> do
          res <- atomically $
                  -- wait until any of the monitoring threads errors
                  ((\(a, res) ->
                      let domain :: DomainAccessPoint
                          domain = case a `elemIndex` as of
                            Nothing  -> error "localRootPeersProvider: impossible happened"
                            Just idx -> case domains !! idx of x -> x
                      in either (Left . (domain,)) absurd res)
                    -- the monitoring thread cannot return, it can only error
                    <$> waitAnyCatchSTM as)
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
      loop dnsSemaphore domainsGroups'

    resolveDomain
      :: resolver
      -> DomainAccessPoint
      -> m (Either [DNS.DNSError] [(peerAddr, DNS.TTL)])
    resolveDomain resolver
                  domain@DomainAccessPoint {dapDomain, dapPortNumber} = do
      (errs, results) <- dnsLookupWithTTL resolvConf resolver dapDomain
      mapM_ (traceWith tracer . TraceLocalRootFailure domain . DNSError)
            errs

      if null errs
         then do
           traceWith tracer (TraceLocalRootResult domain results)
           return $ Right [ ( toPeerAddr addr dapPortNumber
                            , _ttl)
                          | (addr, _ttl) <- results ]
         else return $ Left errs

    -- | Function that runs on a monitoring thread. This function will, every
    -- TTL, issue a DNS resolution request and collect the results for its
    -- particular domain in the DNS Domain Map TVar. After having the result it
    -- will construct the new view of the local root groups by replacing every
    -- domain name in the static configuration with the most up to date results
    -- from the DNS Domain Map.
    monitorDomain
      :: Resource m (DNSorIOError exception) resolver
      -> DNSSemaphore m
      -> StrictTVar m (Map DomainAccessPoint [peerAddr])
      -> DomainAccessPoint
      -> m Void
    monitorDomain rr0 dnsSemaphore dnsDomainMapVar domain =
        go rr0 0
      where
        go :: Resource m (DNSorIOError exception) resolver
           -> DiffTime
           -> m Void
        go !rr !ttl = do
          when (ttl > 0) $ do
            traceWith tracer (TraceLocalRootWaiting domain ttl)
            threadDelay ttl

          (resolver, rrNext) <-
            withResource' (TraceLocalRootFailure domain `contramap` tracer)
                          (1 :| [3, 6, 9, 12])
                          rr

          --- Resolve 'domain'
          reply <- withDNSSemaphore dnsSemaphore (resolveDomain resolver domain)
          case reply of
            Left errs -> go rrNext
                           (minimum $ map (\err -> ttlForDnsError err ttl) errs)
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

              go rrNext (ttlForResults (map snd results))

    -- | Returns local root peers without any domain names, only 'peerAddr'
    -- (IP + PortNumber).
    --
    -- It does so by reading a DNS Domain Map and replacing all instances of a
    -- DomainAccessPoint in the static configuration with the values from the
    -- map.
    getLocalRootPeersGroups :: Map DomainAccessPoint [peerAddr]
                            -> [( HotValency
                                , WarmValency
                                , Map RelayAccessPoint PeerAdvertise)]
                            -> [( HotValency
                                , WarmValency
                                , Map peerAddr PeerAdvertise)]
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
                             RelayDomainAccessPoint dap ->
                               let newEntries = maybe Map.empty
                                                      Map.fromList
                                              $ fmap (, pa)
                                              <$> Map.lookup dap dnsMap
                                in accMap <> newEntries
                      )
                   Map.empty
                   )
           )

---------------------------------------------
-- Public root peer set provider using DNS
--

data TracePublicRootPeers =
       TracePublicRootRelayAccessPoint (Map RelayAccessPoint PeerAdvertise)
     | TracePublicRootDomains [DomainAccessPoint]
     | TracePublicRootResult  DNS.Domain [(IP, DNS.TTL)]
     | TracePublicRootFailure DNS.Domain DNS.DNSError
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
  -> ((Int -> m (Map peerAddr PeerAdvertise, DiffTime)) -> m a)
  -> m a
publicRootPeersProvider tracer
                        toPeerAddr
                        dnsSemaphore
                        resolvConf
                        readDomains
                        DNSActions {
                          dnsResolverResource,
                          dnsLookupWithTTL
                        }
                        action = do
    domains <- atomically readDomains
    traceWith tracer (TracePublicRootRelayAccessPoint domains)
    rr <- dnsResolverResource resolvConf
    resourceVar <- newTVarIO rr
    action (requestPublicRootPeers resourceVar)
  where
    processResult :: ((DomainAccessPoint, PeerAdvertise), ([DNS.DNSError], [(IP, DNS.TTL)]))
                  -> m ((DomainAccessPoint, PeerAdvertise), [(IP, DNS.TTL)])
    processResult ((domain, pa), (errs, result)) = do
        mapM_ (traceWith tracer . TracePublicRootFailure (dapDomain domain))
              errs
        when (not $ null result) $
            traceWith tracer $ TracePublicRootResult (dapDomain domain) result

        return ((domain, pa), result)

    requestPublicRootPeers
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> Int
      -> m (Map peerAddr PeerAdvertise, DiffTime)
    requestPublicRootPeers resourceVar _numRequested = do
        domains <- atomically readDomains
        traceWith tracer (TracePublicRootRelayAccessPoint domains)
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ ((DomainAccessPoint domain port, pa),)
                      <$> withDNSSemaphore dnsSemaphore
                            (dnsLookupWithTTL
                              resolvConf
                              resolver
                              domain)
                  | (RelayAccessDomain domain port, pa) <- Map.assocs domains ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            results' <- mapM processResult results
            let successes = [ ( (toPeerAddr ip dapPortNumber, pa)
                              , ipttl)
                            | ( (DomainAccessPoint {dapPortNumber}, pa)
                              , ipttls) <- results'
                            , (ip, ipttl) <- ipttls
                            ]
                !domainsIps = [(toPeerAddr ip port, pa)
                              | (RelayAccessAddress ip port, pa) <- Map.assocs domains ]
                !ips      = Map.fromList (map fst successes) `Map.union` Map.fromList domainsIps
                !ttl      = ttlForResults (map snd successes)
            -- If all the lookups failed we'll return an empty set with a minimum
            -- TTL, and the governor will invoke its exponential backoff.
            return (ips, ttl)

-- | Provides DNS resolution functionality.
--
-- Concurrently resolve DNS names, respecting the 'maxDNSConcurrency' limit.
--
resolveDomainAccessPoint
  :: forall exception resolver m.
     (MonadThrow m, MonadAsync m, Exception exception)
  => Tracer m TracePublicRootPeers
  -> DNSSemaphore m
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> [DomainAccessPoint]
  -> m (Map DomainAccessPoint (Set Socket.SockAddr))
resolveDomainAccessPoint tracer
                         dnsSemaphore
                         resolvConf
                         DNSActions {
                            dnsResolverResource,
                            dnsLookupWithTTL
                          }
                         domains
                         = do
    traceWith tracer (TracePublicRootDomains domains)
    rr <- dnsResolverResource resolvConf
    resourceVar <- newTVarIO rr
    resolveDomains resourceVar
  where
    resolveDomains
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> m (Map DomainAccessPoint (Set Socket.SockAddr))
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
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            foldlM processResult Map.empty results

    processResult :: Map DomainAccessPoint (Set Socket.SockAddr)
                  -> (DomainAccessPoint, ([DNS.DNSError], [(IP, DNS.TTL)]))
                  -> m (Map DomainAccessPoint (Set Socket.SockAddr))
    processResult mr (domain, (errs, ipsttls)) = do
        mapM_ (traceWith tracer . TracePublicRootFailure (dapDomain domain))
              errs
        when (not $ null ipsttls) $
            traceWith tracer $ TracePublicRootResult (dapDomain domain) ipsttls

        return $ Map.alter addFn domain mr
      where
        addFn :: Maybe (Set Socket.SockAddr) -> Maybe (Set Socket.SockAddr)
        addFn Nothing =
            let ips = map fst ipsttls
                !addrs = map (\ip -> IP.toSockAddr (ip, dapPortNumber domain))
                             ips
                !addrSet = Set.fromList addrs in
            Just addrSet
        addFn (Just addrSet) =
            let ips = map fst ipsttls
                !addrs = map (\ip -> IP.toSockAddr (ip, dapPortNumber domain))
                             ips
                !addrSet' = Set.union addrSet (Set.fromList addrs) in
            Just addrSet'


---------------------------------------------
-- Shared utils
--

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

-- | Policy for TTL for negative results
-- Cache negative response for 3hrs
-- Otherwise, use exponential backoff, up to a limit
ttlForDnsError :: DNS.DNSError -> DiffTime -> DiffTime
ttlForDnsError DNS.NameError _ = 10800
ttlForDnsError _           ttl = clipTTLAbove (ttl * 2 + 5)

-- | Limit insane TTL choices.
clipTTLAbove, clipTTLBelow :: DiffTime -> DiffTime
clipTTLBelow = max 60     -- between 1min
clipTTLAbove = min 86400  -- and 24hrs

withAsyncAll :: MonadAsync m => [m a] -> ([Async m a] -> m b) -> m b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action (reverse as)
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

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
