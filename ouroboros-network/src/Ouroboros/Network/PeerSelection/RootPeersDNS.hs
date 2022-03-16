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
    -- * DNS resolver IO auxiliar functions
  , constantResource
    -- ** DNSActions IO
  , ioDNSActions
  , LookupReqs (..)
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
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void, absurd)
import           Data.Word (Word32)

import           Control.Applicative ((<|>))
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer (..), contramap, traceWith)


import qualified Data.IP as IP
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Ouroboros.Network.PeerSelection.RelayAccessPoint
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                     (DNSActions (..), DNSorIOError (..), LookupReqs (..),
                     Resource (..), constantResource, ioDNSActions,
                     withResource')
import           Ouroboros.Network.PeerSelection.Types

-----------------------------------------------
-- local root peer set provider based on DNS
--

data TraceLocalRootPeers peerAddr exception =
       TraceLocalRootDomains [(Int, Map RelayAccessPoint PeerAdvertise)]
       -- ^ 'Int' is the configured valency for the local producer groups
     | TraceLocalRootWaiting DomainAccessPoint DiffTime
     | TraceLocalRootResult  DomainAccessPoint [(IP, DNS.TTL)]
     | TraceLocalRootGroups  (Seq (Int, Map peerAddr PeerAdvertise))
       -- ^ This traces the results of the local root peer provider
     | TraceLocalRootFailure DomainAccessPoint (DNSorIOError exception)
       --TODO: classify DNS errors, config error vs transitory
     | TraceLocalRootError   DomainAccessPoint SomeException
  deriving Show

-- | Resolve 'RelayAddress'-es of local root peers using dns if needed.  Local
-- roots are provided wrapped in a 'StrictTVar', which value might change
-- (re-read form a config file).  The resolved dns names are available through
-- the output 'StrictTVar'.
--
localRootPeersProvider
  :: forall m peerAddr resolver exception.
     ( MonadAsync m
     , MonadDelay m
     , Eq (Async m Void)
     , Ord peerAddr
     )
  => Tracer m (TraceLocalRootPeers peerAddr exception)
  -> (IP -> Socket.PortNumber -> peerAddr)
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
  -- ^ input
  -> StrictTVar m (Seq (Int, Map peerAddr PeerAdvertise))
  -- ^ output 'TVar'
  -> m Void
localRootPeersProvider tracer
                       toPeerAddr
                       resolvConf
                       DNSActions {
                         dnsAsyncResolverResource,
                         dnsLookupWithTTL
                       }
                       readDomainsGroups
                       rootPeersGroupsVar =
    atomically readDomainsGroups >>= loop
  where
    loop domainsGroups = do
      traceWith tracer (TraceLocalRootDomains domainsGroups)
      rr <- dnsAsyncResolverResource resolvConf
      let
          -- Flatten the local root peers groups and associate its index to
          -- each DomainAddress to be monitorized.
          -- NOTE: We need to pair the index because the resulting list can be
          -- sparse.
          domains :: [(Int, DomainAccessPoint, PeerAdvertise)]
          domains = [ (index, domain, pa)
                    | (index, (_, m)) <- zip [0..] domainsGroups
                    , (RelayDomainAccessPoint domain, pa) <- Map.toList m ]
          -- Since we want to preserve the number of groups, the targets, and
          -- the addresses within each group, we fill the TVar with
          -- a placeholder list, in order for each monitored DomainAddress to
          -- be updated in the correct group.
          --
          -- This is the static configuration.
          rootPeersGroups :: Seq (Int, Map peerAddr PeerAdvertise)
          rootPeersGroups = Seq.fromList $ map (\(target, m) -> (target, f m)) domainsGroups
            where
               f :: Map RelayAccessPoint PeerAdvertise
                 -> Map peerAddr PeerAdvertise
               f = Map.mapKeys
                     (\k -> case k of
                       RelayAccessAddress ip port ->
                         toPeerAddr ip port
                       _ ->
                         error "localRootPeersProvider: impossible happened"
                     )
                 . Map.filterWithKey
                     (\k _ -> case k of
                       RelayAccessAddress {} -> True
                       RelayAccessDomain {}  -> False
                     )
      atomically $
        writeTVar rootPeersGroupsVar rootPeersGroups
      traceWith tracer (TraceLocalRootGroups rootPeersGroups)

      -- Launch DomainAddress monitoring threads and wait for threads to error
      -- or for local configuration changes.
      domainsGroups' <-
        withAsyncAll (monitorDomain rr rootPeersGroups `map` domains) $ \as -> do
          res <- atomically $
                  -- wait until any of the monitoring threads errors
                  ((\(a, res) ->
                      let domain :: DomainAccessPoint
                          domain = case a `elemIndex` as of
                            Nothing  -> error "localRootPeersProvider: impossible happened"
                            Just idx -> case (domains !! idx) of (_, x, _) -> x
                      in either (Left . (domain,)) absurd res)
                    -- the monitoring thread cannot return, it can only error
                    <$> waitAnyCatchSTM as)
              <|>
                  -- wait for configuraiton changes
                  (do a <- readDomainsGroups
                      -- wait until the input domains groups changes
                      check (a /= domainsGroups)
                      return (Right a))
          case res of
            Left (domain, err)    -> traceWith tracer (TraceLocalRootError domain err)
                                  -- current domain groups haven't changed, we
                                  -- can return them
                                  >> return domainsGroups
            Right domainsGroups'  -> return domainsGroups'
      -- we continue the loop outside of 'withAsyncAll',  this makes sure that
      -- all the monitoring threads are killed.
      loop domainsGroups'


    resolveDomain
      :: resolver
      -> DomainAccessPoint
      -> PeerAdvertise
      -> m (Either [DNS.DNSError] [((peerAddr, PeerAdvertise), DNS.TTL)])
    resolveDomain resolver
                  domain@DomainAccessPoint {dapDomain, dapPortNumber}
                  advertisePeer = do
      (errs, results) <- dnsLookupWithTTL resolvConf resolver dapDomain
      mapM_ (traceWith tracer . TraceLocalRootFailure domain . DNSError)
            errs

      if null results
         then return $ Left errs
         else do
           traceWith tracer (TraceLocalRootResult domain results)
           return $ Right [ (( toPeerAddr addr dapPortNumber
                            , advertisePeer)
                            , _ttl)
                          | (addr, _ttl) <- results ]

    monitorDomain
      :: Resource m (DNSorIOError exception) resolver
      -> Seq (Int, Map peerAddr PeerAdvertise)
      -- ^ Static configuration, this always comes from the source
      -- STM transaction 'readDomainGroups'.
      -> (Int, DomainAccessPoint, PeerAdvertise)
      -> m Void
    monitorDomain rr0 rpgStatic (index, domain, advertisePeer) =
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

          reply <- resolveDomain resolver domain advertisePeer
          case reply of
            Left errs -> go rrNext
                           (minimum $ map (\err -> ttlForDnsError err ttl) errs)
            Right results -> do
              rootPeersGroups <- atomically $ do
                rootPeersGroups <- readTVar rootPeersGroupsVar
                    -- We should get the entry from the static configuration in
                    -- order to garbage collect old lookup values for this
                    -- entry. It's important not to overwrite the statically
                    -- configured IPs, that's why we get the entry from the
                    -- statically configured rootPeersGroups list.
                    --
                let (target, entry)  = rpgStatic `Seq.index` index
                    resultsMap       = Map.fromList (map fst results)
                    -- Discard old values and only keep current lookup result.
                    --
                    -- Since the 'loop' function always receives the groups read
                    -- from the source stm transaction 'readDomainGroups', we
                    -- need to merge against it (because it has the statically
                    -- configurated IPs) and not what is read from the TVar
                    -- 'rootPeersGroupsVar'.
                    entry'           = resultsMap <> entry
                    rootPeersGroups' =
                      Seq.update index
                                 (target, entry')
                                 rootPeersGroups

                -- Only overwrite if it changed:
                when (entry /= entry') $
                  writeTVar rootPeersGroupsVar rootPeersGroups'

                return rootPeersGroups'

              traceWith tracer (TraceLocalRootGroups rootPeersGroups)
              go rrNext (ttlForResults (map snd results))

---------------------------------------------
-- Public root peer set provider using DNS
--

data TracePublicRootPeers =
       TracePublicRootRelayAccessPoint [RelayAccessPoint]
     | TracePublicRootDomains [DomainAccessPoint]
     | TracePublicRootResult  DNS.Domain [(IP, DNS.TTL)]
     | TracePublicRootFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
-- TODO track PeerAdvertise
--
publicRootPeersProvider
  :: forall peerAddr resolver exception a m.
     (MonadThrow m, MonadAsync m, Exception exception,
      Ord peerAddr)
  => Tracer m TracePublicRootPeers
  -> (IP -> Socket.PortNumber -> peerAddr)
  -> DNS.ResolvConf
  -> STM m [RelayAccessPoint]
  -> DNSActions resolver exception m
  -> ((Int -> m (Set peerAddr, DiffTime)) -> m a)
  -> m a
publicRootPeersProvider tracer
                        toPeerAddr
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
    processResult :: (DomainAccessPoint, ([DNS.DNSError], [(IP, DNS.TTL)]))
                  -> m (DomainAccessPoint, [(IP, DNS.TTL)])
    processResult (domain, (errs, result)) = do
        mapM_ (traceWith tracer . TracePublicRootFailure (dapDomain domain))
              errs
        when (not $ null result) $
            traceWith tracer $ TracePublicRootResult (dapDomain domain) result

        return (domain, result)

    requestPublicRootPeers
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> Int
      -> m (Set peerAddr, DiffTime)
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
                  [ (,) (DomainAccessPoint domain port)
                      <$> dnsLookupWithTTL
                            resolvConf
                            resolver
                            domain
                  | RelayAccessDomain domain port <- domains ]
            -- The timeouts here are handled by the 'lookupWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            results' <- mapM processResult results
            let successes = [ ( toPeerAddr ip dapPortNumber
                              , ipttl)
                            | ( DomainAccessPoint {dapPortNumber}
                              , ipttls) <- results'
                            , (ip, ipttl) <- ipttls
                            ]
                !domainsIps = [toPeerAddr ip port
                              | RelayAccessAddress ip port <- domains ]
                !ips      = Set.fromList  (map fst successes ++ domainsIps)
                !ttl      = ttlForResults (map snd successes)
            -- If all the lookups failed we'll return an empty set with a minimum
            -- TTL, and the governor will invoke its exponential backoff.
            return (ips, ttl)

-- | Provides DNS resolution functionality.
--
resolveDomainAccessPoint
  :: forall exception resolver m.
     (MonadThrow m, MonadAsync m, Exception exception)
  => Tracer m TracePublicRootPeers
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> [DomainAccessPoint]
  -> m (Map DomainAccessPoint (Set Socket.SockAddr))
resolveDomainAccessPoint tracer
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
    requestPublicRootPeers resourceVar
  where
    requestPublicRootPeers
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> m (Map DomainAccessPoint (Set Socket.SockAddr))
    requestPublicRootPeers resourceVar = do
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (,) domain
                      <$> dnsLookupWithTTL
                            resolvConf
                            resolver
                            (dapDomain domain)
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
