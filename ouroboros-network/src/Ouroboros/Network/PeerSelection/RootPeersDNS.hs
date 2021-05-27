{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Ouroboros.Network.PeerSelection.RootPeersDNS (
    -- * DNS based actions for local and public root providers
    DNSActions (..),

    -- * DNS resolver IO auxiliar functions
    constantResource,
    -- ** DNSActions IO
    ioDNSActions,

    -- * DNS based provider for local root peers
    localRootPeersProvider,
    DomainAddress (..),
    RelayAddress (..),
    IP.IP (..),
    TraceLocalRootPeers(..),

    -- * DNS based provider for public root peers
    publicRootPeersProvider,
    TracePublicRootPeers(..),

    -- DNS lookup support
    resolveDomainAddresses,

    -- * DNS type re-exports
    DNS.ResolvConf,
    DNS.Domain,
    DNS.TTL,

    -- * Socket type re-exports
    Socket.PortNumber,
  ) where

import           Data.Aeson
import           Data.Word (Word32)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import           Data.Void (Void)

import           Control.Applicative ((<|>))
import           Control.Monad (when, void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer(..), contramap, traceWith)


import           Data.IP (IPv4)
import qualified Data.IP as IP
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                 ( DNSorIOError (..)
                 , DNSActions (..)
                 , Resource (..)
                 , ioDNSActions
                 , constantResource
                 , withResource'
                 )

-- | A product of a 'DNS.Domain' and 'Socket.PortNumber'.  After resolving the
-- domain we will use the 'Socket.PortNumber' to form 'Socket.SockAddr'.
--
data DomainAddress = DomainAddress {
    daDomain     :: !DNS.Domain,
    daPortNumber :: !Socket.PortNumber
  }
  deriving (Show, Eq, Ord)

instance FromJSON DomainAddress where
  parseJSON = withObject "DomainAddress" $ \v ->
    DomainAddress
      <$> (encodeUtf8 <$> v .: "addr")
      <*> ((fromIntegral :: Int -> Socket.PortNumber) <$> v .: "port")

instance ToJSON DomainAddress where
  toJSON da =
    object
      [ "addr" .= decodeUtf8 (daDomain da)
      , "port" .= (fromIntegral (daPortNumber da) :: Int)
      ]

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
--
data RelayAddress = RelayDomain !DomainAddress
                  | RelayAddress !IP.IP !Socket.PortNumber
                  deriving (Show, Eq, Ord)

instance FromJSON RelayAddress where
  parseJSON = withObject "RelayAddress" $ \v -> do
    addr <- v .: "addr"
    port <- v .: "port"
    return (toRelayAddress addr port)

instance ToJSON RelayAddress where
  toJSON (RelayDomain (DomainAddress addr port)) =
    object
      [ "addr" .= decodeUtf8 addr
      , "port" .= (fromIntegral port :: Int)
      ]
  toJSON (RelayAddress ip port) =
    object
      [ "addr" .= Text.pack (show ip)
      , "port" .= (fromIntegral port :: Int)
      ]

-- | Parse a address field as either an IP address or a DNS address.
-- Returns corresponding RelayAddress.
--
toRelayAddress :: Text -> Int -> RelayAddress
toRelayAddress address port =
    case readMaybe (Text.unpack address) of
      Nothing   -> RelayDomain (DomainAddress (encodeUtf8 address) (fromIntegral port))
      Just addr -> RelayAddress addr (fromIntegral port)

-----------------------------------------------
-- local root peer set provider based on DNS
--

data TraceLocalRootPeers exception =
       TraceLocalRootDomains [(Int, Map RelayAddress PeerAdvertise)]
       -- ^ 'Int' is the configured valency for the local producer groups
     | TraceLocalRootWaiting DomainAddress DiffTime
     | TraceLocalRootResult  DomainAddress [(IPv4, DNS.TTL)]
     | TraceLocalRootGroups  (Seq (Int, Map Socket.SockAddr PeerAdvertise))
       -- ^ This traces the results of the local root peer provider
     | TraceLocalRootFailure DomainAddress (DNSorIOError exception)
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
localRootPeersProvider
  :: forall m resolver exception.
    (MonadAsync m, MonadDelay m)
  => Tracer m (TraceLocalRootPeers exception)
  -> TimeoutFn m
  -> DNS.ResolvConf
  -> StrictTVar m (Seq (Int, Map Socket.SockAddr PeerAdvertise))
  -> STM m [(Int, Map RelayAddress PeerAdvertise)]
  -> DNSActions resolver exception m
  -> m Void
localRootPeersProvider tracer
                       timeout
                       resolvConf
                       rootPeersGroupsVar
                       readDomainsGroups
                       dnsA@DNSActions {
                        dnsAsyncResolverResource,
#if defined(mingw32_HOST_OS)
                        dnsNewResolverResource,
#endif
                        dnsLookupAWithTTL
                       } = do
  domainsGroups <- atomically readDomainsGroups
  traceWith tracer (TraceLocalRootDomains domainsGroups)
#if !defined(mingw32_HOST_OS)
  rr <- dnsAsyncResolverResource resolvConf
#else
  let rr = dnsNewResolverResource resolvConf
#endif
  let
      -- Flatten the local root peers groups and associate its index to each
      -- DomainAddress to be monitorized.
      -- NOTE: We need to pair the index because the resulting list can be
      -- sparse.
      domains :: [(Int, DomainAddress, PeerAdvertise)]
      domains = [ (index, domain, pa)
                | (index, (_, m)) <- zip [0..] domainsGroups
                , (RelayDomain domain, pa) <- Map.toList m ]
      -- Since we want to preserve the number of groups, the targets, and
      -- the addresses within each group, we fill the TVar with
      -- a placeholder list, in order for each monitored DomainAddress to
      -- be updated in the correct group.
      rootPeersGroups :: Seq (Int, Map Socket.SockAddr PeerAdvertise)
      rootPeersGroups =
        Seq.fromList
          $ map (\(target, m) -> (target, f m)) domainsGroups
        where
           f :: Map RelayAddress PeerAdvertise
             -> Map Socket.SockAddr PeerAdvertise
           f =   Map.mapKeys
                   (\k -> case k of
                     RelayAddress ip port ->
                       IP.toSockAddr (ip, port)
                     _ ->
                       error "localRootPeersProvider: impossible happened"
                   )
               . Map.filterWithKey
                   (\k _ -> case k of
                     RelayAddress {} -> True
                     RelayDomain {}  -> False
                   )

  traceWith tracer (TraceLocalRootGroups rootPeersGroups)
  atomically $
    writeTVar rootPeersGroupsVar rootPeersGroups

  -- Launch DomainAddress monitoring threads and
  -- wait for threads to return or for local config changes
  withAsyncAll (map (monitorDomain rr) domains) $ \as ->
    atomically $
        void (waitAnySTM as) <|> waitConfigChanged domainsGroups

  -- Recursive call to remonitor all new/updated local root peers
  localRootPeersProvider tracer
                         timeout
                         resolvConf
                         rootPeersGroupsVar
                         readDomainsGroups
                         dnsA

  where
    waitConfigChanged :: [(Int, Map RelayAddress PeerAdvertise)] -> STM m ()
    waitConfigChanged dg = do
      dg' <- readDomainsGroups
      check (dg /= dg')

    resolveDomain
      :: resolver
      -> DomainAddress
      -> PeerAdvertise
      -> m (Either DNS.DNSError [((Socket.SockAddr, PeerAdvertise), DNS.TTL)])
    resolveDomain resolver
                  domain@DomainAddress {daDomain, daPortNumber}
                  advertisePeer = do
      reply <- dnsLookupAWithTTL timeout resolvConf resolver daDomain
      case reply of
        Left  err -> do
          traceWith tracer (TraceLocalRootFailure domain (DNSError err))
          return $ Left err

        Right results -> do
          traceWith tracer (TraceLocalRootResult domain results)
          return $ Right [ (( Socket.SockAddrInet
                               daPortNumber
                               (IP.toHostAddress addr)
                           , advertisePeer)
                           , _ttl)
                         | (addr, _ttl) <- results ]

    monitorDomain
      :: Resource m (DNSorIOError exception) resolver
      -> (Int, DomainAddress, PeerAdvertise)
      -> m Void
    monitorDomain rr0 (index, domain, advertisePeer) =
        go rr0 0
      where
        go :: Resource m (DNSorIOError exception) resolver -> DiffTime -> m Void
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
            Left err -> go rrNext (ttlForDnsError err ttl)
            Right results -> do
              result <- atomically $ do
                rootPeersGroups <- readTVar rootPeersGroupsVar
                let (target, entry)  = rootPeersGroups `Seq.index` index
                    resultsMap       = Map.fromList (map fst results)
                    entry'           = resultsMap <> entry
                    rootPeersGroups' =
                      Seq.update index
                                 (target, entry')
                                 rootPeersGroups

                -- Only overwrite if it changed:
                when (entry /= resultsMap) $
                  writeTVar rootPeersGroupsVar rootPeersGroups'

                return rootPeersGroups'

              traceWith tracer (TraceLocalRootGroups result)
              go rrNext (ttlForResults (map snd results))

---------------------------------------------
-- Public root peer set provider using DNS
--

data TracePublicRootPeers =
       TracePublicRootRelayAddresses [RelayAddress]
     | TracePublicRootDomains [DomainAddress]
     | TracePublicRootResult  DNS.Domain [(IPv4, DNS.TTL)]
     | TracePublicRootFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
-- TODO track PeerAdvertise
--
publicRootPeersProvider
  :: forall resolver exception a m.
     (MonadThrow m, MonadAsync m, Exception exception)
  => Tracer m TracePublicRootPeers
  -> TimeoutFn m
  -> DNS.ResolvConf
  -> STM m [RelayAddress]
  -> DNSActions resolver exception m
  -> ((Int -> m (Set Socket.SockAddr, DiffTime)) -> m a)
  -> m a
publicRootPeersProvider tracer
                        timeout
                        resolvConf
                        readDomains
                        DNSActions {
                          dnsResolverResource,
#if defined(mingw32_HOST_OS)
                          dnsNewResolverResource,
#endif
                          dnsLookupAWithTTL
                        }
                        action = do
    domains <- atomically readDomains
    traceWith tracer (TracePublicRootRelayAddresses domains)
#if !defined(mingw32_HOST_OS)
    rr <- dnsResolverResource resolvConf
#else
    let rr = dnsNewResolverResource resolvConf
#endif
    resourceVar <- newTVarIO rr
    action (requestPublicRootPeers resourceVar)
  where
    requestPublicRootPeers
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> Int
      -> m (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers resourceVar _numRequested = do
        domains <- atomically readDomains
        traceWith tracer (TracePublicRootRelayAddresses domains)
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (,) domain
                      <$> dnsLookupAWithTTL
                            timeout
                            resolvConf
                            resolver
                            (daDomain domain)
                  | RelayDomain domain <- domains ]
            -- The timeouts here are handled by the 'lookupAWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            sequence_
              [ traceWith tracer $ case result of
                  Left  dnserr -> TracePublicRootFailure daDomain dnserr
                  Right ipttls -> TracePublicRootResult  daDomain ipttls
              | (DomainAddress {daDomain}, result) <- results ]
            let successes = [ ( Socket.SockAddrInet daPortNumber
                                                    (IP.toHostAddress ip)
                              , ipttl)
                            | ( DomainAddress {daPortNumber}
                              , Right ipttls) <- results
                            , (ip, ipttl) <- ipttls
                            ]
                !domainsIps = [ IP.toSockAddr (ip, port)
                              | RelayAddress ip port <- domains ]
                !ips      = Set.fromList  (map fst successes ++ domainsIps)
                !ttl      = ttlForResults (map snd successes)
            -- If all the lookups failed we'll return an empty set with a minimum
            -- TTL, and the governor will invoke its exponential backoff.
            return (ips, ttl)

-- | Provides DNS resolution functionality.
--
resolveDomainAddresses
  :: forall exception resolver m.
  (MonadThrow m, MonadAsync m, Exception exception)
  => Tracer m TracePublicRootPeers
  -> TimeoutFn m
  -> DNS.ResolvConf
  -> DNSActions resolver exception m
  -> [DomainAddress]
  -> m (Map DomainAddress (Set Socket.SockAddr))
resolveDomainAddresses tracer
                       timeout
                       resolvConf
                       DNSActions {
                          dnsResolverResource,
#if defined(mingw32_HOST_OS)
                          dnsNewResolverResource,
#endif
                          dnsLookupAWithTTL
                        }
                       domains
                       = do
    traceWith tracer (TracePublicRootDomains domains)
#if !defined(mingw32_HOST_OS)
    rr <- dnsResolverResource resolvConf
#else
    let rr = dnsNewResolverResource resolvConf
#endif
    resourceVar <- newTVarIO rr
    requestPublicRootPeers resourceVar
  where
    requestPublicRootPeers
      :: StrictTVar m (Resource m (DNSorIOError exception) resolver)
      -> m (Map DomainAddress (Set Socket.SockAddr))
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
                      <$> dnsLookupAWithTTL
                            timeout
                            resolvConf
                            resolver
                            (daDomain domain)
                  | domain <- domains ]
            -- The timeouts here are handled by the 'lookupAWithTTL'. They're
            -- configured via the DNS.ResolvConf resolvTimeout field and defaults
            -- to 3 sec.
            results <- withAsyncAll lookups (atomically . mapM waitSTM)
            sequence_
              [ traceWith tracer $ case result of
                  Left  dnserr -> TracePublicRootFailure daDomain dnserr
                  Right ipttls -> TracePublicRootResult  daDomain ipttls
              | (DomainAddress {daDomain}, result) <- results ]
            return $ foldl' buildResult Map.empty results

    buildResult :: Map DomainAddress (Set Socket.SockAddr)
                -> (DomainAddress, Either DNS.DNSError [(IPv4, DNS.TTL)])
                -> Map DomainAddress (Set Socket.SockAddr)
    buildResult mr (_, Left _) = mr
    buildResult mr (domain, Right ipsttls) =
        Map.alter addFn domain mr
      where
        addFn :: Maybe (Set Socket.SockAddr) -> Maybe (Set Socket.SockAddr)
        addFn Nothing =
            let ips = map fst ipsttls
                !addrs =
                  map ( Socket.SockAddrInet (daPortNumber domain)
                      . IP.toHostAddress)
                      ips
                !addrSet = Set.fromList addrs in
            Just addrSet
        addFn (Just addrSet) =
            let ips = map fst ipsttls
                !addrs =
                  map ( Socket.SockAddrInet (daPortNumber domain)
                      . IP.toHostAddress)
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
    go as []     = action as
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

---------------------------------------------
-- Examples
--
{-
exampleLocal :: [DomainAddress] -> IO ()
exampleLocal domains = do
      rootPeersVar <- newTVarIO Map.empty
      withAsync (observer rootPeersVar Map.empty) $ \_ ->
        provider rootPeersVar
  where
    provider rootPeersVar =
      withTimeoutSerial $ \timeout ->
        localRootPeersProvider
          (showTracing stdoutTracer)
          timeout
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

examplePublic :: [DomainAddress] -> IO ()
examplePublic domains = do
    withTimeoutSerial $ \timeout ->
      publicRootPeersProvider
        (showTracing stdoutTracer)
        timeout
        DNS.defaultResolvConf
        domains $ \requestPublicRootPeers ->
          forever $ do
            (ips, ttl) <- requestPublicRootPeers 42
            traceWith (showTracing stdoutTracer) (ips, ttl)
            threadDelay ttl
-}
