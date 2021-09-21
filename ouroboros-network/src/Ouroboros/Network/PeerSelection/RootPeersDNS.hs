{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

--  'resolverResource' and 'asyncResolverResource' are not used when compiled
--  on @Windows@
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS (
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

import           Data.Word (Word32)
import           Data.List (foldl')
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import           Data.Void (Void)

import           Control.Applicative ((<|>))
import           Control.DeepSeq (NFData (..))
import           Control.Exception (IOException)
import           Control.Monad (when, void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer(..), contramap, traceWith)

import           System.Directory (getModificationTime)

import           Data.IP (IPv4)
import qualified Data.IP as IP
import           Network.DNS (DNSError)
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types

-- | A product of a 'DNS.Domain' and 'Socket.PortNumber'.  After resolving the
-- domain we will use the 'Socket.PortNumber' to form 'Socket.SockAddr'.
--
data DomainAddress = DomainAddress {
    daDomain     :: !DNS.Domain,
    daPortNumber :: !Socket.PortNumber
  }
  deriving (Show, Eq, Ord)

-- | A relay can have either an IP address and a port number or
-- a domain with a port number
data RelayAddress = RelayDomain !DomainAddress
                  | RelayAddress !IP.IP !Socket.PortNumber
                  deriving (Show, Eq, Ord)

-- 'IP' nor 'IPv6' is strict, 'IPv4' is strict only because it's a newtype for
-- a primitive type ('Word32').
--
instance NFData RelayAddress where
  rnf (RelayDomain domain) = domain `seq` ()
  rnf (RelayAddress ip !_port) =
    case ip of
      IP.IPv4 ipv4 -> rnf (IP.fromIPv4w ipv4)
      IP.IPv6 ipv6 -> rnf (IP.fromIPv6w ipv6)

-----------------------------------------------
-- Resource
--

-- | Evolving resource; We use it to reinitialise the dns library if the
-- `/etc/resolv.conf` file was modified.
--
data Resource err a = Resource {
    withResource :: IO (Either err a, Resource err a)
  }

-- | Like 'withResource' but retries untill success.
--
withResource' :: Tracer IO err
              -> NonEmpty DiffTime
              -- ^ delays between each re-try
              -> Resource err a
              -> IO (a, Resource err a)
withResource' tracer delays0 = go delays0
  where
    dropHead :: NonEmpty a -> NonEmpty a
    dropHead as@(_ :| [])  = as
    dropHead (_ :| a : as) = a :| as

    go !delays resource = do
      er <- withResource resource
      case er of
        (Left err, resource') -> do
          traceWith tracer err
          threadDelay (NonEmpty.head delays)
          withResource' tracer (dropHead delays) resource'
        (Right r, resource') ->
          pure (r, resource')


constantResource :: a -> Resource err a
constantResource a = Resource (pure (Right a, constantResource a))

data DNSorIOError
    = DNSError !DNSError
    | IOError  !IOException
  deriving Show

instance Exception DNSorIOError where


-- | Strict version of 'Maybe' adjusted to the needs ot
-- 'asyncResolverResource'.
--
data TimedResolver
    = TimedResolver !DNS.Resolver !UTCTime
    | NoResolver

-- |
--
-- TODO: it could be useful for `publicRootPeersProvider`.
--
resolverResource :: DNS.ResolvConf -> IO (Resource DNSorIOError DNS.Resolver)
resolverResource resolvConf = do
    rs <- DNS.makeResolvSeed resolvConf
    case DNS.resolvInfo resolvConf of
      DNS.RCFilePath filePath ->
        pure $ go filePath NoResolver

      _ -> DNS.withResolver rs (pure . constantResource)

  where
    handlers :: FilePath
             -> TimedResolver
             -> [Handler IO
                  ( Either DNSorIOError DNS.Resolver
                  , Resource DNSorIOError DNS.Resolver)]
    handlers filePath tr =
      [ Handler $
          \(err :: IOException) ->
            pure (Left (IOError err), go filePath tr)
      , Handler $
          \(err :: DNS.DNSError) ->
              pure (Left (DNSError err), go filePath tr)
      ]

    go :: FilePath
       -> TimedResolver
       -> Resource DNSorIOError DNS.Resolver
    go filePath tr@NoResolver = Resource $
      do
        modTime <- getModificationTime filePath
        rs <- DNS.makeResolvSeed resolvConf
        DNS.withResolver rs
          (\resolver ->
            pure (Right resolver, go filePath (TimedResolver resolver modTime)))
      `catches` handlers filePath tr

    go filePath tr@(TimedResolver resolver modTime) = Resource $
      do
        modTime' <- getModificationTime filePath
        if modTime' <= modTime
          then pure (Right resolver, go filePath (TimedResolver resolver modTime))
          else do
            rs <- DNS.makeResolvSeed resolvConf
            DNS.withResolver rs
              (\resolver' ->
                pure (Right resolver', go filePath (TimedResolver resolver' modTime')))
      `catches` handlers filePath tr


-- | `Resource` which passes the 'DNS.Resolver' through a 'StrictTVar'.  Better
-- than 'resolverResource' when using in multiple threads.
--
asyncResolverResource :: DNS.ResolvConf -> IO (Resource DNSorIOError DNS.Resolver)
asyncResolverResource resolvConf =
    case DNS.resolvInfo resolvConf of
      DNS.RCFilePath filePath -> do
        resourceVar <- newTVarIO NoResolver
        pure $ go filePath resourceVar
      _ -> do
        rs <- DNS.makeResolvSeed resolvConf
        DNS.withResolver rs (pure . constantResource)
  where
    handlers :: FilePath -> StrictTVar IO TimedResolver
             -> [Handler IO
                  ( Either DNSorIOError DNS.Resolver
                  , Resource DNSorIOError DNS.Resolver)]
    handlers filePath resourceVar =
      [ Handler $
          \(err :: IOException) ->
            pure (Left (IOError err), go filePath resourceVar)
      , Handler $
          \(err :: DNS.DNSError) ->
            pure (Left (DNSError err), go filePath resourceVar)
      ]

    go :: FilePath -> StrictTVar IO TimedResolver
       -> Resource DNSorIOError DNS.Resolver
    go filePath resourceVar = Resource $ do
      r <- atomically (readTVar resourceVar)
      case r of
        NoResolver ->
          do
            modTime <- getModificationTime filePath
            rs <- DNS.makeResolvSeed resolvConf
            DNS.withResolver rs $ \resolver -> do
              atomically (writeTVar resourceVar (TimedResolver resolver modTime))
              pure (Right resolver, go filePath resourceVar)
          `catches` handlers filePath resourceVar

        TimedResolver resolver modTime ->
          do
            modTime' <- getModificationTime filePath
            if modTime' <= modTime
                then pure (Right resolver, go filePath resourceVar)
                else do
                  rs <- DNS.makeResolvSeed resolvConf
                  DNS.withResolver rs $ \resolver' -> do
                    atomically (writeTVar resourceVar (TimedResolver resolver' modTime'))
                    pure (Right resolver', go filePath resourceVar)
          `catches` handlers filePath resourceVar


#if defined(mingw32_HOST_OS)
-- | Returns a newly intiatialised 'DNS.Resolver' at each step;  This is only
-- for Windows, where we don't have a way to check that the network
-- configuration has changed.  On /Windows/ the 'dns' library is using
-- @GetNetworkParams@ win32 api call to get the list of default dns servers.
--
newResolverResource :: DNS.ResolvConf -> Resource DNSorIOError DNS.Resolver
newResolverResource resolvConf = go
    where
      go = Resource $
        do
          rs <- DNS.makeResolvSeed resolvConf
          DNS.withResolver rs $ \resolver -> pure (Right resolver, go)
        `catches` handlers

      handlers :: [Handler IO
                    ( Either DNSorIOError DNS.Resolver
                    , Resource DNSorIOError DNS.Resolver)]
      handlers =
        [ Handler $
            \(err :: IOException) ->
              pure (Left (IOError err), go)
        , Handler $
            \(err :: DNS.DNSError) ->
              pure (Left (DNSError err), go)
        ]
#endif


-----------------------------------------------
-- local root peer set provider based on DNS
--

data TraceLocalRootPeers =
       TraceLocalRootDomains [(Int, Map RelayAddress PeerAdvertise)]
       -- ^ 'Int' is the configured valency for the local producer groups
     | TraceLocalRootWaiting DomainAddress DiffTime
     | TraceLocalRootResult  DomainAddress [(IPv4, DNS.TTL)]
     | TraceLocalRootFailure DomainAddress DNSorIOError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
localRootPeersProvider
  :: Tracer IO TraceLocalRootPeers
  -> TimeoutFn IO
  -> DNS.ResolvConf
  -> StrictTVar IO [(Int, Map Socket.SockAddr PeerAdvertise)]
  -> StrictTVar IO [(Int, Map RelayAddress PeerAdvertise)]
  -> IO Void
localRootPeersProvider tracer
                       timeout
                       resolvConf
                       rootPeersGroupsVar
                       domainsGroupsVar = do
  domainsGroups <- atomically $ readTVar domainsGroupsVar
  traceWith tracer (TraceLocalRootDomains domainsGroups)
#if !defined(mingw32_HOST_OS)
  rr <- asyncResolverResource resolvConf
#else
  let rr = newResolverResource resolvConf
#endif
  let
      -- Flatten the local root peers groups and associate a group index to each
      -- DomainAddress to be monitorized.
      domains :: [(Int, DomainAddress, PeerAdvertise)]
      domains = [ (group, domain, pa)
                  | (group, (_, m)) <- zip [0..] domainsGroups
                  , (RelayDomain domain, pa) <- Map.toList m ]
      -- Since we want to preserve the number of groups, the targets, and
      -- the addresses within each group, we fill the TVar with
      -- a placeholder list, in order for each monitored DomainAddress to
      -- be updated in the correct group.
      rootPeersGroups :: [(Int, Map Socket.SockAddr PeerAdvertise)]
      rootPeersGroups = map (\(target, m) -> (target, f m)) domainsGroups
        where
           f :: Map RelayAddress PeerAdvertise -> Map Socket.SockAddr PeerAdvertise
           f =   Map.mapKeys
                   (\k -> case k of
                     RelayAddress ip port ->
                       IP.toSockAddr (ip, port)
                     _ ->
                       error "localRootPeersProvider: impossible happend"
                   )
               . Map.filterWithKey
                   (\k _ -> case k of
                     RelayAddress {} -> True
                     RelayDomain {}  -> False
                   )

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
                         domainsGroupsVar

  where
    waitConfigChanged :: [(Int, Map RelayAddress PeerAdvertise)] -> STM IO ()
    waitConfigChanged dg = do
      dg' <- readTVar domainsGroupsVar
      check (dg /= dg')

    resolveDomain
      :: DNS.Resolver
      -> DomainAddress
      -> PeerAdvertise
      -> IO (Either DNSError [((Socket.SockAddr, PeerAdvertise), DNS.TTL)])
    resolveDomain resolver
                  domain@DomainAddress {daDomain, daPortNumber}
                  advertisePeer = do
      reply <- lookupAWithTTL timeout resolvConf resolver daDomain
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
      :: Resource DNSorIOError DNS.Resolver
      -> (Int, DomainAddress, PeerAdvertise)
      -> IO Void
    monitorDomain rr0 (group, domain, advertisePeer) =
        go rr0 0
      where
        go :: Resource DNSorIOError DNS.Resolver -> DiffTime -> IO Void
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
              atomically $ do
                rootPeersGroups <- readTVar rootPeersGroupsVar
                let rootPeersGroups' = IntMap.fromList $ zip [0,1..] rootPeersGroups
                    (target, entry)  = rootPeersGroups' IntMap.! group
                    resultsMap       = Map.fromList (map fst results)
                    entry'           = resultsMap <> entry
                    newRootPeers     =
                      IntMap.elems $
                      IntMap.insert group
                                    (target, entry')
                                    rootPeersGroups'

                -- Only overwrite if it changed:
                when (entry /= entry') $
                  writeTVar rootPeersGroupsVar newRootPeers

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
publicRootPeersProvider :: Tracer IO TracePublicRootPeers
                        -> TimeoutFn IO
                        -> DNS.ResolvConf
                        -> StrictTVar IO [RelayAddress]
                        -> ((Int -> IO (Set Socket.SockAddr, DiffTime)) -> IO a)
                        -> IO a
publicRootPeersProvider tracer timeout resolvConf domainsVar action = do
    domains <- atomically $ readTVar domainsVar
    traceWith tracer (TracePublicRootRelayAddresses domains)
#if !defined(mingw32_HOST_OS)
    rr <- resolverResource resolvConf
#else
    let rr = newResolverResource resolvConf
#endif
    resourceVar <- newTVarIO rr
    action (requestPublicRootPeers resourceVar)
  where
    requestPublicRootPeers :: StrictTVar IO (Resource DNSorIOError DNS.Resolver)
                           -> Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers resourceVar _numRequested = do
        domains <- atomically $ readTVar domainsVar
        traceWith tracer (TracePublicRootRelayAddresses domains)
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (,) domain <$> lookupAWithTTL timeout resolvConf resolver (daDomain domain)
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
            let successes = [ (Socket.SockAddrInet daPortNumber (IP.toHostAddress ip), ipttl)
                            | (DomainAddress {daPortNumber}, Right ipttls) <- results
                            , (ip, ipttl) <- ipttls
                            ]
                !domainsIps = [ IP.toSockAddr (ip, port)
                              | RelayAddress ip port <- domains ]
                !ips      = Set.fromList  (map fst successes ++ domainsIps)
                !ttl      = ttlForResults (map snd successes)
            -- If all the lookups failed we'll return an empty set with a minimum
            -- TTL, and the governor will invoke its exponential backoff.
            return (ips, ttl)

-- | Provides DNS resulution functionality.
--
resolveDomainAddresses :: Tracer IO TracePublicRootPeers
                       -> TimeoutFn IO
                       -> DNS.ResolvConf
                       -> [DomainAddress]
                       -> IO (Map DomainAddress (Set Socket.SockAddr))
resolveDomainAddresses tracer timeout resolvConf domains = do
    traceWith tracer (TracePublicRootDomains domains)
#if !defined(mingw32_HOST_OS)
    rr <- resolverResource resolvConf
#else
    let rr = newResolverResource resolvConf
#endif
    resourceVar <- newTVarIO rr
    requestPublicRootPeers resourceVar
  where
    requestPublicRootPeers :: StrictTVar IO (Resource DNSorIOError DNS.Resolver)
                           -> IO (Map DomainAddress (Set Socket.SockAddr))
    requestPublicRootPeers resourceVar = do
        rr <- atomically $ readTVar resourceVar
        (er, rr') <- withResource rr
        atomically $ writeTVar resourceVar rr'
        case er of
          Left (DNSError err) -> throwIO err
          Left (IOError  err) -> throwIO err
          Right resolver -> do
            let lookups =
                  [ (,) domain <$> lookupAWithTTL timeout resolvConf resolver (daDomain domain)
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
                !addrs = map (Socket.SockAddrInet (daPortNumber domain) . IP.toHostAddress) ips
                !addrSet = Set.fromList addrs in
            Just addrSet
        addFn (Just addrSet) =
            let ips = map fst ipsttls
                !addrs = map (Socket.SockAddrInet (daPortNumber domain) . IP.toHostAddress) ips
                !addrSet' = Set.union addrSet (Set.fromList addrs) in
            Just addrSet'


---------------------------------------------
-- Shared utils
--

-- | Like 'DNS.lookupA' but also return the TTL for the results.
--
-- DNS library timeouts do not work reliably on Windows (#1873), hende the
-- additional timeout.
--
lookupAWithTTL :: TimeoutFn IO
               -> DNS.ResolvConf
               -> DNS.Resolver
               -> DNS.Domain
               -> IO (Either DNS.DNSError [(IPv4, DNS.TTL)])
lookupAWithTTL timeout resolvConf resolver domain = do
    reply <- timeout (microsecondsAsIntToDiffTime $ DNS.resolvTimeout resolvConf)  $ DNS.lookupRaw resolver domain DNS.A
    case reply of
      Nothing -> return (Left DNS.TimeoutExpired)
      Just (Left  err) -> return (Left err)
      Just (Right ans) -> return (DNS.fromDNSMessage ans selectA)
      --TODO: we can get the SOA TTL on NXDOMAIN here if we want to
  where
    selectA DNS.DNSMessage { DNS.answer } =
      [ (addr, ttl)
      | DNS.ResourceRecord {
          DNS.rdata = DNS.RD_A addr,
          DNS.rrttl = ttl
        } <- answer
      ]

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

withAsyncAll :: [IO a] -> ([Async IO a] -> IO b) -> IO b
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
