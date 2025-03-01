{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
  ( -- * DNS based actions for local and public root providers
    DNSActions (..)
    -- * DNSActions IO
  , ioDNSActions
  , DNSLookupType (..)
  , DNSLookupResult
    -- * Utils
    -- ** Resource
  , Resource (..)
  , retryResource
  , constantResource
    -- ** Exposed for testing purposes
  , dispatchLookupWithTTL
    -- ** Error type
  , DNSorIOError (..)
    -- * Tracing types
  , DnsTrace (..)
  , DnsPeersKind (..)
  ) where

import Data.Foldable qualified as Fold
import Data.Function (fix)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromJust, listToMaybe)
import Data.Word (Word16)

import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.Trans ()

import Control.Concurrent.Class.MonadSTM.Strict

import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.Except
#else
import Control.Monad.Except hiding (fix)
#endif
import Control.Tracer (Tracer (..), traceWith)

#if !defined(mingw32_HOST_OS)
import System.Directory (getModificationTime)
#endif

import Network.DNS (DNSError, DNSMessage)
import Network.DNS qualified as DNS
import System.Random

import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeersKind)

data DnsPeersKind = DnsLocalPeer | DnsPublicPeer | DnsLedgerPeer !LedgerPeersKind
  deriving (Show)

data DnsTrace = DnsResult
                  DnsPeersKind
                  DNS.Domain
                  -- ^ source of addresses
                  (Maybe DNS.Domain)
                  -- ^ SRV domain, if relevant
                  [(IP, PortNumber, DNS.TTL)]
                  -- ^ payload
              | DnsTraceLookupError !DnsPeersKind !(Maybe DNSLookupType) !DNS.Domain !DNS.DNSError
              | DnsSRVFail
                 !DnsPeersKind
                 !DNS.Domain -- ^ SRV domain
  deriving (Show)

data DNSLookupType = LookupReqAOnly
                   | LookupReqAAAAOnly
                   | LookupReqAAndAAAA
                   deriving Show

data DNSorIOError exception
    = DNSError !DNSError
    | IOError  !exception
  deriving Show

-- | The type returned by 'DNSActions.dnsLookupWithTTL'
-- indicating the type of domain attempted to resolve
--
type DNSLookupResult peerAddr =
    Either [DNS.DNSError] [(peerAddr, DNS.TTL)]

instance Exception exception => Exception (DNSorIOError exception) where

-----------------------------------------------
-- Resource
--

-- | Evolving resource; We use it to reinitialise the DNS library if the
-- `/etc/resolv.conf` file was modified.
--
-- Note: `constantResource` and `retryResource` are written using a simplified approach
-- inspired by _"The Different Aspects of Monads and Mixins"_, by Bruno C. d S.
-- Oliveira, see https://www.youtube.com/watch?v=pfwP4hXM5hA.
--
newtype Resource m a = Resource {
    withResource :: m (a, Resource m a)
  }
  deriving Functor

constantResource :: forall m a. Applicative m => a -> Resource m a
constantResource = fix go
  where
    go :: (a -> Resource m a)
       -> (a -> Resource m a)
    go this a = Resource $ pure (a, this a)

-- | A 'Resource` which will exhibit the given `Resource` but retry it with
-- a given delay until a success. On first success it will reset the delays.
--
retryResource :: forall m e a. MonadDelay m
              => Tracer m e
              -> NonEmpty DiffTime
              -> Resource m (Either e a)
              -> Resource m a
retryResource tracer ds0 = fix step ds0
  where
    step :: (NonEmpty DiffTime -> Resource m (Either e a) -> Resource m a)
         -> (NonEmpty DiffTime -> Resource m (Either e a) -> Resource m a)
    step rec_ ds@(d :| _) resource = Resource $ do
        ea <- withResource resource
        case ea of
          (Left e, resource') -> do
            traceWith tracer e
            threadDelay d
            -- continue with next delays and `resource'`
            withResource (rec_ (dropHead ds) resource')
          (Right a, resource') ->
            -- reset delays, continue with `resource'`
            return (a, rec_ ds0 resource')

    dropHead :: forall x. NonEmpty x -> NonEmpty x
    dropHead xs@(_ :| [])  = xs
    dropHead (_ :| x : xs) = x :| xs

#if !defined(mingw32_HOST_OS)
type TimeStamp = UTCTime
#else
type TimeStamp = Time
#endif

#if defined(mingw32_HOST_OS)
-- | on Windows we will reinitialise the dns library every 60s.
--
dns_REINITIALISE_INTERVAL :: DiffTime
dns_REINITIALISE_INTERVAL = 60
#endif

getTimeStamp :: FilePath
             -> IO TimeStamp
#if !defined(mingw32_HOST_OS)
getTimeStamp = getModificationTime
#else
getTimeStamp _ = addTime dns_REINITIALISE_INTERVAL <$> getMonotonicTime
#endif


-- | Strict version of 'Maybe' adjusted to the needs ot
-- 'asyncResolverResource'.
--
data TimedResolver
    = TimedResolver !DNS.Resolver !TimeStamp
    | NoResolver

-- | Dictionary of DNS actions vocabulary
--
data DNSActions peerAddr resolver exception m = DNSActions {

    -- |
    --
    -- TODO: it could be useful for `publicRootPeersProvider`.
    --
    dnsResolverResource      :: DNS.ResolvConf
                             -> m (Resource m (Either (DNSorIOError exception) resolver)),

    -- | `Resource` which passes the 'DNS.Resolver' (or abstract resolver type)
    -- through a 'StrictTVar'. Better than 'resolverResource' when using in
    -- multiple threads.
    --
    -- On /Windows/ we use a different implementation which always returns
    -- a newly initialised 'DNS.Resolver' at each step.  This is because on
    -- /Windows/ we don't have a way to check that the network configuration has
    -- changed.  The 'dns' library is using 'GetNetworkParams@ win32 api call
    -- to get the list of default dns servers.
    dnsAsyncResolverResource :: DNS.ResolvConf
                             -> m (Resource m (Either (DNSorIOError exception) resolver)),

    -- | Like 'DNS.lookupA' but also return the TTL for the results.
    --
    -- DNS library timeouts do not work reliably on Windows (#1873), hence the
    -- additional timeout.
    --
    dnsLookupWithTTL         :: DnsPeersKind
                             -> RelayAccessPoint
                             -> DNS.ResolvConf
                             -> resolver
                             -> StdGen
                             -> m (DNSLookupResult peerAddr)
  }



-- | Get a resolver from 'DNS.ResolvConf'.
--
-- 'DNS.withResolver' is written in continuation passing style, there's no
-- handler with closes in anyway when it returns, hence 'getResolver' does not
-- break it.
--
getResolver :: DNS.ResolvConf -> IO DNS.Resolver
getResolver resolvConf = do
    rs <- DNS.makeResolvSeed resolvConf
    DNS.withResolver rs return


-- | IO DNSActions which resolve domain names with 'DNS.Resolver'.
--
-- The IPv4 and IPv6 addresses the node will be using should determine the
-- LookupReqs so that we can avoid lookups for address types that wont be used.
--
-- It guarantees that returned TTLs are strictly greater than 0.
--
ioDNSActions :: Tracer IO DnsTrace
             -> DNSLookupType
             -> (IP -> PortNumber -> peerAddr)
             -> DNSActions peerAddr DNS.Resolver IOException IO
ioDNSActions tracer lookupType toPeerAddr =
    DNSActions {
      dnsResolverResource      = resolverResource,
      dnsAsyncResolverResource = asyncResolverResource,
      dnsLookupWithTTL         = dispatchLookupWithTTL lookupType mkResolveDNSIOAction tracer toPeerAddr
      }
  where
    mkResolveDNSIOAction resolver resolvConf domain ofType =
      timeout (microsecondsAsIntToDiffTime
               $ DNS.resolvTimeout resolvConf)
              (DNS.lookupRaw resolver domain ofType)

    -- |
    --
    -- TODO: it could be useful for `publicRootPeersProvider`.
    --
    resolverResource :: DNS.ResolvConf
                     -> IO (Resource IO (Either (DNSorIOError IOException) DNS.Resolver))
    resolverResource resolvConf = do
        rs <- DNS.makeResolvSeed resolvConf
        case DNS.resolvInfo resolvConf of
          DNS.RCFilePath filePath ->
            pure $ go filePath NoResolver

          _ -> DNS.withResolver rs (pure . fmap Right . constantResource)

      where
        handlers :: [ Handler IO (Either (DNSorIOError IOException) a) ]
        handlers  = [ Handler $ pure . Left . IOError
                    , Handler $ pure . Left . DNSError
                    ]

        go :: FilePath
           -> TimedResolver
           -> Resource IO (Either (DNSorIOError IOException) DNS.Resolver)
        go filePath tr@NoResolver = Resource $
          do
            result
              <- (curry Right
                   <$> getTimeStamp filePath
                   <*> getResolver resolvConf)
                 `catches` handlers
            case result of
              Left err ->
                pure (Left err, go filePath tr)
              Right (modTime, resolver) -> do
                pure (Right resolver, go filePath (TimedResolver resolver modTime))

        go filePath tr@(TimedResolver resolver modTime) = Resource $ do
          result <- runExceptT $ do
            modTime' <- ExceptT $ (Right <$> getTimeStamp filePath)
                                  `catches` handlers
            if modTime' <= modTime
              then return (resolver, modTime)
              else do
                resolver' <- ExceptT $ (Right <$> getResolver resolvConf)
                                       `catches` handlers
                return (resolver', modTime')
          case result of
            Left err ->
              return (Left err, go filePath tr)
            Right (resolver', modTime') ->
              return (Right resolver', go filePath (TimedResolver resolver' modTime'))


    -- | `Resource` which passes the 'DNS.Resolver' through a 'StrictTVar'.  Better
    -- than 'resolverResource' when using in multiple threads.
    --
    asyncResolverResource :: DNS.ResolvConf
                          -> IO (Resource IO (Either (DNSorIOError IOException) DNS.Resolver))

    asyncResolverResource resolvConf =
        case DNS.resolvInfo resolvConf of
          DNS.RCFilePath filePath -> do
            resourceVar <- newTVarIO NoResolver
            pure $ go filePath resourceVar
          _ -> do
            fmap Right . constantResource <$> getResolver resolvConf
      where
        handlers :: [ Handler IO (Either (DNSorIOError IOException) a) ]
        handlers  = [ Handler $ pure . Left . IOError
                    , Handler $ pure . Left . DNSError
                    ]

        go :: FilePath -> StrictTVar IO TimedResolver
           -> Resource IO (Either (DNSorIOError IOException) DNS.Resolver)
        go filePath resourceVar = Resource $ do
          r <- readTVarIO resourceVar
          case r of
            NoResolver ->
              do
                result
                  <- (curry Right
                       <$> getTimeStamp filePath
                       <*> getResolver resolvConf)
                     `catches` handlers
                case result of
                  Left err ->
                    pure (Left err, go filePath resourceVar)
                  Right (modTime, resolver) -> do
                    atomically (writeTVar resourceVar (TimedResolver resolver modTime))
                    pure (Right resolver, go filePath resourceVar)

            TimedResolver resolver modTime -> do
              result <- runExceptT $ do
                modTime' <- ExceptT $ (Right <$> getTimeStamp filePath)
                                      `catches` handlers
                if modTime' <= modTime
                  then return resolver
                  else do
                  resolver' <- ExceptT $ (Right <$> getResolver resolvConf)
                                         `catches` handlers
                  atomically (writeTVar (castStrictTVar resourceVar)
                             (TimedResolver resolver' modTime'))
                  return resolver'
              case result of
                Left err ->
                  return (Left err, go filePath resourceVar)
                Right resolver' ->
                  return (Right resolver', go filePath resourceVar)


srvRecordLookupWithTTL :: forall peerAddr m. (MonadAsync m)
                       => DNSLookupType
                       -> Tracer m DnsTrace
                       -> (IP -> PortNumber -> peerAddr)
                       -> DnsPeersKind
                       -> DNS.Domain
                       -> (   DNS.Domain
                           -> DNS.TYPE
                       -> m (Maybe (Either DNSError DNSMessage)))
                       -> StdGen
                       -> m (DNSLookupResult peerAddr)
srvRecordLookupWithTTL ofType tracer toPeerAddr peerType domainSRV resolveDNS rng = do
    reply <- resolveDNS domainSRV DNS.SRV
    case reply of
      Nothing          -> do
        traceWith tracer $ DnsTraceLookupError peerType Nothing domainSRV DNS.TimeoutExpired
        return . Left $ [DNS.TimeoutExpired]
      Just (Left  err) -> do
        traceWith tracer $ DnsTraceLookupError peerType Nothing domainSRV err
        return . Left $ [err]
      Just (Right msg) ->
        case DNS.fromDNSMessage msg selectSRV of
          Left err -> do
            traceWith tracer $ DnsTraceLookupError peerType Nothing domainSRV err
            return . Left $ [err]
          Right services -> do
            let srvByPriority = sortOn priority services
                grouped       = NE.groupWith priority srvByPriority
            (result, domain) <- do
              case listToMaybe grouped of
                Just topPriority ->
                  case topPriority of
                    (domain, _, _, port, ttl) NE.:| [] -> do -- fast path
                      result <- domainLookupWithTTL tracer ofType domain peerType resolveDNS
                      let result' = ipsttlsWithPort port ttl <$> result
                      return (result', domain)
                    many -> -- general path
                      runWeightedLookup many
                Nothing -> return (Right [], "")
            case result of
              Left {} -> traceWith tracer $ DnsSRVFail peerType domainSRV
              Right ipsttls ->
                traceWith tracer $ DnsResult peerType domain (Just domainSRV) ipsttls
            return $ map (\(ip, port, ttl) -> (toPeerAddr ip port, ttl)) <$> result

      where
        ipsttlsWithPort port ttl = map (\(ip, _ttl) -> (ip, fromIntegral port, ttl))
        runWeightedLookup :: NonEmpty (DNS.Domain, Word16, Word16, Word16, DNS.TTL)
                          -> m (Either [DNSError] [(IP, PortNumber, DNS.TTL)], DNS.Domain)
        runWeightedLookup services =
           let (upperBound, cdf) = Fold.foldl' aggregate (0, []) services
               mapCdf = Map.fromList cdf
               (pick, _) = randomR (0, upperBound) rng
               (domain, _, _, port, ttl) = snd . fromJust $ Map.lookupGE pick mapCdf
           in (,domain) . fmap (ipsttlsWithPort port ttl) <$> domainLookupWithTTL tracer ofType domain peerType resolveDNS

        aggregate (!upper, cdf) srv =
          let upper' = weight srv + upper
           in (upper', (upper', srv):cdf)

        selectSRV DNS.DNSMessage { DNS.answer } =
          [ (domain', priority', weight', port, ttl)
          | DNS.ResourceRecord {
              DNS.rdata = DNS.RD_SRV priority' weight' port domain',
              DNS.rrttl = ttl
            } <- answer
          ]

        weight   (_, _, w, _, _) = w
        priority (_, p, _, _, _) = p

dispatchLookupWithTTL :: (MonadAsync m)
                      => DNSLookupType
                      -> (   resolver
                          -> resolvConf
                          -> DNS.Domain
                          -> DNS.TYPE
                          -> m (Maybe (Either DNSError DNSMessage)))
                      -> Tracer m DnsTrace
                      -> (IP -> PortNumber -> peerAddr)
                      -> DnsPeersKind
                      -> RelayAccessPoint
                      -> resolvConf
                      -> resolver
                      -> StdGen
                      -> m (DNSLookupResult peerAddr)
dispatchLookupWithTTL lookupType mkResolveDNS tracer toPeerAddr peerType domain conf resolver rng =
  let resolveDNS = mkResolveDNS resolver conf
  in case domain of
    RelayAccessDomain d p -> do
      result <- domainLookupWithTTL tracer lookupType d peerType resolveDNS
      let trace = map (\(ip, ttl) -> (ip, p, ttl)) <$> result
      Fold.traverse_ (traceWith tracer . DnsResult peerType d Nothing) trace
      return $ map (\(ip, _ttl) -> (toPeerAddr ip p, _ttl)) <$> result
    RelayAccessSRVDomain d -> srvRecordLookupWithTTL lookupType tracer toPeerAddr peerType d resolveDNS rng
    RelayAccessAddress addr p  -> return . Right $ [(toPeerAddr addr p, maxBound)]

domainLookupWithTTL :: (MonadAsync m)
                    => Tracer m DnsTrace
                    -> DNSLookupType
                    -> DNS.Domain
                    -> DnsPeersKind
                    -> (   DNS.Domain
                        -> DNS.TYPE
                    -> m (Maybe (Either DNSError DNSMessage)))
                    -> m (Either [DNSError] [(IP, DNS.TTL)])
domainLookupWithTTL tracer look@LookupReqAOnly d peerType resolveDNS = do
    res <- domainALookupWithTTL (resolveDNS d DNS.A)
    case res of
         Left err -> do
           traceWith tracer $ DnsTraceLookupError peerType (Just look) d err
           return . Left $ [err]
         Right r  -> return . Right $ r

domainLookupWithTTL tracer look@LookupReqAAAAOnly d peerType resolveDNS = do
    res <- domainAAAALookupWithTTL (resolveDNS d DNS.AAAA)
    case res of
         Left err -> do
           traceWith tracer $ DnsTraceLookupError peerType (Just look) d err
           return . Left $ [err] --([err], [])
         Right r  -> return . Right $ r

domainLookupWithTTL tracer LookupReqAAndAAAA d peerType resolveDNS = do
    (r_ipv6, r_ipv4) <- concurrently (domainAAAALookupWithTTL (resolveDNS d DNS.AAAA))
                                     (domainALookupWithTTL (resolveDNS d DNS.A))
    case (r_ipv6, r_ipv4) of
         (Left  e6, Left  e4) -> do
           traceWith tracer $ DnsTraceLookupError
                                peerType (Just LookupReqAOnly) d e4
           traceWith tracer $ DnsTraceLookupError
                                peerType (Just LookupReqAAAAOnly) d e6
           return . Left $ [e6, e4]
         (Right r6, Left  e4) -> do
           traceWith tracer $ DnsTraceLookupError
                                peerType (Just LookupReqAOnly) d e4
           return . Right $ r6
         (Left  e6, Right r4) -> do
           traceWith tracer $ DnsTraceLookupError
                                peerType (Just LookupReqAAAAOnly) d e6
           return . Right $ r4
         (Right r6, Right r4) -> return . Right $ r6 <> r4

-- | Like 'DNS.lookupA' but also return the TTL for the results.
--
-- DNS library timeouts do not work reliably on Windows (#1873), hence the
-- additional timeout.
--
domainALookupWithTTL :: (Monad m)
               => m (Maybe (Either DNSError DNSMessage))
               -> m (Either DNS.DNSError [(IP, DNS.TTL)])
domainALookupWithTTL action = do
    reply <- action
    case reply of
      Nothing          -> return (Left DNS.TimeoutExpired)
      Just (Left  err) -> return (Left err)
      Just (Right ans) -> return (DNS.fromDNSMessage ans selectA)
      --TODO: we can get the SOA TTL on NXDOMAIN here if we want to
  where
    selectA DNS.DNSMessage { DNS.answer } =
      [ (IPv4 addr, fixupTTL ttl)
      | DNS.ResourceRecord {
          DNS.rdata = DNS.RD_A addr,
          DNS.rrttl = ttl
        } <- answer
      ]


domainAAAALookupWithTTL :: (Monad m)
                  => m (Maybe (Either DNSError DNSMessage))
                  -> m (Either DNS.DNSError [(IP, DNS.TTL)])
domainAAAALookupWithTTL action = do
    reply <- action
    case reply of
      Nothing          -> return (Left DNS.TimeoutExpired)
      Just (Left  err) -> return (Left err)
      Just (Right ans) -> return (DNS.fromDNSMessage ans selectAAAA)
      --TODO: we can get the SOA TTL on NXDOMAIN here if we want to
  where
    selectAAAA DNS.DNSMessage { DNS.answer } =
      [ (IPv6 addr, fixupTTL ttl)
      | DNS.ResourceRecord {
          DNS.rdata = DNS.RD_AAAA addr,
          DNS.rrttl = ttl
        } <- answer
      ]

--
-- Utils
--


fixupTTL :: DNS.TTL -> DNS.TTL
fixupTTL 0 = maxBound
fixupTTL a = a `max` 30
