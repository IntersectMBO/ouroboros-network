{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
  ( -- * DNS based actions for local and public root providers
    DNSActions (..)
    -- * DNSActions IO
  , ioDNSActions
  , DNSLookupType (..)
    -- * Utils
    -- ** Resource
  , Resource (..)
  , retryResource
  , constantResource
    -- ** Exposed for testing purposes
  , srvRecordLookupWithTTL
  , dispatchLookupWithTTL
    -- ** Error type
  , DNSorIOError (..)
  , DNSLookupResult (..)
  ) where

import Data.Word (Word16)
import Data.Function (fix)
import Data.List (sortOn, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import System.Random

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

import Data.Map qualified as Map
import Network.DNS (DNSError, DNSMessage)
import Network.DNS qualified as DNS

import Ouroboros.Network.PeerSelection.RelayAccessPoint

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
data DNSLookupResult peeraddr =
    DNSLookup ( DomainPlainAccessPoint
              , [DNSError]
              ,  [(peeraddr, DNS.TTL)])
  | DNSLookupSRV ( DomainSRVAccessPoint
                 , [DNSError]
                 , Maybe ( DNS.Domain -- ^ selected service domain, useful for tracing
                         , PortNumber -- ^ service port
                         , [(peeraddr, DNS.TTL)]))
  deriving (Show)

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
data DNSActions resolver exception m = DNSActions {

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
    dnsLookupWithTTL         :: DomainAccessPoint
                             -> DNS.ResolvConf
                             -> resolver
                             -> StdGen
                             -> m (DNSLookupResult IP)
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
ioDNSActions :: DNSLookupType
             -> DNSActions DNS.Resolver IOException IO
ioDNSActions =
    \reqs -> DNSActions {
               dnsResolverResource      = resolverResource,
               dnsAsyncResolverResource = asyncResolverResource,
               dnsLookupWithTTL         = dispatchLookupWithTTL reqs mkIOAction4
             }
  where
    mkIOAction4 resolver resolvConf domain ofType =
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


srvRecordLookupWithTTL :: forall m. (MonadAsync m)
                 => DNSLookupType
                 -> DNS.Domain
                 -> (   DNS.Domain
                     -> DNS.TYPE
                     -> m (Maybe (Either DNSError DNSMessage)))
                 -> StdGen
                 -> m (DNSLookupResult IP)
srvRecordLookupWithTTL ofType domain0 mkAction2 rng = do
    reply <- mkAction2 domain0 DNS.SRV
    case reply of
      Nothing          -> return $ DNSLookupSRV (srvDomain, [DNS.TimeoutExpired], Nothing)
      Just (Left  err) -> return $ DNSLookupSRV (srvDomain, [err], Nothing)
      Just (Right msg) ->
        case DNS.fromDNSMessage msg selectSRV of
          Left err -> return $ DNSLookupSRV (srvDomain, [err], Nothing)
          Right services -> do
            let srvByPriority = sortOn priority services
                grouped       = NE.groupWith priority srvByPriority

            case listToMaybe grouped of
              Just topPriority -> do
                case topPriority of
                  (domain, _, _, port) NE.:| [] -> -- fast path
                      DNSLookupSRV
                    . annotateDomainAndPort domain port <$>
                        domainLookupWithTTL ofType (mkAction2 domain)
                  many -> -- general path
                    DNSLookupSRV <$> runWeightedLookup many
              Nothing ->
                -- this shouldn't happen in practice, and so should
                -- this be an error? It is convenient for some DNS tests
                -- to observe a DNS lookup attempt even though it will lead nowhere.
                return $ DNSLookupSRV (srvDomain, [], Nothing)

      where
        srvDomain = DomainSRV domain0
        annotateDomainAndPort domain !port (e, ipsttls) = (srvDomain, e, Just (domain, fromIntegral port, ipsttls))

        runWeightedLookup :: NonEmpty (DNS.Domain, Word16, Word16, Word16)
                          -> m (DomainSRVAccessPoint, [DNSError], Maybe (DNS.Domain, PortNumber, [(IP, DNS.TTL)]))
        runWeightedLookup services =
           let (upperBound, cdf) = foldl' aggregate (0, []) services
               mapCdf = Map.fromList cdf
               (pick, _) = randomR (0, upperBound) rng
               (domain, _, _, port) = snd . fromJust $ Map.lookupGE pick mapCdf
           in annotateDomainAndPort domain port <$>
                domainLookupWithTTL ofType (mkAction2 domain)

        aggregate (!upper, cdf) srv =
          let upper' = weight srv + upper
           in (upper', (upper', srv):cdf)

        selectSRV DNS.DNSMessage { DNS.answer } =
          [ (domain', priority', weight', port)
          | DNS.ResourceRecord {
              DNS.rdata = DNS.RD_SRV priority' weight' port domain'
            } <- answer
          ]

        weight   (_, _, w, _) = w
        priority (_, p, _, _) = p

dispatchLookupWithTTL :: (MonadAsync m)
                      => DNSLookupType
                      -> (   resolver
                          -> resolvConf
                          -> DNS.Domain
                          -> DNS.TYPE
                          -> m (Maybe (Either DNSError DNSMessage)))
                      -> DomainAccessPoint
                      -> resolvConf
                      -> resolver
                      -> StdGen
                      -> m (DNSLookupResult IP)
dispatchLookupWithTTL lookupType mkAction4 domain conf resolver rng =
  let mkAction2 = mkAction4 resolver conf
  in case domain of
    DomainAccessPoint d -> push <$> domainLookupWithTTL lookupType (mkAction2 dapDomain)
     where
       DomainPlain { dapDomain } = d
       push (a, b) = DNSLookup (d, a, b)
    DomainSRVAccessPoint d -> srvRecordLookupWithTTL lookupType (srvDomain d) mkAction2 rng

domainLookupWithTTL :: (MonadAsync m)
              => DNSLookupType
              -> (   DNS.TYPE
                  -> m (Maybe (Either DNSError DNSMessage)))
              -> m ([DNS.DNSError], [(IP, DNS.TTL)])
domainLookupWithTTL LookupReqAOnly action1 = do
    res <- domainALookupWithTTL (action1 DNS.A)
    case res of
         Left err -> return ([err], [])
         Right r  -> return ([], r)

domainLookupWithTTL LookupReqAAAAOnly action1 = do
    res <- domainAAAALookupWithTTL (action1 DNS.AAAA)
    case res of
         Left err -> return ([err], [])
         Right r  -> return ([], r)

domainLookupWithTTL LookupReqAAndAAAA action1 = do
    (r_ipv6, r_ipv4) <- concurrently (domainAAAALookupWithTTL (action1 DNS.AAAA))
                                     (domainALookupWithTTL (action1 DNS.A))
    case (r_ipv6, r_ipv4) of
         (Left  e6, Left  e4) -> return ([e6, e4], [])
         (Right r6, Left  e4) -> return ([e4], r6)
         (Left  e6, Right r4) -> return ([e6], r4)
         (Right r6, Right r4) -> return ([], r6 <> r4)

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
