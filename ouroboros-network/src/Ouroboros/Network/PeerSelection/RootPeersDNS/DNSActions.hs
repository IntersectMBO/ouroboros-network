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
  , LookupReqs (..)
    -- * Utils
    -- ** Resource
  , Resource (..)
  , retryResource
  , constantResource
    -- ** Error type
  , DNSorIOError (..)
  ) where

#if !defined(mingw32_HOST_OS)
import           Data.Function (fix)
#endif
import           Data.List.NonEmpty (NonEmpty (..))

import           Control.Exception (IOException)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.Trans ()

import           Control.Concurrent.Class.MonadSTM.Strict

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Monad.Except
import           Control.Tracer (Tracer (..), traceWith)

#if !defined(mingw32_HOST_OS)
import           System.Directory (getModificationTime)
#endif

import           Data.IP (IP (..))
import           Network.DNS (DNSError)
import qualified Network.DNS as DNS


data LookupReqs = LookupReqAOnly
                | LookupReqAAAAOnly
                | LookupReqAAndAAAA
                deriving Show

data DNSorIOError exception
    = DNSError !DNSError
    | IOError  !exception
  deriving Show

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
    dnsLookupWithTTL         :: DNS.ResolvConf
                             -> resolver
                             -> DNS.Domain
                             -> m ([DNS.DNSError], [(IP, DNS.TTL)])
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
ioDNSActions :: LookupReqs
             -> DNSActions DNS.Resolver IOException IO
ioDNSActions =
    \reqs -> DNSActions {
               dnsResolverResource      = resolverResource,
               dnsAsyncResolverResource = asyncResolverResource,
               dnsLookupWithTTL         = lookupWithTTL reqs
             }
  where
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


    -- | Like 'DNS.lookupA' but also return the TTL for the results.
    --
    -- DNS library timeouts do not work reliably on Windows (#1873), hence the
    -- additional timeout.
    --
    lookupAWithTTL :: DNS.ResolvConf
                   -> DNS.Resolver
                   -> DNS.Domain
                   -> IO (Either DNS.DNSError [(IP, DNS.TTL)])
    lookupAWithTTL resolvConf resolver domain = do
        reply <- timeout (microsecondsAsIntToDiffTime
                           $ DNS.resolvTimeout resolvConf)
                         (DNS.lookupRaw resolver domain DNS.A)
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


    lookupAAAAWithTTL :: DNS.ResolvConf
                      -> DNS.Resolver
                      -> DNS.Domain
                      -> IO (Either DNS.DNSError [(IP, DNS.TTL)])
    lookupAAAAWithTTL resolvConf resolver domain = do
        reply <- timeout (microsecondsAsIntToDiffTime
                           $ DNS.resolvTimeout resolvConf)
                         (DNS.lookupRaw resolver domain DNS.AAAA)
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


    lookupWithTTL :: LookupReqs
                  -> DNS.ResolvConf
                  -> DNS.Resolver
                  -> DNS.Domain
                  -> IO ([DNS.DNSError], [(IP, DNS.TTL)])
    lookupWithTTL LookupReqAOnly resolvConf resolver domain = do
        res <- lookupAWithTTL resolvConf resolver domain
        case res of
             Left err -> return ([err], [])
             Right r  -> return ([], r)

    lookupWithTTL LookupReqAAAAOnly resolvConf resolver domain = do
        res <- lookupAAAAWithTTL resolvConf resolver domain
        case res of
             Left err -> return ([err], [])
             Right r  -> return ([], r)

    lookupWithTTL LookupReqAAndAAAA resolvConf resolver domain = do
        (r_ipv6, r_ipv4) <- concurrently (lookupAAAAWithTTL resolvConf resolver domain)
                                         (lookupAWithTTL resolvConf resolver domain)
        case (r_ipv6, r_ipv4) of
             (Left  e6, Left  e4) -> return ([e6, e4], [])
             (Right r6, Left  e4) -> return ([e4], r6)
             (Left  e6, Right r4) -> return ([e6], r4)
             (Right r6, Right r4) -> return ([], r6 <> r4)


--
-- Utils
--


fixupTTL :: DNS.TTL -> DNS.TTL
fixupTTL 0 = maxBound
fixupTTL a = a `max` 30
