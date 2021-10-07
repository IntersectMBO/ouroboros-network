{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
  (
    -- * DNS based actions for local and public root providers
    DNSActions (..),

    -- * DNSActions IO
    ioDNSActions,

    -- * Utils
    -- ** Resource
    Resource (..),
    withResource',
    constantResource,

    -- ** Error type
    DNSorIOError (..)
  )
  where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

import           Control.Exception (IOException)
#if !defined(mingw32_HOST_OS)
import           Control.Monad.Class.MonadSTM.Strict
#endif
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer(..), traceWith)

import           System.Directory (getModificationTime)

import           Data.IP (IPv4)
import           Network.DNS (DNSError)
import qualified Network.DNS as DNS


data DNSorIOError exception
    = DNSError !DNSError
    | IOError  !exception
  deriving Show

instance Exception exception => Exception (DNSorIOError exception) where

-----------------------------------------------
-- Resource
--

-- | Evolving resource; We use it to reinitialise the dns library if the
-- `/etc/resolv.conf` file was modified.
--
newtype Resource m err a = Resource {
    withResource :: m (Either err a, Resource m err a)
  }

-- | Like 'withResource' but retries untill success.
--
withResource' :: MonadDelay m
              => Tracer m err
              -> NonEmpty DiffTime
              -- ^ delays between each re-try
              -> Resource m err a
              -> m (a, Resource m err a)
withResource' tracer = go
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

constantResource :: Applicative m => a -> Resource m err a
constantResource a = Resource (pure (Right a, constantResource a))

-- | Strict version of 'Maybe' adjusted to the needs ot
-- 'asyncResolverResource'.
--
data TimedResolver
    = TimedResolver !DNS.Resolver !UTCTime
    | NoResolver

-- | Dictionary of DNS actions vocabulary
--
data DNSActions resolver exception m = DNSActions {

    -- |
    --
    -- TODO: it could be useful for `publicRootPeersProvider`.
    --
    dnsResolverResource      :: DNS.ResolvConf
                             -> m (Resource m (DNSorIOError exception) resolver),

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
                             -> m (Resource m (DNSorIOError exception) resolver),

    -- | Like 'DNS.lookupA' but also return the TTL for the results.
    --
    -- DNS library timeouts do not work reliably on Windows (#1873), hence the
    -- additional timeout.
    --
    dnsLookupAWithTTL        :: DNS.ResolvConf
                             -> resolver
                             -> DNS.Domain
                             -> m (Either DNS.DNSError [(IPv4, DNS.TTL)])
  }




-- |
--
-- TODO: it could be useful for `publicRootPeersProvider`.
--
resolverResource :: DNS.ResolvConf
                 -> IO (Resource IO (DNSorIOError IOException) DNS.Resolver)
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
                  ( Either (DNSorIOError IOException) DNS.Resolver
                  , Resource IO (DNSorIOError IOException) DNS.Resolver)]
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
       -> Resource IO (DNSorIOError IOException) DNS.Resolver
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
-- On /Windows/ returns newly intiatialised 'DNS.Resolver' at each step;  This
-- is because on /Windows/ we don't have a way to check that the network
-- configuration has changed.  The 'dns' library is using 'GetNetworkParams@
-- win32 api call to get the list of default dns servers.
asyncResolverResource :: DNS.ResolvConf
                      -> IO (Resource IO (DNSorIOError IOException)
                                         DNS.Resolver)
#if !defined(mingw32_HOST_OS)
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
                  ( Either (DNSorIOError IOException) DNS.Resolver
                  , Resource IO (DNSorIOError IOException) DNS.Resolver)]
    handlers filePath resourceVar =
      [ Handler $
          \(err :: IOException) ->
            pure (Left (IOError err), go filePath resourceVar)
      , Handler $
          \(err :: DNS.DNSError) ->
            pure (Left (DNSError err), go filePath resourceVar)
      ]

    go :: FilePath -> StrictTVar IO TimedResolver
       -> Resource IO (DNSorIOError IOException) DNS.Resolver
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
#else
asyncResolverResource resolvConf = return go
    where
      go = Resource $
        do
          rs <- DNS.makeResolvSeed resolvConf
          DNS.withResolver rs $ \resolver -> pure (Right resolver, go)
        `catches` handlers

      handlers :: [Handler IO
                    ( Either (DNSorIOError IOException) DNS.Resolver
                    , Resource IO (DNSorIOError IOException) DNS.Resolver)]
      handlers =
        [ Handler $
            \(err :: IOException) ->
              pure (Left (IOError err), go)
        , Handler $
            \(err :: DNS.DNSError) ->
              pure (Left (DNSError err), go)
        ]
#endif

-- | Like 'DNS.lookupA' but also return the TTL for the results.
--
-- DNS library timeouts do not work reliably on Windows (#1873), hence the
-- additional timeout.
--
lookupAWithTTL :: DNS.ResolvConf
               -> DNS.Resolver
               -> DNS.Domain
               -> IO (Either DNS.DNSError [(IPv4, DNS.TTL)])
lookupAWithTTL resolvConf resolver domain = do
    reply <- timeout (microsecondsAsIntToDiffTime
                      $ DNS.resolvTimeout resolvConf)
                    (DNS.lookupRaw resolver domain DNS.A)
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

-- | Bundle of DNS Actions that runs in IO
--
ioDNSActions :: DNSActions DNS.Resolver IOException IO
ioDNSActions = DNSActions {
    dnsResolverResource      = resolverResource,
    dnsAsyncResolverResource = asyncResolverResource,
    dnsLookupAWithTTL        = lookupAWithTTL
  }

