{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS (
    -- * DNS based provider for local root peers
    localRootPeersProvider,
    TraceLocalRootPeers(..),

    -- * DNS based provider for public root peers
    publicRootPeersProvider,
    TracePublicRootPeers(..),

    -- * DNS type re-exports
    DNS.ResolvConf,
    DNS.Domain,
    DNS.TTL,
    IPv4,
  ) where

import           Data.Word (Word32)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer(..), traceWith)

import           Data.IP (IPv4)
import qualified Network.DNS as DNS

import           Ouroboros.Network.PeerSelection.Types


-----------------------------------------------
-- local root peer set provider based on DNS
--

data TraceLocalRootPeers =
       TraceLocalRootDomains [(DNS.Domain, PeerAdvertise)]
     | TraceLocalRootWaiting DNS.Domain DiffTime
     | TraceLocalRootResult  DNS.Domain [(IPv4, DNS.TTL)]
     | TraceLocalRootFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
-- This action typically runs indefinitely, but can terminate successfully in
-- corner cases where there is nothing to do.
--
localRootPeersProvider :: Tracer IO TraceLocalRootPeers
                       -> DNS.ResolvConf
                       -> TVar IO (Map DNS.Domain (Map IPv4 PeerAdvertise))
                       -> [(DNS.Domain, PeerAdvertise)]
                       -> IO ()
localRootPeersProvider tracer resolvConf rootPeersVar domains = do
    traceWith tracer (TraceLocalRootDomains domains)
    unless (null domains) $ do
      rs <- DNS.makeResolvSeed resolvConf
      DNS.withResolver rs $ \resolver ->
        withAsyncAll (map (monitorDomain resolver) domains) $ \asyncs ->
          waitAny asyncs >> return ()
  where
    monitorDomain resolver (domain, advertisePeer) =
        go 0
      where
        go :: DiffTime -> IO ()
        go !ttl = do
          when (ttl > 0) $ do
            traceWith tracer (TraceLocalRootWaiting domain ttl)
            threadDelay ttl
          reply <- lookupAWithTTL resolver domain
          case reply of
            Left  err -> do
              traceWith tracer (TraceLocalRootFailure domain err)
              go (ttlForDnsError err ttl)

            Right results -> do
              traceWith tracer (TraceLocalRootResult domain results)
              atomically $ do
                rootPeers <- readTVar rootPeersVar
                let resultsMap :: Map IPv4 PeerAdvertise
                    resultsMap = Map.fromList [ (addr, advertisePeer)
                                              | (addr, _ttl) <- results ]
                    rootPeers' :: Map DNS.Domain (Map IPv4 PeerAdvertise)
                    rootPeers' = Map.insert domain resultsMap rootPeers

                -- Only overwrite if it changed:
                when (Map.lookup domain rootPeers /= Just resultsMap) $
                  writeTVar rootPeersVar rootPeers'

              go (ttlForResults (map snd results))


---------------------------------------------
-- Public root peer set provider using DNS
--

data TracePublicRootPeers =
       TracePublicRootDomains [DNS.Domain]
     | TracePublicRootResult  DNS.Domain [(IPv4, DNS.TTL)]
     | TracePublicRootFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
publicRootPeersProvider :: Tracer IO TracePublicRootPeers
                        -> DNS.ResolvConf
                        -> [DNS.Domain]
                        -> ((Int -> IO (Set IPv4, DiffTime)) -> IO a)
                        -> IO a
publicRootPeersProvider tracer resolvConf domains action = do
    traceWith tracer (TracePublicRootDomains domains)
    rs <- DNS.makeResolvSeed resolvConf
    DNS.withResolver rs $ \resolver ->
      action (requestPublicRootPeers resolver)
  where
    requestPublicRootPeers :: DNS.Resolver -> Int -> IO (Set IPv4, DiffTime)
    requestPublicRootPeers resolver _numRequested = do
        let lookups = [ lookupAWithTTL resolver domain | domain <- domains ]
        -- The timeouts here are handled by the dns library. They're configured
        -- via the DNS.ResolvConf resolvTimeout field and defaults to 3 sec.
        results <- withAsyncAll lookups (atomically . mapM waitSTM)
        sequence_
          [ traceWith tracer $ case result of
              Left  dnserr -> TracePublicRootFailure domain dnserr
              Right ipttls -> TracePublicRootResult  domain ipttls
          | (domain, result) <- zip domains results ]
        let successes = [ ipttl | Right ipttls <- results, ipttl <- ipttls ]
            !ips      = Set.fromList  (map fst successes)
            !ttl      = ttlForResults (map snd successes)
        -- If all the lookups failed we'll return an empty set with a minimum
        -- TTL, and the governor will invoke its exponential backoff.
        return (ips, ttl)


---------------------------------------------
-- Shared utils
--

-- | Like 'DNS.lookupA' but also return the TTL for the results.
--
lookupAWithTTL :: DNS.Resolver
               -> DNS.Domain
               -> IO (Either DNS.DNSError [(IPv4, DNS.TTL)])
lookupAWithTTL resolver domain = do
    reply <- DNS.lookupRaw resolver domain DNS.A
    case reply of
      Left  err -> return (Left err)
      Right ans -> return (DNS.fromDNSMessage ans selectA)
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
exampleLocal :: [DNS.Domain] -> IO ()
exampleLocal domains = do
    rootPeersVar <- newTVarM Map.empty
    withAsync (observer rootPeersVar Map.empty) $ \_ ->
      provider rootPeersVar
  where
    provider rootPeersVar =
      localRootPeersProvider
        (showTracing stdoutTracer)
        DNS.defaultResolvConf
        rootPeersVar
        (map (\d -> (d, DoAdvertisePeer)) domains)

    observer :: (Eq a, Show a) => TVar IO a -> a -> IO ()
    observer var fingerprint = do
      x <- atomically $ do
        x <- readTVar var
        check (x /= fingerprint)
        return x
      traceWith (showTracing stdoutTracer) x
      observer var x

examplePublic :: [DNS.Domain] -> IO ()
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
