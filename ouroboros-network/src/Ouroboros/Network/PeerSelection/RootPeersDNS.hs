{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.RootPeersDNS (
    rootPeerSetProviderDns,
    TraceRootPeerSetDns(..),
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer(..), traceWith, showTracing, stdoutTracer)

-- for DNS provider
import           Data.Word (Word32)
import           Data.IP (IPv4)
import qualified Network.DNS as DNS

import           Ouroboros.Network.PeerSelection.Types


-------------------------------
-- DNS root peer set provider
--

data TraceRootPeerSetDns =
       TraceMonitoringDomains [DNS.Domain]
     | TraceWaitingForTTL DNS.Domain DiffTime
     | TraceLookupResult  DNS.Domain [(IPv4, DNS.TTL)]
     | TraceLookupFailure DNS.Domain DNS.DNSError
       --TODO: classify DNS errors, config error vs transitory
  deriving Show

-- |
--
-- This action typically runs indefinitely, but can terminate successfully in
-- corner cases where there is nothing to do.
--
rootPeerSetProviderDns :: Tracer IO TraceRootPeerSetDns
                       -> DNS.ResolvConf
                       -> TVar IO (Map DNS.Domain (Map IPv4 RootPeerInfo))
                       -> [DNS.Domain]
                       -> IO ()
rootPeerSetProviderDns tracer resolvConf rootPeersVar domains = do
    traceWith tracer (TraceMonitoringDomains domains)
    unless (null domains) $ do
      rs <- DNS.makeResolvSeed resolvConf
      DNS.withResolver rs $ \resolver ->
        withAsyncAll (map (monitorDomain resolver) domains) $ \asyncs ->
          waitAny asyncs >> return ()
  where
    rootPeerInfo = RootPeerInfo {-False-} True --TODO
    monitorDomain resolver domain =
        go 0
      where
        go :: DiffTime -> IO ()
        go !ttl = do
          when (ttl > 0) $ do
            traceWith tracer (TraceWaitingForTTL domain ttl)
            threadDelay ttl
          reply <- lookupAWithTTL resolver domain
          case reply of
            Left  err -> do
              traceWith tracer (TraceLookupFailure domain err)
              go (ttlForDnsError err ttl)

            Right results -> do
              traceWith tracer (TraceLookupResult domain results)
              atomically $ do
                rootPeers <- readTVar rootPeersVar
                let resultsMap :: Map IPv4 RootPeerInfo
                    resultsMap = Map.fromList [ (addr, rootPeerInfo)
                                              | (addr, _ttl) <- results ]
                    rootPeers' :: Map DNS.Domain (Map IPv4 RootPeerInfo)
                    rootPeers' = Map.insert domain resultsMap rootPeers

                -- Only overwrite if it changed:
                when (Map.lookup domain rootPeers /= Just resultsMap) $
                  writeTVar rootPeersVar rootPeers'

              go (ttlForResults results ttl)

    -- Policy for TTL for positive results
    ttlForResults :: [(IPv4, DNS.TTL)] -> DiffTime -> DiffTime

    -- This case says we have a successful reply but there is no answer.
    -- This covers for example non-existent TLDs since there is no authority
    -- to say that they should not exist.
    ttlForResults [] ttl = ttlForDnsError DNS.NameError ttl

    ttlForResults rs _   = clipTTLBelow
                         . clipTTLAbove
                         . (fromIntegral :: Word32 -> DiffTime)
                         $ maximum (map snd rs)

    -- Policy for TTL for negative results
    -- Cache negative response for 3hrs
    -- Otherwise, use exponential backoff, up to a limit
    ttlForDnsError :: DNS.DNSError -> DiffTime -> DiffTime
    ttlForDnsError DNS.NameError _ = 10800
    ttlForDnsError _           ttl = clipTTLAbove (ttl * 2 + 5)

    -- Limit insane TTL choices.
    clipTTLAbove, clipTTLBelow :: DiffTime -> DiffTime
    clipTTLBelow = max 60     -- between 1min
    clipTTLAbove = min 86400  -- and 24hrs


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


withAsyncAll :: [IO a] -> ([Async IO a] -> IO b) -> IO b
withAsyncAll xs0 action = go [] xs0
  where
    go as []     = action as
    go as (x:xs) = withAsync x (\a -> go (a:as) xs)

example :: [DNS.Domain] -> IO ()
example domains = do
    rootPeersVar <- newTVarM Map.empty
--    withAsync (observer rootPeersVar Map.empty) $ \_ ->
    (provider rootPeersVar)
  where
    provider rootPeersVar =
      rootPeerSetProviderDns
        (showTracing stdoutTracer)
        DNS.defaultResolvConf
        rootPeersVar
        domains

    observer :: (Eq a, Show a) => TVar IO a -> a -> IO ()
    observer var fingerprint = do
      x <- atomically $ do
        x <- readTVar var
        check (x /= fingerprint)
        return x
      traceWith (showTracing stdoutTracer) x
      observer var x

