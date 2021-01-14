{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{- Partial implementation of RFC8305, https://tools.ietf.org/html/rfc8305 .
 - Prioritization of destination addresses doesn't implement longest prefix matching
 - and doesn't take address scope etc. into account.
 -}

module Ouroboros.Network.Subscription.Dns
    ( DnsSubscriptionTarget (..)
    , Resolver (..)
    , DnsSubscriptionParams
    , dnsSubscriptionWorker'
    , dnsSubscriptionWorker
    , dnsResolve
    , resolutionDelay

      -- * Traces
    , SubscriptionTrace (..)
    , DnsTrace (..)
    , ErrorPolicyTrace (..)
    , WithDomainName (..)
    , WithAddr (..)
    ) where

import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as Lazy
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import qualified Data.IP as IP
import           Data.Void (Void)
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Text.Printf
import           Data.Maybe (isJust)

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Subscription.Worker
import           Ouroboros.Network.Snocket (Snocket)
import           Ouroboros.Network.Socket


-- | Time to wait for an AAAA response after receiving an A response.
resolutionDelay :: DiffTime
resolutionDelay = 0.05 -- 50ms delay


data DnsSubscriptionTarget = DnsSubscriptionTarget {
      dstDomain :: !DNS.Domain
    , dstPort   :: !Socket.PortNumber
    , dstValency :: !Int
    } deriving (Eq, Show)


data Resolver m = Resolver {
      lookupA    :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    , lookupAAAA :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    }

withResolver :: Socket.PortNumber -> DNS.ResolvSeed -> (Resolver IO -> IO a) -> IO a
withResolver port rs k = do
    DNS.withResolver rs $ \dnsResolver ->
        k (Resolver
             (ipv4ToSockAddr dnsResolver)
             (ipv6ToSockAddr dnsResolver))
  where
    ipv4ToSockAddr dnsResolver d = do
        r <- DNS.lookupA dnsResolver d
        case r of
             (Right ips) -> return $ Right $ map (Socket.SockAddrInet port .
                                                  IP.toHostAddress) ips
             (Left e)    -> return $ Left e

    ipv6ToSockAddr dnsResolver d = do
        r <- DNS.lookupAAAA dnsResolver d
        case r of
             (Right ips) -> return $ Right $ map (\ip -> Socket.SockAddrInet6 port 0 (IP.toHostAddress6 ip) 0) ips
             (Left e)    -> return $ Left e


dnsResolve :: forall a m s.
     ( MonadAsync m
     , MonadCatch m
     , MonadTime  m
     , MonadTimer m
     )
    => Tracer m DnsTrace
    -> m a
    -> (a -> (Resolver m -> m (SubscriptionTarget m Socket.SockAddr)) -> m (SubscriptionTarget m Socket.SockAddr))
    -> StrictTVar m s
    -> BeforeConnect m s Socket.SockAddr
    -> DnsSubscriptionTarget
    -> m (SubscriptionTarget m Socket.SockAddr)
dnsResolve tracer getSeed withResolverFn peerStatesVar beforeConnect (DnsSubscriptionTarget domain _ _) = do
    rs_e <- (Right <$> getSeed) `catches`
        [ Handler (\ (e :: DNS.DNSError) ->
            return (Left $ toException e) :: m (Either SomeException a))
        -- On windows getSeed fails with BadConfiguration if the network is down.
        , Handler (\ (e :: IOError) ->
            return (Left $ toException e) :: m (Either SomeException a))
        -- On OSX getSeed can fail with IOError if all network devices are down.
        ]
    case rs_e of
         Left e -> do
             traceWith tracer $ DnsTraceLookupException e
             return $ listSubscriptionTarget []

         Right rs -> do
             withResolverFn rs $ \resolver -> do
                 -- Though the DNS lib does have its own timeouts, these do not work
                 -- on Windows reliably so as a workaround we add an extra layer
                 -- of timeout on the outside.
                 -- TODO: Fix upstream dns lib.
                 --       On windows the aid_ipv6 and aid_ipv4 threads are leaked incase
                 --       of an exception in the main thread.
                 res <- timeout 20 $ do
                          aid_ipv6 <- async $ resolveAAAA resolver
                          aid_ipv4 <- async $ resolveA resolver aid_ipv6
                          rd_e <- waitEitherCatch aid_ipv6 aid_ipv4
                          handleResult aid_ipv6 aid_ipv4 rd_e
                 case res of
                   Nothing -> do
                     -- TODO: the thread timedout, we should trace it
                     return (SubscriptionTarget $ pure Nothing)
                   Just st ->
                     return st
  where
    handleResult :: Async m [Socket.SockAddr]
                 -> Async m [Socket.SockAddr]
                 -> Either
                      (Either SomeException [Socket.SockAddr])
                      (Either SomeException [Socket.SockAddr])
                 -> m (SubscriptionTarget m Socket.SockAddr)

    handleResult _ ipv4Rsps (Left (Left e_ipv6)) = do
        -- AAAA lookup finished first, but with an error.
        traceWith tracer $ DnsTraceLookupException e_ipv6
        return $ SubscriptionTarget $ listTargets (Right ipv4Rsps) (Left [])

    handleResult _ ipv4Rsps (Left (Right ipv6Res)) = do
        -- Try to use IPv6 result first.
        traceWith tracer DnsTraceLookupIPv6First
        return $ SubscriptionTarget $ listTargets (Left ipv6Res) (Right ipv4Rsps)

    handleResult ipv6Rsps _ (Right (Left e_ipv4)) = do
        -- A lookup finished first, but with an error.
        traceWith tracer $ DnsTraceLookupException e_ipv4
        return $ SubscriptionTarget $ listTargets (Right ipv6Rsps) (Left [])

    handleResult ipv6Rsps _ (Right (Right ipv4Res)) = do
        -- Try to use IPv4 result first.
        traceWith tracer DnsTraceLookupIPv4First
        return $ SubscriptionTarget $ listTargets (Left ipv4Res) (Right ipv6Rsps)


    -- | Returns a series of SockAddr, where the address family will alternate
    -- between IPv4 and IPv6 as soon as the corresponding lookup call has
    -- completed.
    --
    listTargets :: Either [Socket.SockAddr] (Async m [Socket.SockAddr])
                -> Either [Socket.SockAddr] (Async m [Socket.SockAddr])
                -> m (Maybe (Socket.SockAddr, SubscriptionTarget m Socket.SockAddr))

    -- No result left to try
    listTargets (Left []) (Left []) = return Nothing

    -- No results left to try for one family
    listTargets (Left []) ipvB = listTargets ipvB (Left [])

    -- Result for one address family
    listTargets (Left (addr : addrs)) ipvB = do
        b <- runBeforeConnect peerStatesVar beforeConnect addr
        if b
          then pure $ Just (addr, SubscriptionTarget (listTargets ipvB (Left addrs)))
          else listTargets ipvB (Left addrs)

    -- No result for either family yet.
    listTargets (Right _) (Right _) = error "Can't happen"

    -- Wait on the result for one family.
    listTargets (Right asyn) (Left []) = do
        result <- waitCatch asyn
        case result of
          Left e -> do
            traceWith tracer $ DnsTraceLookupException e
            listTargets (Left []) (Left [])
          Right addrs -> do
            listTargets (Left addrs) (Left [])

    -- Peek at the result for one family.
    listTargets (Right asyn) (Left a) = do
        result <- poll asyn
        case result of
          Just (Left e) -> do
            traceWith tracer $ DnsTraceLookupException e
            listTargets (Left []) (Left a)
          Just (Right addrs) -> listTargets (Left addrs) (Left a)
          Nothing -> listTargets (Left a) (Right asyn)

    resolveAAAA :: Resolver m
                -> m [Socket.SockAddr]
    resolveAAAA resolver = do
        r_e <- lookupAAAA resolver domain
        case r_e of
             Left e  -> do
                 traceWith tracer $ DnsTraceLookupAAAAError e
                 return []
             Right r -> do
                 traceWith tracer $ DnsTraceLookupAAAAResult r

                 -- XXX Addresses should be sorted here based on DeltaQueue.
                 return r

    resolveA :: Resolver m
             -> Async m [Socket.SockAddr]
             -> m [Socket.SockAddr]
    resolveA resolver aid_ipv6 = do
        r_e <- lookupA resolver domain
        case r_e of
             Left e  -> do
                 traceWith tracer $ DnsTraceLookupAError e
                 return []
             Right r -> do
                 traceWith tracer $ DnsTraceLookupAResult r

                 {- From RFC8305.
                  - If a positive A response is received first due to reordering, the client
                  - SHOULD wait a short time for the AAAA response to ensure that preference is
                  - given to IPv6.
                  -}
                 timeoutVar <- registerDelay resolutionDelay
                 atomically $ do
                     timedOut   <- Lazy.readTVar timeoutVar
                     ipv6Done <- pollSTM aid_ipv6
                     check (timedOut || isJust ipv6Done)

                 -- XXX Addresses should be sorted here based on DeltaQueue.
                 return r


dnsSubscriptionWorker'
    :: Snocket IO Socket.Socket Socket.SockAddr
    -> Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState Socket.SockAddr
    -> IO b
    -> (b -> (Resolver IO -> IO (SubscriptionTarget IO Socket.SockAddr))
          -> IO (SubscriptionTarget IO Socket.SockAddr))
    -> DnsSubscriptionParams a
    -> Main IO (PeerStates IO Socket.SockAddr) x
    -> (Socket.Socket -> IO a)
    -> IO x
dnsSubscriptionWorker' snocket subTracer dnsTracer errorPolicyTracer
                       networkState@NetworkMutableState { nmsPeerStates }
                       setupResolver resolver
                       SubscriptionParams { spLocalAddresses
                                          , spConnectionAttemptDelay
                                          , spSubscriptionTarget = dst
                                          , spErrorPolicies
                                          }
                       main k =
    subscriptionWorker snocket
                       (WithDomainName (dstDomain dst) `contramap` subTracer)
                       errorPolicyTracer
                       networkState
                       WorkerParams { wpLocalAddresses = spLocalAddresses
                                    , wpConnectionAttemptDelay = spConnectionAttemptDelay
                                    , wpSubscriptionTarget =
                                        dnsResolve
                                          (WithDomainName (dstDomain dst) `contramap` dnsTracer)
                                          setupResolver resolver nmsPeerStates beforeConnectTx dst
                                    , wpValency = dstValency dst
                                    , wpSelectAddress = selectSockAddr 
                                    }
                       spErrorPolicies
                       main
                       k


type DnsSubscriptionParams a = SubscriptionParams a DnsSubscriptionTarget

dnsSubscriptionWorker
    :: Snocket IO Socket.Socket Socket.SockAddr
    -> Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
    -> Tracer IO (WithDomainName DnsTrace)
    -> Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    -> NetworkMutableState Socket.SockAddr
    -> DnsSubscriptionParams a
    -> (Socket.Socket -> IO a)
    -> IO Void
dnsSubscriptionWorker snocket subTracer dnsTracer errTrace networkState
                      params@SubscriptionParams { spSubscriptionTarget } k =
   dnsSubscriptionWorker'
       snocket
       subTracer dnsTracer errTrace
       networkState
       (DNS.makeResolvSeed DNS.defaultResolvConf)
       (withResolver (dstPort spSubscriptionTarget)) 
       params
       mainTx
       k

data WithDomainName a = WithDomainName {
      wdnDomain :: !DNS.Domain
    , wdnEvent  :: !a
    }

instance Show a => Show (WithDomainName a) where
    show WithDomainName {wdnDomain, wdnEvent} = printf  "Domain: %s %s" (show wdnDomain) (show wdnEvent)

data DnsTrace =
      DnsTraceLookupException SomeException
    | DnsTraceLookupAError DNS.DNSError
    | DnsTraceLookupAAAAError DNS.DNSError
    | DnsTraceLookupIPv6First
    | DnsTraceLookupIPv4First
    | DnsTraceLookupAResult [Socket.SockAddr]
    | DnsTraceLookupAAAAResult [Socket.SockAddr]

instance Show DnsTrace where
    show (DnsTraceLookupException e)   = "lookup exception " ++ show e
    show (DnsTraceLookupAError e)      = "A lookup failed with " ++ show e
    show (DnsTraceLookupAAAAError e)   = "AAAA lookup failed with " ++ show e
    show DnsTraceLookupIPv4First       = "Returning IPv4 address first"
    show DnsTraceLookupIPv6First       = "Returning IPv6 address first"
    show (DnsTraceLookupAResult as)    = "Lookup A result: " ++ show as
    show (DnsTraceLookupAAAAResult as) = "Lookup AAAAA result: " ++ show as
