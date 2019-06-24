{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Ouroboros.Network.Subscription.Dns
    ( DnsSubscriptionTarget (..)
    , Resolver (..)
    , dnsSubscriptionWorker'
    , dnsSubscriptionWorker
    , dnsResolve
    , resolutionDelay
    ) where

import           Control.Monad (forever, unless, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import           Data.Functor (void)
import qualified Data.IP as IP
import           Data.Word
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Text.Printf

import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Subscriber

-- | Time to wait for an AAAA response after receiving an A response.
resolutionDelay :: DiffTime
resolutionDelay = 0.05 -- 50ms delay

-- | Minimum time to wait between dns lookups
dnsRetryDelay :: DiffTime
dnsRetryDelay = 10 -- 10s delay

data DnsSubscriptionTarget = DnsSubscriptionTarget {
      dstDomain :: !DNS.Domain
    , dstPort   :: !Word16
    , dstValeny :: !Word16
    } deriving (Eq, Show)

data Resolver m = Resolver {
      lookupA    :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    , lookupAAAA :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    }

dnsResolve :: forall m.
     ( MonadAsync m
     , MonadSay   m
     , MonadSTM   m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     )
    => Tracer m DnsTrace
    -> Resolver m
    -> DnsSubscriptionTarget
    -> m (SubscriptionTarget m Socket.SockAddr)
dnsResolve tracer resolver (DnsSubscriptionTarget domain _ _) = do
    ipv6Rsps <- newEmptyTMVarM
    ipv4Rsps <- newEmptyTMVarM
    gotIpv6Rsp <- newTVarM False

    aid_ipv6 <- async $ resolveAAAA gotIpv6Rsp ipv6Rsps
    aid_ipv4 <- async $ resolveA gotIpv6Rsp ipv4Rsps

    rd_e <- waitEitherCatch aid_ipv6 aid_ipv4
    case rd_e of
         Left r_ipv6 -> -- AAAA lookup finished first
             case r_ipv6 of
                  Left e_ipv6 -> do -- AAAA lookup failed
                      traceWith tracer $ DnsTraceLookupException e_ipv6
                      return $ SubscriptionTarget $ listTargets (Right ipv4Rsps) (Left [])
                  Right _ -> do
                      -- Try to use IPv6 result first.
                      traceWith tracer DnsTraceLookupIPv6First
                      ipv6Res <- atomically $ takeTMVar ipv6Rsps
                      return $ SubscriptionTarget $ listTargets (Left ipv6Res) (Right ipv4Rsps)
         Right r_ipv4 ->
              case r_ipv4 of
                  Left e_ipv4 -> do -- A lookup failed
                      traceWith tracer $ DnsTraceLookupException e_ipv4
                      return $ SubscriptionTarget $ listTargets (Right ipv6Rsps) (Left [])
                  Right _ -> do
                      traceWith tracer DnsTraceLookupIPv4First
                      return $ SubscriptionTarget $ listTargets (Right ipv4Rsps) (Right ipv6Rsps)

  where

    {-
     - Returns a series of SockAddr, where the address family will alternate
     - between IPv4 and IPv6 as soon as the corresponding lookup call has completed.
     -}
    listTargets :: Either [Socket.SockAddr] (TMVar m [Socket.SockAddr])
                -> Either [Socket.SockAddr] (TMVar m [Socket.SockAddr])
                -> m (Maybe (Socket.SockAddr, SubscriptionTarget m Socket.SockAddr))

    -- No result left to try
    listTargets (Left []) (Left []) = return Nothing

    -- No results left to try for one family
    listTargets (Left []) ipvB = listTargets ipvB (Left [])

    -- Result for one address family
    listTargets (Left (addr : addrs)) ipvB =
        return $ Just (addr, SubscriptionTarget (listTargets ipvB (Left addrs)))

    -- No result for either family yet.
    listTargets (Right addrsVarA) (Right addrsVarB) = do
        addrsRes <- atomically $ do
            a_m <- tryReadTMVar addrsVarA
            b_m <- tryReadTMVar addrsVarB
            case (a_m, b_m) of
                 (Nothing, Nothing) -> retry
                 (Just a, _)        -> return $ Left a
                 (_, Just b)        -> return $ Right b
        let (addrs, nextAddrs) = case addrsRes of
                                      Left a  -> (a, Right addrsVarB)
                                      Right a -> (a, Right addrsVarA)
        if null addrs
           then listTargets (Right addrsVarB) (Left [])
           else return $ Just (head addrs, SubscriptionTarget (listTargets nextAddrs (Left $ tail addrs)))

    -- Wait on the result for one family.
    listTargets (Right addrsVar) (Left []) = do
        addrs <- atomically $ takeTMVar addrsVar
        listTargets (Left addrs) (Left [])

    -- Peek at the result for one family.
    listTargets (Right addrsVar) (Left a) = do
        addrs_m <- atomically $ tryTakeTMVar addrsVar
        case addrs_m of
             Just addrs -> listTargets (Left addrs) (Left a)
             Nothing    -> listTargets (Left a) (Right addrsVar)

    resolveAAAA gotIpv6RspVar rspsVar = do
        r_e <- lookupAAAA resolver domain
        case r_e of
             Left e  -> do
                 atomically $ putTMVar rspsVar []
                 atomically $ writeTVar gotIpv6RspVar True
                 traceWith tracer $ DnsTraceLookupAAAAError e
                 return $ Just e
             Right r -> do
                 traceWith tracer $ DnsTraceLookupAAAAResult r

                 -- XXX Addresses should be sorted here based on DeltaQueue.
                 atomically $ putTMVar rspsVar r
                 atomically $ writeTVar gotIpv6RspVar True
                 return Nothing

    resolveA :: TVar m Bool -> TMVar m [Socket.SockAddr] -> m (Maybe DNS.DNSError)
    resolveA gotIpv6RspVar rspsVar= do
        r_e <- lookupA resolver domain
        case r_e of
             Left e  -> do
                 atomically $ putTMVar rspsVar []
                 traceWith tracer $ DnsTraceLookupAError e
                 return $ Just e
             Right r -> do
                 traceWith tracer $ DnsTraceLookupAResult r

                 {- From RFC8305.
                  - If a positive A response is received first due to reordering, the client
                  - SHOULD wait a short time for the AAAA response to ensure that preference is
                  - given to IPv6.
                  -}
                 timeoutVar <- registerDelay resolutionDelay
                 atomically $ do
                     timeout <- readTVar timeoutVar
                     gotIpv6Rsp <- readTVar gotIpv6RspVar
                     unless (timeout || gotIpv6Rsp) retry

                 -- XXX Addresses should be sorted here based on DeltaQueue.
                 atomically $ putTMVar rspsVar r
                 return Nothing

dnsSubscriptionWorker'
    :: Tracer IO (WithDomainName SubscriptionTrace)
    -> Tracer IO (WithDomainName DnsTrace)
    -> Resolver IO
    -> Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> DnsSubscriptionTarget
    -> (Socket.Socket -> IO ())
    -> IO ()
dnsSubscriptionWorker' subTracer dnsTracer resolver localPort connectionAttemptDelay dst k = do
        valencyVar <- newTVarM (dstValeny dst)
        let subTracer' = domainNameTracer (dstDomain dst) subTracer
        let dnsTracer' = domainNameTracer (dstDomain dst) dnsTracer
        traceWith dnsTracer' $ DnsTraceStart (dstValeny dst)
        forever $ do
            start <- getMonotonicTime
            targets <- dnsResolve dnsTracer' resolver dst
            subscribeTo subTracer' localPort connectionAttemptDelay valencyVar k targets
            atomically $ do
                v <- readTVar valencyVar
                when (v == 0)
                    retry
            end <- getMonotonicTime

            -- We allways wait at least dnsRetryDelay seconds between calls to dnsResolve, and
            -- before trying resovlve the domain name and restart the subscriptions we
            -- also wait 1 second so that if multiple subscription targets fail around
            -- the same time we will try to restart with a valency higher than 1.
            threadDelay 1
            let duration = diffTime end start
            valency <- atomically $ readTVar valencyVar
            traceWith dnsTracer' $ DnsTraceRestart duration (dstValeny dst)
                (dstValeny dst - valency)
            when (duration < dnsRetryDelay) $
                threadDelay $ dnsRetryDelay - duration

dnsSubscriptionWorker
    :: Tracer IO (WithDomainName SubscriptionTrace)
    -> Tracer IO (WithDomainName DnsTrace)
    -> Socket.PortNumber
    -> (Socket.SockAddr -> Maybe DiffTime)
    -> [DnsSubscriptionTarget]
    -> (Socket.Socket -> IO ())
    -> IO ()
dnsSubscriptionWorker subTracer dnsTracer localPort connectionAttemptDelay dsts k = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf

    void $ DNS.withResolver rs $ \dnsResolver ->
        mask $ \unmask -> do
            workers <- traverse (async . unmask . spawnResolver dnsResolver) dsts
            unmask (void $ waitAnyCancel workers)
  where
    spawnResolver :: DNS.Resolver -> DnsSubscriptionTarget -> IO ()
    spawnResolver dnsResolver dst =
        let resolver = Resolver (ipv4ToSockAddr (dstPort dst) dnsResolver)
                                (ipv6ToSockAddr (dstPort dst) dnsResolver) in
        dnsSubscriptionWorker' subTracer dnsTracer resolver localPort connectionAttemptDelay dst k

    ipv4ToSockAddr port dnsResolver dst = do
        r <- DNS.lookupA dnsResolver dst
        case r of
             (Right ips) -> return $ Right $ map (Socket.SockAddrInet (fromIntegral port) .
                                                  IP.toHostAddress) ips
             (Left e)    -> return $ Left e

    ipv6ToSockAddr port dnsResolver dst = do
        r <- DNS.lookupAAAA dnsResolver dst
        case r of
             (Right ips) -> return $ Right $ map (\ip -> Socket.SockAddrInet6 (fromIntegral port) 0 (IP.toHostAddress6 ip) 0) ips
             (Left e)    -> return $ Left e

data WithDomainName a = WithDomainName {
      wdnDomain :: !DNS.Domain
    , wdnEvent  :: !a
    }

instance (Show a) => Show (WithDomainName a) where
    show WithDomainName {..} = printf  "Domain: %s %s" (show wdnDomain) (show wdnEvent)

domainNameTracer :: DNS.Domain -> Tracer IO (WithDomainName a) -> Tracer IO a
domainNameTracer domain tr = Tracer $ \s -> traceWith tr $ WithDomainName domain s

data DnsTrace =
      DnsTraceLookupException SomeException
    | DnsTraceLookupAError DNS.DNSError
    | DnsTraceLookupAAAAError DNS.DNSError
    | DnsTraceLookupIPv6First
    | DnsTraceLookupIPv4First
    | DnsTraceLookupAResult [Socket.SockAddr]
    | DnsTraceLookupAAAAResult [Socket.SockAddr]
    | DnsTraceStart Word16
    | DnsTraceRestart DiffTime Word16 Word16

instance Show DnsTrace where
    show (DnsTraceLookupException e) = "lookup exception " ++ show e
    show (DnsTraceLookupAError e) = "A lookup failed with " ++ show e
    show (DnsTraceLookupAAAAError e) = "AAAA lookup failed with " ++ show e
    show DnsTraceLookupIPv4First = "Returning IPv4 address first"
    show DnsTraceLookupIPv6First = "Returning IPv6 address first"
    show (DnsTraceLookupAResult as) = "Lookup A result: " ++ show as
    show (DnsTraceLookupAAAAResult as) = "Lookup AAAAA result: " ++ show as
    show (DnsTraceStart val) = "Starting Dns Subscription Worker, valency " ++ show val
    show (DnsTraceRestart duration desiredVal currentVal) =
        "Restarting Dns Subscription after " ++ show duration ++ " desired valency " ++
        show desiredVal ++ " current valency " ++ show currentVal

