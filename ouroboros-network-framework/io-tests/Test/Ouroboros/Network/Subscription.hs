{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial   #-}
#endif

module Test.Ouroboros.Network.Subscription (tests) where

import Control.Concurrent hiding (threadDelay)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (replicateM, unless, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim (runSimStrictShutdown)
import Control.Tracer
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Functor (void)
import Data.IP qualified as IP
import Data.List qualified as L
import Data.Map qualified as M
import Data.Void (Void)
import Data.Word
import Network.DNS qualified as DNS
import Network.Socket qualified as Socket
#if !defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy qualified as Socket (recv, sendAll)
#endif

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
--TODO: time utils should come from elsewhere
import Network.Mux.Time (microsecondsToDiffTime)

import Network.TypedProtocol.ReqResp.Client qualified as ReqResp
import Network.TypedProtocol.ReqResp.Codec.CBOR qualified as ReqResp
import Network.TypedProtocol.ReqResp.Examples qualified as ReqResp
import Network.TypedProtocol.ReqResp.Server qualified as ReqResp

import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Unversioned
import Ouroboros.Network.Protocol.Handshake.Version

import Ouroboros.Network.Driver
import Ouroboros.Network.ErrorPolicy
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket

import Ouroboros.Network.Test.Orphans ()

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf
import Text.Show.Functions ()


defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

testProtocols1 :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
               -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
testProtocols1 chainSync =
    OuroborosApplication [
       MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = chainSync
      }
    ]

-- |
-- Allow to run a singly req-resp protocol.
--
testProtocols2 :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
               -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
testProtocols2 reqResp =
    OuroborosApplication [
       MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 4,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = reqResp
      }
    ]


activeTracer :: Tracer IO a
activeTracer = nullTracer
-- activeTracer = _verboseTracer -- Dump log messages to stdout.

--
-- The list of all tests
--

tests :: TestTree
tests =
    testGroup "Subscription"
        [
          testProperty "Resolve (Sim)"      prop_resolv_sim
        --, testProperty "Resolve (IO)"      _prop_resolv_io
        -- the above tests takes about 10 minutes to run due to delays in
        -- realtime.
        -- , testProperty "subscription demo" _demo
        ]

data LookupResult = LookupResult {
      lrIpv4Result  :: !(Either DNS.DNSError [Socket.SockAddr])
    , lrIpv4Delay   :: !DiffTime
    , lrIpv6Result  :: !(Either DNS.DNSError [Socket.SockAddr])
    , lrIpv6Delay   :: !DiffTime
    , connectionRtt :: !DiffTime
    }

data LookupResultIO = LookupResultIO {
      lrioIpv4Result :: !(Either DNS.DNSError [Word16])
    , lrioIpv6Result :: !(Either DNS.DNSError [Word16])
    , lrioFirst      :: !Socket.Family
    , lrioValency    :: !Int
    }

mockResolver :: forall m. MonadDelay m => LookupResult -> Resolver m
mockResolver lr = Resolver lA lAAAA
   where
    lA :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    lA    _ = do
        threadDelay (lrIpv4Delay lr)
        return $ lrIpv4Result lr

    lAAAA :: DNS.Domain -> m (Either DNS.DNSError [Socket.SockAddr])
    lAAAA _ = do
        threadDelay (lrIpv6Delay lr)
        return $ lrIpv6Result lr

withMockResolver :: MonadDelay m
                 => LookupResult
                 -> (Resolver m -> m a)
                 -> m a
withMockResolver lr k = k (mockResolver lr)


mockResolverIO :: StrictTMVar IO ()
               -> M.Map (Socket.Family, Word16) Socket.PortNumber
               -> LookupResultIO
               -> Resolver IO
mockResolverIO firstDoneMVar portMap lr = Resolver lA lAAAA
  where
    sidToPort sid =
        case M.lookup sid portMap of
             Just port -> port
             Nothing   -> error $ "missing port for sid " ++ show sid -- XXX

    lA :: DNS.Domain -> IO (Either DNS.DNSError [Socket.SockAddr])
    lA    _ = do
        when (lrioFirst lr == Socket.AF_INET6) $ do
            void $ atomically $ takeTMVar firstDoneMVar
            threadDelay 0.1
        let r = case lrioIpv4Result lr of
                     (Right sids) -> Right $ map (\sid -> Socket.SockAddrInet
                         (sidToPort (Socket.AF_INET, sid))
                         (IP.toHostAddress "127.0.0.1")) sids
                     (Left e)      -> Left e
        when (lrioFirst lr == Socket.AF_INET) $
            atomically $ putTMVar firstDoneMVar ()
        return r

    lAAAA :: DNS.Domain -> IO (Either DNS.DNSError [Socket.SockAddr])
    lAAAA _ = do
        when (lrioFirst lr == Socket.AF_INET) $ do
            void $ atomically $ takeTMVar firstDoneMVar
            threadDelay $ 0.1 + resolutionDelay
        let r = case lrioIpv6Result lr of
                     (Right sids) -> Right $ map (\sid ->
                         Socket.SockAddrInet6 (sidToPort (Socket.AF_INET6, sid)) 0
                                              (IP.toHostAddress6 "::1") 0) sids
                     (Left e)      -> Left e
        when (lrioFirst lr == Socket.AF_INET6) $
            atomically $ putTMVar firstDoneMVar ()
        return r

withMockResolverIO :: StrictTMVar IO ()
                   -> M.Map (Socket.Family, Word16) Socket.PortNumber
                   -> LookupResultIO
                   -> (Resolver IO -> IO a)
                   -> IO a
withMockResolverIO firstDoneMVar portMap lr k = k (mockResolverIO firstDoneMVar portMap lr)

instance Show LookupResult where
    show a = printf "LookupResult: ipv4: %s delay %s ipv6: %s delay %s rtt %s" (show $ lrIpv4Result a)
                    (show $ lrIpv4Delay a) (show $ lrIpv6Result a) (show $ lrIpv6Delay a)
                    (show $ connectionRtt a)

instance Show LookupResultIO where
    show a = printf "LookupResultIO: ipv4: %s ipv6: %s first %s valency %d"
                    (show $ lrioIpv4Result a)
                    (show $ lrioIpv6Result a)
                    (show $ lrioFirst a)
                    (lrioValency a)

instance Arbitrary DNS.DNSError where
    arbitrary = oneof [ return DNS.SequenceNumberMismatch
                      , return DNS.RetryLimitExceeded
                      ]

instance Arbitrary IP.IPv4 where
    arbitrary = do
        a <- replicateM 4 (choose (0,255))
        return $ IP.toIPv4 a

instance Arbitrary IP.IPv6 where
    arbitrary = do
        a <- replicateM 8 (choose (0,0xffff))
        return $ IP.toIPv6 a

instance Arbitrary Socket.Family where
    arbitrary = oneof [ return Socket.AF_INET
                      , return Socket.AF_INET6
                      ]

instance Arbitrary LookupResult where
    arbitrary = do
      ipv4r <- arbitrary :: Gen (Either DNS.DNSError [IP.IPv4])
      ipv4d <- choose (0, 3000)
      ipv6r <- arbitrary
      ipv6d <- oneof [ choose (0, 3000)
                     , choose (ipv4d, ipv4d + round (1000 * resolutionDelay))
                     ]
      conrtt <- choose (0, 250)

      let minDistance = 10 -- 10ms minimum time between IPv4 and IPv6 result.

      {-
       - For predictability we don't generate lookup results that are closer than 10ms to
       - each other. Since 10ms is still less than resolutionDelay we can still test that
       - behaviour related to resolutionDelay works correctly.
       -}
      let (ipv4d', ipv6d') = if abs (ipv4d - ipv6d) < minDistance
                                 then if ipv4d > ipv6d then (ipv4d + minDistance, ipv6d)
                                                       else (ipv4d, ipv6d + minDistance)
                                 else (ipv4d, ipv6d)
      let sa4s = case ipv4r of
                      (Right ips) -> Right $ map (Socket.SockAddrInet 1 . IP.toHostAddress) ips
                      (Left e)    -> Left e
      let sa6s = case ipv6r of
                      (Right ips) -> Right $ map (\ip -> Socket.SockAddrInet6 1 0
                                                 (IP.toHostAddress6 ip) 0) ips
                      (Left e)   -> Left e
      return $ LookupResult sa4s (microsecondsToDiffTime $ 1000 * ipv4d') sa6s
                            (microsecondsToDiffTime $ 1000 * ipv6d')
                            (microsecondsToDiffTime $ 1000 * conrtt)


instance Arbitrary LookupResultIO where
    arbitrary = do
        ipv4r <- oneof [ Left <$> arbitrary
                       , Right <$> shortList
                       ]
        ipv6r <- oneof [ Left <$> arbitrary
                       , Right <$> shortList
                       ]
        first <- arbitrary
        valency <- choose (1, 8)
        return $ LookupResultIO ipv4r ipv6r first valency
      where
        shortList :: Gen [Word16]
        shortList = do
            lx <- shuffle [0..3]
            k <- choose (0, 4)
            return $ take k lx

-- | Return true if  `a` is a permutation of `b`.
permCheck :: (Ord o, Show o) => [o] -> [o] -> Property
permCheck a b = L.sort a === L.sort b

--
-- Properties
--

prop_resolv :: forall m.
     ( MonadAsync m
     , MonadCatch m
     , MonadDelay m
     , MonadTimer m
     )
     => LookupResult
     -> m Property
prop_resolv lr =  do
    --say $ printf "%s" $ show lr
    peerStatesVar <- newTVarIO ()
    x <- dnsResolve nullTracer (return lr) withMockResolver peerStatesVar (\_ _ s -> pure (AllowConnection s)) $ DnsSubscriptionTarget "shelley-1.iohk.example" 1 2
    !res <- checkResult <$> extractResult x []

    {-
     - We wait 100ms here so that the resolveAAAA and resolveA thread have time to
     - exit, otherwise runSimStrictShutdown will complain about thread leaks.
     -
     - Change dnsResolv to return the two Asyncs so we can wait on them?
     -}
    threadDelay 0.1
    return $ tabulate "Resolution Result" [resolvLabel] res

  where
    checkResult :: [Socket.SockAddr] -> Property
    checkResult addrs =
        case (lrIpv4Result lr, lrIpv6Result lr) of
            (Left _, Left _)   -> property $ null addrs

            (Right [], Right [])   -> property $ null addrs

            (Right ea, Left _) ->
                -- Expect a permutation of the result of the A lookup.
                permCheck addrs ea

            (Left _, Right ea) ->
                -- Expect a permutation of the result of the AAAA lookup.
                permCheck addrs ea

            (Right sa4s, Right sa6s) ->
                let (cntA, cntB, headFamily) =
                        if sa4s /= [] && (lrIpv4Delay lr + resolutionDelay < lrIpv6Delay lr
                                        || null sa6s)
                            then (length sa4s, length sa6s, Socket.AF_INET)
                            else (length sa6s, length sa4s, Socket.AF_INET6) in
                permCheck addrs (sa4s ++ sa6s) .&&.
                        sockAddrFamily (head addrs) === headFamily .&&.
                        alternateFamily addrs (sockAddrFamily (head addrs)) True
                            cntA cntB

    -- Once both the A and the AAAA lookup has returned the result should
    -- alternate between the address families until one family is out of addresses.
    -- This means that:
    -- AAAABABABABABABBB is a valid sequense.
    -- AAAABABAAABABABBB is not a valid sequense.
    alternateFamily :: [Socket.SockAddr] -> Socket.Family -> Bool -> Int -> Int -> Bool
    alternateFamily []       _  _    _    _    = True
    alternateFamily _       _  _     (-1)  _   = False
    alternateFamily _       _  _     _    (-1) = False
    alternateFamily (sa:sas) fa True cntA cntB =
        if sockAddrFamily sa == fa
            then alternateFamily sas fa True (cntA - 1) cntB
            else alternateFamily sas (sockAddrFamily sa) False (cntB - 1) cntA
    alternateFamily (sa:sas) fa False cntA cntB =
        if sockAddrFamily sa == fa
            then (cntB == 0) && alternateFamily sas fa False (cntA - 1) cntB
            else alternateFamily sas (sockAddrFamily sa) False (cntB - 1) cntA

    extractResult :: SubscriptionTarget m Socket.SockAddr -> [Socket.SockAddr] -> m [Socket.SockAddr]
    extractResult targets addrs = do
        target_m <- getSubscriptionTarget targets
        case target_m of
             Just (addr, nextTargets) -> do
                 threadDelay (connectionRtt lr)
                 extractResult nextTargets (addr:addrs)
             Nothing -> return $ reverse addrs

    resolvLabel :: String
    resolvLabel =
        case (lrIpv4Result lr, lrIpv6Result lr) of
             (Left _, Left _) -> "A and AAAA error"
             (Left _, Right []) -> "A error, AAAA no result"
             (Left _, Right _)  -> "A error, AAAA success"
             (Right [], Left _) -> "A error, AAAA no result"
             (Right _, Left _)  -> "A success, AAAA error"
             (Right _, Right _) | lrIpv6Delay lr < lrIpv4Delay lr -> "AAAA before A"
                                | lrIpv4Delay lr + resolutionDelay > lrIpv6Delay lr ->
                                    "AAAA before A (Resolution Delay)"
                                | otherwise -> "A before AAAA"

prop_resolv_sim :: LookupResult -> Property
prop_resolv_sim lr =
    case runSimStrictShutdown $ prop_resolv lr of
         Left _  -> property False
         Right r -> r

_prop_resolv_io :: LookupResult -> Property
_prop_resolv_io lr = ioProperty $ prop_resolv lr
