{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Subscription (tests) where

import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise)
import           Control.Monad (replicateM, unless, when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimStrictShutdown)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (void)
import qualified Data.IP as IP
import           Data.List as L
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word
import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Text.Printf
import           Text.Show.Functions ()

import           Test.Tasty.QuickCheck (shuffle, testProperty)

import qualified Network.TypedProtocol.ReqResp.Client as ReqResp
import qualified Network.TypedProtocol.ReqResp.Server as ReqResp
import qualified Ouroboros.Network.Protocol.ReqResp.Codec as ReqResp

import           Codec.SerialiseTerm
import           Ouroboros.Network.Mux.Interface
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.NodeToNode
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Subscription.Common
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Subscriber
import           Ouroboros.Network.Time

import qualified Test.Mux as Mxt
import           Test.Mux.ReqResp


--
-- The list of all tests
--

tests :: TestTree
tests =
    testGroup "Subscription"
        [
          testProperty "Resolve (Sim)"      prop_resolv_sim
        --, testProperty "Resolve (IO)"      _prop_resolv_io
        -- ^ takes about 10 minutes to run due to delays in realtime.
        , testProperty "Resolve Subscribe (IO)" prop_sub_io
        , testProperty "Send Recive with Dns worker (IO)" prop_send_recv
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
    , lrioValency    :: !Word16
    }

mockResolver :: forall m. (MonadTimer m) => LookupResult -> Resolver m
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

mockResolverIO :: TMVar IO ()
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
                         (fromIntegral $ sidToPort (Socket.AF_INET, sid))
                         (IP.toHostAddress "127.0.0.1")) sids
                     (Left e)      -> Left e
        when (lrioFirst lr == Socket.AF_INET) $
            atomically $ putTMVar firstDoneMVar ()
        return r

    lAAAA :: DNS.Domain -> IO (Either DNS.DNSError [Socket.SockAddr])
    lAAAA _ = do
        when (lrioFirst lr == Socket.AF_INET) $ do
            void $ atomically $ takeTMVar firstDoneMVar
            threadDelay $ 0.01 + resolutionDelay
        let r = case lrioIpv6Result lr of
                     (Right sids) -> Right $ map (\sid ->
                         Socket.SockAddrInet6 (fromIntegral $ sidToPort (Socket.AF_INET6, sid)) 0
                                              (IP.toHostAddress6 "::1") 0) sids
                     (Left e)      -> Left e
        when (lrioFirst lr == Socket.AF_INET6) $
            atomically $ putTMVar firstDoneMVar ()
        return r

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
permCheck :: Ord o => [o] -> [o] -> Bool
permCheck a b = L.sort a == L.sort b

--
-- Properties
--

prop_resolv :: forall m.
     ( MonadAsync m
     , MonadSay   m
     , MonadSTM   m
     , MonadTime  m
     , MonadTimer m
     , MonadThrow m
     )
     => LookupResult
     -> m Property
prop_resolv lr =  do
    --say $ printf "%s" $ show lr
    let resolver = mockResolver lr
    x <- dnsResolve resolver $ DnsSubscriptionTarget "shelley-1.iohk.example" 1 2
    !res <- checkResult <$> dumpResult x []

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
                property $ permCheck addrs ea

            (Left _, Right ea) ->
                -- Expect a permutation of the result of the AAAA lookup.
                property $ permCheck addrs ea

            (Right sa4s, Right sa6s) ->
                let (cntA, cntB, headFamily) =
                        if sa4s /= [] && (lrIpv4Delay lr + resolutionDelay < lrIpv6Delay lr
                                        || null sa6s)
                            then (length sa4s, length sa6s, Socket.AF_INET)
                            else (length sa6s, length sa4s, Socket.AF_INET6) in
                property $ permCheck addrs (sa4s ++ sa6s) &&
                        sockAddrFamily (head addrs) == headFamily &&
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

    dumpResult :: SubscriptionTarget m Socket.SockAddr -> [Socket.SockAddr] -> m [Socket.SockAddr]
    dumpResult targets addrs = do
        target_m <- getSubscriptionTarget targets
        case target_m of
             Just (addr, nextTargets) -> do
                 --say $ printf "%s" $ show addr
                 threadDelay (connectionRtt lr)
                 dumpResult nextTargets (addr:addrs)
             Nothing -> do
                 --say $ printf "done"
                 return $ reverse addrs

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

prop_sub_io :: LookupResultIO
            -> Property
prop_sub_io lr = ioProperty $ do
    --printf "prop_sub_io: %s\n" (show lr)
    let serverIdsv4 = case lrioIpv4Result lr of
                           Left  _ -> []
                           Right r -> zip (repeat Socket.AF_INET) r
        serverIdsv6 = case lrioIpv6Result lr of
                               Left  _ -> []
                               Right r -> zip (repeat Socket.AF_INET6) r
        ipv4ClientCount = case lrioIpv4Result lr of
                               Left  _ -> 0
                               Right r -> length r
        ipv6ClientCount = case lrioIpv6Result lr of
                               Left  _ -> 0
                               Right r -> length r

    clientCountVar <- newTVarM (ipv4ClientCount + ipv6ClientCount)
    serverCountVar <- newTVarM (ipv4ClientCount + ipv6ClientCount)
    serverPortMapVar  <- newTVarM M.empty
    observerdConnectionOrderVar <- newTVarM []
    firstDoneVar <- newEmptyTMVarM
    serverWaitVar <- newTVarM False

    ipv4Servers <- replicateM (length serverIdsv4) (head <$> Socket.getAddrInfo Nothing (Just "127.0.0.1")
                            (Just "0"))
    ipv6Servers <- replicateM (length serverIdsv6) (head <$> Socket.getAddrInfo Nothing (Just "::1")
                            (Just "0"))
    serverAids <- mapM (async . spawnServer serverCountVar serverPortMapVar
                        observerdConnectionOrderVar serverWaitVar ) $
                           zip (serverIdsv4 ++ serverIdsv6) $ ipv4Servers ++ ipv6Servers

    --printf "waiting on servers\n"
    atomically $ do
        c <- readTVar serverCountVar
        when (c > 0) retry

    serverPortMap <- atomically $ readTVar serverPortMapVar
    --printf "server ports %s\n" $ show serverPortMap
    workerAid <- async $ dnsSubscriptionWorker' (mockResolverIO firstDoneVar serverPortMap lr)
            0
            (\_ -> Just minConnectionAttemptDelay)
            (DnsSubscriptionTarget "shelley-0.iohk.example" 6062 (lrioValency lr))
            (initiatorCallback clientCountVar)

    --printf "waiting on clients\n"
    atomically $ do
        c <- readTVar clientCountVar
        when (c > 0) retry

    --printf "cancelling worker\n"
    cancel workerAid
    atomically $ writeTVar serverWaitVar True

    --printf "waiting on servers\n"
    mapM_ wait serverAids
    --printf "done\n"

    observerdConnectionOrder <- fmap reverse $ atomically $ readTVar observerdConnectionOrderVar

    --printf "%s\n" $ show observerdConnectionOrder
    return $ property $ verifyOrder observerdConnectionOrder

  where

    verifyOrder
        :: [(Socket.Family, Word16)]
        -> Bool
    verifyOrder observerdConnectionOrder =
        case (lrioIpv4Result lr, lrioIpv6Result lr) of
             (Left _, Left _)     -> null observerdConnectionOrder
             (Right [], Right []) -> null observerdConnectionOrder
             (Left _, Right a)    -> a == map snd observerdConnectionOrder
             (Right a, Left _)    -> a == map snd observerdConnectionOrder
             (Right a, Right [])  -> a == map snd observerdConnectionOrder
             (Right [], Right a)  -> a == map snd observerdConnectionOrder
             (Right r4, Right r6) ->
                 not (null observerdConnectionOrder) &&
                 (lrioFirst lr == fst (head observerdConnectionOrder)) &&
                 permCheck (r4 ++ r6) (map snd observerdConnectionOrder)

    initiatorCallback
        :: TVar IO Int
        -> Socket.Socket
        -> IO ()
    initiatorCallback clientCountVar _ = do
        c <- atomically $ readTVar clientCountVar
        --printf "client count %d\n" c
        atomically $ do
            clientsLeft <- readTVar clientCountVar
            case clientsLeft of
                 0 -> retry
                 _ -> modifyTVar' clientCountVar (\a -> a - 1)


    spawnServer serverCountVar serverPortMapVar traceVar stopVar (sid, addr) =
        bracket
            (Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol)
            Socket.close
            (\sd -> do
                --printf "%s server hello\n" $ show sid
                -- bind the socket
                Socket.setSocketOption sd Socket.ReuseAddr 1
                Socket.bind sd (Socket.addrAddress addr)
                localPort <- Socket.socketPort sd
                atomically $ modifyTVar' serverPortMapVar (M.insert sid localPort)
                Socket.listen sd 10
                atomically $ modifyTVar' serverCountVar (\a -> a - 1)
                bracket
                    (Socket.accept sd)
                    (\(sd',_) -> Socket.close sd')
                    (\(_,_) -> do
                        now <- getMonotonicTime
                        --printf "%s %s client connected\n" (show sid) (show now)
                        --printf "%s client connected\n" (show sid)
                        atomically $ modifyTVar' traceVar (\sids -> sid:sids)
                        atomically $ do
                            doneWaiting <- readTVar stopVar
                            unless doneWaiting retry
                        --printf "%s server done waiting\n" $ show sid
                    )
             )


prop_send_recv
    :: (Int -> Int -> (Int, Int))
    -> [Int]
    -> Socket.Family
    -> Property
prop_send_recv f xs first = ioProperty $ do

    let lr = LookupResultIO (Right [0]) (Right [0]) first 1
        serverPortMap = M.fromList [((Socket.AF_INET, 0), 6062), ((Socket.AF_INET6, 0), 6062)]

    responderAddr4:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6062")
    responderAddr6:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6062")
    let (responderAddr, faultyAddress) = case first of
                                              Socket.AF_INET  -> (responderAddr6, responderAddr4)
                                              Socket.AF_INET6 -> (responderAddr4, responderAddr6)
                                              _  -> error "prop_send_recv: invalid address family"

    firstDoneVar <- newEmptyTMVarM

    cv <- newEmptyTMVarM
    sv <- newEmptyTMVarM

    let -- Server Node; only req-resp server
        responderApp :: MuxApplication ResponderApp Mxt.TestProtocols3 IO BL.ByteString Void ()
        responderApp = simpleMuxResponderApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespServerPeer (reqRespServerMapAccumL sv (\a -> pure . f a) 0))

        -- Client Node; only req-resp client
        initiatorApp :: MuxApplication InitiatorApp Mxt.TestProtocols3 IO BL.ByteString () Void
        initiatorApp = simpleMuxInitiatorApplication $
          \Mxt.ReqResp1 ->
            MuxPeer nullTracer
                    ReqResp.codecReqResp
                    (ReqResp.reqRespClientPeer (reqRespClientMap cv xs))

    res <-
     withDummyServer faultyAddress $
      withSimpleServerNode
        responderAddr
        (\(DictVersion codec) -> encodeTerm codec)
        (\(DictVersion codec) -> decodeTerm codec)
        (\(DictVersion _) -> acceptEq)
        (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0) (DictVersion nodeToNodeCodecCBORTerm) responderApp)
        $ \_ -> do
          worker <- async $ dnsSubscriptionWorker'
            (mockResolverIO firstDoneVar serverPortMap lr)
            0
            (\_ -> Just minConnectionAttemptDelay)
            (DnsSubscriptionTarget "shelley-0.iohk.example" 6062 1)
            (connectToNode' (\(DictVersion codec) -> encodeTerm codec)
                (\(DictVersion codec) -> decodeTerm codec)
                (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0)
                (DictVersion nodeToNodeCodecCBORTerm) initiatorApp))

          r <- atomically $ (,) <$> takeTMVar sv <*> takeTMVar cv
          cancel worker
          return r

    return (res == mapAccumL f 0 xs)

  where
    withDummyServer addr k =
        bracket
            (Socket.socket (Socket.addrFamily addr) Socket.Stream Socket.defaultProtocol)
            Socket.close
            (\sd -> do
                -- bind the socket, so that it is used, but don't listen to it.
                Socket.setSocketOption sd Socket.ReuseAddr 1
                Socket.bind sd (Socket.addrAddress addr)
                k
            )


{-
 - XXX Doesn't really test anything, doesn't exit in a resonable time.
 - XXX Depends on external network config
 - unbound DNS config example:
local-data: "shelley-1.iohk.example. IN A 192.168.1.115"
local-data: "shelley-1.iohk.example. IN A 192.168.1.215"
local-data: "shelley-1.iohk.example. IN A 192.168.1.216"
local-data: "shelley-1.iohk.example. IN A 192.168.1.100"
local-data: "shelley-1.iohk.example. IN A 192.168.1.101"
local-data: "shelley-1.iohk.example. IN A 127.0.0.1"
local-data: "shelley-1.iohk.example. IN AAAA ::1"

local-data: "shelley-0.iohk.example. IN AAAA ::1"
-}
_demo :: Property
_demo = ioProperty $ do
    server:_ <- Socket.getAddrInfo Nothing (Just "192.168.1.100") (Just "6062")
    server':_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6062")
    server6:_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6062")
    server6':_ <- Socket.getAddrInfo Nothing (Just "::1") (Just "6064")

    spawnServer server 10000
    spawnServer server' 10000
    spawnServer server6 100
    spawnServer server6' 45

    _ <- async $ dnsSubscriptionWorker 6061
            (\_ -> Just minConnectionAttemptDelay)
            [ DnsSubscriptionTarget "shelley-0.iohk.example" 6064 1
            , DnsSubscriptionTarget "shelley-1.iohk.example" 6062 2
            , DnsSubscriptionTarget "shelley-9.iohk.example" 6066 1
            ]
            (connectToNode' (\(DictVersion codec) -> encodeTerm codec)
                (\(DictVersion codec) -> decodeTerm codec)
                (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0)
                (DictVersion nodeToNodeCodecCBORTerm) appReq))

    threadDelay 130
    -- bring the servers back again
    spawnServer server6 10000
    spawnServer server6' 10000
    threadDelay 1000
    return ()

  where

    spawnServer addr delay =
        void $ async $ withSimpleServerNode addr
            (\(DictVersion codec) -> encodeTerm codec)
            (\(DictVersion codec) -> decodeTerm codec)
            (\(DictVersion _) -> acceptEq)
            (simpleSingletonVersions NodeToNodeV_1 (NodeToNodeVersionData 0)
                (DictVersion nodeToNodeCodecCBORTerm) appRsp)
            (\_ -> threadDelay delay)


    appReq = MuxInitiatorApplication (\Mxt.ChainSync1 -> error "req fail")
    appRsp = MuxResponderApplication (\Mxt.ChainSync1 -> error "rsp fail")

