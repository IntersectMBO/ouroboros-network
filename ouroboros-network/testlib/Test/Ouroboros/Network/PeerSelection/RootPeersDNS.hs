{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.PeerSelection.RootPeersDNS
  ( tests
  , mockDNSActions
  , genGroupSrvs
  , genDomainName
  , DomainAccessPoint (..)
  , MockRoots (..)
  , MockDNSMap
  , MockDNSLookupResult
  , DNSTimeout (..)
  , DNSLookupDelay (..)
  , DelayAndTimeoutScripts (..)
  ) where

import Control.Applicative (Alternative)
import Control.Monad (forever, replicateM_)
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 qualified as BSC
import Data.Dynamic (Typeable, fromDynamic)
import Data.Either (fromLeft, rights)
import Data.Foldable as Foldable (foldl')
import Data.Function (fix)
import Data.Functor (void)
import Data.IP (fromHostAddress, toSockAddr)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Void (Void)
import Data.Word (Word16)
import Network.DNS (DNSError (NameError), DNSMessage, ResourceRecord (..), TTL,
           answer, defaultResponse)
import Network.DNS qualified as DNS
import Network.DNS.Resolver qualified as DNSResolver
import Network.Socket (SockAddr (..))
import System.Random

import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (throw)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadSay (MonadSay (..))
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime),
           Time (..), addTime)
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Class.MonadTimer.SI qualified as MonadTimer
import Control.Monad.IOSim
import Control.Tracer (Tracer (Tracer), contramap, nullTracer, traceWith)

import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           LocalRootConfig (..), WarmValency (..))
import Test.Ouroboros.Network.Data.Script (Script (Script), initScript',
           scriptHead, singletonScript, stepScript')
import Test.Ouroboros.Network.PeerSelection.Instances
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "RootPeersDNS"
    [ testGroup "localRootPeersProvider"
       [ testProperty "preserve IPs"
                      prop_local_preservesIPs
       , testProperty "preserves groups and targets"
                      prop_local_preservesGroupNumberAndTargets
       , testProperty "resolves domains correctly"
                      prop_local_resolvesDomainsCorrectly
       , testProperty "updates domains correctly"
                      prop_local_updatesDomainsCorrectly
       ]
    , testGroup "publicRootPeersProvider"
       [ testProperty "resolves domains correctly"
                      prop_public_resolvesDomainsCorrectly
       ]
    , testGroup "delayedResource"
       [
       ]
    ]
  ]

--
-- Mock Environment and Utils
--

data DomainAccessPoint = DomainAccessPoint !DNS.Domain PortNumber
                       | DomainSRVAccessPoint !DNS.Domain
  deriving (Eq, Show, Ord)

instance Arbitrary DomainAccessPoint where
  arbitrary = oneof [plain, srv]
    where
      plain = DomainAccessPoint
                <$> genDomainName
                <*> genPort
      srv = DomainSRVAccessPoint <$> genDomainName

genDomainName :: Gen BSC.ByteString
genDomainName = elements $ (\i -> "test" <> (BSC.pack . show $ i)) <$> [1..6 :: Int]

type MockDNSLookupResult = Either [(IP, TTL)]
                                  [( DNS.Domain
                                   , Word16 -- ^ priority
                                   , Word16 -- ^ weight
                                   , PortNumber)]
type MockDNSMap = (Map (DNS.Domain, DNS.TYPE) MockDNSLookupResult)

data MockRoots = MockRoots {
    mockLocalRootPeers        :: [( HotValency
                                  , WarmValency
                                  , Map RelayAccessPoint (LocalRootConfig ()))
                                  -- ^ extraFlags isn't used here since it is
                                  -- not required for testing.
                                 ]
  , mockLocalRootPeersDNSMap  :: Script MockDNSMap
  , mockPublicRootPeers       :: Map RelayAccessPoint PeerAdvertise
  , mockPublicRootPeersDNSMap :: Script MockDNSMap
  }
  deriving Show

-- | Generates MockRoots environments
--
genMockRoots :: Gen MockRoots
genMockRoots = sized $ \relaysNumber -> do
    -- Generate LocalRootPeers
    --
    relaysPerGroup <- chooseEnum (1, relaysNumber `div` 3)

    -- concat unique identifier to DNS domains to simplify tests
    taggedLocalRelays <- tagRelays <$> vectorOf relaysNumber arbitrary
    targets <- vectorOf relaysNumber genTargets
    peerAdvertise <- blocks relaysPerGroup
                      <$> vectorOf relaysNumber arbitrary

    let ipsPerDomain = 2
        genLookup relays = do
          let (_relayAddress, relayDomains, relaySRVs) =
                foldl' threeWay ([], [], []) relays
          lookupIP <- genDomainIPLookupTable ipsPerDomain (dapDomain <$> relayDomains)
          srvs <- dealDomains relayDomains relaySRVs
          let srvs' = bimap dapDomain (map dapDomain) <$> srvs
          lookupSRV <- Map.fromList . map (bimap (,DNS.SRV) Right)
                       <$> genGroupSrvs srvs'
          return $ Map.union lookupIP lookupSRV

        localRelaysBlocks = blocks relaysPerGroup taggedLocalRelays
        localRelaysMap    = map Map.fromList $ zipWith zip localRelaysBlocks
                                                           peerAdvertise
        localRootPeers    = zipWith (\(h, w) g -> (h, w, g)) targets localRelaysMap

    lrpDNSMap <- Script . NonEmpty.fromList
              <$> listOf1 (genLookup taggedLocalRelays)

    -- Generate PublicRootPeers
    --
    publicRootRelays <- tagRelays <$> vectorOf relaysNumber arbitrary
    let publicRootAdvertise = vectorOf relaysNumber arbitrary

    publicRootPeers <- Map.fromList . zip publicRootRelays <$> publicRootAdvertise
    publicRootPeersDNSMap <- Script . NonEmpty.fromList <$> listOf1 (genLookup publicRootRelays)

    return (MockRoots {
      mockLocalRootPeers        = localRootPeers,
      mockLocalRootPeersDNSMap  = lrpDNSMap,
      mockPublicRootPeers       = publicRootPeers,
      mockPublicRootPeersDNSMap = publicRootPeersDNSMap
    })
  where
    dapDomain (DomainAccessPoint d _p) = d
    dapDomain (DomainSRVAccessPoint d) = d

    -- assigns some domains to srv records
    dealDomains :: [DomainAccessPoint]
                -> [DomainAccessPoint]
                -> Gen [(DomainAccessPoint, [DomainAccessPoint])]
    dealDomains = dealDomains' []

    -- kickstart the dealing
    dealDomains' [] (domain : domains) (srv : srvs') =
      dealDomains' [(srv, [domain])] domains srvs'

    -- when no more plain domains are available, return empty lookup
    -- which should trace as an error, but a lookup attempt is registered
    dealDomains' acc [] (srv : srvs') =
      dealDomains' ((srv, []):acc) [] srvs'

    -- toss a coin, if True pop a `DomainPlain` and assign it to
    -- the top srv record. Otherwise, pop the next srv record, and
    -- associate the plain domain with that one.
    dealDomains' as'@((s, ds):as) (domain : domains) srvs@(srv : srvs') = do
      toss <- arbitrary
      if toss
        then dealDomains' ((s, domain : ds):as) domains srvs
        else dealDomains' ((srv, [domain]):as') domains srvs'

    dealDomains' as _ds _srvs = return as

    threeWay :: ([RelayAccessPoint], [DomainAccessPoint], [DomainAccessPoint])
             -> RelayAccessPoint
             -> ([RelayAccessPoint], [DomainAccessPoint], [DomainAccessPoint])
    threeWay (rAddressAcc, rDomainAcc, rSRVAcc) = \case
      a@RelayAccessAddress {} -> (a : rAddressAcc, rDomainAcc, rSRVAcc)
      RelayAccessDomain d p  -> (rAddressAcc, DomainAccessPoint d p : rDomainAcc, rSRVAcc)
      RelayAccessSRVDomain d -> (rAddressAcc, rDomainAcc, DomainSRVAccessPoint d : rSRVAcc)

    genTargets :: Gen (HotValency, WarmValency)
    genTargets = do
      warmValency <- WarmValency <$> chooseEnum (1, 5)
      hotValency <- HotValency <$> chooseEnum (1, getWarmValency warmValency)
      return (hotValency, warmValency)

    genDomainIPLookupTable :: Int -> [DNS.Domain] -> Gen (Map (DNS.Domain, DNS.TYPE)
                                                              MockDNSLookupResult)
    genDomainIPLookupTable ipsPerDomain localRootDomains = do
      localRootDomainIPs <- blocks ipsPerDomain
              -- Modules under test do not differ by IP version so we only
              -- generate IPv4 addresses.
              <$> vectorOf (ipsPerDomain * length localRootDomains)
                           genIPv4
      localRootDomainTTLs <- blocks ipsPerDomain
              <$> vectorOf (ipsPerDomain * length localRootDomains)
                           (arbitrary :: Gen TTL)

      let localRootDomainsIP_TTls = zipWith zip localRootDomainIPs localRootDomainTTLs
          rootDomainKeys = (, DNS.A) <$> localRootDomains
          lrpDNSMap = Map.fromList $ zip rootDomainKeys (Left <$> localRootDomainsIP_TTls)

      return lrpDNSMap

    tagRelays =
      zipWith
        (\tag rel
          -> case rel of
               RelayAccessDomain domain port ->
                 RelayAccessDomain (domain <> (pack . show) tag) port
               RelayAccessSRVDomain domain ->
                 RelayAccessSRVDomain (domain <> (pack . show) tag)
               x -> x
        )
        [(0 :: Int), 1 .. ]

    blocks _ [] = []
    blocks s l  = take s l : blocks s (drop s l)

-- assigns weights and priorities to SRV record's subordinate domains
-- such that several subdomains may have the same priority level and port
-- number, and each one will have a random weight, and result is shuffled
--
genGroupSrvs :: (Arbitrary prio, Arbitrary wt)
             => [(srv, [subordinate])]
             -> Gen [(srv, [(subordinate, prio, wt, PortNumber)])]
genGroupSrvs = go []
 where
  go acc [] = return acc
  go acc ((srv, subordinates):srvs) = do
    let worker grouped 0 _ = shuffle grouped
        worker grouped count domains' = do
          howMany <- chooseInt (1, count)
          port <- genPort
          prio <- arbitrary
          wts <- vectorOf howMany arbitrary
          let group = take howMany domains'
              smash dom wt = (dom, prio, wt, port)
              grouped' = zipWith smash group wts
                         <> grouped
          worker grouped' (count - howMany) (drop howMany domains')
    organized <- worker [] (length subordinates) subordinates
    go ((srv, organized) : acc) srvs

instance Arbitrary MockRoots where
    arbitrary = genMockRoots
    shrink roots@MockRoots
      { mockLocalRootPeers
      , mockLocalRootPeersDNSMap
      , mockPublicRootPeers
      , mockPublicRootPeersDNSMap
      } =
      [ roots { mockLocalRootPeers        = lrp
              , mockLocalRootPeersDNSMap  = lrpDNSMap
              }
      | lrp <- shrinkList (const []) mockLocalRootPeers,
        let lrpDomains =
              Set.fromList $
                concatMap
                  (mapMaybe
                    (\case
                        RelayAccessDomain d _p -> Just (d, DNS.A)
                        RelayAccessSRVDomain d -> Just (d, DNS.SRV)
                        _otherwise -> Nothing)
                    . Map.keys . thrd)
                  lrp
            lrpDNSMap  = (`Map.restrictKeys` lrpDomains)
                       <$> mockLocalRootPeersDNSMap
      ] ++
      [ roots { mockPublicRootPeers       = prp
              , mockPublicRootPeersDNSMap = prpDNSMap
              }
      | prp <- shrink mockPublicRootPeers,
        let prpDomains = Set.fromList $
              mapMaybe
                ((\case
                    RelayAccessDomain d _p -> Just (d, DNS.A)
                    RelayAccessSRVDomain d -> Just (d, DNS.SRV)
                    _otherwise -> Nothing) . fst)
               (Map.assocs prp)
            prpDNSMap  = (`Map.restrictKeys` prpDomains)
                       <$> mockPublicRootPeersDNSMap
      ]
        where
          thrd (_, _, c) = c

-- | Used for debugging in GHCI
--
simpleMockRoots :: MockRoots
simpleMockRoots = MockRoots localRootPeers dnsMap Map.empty (singletonScript Map.empty)
  where
    localRootPeers =
      [ ( 2, 2
        , Map.fromList
          [ ( RelayAccessAddress (read "192.0.2.1") (read "3333")
            , LocalRootConfig DoAdvertisePeer InitiatorAndResponderDiffusionMode ()
            )
          , ( RelayAccessDomain  "test.domain"      (read "4444")
            , LocalRootConfig DoNotAdvertisePeer InitiatorAndResponderDiffusionMode ()
            )
          ]
        )
      ]
    dnsMap = singletonScript $ Map.fromList
              [ (("test.domain", DNS.A), Left [read "192.1.1.1", read "192.2.2.2"])
              ]


genDiffTime :: Integer
            -> Integer
            -> Gen DiffTime
genDiffTime lo hi =
      picosecondsToDiffTime
    . (lo * 1_000_000_000 +)
    . (1_000_000_000 *)
    . getNonNegative
  <$> resize (fromIntegral hi) arbitrary


newtype DNSTimeout = DNSTimeout { getDNSTimeout :: DiffTime }
  deriving Show

instance Arbitrary DNSTimeout where
    arbitrary = DNSTimeout <$> genDiffTime 110 300
    shrink (DNSTimeout delta) =
      [ DNSTimeout (fromRational delta')
      | delta' <- shrink (realToFrac delta)
      , delta' >= 110
      ]


newtype DNSLookupDelay = DNSLookupDelay { getDNSLookupDelay :: DiffTime }
  deriving Show

instance Arbitrary DNSLookupDelay where
    arbitrary = DNSLookupDelay <$> genDiffTime 20 120
    shrink (DNSLookupDelay delta) =
      [ DNSLookupDelay (fromRational delta')
      | delta' <- shrink (realToFrac delta)
      , delta' >= 20
      ]

-- | Mock DNSActions data structure for testing purposes.
-- Adds DNS Lookup function for IOSim with different timeout and lookup
-- delays for every attempt.
mockDNSActions :: forall peerAddr m.
                  ( MonadDelay m
                  , MonadTimer m
                  , MonadAsync m
                  )
               => Tracer m DNSTrace
               -> DNSLookupType
               -> (IP -> PortNumber -> peerAddr)
               -> StrictTVar m MockDNSMap
               -> StrictTVar m (Script DNSTimeout)
               -> StrictTVar m (Script DNSLookupDelay)
               -> DNSActions peerAddr () m
mockDNSActions tracer ofType0 toPeerAddr dnsMapVar dnsTimeoutScript dnsLookupDelayScript =
    DNSActions {
      dnsResolverResource,
      dnsAsyncResolverResource,
      dnsLookupWithTTL = dispatchLookupWithTTL ofType0 mockLookup tracer toPeerAddr
    }
 where
   dnsResolverResource      _ = return (Right <$> constantResource ())
   dnsAsyncResolverResource _ = return (Right <$> constantResource ())

   mockLookup :: resolver
              -> resolvConf
              -> DNS.Domain
              -> DNS.TYPE
              -> m (Maybe (Either DNSError DNSMessage))
   mockLookup _ _ domain ofType = do
     dnsMap <- readTVarIO dnsMapVar
     DNSTimeout dnsTimeout <- stepScript' dnsTimeoutScript
     DNSLookupDelay dnsLookupDelay <- stepScript' dnsLookupDelayScript

     MonadTimer.timeout dnsTimeout do
       MonadTimer.threadDelay dnsLookupDelay
       case Map.lookup (domain, ofType) dnsMap of
         Nothing -> return (Left NameError)
         Just x  -> return (Right $ toDNSMessage x)

     where
       toDNSMessage = \case
         Left ipsttls ->
           defaultResponse {
             answer = [ResourceRecord domain DNS.NULL 0 ttl rdata
                      | (ip, ttl) <- ipsttls
                      , let rdata = case ip of
                              IPv4 ip' -> DNS.RD_A ip'
                              IPv6 ip' -> DNS.RD_AAAA ip']}
         Right ds ->
           defaultResponse {
             answer = [ResourceRecord domain DNS.NULL 0 0 rdata
                      | (domain', prio, weight, port) <- ds
                      , let rdata = DNS.RD_SRV prio weight (fromIntegral port) domain']}

-- | 'localRootPeersProvider' running with a given MockRoots env
--
mockLocalRootPeersProvider :: forall m.
                              ( Alternative (STM m)
                              , MonadAsync    m
                              , MonadDelay    m
                              , MonadThrow    m
                              , MonadTimer    m
                              , MonadTraceSTM m
                              , MonadLabelledSTM m
                              )
                           => Tracer m (TestTraceEvent (TraceLocalRootPeers () SockAddr))
                           -> MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> TestSeed
                           -> m ()
mockLocalRootPeersProvider tracer (MockRoots localRootPeers dnsMapScript _ _)
                           dnsTimeoutScript dnsLookupDelayScript dnsSeed = do
      dnsMapScriptVar <- initScript' dnsMapScript
      dnsMap <- stepScript' dnsMapScriptVar
      dnsMapVar <- newTVarIO dnsMap

      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty
      withAsync (updateDNSMap dnsMapScriptVar dnsMapVar) $ \_ -> do
        void $ MonadTimer.timeout 3600 $
          localRootPeersProvider (contramap Left tracer)
                                 PeerActionsDNS {
                                   paToPeerAddr = curry toSockAddr,
                                   paDnsActions =
                                     mockDNSActions
                                       (contramap Right tracer)
                                       LookupReqAOnly
                                       (curry toSockAddr)
                                       dnsMapVar
                                       dnsTimeoutScriptVar
                                       dnsLookupDelayScriptVar
                                 }
                                 DNSResolver.defaultResolvConf
                                 (mkStdGen $ unTestSeed dnsSeed)
                                 (readTVar localRootPeersVar)
                                 resultVar
        -- if there's no dns domain, `localRootPeersProvider` will never write
        -- to `resultVar`; thus the `traceTVarIO` callback will never execute.
        -- By reading & writing to the `TVar` we are forcing it to run at least
        -- once.
        atomically $ readTVar resultVar >>= writeTVar resultVar
  where
    updateDNSMap :: StrictTVar m (Script MockDNSMap)
                 -> StrictTVar m MockDNSMap
                 -> m Void
    updateDNSMap dnsMapScriptVar dnsMapVar =
      forever $ do
        threadDelay 10
        dnsMap <- stepScript' dnsMapScriptVar
        atomically (writeTVar dnsMapVar dnsMap)


-- | 'publicRootPeersProvider' running with a given MockRoots env.
--
-- NOTE: This function is used in 'prop_public_resolvesDomainsCorrectly'. Due to
-- API limitations it is needed to run 'publicRootPeersProvider' multiple times,
-- in order to run only 1 simulation which resolves untill we get the expected
-- result, instead of a recursive loop which at each step runs IOSim.
--
mockPublicRootPeersProvider :: forall m a.
                               ( MonadAsync m
                               , MonadDelay m
                               , MonadThrow m
                               , MonadTimer m
                               )
                            => Tracer m (TestTraceEvent TracePublicRootPeers)
                            -> MockRoots
                            -> Script DNSTimeout
                            -> Script DNSLookupDelay
                            -> TestSeed
                            -> ((Int -> m (Map SockAddr PeerAdvertise, DiffTime)) -> m a)
                            -> m ()
mockPublicRootPeersProvider tracer (MockRoots _ _ publicRootPeers dnsMapScript)
                            dnsTimeoutScript dnsLookupDelayScript dnsSeed action = do
      dnsMapScriptVar <- initScript' dnsMapScript
      dnsMap <- stepScript' dnsMapScriptVar
      dnsMapVar <- newTVarIO dnsMap
      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      publicRootPeersVar <- newTVarIO publicRootPeers
      replicateM_ 5 $ do
        dnsMap' <- stepScript' dnsMapScriptVar
        atomically (writeTVar dnsMapVar dnsMap')

        publicRootPeersProvider (contramap Left tracer)
                                (curry toSockAddr)
                                dnsSemaphore
                                DNSResolver.defaultResolvConf
                                (readTVar publicRootPeersVar)
                                (mockDNSActions
                                  (contramap Right tracer)
                                  LookupReqAOnly
                                  (curry toSockAddr)
                                  dnsMapVar
                                  dnsTimeoutScriptVar
                                  dnsLookupDelayScriptVar)
                                (mkStdGen $ unTestSeed dnsSeed)
                                action

-- | 'resolveDomainAddresses' running with a given MockRoots env
--
mockResolveLedgerPeers :: ( MonadAsync m
                          , MonadDelay m
                          , MonadThrow m
                          , MonadTimer m
                          )
                       => Tracer m (TestTraceEvent TraceLedgerPeers)
                       -> MockRoots
                       -> Script DNSTimeout
                       -> Script DNSLookupDelay
                       -> TestSeed
                       -> m (Map DNS.Domain (Set SockAddr))
mockResolveLedgerPeers tracer (MockRoots _ _ publicRootPeers dnsMapScript)
                       dnsTimeoutScript dnsLookupDelayScript (TestSeed dnsSeed) = do
      dnsMapScriptVar <- initScript' dnsMapScript
      dnsMap <- stepScript' dnsMapScriptVar
      dnsMapVar <- newTVarIO dnsMap
      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      let relays = [ dap
                   | (relay, _) <- Map.assocs publicRootPeers
                   , dap <- case relay of
                              RelayAccessAddress {} -> []
                              x                     -> [x]]
      traceWith tracer . Left $ TraceLedgerPeersDomains relays
      resolveLedgerPeers dnsSemaphore
                         DNSResolver.defaultResolvConf
                         (mockDNSActions
                           (contramap Right tracer)
                           LookupReqAOnly
                           (curry toSockAddr)
                           dnsMapVar
                           dnsTimeoutScriptVar
                           dnsLookupDelayScriptVar)
                         AllLedgerPeers
                         relays
                         (mkStdGen dnsSeed)

--
-- Utils for properties
--

type TestTraceEvent a = Either a DNSTrace

tracerTraceLocalRoots :: Tracer (IOSim s) (TestTraceEvent (TraceLocalRootPeers () SockAddr))
tracerTraceLocalRoots = Tracer traceM

tracerTracePublicRoots :: Tracer (IOSim s) (TestTraceEvent TracePublicRootPeers)
tracerTracePublicRoots = Tracer traceM

selectTestTraceEvents :: (Typeable b) => SimTrace a
                      -> [(Time, TestTraceEvent b)]
selectTestTraceEvents = go
  where
    go (SimTrace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e = (t, x) : go trace
    go (SimPORTrace t _ _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimTrace _ _ _ _ trace)      =         go trace
    go (SimPORTrace _ _ _ _ _ trace) =         go trace
    go (TraceMainException _ _ e _)  = throw e
    go (TraceDeadlock      _   _)    = [] -- expected result in many cases
    go  TraceMainReturn {}           = []
    go (TraceInternalError e)        = error ("IOSim: " ++ e)
    go TraceLoop                     = error "IOSimPOR step time limit exceeded"

selectLocalRootPeersWithDNSEvents :: SimTrace a
                                  -> [(Time, TestTraceEvent (TraceLocalRootPeers () SockAddr))]
selectLocalRootPeersWithDNSEvents = filter them . selectTestTraceEvents
  where
    them (_t, Right dns) =
      case dns of
        (DNSLookupResult DNSLocalPeer _ _ _) -> True
        (DNSLookupError DNSLocalPeer _ _ _)  -> True
        (SRVLookupResult DNSLocalPeer _ _)   -> True
        (SRVLookupError DNSLocalPeer _)      -> True
        _otherwise                           -> False
    them _ = True

selectLocalRootGroupsEvents :: [(Time, TestTraceEvent (TraceLocalRootPeers () SockAddr))]
                            -> [(Time, [(HotValency, WarmValency, Map SockAddr (LocalRootConfig ()))])]
selectLocalRootGroupsEvents trace = [ (t, r)
                                    | (t, Left (TraceLocalRootGroups r)) <- trace ]

selectPublicRootPeersWithDNSEvents :: SimTrace a
                                   -> [(Time, TestTraceEvent TracePublicRootPeers)]
selectPublicRootPeersWithDNSEvents = filter them . selectTestTraceEvents
  where
    them (_t, Right dns) =
      case dns of
        (DNSLookupResult DNSPublicPeer _ _ _) -> True
        (DNSLookupError DNSPublicPeer _ _ _)  -> True
        (SRVLookupResult DNSPublicPeer _ _)   -> True
        (SRVLookupError DNSPublicPeer _)      -> True
        _otherwise                            -> False
    them _ = True

selectDnsResultEvents :: [(Time, TestTraceEvent a)]
                      -> [(Time, DNSTrace)]
selectDnsResultEvents trace = [(t, r)
                              | (t, Right r@(DNSLookupResult {})) <- trace]

--
-- Local Root Peers Provider Tests
--

-- | The 'localRootPeersProvider' should use the IP addresses. This property
-- tests whether local root peer groups contain the IP addresses provided.
--
prop_local_preservesIPs :: MockRoots
                        -> Script DNSTimeout
                        -> Script DNSLookupDelay
                        -> TestSeed
                        -> Property
prop_local_preservesIPs mockRoots@(MockRoots localRoots _ _ _)
                        dnsTimeoutScript
                        dnsLookupDelayScript
                        dnsSeed =
    let tr = selectLocalRootGroupsEvents
           $ selectLocalRootPeersWithDNSEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript
                                        dnsSeed

     in counterexample (intercalate "\n" $ map show tr)
      $ classify (length tr > 0) "Actually testing something"
      $ checkAll tr
  where
    checkAll :: [(Time, [( HotValency
                         , WarmValency
                         , Map SockAddr (LocalRootConfig ()))])]
             -> Property
    checkAll [] = property True
    checkAll (x:t) =
      let thrd (_, _, c) = c
          -- get local root ip addresses
          localRootAddresses :: [(a, b, Map RelayAccessPoint (LocalRootConfig ()))]
                             -> Set SockAddr
          localRootAddresses lrp =
            Set.fromList
            [ toSockAddr (ip, port)
            | (_, _, m) <- lrp
            , RelayAccessAddress ip port <- Map.keys m
            ]

          -- get ip addresses out of LocalRootGroup trace events
          localGroupEventsAddresses :: (a, [( HotValency
                                            , WarmValency
                                            , Map SockAddr (LocalRootConfig ()))])
                                    -> Set SockAddr
          localGroupEventsAddresses (_, s) =
              Set.fromList
            $ concatMap (Map.keys . thrd)
            $ s

          localRootAddressesSet = localRootAddresses localRoots
          localGroupEventsAddressesSet = localGroupEventsAddresses x
       in counterexample (show localRootAddressesSet ++ " is not subset of "
                         ++ show localGroupEventsAddressesSet)
        $ localRootAddressesSet `Set.isSubsetOf` localGroupEventsAddressesSet
        .&&. checkAll t

-- | The 'localRootPeersProvider' should preserve the local root peers
-- group number and respective targets. This property tests whether local
-- root peer groups update due to DNS resolution results, does not alter
-- the initial groups configuration.
--
prop_local_preservesGroupNumberAndTargets :: MockRoots
                                          -> Script DNSTimeout
                                          -> Script DNSLookupDelay
                                          -> TestSeed
                                          -> Property
prop_local_preservesGroupNumberAndTargets mockRoots@(MockRoots lrp _ _ _)
                                          dnsTimeoutScript
                                          dnsLookupDelayScript
                                          dnsSeed =
    let tr = selectLocalRootGroupsEvents
           $ selectLocalRootPeersWithDNSEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript
                                        dnsSeed

        -- For all LocalRootGroup results, the number of groups should be
        -- preserved, i.e. no new groups are added nor deleted along the
        -- trace by localRootPeersProvider.
        preservesGroupNumber = all ((== length lrp) . length . snd) tr

        -- For all LocalRootGroup results, the targets for each group
        -- should be preserved, i.e. targets are not modified along the
        -- trace by localRootPeersProvider.
        preservesTargets     = all (all (\((a, b, _), (a', b', _)) -> a == a' && b == b'))
                                   [ zip lrp r | r <- map snd tr ]

     in label (show $ length tr `div` 100 * 100) $
        preservesGroupNumber .&&. preservesTargets

-- | The 'localRootPeersProvider' should be able to resolve DNS domains
-- correctly, assuming the domain maps to any IP address. This property
-- tests whether 'localRootPeersProvider' is capable of eventually resolving
-- domain addresses even after having failed to do so in the first attempt.
--
prop_local_resolvesDomainsCorrectly :: MockRoots
                                    -> Script DNSTimeout
                                    -> Script DNSLookupDelay
                                    -> TestSeed
                                    -> Property
prop_local_resolvesDomainsCorrectly mockRoots@(MockRoots localRoots lDNSMap _ _)
                                    dnsTimeoutScript
                                    dnsLookupDelayScript
                                    dnsSeed =
    let mockRoots' =
          mockRoots { mockLocalRootPeersDNSMap =
                        singletonScript (scriptHead lDNSMap)
                    }
        tr = selectLocalRootPeersWithDNSEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots'
                                        dnsTimeoutScript
                                        dnsLookupDelayScript
                                        dnsSeed

        -- local root domains
        localRootDomains :: Set (DNS.Domain, DNS.TYPE)
        localRootDomains =
          Set.fromList
          [ item
          | (_, _, m) <- localRoots
          , item <- flip mapMaybe (Map.keys m) \case
              RelayAccessDomain d _p -> Just (d, DNS.A)
              RelayAccessSRVDomain d -> Just (d, DNS.SRV)
              _otherwise -> Nothing
          ]

        -- domains that were resolved during simulation
        resultMap :: Set (DNS.Domain, DNS.TYPE)
        resultMap = Set.fromList
                  . mapMaybe (filtering . snd)
                  . selectDnsResultEvents
                  $ tr
          where
            filtering = \case
              DNSLookupResult _ domain Nothing _ -> Just (domain, DNS.A)
              DNSLookupResult _ _ (Just domain) _ -> Just (domain, DNS.SRV)
              _otherwise -> Nothing

        -- all domains that could have been resolved in each script
        maxResultMap :: Script (Set (DNS.Domain, DNS.TYPE))
        maxResultMap = Map.keysSet
                       . (`Map.restrictKeys` localRootDomains)
                       <$> lDNSMap

        -- all domains that were tried to resolve during the simulation
        allTriedDomains :: Set (DNS.Domain, DNS.TYPE)
        allTriedDomains = Set.fromList
                        . map filtering
                        . mapMaybe (selectDnsTraces . snd)
                        $ tr
          where
            filtering = \case
              DNSLookupResult _ domain Nothing _ -> (domain, DNS.A)
              DNSLookupResult _ _ (Just domain) _ -> (domain, DNS.SRV)
              DNSLookupError _ Nothing srvDomain _ -> (srvDomain, DNS.SRV)
              DNSLookupError _ (Just _) domain _ -> (domain, DNS.A)
              SRVLookupResult _ domain _ -> (domain, DNS.A)
              SRVLookupError _ domain -> (domain, DNS.SRV)
            selectDnsTraces = \case
              Right trace -> Just trace
              Left  _ -> Nothing
    in
      -- we verify that we tried to resolve all local root domains, and that the
      -- resolved ones are a subset of `maxResultMap`
           counterexample ("violation: localRootDomains isSubsetOf allTriedDomains\nlocalRootDomains: "
                           <> show localRootDomains
                           <> "\nallTriedDomains: " <> show allTriedDomains) $
           localRootDomains `Set.isSubsetOf` allTriedDomains
      .&&. foldr (\rm r -> counterexample (show resultMap ++ " is subset of "
                                     ++ show rm)
                          (resultMap `Set.isSubsetOf` rm)
                        .&&. r
                 )
                 (property True)
                 maxResultMap


-- | The 'localRootPeersProvider' after resolving a DNS domain address
-- should update the local result group list correctly, i.e. add the
-- resolved ip addresses to the correct group where the domain address was
-- (in the initial configuration specification). This property tests whether
-- after a successful DNS lookup the result list is updated correctly.
--
-- Correctly means: Updates in the right place and does not overwrite the
-- previous state.
--
prop_local_updatesDomainsCorrectly :: MockRoots
                                   -> Script DNSTimeout
                                   -> Script DNSLookupDelay
                                   -> TestSeed
                                   -> Property
prop_local_updatesDomainsCorrectly mockRoots@(MockRoots lrp _ _ _)
                                   dnsTimeoutScript
                                   dnsLookupDelayScript
                                   dnsSeed =
    let tr = selectLocalRootPeersWithDNSEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript
                                        dnsSeed

        r = Foldable.foldl' (\(b, (t, x)) (t', y) ->
                    case (x, y) of
                      -- Last result groups value, Current result groups value
                      (Left (TraceLocalRootGroups lrpg), Left (TraceLocalRootGroups lrpg')) ->
                        let -- Get all IPs present in last group at position
                            -- 'index'
                            ipsAtIndex = Map.keys
                                       $ foldMap thrd lrpg

                            -- Get all IPs present in current group at position
                            -- 'index'
                            ipsAtIndex' = Map.keys
                                        $ foldMap thrd lrpg'

                            arePreserved = all (`elem` ipsAtIndex') ipsAtIndex

                         in (arePreserved && b, (t', y))
                      -- Last DNS lookup result   , Current result groups value
                      (Right (DNSLookupResult _ dFollow dSRV ipsttls@(ipttl : _)), Left (TraceLocalRootGroups lrpg)) ->
                        -- create and index db for each group
                        let rap =
                              case dSRV of
                                Just dSRV' -> RelayAccessSRVDomain dSRV'
                                Nothing    -> RelayAccessDomain dFollow (port ipttl)

                            db = zip [0,1..] lrp
                            -- since our MockRoots generator generates
                            -- unique domain addresses we can look for
                            -- which group index does a particular domain
                            -- address belongs
                            index = foldr (\(i, (_, _, m)) prev ->
                                            case Map.lookup rap m of
                                              Nothing -> prev
                                              Just _  -> i
                                          ) (-1) db
                            -- Get all IPs present in group at position
                            -- 'index'
                            ipsAtIndex = map (\sockAddr ->
                                             case sockAddr of
                                               SockAddrInet _ hostAddr
                                                 -> IPv4 $ fromHostAddress hostAddr
                                               _ -> error $ show sockAddr --error "Impossible happened!"

                                         ) $ Map.keys
                                           $ thrd
                                           $ lrpg !! index :: [IP]
                            -- Check if all ips from the previous DNS
                            -- lookup result are present in the current
                            -- result group at the correct index
                            arePresent = all ((`elem` ipsAtIndex) . ip) ipsttls
                         in (arePresent && b, (t', y))
                      -- the empty DNS result trivially passes
                      (Right (DNSLookupResult _ _ _ []), Left (TraceLocalRootGroups {})) ->
                        (b, (t', y))
                      (Right {}, _) -> (b, (t, x))
                      (_, _)        -> (b, (t', y))
                   )
              (True, head tr)
              (tail tr)
     in property (fst r)
  where
    thrd (_, _, c) = c
    port (_, it, _) = it
    ip   (it, _, _) = it

--
-- Public Root Peers Provider Tests
--

-- | Delay and timeout script which make sure that eventually the dns lookup
-- will not timeout.
--
data DelayAndTimeoutScripts = DelayAndTimeoutScripts
       (Script DNSLookupDelay)
       (Script DNSTimeout)
    deriving Show

fixupDelayAndTimeoutScripts :: DelayAndTimeoutScripts
                            -> DelayAndTimeoutScripts
fixupDelayAndTimeoutScripts (DelayAndTimeoutScripts lookupScript@(Script delays)
                                                    timeoutScript@(Script timeouts)) =
      let lastTimeout :: DiffTime
          lastTimeout = getDNSTimeout $ NonEmpty.last timeouts

          lookupScript' =
            if getDNSLookupDelay (NonEmpty.last delays) >= lastTimeout
              then Script (delays <> (DNSLookupDelay (lastTimeout / 2) :| []))
              else lookupScript

      in DelayAndTimeoutScripts lookupScript' timeoutScript

instance Arbitrary DelayAndTimeoutScripts where
    arbitrary = fmap fixupDelayAndTimeoutScripts
              $ DelayAndTimeoutScripts
                  <$> arbitrary
                  <*> arbitrary

    shrink (DelayAndTimeoutScripts lookupScript timeoutScript) =
      [ fixupDelayAndTimeoutScripts
          (DelayAndTimeoutScripts lookupScript timeoutScript')
      | timeoutScript' <- shrink timeoutScript
      ]
      ++
      [ fixupDelayAndTimeoutScripts
          (DelayAndTimeoutScripts lookupScript' timeoutScript)
      | lookupScript' <- shrink lookupScript
      ]



-- | The 'publicRootPeersProvider' should be able to resolve DNS domains
-- correctly, assuming the domain maps to any IP address. This property
-- tests whether 'publicRootPeersProvider' is capable of eventually resolving domain
-- addresses even after having failed to do so in the first attempt, in
-- a bounded amount of time.
--
prop_public_resolvesDomainsCorrectly :: MockRoots
                                     -> DelayAndTimeoutScripts
                                     -> Int
                                     -> TestSeed
                                     -> Property
prop_public_resolvesDomainsCorrectly
    mockRoots@(MockRoots _ _ _ pDNSMap)
    (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
    n
    dnsSeed
  =
    let pDNSMap' = scriptHead pDNSMap
        mockPublicRootPeersDNSMap = singletonScript pDNSMap'
        mockRoots' =
          mockRoots { mockPublicRootPeersDNSMap }
        tr = runSimTrace
           $ mockPublicRootPeersProvider tracerTracePublicRoots
                                         mockRoots'
                                         dnsTimeoutScript
                                         dnsLookupDelayScript
                                         dnsSeed
                                         ($ n)

        successes = selectDnsResultEvents @TracePublicRootPeers
                  . selectTestTraceEvents
                  $ tr

        successes' = map snd successes

        step :: DNSTrace -> Property -> Property
        step (DNSLookupResult _ "" (Just srvDomain) []) r =
          counterexample "SRV record not found in mock lookup map" $
            Map.member (srvDomain, DNS.SRV) pDNSMap' .&&. r
        step (DNSLookupResult _ domain srvDomain ipsttls) r =
          case srvDomain of
            Nothing ->
               counterexample "DNS.A IP mismatch error" $
                 (fromLookup === fromTrace) .&&. r
               where
                 fromLookup =
                  fst <$> fromLeft (error e) (pDNSMap' Map.! (domain, DNS.A))
                 e = "Domain " <> show domain <> " not found in lookup map"
                 fromTrace = ip <$> ipsttls
            Just srvDomain' ->
              counterexample "SRV lookup error" $
                case Map.lookup (srvDomain', DNS.SRV) pDNSMap' of
                  Nothing -> property False
                  Just (Left {}) -> property False
                  Just (Right ds) ->
                    case find ((domain ==) . dFollow) ds of
                      Nothing -> property False
                      Just (d, _, _, _) ->
                        counterexample "IP mismatch" $
                          fromLookup === fromTrace .&&. r
                        where
                          fromLookup =
                            fst <$> fromLeft (error err) (pDNSMap' Map.! (d, DNS.A))
                          err = "Domain " <> show d <> " from SRV lookup of " <> show srvDomain'
                                <> " not found."
                          fromTrace = ip <$> ipsttls

        step (SRVLookupResult _ domain _) r =
          Map.member (domain, DNS.SRV) pDNSMap' .&&. r
        step _ _ = error "impossible!"
     in
       foldr step (property True) successes'
  where
    ip (it, _, _) = it
    dFollow (it, _, _, _) = it



-- | Create a resource from a list.
--
-- Invariant: the resource fails if it is run more than the number of items.
--
listResource :: forall e m a. Monad m
             => Tracer m a
             -> [Either e a] -> Resource m (Either e a)
listResource tracer = fix go
  where
    go :: ([Either e a] -> Resource m (Either e a))
       -> ([Either e a] -> Resource m (Either e a))
    go _this [] = error "listResource: invariant vaiolation"
    go this (a@(Right x) : as) = Resource $ do
      traceWith tracer x
      pure (a, this as)
    go this (a@Left {}: as) = Resource $
      pure (a, this as)


-- | Verify retryResource
--
prop_retryResource :: NonEmptyList DNSTimeout -> [Either Int Int] -> Property
prop_retryResource (NonEmpty delays0) as =
    counterexample (ppTrace trace) $
    selectTraceEventsDynamic trace /= model delays as
  where
    delays :: NonEmpty DiffTime
    delays = getDNSTimeout <$> NonEmpty.fromList delays0

    tracer :: Tracer (IOSim s) Int
    tracer = Tracer (\a -> getMonotonicTime >>= \t -> traceM (t, a))
          <> Tracer (\a -> getMonotonicTime >>= \t -> say (show (t, a)))

    resource :: Resource (IOSim s) Int
    resource = retryResource nullTracer delays
             $ listResource tracer as

    sim :: IOSim s [Int]
    sim = run (length (rights as)) resource
      where
        run :: Int -> Resource (IOSim s) Int -> IOSim s [Int]
        run = go []
          where
            go xs n _ | n <= 0 = return (reverse xs)
            go xs n r = do
              (x, r') <- withResource r
              go (x : xs) (pred n) r'


    trace = runSimTrace sim

    -- pure model of `retryResource`
    model :: NonEmpty DiffTime -> [Either Int Int] -> [(Time, Int)]
    model ds0 = go (Time 0) [] ds0
      where
        dropHead :: forall x. NonEmpty x -> NonEmpty x
        dropHead xs@(_ :| [])  = xs
        dropHead (_ :| x : xs) = x :| xs

        go :: Time
           -- ^ current time
           -> [(Time, Int)]
           -- ^ results
           -> NonEmpty DiffTime
           -- ^ delays stack
           -> [Either Int Int]
           -> [(Time, Int)]
        go _t r _ds [] = reverse r
        go t r ds (Left _ : xs) =
          -- lefts cause delay
          go (NonEmpty.head ds `addTime` t)
             r
             (dropHead ds)
             xs
        go t r _ds (Right x : xs) =
          -- rights do not take time
          go t ((t, x) : r) ds0 xs

ex :: (MonadLabelledSTM m, MonadTimer m, MonadTraceSTM m, MonadSay m) => m ()
ex = do
    d <- registerDelay 1
    LazySTM.labelTVarIO d "delayVar"
    LazySTM.traceTVarIO d (\_ a -> pure (TraceString (show a)))
    atomically (LazySTM.readTVar d >>= check)
    say "Sink me!"
