{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

module Test.Ouroboros.Network.PeerSelection.RootPeersDNS
  ( tests
  , mockDNSActions
  , MockRoots (..)
  , DNSTimeout (..)
  , DNSLookupDelay (..)
  , DelayAndTimeoutScripts (..)
  ) where

import Control.Applicative (Alternative)
import Control.Monad (forever, replicateM_)
import Data.ByteString.Char8 (pack)
import Data.Dynamic (Typeable, fromDynamic)
import Data.Either (rights)
import Data.Foldable as Foldable (foldl')
import Data.Function (fix)
import Data.Functor (void)
import Data.IP (fromHostAddress, toIPv4w, toSockAddr)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Void (Void)
import Network.DNS (DNSError (NameError, TimeoutExpired), Domain, TTL)
import Network.DNS.Resolver qualified as DNSResolver
import Network.Socket (SockAddr (..))

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

import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (HotValency (..),
           WarmValency (..))
import Ouroboros.Network.Testing.Data.Script (Script (Script), initScript',
           scriptHead, singletonScript, stepScript')
import Test.Ouroboros.Network.PeerSelection.Instances ()
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Cardano.Node.PeerSelection.PeerTrustable (PeerTrustable (..))

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

data MockRoots = MockRoots {
    mockLocalRootPeers        :: [( HotValency
                                  , WarmValency
                                  , Map RelayAccessPoint (PeerAdvertise, PeerTrustable))]
  , mockLocalRootPeersDNSMap  :: Script (Map Domain [(IP, TTL)])
  , mockPublicRootPeers       :: Map RelayAccessPoint PeerAdvertise
  , mockPublicRootPeersDNSMap :: Script (Map Domain [(IP, TTL)])
  }
  deriving Show

-- | Generates MockRoots environments
--
genMockRoots :: Gen MockRoots
genMockRoots = sized $ \relaysNumber -> do
    -- Generate LocalRootPeers
    --
    relaysPerGroup <- chooseEnum (1, relaysNumber `div` 3)

    localRootRelays <- vectorOf relaysNumber arbitrary
    targets <- vectorOf relaysNumber genTargets

    peerAdvertise <- blocks relaysPerGroup
                      <$> vectorOf relaysNumber arbitrary

        -- concat unique identifier to DNS domains to simplify tests
    let taggedLocalRelays = tagRelays localRootRelays
        localRelaysBlocks = blocks relaysPerGroup taggedLocalRelays
        localRelaysMap    = map Map.fromList $ zipWith zip localRelaysBlocks
                                                           peerAdvertise
        localRootPeers    = zipWith (\(h, w) g -> (h, w, g)) targets localRelaysMap
        localRootDomains  = [ domain
                            | RelayAccessDomain domain _ <- taggedLocalRelays ]

        ipsPerDomain = 2

    lrpDNSMap <- Script . NonEmpty.fromList
              <$> listOf1 (genDomainLookupTable ipsPerDomain localRootDomains)

    -- Generate PublicRootPeers
    --
    publicRootRelays <- vectorOf relaysNumber arbitrary
    publicRootPeersAdvertise <- vectorOf relaysNumber arbitrary

    let publicRootPeers =
          Map.fromList (zip (tagRelays publicRootRelays)
                            publicRootPeersAdvertise)

        publicRootDomains = [ domain
                            | (RelayAccessDomain domain _, _)
                                <- Map.assocs publicRootPeers ]

    publicRootPeersDNSMap <- Script . NonEmpty.fromList
                          <$> listOf1 (genDomainLookupTable ipsPerDomain publicRootDomains)

    return (MockRoots {
      mockLocalRootPeers        = localRootPeers,
      mockLocalRootPeersDNSMap  = lrpDNSMap,
      mockPublicRootPeers       = publicRootPeers,
      mockPublicRootPeersDNSMap = publicRootPeersDNSMap
    })
  where
    genTargets :: Gen (HotValency, WarmValency)
    genTargets = do
      warmValency <- WarmValency <$> chooseEnum (1, 5)
      hotValency <- HotValency <$> chooseEnum (1, getWarmValency warmValency)
      return (hotValency, warmValency)

    genDomainLookupTable :: Int -> [Domain] -> Gen (Map Domain [(IP, TTL)])
    genDomainLookupTable ipsPerDomain localRootDomains = do
      localRootDomainIPs <- blocks ipsPerDomain
              -- Modules under test do not differ by IP version so we only
              -- generate IPv4 addresses.
              <$> vectorOf (ipsPerDomain * length localRootDomains)
                           (IPv4 . toIPv4w <$> arbitrary)
      localRootDomainTTLs <- blocks ipsPerDomain
              <$> vectorOf (ipsPerDomain * length localRootDomains)
                           (arbitrary :: Gen TTL)

      let localRootDomainsIP_TTls = zipWith zip localRootDomainIPs localRootDomainTTLs
          lrpDNSMap = Map.fromList $ zip localRootDomains localRootDomainsIP_TTls

      return lrpDNSMap

    tagRelays relays =
      zipWith
        (\tag rel
          -> case rel of
               RelayAccessDomain domain port
                 -> RelayAccessDomain (domain <> (pack . show) tag) port
               x -> x
        )
        [(0 :: Int), 1 .. ]
        relays

    blocks _ [] = []
    blocks s l  = take s l : blocks s (drop s l)

instance Arbitrary MockRoots where
    arbitrary = genMockRoots
    shrink roots@MockRoots { mockLocalRootPeers
                           , mockLocalRootPeersDNSMap
                           , mockPublicRootPeers
                           , mockPublicRootPeersDNSMap
                           } =
      [ roots { mockLocalRootPeers        = lrp
              , mockLocalRootPeersDNSMap  = lrpDNSMap
              }
      | lrp <- shrinkList (const []) mockLocalRootPeers,
        let lrpDomains =
              Set.fromList [ domain
                           | RelayAccessDomain domain _
                              <- concatMap (Map.keys . thrd) lrp ]
            lrpDNSMap  = (`Map.restrictKeys` lrpDomains)
                       <$> mockLocalRootPeersDNSMap
      ] ++
      [ roots { mockPublicRootPeers       = prp
              , mockPublicRootPeersDNSMap = prpDNSMap
              }
      | prp <- shrink mockPublicRootPeers,
        let prpDomains = Set.fromList [ domain
                                      | (RelayAccessDomain domain _, _)
                                          <- Map.assocs prp ]
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
            , (DoAdvertisePeer, IsNotTrustable)
            )
          , ( RelayAccessDomain  "test.domain"      (read "4444")
            , (DoNotAdvertisePeer, IsNotTrustable)
            )
          ]
        )
      ]
    dnsMap = singletonScript $ Map.fromList
              [ ("test.domain", [read "192.1.1.1", read "192.2.2.2"])
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
mockDNSActions :: forall exception m.
                  ( MonadDelay m
                  , MonadTimer m
                  )
               => StrictTVar m (Map Domain [(IP, TTL)])
               -> StrictTVar m (Script DNSTimeout)
               -> StrictTVar m (Script DNSLookupDelay)
               -> DNSActions () exception m
mockDNSActions dnsMapVar dnsTimeoutScript dnsLookupDelayScript =
    DNSActions {
      dnsResolverResource,
      dnsAsyncResolverResource,
      dnsLookupWithTTL
    }
 where
   dnsResolverResource      _ = return (Right <$> constantResource ())
   dnsAsyncResolverResource _ = return (Right <$> constantResource ())

   dnsLookupWithTTL :: resolvConf
                    -> resolver
                    -> Domain
                    -> m ([DNSError], [(IP, TTL)])
   dnsLookupWithTTL _ _ domain = do
     dnsMap <- readTVarIO dnsMapVar
     DNSTimeout dnsTimeout <- stepScript' dnsTimeoutScript
     DNSLookupDelay dnsLookupDelay <- stepScript' dnsLookupDelayScript

     dnsLookup <-
        MonadTimer.timeout dnsTimeout $ do
          MonadTimer.threadDelay dnsLookupDelay
          case Map.lookup domain dnsMap of
            Nothing -> return (Left NameError)
            Just x  -> return (Right x)

     case dnsLookup of
       Nothing        -> return ([TimeoutExpired], [])
       Just (Left e)  -> return ([e], [])
       Just (Right a) -> return ([], a)

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
                           => Tracer m (TraceLocalRootPeers PeerTrustable SockAddr Failure)
                           -> MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> m ()
mockLocalRootPeersProvider tracer (MockRoots localRootPeers dnsMapScript _ _)
                           dnsTimeoutScript dnsLookupDelayScript = do
      dnsMapScriptVar <- initScript' dnsMapScript
      dnsMap <- stepScript' dnsMapScriptVar
      dnsMapVar <- newTVarIO dnsMap

      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty
      _ <- labelTVarIO resultVar "resultVar"
      _ <- traceTVarIO resultVar
                       (\_ a -> pure $ TraceDynamic (LocalRootPeersResults a))
      withAsync (updateDNSMap dnsMapScriptVar dnsMapVar) $ \_ -> do
        void $ MonadTimer.timeout 3600 $
          localRootPeersProvider tracer
                                 (curry toSockAddr)
                                 DNSResolver.defaultResolvConf
                                 (mockDNSActions dnsMapVar
                                                 dnsTimeoutScriptVar
                                                 dnsLookupDelayScriptVar)
                                 (readTVar localRootPeersVar)
                                 resultVar
        -- if there's no dns domain, `localRootPeersProvider` will never write
        -- to `resultVar`; thus the `traceTVarIO` callback will never execute.
        -- By reading & writing to the `TVar` we are forcing it to run at least
        -- once.
        atomically $ readTVar resultVar >>= writeTVar resultVar
  where
    updateDNSMap :: StrictTVar m (Script (Map Domain [(IP, TTL)]))
                 -> StrictTVar m (Map Domain [(IP, TTL)])
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
                            => Tracer m TracePublicRootPeers
                            -> MockRoots
                            -> Script DNSTimeout
                            -> Script DNSLookupDelay
                            -> ((Int -> m (Map SockAddr PeerAdvertise, DiffTime)) -> m a)
                            -> m ()
mockPublicRootPeersProvider tracer (MockRoots _ _ publicRootPeers dnsMapScript)
                            dnsTimeoutScript dnsLookupDelayScript action = do
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

        publicRootPeersProvider tracer
                                (curry toSockAddr)
                                dnsSemaphore
                                DNSResolver.defaultResolvConf
                                (readTVar publicRootPeersVar)
                                (mockDNSActions @Failure
                                                dnsMapVar
                                                dnsTimeoutScriptVar
                                                dnsLookupDelayScriptVar)
                                action

-- | 'resolveDomainAddresses' running with a given MockRoots env
--
mockResolveLedgerPeers :: ( MonadAsync m
                         , MonadDelay m
                         , MonadThrow m
                         , MonadTimer m
                         )
                       => Tracer m TraceLedgerPeers
                       -> MockRoots
                       -> Script DNSTimeout
                       -> Script DNSLookupDelay
                       -> m (Map DomainAccessPoint (Set SockAddr))
mockResolveLedgerPeers tracer (MockRoots _ _ publicRootPeers dnsMapScript)
                       dnsTimeoutScript dnsLookupDelayScript = do
      dnsMapScriptVar <- initScript' dnsMapScript
      dnsMap <- stepScript' dnsMapScriptVar
      dnsMapVar <- newTVarIO dnsMap
      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      resolveLedgerPeers tracer
                         (curry toSockAddr)
                         dnsSemaphore
                         DNSResolver.defaultResolvConf
                         (mockDNSActions @Failure dnsMapVar
                                                  dnsTimeoutScriptVar
                                                  dnsLookupDelayScriptVar)
                         [ domain
                         | (RelayDomainAccessPoint domain, _)
                              <- Map.assocs publicRootPeers ]

--
-- Utils for properties
--

data TestTraceEvent = RootPeerDNSLocal  (TraceLocalRootPeers PeerTrustable SockAddr Failure)
                    | LocalRootPeersResults [(HotValency, WarmValency, Map SockAddr (PeerAdvertise, PeerTrustable))]
                    | RootPeerDNSPublic TracePublicRootPeers
  deriving (Show, Typeable)

tracerTraceLocalRoots :: Tracer (IOSim s) (TraceLocalRootPeers PeerTrustable SockAddr Failure)
tracerTraceLocalRoots = contramap RootPeerDNSLocal tracerTestTraceEvent

tracerTracePublicRoots :: Tracer (IOSim s) TracePublicRootPeers
tracerTracePublicRoots = contramap RootPeerDNSPublic tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) TestTraceEvent
tracerTestTraceEvent = dynamicTracer

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectRootPeerDNSTraceEvents :: SimTrace a -> [(Time, TestTraceEvent)]
selectRootPeerDNSTraceEvents = go
  where
    go (SimTrace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimPORTrace t _ _ _ (EventLog e) trace)
     | Just x <- fromDynamic e       = (t,x) : go trace
    go (SimTrace _ _ _ _ trace)      =         go trace
    go (SimPORTrace _ _ _ _ _ trace) =         go trace
    go (TraceMainException _ _ e _)  = throw e
    go (TraceDeadlock      _   _)    = [] -- expected result in many cases
    go  TraceMainReturn {}           = []
    go (TraceInternalError e)        = error ("IOSim: " ++ e)
    go TraceLoop                     = error "IOSimPOR step time limit exceeded"

selectLocalRootPeersEvents :: [(Time, TestTraceEvent)]
                           -> [(Time, TraceLocalRootPeers PeerTrustable SockAddr Failure)]
selectLocalRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSLocal e) <- trace ]

selectLocalRootPeersResults :: [(Time, TestTraceEvent)]
                            -> [(Time, [(HotValency, WarmValency, Map SockAddr (PeerAdvertise, PeerTrustable))])]
selectLocalRootPeersResults trace = [ (t, r) | (t, LocalRootPeersResults r) <- trace ]

selectLocalRootGroupsEvents :: [(Time, TraceLocalRootPeers PeerTrustable SockAddr Failure)]
                            -> [(Time, [( HotValency
                                        , WarmValency
                                        , Map SockAddr (PeerAdvertise, PeerTrustable))])]
selectLocalRootGroupsEvents trace = [ (t, e) | (t, TraceLocalRootGroups e) <- trace ]

selectLocalRootResultEvents :: [(Time, TraceLocalRootPeers PeerTrustable SockAddr Failure)]
                            -> [(Time, (Domain, [IP]))]
selectLocalRootResultEvents trace = [ (t, (domain, map fst r))
                                    | (t, TraceLocalRootResult (DomainAccessPoint domain _) r) <- trace ]

selectPublicRootPeersEvents :: [(Time, TestTraceEvent)]
                            -> [(Time, TracePublicRootPeers)]
selectPublicRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSPublic e) <- trace ]

selectPublicRootFailureEvents :: [(Time, TracePublicRootPeers)]
                              -> [(Time, Domain)]
selectPublicRootFailureEvents trace = [ (t, domain)
                                      | (t, TracePublicRootFailure domain _) <- trace ]

selectPublicRootResultEvents :: [(Time, TracePublicRootPeers)]
                             -> [(Time, (Domain, [IP]))]
selectPublicRootResultEvents trace = [ (t, (domain, map fst r))
                                     | (t, TracePublicRootResult domain r) <- trace ]

--
-- Local Root Peers Provider Tests
--

-- | The 'localRootPeersProvider' should use the IP addresses. This property
-- tests whether local root peer groups contain the IP addresses provided.
--
prop_local_preservesIPs :: MockRoots
                        -> Script DNSTimeout
                        -> Script DNSLookupDelay
                        -> Property
prop_local_preservesIPs mockRoots@(MockRoots localRoots _ _ _)
                        dnsTimeoutScript
                        dnsLookupDelayScript =
    let tr = selectLocalRootPeersResults
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

     in counterexample (intercalate "\n" $ map show tr)
      $ classify (length tr > 0) "Actually testing something"
      $ checkAll tr
  where
    checkAll :: [(Time, [( HotValency
                         , WarmValency
                         , Map SockAddr (PeerAdvertise, PeerTrustable))])]
             -> Property
    checkAll [] = property True
    checkAll (x:t) =
      let thrd (_, _, c) = c
          -- get local root ip addresses
          localRootAddresses :: [(a, b, Map RelayAccessPoint (PeerAdvertise, PeerTrustable))]
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
                                            , Map SockAddr (PeerAdvertise, PeerTrustable))])
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
                                          -> Property
prop_local_preservesGroupNumberAndTargets mockRoots@(MockRoots lrp _ _ _)
                                          dnsTimeoutScript
                                          dnsLookupDelayScript =
    let tr = selectLocalRootPeersResults
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

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
                                    -> Property
prop_local_resolvesDomainsCorrectly mockRoots@(MockRoots localRoots lDNSMap _ _)
                                    dnsTimeoutScript
                                    dnsLookupDelayScript =
    let mockRoots' =
          mockRoots { mockLocalRootPeersDNSMap =
                        singletonScript (scriptHead lDNSMap)
                    }
        tr = selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots'
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

        -- local root domains
        localRootDomains :: Set Domain
        localRootDomains =
          Set.fromList
          [ domain
          | (_, _, m) <- localRoots
          , RelayAccessDomain domain _ <- Map.keys m
          ]

        -- domains that were resolved during simulation
        resultMap :: Set Domain
        resultMap = Set.fromList
                  $ map (fst . snd)
                  $ selectLocalRootResultEvents
                  $ tr

        -- all domains that could have been resolved in each script
        maxResultMap :: Script (Set Domain)
        maxResultMap = Map.keysSet
                     . (`Map.restrictKeys` localRootDomains)
                     <$> lDNSMap

        -- all domains that were tried to resolve during the simulation
        allTriedDomains :: Set Domain
        allTriedDomains
          = Set.fromList
          $ catMaybes
          [ mbDomain
          | (_, ev) <- tr
          , let mbDomain = case ev of
                  TraceLocalRootResult  (DomainAccessPoint domain _)  _ -> Just domain
                  TraceLocalRootFailure (DomainAccessPoint domain _)  _ -> Just domain
                  TraceLocalRootError   (DomainAccessPoint _domain _) _ -> Nothing
                  _                                                     -> Nothing

          ]


    in
      -- we verify that we tried to resolve all local root domains, and that the
      -- resolved ones are a subset of `maxResultMap`
           localRootDomains === allTriedDomains
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
                                   -> Property
prop_local_updatesDomainsCorrectly mockRoots@(MockRoots lrp _ _ _)
                                   dnsTimeoutScript
                                   dnsLookupDelayScript =
    let tr = selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

        r = Foldable.foldl' (\(b, (t, x)) (t', y) ->
                    case (x, y) of
                      -- Last result groups value, Current result groups value
                      (TraceLocalRootGroups lrpg, TraceLocalRootGroups lrpg') ->
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
                      (TraceLocalRootResult da res, TraceLocalRootGroups lrpg) ->
                            -- create and index db for each group
                        let db = zip [0,1..] lrp
                            -- since our MockRoots generator generates
                            -- unique domain addresses we can look for
                            -- which group index does a particular domain
                            -- address belongs
                            index = foldr (\(i, (_, _, m)) prev ->
                                            case Map.lookup (RelayDomainAccessPoint da) m of
                                              Nothing -> prev
                                              Just _  -> i
                                          ) (-1) db
                            -- Get all IPs present in group at position
                            -- 'index'
                            ipsAtIndex = map (\sockAddr ->
                                             case sockAddr of
                                               SockAddrInet _ hostAddr
                                                 -> IPv4 $ fromHostAddress hostAddr
                                               _ -> error "Impossible happened!"

                                         ) $ Map.keys
                                           $ thrd
                                           $ lrpg !! index :: [IP]
                            -- Check if all ips from the previous DNS
                            -- lookup result are present in the current
                            -- result group at the correct index
                            arePresent = all ((`elem` ipsAtIndex) . fst) res
                         in (arePresent && b, (t', y))

                      (TraceLocalRootResult _ _, _) -> (b, (t, x))
                      (_, _)                        -> (b, (t', y))
                   )
              (True, head tr)
              (tail tr)
     in property (fst r)
  where
    thrd (_, _, c) = c

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

      in (DelayAndTimeoutScripts lookupScript' timeoutScript)

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
                                     -> Property
prop_public_resolvesDomainsCorrectly
    mockRoots@(MockRoots _ _ _ pDNSMap)
    (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
    n
  =
    let mockRoots' =
          mockRoots { mockPublicRootPeersDNSMap =
                        singletonScript (scriptHead pDNSMap)
                    }
        tr = runSimTrace
           $ mockPublicRootPeersProvider tracerTracePublicRoots
                                         mockRoots'
                                         dnsTimeoutScript
                                         dnsLookupDelayScript
                                         ($ n)

        successes = selectPublicRootResultEvents
                  $ selectPublicRootPeersEvents
                  $ selectRootPeerDNSTraceEvents
                  $ tr

        successesMap = Map.fromList $ map snd successes

     in counterexample (show successes)
      $ successesMap == (map fst <$> Map.unions pDNSMap)


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
