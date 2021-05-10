{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
 module Test.Ouroboros.Network.PeerSelection.RootPeersDNS (
  tests
  ) where

import           Ouroboros.Network.PeerSelection.RootPeersDNS
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))

import           Data.Dynamic (fromDynamic, Typeable)
import           Data.Foldable (toList, foldl')
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Void (Void)
import           Data.IP (IPv4, toIPv4w, fromHostAddress)
import           Data.Time.Clock (picosecondsToDiffTime)
import           Data.ByteString.Char8 (pack)
import           Data.Set (Set)
import           Data.Time (DiffTime)
import           Network.Mux.Timeout (TimeoutFn)
import qualified Network.DNS.Resolver as DNSResolver
import           Network.DNS (DNSError(NameError, TimeoutExpired))
import           Network.Socket (SockAddr (..))

import           Control.Exception (throw)
import           Control.Monad.IOSim
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Tracer (Tracer(Tracer), contramap)
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO, readTVar,
                                                      MonadSTM (atomically),
                                                      writeTVar, StrictTVar)
import           Control.Monad.Class.MonadTime (Time)

import           Test.Ouroboros.Network.PeerSelection.Instances()
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           System.Random (StdGen, split, uniformR, mkStdGen)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection.RootPeersDNS"
  [ testGroup "localRootPeersProvider"
     [ testProperty "preserves groups and targets"
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
  ]

--
-- Mock Environment and Utils
--

data MockRoots = MockRoots {
  mockLocalRootPeers :: [(Int, Map RelayAddress PeerAdvertise)],
  mockDNSMap :: Map Domain [IPv4],
  mockStdGen :: StdGen
} deriving Show

-- | Generates MockRoots environments
--
genMockRoots :: Gen MockRoots
genMockRoots = sized $ \relaysNumber -> do
    relaysPerGroup <- chooseEnum (1, relaysNumber)

    relays <- vectorOf relaysNumber arbitrary
    targets <- vectorOf relaysNumber (chooseEnum (1, 5))

    peerAdvertise <- blocks relaysPerGroup
                      <$> vectorOf relaysNumber (arbitrary @PeerAdvertise)

        -- concat unique identifier to DNS domains to simplify tests
    let taggedRelays =
          zipWith
            (\tag rel
              -> case rel of
                   RelayDomain (DomainAddress domain port)
                     -> RelayDomain
                        (DomainAddress (domain <> (pack . show) tag)
                                       port)
                   x -> x
            )
            [(0 :: Int), 1 .. ]
            relays
        relaysBlocks = blocks relaysPerGroup taggedRelays
        relaysMap    = map Map.fromList $ zipWith zip relaysBlocks peerAdvertise

        localRootPeers = zip targets relaysMap

        domains = [ domain | RelayDomain (DomainAddress domain _) <- taggedRelays ]

        ipsPerDomain = 2

    ips <- blocks ipsPerDomain
            <$> vectorOf (ipsPerDomain * length domains) (toIPv4w <$> arbitrary)
    gen <- mkStdGen <$> arbitrary

    let dnsMap = Map.fromList $ zip domains ips

    return (MockRoots {
      mockLocalRootPeers = localRootPeers,
      mockDNSMap         = dnsMap,
      mockStdGen         = gen
    })
  where
    blocks _ [] = []
    blocks s l  = take s l : blocks s (drop s l)

instance Arbitrary MockRoots where
    arbitrary = genMockRoots

-- | Used for debugging in GHCI
--
simpleMockRoots :: MockRoots
simpleMockRoots = MockRoots localRootPeers dnsMap (mkStdGen 60)
  where
    localRootPeers =
      [ ( 2
        , Map.fromList
          [ ( RelayAddress (read "192.0.2.1")           (read "3333")
            , DoAdvertisePeer
            )
          , ( RelayDomain  (DomainAddress "test.domain" (read "4444"))
            , DoNotAdvertisePeer
            )
          ]
        )
      ]
    dnsMap = Map.fromList [ ("test.domain", [read "192.1.1.1", read "192.2.2.2"])
                          ]

-- | Mock DNSActions data structure for testing purposes.
-- Adds DNS Lookup function for IOSim with different timeout and lookup
-- delays for every attempt.
mockDNSActions :: forall exception s.
                  Map Domain [IPv4]
               -> StrictTVar (IOSim s) StdGen
               -> DNSActions () exception (IOSim s)
mockDNSActions dnsMap stdGenVar =
    DNSActions {
      dnsResolverResource,
      dnsAsyncResolverResource,
      dnsLookupAWithTTL
    }
 where
   genDiffTime :: Integer -> Integer -> StdGen -> DiffTime
   genDiffTime lo hi =
    picosecondsToDiffTime
    . fst
    . uniformR (lo * 1000000000, hi * 1000000000)

   dnsResolverResource      _ = return (constantResource ())
   dnsAsyncResolverResource _ = return (constantResource ())

   dnsLookupAWithTTL :: TimeoutFn (IOSim s)
                     -> resolvConf
                     -> resolver
                     -> Domain
                     -> IOSim s (Either DNSError [(IPv4, TTL)])
   dnsLookupAWithTTL timeout _ _ domain = do
     gen <- atomically $ readTVar stdGenVar

         -- Small probability of timeout
     let (_, (g', g'')) = split <$> split gen
         dtTimeout      = genDiffTime 110 300 g'
         dtDelay        = genDiffTime 20 120 g''

     atomically $ writeTVar stdGenVar g'

     dnsLookup <-
        timeout dtTimeout $ do
          MonadTimer.threadDelay dtDelay
          case Map.lookup domain dnsMap of
            Nothing -> return (Left NameError)
            Just x  -> return (Right (map (\a -> (a, 0)) x))

     case dnsLookup of
       Nothing -> return (Left TimeoutExpired)
       Just a  -> return a

-- | 'localRootPeersProvider' running with a given MockRoots env
--
mockLocalRootPeersProvider :: forall s. MockRoots -> IOSim s Void
mockLocalRootPeersProvider (MockRoots localRootPeers dnsMap stdGen) = do
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty
      genVar <- newTVarIO stdGen

      localRootPeersProvider tracerTraceLocalRoots
                             MonadTimer.timeout
                             DNSResolver.defaultResolvConf
                             (mockDNSActions dnsMap genVar)
                             (readTVar localRootPeersVar)
                             resultVar

-- | 'publicRootPeersProvider' running with a given MockRoots env
--
mockPublicRootPeersProvider :: forall s. MockRoots
                            -> Int
                            -> IOSim s (Set SockAddr, DiffTime)
mockPublicRootPeersProvider (MockRoots localRootPeers dnsMap stdGen) n = do
      localRootPeersVar <- newTVarIO (concatMap (Map.keys . snd) localRootPeers)
      genVar <- newTVarIO stdGen

      publicRootPeersProvider tracerTracePublicRoots
                              MonadTimer.timeout
                              DNSResolver.defaultResolvConf
                              (readTVar localRootPeersVar)
                              (mockDNSActions @Failure dnsMap genVar)
                              ($ n)

-- | 'resolveDomainAddresses' running with a given MockRoots env
--
mockResolveDomainAddresses :: forall s. MockRoots
                           -> IOSim s (Map DomainAddress (Set SockAddr))
mockResolveDomainAddresses (MockRoots localRootPeers dnsMap stdGen) = do
      genVar <- newTVarIO stdGen

      resolveDomainAddresses tracerTracePublicRoots
                             MonadTimer.timeout
                             DNSResolver.defaultResolvConf
                             (mockDNSActions @Failure dnsMap genVar)
                             [ domain
                             | (_, m) <- localRootPeers
                             , RelayDomain domain <- Map.keys m ]

--
-- Utils for properties
--

data TestTraceEvent exception = RootPeerDNSLocal  (TraceLocalRootPeers exception)
                              | RootPeerDNSPublic TracePublicRootPeers
  deriving Show

tracerTraceLocalRoots :: Tracer (IOSim s) (TraceLocalRootPeers Failure)
tracerTraceLocalRoots = contramap RootPeerDNSLocal tracerTestTraceEvent

tracerTracePublicRoots :: Tracer (IOSim s) TracePublicRootPeers
tracerTracePublicRoots = contramap RootPeerDNSPublic tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) (TestTraceEvent Failure)
tracerTestTraceEvent = dynamicTracer

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectRootPeerDNSTraceEvents :: Trace a -> [(Time, TestTraceEvent Failure)]
selectRootPeerDNSTraceEvents = go
  where
    go (Trace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (Trace _ _ _ _ trace)      =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

selectLocalRootPeersEvents :: [(Time, TestTraceEvent Failure)]
                           -> [(Time, TraceLocalRootPeers Failure)]
selectLocalRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSLocal e) <- trace ]

selectLocalRootGroupsEvents :: [(Time, TraceLocalRootPeers Failure)]
                            -> [(Time, Seq (Int, Map SockAddr PeerAdvertise))]
selectLocalRootGroupsEvents trace = [ (t, e) | (t, TraceLocalRootGroups e) <- trace ]

selectLocalRootResultEvents :: [(Time, TraceLocalRootPeers Failure)]
                            -> [(Time, (Domain, [IPv4]))]
selectLocalRootResultEvents trace = [ (t, (domain, map fst r))
                                    | (t, TraceLocalRootResult (DomainAddress domain _) r) <- trace ]

selectPublicRootPeersEvents :: [(Time, TestTraceEvent Failure)]
                            -> [(Time, TracePublicRootPeers)]
selectPublicRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSPublic e) <- trace ]

selectPublicRootFailureEvents :: [(Time, TracePublicRootPeers)]
                              -> [(Time, Domain)]
selectPublicRootFailureEvents trace = [ (t, domain)
                                      | (t, TracePublicRootFailure domain _) <- trace ]

selectPublicRootResultEvents :: [(Time, TracePublicRootPeers)]
                             -> [(Time, (Domain, [IPv4]))]
selectPublicRootResultEvents trace = [ (t, (domain, map fst r))
                                     | (t, TracePublicRootResult domain r) <- trace ]

--
-- Local Root Peers Provider Tests
--

-- | The 'localRootPeersProvider' should preserve the local root peers
-- group number and respective targets. This property tests whether local
-- root peer groups update due to DNS resolution results, does not alter
-- the initial groups configuration.
--
prop_local_preservesGroupNumberAndTargets :: MockRoots -> Property
prop_local_preservesGroupNumberAndTargets mockRoots@(MockRoots lrp _ _) =
    let tr = take 1000
              $ selectLocalRootGroupsEvents
              $ selectLocalRootPeersEvents
              $ selectRootPeerDNSTraceEvents
              $ runSimTrace
              $ mockLocalRootPeersProvider mockRoots

        -- For all LocalRootGroup results, the number of groups should be
        -- preserved, i.e. no new groups are added nor deleted along the
        -- trace by localRootPeersProvider.
        preservesGroupNumber = all ((== length lrp) . length . snd) tr

        -- For all LocalRootGroup results, the targets for each group
        -- should be preserved, i.e. targets are not modified along the
        -- trace by localRootPeersProvider.
        preservesTargets     = all (all (\(a, b) -> fst a == fst b))
                                   [ zip lrp (toList r) | r <- map snd tr ]

     in preservesGroupNumber .&&. preservesTargets

-- | The 'localRootPeersProvider' should be able to resolve DNS domains
-- correctly, assuming the domain maps to any IP address. This property
-- tests whether 'localRootPeersProvider' is capable of eventually resolving
-- domain addresses even after having failed to do so in the first attempt.
--
prop_local_resolvesDomainsCorrectly :: MockRoots -> Property
prop_local_resolvesDomainsCorrectly mockRoots@(MockRoots _ dnsMap _) =
    let tr = take 1000
              $ selectLocalRootResultEvents
              $ selectLocalRootPeersEvents
              $ selectRootPeerDNSTraceEvents
              $ runSimTrace
              $ mockLocalRootPeersProvider mockRoots

        finalResultMap = Map.fromList $ map snd tr

     in finalResultMap === dnsMap

-- | The 'localRootPeersProvider' after resolving a DNS domain address
-- should update the local result group list correctly, i.e. add the
-- resolved ip addresses to the correct group where the domain address was
-- (in the initial configuration specification). This property tests whether
-- after a successful DNS lookup the result list is updated correctly.
prop_local_updatesDomainsCorrectly :: MockRoots -> Property
prop_local_updatesDomainsCorrectly mockRoots@(MockRoots lrp _ _) =
    let tr = take 1000
              $ selectLocalRootPeersEvents
              $ selectRootPeerDNSTraceEvents
              $ runSimTrace
              $ mockLocalRootPeersProvider mockRoots
        r = foldl' (\(b, (t, x)) (t', y) ->
                    case (x, y) of
                      -- Last DNS lookup result   , Current result groups value
                      (TraceLocalRootResult da res, TraceLocalRootGroups lrpg) ->
                            -- create and index db for each group
                        let db = zip [0,1..] lrp
                            -- since our MockRoots generator generates
                            -- unique domain addresses we can look for
                            -- which group index does a particular domain
                            -- address belongs
                            index = foldr (\(i, (_, m)) prev ->
                                            case Map.lookup (RelayDomain da) m of
                                              Nothing -> prev
                                              Just _  -> i
                                          ) (-1) db
                            -- Get all IPv4 present in group at position
                            -- 'index'
                            ipsAtIndex = map (\sockAddr ->
                                             case sockAddr of
                                               SockAddrInet _ hostAddr
                                                 -> fromHostAddress hostAddr
                                               _ -> error "Impossible happened!"

                                         ) $ Map.keys
                                           $ snd
                                           $ lrpg `Seq.index` index
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


--
-- Public Root Peers Provider Tests
--

-- | The 'publicRootPeersProvider' should be able to resolve DNS domains
-- correctly, assuming the domain maps to any IP address. This property
-- tests whether 'publicRootPeersProvider' is capable of eventually resolving domain
-- addresses even after having failed to do so in the first attempt, in
-- a bounded amount of time.
--
prop_public_resolvesDomainsCorrectly :: MockRoots -> Int -> Property
prop_public_resolvesDomainsCorrectly mockRoots@(MockRoots _ dnsMap _) n =
    within 1_000_000 $
      lookupLoop mockRoots dnsMap === dnsMap
  where
    -- Perform public root DNS lookup until no failures
    lookupLoop :: MockRoots -> Map Domain [IPv4] -> Map Domain [IPv4]
    lookupLoop mr res =
      let successes = selectPublicRootResultEvents
                        $ selectPublicRootPeersEvents
                        $ selectRootPeerDNSTraceEvents
                        $ runSimTrace
                        $ mockPublicRootPeersProvider mr n

          failures = selectPublicRootFailureEvents
                      $ selectPublicRootPeersEvents
                      $ selectRootPeerDNSTraceEvents
                      $ runSimTrace
                      $ mockPublicRootPeersProvider mr n

          successesMap = Map.fromList $ map snd successes

          -- Update MockRoots with only the RelayAddresses that failed the
          -- DNS timeouts
          failuresMap  =
            [ ( i
              , Map.fromList [ (RelayDomain domain, pa)
                             | (RelayDomain domain, pa) <- Map.toList m
                             , daDomain domain `elem` map snd failures
                             ]
              )
            | (i, m) <- mockLocalRootPeers mr
            ]
          newMR = mr { mockLocalRootPeers = failuresMap }

       in if null failures
            then res
            else lookupLoop newMR (res <> successesMap)
