{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Network.DNS.Resolver as DNSResolver
import           Network.DNS (DNSError(NameError, TimeoutExpired))
import           Network.Socket (SockAddr (..))

import           Control.Exception (throw)
import           Control.Monad.IOSim
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Tracer (Tracer(Tracer), contramap)
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO, readTVar, MonadSTM (atomically), writeTVar)
import           Control.Monad.Class.MonadTime (Time)

import           Test.Ouroboros.Network.PeerSelection.Instances()
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           System.Random (StdGen, split, uniformR, mkStdGen)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "RootPeersDNS"
  [ testProperty "preserves groups and targets" prop_local_preservesGroupNumberAndTargets
  , testProperty "resolves domains correctly" prop_local_resolvesDomainsCorrectly
  , testProperty "updates domains correctly" prop_local_updatesDomainsCorrectly
  ]

data MockRoots = MockRoots {
  mockLocalRootPeers :: [(Int, Map RelayAddress PeerAdvertise)],
  mockDNSMap :: Map Domain [IPv4],
  mockStdGen :: StdGen
} deriving Show

genMockRoots :: Gen MockRoots
genMockRoots = sized $ \relaysNumber -> do
    relaysPerGroup <- chooseEnum (1 + (relaysNumber `div` 4), 1 + (relaysNumber `div` 2))

    relays <- vectorOf relaysNumber arbitrary
    targets <- vectorOf (1 + (relaysNumber `div` relaysPerGroup)) (chooseEnum (1, 5))

    peerAdvertise <- blocks relaysPerGroup <$> vectorOf relaysNumber (arbitrary @PeerAdvertise)

        -- concat unique identifier to DNS domains to simplify tests
    let taggedRelays = zipWith
                        (\tag rel
                              -> case rel of
                                   RelayDomain (DomainAddress domain port)
                                     -> RelayDomain (DomainAddress (domain <> (pack . show) tag) port)
                                   x -> x
                        )
                        [0, 1 .. ]
                        relays
        relaysBlocks = blocks relaysPerGroup taggedRelays
        relaysMap    = map Map.fromList $ zipWith zip relaysBlocks peerAdvertise

        localRootPeers = zip targets relaysMap

        domains = [ domain | RelayDomain (DomainAddress domain _) <- taggedRelays ]

        ipsPerDomain = 2

    ips <- blocks ipsPerDomain <$> vectorOf (ipsPerDomain * length domains) (toIPv4w <$> arbitrary)
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

simpleMockRoots :: MockRoots
simpleMockRoots = MockRoots localRootPeers dnsMap (mkStdGen 60)
  where
    localRootPeers = [ (2, Map.fromList [ (RelayAddress (read "192.0.2.1")           (read "3333") , DoAdvertisePeer)
                                        , (RelayDomain  (DomainAddress "test.domain" (read "4444")), DoNotAdvertisePeer) ]
                       )
                  ]
    dnsMap = Map.fromList [ ("test.domain", [read "192.1.1.1", read "192.2.2.2"])
                          ]

mockLocalRootPeersProvider :: forall s. MockRoots -> IOSim s Void
mockLocalRootPeersProvider (MockRoots localRootPeers dnsMap stdGen) = do
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty
      genVar <- newTVarIO stdGen

      localRootPeersProvider tracerTracePeerSelection
                             MonadTimer.timeout
                             DNSResolver.defaultResolvConf
                             resultVar
                             (readTVar localRootPeersVar)
                             (dnsActions genVar)
 where
   genDiffTime lo hi = picosecondsToDiffTime . fst . uniformR (lo * 1000000, hi * 1000000)

   dnsResolverResource _ = return (constantResource ())
   dnsAsyncResolverResource _ = return (constantResource ())
   dnsLookupAWithTTL genVar timeout _ _ domain = do
     gen <- atomically $ readTVar genVar

     let (dtTimeout, dtDelay) = (genDiffTime 100 300 gen, genDiffTime 0 400 gen)
         (_, g') = split gen

     atomically @(IOSim s) $ writeTVar genVar g'

     dnsLookup <-
        timeout dtTimeout $ do
          MonadTimer.threadDelay dtDelay
          case Map.lookup domain dnsMap of
            Nothing -> return (Left NameError)
            Just x  -> return (Right (map (\a -> (a, 0)) x))

     case dnsLookup of
       Nothing -> return (Left TimeoutExpired)
       Just a  -> return a

   dnsActions genVar = DNSActions {
                  dnsResolverResource,
                  dnsAsyncResolverResource,
                  dnsLookupAWithTTL = dnsLookupAWithTTL genVar
                }


--
-- Utils for properties
--

data TestTraceEvent exception = RootPeerDNSLocal  (TraceLocalRootPeers exception)
                              | RootPeerDNSPublic TracePublicRootPeers
  deriving Show

tracerTracePeerSelection :: Tracer (IOSim s) (TraceLocalRootPeers Failure)
tracerTracePeerSelection = contramap RootPeerDNSLocal tracerTestTraceEvent

tracerDebugPeerSelection :: Tracer (IOSim s) TracePublicRootPeers
tracerDebugPeerSelection = contramap RootPeerDNSPublic
                                     tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) (TestTraceEvent Failure)
tracerTestTraceEvent = dynamicTracer

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectRootPeerDNSTraceEvents :: Trace a -> [(Time, TestTraceEvent Failure)]
selectRootPeerDNSTraceEvents = go
  where
    go (Trace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (Trace _ _ _ _ trace)     =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

selectLocalRootPeersEvents :: [(Time, TestTraceEvent Failure)] -> [(Time, TraceLocalRootPeers Failure)]
selectLocalRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSLocal e) <- trace ]

selectLocalRootGroupsEvents :: [(Time, TraceLocalRootPeers Failure)] -> [(Time, Seq (Int, Map SockAddr PeerAdvertise))]
selectLocalRootGroupsEvents trace = [ (t, e) | (t, TraceLocalRootGroups e) <- trace ]

selectLocalRootResultEvents :: [(Time, TraceLocalRootPeers Failure)] -> [(Time, (Domain, [IPv4]))]
selectLocalRootResultEvents trace = [ (t, (domain, map fst r))
                                    | (t, TraceLocalRootResult (DomainAddress domain _) r) <- trace ]

-- TraceLocalRootResult  DomainAddress [(IPv4, DNS.TTL)]

-- | The 'localRootPeersProvider' should preserve the local root peers
-- group number and respective targets. This property tests wether local
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
        preservesTargets     = all (all (\(a, b) -> fst a == fst b)) [ zip lrp (toList r) | r <- map snd tr ]

     in property (preservesGroupNumber && preservesTargets)

-- | The 'localRootPeersProvider' should be able to resolve DNS domains
-- correctly, assuming the domain maps to any IP address. This property
-- tests wether 'localRootPeersProvider' is capable of eventually resolving domain
-- addresses even after having failed to do so in the first attempt.
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

     in property (finalResultMap == dnsMap)

-- | The 'localRootPeersProvider' after resolving a DNS domain address
-- should update the local result group list correctly, i.e. add the
-- resolved ip addresses to the correct group where the domain address was
-- (in the initial configuration specification). This property tests wether
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
                                               SockAddrInet _ hostAddr -> fromHostAddress hostAddr
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
