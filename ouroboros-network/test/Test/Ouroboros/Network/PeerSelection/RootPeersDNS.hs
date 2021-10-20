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
import           Data.Functor (void)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.IP (IPv4, toIPv4w, fromHostAddress, toSockAddr)
import           Data.Time.Clock (picosecondsToDiffTime)
import           Data.ByteString.Char8 (pack)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (DiffTime)
import qualified Network.DNS.Resolver as DNSResolver
import           Network.DNS (DNSError(NameError, TimeoutExpired))
import           Network.Socket (SockAddr (..))

import           Control.Exception (throw)
import           Control.Monad.IOSim
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Tracer (Tracer(Tracer), contramap)
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO, readTVar)
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadTime (Time)

import           Test.Ouroboros.Network.PeerSelection.Script
import           Test.Ouroboros.Network.PeerSelection.Instances()
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.PeerSelection"
  [ testGroup "RootPeersDNS"
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
  ]

--
-- Mock Environment and Utils
--

data MockRoots = MockRoots {
    mockLocalRootPeers :: [(Int, Map RelayAccessPoint PeerAdvertise)]
  , mockDNSMap :: Map Domain [IPv4]
  }
  deriving Show

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
                   RelayAccessDomain domain port
                     -> RelayAccessDomain (domain <> (pack . show) tag) port
                   x -> x
            )
            [(0 :: Int), 1 .. ]
            relays
        relaysBlocks = blocks relaysPerGroup taggedRelays
        relaysMap    = map Map.fromList $ zipWith zip relaysBlocks peerAdvertise

        localRootPeers = zip targets relaysMap

        domains = [ domain | RelayAccessDomain domain _ <- taggedRelays ]

        ipsPerDomain = 2

    ips <- blocks ipsPerDomain
            <$> vectorOf (ipsPerDomain * length domains) (toIPv4w <$> arbitrary)

    let dnsMap = Map.fromList $ zip domains ips

    return (MockRoots {
      mockLocalRootPeers = localRootPeers,
      mockDNSMap         = dnsMap
    })
  where
    blocks _ [] = []
    blocks s l  = take s l : blocks s (drop s l)

instance Arbitrary MockRoots where
    arbitrary = genMockRoots
    shrink roots@MockRoots { mockLocalRootPeers, mockDNSMap } =
      [ roots { mockLocalRootPeers = peers} 
      | peers <- shrinkList (const []) mockLocalRootPeers
      ]
      ++
      [ roots { mockDNSMap = Map.fromList dnsMap }
      | dnsMap <- shrinkList (const []) (Map.assocs mockDNSMap)
      ]

-- | Used for debugging in GHCI
--
simpleMockRoots :: MockRoots
simpleMockRoots = MockRoots localRootPeers dnsMap
  where
    localRootPeers =
      [ ( 2
        , Map.fromList
          [ ( RelayAccessAddress (read "192.0.2.1") (read "3333")
            , DoAdvertisePeer
            )
          , ( RelayAccessDomain  "test.domain"      (read "4444")
            , DoNotAdvertisePeer
            )
          ]
        )
      ]
    dnsMap = Map.fromList [ ("test.domain", [read "192.1.1.1", read "192.2.2.2"])
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
mockDNSActions :: forall exception s.
                  Map Domain [IPv4]
               -> LazySTM.TVar (IOSim s) (Script DNSTimeout)
               -> LazySTM.TVar (IOSim s) (Script DNSLookupDelay)
               -> DNSActions () exception (IOSim s)
mockDNSActions dnsMap dnsTimeoutScript dnsLookupDelayScript =
    DNSActions {
      dnsResolverResource,
      dnsAsyncResolverResource,
      dnsLookupAWithTTL
    }
 where
   dnsResolverResource      _ = return (constantResource ())
   dnsAsyncResolverResource _ = return (constantResource ())

   dnsLookupAWithTTL :: resolvConf
                     -> resolver
                     -> Domain
                     -> IOSim s (Either DNSError [(IPv4, TTL)])
   dnsLookupAWithTTL _ _ domain = do
     DNSTimeout dnsTimeout <- stepScript dnsTimeoutScript
     DNSLookupDelay dnsLookupDelay <- stepScript dnsLookupDelayScript

     dnsLookup <-
        MonadTimer.timeout dnsTimeout $ do
          MonadTimer.threadDelay dnsLookupDelay
          case Map.lookup domain dnsMap of
            Nothing -> return (Left NameError)
            Just x  -> return (Right (map (\a -> (a, 0)) x))

     case dnsLookup of
       Nothing -> return (Left TimeoutExpired)
       Just a  -> return a

-- | 'localRootPeersProvider' running with a given MockRoots env
--
mockLocalRootPeersProvider :: forall s.
                              MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> IOSim s ()
mockLocalRootPeersProvider (MockRoots localRootPeers dnsMap)
                           dnsTimeoutScript dnsLookupDelayScript = do
      dnsTimeoutScriptVar <- initScript dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript dnsLookupDelayScript
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty

      void $ MonadTimer.timeout 3600 $
        localRootPeersProvider tracerTraceLocalRoots
                               (curry toSockAddr)
                               DNSResolver.defaultResolvConf
                               (mockDNSActions dnsMap
                                               dnsTimeoutScriptVar
                                               dnsLookupDelayScriptVar)
                               (readTVar localRootPeersVar)
                               resultVar

-- | 'publicRootPeersProvider' running with a given MockRoots env
--
mockPublicRootPeersProvider :: forall s.
                               MockRoots
                            -> Script DNSTimeout
                            -> Script DNSLookupDelay
                            -> Int
                            -> IOSim s (Set SockAddr, DiffTime)
mockPublicRootPeersProvider (MockRoots localRootPeers dnsMap)
                            dnsTimeoutScript dnsLookupDelayScript n = do
      dnsTimeoutScriptVar <- initScript dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript dnsLookupDelayScript
      localRootPeersVar <- newTVarIO (concatMap (Map.keys . snd) localRootPeers)

      publicRootPeersProvider tracerTracePublicRoots
                              (curry toSockAddr)
                              DNSResolver.defaultResolvConf
                              (readTVar localRootPeersVar)
                              (mockDNSActions @Failure dnsMap
                                                       dnsTimeoutScriptVar
                                                       dnsLookupDelayScriptVar)
                              ($ n)

-- | 'resolveDomainAddresses' running with a given MockRoots env
--
mockResolveDomainAddresses :: forall s.
                              MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> IOSim s (Map DomainAccessPoint (Set SockAddr))
mockResolveDomainAddresses (MockRoots localRootPeers dnsMap)
                           dnsTimeoutScript dnsLookupDelayScript = do
      dnsTimeoutScriptVar <- initScript dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript dnsLookupDelayScript
      resolveDomainAccessPoint tracerTracePublicRoots
                               DNSResolver.defaultResolvConf
                               (mockDNSActions @Failure dnsMap
                                                        dnsTimeoutScriptVar
                                                        dnsLookupDelayScriptVar)
                               [ domain
                               | (_, m) <- localRootPeers
                               , RelayDomainAccessPoint domain <- Map.keys m ]

--
-- Utils for properties
--

data TestTraceEvent exception = RootPeerDNSLocal  (TraceLocalRootPeers SockAddr exception)
                              | RootPeerDNSPublic TracePublicRootPeers
  deriving Show

tracerTraceLocalRoots :: Tracer (IOSim s) (TraceLocalRootPeers SockAddr Failure)
tracerTraceLocalRoots = contramap RootPeerDNSLocal tracerTestTraceEvent

tracerTracePublicRoots :: Tracer (IOSim s) TracePublicRootPeers
tracerTracePublicRoots = contramap RootPeerDNSPublic tracerTestTraceEvent

tracerTestTraceEvent :: Tracer (IOSim s) (TestTraceEvent Failure)
tracerTestTraceEvent = dynamicTracer

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM

selectRootPeerDNSTraceEvents :: SimTrace a -> [(Time, TestTraceEvent Failure)]
selectRootPeerDNSTraceEvents = go
  where
    go (SimTrace t _ _ (EventLog e) trace)
     | Just x <- fromDynamic e    = (t,x) : go trace
    go (SimTrace _ _ _ _ trace)   =         go trace
    go (TraceMainException _ e _) = throw e
    go (TraceDeadlock      _   _) = [] -- expected result in many cases
    go (TraceMainReturn    _ _ _) = []

selectLocalRootPeersEvents :: [(Time, TestTraceEvent Failure)]
                           -> [(Time, TraceLocalRootPeers SockAddr Failure)]
selectLocalRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSLocal e) <- trace ]

selectLocalRootGroupsEvents :: [(Time, TraceLocalRootPeers SockAddr Failure)]
                            -> [(Time, Seq (Int, Map SockAddr PeerAdvertise))]
selectLocalRootGroupsEvents trace = [ (t, e) | (t, TraceLocalRootGroups e) <- trace ]

selectLocalRootResultEvents :: [(Time, TraceLocalRootPeers SockAddr Failure)]
                            -> [(Time, (Domain, [IPv4]))]
selectLocalRootResultEvents trace = [ (t, (domain, map fst r))
                                    | (t, TraceLocalRootResult (DomainAccessPoint domain _) r) <- trace ]

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
prop_local_preservesGroupNumberAndTargets :: MockRoots
                                          -> Script DNSTimeout
                                          -> Script DNSLookupDelay
                                          -> Property
prop_local_preservesGroupNumberAndTargets mockRoots@(MockRoots lrp _)
                                          dnsTimeoutScript
                                          dnsLookupDelayScript =
    let tr = selectLocalRootGroupsEvents
           $ selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

        -- For all LocalRootGroup results, the number of groups should be
        -- preserved, i.e. no new groups are added nor deleted along the
        -- trace by localRootPeersProvider.
        preservesGroupNumber = all ((== length lrp) . length . snd) tr

        -- For all LocalRootGroup results, the targets for each group
        -- should be preserved, i.e. targets are not modified along the
        -- trace by localRootPeersProvider.
        preservesTargets     = all (all (\(a, b) -> fst a == fst b))
                                   [ zip lrp (toList r) | r <- map snd tr ]

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
prop_local_resolvesDomainsCorrectly mockRoots@(MockRoots localRoots dnsMap)
                                    dnsTimeoutScript
                                    dnsLookupDelayScript =
    let tr = selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

        -- local root domains
        localRootDomains :: Set Domain
        localRootDomains =
          Set.fromList
          [ domain
          | (_, m) <- localRoots
          , RelayAccessDomain domain _ <- Map.keys m
          ]

        -- domains that were resolved during simulation
        resultMap :: Map Domain [IPv4]
        resultMap = Map.fromList
                  $ map snd
                  $ selectLocalRootResultEvents
                  $ tr

        -- all domains that could have been resolved
        maxResultMap :: Map Domain [IPv4]
        maxResultMap = dnsMap `Map.restrictKeys` localRootDomains

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
      .&&. property (resultMap `Map.isSubmapOf` maxResultMap)


-- | The 'localRootPeersProvider' after resolving a DNS domain address
-- should update the local result group list correctly, i.e. add the
-- resolved ip addresses to the correct group where the domain address was
-- (in the initial configuration specification). This property tests whether
-- after a successful DNS lookup the result list is updated correctly.
prop_local_updatesDomainsCorrectly :: MockRoots
                                   -> Script DNSTimeout
                                   -> Script DNSLookupDelay
                                   -> Property
prop_local_updatesDomainsCorrectly mockRoots@(MockRoots lrp _)
                                   dnsTimeoutScript
                                   dnsLookupDelayScript =
    let tr = selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

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
                                            case Map.lookup (RelayDomainAccessPoint da) m of
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
    mockRoots@(MockRoots _ dnsMap)
    (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
    n
  = lookupLoop mockRoots dnsMap === dnsMap
  where
    -- Perform public root DNS lookup until no failures
    lookupLoop :: MockRoots -> Map Domain [IPv4] -> Map Domain [IPv4]
    lookupLoop mr res =
      let tr = runSimTrace
             $ mockPublicRootPeersProvider mr
                                           dnsTimeoutScript
                                           dnsLookupDelayScript
                                           n

          successes = selectPublicRootResultEvents
                    $ selectPublicRootPeersEvents
                    $ selectRootPeerDNSTraceEvents
                    $ tr

          failures = selectPublicRootFailureEvents
                   $ selectPublicRootPeersEvents
                   $ selectRootPeerDNSTraceEvents
                   $ tr

          successesMap = Map.fromList $ map snd successes

          -- Update MockRoots with only the RelayAccessPoint that failed the
          -- DNS timeouts
          failuresMap  =
            [ ( i
              , Map.fromList [ (RelayAccessDomain domain port, pa)
                             | (RelayAccessDomain domain port, pa) <- Map.toList m
                             , domain `elem` map snd failures
                             ]
              )
            | (i, m) <- mockLocalRootPeers mr
            ]
          newMR = mr { mockLocalRootPeers = failuresMap }

        in if null failures || res == dnsMap
            then res
            else lookupLoop newMR (res <> successesMap)
