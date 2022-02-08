{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Test.Ouroboros.Network.PeerSelection.RootPeersDNS
  ( tests
  , mockDNSActions
  , DNSTimeout (..)
  , DNSLookupDelay (..)
  ) where

import           Ouroboros.Network.PeerSelection.RootPeersDNS
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))

import           Control.Monad (replicateM_)
import           Data.ByteString.Char8 (pack)
import           Data.Dynamic (Typeable, fromDynamic)
import           Data.Foldable (foldl', toList)
import           Data.Functor (void)
import           Data.IP (fromHostAddress, toIPv4w, toSockAddr)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (picosecondsToDiffTime)
import           Data.Void (Void)
import           Network.DNS (DNSError (NameError, TimeoutExpired))
import qualified Network.DNS.Resolver as DNSResolver
import           Network.Socket (SockAddr (..))

import           Control.Exception (throw)
import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM, newTVarIO,
                     readTVar)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (Time (..))
import           Control.Monad.Class.MonadTimer
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (Tracer), contramap)

import           Ouroboros.Network.Testing.Data.Script (NonEmpty ((:|)),
                     Script (Script), initScript', stepScript)
import           Test.Ouroboros.Network.PeerSelection.Instances ()
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

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
    ]
  ]

--
-- Mock Environment and Utils
--

data MockRoots = MockRoots {
    mockLocalRootPeers        :: [(Int, Map RelayAccessPoint PeerAdvertise)]
  , mockLocalRootPeersDNSMap  :: Map Domain [IP]
  , mockPublicRootPeers       :: [RelayAccessPoint]
  , mockPublicRootPeersDNSMap :: Map Domain [IP]
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
    targets <- vectorOf relaysNumber (chooseEnum (1, 5))

    peerAdvertise <- blocks relaysPerGroup
                      <$> vectorOf relaysNumber (arbitrary @PeerAdvertise)

        -- concat unique identifier to DNS domains to simplify tests
    let taggedLocalRelays = tagRelays localRootRelays
        localRelaysBlocks = blocks relaysPerGroup taggedLocalRelays
        localRelaysMap    = map Map.fromList $ zipWith zip localRelaysBlocks
                                                           peerAdvertise
        localRootPeers    = zip targets localRelaysMap
        localRootDomains  = [ domain
                            | RelayAccessDomain domain _ <- taggedLocalRelays ]

        ipsPerDomain = 2

    localRootDomainIPs <- blocks ipsPerDomain
            -- Modules under test do not differ by IP version so we only
            -- generate IPv4 addresses.
            <$> vectorOf (ipsPerDomain * length localRootDomains)
                         (IPv4 . toIPv4w <$> arbitrary)

    let lrpDNSMap = Map.fromList $ zip localRootDomains localRootDomainIPs

    -- Generate PublicRootPeers
    --
    publicRootRelays <- vectorOf relaysNumber arbitrary

    let publicRootPeers = tagRelays publicRootRelays

        publicRootDomains = [ domain
                            | RelayAccessDomain domain _ <- publicRootPeers ]

    publicRootDomainIPs <- blocks ipsPerDomain
            -- Modules under test do not differ by IP version so we only
            -- generate IPv4 addresses.
            <$> vectorOf (ipsPerDomain * length publicRootDomains)
                         (IPv4 . toIPv4w <$> arbitrary)

    let publicRootPeersDNSMap = Map.fromList
                              $ zip publicRootDomains publicRootDomainIPs

    return (MockRoots {
      mockLocalRootPeers        = localRootPeers,
      mockLocalRootPeersDNSMap  = lrpDNSMap,
      mockPublicRootPeers       = publicRootPeers,
      mockPublicRootPeersDNSMap = publicRootPeersDNSMap
    })
  where
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
                              <- concatMap (Map.keys . snd) lrp ]
            lrpDNSMap  = Map.restrictKeys mockLocalRootPeersDNSMap
                                          lrpDomains
      ] ++
      [ roots { mockPublicRootPeers       = prp
              , mockPublicRootPeersDNSMap = prpDNSMap
              }
      | prp <- shrinkList (const []) mockPublicRootPeers,
        let prpDomains = Set.fromList [ domain
                                      | RelayAccessDomain domain _ <- prp ]
            prpDNSMap  = Map.restrictKeys mockPublicRootPeersDNSMap
                                          prpDomains
      ]

-- | Used for debugging in GHCI
--
simpleMockRoots :: MockRoots
simpleMockRoots = MockRoots localRootPeers dnsMap [] Map.empty
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
    dnsMap = Map.fromList
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
                  ( MonadSTM   m
                  , MonadDelay m
                  , MonadTimer m
                  )
               => Map Domain [IP]
               -> LazySTM.TVar m (Script DNSTimeout)
               -> LazySTM.TVar m (Script DNSLookupDelay)
               -> DNSActions () exception m
mockDNSActions dnsMap dnsTimeoutScript dnsLookupDelayScript =
    DNSActions {
      dnsResolverResource,
      dnsAsyncResolverResource,
      dnsLookupWithTTL
    }
 where
   dnsResolverResource      _ = return (constantResource ())
   dnsAsyncResolverResource _ = return (constantResource ())

   dnsLookupWithTTL :: resolvConf
                    -> resolver
                    -> Domain
                    -> m ([DNSError], [(IP, TTL)])
   dnsLookupWithTTL _ _ domain = do
     DNSTimeout dnsTimeout <- stepScript dnsTimeoutScript
     DNSLookupDelay dnsLookupDelay <- stepScript dnsLookupDelayScript

     dnsLookup <-
        MonadTimer.timeout dnsTimeout $ do
          MonadTimer.threadDelay dnsLookupDelay
          case Map.lookup domain dnsMap of
            Nothing -> return (Left NameError)
            Just x  -> return (Right (map (\a -> (a, 0)) x))

     case dnsLookup of
       Nothing        -> return ([TimeoutExpired], [])
       Just (Left e)  -> return ([e], [])
       Just (Right a) -> return ([], a)

-- | 'localRootPeersProvider' running with a given MockRoots env
--
mockLocalRootPeersProvider :: forall m.
                              ( MonadAsync m
                              , MonadDelay m
                              , MonadTimer m
                              , Eq (Async m Void)
                              )
                           => Tracer m (TraceLocalRootPeers SockAddr Failure)
                           -> MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> m ()
mockLocalRootPeersProvider tracer (MockRoots localRootPeers dnsMap _ _)
                           dnsTimeoutScript dnsLookupDelayScript = do
      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty

      void $ MonadTimer.timeout 3600 $
        localRootPeersProvider tracer
                               (curry toSockAddr)
                               DNSResolver.defaultResolvConf
                               (mockDNSActions dnsMap
                                               dnsTimeoutScriptVar
                                               dnsLookupDelayScriptVar)
                               (readTVar localRootPeersVar)
                               resultVar

-- | 'publicRootPeersProvider' running with a given MockRoots env.
--
-- NOTE: This function is used in 'prop_public_resolvesDomainsCorrectly'. Due to
-- API limitations it is needed to run 'publicRootPeersProvider' multiple times,
-- in order to run only 1 simulation which resolves untill we get the expected
-- result, instead of a recursive loop which at each step runs IOSim.
--
mockPublicRootPeersProvider :: forall m a.
                               ( MonadAsync m
                               , MonadThrow m
                               , MonadTimer m
                               )
                            => Tracer m TracePublicRootPeers
                            -> MockRoots
                            -> Script DNSTimeout
                            -> Script DNSLookupDelay
                            -> ((Int -> m (Set SockAddr, DiffTime)) -> m a)
                            -> m ()
mockPublicRootPeersProvider tracer (MockRoots _ _ publicRootPeers dnsMap)
                            dnsTimeoutScript dnsLookupDelayScript action = do
      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      publicRootPeersVar <- newTVarIO publicRootPeers
      replicateM_ 5 $
        publicRootPeersProvider tracer
                                (curry toSockAddr)
                                DNSResolver.defaultResolvConf
                                (readTVar publicRootPeersVar)
                                (mockDNSActions @Failure
                                                dnsMap
                                                dnsTimeoutScriptVar
                                                dnsLookupDelayScriptVar)
                                action

-- | 'resolveDomainAddresses' running with a given MockRoots env
--
mockResolveDomainAddresses :: ( MonadAsync m
                              , MonadThrow m
                              , MonadTimer m
                              )
                           => Tracer m TracePublicRootPeers
                           -> MockRoots
                           -> Script DNSTimeout
                           -> Script DNSLookupDelay
                           -> m (Map DomainAccessPoint (Set SockAddr))
mockResolveDomainAddresses tracer (MockRoots _ _ publicRootPeers dnsMap)
                           dnsTimeoutScript dnsLookupDelayScript = do
      dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
      dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
      resolveDomainAccessPoint tracer
                               DNSResolver.defaultResolvConf
                               (mockDNSActions @Failure dnsMap
                                                        dnsTimeoutScriptVar
                                                        dnsLookupDelayScriptVar)
                               [ domain
                               | RelayDomainAccessPoint domain <- publicRootPeers ]

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
    go TraceLoop                  = error "IOSimPOR step time limit exceeded"

selectLocalRootPeersEvents :: [(Time, TestTraceEvent Failure)]
                           -> [(Time, TraceLocalRootPeers SockAddr Failure)]
selectLocalRootPeersEvents trace = [ (t, e) | (t, RootPeerDNSLocal e) <- trace ]

selectLocalRootGroupsEvents :: [(Time, TraceLocalRootPeers SockAddr Failure)]
                            -> [(Time, Seq (Int, Map SockAddr PeerAdvertise))]
selectLocalRootGroupsEvents trace = [ (t, e) | (t, TraceLocalRootGroups e) <- trace ]

selectLocalRootResultEvents :: [(Time, TraceLocalRootPeers SockAddr Failure)]
                            -> [(Time, (Domain, [IP]))]
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
    let tr = selectLocalRootGroupsEvents
           $ selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
                                        dnsTimeoutScript
                                        dnsLookupDelayScript

        -- local root addresses
        localRootAddresses :: [(a, Map RelayAccessPoint PeerAdvertise)]
                           -> Set SockAddr
        localRootAddresses lrp =
          Set.fromList
          [ toSockAddr (ip, port)
          | (_, m) <- lrp
          , RelayAccessAddress ip port <- Map.keys m
          ]

        localGroupEventsAddresses :: (a, Seq (Int, Map SockAddr PeerAdvertise))
                                  -> Set SockAddr
        localGroupEventsAddresses (_, s) =
            Set.fromList
          $ concatMap (Map.keys . snd)
          $ toList s

     in property
      $ all (\x -> localRootAddresses localRoots
                   `Set.isSubsetOf`
                   localGroupEventsAddresses x
            )
            tr
      && not (null tr)

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
    let tr = selectLocalRootGroupsEvents
           $ selectLocalRootPeersEvents
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
prop_local_resolvesDomainsCorrectly mockRoots@(MockRoots localRoots dnsMap _ _)
                                    dnsTimeoutScript
                                    dnsLookupDelayScript =
    let tr = selectLocalRootPeersEvents
           $ selectRootPeerDNSTraceEvents
           $ runSimTrace
           $ mockLocalRootPeersProvider tracerTraceLocalRoots
                                        mockRoots
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
        resultMap :: Map Domain [IP]
        resultMap = Map.fromList
                  $ map snd
                  $ selectLocalRootResultEvents
                  $ tr

        -- all domains that could have been resolved
        maxResultMap :: Map Domain [IP]
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
                            -- Get all IPs present in group at position
                            -- 'index'
                            ipsAtIndex = map (\sockAddr ->
                                             case sockAddr of
                                               SockAddrInet _ hostAddr
                                                 -> IPv4 $ fromHostAddress hostAddr
                                               _ -> error "Impossible happened!"

                                         ) $ Map.keys
                                           $ snd
                                           $ lrpg `Seq.index` index :: [IP]
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
    mockRoots@(MockRoots _ _ _ dnsMap)
    (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
    n
  = let tr = runSimTrace
           $ mockPublicRootPeersProvider tracerTracePublicRoots
                                         mockRoots
                                         dnsTimeoutScript
                                         dnsLookupDelayScript
                                         ($ n)

        successes = selectPublicRootResultEvents
                  $ selectPublicRootPeersEvents
                  $ selectRootPeerDNSTraceEvents
                  $ tr

        successesMap = Map.fromList $ map snd successes

     in counterexample (show successes)
      $ successesMap == dnsMap
