{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Diffusion.PoolRelays (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow (MonadCatch, bracket_)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (nullTracer)
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict qualified as IntMap
import Data.IP qualified as IP
import Data.List (mapAccumL, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word16)
import Network.DNS qualified as DNS
import Network.Socket (PortNumber, SockAddr)

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.Diffusion.PoolRelays
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS (DNSActions (..),
           DNSLookupType (..), PeerActionsDNS (..))
import Ouroboros.Network.Point (WithOrigin (..))

import Test.Ouroboros.Network.Data.Script (initScript', singletonScript)
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS (DNSLookupDelay (..),
           DNSTimeout (..), MockDNSMap, mockDNSActions)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.PoolRelays"
    [ testProperty "the index follows the ledger and what was last known; a pool keeps its bucket by its relays" prop_follows
    , testProperty "a slow name delays no other"  prop_slow_name
    , testProperty "an answer replaces its name's addresses as it comes" prop_answer_replaces
    , testProperty "a setting change ends the wait; a poll that indexed nothing is retried" prop_wakes_and_retries
    , testProperty "an IPv4-mapped address keys as the IPv4 address" prop_key_unmaps
    ]

-- | A relay as a pool registers it: one of six names, or a literal address,
-- each on one of two ports, or one of three service names whose SRV answer
-- the phase gives.
data Relay = Named Int PortNumber | Literal IP.IPv4 PortNumber | Service Int
  deriving (Show, Eq)

-- | One SRV target: one of the six names, whose A records the phase has or
-- not, or the "." marker that is never resolved; at a priority and a weight,
-- on a port.
data Target = Target {
    tName     :: Maybe Int,
    tPriority :: Word16,
    tWeight   :: Word16,
    tPort     :: PortNumber
  }
  deriving (Show, Eq)

-- | One stretch of the run: the pools with their weights, or ledger peers
-- off; the A records DNS has for each name and the SRV records it has for
-- each service name (absent: a name error); and the names and services
-- whose lookups fail for the moment, with a timeout.
data Phase = Phase {
    phPools        :: Maybe [(Int, NonEmpty Relay)],
    phDns          :: Map Int [IP.IPv4],
    phSrv          :: Map Int [Target],
    phTransient    :: Set Int,
    phSrvTransient :: Set Int
  }
  deriving Show

newtype Schedule = Schedule [Phase]
  deriving Show

testSRVPrefix :: SRVPrefix
testSRVPrefix = "_cardano._tcp"

domainName :: Int -> DNS.Domain
domainName i = BSC.pack ("test" ++ show i)

-- | A service name as the ledger carries it, and as the thread looks it up.
serviceName, srvDomain :: Int -> DNS.Domain
serviceName i = BSC.pack ("svc" ++ show i)
srvDomain i   = testSRVPrefix <> "." <> serviceName i

genIPv4 :: Gen IP.IPv4
genIPv4 = IP.toIPv4 <$> vectorOf 4 (choose (1, 254))

genRelay :: Gen Relay
genRelay = frequency
  [ (3, Named   <$> choose (1, 6) <*> elements [3001, 3002])
  , (1, Literal <$> genIPv4       <*> elements [3001, 3002])
  , (2, Service <$> choose (1, 3)) ]

genTarget :: Gen Target
genTarget = Target <$> frequency [ (8, Just <$> choose (1, 6)), (1, pure Nothing) ]
                   <*> elements [0, 1]
                   <*> elements [1, 2]
                   <*> elements [3001, 3002]

genSubset :: [Int] -> Gen (Set Int)
genSubset xs = Set.fromList . concat <$> forM xs (\x -> frequency [ (5, pure []), (1, pure [x]) ])

genPhase :: Gen Phase
genPhase = do
  disabled <- frequency [ (1, pure True), (6, pure False) ]
  n        <- choose (0, 8)
  pools    <- vectorOf n $ (,) <$> choose (1, 100) <*> (do k  <- choose (1, 3)
                                                           rs <- vectorOf k genRelay
                                                           return (NonEmpty.fromList rs))
  dns      <- fmap (Map.fromList . concat) $ forM [1 .. 6] $ \i -> do
                present <- frequency [ (5, pure True), (1, pure False) ]
                ips     <- choose (1, 2) >>= \k -> vectorOf k genIPv4
                return [ (i, ips) | present ]
  srv      <- fmap (Map.fromList . concat) $ forM [1 .. 3] $ \i -> do
                present <- frequency [ (5, pure True), (1, pure False) ]
                targets <- choose (1, 3) >>= \k -> vectorOf k genTarget
                return [ (i, targets) | present ]
  transient    <- genSubset [1 .. 6]
  srvTransient <- genSubset [1 .. 3]
  return Phase { phPools = if disabled then Nothing else Just pools, phDns = dns, phSrv = srv,
                 phTransient = transient, phSrvTransient = srvTransient }

-- | Phases in sequence: some keep the previous phase's pools and change only
-- DNS; some keep the number of pools and change their stake and relays; and
-- some keep the pools' relays and change their stakes, dropping a pool
-- maybe, so a bucket moving with its pool is exercised.
instance Arbitrary Schedule where
  arbitrary = do
    n  <- choose (1, 4)
    p0 <- genPhase
    Schedule . reverse <$> go (n - 1) [p0]
    where
      go :: Int -> [Phase] -> Gen [Phase]
      go 0 acc = return acc
      go k acc@(prev : _) = do
        next <- frequency
          [ (2, genPhase)
          , (1, do fresh <- genPhase
                   return prev { phDns = phDns fresh, phSrv = phSrv fresh,
                                 phTransient = phTransient fresh, phSrvTransient = phSrvTransient fresh })
          , (1, case phPools prev of
                     Just ps@(_ : _) -> do
                       fresh <- genPhase
                       ps'   <- forM ps $ \_ -> (,) <$> choose (1, 100)
                                                     <*> (NonEmpty.fromList <$> (choose (1, 3) >>= \nr -> vectorOf nr genRelay))
                       return fresh { phPools = Just ps' }
                     _ -> genPhase)
          , (1, case phPools prev of
                     Just ps@(_ : _) -> do
                       fresh <- genPhase
                       ps'   <- forM ps $ \(_, rs) -> (,) <$> choose (1, 100) <*> pure rs
                       kept  <- frequency
                                  [ (2, pure ps')
                                  , (1, (\i -> take i ps' ++ drop (i + 1) ps') <$> choose (0, length ps' - 1)) ]
                       return fresh { phPools = Just kept }
                     _ -> genPhase) ]
        go (k - 1) (next : acc)
      go _ [] = return []
  shrink (Schedule phases) =
       [ Schedule ps | ps <- shrinkList shrinkPhase phases, not (null ps) ]

instance Arbitrary Phase where
  arbitrary = genPhase
  shrink    = shrinkPhase

shrinkPhase :: Phase -> [Phase]
shrinkPhase ph@Phase { phPools = Just pools } =
     [ ph { phPools = Just ps } | ps <- shrinkList (const []) pools ]
  ++ shrinkRecords ph
shrinkPhase ph = shrinkRecords ph

shrinkRecords :: Phase -> [Phase]
shrinkRecords ph@Phase { phDns, phSrv, phTransient, phSrvTransient } =
     [ ph { phDns = Map.fromList d } | d <- shrinkList (const []) (Map.toList phDns) ]
  ++ [ ph { phSrv = Map.fromList s } | s <- shrinkList shrinkTargets (Map.toList phSrv) ]
  ++ [ ph { phTransient = Set.fromList t } | t <- shrinkList (const []) (Set.toList phTransient) ]
  ++ [ ph { phSrvTransient = Set.fromList t } | t <- shrinkList (const []) (Set.toList phSrvTransient) ]
  where
    shrinkTargets (i, ts) = [ (i, ts') | ts' <- shrinkList (const []) ts ]

toLedger :: [(Int, NonEmpty Relay)] -> [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
toLedger pools =
    [ (PoolStake (fromIntegral w % weight), fmap relay rs) | (w, rs) <- pools ]
  where
    weight = fromIntegral (max 1 (sum (map fst pools))) :: Integer
    relay (Named i port)    = LedgerRelayAccessDomain (domainName i) port
    relay (Literal ip port) = LedgerRelayAccessAddress (IP.IPv4 ip) port
    relay (Service i)       = LedgerRelayAccessSRVDomain (serviceName i)

toDns :: Map Int [IP.IPv4] -> Map Int [Target] -> MockDNSMap
toDns dns srv = Map.fromList $
     [ ((domainName i, DNS.A), Left [ (IP.IPv4 ip, 300) | ip <- ips ])
     | (i, ips) <- Map.toList dns ]
  ++ [ ((srvDomain i, DNS.SRV), Right [ (maybe "." domainName tName, tPriority, tWeight, tPort)
                                      | Target { tName, tPriority, tWeight, tPort } <- targets ])
     | (i, targets) <- Map.toList srv ]

-- | The big-ledger prefix as the thread must see it: positions in
-- descending stake, every relay resolved, ports stripped.
expectedBig :: Phase -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
expectedBig Phase { phPools = Nothing }    = Map.empty
expectedBig Phase { phPools = Just pools } =
  accBigPoolStakeMap [ (s, fmap (prefixLedgerRelayAccessPoint testSRVPrefix) rs)
                     | (s, rs) <- toLedger pools ]

-- | The services and the names the phase's big-ledger pools register: the
-- thread sees the prefix, not every pool.
services, names :: Phase -> Set Int
services ph = Set.fromList [ i | RelayAccessSRVDomain d <- bigRelays ph, i <- [1 .. 3], srvDomain i == d ]
names ph    = Set.fromList [ i | RelayAccessDomain d _ <- bigRelays ph, i <- [1 .. 6], domainName i == d ]

bigRelays :: Phase -> [RelayAccessPoint]
bigRelays = concatMap (NonEmpty.toList . snd) . Map.elems . expectedBig

-- | What the thread knows: each service's targets and each name's
-- addresses, as last answered.
data Memory = Memory {
    memTargets :: Map Int [Target],
    memAddrs   :: Map Int [IP.IPv4]
  }
  deriving Show

-- | A lookup's outcome applied to what is known under a key: an answer
-- replaces, a name error forgets, a transient failure keeps.
learn :: Ord k => Set k -> Map k v -> k -> Map k v -> Map k v
learn transient answers k m
  | Set.member k transient = m
  | otherwise = case Map.lookup k answers of
                     Just v  -> Map.insert k v m
                     Nothing -> Map.delete k m

-- | One phase's poll, settled: the memory after it, and the index it
-- publishes. Stage one learns the services' targets; stage two learns the
-- names, the relays' own and the targets'; both are then pruned to the
-- list. With ledger peers off nothing is polled, and what was known keeps
-- until the next poll prunes it.
pollPhase :: Memory -> Phase -> (Memory, Map SockAddr [Int])
pollPhase mem Phase { phPools = Nothing } = (mem, Map.empty)
pollPhase Memory { memTargets, memAddrs } ph@Phase { phDns, phSrv, phTransient, phSrvTransient } =
    (mem', index)
  where
    svcs     = services ph
    targets  = Map.restrictKeys (foldr (learn phSrvTransient phSrv) memTargets (Set.toList svcs)) svcs
    targetNames = Set.fromList [ n | i <- Set.toList svcs, Target { tName = Just n } <- Map.findWithDefault [] i targets ]
    nms      = names ph `Set.union` targetNames
    addrs    = Map.restrictKeys (foldr (learn phTransient phDns) memAddrs (Set.toList nms)) nms
    mem'     = Memory targets addrs
    index    = Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
                 [ (sockAddrWithoutPort a, [pos])
                 | (pos, (_, rs)) <- zip [0 ..] (Map.elems (expectedBig ph))
                 , r <- NonEmpty.toList rs
                 , a <- addressesOf r ]
    addressesOf (RelayAccessAddress ip port) = [IP.toSockAddr (ip, port)]
    addressesOf (RelayAccessDomain d port)   = named d port
    addressesOf (RelayAccessSRVDomain d)     =
      concat [ named (domainName n) tPort
             | i <- [1 .. 3], srvDomain i == d
             , Target { tName = Just n, tPort } <- Map.findWithDefault [] i targets ]
    named d port = [ IP.toSockAddr (IP.IPv4 ip, port)
                   | (i, ips) <- Map.toList addrs, domainName i == d, ip <- ips ]

-- | The index after each phase, from nothing known at the start.
expectedIndices :: [Phase] -> [Map SockAddr [Int]]
expectedIndices = snd . mapAccumL pollPhase (Memory Map.empty Map.empty)

-- | The keys of a phase's pools, in position order: their relay lists as
-- the thread sees them, with ordinals among equal lists.
expectedKeys :: Phase -> [PoolKey]
expectedKeys = poolKeys . map snd . Map.elems . expectedBig

-- | What the harness marks every bucket with after observing a phase: a
-- charged total the buckets would never reach by themselves.
mark :: Charged
mark = Charged (-1234)

-- | Every bucket's credit after each phase, with its key: the mark the
-- harness left on it in the phase before, when the pool was in that
-- phase's list too and so kept its bucket; zero otherwise, since a new
-- bucket starts empty and the reading never moves.
expectedCredits :: [Phase] -> [[(PoolKey, Int)]]
expectedCredits phases =
  [ [ (k, if k `elem` before then credit testAllowance (Fresh 0) mark else 0)
    | k <- expectedKeys ph ]
  | (before, ph) <- zip ([] : map expectedKeys phases) phases ]

testAllowance :: Allowance
testAllowance = Allowance 1000

-- | The thread under test with a mock DNS: the current phase is applied
-- through @apply@, and @slow@ names each lookup's extra delay. Transient
-- failures are the harness's own: a timeout returned without the mock, so
-- the mock's answer for the name stays what the phase says. Also the most
-- lookups ever in flight at once.
data Harness m = Harness {
    hApply      :: Phase -> m (),
    hAllowances :: PoolAllowances m SockAddr,
    hThread     :: m (),
    hInFlight   :: StrictTVar m (Int, Int)
  }

newHarness :: forall m. (MonadAsync m, MonadCatch m, MonadDelay m, MonadTimer m)
           => (Int -> DiffTime)   -- ^ each name's extra lookup delay
           -> DiffTime            -- ^ the poll interval
           -> DiffTime            -- ^ the retry interval
           -> m (Harness m)
newHarness slow pollEvery retryAfter = do
    ledgerVar  <- newTVarIO []
    useVar     <- newTVarIO DontUseLedgerPeers
    dnsMapVar  <- newTVarIO Map.empty
    transVar   <- newTVarIO (Set.empty, Set.empty)
    timeouts   <- initScript' (singletonScript (DNSTimeout 10))
    delays     <- initScript' (singletonScript (DNSLookupDelay 0.01))
    allowances <- newPoolAllowances testAllowance (const (Fresh 0)) (Fresh 0)
    inFlight   <- newTVarIO (0 :: Int, 0 :: Int)      -- now, and the most ever
    let apply Phase { phPools, phDns, phSrv, phTransient, phSrvTransient } = atomically $ do
          writeTVar ledgerVar (maybe [] toLedger phPools)
          writeTVar useVar (if isNothing phPools then DontUseLedgerPeers else UseLedgerPeers Always)
          writeTVar dnsMapVar (toDns phDns phSrv)
          writeTVar transVar (phTransient, phSrvTransient)
        interface = LedgerPeersConsensusInterface {
            lpGetLatestSlot  = pure (At (SlotNo 1000)),
            lpGetLedgerPeers = readTVar ledgerVar,
            lpExtraAPI       = ()
          }
        mock = mockDNSActions nullTracer LookupReqAOnly (curry IP.toSockAddr)
                              dnsMapVar timeouts delays
        -- every lookup counted while it runs
        counted :: m a -> m a
        counted = bracket_ (atomically $ modifyTVar inFlight (\(n, most) -> (n + 1, max most (n + 1))))
                           (atomically $ modifyTVar inFlight (\(n, most) -> (n - 1, most)))
        nameOf d  = [ i | i <- [1 .. 6], domainName i == d ]
        serviceOf d = [ i | i <- [1 .. 3], srvDomain i == d ]
        actions = mock {
            dnsLookupSRV = \kind d conf resolver -> counted $ do
              (_, srvTransient) <- readTVarIO transVar
              if any (`Set.member` srvTransient) (serviceOf d)
                 then return (Left DNS.TimeoutExpired)
                 else dnsLookupSRV mock kind d conf resolver,
            dnsLookupIPs = \kind d conf resolver -> counted $ do
              (transient, _) <- readTVarIO transVar
              mapM_ (threadDelay . slow) (nameOf d)
              if any (`Set.member` transient) (nameOf d)
                 then return [(DNS.A, Left DNS.TimeoutExpired)]
                 else dnsLookupIPs mock kind d conf resolver
          }
        dns = PeerActionsDNS {
            paToPeerAddr = curry IP.toSockAddr,
            paDnsActions = actions
          }
        args = PoolRelaysArgs {
            prTracer       = nullTracer,
            prConsensus    = interface,
            prUseLedger    = readTVar useVar,
            prSnapshot     = pure Nothing,
            prSRVPrefix    = testSRVPrefix,
            prDNS          = dns,
            prAddressKey   = sockAddrWithoutPort,
            prPollInterval = pollEvery,
            prRetryInterval = retryAfter
          }
    return Harness { hApply = apply, hAllowances = allowances,
                     hThread = () <$ poolRelaysThread args allowances, hInFlight = inFlight }

-- | Run the thread through the schedule, settling 3 s after each phase: a
-- poll a second with a 10 ms DNS delay per lookup, so both a list change and
-- a DNS change have taken effect by then. Also the most lookups ever in
-- flight at once.
runSchedule :: Schedule -> ([(Map SockAddr [Int], [(PoolKey, Int)])], Int)
runSchedule (Schedule phases) = runSimOrThrow $ do
    Harness { hApply, hAllowances = allowances, hThread, hInFlight } <- newHarness (const 0) 1 1
    case phases of
         []       -> return ([], 0)
         p0 : _   -> do
           hApply p0
           observed <- withAsync hThread $ \_ ->
             forM phases $ \ph -> do
               hApply ph
               threadDelay 3
               atomically $ do
                 idx     <- readTVar (paIndex allowances)
                 buckets <- IntMap.elems <$> readTVar (paBuckets allowances)
                 credits <- mapM (\(k, tv) -> (,) k . credit testAllowance (Fresh 0) <$> readTVar tv)
                                 buckets
                 mapM_ ((`writeTVar` mark) . snd) buckets
                 return (idx, credits)
           most <- snd <$> readTVarIO hInFlight
           return (observed, most)

prop_follows :: Schedule -> Property
prop_follows sched@(Schedule phases) =
    counterexample (show observed)
  . classify (any (isNothing . phPools) phases) "a phase with ledger peers off"
  . classify dnsOnlyChange "a DNS-only change between phases"
  . classify (any ((> 1) . Map.size . expectedBig) phases) "more than one pool"
  . tabulate "addresses indexed" (map (show . Map.size) expected)
  . classify (mostInFlight == 2) "both DNS slots used at once"
  . classify sameCountChange "a list change keeping the pool count"
  . classify keptBucket "a pool kept its bucket through a list change"
  . classify (anyService distinctTargets) "a service whose targets reach distinct addresses"
  . classify (anyService backupPriority) "a service with a backup priority"
  . classify (anyService failingTarget) "a service with a target that fails"
  . classify (anyService unanswered) "a service without an SRV answer"
  . classify keptName "a name kept as last known through a transient failure"
  . classify keptService "a service kept as last known through a transient failure"
  . classify forgotten "a name forgotten on a name error after being known"
  $ conjoin [ counterexample ("phase " ++ show i) (idx === expectedIdx)
            | (i, expectedIdx, (idx, _)) <- zip3 [0 :: Int ..] expected observed ]
    .&&. counterexample "bucket credits" ([ c | (_, c) <- observed ] === expectedCredits phases)
    -- the thread looks names up on two workers by design, stated here as
    -- a number so a change to the constant is a change to the test
    .&&. counterexample ("lookups in flight at once: " ++ show mostInFlight)
           (mostInFlight <= 2)
  where
    (observed, mostInFlight) = runSchedule sched
    expected = expectedIndices phases
    -- what was known entering each phase
    befores  = take (length phases) (scanl (\m ph -> fst (pollPhase m ph)) (Memory Map.empty Map.empty) phases)
    dnsOnlyChange = or [ expectedBig a == expectedBig b && ia /= ib
                       | ((a, ia), (b, ib)) <- zip (zip phases expected) (drop 1 (zip phases expected)) ]
    anyService p = or [ p ph i | ph <- phases, i <- Set.toList (services ph) ]
    -- what tells enumeration from a pick: two targets that resolve, to
    -- different addresses
    distinctTargets ph i =
      Set.size (Set.fromList [ hosts | hosts <- map (map sockAddrWithoutPort) (targetAddrs ph i)
                                     , not (null hosts) ]) >= 2
    backupPriority ph i =
      Set.size (Set.fromList (map tPriority (Map.findWithDefault [] i (phSrv ph)))) >= 2
    failingTarget ph i = or [ Map.notMember n (phDns ph)
                            | Target { tName = Just n } <- Map.findWithDefault [] i (phSrv ph) ]
    unanswered ph i = Map.notMember i (phSrv ph)
    sameCountChange = or [ expectedBig a /= expectedBig b
                           && Map.size (expectedBig a) == Map.size (expectedBig b)
                           && Map.size (expectedBig a) > 0
                         | (a, b) <- zip phases (drop 1 phases) ]
    keptBucket = or [ expectedBig a /= expectedBig b && any (`elem` expectedKeys a) (expectedKeys b)
                    | (a, b) <- zip phases (drop 1 phases) ]
    -- a transient failure with something known under the name, or service
    keptName    = or [ Set.member n (phTransient ph) && Map.member n (memAddrs before) && Set.member n (names ph)
                     | (ph, before) <- zip phases befores, n <- [1 .. 6] ]
    keptService = or [ Set.member i (phSrvTransient ph) && Map.member i (memTargets before) && Set.member i (services ph)
                     | (ph, before) <- zip phases befores, i <- [1 .. 3] ]
    forgotten   = or [ Map.notMember n (phDns ph) && Set.notMember n (phTransient ph)
                       && Map.member n (memAddrs before) && Set.member n (names ph)
                     | (ph, before) <- zip phases befores, n <- [1 .. 6] ]
    targetAddrs ph i =
      [ [ IP.toSockAddr (IP.IPv4 ip, tPort) | ip <- Map.findWithDefault [] n (phDns ph) ]
      | Target { tName = Just n, tPort } <- Map.findWithDefault [] i (phSrv ph) ]

-- | One slow name among fast ones: its lookup takes 20 s, theirs 10 ms,
-- and the index has the fast names' addresses by the settle point, and the
-- slow one's only once its lookup is through. Published lookup by lookup,
-- no name waits for another, not even one of its own batch.
prop_slow_name :: Property
prop_slow_name =
  forAllShrink (choose (1, 5)) (filter (>= 1) . shrink) $ \k ->
  forAll (vectorOf 6 genIPv4) $ \ips ->
    let slowName  = 6
        fastNames = [1 .. k]
        phase = Phase { phPools = Just [ (1, Named n 3001 :| []) | n <- fastNames ++ [slowName] ],
                        phDns = Map.fromList (zip [1 .. 6] (map (: []) ips)),
                        phSrv = Map.empty, phTransient = Set.empty, phSrvTransient = Set.empty }
        addr n = IP.toSockAddr (IP.IPv4 (ips !! (n - 1)), 0)
        (early, late) = runSimOrThrow $ do
          Harness { hApply, hAllowances, hThread } <- newHarness (\n -> if n == slowName then 20 else 0) 1 1
          hApply phase
          withAsync hThread $ \_ -> do
            threadDelay 3
            e <- readTVarIO (paIndex hAllowances)
            threadDelay 22
            l <- readTVarIO (paIndex hAllowances)
            return (e, l)
    in counterexample (show (early, late)) $
            counterexample "fast names by 3 s" (all (`Map.member` early) (map addr fastNames))
       .&&. counterexample "the slow name not yet" (Map.notMember (addr slowName) early)
       .&&. counterexample "the slow name once resolved" (Map.member (addr slowName) late)

-- | An answer replaces what was known of its name the moment it comes, not
-- at the end of the poll: a name whose address changed between polls has
-- its new address in the index and not its old one while a slow name's
-- lookup of the same poll is still pending, that one's address kept as
-- last known.
prop_answer_replaces :: Property
prop_answer_replaces =
  forAll (vectorOf 3 genIPv4) $ \ips ->
    ips == nub ips ==>
    let (ipA, ipB, ipS) = case ips of { [a, b, s] -> (a, b, s); _ -> error "three addresses" }
        slowName = 6
        pools = Just [ (1, Named 1 3001 :| []), (1, Named slowName 3001 :| []) ]
        phase dns = Phase { phPools = pools, phDns = dns, phSrv = Map.empty,
                            phTransient = Set.empty, phSrvTransient = Set.empty }
        addr ip = IP.toSockAddr (IP.IPv4 ip, 0)
        (atB, later) = runSimOrThrow $ do
          Harness { hApply, hAllowances, hThread } <- newHarness (\n -> if n == slowName then 20 else 0) 1 1
          hApply (phase (Map.fromList [(1, [ipA]), (slowName, [ipS])]))
          withAsync hThread $ \_ -> do
            -- the first poll through: both names known
            atomically $ readTVar (paIndex hAllowances) >>= check . Map.member (addr ipS)
            hApply (phase (Map.fromList [(1, [ipB]), (slowName, [ipS])]))
            -- the next poll answers the fast name in its first instants
            atomically $ readTVar (paIndex hAllowances) >>= check . Map.member (addr ipB)
            e <- readTVarIO (paIndex hAllowances)
            threadDelay 25
            l <- readTVarIO (paIndex hAllowances)
            return (e, l)
    in counterexample (show (atB, later)) $
            counterexample "the old address stayed" (Map.notMember (addr ipA) atB)
       .&&. counterexample "the slow name's address was dropped" (Map.member (addr ipS) atB)
       .&&. counterexample "later" (Map.keysSet later === Set.fromList [addr ipB, addr ipS])

-- | The wait between polls ends when the ledger peers setting changes, so
-- a phase applied after a toggle is polled at once, not a period later; a
-- poll that indexed nothing of a list with pools, DNS not answering as at
-- boot, is followed by the next after the retry interval; and one that
-- indexed anything waits the period, a name kept as last known among others
-- included, so a dead name on one pool does not shorten the cadence. The
-- schedule's phases are polled in turn, each but the first on the wake a
-- toggle of the setting gives, 3 s apart; the last is applied without a
-- toggle, well inside the period, and the index is read 3 s after it. With
-- @failing@ every name and service of the first phase fails.
prop_wakes_and_retries :: Schedule -> Bool -> Property
prop_wakes_and_retries (Schedule phases) failing =
    counterexample (show (polled, final, observed))
  . classify (isNothing (phPools lastPolled) && not (isNothing (phPools final))) "ledger peers turned on"
  . classify soon "retried: nothing indexed"
  . classify (kept && not soon && not woken && snd (pollPhase memLast final) /= idxLast) "a name kept as last known, the final phase answering otherwise"
  $ observed === expected
  where
    first = case phases of
      p : _ | failing   -> p { phTransient = Set.fromList [1 .. 6], phSrvTransient = Set.fromList [1 .. 3] }
            | otherwise -> p
      []                -> error "a schedule has a phase"
    polled     = first : drop 1 (init phases)
    final      = last phases
    lastPolled = last polled
    -- the model, polled phase by polled phase: a toggle changes nothing
    -- known, and the thread keeps what it knows through it
    step (m, _) ph = pollPhase m ph
    memBefore  = fst (foldl step (Memory Map.empty Map.empty, Map.empty) (init polled))
    (memLast, idxLast) = step (memBefore, Map.empty) lastPolled
    soon  = Map.null idxLast && not (null (bigRelays lastPolled))
    woken = isNothing (phPools lastPolled) /= isNothing (phPools final)
    expected | soon || woken = snd (pollPhase memLast final)
             | otherwise     = idxLast
    kept  = or [ Set.member n (phTransient lastPolled) && Map.member n (memAddrs memBefore)
               | n <- Set.toList (names lastPolled) ]
         || or [ Set.member i (phSrvTransient lastPolled) && Map.member i (memTargets memBefore)
               | i <- Set.toList (services lastPolled) ]
    observed = runSimOrThrow $ do
      Harness { hApply, hAllowances, hThread } <- newHarness (const 0) 1000 1
      hApply first
      withAsync hThread $ \_ -> do
        forM_ (drop 1 polled) $ \ph -> do
          threadDelay 2
          hApply ph { phPools = Nothing }
          threadDelay 1
          hApply ph
        threadDelay 2
        hApply final
        threadDelay 3
        readTVarIO (paIndex hAllowances)

-- | The index key of an IPv4 address a dual-stack socket presents as
-- IPv4-mapped IPv6 is the IPv4 address's own key; any other address keeps
-- its form with its port dropped.
prop_key_unmaps :: Property
prop_key_unmaps =
  forAllShrink genIPv4 shrinkIPv4 $ \ip ->
  forAllShrink genIPv6 shrinkIPv6 $ \ip6 ->
  forAllShrink (choose (1, 65535 :: Int)) shrink $ \p ->
    let port   = fromIntegral p
        plain  = IP.toSockAddr (IP.IPv4 ip, port)
        mapped = IP.toSockAddr (IP.IPv6 (IP.ipv4ToIPv6 ip), port)
        v6     = IP.toSockAddr (IP.IPv6 ip6, port)
    in     counterexample "mapped" (sockAddrWithoutPort mapped === sockAddrWithoutPort plain)
      .&&. counterexample "plain"  (sockAddrWithoutPort plain  === IP.toSockAddr (IP.IPv4 ip, 0))
      .&&. counterexample "v6"     (sockAddrWithoutPort v6     === IP.toSockAddr (IP.IPv6 ip6, 0))
  where
    shrinkIPv4 ip = [ IP.toIPv4 os | os <- shrink (IP.fromIPv4 ip), length os == 4, all octet os ]
    octet o = o >= 0 && o <= 255
    -- any IPv6 address but an IPv4-mapped one
    genIPv6 = do
      ws5 <- vectorOf 5 (choose (0, 65535))
      w5  <- choose (0, 65534)
      ws2 <- vectorOf 2 (choose (0, 65535))
      return (IP.toIPv6 (ws5 ++ [w5] ++ ws2))
    shrinkIPv6 ip6 = [ IP.toIPv6 ws | ws <- shrink (IP.fromIPv6 ip6), length ws == 8
                                    , all (\w -> w >= 0 && w <= 65535) ws
                                    , take 6 ws /= [0, 0, 0, 0, 0, 0xffff] ]
