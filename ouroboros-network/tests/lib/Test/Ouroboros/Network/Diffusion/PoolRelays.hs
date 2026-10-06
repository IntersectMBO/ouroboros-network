{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.Diffusion.PoolRelays (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow (bracket_)
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Tracer (nullTracer)
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict qualified as IntMap
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Word (Word16)
import Network.DNS qualified as DNS
import Network.Socket (PortNumber, SockAddr)
import System.Random (mkStdGen)

import Ouroboros.Network.Block (SlotNo (..))
import Ouroboros.Network.Diffusion.PoolAllowances
import Ouroboros.Network.Diffusion.PoolRelays
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS (DNSActions (..),
           DNSLookupType (..), PeerActionsDNS (..), newPoolRelaysDNSSemaphore)
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
    [ testProperty "the index follows the ledger; only a pool-count change rebuilds, empty" prop_follows
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
-- off, the A records DNS has for each name and the SRV records it has for
-- each service name (absent: a name error).
data Phase = Phase {
    phPools :: Maybe [(Int, NonEmpty Relay)],
    phDns   :: Map Int [IP.IPv4],
    phSrv   :: Map Int [Target]
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
  return Phase { phPools = if disabled then Nothing else Just pools, phDns = dns, phSrv = srv }

-- | Phases in sequence: some keep the previous phase's pools and change only
-- DNS, and some keep the number of pools and change their stake and relays,
-- so both kinds of change without a rebuild are exercised.
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
                   return prev { phDns = phDns fresh, phSrv = phSrv fresh })
          , (1, case phPools prev of
                     Just ps@(_ : _) -> do
                       fresh <- genPhase
                       ps'   <- forM ps $ \_ -> (,) <$> choose (1, 100)
                                                     <*> (NonEmpty.fromList <$> (choose (1, 3) >>= \nr -> vectorOf nr genRelay))
                       return fresh { phPools = Just ps' }
                     _ -> genPhase) ]
        go (k - 1) (next : acc)
      go _ [] = return []
  shrink (Schedule phases) =
       [ Schedule ps | ps <- shrinkList shrinkPhase phases, not (null ps) ]
    where
      shrinkPhase ph@Phase { phPools = Just pools } =
           [ ph { phPools = Just ps } | ps <- shrinkList (const []) pools ]
        ++ shrinkRecords ph
      shrinkPhase ph = shrinkRecords ph
      shrinkRecords ph@Phase { phDns, phSrv } =
           [ ph { phDns = Map.fromList d } | d <- shrinkList (const []) (Map.toList phDns) ]
        ++ [ ph { phSrv = Map.fromList s } | s <- shrinkList shrinkTargets (Map.toList phSrv) ]
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

-- | The addresses of a name: what DNS has for it, or none.
namedAddrs :: Phase -> DNS.Domain -> PortNumber -> [SockAddr]
namedAddrs Phase { phDns } d port =
  [ IP.toSockAddr (IP.IPv4 ip, port) | (i, ips) <- Map.toList phDns, domainName i == d, ip <- ips ]

-- | The addresses of a service, per target: every target of every priority
-- that names a name, each with the port of its record.
targetAddrs :: Phase -> Int -> [[SockAddr]]
targetAddrs ph@Phase { phSrv } i =
  [ namedAddrs ph (domainName n) tPort
  | Target { tName = Just n, tPort } <- Map.findWithDefault [] i phSrv ]

expectedIndex :: Phase -> Map SockAddr [Int]
expectedIndex ph =
    Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
      [ (sockAddrWithoutPort a, [pos])
      | (pos, (_, rs)) <- zip [0 ..] (Map.elems (expectedBig ph))
      , r <- NonEmpty.toList rs
      , a <- addrs r ]
  where
    addrs (RelayAccessAddress ip port) = [IP.toSockAddr (ip, port)]
    addrs (RelayAccessDomain d port)   = namedAddrs ph d port
    addrs (RelayAccessSRVDomain d)     = concat [ concat (targetAddrs ph i)
                                                | i <- [1 .. 3], srvDomain i == d ]

-- | The services the phase's pools register.
services :: Phase -> [Int]
services Phase { phPools } =
  [ i | Just pools <- [phPools], (_, rs) <- pools, Service i <- NonEmpty.toList rs ]

-- | Whether each phase rebuilds: the first always, and later ones when the
-- number of pools differs from the phase before.
expectedRebuilds :: [Phase] -> [Bool]
expectedRebuilds phases = zipWith (/=) (Nothing : map Just counts) (map Just counts)
  where
    counts = map (Map.size . expectedBig) phases

-- | The generation after each phase: one more for every rebuild.
expectedGenerations :: [Phase] -> [Int]
expectedGenerations = tail . scanl (\g r -> if r then g + 1 else g) 0 . expectedRebuilds

-- | What the harness marks every bucket with after observing a phase: a
-- charged total the buckets would never reach by themselves.
mark :: Charged
mark = Charged (-1234)

-- | Every bucket's credit after each phase: zero after a rebuild, since new
-- buckets start empty and the reading never moves; otherwise the mark the
-- harness left on the previous phase's buckets, untouched.
expectedCredits :: [Phase] -> [[Int]]
expectedCredits phases =
  [ replicate (Map.size (expectedBig ph)) (if r then 0 else credit testAllowance (Fresh 0) mark)
  | (ph, r) <- zip phases (expectedRebuilds phases) ]

testAllowance :: Allowance
testAllowance = Allowance 1000

-- | Run the thread through the schedule, settling 3 s after each phase: a
-- poll a second with a 10 ms DNS delay per lookup, so both a list change and
-- a DNS change have taken effect by then. Also the most lookups ever in
-- flight at once.
runSchedule :: Int -> Schedule -> ([(Int, Map SockAddr [Int], [Int])], Int)
runSchedule seed (Schedule phases) = runSimOrThrow $ do
    ledgerVar  <- newTVarIO []
    useVar     <- newTVarIO DontUseLedgerPeers
    dnsMapVar  <- newTVarIO Map.empty
    timeouts   <- initScript' (singletonScript (DNSTimeout 10))
    delays     <- initScript' (singletonScript (DNSLookupDelay 0.01))
    semaphore  <- newPoolRelaysDNSSemaphore
    allowances <- newPoolAllowances testAllowance (const (Fresh 0)) (Fresh 0)
    inFlight   <- newTVarIO (0 :: Int, 0 :: Int)      -- now, and the most ever
    let apply Phase { phPools, phDns, phSrv } = atomically $ do
          writeTVar ledgerVar (maybe [] toLedger phPools)
          writeTVar useVar (if isNothing phPools then DontUseLedgerPeers else UseLedgerPeers Always)
          writeTVar dnsMapVar (toDns phDns phSrv)
        interface = LedgerPeersConsensusInterface {
            lpGetLatestSlot  = pure (At (SlotNo 1000)),
            lpGetLedgerPeers = readTVar ledgerVar,
            lpExtraAPI       = ()
          }
        mock = mockDNSActions nullTracer LookupReqAOnly (curry IP.toSockAddr)
                              dnsMapVar timeouts delays
        -- every lookup counted while it runs
        counted = mock {
            dnsLookupWithTTL = \kind domain conf resolver rng ->
              bracket_ (atomically $ modifyTVar inFlight (\(n, most) -> (n + 1, max most (n + 1))))
                       (atomically $ modifyTVar inFlight (\(n, most) -> (n - 1, most)))
                       (dnsLookupWithTTL mock kind domain conf resolver rng)
          }
        dns = PeerActionsDNS {
            paToPeerAddr = curry IP.toSockAddr,
            paDnsActions = counted
          }
        args = PoolRelaysArgs {
            prTracer       = nullTracer,
            prConsensus    = interface,
            prUseLedger    = readTVar useVar,
            prSnapshot     = pure Nothing,
            prSRVPrefix    = testSRVPrefix,
            prDNS          = dns,
            prSemaphore    = semaphore,
            prRng          = mkStdGen seed,
            prAddressKey   = sockAddrWithoutPort,
            prPollInterval = 1
          }
    case phases of
         []       -> return ([], 0)
         p0 : _   -> do
           apply p0
           observed <- withAsync (poolRelaysThread args allowances) $ \_ ->
             forM phases $ \ph -> do
               apply ph
               threadDelay 3
               atomically $ do
                 g       <- readTVar (paGeneration allowances)
                 idx     <- readTVar (paIndex allowances)
                 buckets <- IntMap.elems <$> readTVar (paBuckets allowances)
                 credits <- mapM (fmap (credit testAllowance (Fresh 0)) . readTVar) buckets
                 mapM_ (`writeTVar` mark) buckets
                 return (g, idx, credits)
           most <- snd <$> readTVarIO inFlight
           return (observed, most)

-- | The seed is the thread's: membership must not depend on it, so the
-- oracle is checked across seeds.
prop_follows :: Int -> Schedule -> Property
prop_follows seed sched@(Schedule phases) =
    counterexample (show observed)
  . classify (any (isNothing . phPools) phases) "a phase with ledger peers off"
  . classify (dnsOnlyChange) "a DNS-only change between phases"
  . classify (any ((> 1) . Map.size . expectedBig) phases) "more than one pool"
  . tabulate "addresses indexed" (map (show . Map.size . expectedIndex) phases)
  . classify (mostInFlight == 2) "both DNS slots used at once"
  . classify sameCountChange "a list change keeping the pool count"
  . classify (anyService distinctTargets) "a service whose targets reach distinct addresses"
  . classify (anyService backupPriority) "a service with a backup priority"
  . classify (anyService failingTarget) "a service with a target that fails"
  . classify (anyService unanswered) "a service without an SRV answer"
  $ conjoin [ counterexample ("phase " ++ show i) (idx === expectedIndex ph)
            | (i, ph, (_, idx, _)) <- zip3 [0 :: Int ..] phases observed ]
    .&&. counterexample "generations" ([ g | (g, _, _) <- observed ] === expectedGenerations phases)
    .&&. counterexample "bucket credits" ([ c | (_, _, c) <- observed ] === expectedCredits phases)
    -- the thread's semaphore has two slots by design, stated here as a
    -- number so a change to the constant is a change to the test
    .&&. counterexample ("lookups in flight at once: " ++ show mostInFlight)
           (mostInFlight <= 2)
  where
    (observed, mostInFlight) = runSchedule seed sched
    dnsOnlyChange = or [ expectedBig a == expectedBig b && expectedIndex a /= expectedIndex b
                       | (a, b) <- zip phases (drop 1 phases) ]
    anyService p = or [ p ph i | ph <- phases, i <- services ph ]
    -- what tells enumeration from a pick: two targets that resolve, to
    -- different addresses
    distinctTargets ph i =
      Set.size (Set.fromList [ hosts | hosts <- map (map sockAddrWithoutPort) (targetAddrs ph i)
                                     , not (null hosts) ]) >= 2
    backupPriority ph i =
      Set.size (Set.fromList (map tPriority (Map.findWithDefault [] i (phSrv ph)))) >= 2
    failingTarget ph i = or [ null (namedAddrs ph (domainName n) 0)
                            | Target { tName = Just n } <- Map.findWithDefault [] i (phSrv ph) ]
    unanswered ph i = Map.notMember i (phSrv ph)
    sameCountChange = or [ expectedBig a /= expectedBig b
                           && Map.size (expectedBig a) == Map.size (expectedBig b)
                           && Map.size (expectedBig a) > 0
                         | (a, b) <- zip phases (drop 1 phases) ]
