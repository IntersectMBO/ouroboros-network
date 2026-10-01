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
    [ testProperty "the index follows the ledger; a pool keeps its bucket by its relays" prop_follows
    , testProperty "an IPv4-mapped address keys as the IPv4 address" prop_key_unmaps
    ]

-- | A relay as a pool registers it: one of six names, or a literal address,
-- each on one of two ports.
data Relay = Named Int PortNumber | Literal IP.IPv4 PortNumber
  deriving (Show, Eq)

-- | One stretch of the run: the pools with their weights, or ledger peers
-- off, and the A records DNS has for each name (absent: a name error).
data Phase = Phase {
    phPools :: Maybe [(Int, NonEmpty Relay)],
    phDns   :: Map Int [IP.IPv4]
  }
  deriving Show

newtype Schedule = Schedule [Phase]
  deriving Show

testSRVPrefix :: SRVPrefix
testSRVPrefix = "_cardano._tcp"

domainName :: Int -> DNS.Domain
domainName i = BSC.pack ("test" ++ show i)

genIPv4 :: Gen IP.IPv4
genIPv4 = IP.toIPv4 <$> vectorOf 4 (choose (1, 254))

genRelay :: Gen Relay
genRelay = frequency
  [ (3, Named   <$> choose (1, 6) <*> elements [3001, 3002])
  , (1, Literal <$> genIPv4       <*> elements [3001, 3002]) ]

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
  return Phase { phPools = if disabled then Nothing else Just pools, phDns = dns }

-- | Phases in sequence: some keep the previous phase's pools and change only
-- DNS, so re-resolution is exercised; some keep the pools' relays and
-- change their stakes, dropping a pool maybe, so a bucket moving with its
-- pool is exercised.
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
          , (1, do dns <- phDns <$> genPhase
                   return prev { phDns = dns })
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
    where
      shrinkPhase ph@Phase { phPools = Just pools, phDns } =
           [ ph { phPools = Just ps } | ps <- shrinkList (const []) pools ]
        ++ [ ph { phDns = Map.fromList d } | d <- shrinkList (const []) (Map.toList phDns) ]
      shrinkPhase ph@Phase { phDns } =
           [ ph { phDns = Map.fromList d } | d <- shrinkList (const []) (Map.toList phDns) ]

toLedger :: [(Int, NonEmpty Relay)] -> [(PoolStake, NonEmpty LedgerRelayAccessPoint)]
toLedger pools =
    [ (PoolStake (fromIntegral w % weight), fmap relay rs) | (w, rs) <- pools ]
  where
    weight = fromIntegral (max 1 (sum (map fst pools))) :: Integer
    relay (Named i port)    = LedgerRelayAccessDomain (domainName i) port
    relay (Literal ip port) = LedgerRelayAccessAddress (IP.IPv4 ip) port

toDns :: Map Int [IP.IPv4] -> MockDNSMap
toDns dns = Map.fromList [ ((domainName i, DNS.A), Left [ (IP.IPv4 ip, 300) | ip <- ips ])
                         | (i, ips) <- Map.toList dns ]

-- | The big-ledger prefix as the thread must see it: positions in
-- descending stake, every relay resolved, ports stripped.
expectedBig :: Phase -> Map AccPoolStake (PoolStake, NonEmpty RelayAccessPoint)
expectedBig Phase { phPools = Nothing }    = Map.empty
expectedBig Phase { phPools = Just pools } =
  accBigPoolStakeMap [ (s, fmap (prefixLedgerRelayAccessPoint testSRVPrefix) rs)
                     | (s, rs) <- toLedger pools ]

expectedIndex :: Phase -> Map SockAddr [Int]
expectedIndex ph@Phase { phDns } =
    Map.map (Set.toAscList . Set.fromList) $ Map.fromListWith (++)
      [ (sockAddrWithoutPort a, [pos])
      | (pos, (_, rs)) <- zip [0 ..] (Map.elems (expectedBig ph))
      , r <- NonEmpty.toList rs
      , a <- addrs r ]
  where
    addrs (RelayAccessAddress ip port) = [IP.toSockAddr (ip, port)]
    addrs (RelayAccessDomain d port)   = [ IP.toSockAddr (IP.IPv4 ip, port)
                                         | (i, ips) <- Map.toList phDns, domainName i == d, ip <- ips ]
    addrs (RelayAccessSRVDomain _)     = []

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
-- phase's list too and so kept its bucket; the capacity otherwise, since a
-- new bucket starts full and the reading never moves.
expectedCredits :: [Phase] -> [[(PoolKey, Int)]]
expectedCredits phases =
  [ [ (k, if k `elem` before then credit testAllowance (Fresh 0) mark else capacity testAllowance)
    | k <- expectedKeys ph ]
  | (before, ph) <- zip ([] : map expectedKeys phases) phases ]

testAllowance :: Allowance
testAllowance = Allowance 1000

-- | Run the thread through the schedule, settling 3 s after each phase: a
-- poll a second with a 10 ms DNS delay per lookup, so both a list change and
-- a DNS change have taken effect by then. Also the most lookups ever in
-- flight at once.
runSchedule :: Schedule -> ([(Map SockAddr [Int], [(PoolKey, Int)])], Int)
runSchedule (Schedule phases) = runSimOrThrow $ do
    ledgerVar  <- newTVarIO []
    useVar     <- newTVarIO DontUseLedgerPeers
    dnsMapVar  <- newTVarIO Map.empty
    timeouts   <- initScript' (singletonScript (DNSTimeout 10))
    delays     <- initScript' (singletonScript (DNSLookupDelay 0.01))
    semaphore  <- newPoolRelaysDNSSemaphore
    allowances <- newPoolAllowances testAllowance (const (Fresh 0))
    inFlight   <- newTVarIO (0 :: Int, 0 :: Int)      -- now, and the most ever
    let apply Phase { phPools, phDns } = atomically $ do
          writeTVar ledgerVar (maybe [] toLedger phPools)
          writeTVar useVar (if isNothing phPools then DontUseLedgerPeers else UseLedgerPeers Always)
          writeTVar dnsMapVar (toDns phDns)
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
            prRng          = mkStdGen 42,
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
                 idx     <- readTVar (paIndex allowances)
                 buckets <- IntMap.elems <$> readTVar (paBuckets allowances)
                 credits <- mapM (\(k, tv) -> (,) k . credit testAllowance (Fresh 0) <$> readTVar tv)
                                 buckets
                 mapM_ ((`writeTVar` mark) . snd) buckets
                 return (idx, credits)
           most <- snd <$> readTVarIO inFlight
           return (observed, most)

prop_follows :: Schedule -> Property
prop_follows sched@(Schedule phases) =
    counterexample (show observed)
  . classify (any (isNothing . phPools) phases) "a phase with ledger peers off"
  . classify (dnsOnlyChange) "a DNS-only change between phases"
  . classify (any ((> 1) . Map.size . expectedBig) phases) "more than one pool"
  . tabulate "addresses indexed" (map (show . Map.size . expectedIndex) phases)
  . classify (mostInFlight == 2) "both DNS slots used at once"
  . classify keptBucket "a pool kept its bucket through a list change"
  $ conjoin [ counterexample ("phase " ++ show i) (idx === expectedIndex ph)
            | (i, ph, (idx, _)) <- zip3 [0 :: Int ..] phases observed ]
    .&&. counterexample "bucket credits" ([ c | (_, c) <- observed ] === expectedCredits phases)
    -- the thread's semaphore has two slots by design, stated here as a
    -- number so a change to the constant is a change to the test
    .&&. counterexample ("lookups in flight at once: " ++ show mostInFlight)
           (mostInFlight <= 2)
  where
    (observed, mostInFlight) = runSchedule sched
    dnsOnlyChange = or [ expectedBig a == expectedBig b && expectedIndex a /= expectedIndex b
                       | (a, b) <- zip phases (drop 1 phases) ]
    keptBucket = or [ expectedBig a /= expectedBig b && any (`elem` expectedKeys a) (expectedKeys b)
                    | (a, b) <- zip phases (drop 1 phases) ]

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
