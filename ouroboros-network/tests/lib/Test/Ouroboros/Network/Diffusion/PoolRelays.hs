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
    [ testProperty "the index follows the ledger and the buckets follow the list" prop_follows
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

-- | Phases in sequence; a third of them keep the previous phase's pools and
-- change only DNS, so re-resolution without a rebuild is exercised.
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
                   return prev { phDns = dns }) ]
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

-- | The generation after each phase: one more every time the list the
-- buckets were built from changes, counting from nothing built.
expectedGenerations :: [Phase] -> [Int]
expectedGenerations phases = tail (scanl step 0 (zip (Nothing : map Just keys) keys))
  where
    keys = map expectedBig phases
    step g (prev, k) = if prev /= Just k then g + 1 else g

-- | Run the thread through the schedule, settling 3 s after each phase: a
-- poll a second with a 10 ms DNS delay per lookup, so both a list change and
-- a DNS change have taken effect by then. Also the most lookups ever in
-- flight at once.
runSchedule :: Schedule -> ([(Int, Map SockAddr [Int])], Int)
runSchedule (Schedule phases) = runSimOrThrow $ do
    ledgerVar  <- newTVarIO []
    useVar     <- newTVarIO DontUseLedgerPeers
    dnsMapVar  <- newTVarIO Map.empty
    timeouts   <- initScript' (singletonScript (DNSTimeout 10))
    delays     <- initScript' (singletonScript (DNSLookupDelay 0.01))
    semaphore  <- newPoolRelaysDNSSemaphore
    allowances <- newPoolAllowances (Allowance 1000) (const (Fresh 0)) (Fresh 0)
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
               atomically $ (,) <$> readTVar (paGeneration allowances)
                                <*> readTVar (paIndex allowances)
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
  $ conjoin [ counterexample ("phase " ++ show i) (idx === expectedIndex ph)
            | (i, ph, (_, idx)) <- zip3 [0 :: Int ..] phases observed ]
    .&&. counterexample "generations" (map fst observed === expectedGenerations phases)
    -- the thread's semaphore has two slots by design, stated here as a
    -- number so a change to the constant is a change to the test
    .&&. counterexample ("lookups in flight at once: " ++ show mostInFlight)
           (mostInFlight <= 2)
  where
    (observed, mostInFlight) = runSchedule sched
    dnsOnlyChange = or [ expectedBig a == expectedBig b && expectedIndex a /= expectedIndex b
                       | (a, b) <- zip phases (drop 1 phases) ]
