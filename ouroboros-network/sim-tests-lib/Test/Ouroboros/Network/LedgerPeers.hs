{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Ouroboros.Network.LedgerPeers where

import Control.Exception (SomeException (..))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.IP qualified as IP
import Data.List (foldl', intercalate, isPrefixOf, nub, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Ord (Down (..))
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word
import System.Random

import Network.DNS (Domain)

import Cardano.Slotting.Slot (SlotNo)
import Control.Concurrent.Class.MonadSTM.Strict
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
import Ouroboros.Network.Testing.Data.Script
import Test.Ouroboros.Network.PeerSelection.RootPeersDNS
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Text.Printf

tests :: TestTree
tests = testGroup "Ouroboros.Network.LedgerPeers"
  [ testProperty "Pick 100%" prop_pick100
  , testProperty "Pick" prop_pick
  , testProperty "accBigPoolStake" prop_accBigPoolStake
  , testProperty "getLedgerPeers invariants" prop_getLedgerPeers
  ]

newtype ArbitraryPortNumber = ArbitraryPortNumber { getArbitraryPortNumber :: PortNumber }

instance Arbitrary ArbitraryPortNumber where
    arbitrary = elements
              $ map (ArbitraryPortNumber . read . show)
              $ ([1000..1100] :: [Int])

newtype ArbitraryRelayAccessPoint =
  ArbitraryRelayAccessPoint { getArbitraryRelayAccessPoint :: RelayAccessPoint }
  deriving (Eq, Ord) via RelayAccessPoint

instance Arbitrary ArbitraryRelayAccessPoint where
    arbitrary =
      ArbitraryRelayAccessPoint <$>
        oneof [ RelayAccessAddress (read "1.1.1.1")     . getArbitraryPortNumber <$> arbitrary
              , RelayAccessDomain  "relay.iohk.example" . getArbitraryPortNumber <$> arbitrary
              ]

newtype ArbitraryLedgerStateJudgement =
  ArbitraryLedgerStateJudgement {
    getArbitraryLedgerStateJudgement :: LedgerStateJudgement
  } deriving Show

instance Arbitrary ArbitraryLedgerStateJudgement where
    arbitrary =
      ArbitraryLedgerStateJudgement <$>
        oneof [ pure YoungEnough
              , pure TooOld
              ]
    shrink (ArbitraryLedgerStateJudgement YoungEnough) =
      [ArbitraryLedgerStateJudgement TooOld]
    shrink (ArbitraryLedgerStateJudgement TooOld)      =
      []

newtype ArbitrarySlotNo =
  ArbitrarySlotNo {
    getArbitrarySlotNo :: SlotNo
  } deriving Show

-- We generate integers including negative ones, which is fine for the purpose
-- of the tests we run.
instance Arbitrary ArbitrarySlotNo where
    arbitrary =
      ArbitrarySlotNo . fromInteger <$> arbitrary

data StakePool = StakePool {
      spStake :: !Word64
    , spRelay :: NonEmpty RelayAccessPoint
    } deriving Show



instance Arbitrary StakePool where
    arbitrary = do
        stake <- choose (0, 1000000)
        (ArbitraryRelayAccessPoint firstRelay) <- arbitrary
        moreRelays <- filter (/= firstRelay) . nub . map unAddr <$> arbitrary
        return $ StakePool stake (firstRelay :| moreRelays)
      where
        unAddr (ArbitraryRelayAccessPoint a) = a

    shrink sp@StakePool { spStake, spRelay } =
      [ sp { spStake = spStake' }
      | spStake' <- shrink spStake
      ]
      ++
      [ sp { spRelay = NonEmpty.fromList spRelay' }
      | spRelay'@(_ : _) <- shrinkList (const [])
                                       (NonEmpty.toList spRelay)
      ]

newtype LedgerPools =
  LedgerPools { getLedgerPools :: [(PoolStake, NonEmpty RelayAccessPoint)] }
  deriving Show

instance Arbitrary LedgerPools where
    arbitrary = LedgerPools . calculateRelativeStake <$> arbitrary

calculateRelativeStake :: [StakePool]
                       -> [(PoolStake, NonEmpty RelayAccessPoint)]
calculateRelativeStake sps =
    let totalStake = foldl' (\s p -> s + spStake p) 0 sps in
    map (\p -> ( PoolStake (fromIntegral (spStake p) % fromIntegral totalStake)
               , spRelay p)) sps

genLedgerPoolsFrom :: [RelayAccessPoint] -> Gen LedgerPools
genLedgerPoolsFrom relays = do
  stake <- choose (0, 1000000)
  (ArbitraryRelayAccessPoint firstRelay) <- arbitrary
  let moreRelays = filter (/= firstRelay) . nub $ relays
      stakePool = StakePool stake (firstRelay :| moreRelays)
  return (LedgerPools $ calculateRelativeStake [stakePool])

newtype ArbLedgerPeersKind = ArbLedgerPeersKind LedgerPeersKind
  deriving Show

instance Arbitrary ArbLedgerPeersKind where
    arbitrary = ArbLedgerPeersKind <$> elements [AllLedgerPeers, BigLedgerPeers]
    shrink (ArbLedgerPeersKind AllLedgerPeers) = [ArbLedgerPeersKind BigLedgerPeers]
    shrink (ArbLedgerPeersKind BigLedgerPeers) = []

-- | A pool with 100% stake should always be picked.
prop_pick100 :: Word16
             -> NonNegative Int -- ^ number of pools with 0 stake
             -> ArbLedgerPeersKind
             -> MockRoots
             -> DelayAndTimeoutScripts
             -> ArbitrarySlotNo
             -> ArbitraryLedgerStateJudgement
             -> Property
prop_pick100 seed (NonNegative n) (ArbLedgerPeersKind ledgerPeersKind) (MockRoots _ dnsMapScript _ _)
             (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
             (ArbitrarySlotNo slot) (ArbitraryLedgerStateJudgement lsj) =
    let rng = mkStdGen $ fromIntegral seed
        sps = [ (0, RelayAccessAddress (read $ "0.0.0." ++ show a) 1 :| [])
              | a <- [0..n]
              ]
           ++ [ (1, RelayAccessAddress (read "1.1.1.1") 1  :| []) ]

        accumulatedStakeMap = case ledgerPeersKind of
          AllLedgerPeers -> accPoolStake sps
          BigLedgerPeers -> accBigPoolStake sps

        sim :: IOSim s [RelayAccessPoint]
        sim = do
          let dnsMap = scriptHead dnsMapScript
          dnsMapVar <- newTVarIO dnsMap

          dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
          dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript

          dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

          withLedgerPeers
                rng dnsSemaphore (curry IP.toSockAddr) verboseTracer
                (pure (UseLedgerPeers (After 0)))
                interface
                (mockDNSActions @SomeException dnsMapVar dnsTimeoutScriptVar dnsLookupDelayScriptVar)
                (\request _ -> do
                  threadDelay 1900 -- we need to invalidate ledger peer's cache
                  resp <- request (NumberOfPeers 1) ledgerPeersKind
                  pure $ case resp of
                    Nothing          -> []
                    Just (peers, _)  -> [ RelayAccessAddress ip port
                                        | Just (ip, port) <- IP.fromSockAddr
                                                         <$> Set.toList peers
                                        ]
                )
          where
            interface =
              LedgerPeersConsensusInterface
                (pure slot)
                (pure lsj)
                (pure (Map.elems accumulatedStakeMap))

    in counterexample (show accumulatedStakeMap) $ ioProperty $ do
        tr' <- evaluateTrace (runSimTrace sim)
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn peers _trace -> do
                 -- printf "Log: %s\n" (intercalate "\n" _trace)
                 return $ peers === [ RelayAccessAddress (read "1.1.1.1") 1 ]

-- | Verify that given at least one peer we manage to pick `count` peers.
prop_pick :: LedgerPools
          -> ArbLedgerPeersKind
          -> Word16
          -> Word16
          -> MockRoots
          -> Script DNSLookupDelay
          -> ArbitrarySlotNo
          -> ArbitraryLedgerStateJudgement
          -> Property
prop_pick (LedgerPools lps) (ArbLedgerPeersKind ledgerPeersKind) count seed (MockRoots _ dnsMapScript _ _)
          dnsLookupDelayScript (ArbitrarySlotNo slot) (ArbitraryLedgerStateJudgement lsj) =
    let rng = mkStdGen $ fromIntegral seed

        sim :: IOSim s [RelayAccessPoint]
        sim = do
          dnsMapScriptVar <- initScript' dnsMapScript
          dnsMap <- stepScript' dnsMapScriptVar
          dnsMapVar <- newTVarIO dnsMap

          dnsTimeoutScriptVar <- initScript' (Script (DNSTimeout 0 :| []))
          dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
          dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

          withLedgerPeers
                rng dnsSemaphore (curry IP.toSockAddr) verboseTracer
                (pure (UseLedgerPeers (After 0)))
                interface
                (mockDNSActions @SomeException dnsMapVar dnsTimeoutScriptVar dnsLookupDelayScriptVar)
                (\request _ -> do
                  threadDelay 1900 -- we need to invalidate ledger peer's cache
                  resp <- request (NumberOfPeers count) ledgerPeersKind
                  pure $ case resp of
                    Nothing          -> []
                    Just (peers, _)  -> [ reverseLookup (RelayAccessAddress ip port)
                                        | Just (ip, port) <- IP.fromSockAddr
                                                      `fmap` Set.toList peers
                                        ]
                )
          where
            interface :: LedgerPeersConsensusInterface (IOSim s)
            interface = LedgerPeersConsensusInterface
                          (pure slot)
                          (pure lsj)
                          (pure lps)

            domainMap :: Map Domain (Set IP)
            domainMap = Map.fromList [("relay.iohk.example", Set.singleton (read "2.2.2.2"))]

            reverseLookup :: RelayAccessPoint -> RelayAccessPoint
            reverseLookup ap@(RelayAccessAddress ip port)
              = case [ domain
                     | (domain, addrs) <- Map.assocs domainMap
                     , ip `Set.member` addrs
                     ] of
                  (domain : _) -> RelayAccessDomain domain port
                  _            -> ap
            reverseLookup ap = ap



    in ioProperty $ do
        tr' <- evaluateTrace (runSimTrace sim)
        case tr' of
             SimException e trace -> do
                 return $ counterexample (intercalate "\n" $ show e : trace) False
             SimDeadLock trace -> do
                 return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
             SimReturn peers trace -> do
                 let numOfPeers = length peers
                 if null lps
                    then return $ property $ null peers
                    else return $ counterexample (intercalate "\n" $
                                                    ( "Lenght missmatch "
                                                      ++ show (length peers)
                                                    )
                                                    : trace)
                                      (numOfPeers
                                        === fromIntegral count `min` numOfPeers)


prop_accBigPoolStake :: LedgerPools -> Property
prop_accBigPoolStake  (LedgerPools [])        = property True
prop_accBigPoolStake  (LedgerPools lps@(_:_)) =

         -- the accumulated map is non empty, whenever ledger peers set is non
         -- empty
         not (Map.null accumulatedStakeMap)

         -- the relative stake of all large pools is greater than
         -- bigLedgerPeerQuota
    .&&. counterexample
           ("not enough stake: " ++ show (accumulatedStakeMap, lps))
           (unPoolStake (getSum (foldMap (Sum . fst) accumulatedStakeMap)
                / sum (fst <$> lps))
             >= unAccPoolStake bigLedgerPeerQuota)

         -- This property checks that elements of
         -- `accBigPoolStake` form an initial sub-list of the ordered ledger
         -- peers by stake (from large to small).
         --
         -- We relay on the fact that `Map.elems` returns a list of elements
         -- ordered by keys (as they are in the `Map`).
    .&&. let lps'  = sortOn (Down . fst) lps
             elems = Map.elems accumulatedStakeMap
         in counterexample ("initial sublist vaiolation: " ++ show (elems, lps'))
          $ elems `isPrefixOf` lps'
  where
    accumulatedStakeMap = accBigPoolStake lps

prop_getLedgerPeers :: ArbitrarySlotNo
                    -> ArbitraryLedgerStateJudgement
                    -> LedgerPools
                    -> ArbitrarySlotNo
                    -> Property
prop_getLedgerPeers (ArbitrarySlotNo curSlot)
                    (ArbitraryLedgerStateJudgement lsj)
                    (LedgerPools lps)
                    slot =
  let sim :: IOSim m LedgerPeers
      sim = atomically $ getLedgerPeers interface (getArbitrarySlotNo slot)

      result :: LedgerPeers
      result = runSimOrThrow sim

   in counterexample (show result) $
      case result of
        LedgerPeers _ _ -> property (curSlot >= getArbitrarySlotNo slot)
        BeforeSlot      -> property (curSlot < getArbitrarySlotNo slot)
  where
    interface :: LedgerPeersConsensusInterface (IOSim s)
    interface = LedgerPeersConsensusInterface
                  (pure curSlot)
                  (pure lsj)
                  (pure (Map.elems (accPoolStake lps)))

-- TODO: Belongs in iosim.
data SimResult a = SimReturn a [String]
                 | SimException SomeException [String]
                 | SimDeadLock [String]

-- Traverses a list of trace events and returns the result along with all log messages.
-- In case of a pure exception, ie an assert, all tracers evaluated so far are returned.
evaluateTrace :: SimTrace a -> IO (SimResult a)
evaluateTrace = go []
  where
    go as tr = do
      r <- try (evaluate tr)
      case r of
        Right (SimTrace _ _ _ (EventSay s) tr')      -> go (s : as) tr'
        Right (SimTrace _ _ _ _ tr' )                -> go as tr'
        Right (SimPORTrace _ _ _ _ (EventSay s) tr') -> go (s : as) tr'
        Right (SimPORTrace _ _ _ _ _ tr' )           -> go as tr'
        Right (TraceMainReturn _ _ a _)              -> pure $ SimReturn a (reverse as)
        Right (TraceMainException _ _ e _)           -> pure $ SimException e (reverse as)
        Right (TraceDeadlock _ _)                    -> pure $ SimDeadLock (reverse as)
        Right TraceLoop                              -> error "IOSimPOR step time limit exceeded"
        Right (TraceInternalError e)                 -> error ("IOSim: " ++ e)
        Left  (SomeException e)                      -> pure $ SimException (SomeException e) (reverse as)

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadSay m
                       , MonadMonotonicTime m
                       , Show a
                       )
               => Tracer m a
verboseTracer = nullTracer -- threadAndTimeTracer $ Tracer (say . show)

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadMonotonicTime m
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- show <$> myThreadId
    traceWith tr $! WithThreadAndTime now tid s
