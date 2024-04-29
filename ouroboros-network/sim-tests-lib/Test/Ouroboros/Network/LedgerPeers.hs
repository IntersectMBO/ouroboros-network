{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Ouroboros.Network.LedgerPeers where

import Codec.CBOR.FlatTerm
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException (..))
import Control.Monad (forM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim hiding (SimResult)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.Aeson
import Data.Aeson.Types as Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.IP qualified as IP
import Data.List (foldl', intercalate, isPrefixOf, nub, sort, sortOn, zip4,
           zip6)
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

import Cardano.Binary
import Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..))
import Ouroboros.Network.PeerSelection.LedgerPeers
import Ouroboros.Network.PeerSelection.LedgerPeers.Utils
           (recomputeRelativeStake)
import Ouroboros.Network.PeerSelection.RelayAccessPoint
import Ouroboros.Network.PeerSelection.RootPeersDNS
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
  , testProperty "accumulateBigLedgerStake" prop_accumulateBigLedgerStake
  , testProperty "recomputeRelativeStake" prop_recomputeRelativeStake
  , testProperty "getLedgerPeers invariants" prop_getLedgerPeers
  , testProperty "LedgerPeerSnapshot encode/decode version 1" prop_ledgerPeerSnapshotV1
  , testProperty "Choose big pool peers from ledger or snapshot" prop_use_snapshot_bigledger_peers
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

-- TODO: import the `SlotNo` instance from
-- `Test.Ouroboros.Network.PeerSelection.Instances`
newtype ArbitrarySlotNo =
  ArbitrarySlotNo {
    getArbitrarySlotNo :: SlotNo
  } deriving Show

-- We generate integers including negative ones, which is fine for the purpose
-- of the tests we run.
instance Arbitrary ArbitrarySlotNo where
    arbitrary =
      ArbitrarySlotNo . SlotNo <$> arbitrary

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

-- | This test checks whether requesting ledger peers works as intended
-- when snapshot data is available. A number of requests is queued up
-- with changing ledger and snapshot slot positions (simulating chain advancement and
-- reloading of snapshot data). For each request, peers must be returned from the right
-- source - either the ledger or snapshot, depending on whether which source is fresher,
-- as well as taking into account the type of ledger peers being requested -- all or big only.
--
prop_use_snapshot_bigledger_peers :: Word16
                                  -> MockRoots
                                  -> DelayAndTimeoutScripts
                                  -> ArbitraryLedgerStateJudgement
                                  -> Property
prop_use_snapshot_bigledger_peers seed (MockRoots _ dnsMapScript _ _)
                                  (DelayAndTimeoutScripts dnsLookupDelayScript dnsTimeoutScript)
                                  (ArbitraryLedgerStateJudgement lsj) = property $ do
  -- snapshotSlots has duplicates removed since the test will fail when two consecutive
  -- snapshot slot numbers are identical but the ledger pools are different (which is likely given random generation).
  -- When using a particular snapshot, the request function provided by withLedgerPeers caches the results for
  -- the corresponding slot number to avoid recomputating the same things every time we request a new peer when snapshot
  -- data is used. Furthermore, each test is limited to roughly ~10 slot changes to speed things up.
  (snapshotSlots, ledgerSlots)    <-     bimap nub sort . unzip
                                     <$> sized (\n -> if n <= 10
                                                      then listOf1 arbitrary
                                                      else resize 10 (listOf1 (resize n arbitrary)))
  let snapshotSlots' = snapshotSlots ++ repeat (last snapshotSlots) -- ^ fill in missing slots removed by nub

  (ledgerPeersKinds, ledgerPools) <- unzip <$> forM ledgerSlots
                                                    (\(Positive relativeToSlot) ->
                                                       (,) <$> arbitrary <*>   resize relativeToSlot arbitrary
                                                                             `suchThat`
                                                                               (not . null . getLedgerPools))

  snapshotPools :: [Maybe LedgerPools] <-
    forM snapshotSlots (\(Positive relativeToSlot) ->            resize relativeToSlot arbitrary
                                                      `suchThat` maybe True (not . null . getLedgerPools))
  let snapshotPools' = snapshotPools ++ repeat (last snapshotPools) -- ^ fill in missing data

  let ledgerPeerSnapshots = [snap <&> \(LedgerPools pools) ->
                                        LedgerPeerSnapshot (At (fromIntegral slot), Map.toList $ accPoolStake pools)
                            | snap <- snapshotPools
                            | (Positive slot) <- snapshotSlots]

      (rng1, rng2) = split . mkStdGen . fromIntegral $ seed
      sim :: IOSim s [(NumberOfPeers, Set RelayAccessPoint)]
      sim = do
        dnsMapVar <- newTVarIO (scriptHead dnsMapScript)
        dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
        dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript
        dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore
        snapshotScript <- initScript' . Script . NonEmpty.fromList $ ledgerPeerSnapshots
        ledgerPeersScript <- initScript' . Script . NonEmpty.fromList $ getLedgerPools <$> ledgerPools
        slotScript <- initScript' . Script . NonEmpty.fromList $ At . fromIntegral . getPositive <$> ledgerSlots
        let interface = LedgerPeersConsensusInterface
                          (stepScriptSTM' slotScript)
                          (pure lsj)
                          (stepScriptSTM' ledgerPeersScript)

        withLedgerPeers
                PeerActionsDNS { paToPeerAddr = curry IP.toSockAddr,
                                 paDnsActions = mockDNSActions @SomeException dnsMapVar dnsTimeoutScriptVar dnsLookupDelayScriptVar,
                                 paDnsSemaphore = dnsSemaphore }
                WithLedgerPeersArgs { wlpRng = rng1,
                                      wlpConsensusInterface = interface,
                                      wlpTracer = verboseTracer,
                                      wlpGetUseLedgerPeers = pure $ UseLedgerPeers Always,
                                      wlpGetLedgerPeerSnapshot = stepScriptSTM' snapshotScript }
                (\request _ ->
                  forM (zip4 ledgerPools snapshotPools'
                             ledgerPeersKinds (iterate (fst . split) rng2)) $
                       \(LedgerPools lp, sp, ArbLedgerPeersKind lpk, rng') -> do
                    threadDelay 1900 -- we need to invalidate ledger peer's cache
                    let maxRequest = case sp of
                                       Just (LedgerPools sp') -> min (length lp) (length sp')
                                       Nothing -> length lp
                        numRequested = NumberOfPeers . fromIntegral . fst . randomR (1, maxRequest) $ rng'
                    resp <- request numRequested lpk
                    pure $ case resp of
                      Nothing          -> (numRequested, Set.empty)
                      Just (peers, _)  -> (numRequested, Set.fromList [ RelayAccessAddress ip port
                                                                      | Just (ip, port) <-     IP.fromSockAddr
                                                                                           <$> Set.toList peers]))

  return . ioProperty $ do
    tr' <- evaluateTrace (runSimTrace sim)
    case tr' of
      SimException e trace -> do
        return $ counterexample (intercalate "\n" $ show e : trace) False
      SimDeadLock trace -> do
        return $ counterexample (intercalate "\n" $ "Deadlock" : trace) False
      SimReturn results _trace -> do
        return $ either (`counterexample` False) id evalChecks
        where
          evalChecks = foldrM step (property True) (zip6 results snapshotSlots' ledgerSlots ledgerPeersKinds ledgerPools snapshotPools')

          step ((numRequested, peers), ss, ls, ArbLedgerPeersKind lpk, lpool, spool) pass =
            let fixupSnapshotSlot = maybe (-1) getPositive (ss <$ spool)
                snapshotSet spools = Set.fromList (concatMap (NonEmpty.toList . snd) $ getLedgerPools spools)
                ledgerSet = Set.fromList (concatMap (NonEmpty.toList . snd) $ getLedgerPools lpool)
                bigLedgerSet = Set.fromList (concatMap (NonEmpty.toList . snd . snd) . accumulateBigLedgerStake . getLedgerPools $ lpool)
                source =
                  case spool of
                    Just spools | fixupSnapshotSlot > getPositive ls  ->
                                  snapshotSet spools `Set.union` ledgerSet
                    _otherwise -> case lpk of
                                    AllLedgerPeers -> ledgerSet
                                    BigLedgerPeers -> bigLedgerSet
            in if peers `Set.isSubsetOf` source
               then return pass
               else Left $ intercalate "\n"
                             ["Counterexample:", "Requested: "++ show numRequested,
                              "Type requested: " ++ show lpk,
                              "Ledger slot: " ++ show ls, "Snapshot slot? " ++ show (ss <$ spool),
                              show peers, "========================", "violates Set.isSubsetOf:",
                              "========================", show source]


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
          BigLedgerPeers -> accBigPoolStakeMap sps

        sim :: IOSim s [RelayAccessPoint]
        sim = do
          let dnsMap = scriptHead dnsMapScript
          dnsMapVar <- newTVarIO dnsMap

          dnsTimeoutScriptVar <- initScript' dnsTimeoutScript
          dnsLookupDelayScriptVar <- initScript' dnsLookupDelayScript

          dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore

          withLedgerPeers
                PeerActionsDNS { paToPeerAddr = curry IP.toSockAddr,
                                 paDnsActions = (mockDNSActions @SomeException dnsMapVar dnsTimeoutScriptVar dnsLookupDelayScriptVar),
                                 paDnsSemaphore = dnsSemaphore }
                WithLedgerPeersArgs { wlpRng = rng,
                                      wlpConsensusInterface = interface,
                                      wlpTracer = verboseTracer,
                                      wlpGetUseLedgerPeers = pure $ UseLedgerPeers Always,
                                      wlpGetLedgerPeerSnapshot = pure Nothing }
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
                (pure $ At slot)
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
                PeerActionsDNS { paToPeerAddr = curry IP.toSockAddr,
                                 paDnsActions = mockDNSActions @SomeException dnsMapVar dnsTimeoutScriptVar dnsLookupDelayScriptVar,
                                 paDnsSemaphore = dnsSemaphore }
                WithLedgerPeersArgs { wlpRng = rng,
                                      wlpConsensusInterface = interface,
                                      wlpTracer = verboseTracer,
                                      wlpGetUseLedgerPeers = pure $ UseLedgerPeers (After 0),
                                      wlpGetLedgerPeerSnapshot = pure $ Nothing }
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
                          (pure $ At slot)
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


prop_accumulateBigLedgerStake :: LedgerPools -> Property
prop_accumulateBigLedgerStake  (LedgerPools [])        = property True
prop_accumulateBigLedgerStake  (LedgerPools lps@(_:_)) =
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
         -- `accBigPoolStakeMap` form an initial sub-list of the ordered ledger
         -- peers by stake (from large to small).
         --
         -- We relay on the fact that `Map.elems` returns a list of elements
         -- ordered by keys (as they are in the `Map`).
    .&&. let lps'  = sortOn (Down . fst) lps
             elems = Map.elems accumulatedStakeMap
         in counterexample ("initial sublist vaiolation: " ++ show (elems, lps'))
          $ elems `isPrefixOf` lps'
  where
    accumulatedStakeMap = accBigPoolStakeMap lps

prop_recomputeRelativeStake :: LedgerPools -> Property
prop_recomputeRelativeStake (LedgerPools []) = property True
prop_recomputeRelativeStake (LedgerPools lps) = property $ do
  lpk <- genLedgerPeersKind
  let (accStake, relayAccessPointsUnchangedNonNegativeStake) = go (reStake lpk) lps (0, True)
  return $     counterexample "recomputeRelativeStake: relays modified or negative pool stake calculated"
                             relayAccessPointsUnchangedNonNegativeStake
          .&&. accStake === 1
          .&&. counterexample "violates idempotency"
                              ((recomputeRelativeStake BigLedgerPeers . recomputeRelativeStake BigLedgerPeers $ lps) == recomputeRelativeStake BigLedgerPeers lps)
  where
    genLedgerPeersKind = elements [AllLedgerPeers, BigLedgerPeers]
    reStake lpk = recomputeRelativeStake lpk lps
    -- compare relay access points in both lists for equality
    -- where we assume that recomputerelativestake doesn't change
    -- the order, and sum up relative stake to make sure it adds up to 1
    go ((normPoolStake, raps):rest) ((_, raps'):rest') (accStake, _) =
      if raps == raps' && normPoolStake >= 0
      then go rest rest' (accStake + normPoolStake, True)
      else (accStake + normPoolStake, False)
    go [] (_:_) (accStake, _) = (accStake, False)
    go (_:_) [] (accStake, _) = (accStake, False)
    go _ _ (accStake, relayAccessPointsUnchangedNonNegativeStake) = (accStake, relayAccessPointsUnchangedNonNegativeStake)


prop_getLedgerPeers :: ArbitrarySlotNo
                    -> ArbitraryLedgerStateJudgement
                    -> LedgerPools
                    -> ArbitrarySlotNo
                    -> Property
prop_getLedgerPeers (ArbitrarySlotNo curSlot)
                    (ArbitraryLedgerStateJudgement lsj)
                    (LedgerPools lps)
                    (ArbitrarySlotNo slot) =
  let afterSlot = if slot == 0
                     then Always
                     else After slot
      sim :: IOSim m LedgerPeers
      sim = atomically $ getLedgerPeers interface afterSlot

      result :: LedgerPeers
      result = runSimOrThrow sim

   in counterexample (show result) $
      case result of
        LedgerPeers _ _ -> property (curSlot >= slot || afterSlot == Always)
        BeforeSlot      -> property (curSlot < slot)
  where
    curSlotWO = if curSlot == 0
                  then Origin
                  else At curSlot

    interface :: LedgerPeersConsensusInterface (IOSim s)
    interface = LedgerPeersConsensusInterface
                  (pure $ curSlotWO)
                  (pure lsj)
                  (pure (Map.elems (accPoolStake lps)))

-- | Tests if the CBOR encoding is valid, and whether a round
-- trip results in the original peer snapshot value.
--
prop_ledgerPeerSnapshotV1 :: ArbitrarySlotNo
                          -> LedgerPools
                          -> Property
prop_ledgerPeerSnapshotV1 (ArbitrarySlotNo slot)
                          (LedgerPools pools) =
  counterexample (show snapshot) $
    conjoin [counterexample "Invalid CBOR encoding" $ validFlatTerm encoded,
             either ((`counterexample` False) . ("JSON decode failed: " <>))
                    (("CBOR round trip failed" `counterexample`) . (snapshot ==))
                    decoded,
             either ((`counterexample` False) . ("JSON decode failed: " <>))
                    (("JSON round trip failed" `counterexample`) . (snapshot ==))
                    decodedJSON]
  where
    poolStakeWithAccumulation = Map.assocs . accPoolStake $ pools
    originOrSlot = if slot == 0
                   then Origin
                   else At slot
    snapshot = LedgerPeerSnapshotV1 (originOrSlot, poolStakeWithAccumulation)
    encoded = toFlatTerm . toCBOR $ snapshot
    decoded = fromFlatTerm fromCBOR encoded
    encodedJSON = toJSON snapshot
    decodedJSON = case fromJSON encodedJSON of
                    Aeson.Success s -> Right s
                    Error str       -> Left str

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
