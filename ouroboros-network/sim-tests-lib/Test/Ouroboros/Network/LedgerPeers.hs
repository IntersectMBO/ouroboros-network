{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Ouroboros.Network.LedgerPeers where

import Codec.CBOR.FlatTerm
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeException (..))
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
import Data.ByteString.Char8 qualified as BS
import Data.IP qualified as IP
import Data.List as List (foldl', intercalate, isPrefixOf, nub, sortOn)
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

import Test.Ouroboros.Network.Data.Script
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
  , testProperty "LedgerPeerSnapshot CBOR version 1" prop_ledgerPeerSnapshotCBORV1
  , testProperty "LedgerPeerSnapshot JSON version 1" prop_ledgerPeerSnapshotJSONV1
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
    let totalStake = List.foldl' (\s p -> s + spStake p) 0 sps in
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

newtype ArbStakeMapOverSource = ArbStakeMapOverSource { getArbStakeMapOverSource :: StakeMapOverSource }
  deriving Show

instance Arbitrary ArbStakeMapOverSource where
  arbitrary = do
    peerSnapshot <-
      oneof [ pure Nothing, Just <$> genPeerSnapshot ]
    ledgerWithOrigin <- genWithOrigin
    ula <- arbitrary
    ledgerPeers <-
      case (ula, ledgerWithOrigin) of
        (Always, _) -> LedgerPeers TooOld . getLedgerPools <$> arbitrary
        (After slotNo, Origin) | slotNo > 0 -> return BeforeSlot
        (After afterSlotNo, At atSlotNo)
          | afterSlotNo <= atSlotNo -> LedgerPeers TooOld . getLedgerPools <$> arbitrary
        _otherwise -> return BeforeSlot
    (peerMap, bigPeerMap, cachedSlot) <-
      return $ case peerSnapshot of
                 Nothing -> (Map.empty, Map.empty, Nothing)
                 Just (LedgerPeerSnapshotV1 (At slot, accPools))
                   -> (Map.fromList accPools, Map.fromList accPools, Just slot)
                 _otherwise -> error "impossible!"
    return $ ArbStakeMapOverSource StakeMapOverSource {
      ledgerWithOrigin,
      ledgerPeers,
      peerSnapshot,
      peerMap,
      bigPeerMap,
      ula,
      cachedSlot }
    where
      genWithOrigin = do
        ArbitrarySlotNo slotNo <- arbitrary
        return $ if slotNo == 0 then Origin else At slotNo
      genPeerSnapshot = do
        slotNo <- At . getPositive <$> arbitrary
        pools <- accumulateBigLedgerStake . getLedgerPools <$> arbitrary
        return $ LedgerPeerSnapshotV1 (slotNo, pools)

-- | This test checks whether requesting ledger peers works as intended
-- when snapshot data is available. For each request, peers must be returned from the right
-- source - either the ledger or snapshot, depending on whether which source is fresher.
--
prop_ledgerPeerSnapshot_requests :: ArbStakeMapOverSource
                                 -> Property
prop_ledgerPeerSnapshot_requests ArbStakeMapOverSource {
      getArbStakeMapOverSource = params@StakeMapOverSource {
          ledgerWithOrigin,
          ledgerPeers,
          peerSnapshot,
          ula } } =
  counterexample (unlines
                   ["Counterexample:", "Ledger slot " ++ show ledgerWithOrigin,
                    "Ledger pools: " ++ show ledgerPeers,
                    "Snapshot? :" ++ show peerSnapshot,
                    "UseLedgerAfter: " ++ show ula]) $
    let (poolMap, bigPoolMap, _slot) = stakeMapWithSlotOverSource params
        bigPoolRelays = fmap (snd . snd) . Map.toList $ bigPoolMap
        poolRelays    = fmap (snd . snd) . Map.toList $ poolMap
    in case (ledgerWithOrigin, ledgerPeers, peerSnapshot) of
        (At t, LedgerPeers _ ledgerPools, Just (LedgerPeerSnapshot (At t', snapshotAccStake)))
          | t' >= t ->
            snapshotRelays === bigPoolRelays .&&. bigPoolRelays === poolRelays
          | otherwise ->
                 bigPoolRelays === ledgerBigPoolRelays
            .&&.    poolRelays === ledgerRelays
          where
            snapshotRelays = fmap (snd . snd) snapshotAccStake
            ledgerBigPoolRelays   = fmap (snd . snd) (accumulateBigLedgerStake ledgerPools)
            ledgerRelays = fmap (snd . snd) . Map.toList $ accPoolStake ledgerPools

        (_, LedgerPeers _ ledgerPools, Nothing) ->
               bigPoolRelays === ledgerBigPoolRelays
          .&&.    poolRelays === ledgerRelays
          where
            ledgerBigPoolRelays = fmap (snd . snd) (accumulateBigLedgerStake ledgerPools)
            ledgerRelays = fmap (snd . snd) . Map.toList $ accPoolStake ledgerPools

        (_, _, Just (LedgerPeerSnapshot (At t', snapshotAccStake)))
          | After slot <- ula, t' >= slot ->
            snapshotRelays === bigPoolRelays .&&. bigPoolRelays === poolRelays
          where
            snapshotRelays = fmap (snd . snd) snapshotAccStake

        _otherwise -> bigPoolRelays === [] .&&. poolRelays === []


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
                                      wlpGetLedgerPeerSnapshot = pure Nothing }
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

-- |This functions checks the following properties:
-- 1. The accumulated relative stake adds up to unity
-- 2. No pool relative stake can be less than 0
-- 3. The relays aren't mangled
-- 4. Running this function multiple times always produces the same result
--
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

-- | Checks validity of LedgerPeerSnapshot CBOR encoding, and whether
--   round trip cycle is the identity function
--
prop_ledgerPeerSnapshotCBORV1 :: ArbitrarySlotNo
                              -> LedgerPools
                              -> Property
prop_ledgerPeerSnapshotCBORV1 slotNo
                              ledgerPools =
  counterexample (show snapshot) $
         counterexample ("Invalid CBOR encoding" <> show encoded)
                        (validFlatTerm encoded)
    .&&. either ((`counterexample` False) . ("CBOR decode failed: " <>))
                (counterexample . ("CBOR round trip failed: " <>) . show <*> (snapshot ==))
                decoded
  where
    snapshot = snapshotV1 slotNo ledgerPools
    encoded = toFlatTerm . toCBOR $ snapshot
    decoded = fromFlatTerm fromCBOR encoded

-- | Tests if LedgerPeerSnapshot JSON round trip is the identity function
--
prop_ledgerPeerSnapshotJSONV1 :: ArbitrarySlotNo
                              -> LedgerPools
                              -> Property
prop_ledgerPeerSnapshotJSONV1 slotNo
                              ledgerPools =
  counterexample (show snapshot) $
     either ((`counterexample` False) . ("JSON decode failed: " <>))
            (counterexample . ("JSON round trip failed: " <>) . show <*> nearlyEqualModuloFullyQualified snapshot)
            roundTrip
  where
    snapshot = snapshotV1 slotNo ledgerPools
    roundTrip = case fromJSON . toJSON $ snapshot of
                  Aeson.Success s -> Right s
                  Error str       -> Left str

    nearlyEqualModuloFullyQualified snapshotOriginal snapshotRoundTripped =
      let LedgerPeerSnapshotV1 (wOrigin, relaysWithAccStake) = snapshotOriginal
          strippedRelaysWithAccStake = stripFQN <$> relaysWithAccStake
          LedgerPeerSnapshotV1 (wOrigin', relaysWithAccStake') = snapshotRoundTripped
          strippedRelaysWithAccStake' = stripFQN <$> relaysWithAccStake'
      in
             wOrigin === wOrigin'
        .&&. counterexample "fully qualified name"
                            (strippedRelaysWithAccStake === strippedRelaysWithAccStake')
        .&&. counterexample "approximation error"
                            (compareApprox relaysWithAccStake relaysWithAccStake')

    stripFQN (_, (_, relays)) = step <$> relays
    step it@(RelayAccessDomain domain port) =
      case BS.unsnoc domain of
        Just (prefix, '.') -> RelayAccessDomain prefix port
        _otherwise         -> it
    step it = it

    compareApprox left right =
      let left' = [(accStake, relativeStake)
                  | (AccPoolStake accStake, (PoolStake relativeStake, _)) <- left]
          right' = [(accStake, relativeStake)
                   | (AccPoolStake accStake, (PoolStake relativeStake, _)) <- right]
          go (accStake, relativeStake)
             (accStake', relativeStake') =
               accStake' / accStake > 999999 % 1000000
            && accStake' / accStake < 1000001 % 1000000
            && relativeStake' / relativeStake > 999999 % 1000000
            && relativeStake' / relativeStake < 1000001 % 1000000

      in all (uncurry go) (zip left' right')

-- | helper functions for ledgerpeersnapshot encoding tests
--
snapshotV1 :: ArbitrarySlotNo
           -> LedgerPools
           -> LedgerPeerSnapshot
snapshotV1 (ArbitrarySlotNo slot)
           (LedgerPools pools) = LedgerPeerSnapshotV1 (originOrSlot, poolStakeWithAccumulation)
  where
    poolStakeWithAccumulation = Map.assocs . accPoolStake $ pools
    originOrSlot = if slot == 0
                   then Origin
                   else At slot

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

verboseTracer :: forall a m. (MonadSay m, Show a)
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
