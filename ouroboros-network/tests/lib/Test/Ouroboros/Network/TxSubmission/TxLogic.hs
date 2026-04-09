{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Test.Ouroboros.Network.TxSubmission.TxLogic
  ( tests
  , ArbTxDecisionPolicy (..)
  , PeerAddr
  , ArbSharedTxState (..)
  , ArbSharedPeerState (..)
  , ArbPeerTxLocalState (..)
  , sharedTxStateInvariant
  , InvariantStrength (..)
  ) where

import Control.Monad.Class.MonadTime.SI (Time (..), addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (nub, nubBy)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word64)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
           (updatePeerPhase)
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCaseSteps,
           (@?=))
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "TxLogic"
    [ testProperty "handleReceivedTxIds inserts new tx entries" prop_handleReceivedTxIds_newEntries
    , testProperty "handleReceivedTxIds resolves txids already in mempool" prop_handleReceivedTxIds_knownToMempool
    , testProperty "handleReceivedTxIds keeps retained txids local-only" prop_handleReceivedTxIds_retainedIsLocalOnly
    , testProperty "handleReceivedTxs buffers received and drops omitted txs" prop_handleReceivedTxs_buffersAndDropsOmitted
    , testProperty "handleReceivedTxs drops late bodies already retained or in mempool" prop_handleReceivedTxs_dropsLateBodies
    , testProperty "handleReceivedTxs penalizes omitted txs after full prune" prop_handleReceivedTxs_penalizesOmittedAfterPrune
    , testProperty "handleSubmittedTxs retains accepted and drops rejected" prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    , testProperty "nextPeerAction prioritises submitting buffered owned txs" prop_nextPeerAction_prioritisesSubmit
    , testProperty "nextPeerAction claims claimable tx for best idle advertiser" prop_nextPeerAction_claimsClaimableTx
    , testProperty "nextPeerAction steals expired lease for best idle advertiser" prop_nextPeerAction_claimsExpiredLease
    , testProperty "nextPeerAction requests an oversized first tx within the soft budget" prop_nextPeerAction_requestsOversizedFirstTx
    , testCaseSteps "nextPeerAction skips blocked available txs and requests later claimable ones" unit_nextPeerAction_skipsBlockedAvailableTxs
    , testProperty "nextPeerAction submits buffered owned txs before acking" prop_nextPeerAction_ownerSubmitsBuffered
    , testCaseSteps "nextPeerAction requests other txs despite a blocked buffered tx" unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx
    , testCaseSteps "nextPeerAction only acks the safe prefix before a blocked buffered tx" unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx
    , testProperty "nextPeerAction keeps non-owner txids unacked until resolved" prop_nextPeerAction_nonOwnerWaitsUntilResolved
    , testProperty "nextPeerActionPipelined suppresses ack-only txid requests" prop_nextPeerActionPipelined_requiresAckAndReq
    , testProperty "nextPeerActionPipelined requests txids when it can ack and request" prop_nextPeerActionPipelined_requestsTxIds
    , testProperty "nextPeerActionPipelined opens a second outstanding body batch" prop_nextPeerActionPipelined_secondBodyBatch
    , testProperty "nextPeerActionPipelined does not open a third outstanding body batch" prop_nextPeerActionPipelined_noThirdBodyBatch
    , testProperty "nextPeerAction prunes expired retained txs" prop_nextPeerAction_prunesExpiredRetained
    , testProperty "nextPeerAction keeps retained txs before expiry" prop_nextPeerAction_keepsRetained
    , testProperty "PeerDoNothing waits for the earliest shared expiry" prop_nextPeerAction_earliestWakeDelay
    , testProperty "PeerDoNothing uses the current peer generation" prop_nextPeerAction_returnsPeerGeneration
    , testProperty "handleSubmittedTxs bumps idle advertiser generations" prop_handleSubmittedTxs_bumpsIdleAdvertisers
    , testCaseSteps "updatePeerPhase only wakes the peer becoming idle" unit_updatePeerPhase_wakesOnlyBecomingIdlePeer
    , testCaseSteps "updatePeerPhase wakes competing idle advertisers when a peer leaves idle" unit_updatePeerPhase_wakesCompetingAdvertisers
    ]

--
-- InboundState properties
--

type PeerAddr = Int

data InvariantStrength = WeakInvariant
                       | StrongInvariant
  deriving (Eq, Show)

-- | 'InboundState` invariant.
--
sharedTxStateInvariant
  :: forall peeraddr txid.
     ( Ord peeraddr
     , Ord txid
     , Show peeraddr
     , Show txid
     )
  => InvariantStrength
  -> SharedTxState peeraddr txid
  -> Property
sharedTxStateInvariant strength SharedTxState {
                          sharedPeers,
                          sharedTxTable,
                          sharedRetainedTxs,
                          sharedTxIdToKey,
                          sharedKeyToTxId,
                          sharedNextTxKey
                        } =
  conjoin $
    [ counterexample "sharedTxIdToKey/sharedKeyToTxId size mismatch"
        (Map.size sharedTxIdToKey === IntMap.size sharedKeyToTxId)
    , counterexample "active and retained tx sets overlap"
        (IntSet.null (IntMap.keysSet sharedTxTable `IntSet.intersection` retainedKeysSet sharedRetainedTxs))
    , counterexample "tx-key maps disagree"
        (property (keysRoundTripForward && keysRoundTripBackward))
    , counterexample "tx-key maps do not track exactly the live tx keys"
        (IntMap.keysSet sharedKeyToTxId === liveKeys)
    , counterexample "sharedNextTxKey does not stay ahead of all live tx keys"
        (property (all (< sharedNextTxKey) (IntSet.toList liveKeys)))
    ]
    ++ case strength of
            WeakInvariant ->
              fmap checkTxEntry activeEntries
            StrongInvariant ->
              fmap checkTxEntry activeEntries
              ++ [ counterexample "active tx entry without any liveness source"
                     (property (all activeEntryLive (IntMap.elems sharedTxTable)))
                 ]
  where
    liveKeys = IntMap.keysSet sharedTxTable `IntSet.union` retainedKeysSet sharedRetainedTxs
    activeEntries = IntMap.toList sharedTxTable
    knownPeers = Map.keysSet sharedPeers

    keysRoundTripForward =
      all (\(txid, txKey) -> IntMap.lookup (unTxKey txKey) sharedKeyToTxId == Just txid)
          (Map.toList sharedTxIdToKey)

    keysRoundTripBackward =
      all (\(k, txid) -> Map.lookup txid sharedTxIdToKey == Just (TxKey k))
          (IntMap.toList sharedKeyToTxId)

    activeEntryLive TxEntry { txLease, txAdvertisers, txAttempts } =
         leaseLive txLease
      || not (Map.null txAdvertisers)
      || not (Map.null txAttempts)

    leaseLive TxClaimable = False
    leaseLive TxLeased {} = True

    checkTxEntry (k, txEntry@TxEntry { txLease, txAdvertisers, txAttempts }) =
      counterexample ("bad active tx entry " ++ show k ++ ": " ++ show txEntry) $
        conjoin
          [ property (Map.keysSet txAdvertisers `Set.isSubsetOf` knownPeers)
          , property (Map.keysSet txAttempts `Set.isSubsetOf` Map.keysSet txAdvertisers)
          , case txLease of
                 TxClaimable ->
                   property True
                 TxLeased owner _ ->
                   property (Map.member owner sharedPeers && Map.member owner txAdvertisers)
          ]

newtype ArbTxDecisionPolicy = ArbTxDecisionPolicy TxDecisionPolicy
  deriving Show

newtype ArbSharedTxState = ArbSharedTxState (SharedTxState PeerAddr TxId)
  deriving Show

newtype ArbSharedPeerState = ArbSharedPeerState SharedPeerState
  deriving Show

newtype ArbPeerTxLocalState = ArbPeerTxLocalState (PeerTxLocalState (Tx TxId))
  deriving Show

instance Arbitrary ArbTxDecisionPolicy where
    arbitrary =
      ArbTxDecisionPolicy <$> (
       TxDecisionPolicy . getSmall . getPositive
         <$> arbitrary
         <*> (getSmall . getPositive <$> arbitrary)
         <*> (SizeInBytes . getPositive <$> arbitrary)
         <*> choose (1, 10)
         <*> (getSmall . getPositive <$> arbitrary)
         <*> (realToFrac <$> choose (0 :: Double, 2))
         <*> choose (0, 1)
         <*> choose (0, 1800)
         <*> (realToFrac <$> choose (0 :: Double, 1)))

    shrink (ArbTxDecisionPolicy a@TxDecisionPolicy {
              maxNumTxIdsToRequest,
              txsSizeInflightPerPeer,
              txInflightMultiplicity }) =
      [ ArbTxDecisionPolicy a { maxNumTxIdsToRequest = NumTxIdsToReq x }
      | (Positive (Small x)) <- shrink (Positive (Small (getNumTxIdsToReq maxNumTxIdsToRequest)))
      ]
      ++
      [ ArbTxDecisionPolicy a { txsSizeInflightPerPeer = SizeInBytes s }
      | Positive s <- shrink (Positive (getSizeInBytes txsSizeInflightPerPeer))
      ]
      ++
      [ ArbTxDecisionPolicy a { txInflightMultiplicity = x }
      | Positive (Small x) <- shrink (Positive (Small txInflightMultiplicity))
      ]

instance Arbitrary ArbSharedPeerState where
  arbitrary = ArbSharedPeerState <$> genSharedPeerState

  shrink (ArbSharedPeerState peerState)
    | peerState == defaultPeerState = []
    | otherwise =
        [ ArbSharedPeerState defaultPeerState
        , ArbSharedPeerState peerState
            { sharedPeerGeneration = 0 }
        ]
    where
      defaultPeerState = mkSharedPeerState PeerIdle (emptyPeerScore now)

instance Arbitrary ArbPeerTxLocalState where
  arbitrary = ArbPeerTxLocalState <$> genPeerTxLocalState

  shrink (ArbPeerTxLocalState peerState)
    | peerState == emptyPeerTxLocalState = []
    | otherwise =
        [ ArbPeerTxLocalState emptyPeerTxLocalState
        , ArbPeerTxLocalState peerState
            { peerRequestedTxs = IntSet.empty
            , peerRequestedTxBatches = StrictSeq.empty
            , peerRequestedTxsSize = 0
            , peerRequestedTxIds = 0
            , peerDownloadedTxs = IntMap.empty
            }
        ]

instance Arbitrary ArbSharedTxState where
  arbitrary = ArbSharedTxState <$> genSharedTxState

  shrink (ArbSharedTxState sharedState)
    | sharedState == emptySharedTxState = []
    | otherwise = ArbSharedTxState <$> shrinkSharedTxState sharedState

-- Verifies that handleReceivedTxIds interns new txids, adds leased entries
-- for them, and preserves unrelated shared-state entries.
prop_handleReceivedTxIds_newEntries
  :: Positive Int
  -> ArbSharedTxState
  -> NonEmptyList (TxId, Positive Int)
  -> Positive Int
  -> Property
prop_handleReceivedTxIds_newEntries (Positive peeraddr) (ArbSharedTxState sharedState0) (NonEmpty txids0) (Positive extraRequested) =
  conjoin
    [ peerRequestedTxIds peerState' === fromIntegral extraRequested
    , StrictSeq.length (peerUnacknowledgedTxIds peerState') === length txidsAndSizes
    , toList (peerUnacknowledgedTxIds peerState') === fmap (\(txid, _) -> lookupKeyOrFail txid sharedState') txidsAndSizes
    , IntMap.size (peerAvailableTxIds peerState') === length txidsAndSizes
    , sharedPeers sharedState' === sharedPeers sharedState0
    , IntMap.restrictKeys (sharedTxTable sharedState') oldKeys === sharedTxTable sharedState0
    , retainedRestrictKeys (sharedRetainedTxs sharedState') oldKeys === sharedRetainedTxs sharedState0
    , IntMap.restrictKeys (sharedKeyToTxId sharedState') oldKeys === sharedKeyToTxId sharedState0
    , sharedGeneration sharedState' === sharedGeneration sharedState0 + 1
    , sharedNextTxKey sharedState' === sharedNextTxKey sharedState0 + length txidsAndSizes
    , conjoin (fmap checkExistingTxId (Map.toList (sharedTxIdToKey sharedState0)))
    , conjoin (fmap checkEntry txidsAndSizes)
    ]
  where
    txidsAndSizes =
      freshBatchAgainstSharedState sharedState0 $
        dedupeBatch [ (abs txid + 1, mkSize txSize) | (txid, txSize) <- txids0 ]
    oldKeys = IntMap.keysSet (sharedKeyToTxId sharedState0)
    requestedToReply = fromIntegral (length txidsAndSizes)
    peerState0 = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply + fromIntegral extraRequested }
    (peerState', sharedState') =
      handleReceivedTxIds (const False) now defaultTxDecisionPolicy peeraddr requestedToReply txidsAndSizes peerState0 sharedState0
    expectedLeaseUntil = addTime (interTxSpace defaultTxDecisionPolicy) now

    checkExistingTxId (txid, txKey) =
      Map.lookup txid (sharedTxIdToKey sharedState') === Just txKey

    checkEntry (txid, _) =
      case IntMap.lookup (unTxKey (lookupKeyOrFail txid sharedState')) (sharedTxTable sharedState') of
           Nothing -> counterexample ("missing tx entry for " ++ show txid) False
           Just TxEntry { txLease, txAdvertisers, txAttempts } ->
             conjoin
               [ txLease === TxLeased peeraddr expectedLeaseUntil
               , txAdvertisers === Map.singleton peeraddr (mkAdvertiser AckWhenBuffered)
               , txAttempts === Map.empty
               ]

-- Verifies that handleReceivedTxIds retains txids already known to the
-- mempool instead of leaving active tx entries behind.
prop_handleReceivedTxIds_knownToMempool
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleReceivedTxIds_knownToMempool (Positive peeraddr) txid0 txSize0 =
  conjoin
    [ peerAvailableTxIds peerState' === IntMap.empty
    , toList (peerUnacknowledgedTxIds peerState') === [key]
    , IntMap.lookup (unTxKey key) (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
    , retainedLookup (unTxKey key) (sharedRetainedTxs sharedState') === Just expectedRetainUntil
    , Map.lookup txid (sharedTxIdToKey sharedState') === Just key
    , IntMap.lookup (unTxKey key) (sharedKeyToTxId sharedState') === Just txid
    , sharedGeneration sharedState' === 1
    ]
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    requestedToReply = 1
    peerState0 = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply }
    (peerState', sharedState') =
      handleReceivedTxIds (== txid) now defaultTxDecisionPolicy peeraddr requestedToReply [(txid, txSize)] peerState0 emptySharedTxState
    key = lookupKeyOrFail txid sharedState'
    expectedRetainUntil = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now

-- Verifies that txids already retained in shared state only update the peer's
-- local queue and do not dirty the shared state again.
prop_handleReceivedTxIds_retainedIsLocalOnly
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleReceivedTxIds_retainedIsLocalOnly (Positive peeraddr) txid0 txSize0 =
  conjoin
    [ peerRequestedTxIds peerState' === 0
    , peerAvailableTxIds peerState' === IntMap.empty
    , toList (peerUnacknowledgedTxIds peerState') === [key]
    , sharedState' === sharedState0
    ]
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    key = TxKey 0
    k = unTxKey key
    retainUntil = addTime 17 now
    peerState0 = emptyPeerTxLocalState { peerRequestedTxIds = 1 }
    sharedState0 = emptySharedTxState
      { sharedRetainedTxs = retainedSingleton k retainUntil
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedGeneration = 7
      }
    (peerState', sharedState') =
      handleReceivedTxIds (const False) now defaultTxDecisionPolicy peeraddr 1 [(txid, txSize)] peerState0 sharedState0

-- Verifies that handleReceivedTxs buffers received bodies and removes omitted
-- requested txs from peer and shared state.
prop_handleReceivedTxs_buffersAndDropsOmitted
  :: Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_handleReceivedTxs_buffersAndDropsOmitted (Positive peeraddr) txidA0 txidB0 txSizeA0 txSizeB0 =
  txidA /= txidB ==>
    conjoin
      [ omittedCount === 1
      , lateCount === 0
      , peerRequestedTxs peerState' === IntSet.empty
      , peerRequestedTxsSize peerState' === 0
      , peerDownloadedTxs peerState' === IntMap.singleton kA txA
      , peerAvailableTxIds peerState' === IntMap.empty
      , fmap (Map.lookup peeraddr . txAttempts) (IntMap.lookup kA (sharedTxTable sharedState')) === Just (Just TxBuffered)
      , IntMap.lookup kB (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
      , retainedLookup kB (sharedRetainedTxs sharedState') === Nothing
      , Map.lookup txidB (sharedTxIdToKey sharedState') === Nothing
      , IntMap.lookup kB (sharedKeyToTxId sharedState') === Nothing
      , sharedGeneration sharedState' === 1
      ]
  where
    txidA = abs txidA0 + 1
    txidB = abs txidB0 + 2
    txSizeA = mkSize txSizeA0
    txSizeB = mkSize txSizeB0
    txA = mkTx txidA txSizeA
    sharedState0 =
      let st = mkSharedState [txidA, txidB]
          keyA' = lookupKeyOrFail txidA st
          keyB' = lookupKeyOrFail txidB st in
      st {
           sharedTxTable = IntMap.fromList
             [ (unTxKey keyA', mkTxEntry peeraddr txSizeA (Just TxDownloading))
             , (unTxKey keyB', mkTxEntry peeraddr txSizeB (Just TxDownloading))
             ]
         }
    keyA = lookupKeyOrFail txidA sharedState0
    keyB = lookupKeyOrFail txidB sharedState0
    kA = unTxKey keyA
    kB = unTxKey keyB
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.fromList [(kA, txSizeA), (kB, txSizeB)]
      , peerRequestedTxs = IntSet.fromList [kA, kB]
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [keyA, keyB] (txSizeA + txSizeB))
      , peerRequestedTxsSize = txSizeA + txSizeB
      }
    (omittedCount, lateCount, peerState', sharedState') = handleReceivedTxs (const False) now defaultTxDecisionPolicy peeraddr [(txidA, txA)] peerState0 sharedState0

-- Verifies that handleReceivedTxs drops late bodies when the tx is already
-- retained or already present in the mempool.
prop_handleReceivedTxs_dropsLateBodies
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleReceivedTxs_dropsLateBodies (Positive peeraddr) txid0 txSize0 =
  conjoin
    [ omittedRetained === 0
    , lateRetained === 1
    , peerStateRetained' === peerState0
        { peerAvailableTxIds = IntMap.empty
        , peerRequestedTxs = IntSet.empty
        , peerRequestedTxBatches = StrictSeq.empty
        , peerRequestedTxsSize = 0
        }
    , sharedTxTable sharedStateRetained' === IntMap.empty
    , retainedLookup k (sharedRetainedTxs sharedStateRetained') === Just retainedUntil
    , omittedMempool === 0
    , lateMempool === 1
    , peerStateMempool' === peerState0
        { peerAvailableTxIds = IntMap.empty
        , peerRequestedTxs = IntSet.empty
        , peerRequestedTxBatches = StrictSeq.empty
        , peerRequestedTxsSize = 0
        }
    , sharedTxTable sharedStateMempool' === IntMap.empty
    , retainedLookup k (sharedRetainedTxs sharedStateMempool') === Just retainedUntil
    ]
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    sharedStateBase =
      let st = mkSharedState [txid]
          key' = lookupKeyOrFail txid st in
      st {
           sharedTxTable = IntMap.singleton (unTxKey key') (mkTxEntry peeraddr txSize Nothing)
         }
    key = lookupKeyOrFail txid sharedStateBase
    k = unTxKey key
    retainedUntil = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now
    peerState0 :: PeerTxLocalState (Tx TxId)
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.singleton k txSize
      , peerRequestedTxs = IntSet.singleton k
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [key] txSize)
      , peerRequestedTxsSize = txSize
      }
    sharedStateRetained0 = sharedStateBase {
        sharedTxTable = IntMap.empty,
        sharedRetainedTxs = retainedSingleton k retainedUntil
      }
    (omittedRetained, lateRetained, peerStateRetained', sharedStateRetained') =
      handleReceivedTxs (const False) now defaultTxDecisionPolicy peeraddr [(txid, tx)] peerState0 sharedStateRetained0
    (omittedMempool, lateMempool, peerStateMempool', sharedStateMempool') =
      handleReceivedTxs (== txid) now defaultTxDecisionPolicy peeraddr [(txid, tx)] peerState0 sharedStateBase

-- Verifies that omitting a requested body still counts as a penalty even if
-- the tx has already been fully pruned from shared state by the time the
-- reply is processed.
prop_handleReceivedTxs_penalizesOmittedAfterPrune
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleReceivedTxs_penalizesOmittedAfterPrune (Positive peeraddr) txid0 txSize0 =
  conjoin
    [ omittedCount === 1
    , lateCount === 0
    , peerAvailableTxIds peerState' === IntMap.empty
    , peerRequestedTxs peerState' === IntSet.empty
    , peerRequestedTxBatches peerState' === StrictSeq.empty
    , peerRequestedTxsSize peerState' === 0
    , peerDownloadedTxs peerState' === (IntMap.empty :: IntMap.IntMap (Tx TxId))
    , sharedTxTable sharedState' === IntMap.empty
    , retainedLookup k (sharedRetainedTxs sharedState') === Nothing
    , Map.lookup txid (sharedTxIdToKey sharedState') === Nothing
    , IntMap.lookup k (sharedKeyToTxId sharedState') === Nothing
    , sharedGeneration sharedState' === 1
    ]
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    sharedStateBase =
      let st = mkSharedState [txid]
          key' = lookupKeyOrFail txid st in
      st {
           sharedTxTable = IntMap.singleton (unTxKey key') (mkTxEntry peeraddr txSize (Just TxDownloading))
         }
    key = lookupKeyOrFail txid sharedStateBase
    k = unTxKey key
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.singleton k txSize
      , peerRequestedTxs = IntSet.singleton k
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [key] txSize)
      , peerRequestedTxsSize = txSize
      }
    sharedStatePruned = sharedStateBase
      { sharedTxTable = IntMap.empty
      , sharedRetainedTxs = retainedEmpty
      , sharedTxIdToKey = Map.empty
      , sharedKeyToTxId = IntMap.empty
      }
    (omittedCount, lateCount, peerState', sharedState') =
      handleReceivedTxs (const False) now defaultTxDecisionPolicy peeraddr [] peerState0 sharedStatePruned

-- Verifies that handleSubmittedTxs retains accepted txs and removes rejected
-- txs from the active table and tx-key maps.
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
  :: Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected (Positive peeraddr) txidA0 txidB0 txSizeA0 txSizeB0 =
  txidA /= txidB ==>
    conjoin
      [ peerDownloadedTxs peerState' === IntMap.empty
      , IntMap.lookup kA (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
      , retainedLookup kA (sharedRetainedTxs sharedState') === Just expectedRetainUntil
      , Map.lookup txidA (sharedTxIdToKey sharedState') === Just keyA
      , IntMap.lookup kA (sharedKeyToTxId sharedState') === Just txidA
      , IntMap.lookup kB (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
      , retainedLookup kB (sharedRetainedTxs sharedState') === Nothing
      , Map.lookup txidB (sharedTxIdToKey sharedState') === Nothing
      , IntMap.lookup kB (sharedKeyToTxId sharedState') === Nothing
      , sharedGeneration sharedState' === 1
      ]
  where
    txidA = abs txidA0 + 1
    txidB = abs txidB0 + 2
    txSizeA = mkSize txSizeA0
    txSizeB = mkSize txSizeB0
    txA = mkTx txidA txSizeA
    txB = mkTx txidB txSizeB
    sharedState0 =
      let st = mkSharedState [txidA, txidB]
          keyA' = lookupKeyOrFail txidA st
          keyB' = lookupKeyOrFail txidB st in
      st {
           sharedTxTable = IntMap.fromList
             [ (unTxKey keyA', mkTxEntry peeraddr txSizeA (Just TxBuffered))
             , (unTxKey keyB', mkTxEntry peeraddr txSizeB (Just TxBuffered))
             ]
         }
    keyA = lookupKeyOrFail txidA sharedState0
    keyB = lookupKeyOrFail txidB sharedState0
    kA = unTxKey keyA
    kB = unTxKey keyB
    peerState0 = emptyPeerTxLocalState { peerDownloadedTxs = IntMap.fromList [(kA, txA), (kB, txB)] }
    expectedRetainUntil = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now
    (peerState', sharedState') = handleSubmittedTxs now defaultTxDecisionPolicy peeraddr [keyA] [keyB] peerState0 sharedState0

-- Verifies that nextPeerAction submits buffered txs owned by the peer before
-- taking any other action.
prop_nextPeerAction_prioritisesSubmit
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_prioritisesSubmit (Positive peeraddr) txid0 txSize0 =
  case peerAction of
       PeerSubmitTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerState' === peerState0
           , sharedState' === sharedState0
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased peeraddr (addTime 10 now)
          , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenBuffered)
          , txTieBreakSalt = txid
          , txAttempts = Map.singleton peeraddr TxBuffered
          }
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy
      , peerDownloadedTxs = IntMap.singleton k tx
      }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerAction leases a claimable tx to the best idle
-- advertiser and requests its body.
prop_nextPeerAction_claimsClaimableTx
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_claimsClaimableTx (Positive peerA0) (Positive peerB0) (Positive peerC0) txid0 txSize0 =
  distinctPeers ==>
    case peerAction of
         PeerRequestTxs txKeys ->
           conjoin
             [ txKeys === [key]
             , peerRequestedTxs peerState' === IntSet.singleton k
             , txLease (lookupEntryOrFail key sharedState') ===
                 TxLeased peerA (addTime (interTxSpace defaultTxDecisionPolicy) now)
             ]
         _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    peerA = peerA0
    peerB = peerB0 + 1000
    peerC = peerC0 + 2000
    distinctPeers = peerA /= peerB && peerA /= peerC && peerB /= peerC
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peerA, mkSharedPeerState PeerIdle (PeerScore 1 now))
          , (peerB, mkSharedPeerState PeerIdle (PeerScore 10 now))
          , (peerC, mkSharedPeerState PeerWaitingTxs (PeerScore 0 now))
          ]
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable
          , txAdvertisers = mkAdvertisers
              [ (peerA, AckWhenResolved)
              , (peerB, AckWhenResolved)
              , (peerC, AckWhenResolved)
              ]
          , txTieBreakSalt = txid
          , txAttempts = Map.empty
          }
      }
    peerState0 = emptyPeerTxLocalState { peerAvailableTxIds = IntMap.singleton k txSize }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peerA peerState0 sharedState0

-- Verifies that nextPeerAction can steal an expired lease for the best idle
-- advertiser and request that tx.
prop_nextPeerAction_claimsExpiredLease
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_claimsExpiredLease (Positive oldOwner0) (Positive peerA0) (Positive peerB0) txid0 txSize0 =
  distinctPeers ==>
    case peerAction of
         PeerRequestTxs txKeys ->
           conjoin
             [ txKeys === [key]
             , peerRequestedTxs peerState' === IntSet.singleton k
             , txLease (lookupEntryOrFail key sharedState') ===
                 TxLeased peerA (addTime (interTxSpace defaultTxDecisionPolicy) now)
             ]
         _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    oldOwner = oldOwner0
    peerA = peerA0 + 1000
    peerB = peerB0 + 2000
    distinctPeers = oldOwner /= peerA && oldOwner /= peerB && peerA /= peerB
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (oldOwner, mkSharedPeerState PeerWaitingTxs (PeerScore 0 now))
          , (peerA, mkSharedPeerState PeerIdle (PeerScore 1 now))
          , (peerB, mkSharedPeerState PeerIdle (PeerScore 10 now))
          ]
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased oldOwner (Time 0)
          , txAdvertisers = mkAdvertisers
              [ (oldOwner, AckWhenResolved)
              , (peerA, AckWhenResolved)
              , (peerB, AckWhenResolved)
              ]
          , txTieBreakSalt = txid
          , txAttempts = Map.empty
          }
      }
    peerState0 = emptyPeerTxLocalState { peerAvailableTxIds = IntMap.singleton k txSize }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peerA peerState0 sharedState0

-- Verifies that nextPeerAction still requests an oversized first tx when it
-- is the only available choice within the soft-budget policy.
prop_nextPeerAction_requestsOversizedFirstTx
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_requestsOversizedFirstTx (Positive peeraddr) txid0 (Positive txSize0) =
  case peerAction of
       PeerRequestTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerRequestedTxs peerState' === IntSet.singleton k
           , peerRequestedTxsSize peerState' === txSize
           , txLease (lookupEntryOrFail key sharedState') ===
               TxLeased peeraddr (addTime (interTxSpace policy) now)
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize (Positive (txSize0 + 1))
    key = TxKey 0
    k = unTxKey key
    policy = defaultTxDecisionPolicy
      { txsSizeInflightPerPeer = txSize - 1
      , maxOutstandingTxBatchesPerPeer = 1
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k (mkTxEntry peeraddr txSize Nothing)
      }
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.singleton k txSize
      , peerRequestedTxIds = maxNumTxIdsToRequest policy
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peeraddr peerState0 sharedState0

-- Verifies that nextPeerAction skips available txs blocked by another
-- peer's lease and requests a later claimable tx instead.
unit_nextPeerAction_skipsBlockedAvailableTxs :: (String -> IO ()) -> Assertion
unit_nextPeerAction_skipsBlockedAvailableTxs step = do
  step "Run nextPeerAction with one blocked tx and one later claimable tx"
  case peerAction of
       PeerRequestTxs [TxKey requested] -> do
         step "Assert the later claimable tx is requested and leased"
         requested @?= kClaimable
         peerRequestedTxs peerState' @?= IntSet.singleton kClaimable
         fmap txLease (IntMap.lookup kClaimable (sharedTxTable sharedState')) @?=
           Just (TxLeased peeraddr (addTime (interTxSpace policy) testNow))
       other ->
         assertFailure ("unexpected action: " ++ show other)
  where
    testNow = Time 100
    policy = defaultTxDecisionPolicy
    peeraddr = 7 :: PeerAddr
    otherPeer = 8 :: PeerAddr
    kBlocked = 1
    kClaimable = 2
    peerState = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.fromList [(kBlocked, 10), (kClaimable, 11)]
      }
    sharedState :: SharedTxState PeerAddr TxId
    sharedState = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, SharedPeerState PeerIdle (emptyPeerScore testNow) 0)
          , (otherPeer, SharedPeerState PeerIdle (emptyPeerScore testNow) 0)
          ]
      , sharedTxTable = IntMap.fromList
          [ (kBlocked, TxEntry
              { txLease = TxLeased otherPeer (addTime 10 testNow)
              , txAdvertisers = mkAdvertisers [(peeraddr, AckWhenResolved), (otherPeer, AckWhenResolved)]
              , txTieBreakSalt = 0
              , txAttempts = Map.empty
              })
          , (kClaimable, TxEntry
              { txLease = TxClaimable
              , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenResolved)
              , txTieBreakSalt = 0
              , txAttempts = Map.empty
              })
          ]
      }
    (peerAction, peerState', sharedState') =
      nextPeerAction testNow policy peeraddr peerState sharedState

-- Verifies that nextPeerAction submits buffered owned txs before
-- acknowledging their txids.
prop_nextPeerAction_ownerSubmitsBuffered
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_ownerSubmitsBuffered (Positive peeraddr) txid0 txSize0 =
  case peerAction of
       PeerSubmitTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerState' === peerState0
           , sharedState' === sharedState0
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable
          , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenBuffered)
          , txTieBreakSalt = txid
          , txAttempts = Map.singleton peeraddr TxBuffered
          }
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy
      , peerDownloadedTxs = IntMap.singleton k tx
      }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that a blocked buffered tx does not prevent the peer from
-- requesting a different claimable tx body.
unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx :: (String -> IO ()) -> Assertion
unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx step = do
  step "Run nextPeerAction with one blocked buffered tx and one claimable tx"
  case peerAction of
       PeerRequestTxs [TxKey requested] -> do
         step "Assert the blocked tx stays buffered while the claimable tx is requested"
         requested @?= kClaimable
         peerUnacknowledgedTxIds peerState' @?= peerUnacknowledgedTxIds peerState0
         peerRequestedTxs peerState' @?= IntSet.singleton kClaimable
         peerDownloadedTxs peerState' @?= peerDownloadedTxs peerState0
         txAttempts (lookupEntryOrFail blockedKey sharedState') @?= txAttempts blockedEntry
         txLease (lookupEntryOrFail claimableKey sharedState') @?=
           TxLeased peeraddr (addTime (interTxSpace defaultTxDecisionPolicy) now)
       other ->
         assertFailure ("unexpected action: " ++ show other)
  where
    peeraddr = 7
    submittingPeer = 8
    blockedTxid = 1
    claimableTxid = 2
    blockedSize = mkSize (Positive 10)
    claimableSize = mkSize (Positive 11)
    blockedKey = TxKey 1
    claimableKey = TxKey 2
    kBlocked = unTxKey blockedKey
    kClaimable = unTxKey claimableKey
    blockedTx = mkTx blockedTxid blockedSize
    blockedEntry = TxEntry
      { txLease = TxLeased peeraddr (addTime 10 now)
      , txAdvertisers = mkAdvertisers
          [ (peeraddr, AckWhenBuffered)
          , (submittingPeer, AckWhenResolved)
          ]
      , txTieBreakSalt = blockedTxid
      , txAttempts = Map.fromList
          [ (peeraddr, TxBuffered)
          , (submittingPeer, TxSubmitting)
          ]
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, mkSharedPeerState PeerIdle (emptyPeerScore now))
          , (submittingPeer, mkSharedPeerState PeerSubmittingToMempool (emptyPeerScore now))
          ]
      , sharedTxTable = IntMap.fromList
          [ (kBlocked, blockedEntry)
          , (kClaimable, TxEntry
              { txLease = TxClaimable
              , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenResolved)
              , txTieBreakSalt = claimableTxid
              , txAttempts = Map.empty
              })
          ]
      , sharedTxIdToKey = Map.fromList
          [ (blockedTxid, blockedKey)
          , (claimableTxid, claimableKey)
          ]
      , sharedKeyToTxId = IntMap.fromList
          [ (kBlocked, blockedTxid)
          , (kClaimable, claimableTxid)
          ]
      , sharedNextTxKey = 3
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [blockedKey, claimableKey]
      , peerAvailableTxIds = IntMap.fromList
          [ (kBlocked, blockedSize)
          , (kClaimable, claimableSize)
          ]
      , peerDownloadedTxs = IntMap.singleton kBlocked blockedTx
      }
    (peerAction, peerState', sharedState') =
      nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that txid acknowledgements stop before a blocked buffered tx, so
-- earlier safe txids can still be acked and replaced with new txid requests.
unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx :: (String -> IO ()) -> Assertion
unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx step = do
  step "Run nextPeerAction with an ackable retained tx followed by a blocked buffered tx"
  case peerAction of
       PeerRequestTxIds txIdsToAcknowledge txIdsToReq -> do
         step "Assert only the safe prefix is acknowledged"
         txIdsToAcknowledge @?= 1
         assertBool ("expected positive txIdsToReq, got " ++ show txIdsToReq) (txIdsToReq > 0)
         peerUnacknowledgedTxIds peerState' @?= StrictSeq.singleton blockedKey
         peerRequestedTxIds peerState' @?= txIdsToReq
         txAdvertisers (lookupEntryOrFail blockedKey sharedState') @?= txAdvertisers blockedEntry
       other ->
         assertFailure ("unexpected action: " ++ show other)
  where
    peeraddr = 7
    submittingPeer = 8
    resolvedTxid = 1
    blockedTxid = 2
    blockedSize = mkSize (Positive 10)
    resolvedKey = TxKey 1
    blockedKey = TxKey 2
    kResolved = unTxKey resolvedKey
    kBlocked = unTxKey blockedKey
    blockedTx = mkTx blockedTxid blockedSize
    blockedEntry = TxEntry
      { txLease = TxLeased peeraddr (addTime 10 now)
      , txAdvertisers = mkAdvertisers
          [ (peeraddr, AckWhenBuffered)
          , (submittingPeer, AckWhenResolved)
          ]
      , txTieBreakSalt = blockedTxid
      , txAttempts = Map.fromList
          [ (peeraddr, TxBuffered)
          , (submittingPeer, TxSubmitting)
          ]
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, mkSharedPeerState PeerIdle (emptyPeerScore now))
          , (submittingPeer, mkSharedPeerState PeerSubmittingToMempool (emptyPeerScore now))
          ]
      , sharedTxTable = IntMap.singleton kBlocked blockedEntry
      , sharedRetainedTxs = retainedSingleton kResolved (addTime 17 now)
      , sharedTxIdToKey = Map.fromList
          [ (resolvedTxid, resolvedKey)
          , (blockedTxid, blockedKey)
          ]
      , sharedKeyToTxId = IntMap.fromList
          [ (kResolved, resolvedTxid)
          , (kBlocked, blockedTxid)
          ]
      , sharedNextTxKey = 3
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [resolvedKey, blockedKey]
      , peerDownloadedTxs = IntMap.singleton kBlocked blockedTx
      }
    (peerAction, peerState', sharedState') =
      nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerAction keeps non-owner txids unacknowledged until
-- the tx has resolved out of the active table.
prop_nextPeerAction_nonOwnerWaitsUntilResolved
  :: Positive Int
  -> Positive Int
  -> TxId
  -> Property
prop_nextPeerAction_nonOwnerWaitsUntilResolved (Positive owner0) (Positive peeraddr0) txid0 =
  owner /= peeraddr ==>
    conjoin
      [ case unresolvedAction of
             PeerDoNothing _ _ ->
               unresolvedExpectations
             PeerRequestTxIds txIdsToAcknowledge _ ->
               conjoin
                 [ txIdsToAcknowledge === 0
                 , unresolvedExpectations
                 ]
             _ -> counterexample ("unexpected unresolved action: " ++ show unresolvedAction) False
      , case resolvedAction of
             PeerRequestTxIds txIdsToAcknowledge _ ->
               conjoin
                 [ txIdsToAcknowledge === 1
                 , peerUnacknowledgedTxIds resolvedPeerState' === StrictSeq.empty
                 ]
             _ -> counterexample ("unexpected resolved action: " ++ show resolvedAction) False
      ]
  where
    owner = owner0 + 1000
    peeraddr = peeraddr0 + 2000
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    sharedPeers0 = Map.fromList
      [ (owner, mkSharedPeerState PeerIdle (emptyPeerScore now))
      , (peeraddr, mkSharedPeerState PeerIdle (emptyPeerScore now))
      ]
    txAdvertisers0 = mkAdvertisers [(owner, AckWhenBuffered), (peeraddr, AckWhenResolved)]
    unresolvedSharedState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable
          , txAdvertisers = txAdvertisers0
          , txTieBreakSalt = txid
          , txAttempts = Map.singleton owner TxBuffered
          }
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    resolvedSharedState = unresolvedSharedState
      { sharedTxTable = IntMap.empty
      , sharedRetainedTxs = retainedSingleton k (addTime 17 now)
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = 0
      }
    (unresolvedAction, unresolvedPeerState', unresolvedSharedState') = nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 unresolvedSharedState
    unresolvedExpectations =
      conjoin
        [ peerUnacknowledgedTxIds unresolvedPeerState' === peerUnacknowledgedTxIds peerState0
        , txAdvertisers (lookupEntryOrFail key unresolvedSharedState') === txAdvertisers (lookupEntryOrFail key unresolvedSharedState)
        ]
    (resolvedAction, resolvedPeerState', _) = nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 resolvedSharedState

-- Verifies that nextPeerActionPipelined does nothing when it can only
-- acknowledge txids and cannot request new ones in the same step.
prop_nextPeerActionPipelined_requiresAckAndReq
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_requiresAckAndReq (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ _ ->
         conjoin
           [ peerUnacknowledgedTxIds peerState' === peerUnacknowledgedTxIds peerState0
           , sharedState' === sharedState0
           ]
       _ -> counterexample ("unexpected pipelined action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedRetainedTxs = retainedSingleton k (addTime 17 now)
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined requests txids once it can both
-- acknowledge old txids and ask for more.
prop_nextPeerActionPipelined_requestsTxIds
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_requestsTxIds (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerRequestTxIds txIdsToAcknowledge txIdsToReq ->
         conjoin
           [ txIdsToAcknowledge === 1
           , counterexample ("expected positive txIdsToReq, got " ++ show txIdsToReq) (txIdsToReq > 0)
           , peerUnacknowledgedTxIds peerState' === StrictSeq.empty
           , peerRequestedTxIds peerState' === txIdsToReq
           , sharedRetainedTxs sharedState' === sharedRetainedTxs sharedState0
           , sharedTxTable sharedState' === sharedTxTable sharedState0
           , sharedTxIdToKey sharedState' === sharedTxIdToKey sharedState0
           , sharedKeyToTxId sharedState' === sharedKeyToTxId sharedState0
           , sharedGeneration sharedState' === sharedGeneration sharedState0
           ]
       _ -> counterexample ("unexpected pipelined action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = 0
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedRetainedTxs = retainedSingleton k (addTime 17 now)
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined opens a second outstanding body
-- batch when another downloadable tx is available.
prop_nextPeerActionPipelined_secondBodyBatch
  :: Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_secondBodyBatch (Positive peeraddr) txidA0 txidB0 txSizeA0 txSizeB0 =
  txidA /= txidB ==>
    case peerAction of
         PeerRequestTxs [txKey] ->
           conjoin
             [ txKey === keyB
             , peerRequestedTxs peerState' === IntSet.fromList [kA, kB]
             , StrictSeq.length (peerRequestedTxBatches peerState') === 2
             , peerRequestedTxsSize peerState' === txSizeA + txSizeB
             , fmap txLease (IntMap.lookup kB (sharedTxTable sharedState')) ===
                 Just (TxLeased peeraddr (addTime (interTxSpace defaultTxDecisionPolicy) now))
             , fmap (Map.lookup peeraddr . txAttempts)
                 (IntMap.lookup kB (sharedTxTable sharedState')) ===
                 Just (Just TxDownloading)
             ]
         _ -> counterexample ("unexpected pipelined action: " ++ show peerAction) False
  where
    txidA = abs txidA0 + 1
    txidB = abs txidB0 + 2
    txSizeA = mkSize txSizeA0
    txSizeB = mkSize txSizeB0
    keyA = TxKey 0
    keyB = TxKey 1
    kA = unTxKey keyA
    kB = unTxKey keyB
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.singleton kB txSizeB
      , peerRequestedTxs = IntSet.singleton kA
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [keyA] txSizeA)
      , peerRequestedTxsSize = txSizeA
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedTxTable = IntMap.fromList
          [ (kA, mkTxEntry peeraddr txSizeA (Just TxDownloading))
          , (kB, TxEntry
              { txLease = TxClaimable
              , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenResolved)
              , txTieBreakSalt = txidB
              , txAttempts = Map.empty
              })
          ]
      , sharedTxIdToKey = Map.fromList [(txidA, keyA), (txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(kA, txidA), (kB, txidB)]
      , sharedNextTxKey = 2
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined does not open a third outstanding
-- body batch once the per-peer batch limit is reached.
prop_nextPeerActionPipelined_noThirdBodyBatch
  :: Positive Int
  -> TxId
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_noThirdBodyBatch (Positive peeraddr) txidA0 txidB0 txidC0 txSizeA0 txSizeB0 txSizeC0 =
  distinctTxIds ==>
    case peerAction of
         PeerDoNothing _ _ ->
           conjoin
             [ peerRequestedTxs peerState' === peerRequestedTxs peerState0
             , peerRequestedTxBatches peerState' === peerRequestedTxBatches peerState0
             , peerRequestedTxsSize peerState' === peerRequestedTxsSize peerState0
             , sharedState' === sharedState0
             ]
         _ -> counterexample ("unexpected pipelined action: " ++ show peerAction) False
  where
    txidA = abs txidA0 + 1
    txidB = abs txidB0 + 2
    txidC = abs txidC0 + 3
    distinctTxIds = length (nub [txidA, txidB, txidC]) == 3
    txSizeA = mkSize txSizeA0
    txSizeB = mkSize txSizeB0
    txSizeC = mkSize txSizeC0
    keyA = TxKey 0
    keyB = TxKey 1
    keyC = TxKey 2
    kA = unTxKey keyA
    kB = unTxKey keyB
    kC = unTxKey keyC
    peerState0 = emptyPeerTxLocalState
      { peerAvailableTxIds = IntMap.singleton kC txSizeC
      , peerRequestedTxs = IntSet.fromList [kA, kB]
      , peerRequestedTxBatches = StrictSeq.fromList
          [ mkRequestedTxBatch [keyA] txSizeA
          , mkRequestedTxBatch [keyB] txSizeB
          ]
      , peerRequestedTxsSize = txSizeA + txSizeB
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedTxTable = IntMap.fromList
          [ (kA, mkTxEntry peeraddr txSizeA (Just TxDownloading))
          , (kB, mkTxEntry peeraddr txSizeB (Just TxDownloading))
          , (kC, TxEntry
              { txLease = TxClaimable
              , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenResolved)
              , txTieBreakSalt = txidC
              , txAttempts = Map.empty
              })
          ]
      , sharedTxIdToKey = Map.fromList [(txidA, keyA), (txidB, keyB), (txidC, keyC)]
      , sharedKeyToTxId = IntMap.fromList [(kA, txidA), (kB, txidB), (kC, txidC)]
      , sharedNextTxKey = 3
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerAction prunes expired retained txs and removes their
-- tx-key mappings while the peer is idle.
prop_nextPeerAction_prunesExpiredRetained
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_prunesExpiredRetained (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ Nothing ->
         conjoin
           [ peerState' === idlePeerState
           , IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
           , retainedLookup k (sharedRetainedTxs sharedState') === Nothing
           , Map.lookup txid (sharedTxIdToKey sharedState') === Nothing
           , IntMap.lookup k (sharedKeyToTxId sharedState') === Nothing
           , sharedGeneration sharedState' === sharedGeneration sharedState0 + 1
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    idlePeerState :: PeerTxLocalState (Tx TxId)
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedRetainedTxs = retainedSingleton k now
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = max 1 (k + 1)
      }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peeraddr idlePeerState sharedState0

-- Verifies that nextPeerAction keeps unexpired retained txs and returns the
-- wake delay until their expiry.
prop_nextPeerAction_keepsRetained
  :: Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_keepsRetained (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ (Just wakeDelay) ->
         conjoin
           [ peerState' === idlePeerState
           , IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
           , retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil
           , Map.lookup txid (sharedTxIdToKey sharedState') === Just key
           , IntMap.lookup k (sharedKeyToTxId sharedState') === Just txid
           , wakeDelay === diffTime retainUntil now
           , sharedGeneration sharedState' === sharedGeneration sharedState0
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    retainUntil = addTime 17 now
    key = TxKey 0
    k = unTxKey key
    idlePeerState :: PeerTxLocalState (Tx TxId)
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle (emptyPeerScore now))
      , sharedRetainedTxs = retainedSingleton k retainUntil
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerAction now defaultTxDecisionPolicy peeraddr idlePeerState sharedState0

-- Verifies that PeerDoNothing waits until the earliest shared expiry, whether
-- it comes from a lease or a retained tx.
prop_nextPeerAction_earliestWakeDelay
  :: Positive Int
  -> Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerAction_earliestWakeDelay (Positive peeraddr) (Positive owner0) txidA0 txidB0 _txSizeA0 _txSizeB0 =
  peeraddr /= owner ==>
    conjoin
      [ case leaseFirstAction of
             PeerDoNothing _ (Just wakeDelay) -> wakeDelay === diffTime leaseUntil now
             _ -> counterexample ("unexpected lease-first action: " ++ show leaseFirstAction) False
      , case retainFirstAction of
             PeerDoNothing _ (Just wakeDelay) -> wakeDelay === diffTime retainUntilSoon now
             _ -> counterexample ("unexpected retain-first action: " ++ show retainFirstAction) False
      ]
  where
    owner = owner0 + 1000
    txidA = abs txidA0 + 1
    txidB = abs txidB0 + 2
    leaseUntil = addTime 11 now
    retainUntilLater = addTime 29 now
    leaseUntilLater = addTime 31 now
    retainUntilSoon = addTime 13 now
    keyA = TxKey 0
    keyB = TxKey 1
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy }
    sharedPeers0 = Map.fromList
      [ (peeraddr, mkSharedPeerState PeerIdle (emptyPeerScore now))
      , (owner, mkSharedPeerState PeerWaitingTxs (emptyPeerScore now))
      ]
    leaseFirstState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton (unTxKey keyA) TxEntry
          { txLease = TxLeased owner leaseUntil
          , txAdvertisers = mkAdvertisers [(owner, AckWhenBuffered), (peeraddr, AckWhenResolved)]
          , txTieBreakSalt = txidA
          , txAttempts = Map.empty
          }
      , sharedRetainedTxs = retainedSingleton (unTxKey keyB) retainUntilLater
      , sharedTxIdToKey = Map.fromList [(txidA, keyA), (txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(unTxKey keyA, txidA), (unTxKey keyB, txidB)]
      , sharedNextTxKey = 2
      }
    retainFirstState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton (unTxKey keyA) TxEntry
          { txLease = TxLeased owner leaseUntilLater
          , txAdvertisers = mkAdvertisers [(owner, AckWhenBuffered), (peeraddr, AckWhenResolved)]
          , txTieBreakSalt = txidA
          , txAttempts = Map.empty
          }
      , sharedRetainedTxs = retainedSingleton (unTxKey keyB) retainUntilSoon
      , sharedTxIdToKey = Map.fromList [(txidA, keyA), (txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(unTxKey keyA, txidA), (unTxKey keyB, txidB)]
      , sharedNextTxKey = 2
      }
    (leaseFirstAction, _, _) = nextPeerAction now defaultTxDecisionPolicy peeraddr idlePeerState leaseFirstState
    (retainFirstAction, _, _) = nextPeerAction now defaultTxDecisionPolicy peeraddr idlePeerState retainFirstState

-- Verifies that PeerDoNothing reports the current generation of the acting
-- peer.
prop_nextPeerAction_returnsPeerGeneration
  :: Positive Int
  -> Property
prop_nextPeerAction_returnsPeerGeneration (Positive peeraddr) =
  case peerAction of
       PeerDoNothing generation Nothing -> generation === expectedGeneration
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    expectedGeneration = 7
    sharedState0 :: SharedTxState PeerAddr TxId
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ ( peeraddr
            , (mkSharedPeerState PeerIdle (emptyPeerScore now))
                { sharedPeerGeneration = expectedGeneration
                }
            )
          , ( peeraddr + 1000
            , (mkSharedPeerState PeerIdle (emptyPeerScore now))
                { sharedPeerGeneration = 11
                }
            )
          ]
      }
    peerState0 = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest defaultTxDecisionPolicy }
    (peerAction, _, _) = nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that handleSubmittedTxs bumps idle advertisers while leaving
-- submitting and waiting advertisers unchanged.
prop_handleSubmittedTxs_bumpsIdleAdvertisers
  :: Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleSubmittedTxs_bumpsIdleAdvertisers (Positive owner0) (Positive peerA0) (Positive peerB0) txid0 txSize0 =
  owner /= peerA && owner /= peerB && peerA /= peerB ==>
    conjoin
      [ sharedPeerGeneration (lookupPeerOrFail peerA sharedState') === 1
      , sharedPeerGeneration (lookupPeerOrFail peerB sharedState') === 0
      , sharedPeerGeneration (lookupPeerOrFail owner sharedState') === 0
      ]
  where
    owner = owner0 + 1000
    peerA = peerA0 + 2000
    peerB = peerB0 + 3000
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (owner, mkSharedPeerState PeerSubmittingToMempool (emptyPeerScore now))
          , (peerA, mkSharedPeerState PeerIdle (emptyPeerScore now))
          , (peerB, mkSharedPeerState PeerWaitingTxs (emptyPeerScore now))
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased owner (addTime 10 now)
          , txAdvertisers = mkAdvertisers
              [ (owner, AckWhenBuffered)
              , (peerA, AckWhenResolved)
              , (peerB, AckWhenResolved)
              ]
          , txTieBreakSalt = txid
          , txAttempts = Map.singleton owner TxBuffered
          }
      , sharedTxIdToKey = Map.singleton txid key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState { peerDownloadedTxs = IntMap.singleton k tx }
    (_, sharedState') = handleSubmittedTxs now defaultTxDecisionPolicy owner [key] [] peerState0 sharedState0

unit_updatePeerPhase_wakesOnlyBecomingIdlePeer :: (String -> IO ()) -> Assertion
unit_updatePeerPhase_wakesOnlyBecomingIdlePeer step = do
  step "Update a peer from waiting to idle"
  sharedPeerPhase (lookupPeerOrFail peer sharedState') @?= PeerIdle
  step "Assert only the becoming-idle peer generation changes"
  sharedPeerGeneration (lookupPeerOrFail peer sharedState') @?= 6
  sharedPeerGeneration (lookupPeerOrFail other sharedState') @?= 11
  where
    peer = 1
    other = 2
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peer, (mkSharedPeerState PeerWaitingTxs (emptyPeerScore now)) { sharedPeerGeneration = 5 })
          , (other, (mkSharedPeerState PeerIdle (emptyPeerScore now)) { sharedPeerGeneration = 11 })
          ]
      }
    sharedState' = updatePeerPhase peer PeerIdle sharedState0

unit_updatePeerPhase_wakesCompetingAdvertisers :: (String -> IO ()) -> Assertion
unit_updatePeerPhase_wakesCompetingAdvertisers step = do
  step "Update an idle peer to a waiting phase"
  sharedPeerPhase (lookupPeerOrFail leavingPeer sharedState') @?= PeerWaitingTxs
  step "Assert competing idle advertisers are woken but unrelated peers are not"
  sharedPeerGeneration (lookupPeerOrFail leavingPeer sharedState') @?= 5
  sharedPeerGeneration (lookupPeerOrFail competingPeer sharedState') @?= 12
  sharedPeerGeneration (lookupPeerOrFail unrelatedPeer sharedState') @?= 17
  where
    leavingPeer = 1
    competingPeer = 2
    unrelatedPeer = 3
    txid = 1
    baseState = mkSharedState [txid]
    key = lookupKeyOrFail txid baseState
    k = unTxKey key
    sharedState0 = baseState
      { sharedPeers = Map.fromList
          [ (leavingPeer, (mkSharedPeerState PeerIdle (emptyPeerScore now)) { sharedPeerGeneration = 5 })
          , (competingPeer, (mkSharedPeerState PeerIdle (emptyPeerScore now)) { sharedPeerGeneration = 11 })
          , (unrelatedPeer, (mkSharedPeerState PeerIdle (emptyPeerScore now)) { sharedPeerGeneration = 17 })
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable
          , txAdvertisers = mkAdvertisers
              [ (leavingPeer, AckWhenResolved)
              , (competingPeer, AckWhenResolved)
              ]
          , txTieBreakSalt = txid
          , txAttempts = Map.empty
          }
      }
    sharedState' = updatePeerPhase leavingPeer PeerWaitingTxs sharedState0

-- Generate a shared peer state.
genSharedPeerState :: Gen SharedPeerState
genSharedPeerState = do
  sharedPeerPhase <- elements [PeerIdle, PeerWaitingTxIds, PeerWaitingTxs, PeerSubmittingToMempool]
  peerScoreValue <- choose (0 :: Double, 100)
  peerScoreTs <- genSmallTime
  sharedPeerGeneration <- genSmallWord64
  pure SharedPeerState {
      sharedPeerPhase,
      sharedPeerScore = PeerScore peerScoreValue peerScoreTs,
      sharedPeerGeneration
    }

-- Generate a self-consistent local peer view of requested, available, and downloaded txs.
genPeerTxLocalState :: Gen (PeerTxLocalState (Tx TxId))
genPeerTxLocalState = sized $ \n -> do
  let maxKeys = min 12 (n + 2)

  numKeys <- chooseInt (0, maxKeys)
  peerRequestedTxIds <- fromIntegral <$> chooseInt (0, min 8 (n + 1))
  peerUnacknowledgedTxIds <- StrictSeq.fromList <$> shuffle [ TxKey key | key <- [0 .. numKeys - 1] ]

  downloadedKeys <- sublistOf (toList peerUnacknowledgedTxIds)
  let downloadedSet =
        IntSet.fromList [ unTxKey key | key <- downloadedKeys ]
      remainingKeys =
        [ key
        | key <- toList peerUnacknowledgedTxIds
        , not (IntSet.member (unTxKey key) downloadedSet)
        ]

  requestedKeys <- sublistOf remainingKeys
  let requestedSet =
        IntSet.fromList [ unTxKey key | key <- requestedKeys ]
      availableExtraCandidates =
        [ key
        | key <- remainingKeys
        , not (IntSet.member (unTxKey key) requestedSet)
        ]
  availableExtraKeys <- sublistOf availableExtraCandidates

  let availableKeys = requestedKeys <> availableExtraKeys
  peerAvailableTxIds <-
    IntMap.fromList <$> mapM genAvailableTx availableKeys

  let requestedKeysOrdered =
        [ key
        | key <- toList peerUnacknowledgedTxIds
        , IntSet.member (unTxKey key) requestedSet
        ]
  (peerRequestedTxBatches, peerRequestedTxsSize) <-
    genRequestedTxBatches peerAvailableTxIds requestedKeysOrdered

  peerDownloadedTxs <-
    IntMap.fromList <$> mapM genDownloadedTx downloadedKeys

  pure PeerTxLocalState {
      peerUnacknowledgedTxIds,
      peerAvailableTxIds,
      peerRequestedTxs = requestedSet,
      peerRequestedTxBatches,
      peerRequestedTxsSize,
      peerRequestedTxIds,
      peerDownloadedTxs
    }
  where
    genAvailableTx key = do
      txSize <- genPositiveSize
      pure (unTxKey key, txSize)

    genDownloadedTx key = do
      txSize <- genPositiveSize
      pure (unTxKey key, mkTx (txIdForKey key) txSize)

data PeerSeed = PeerSeed {
    peerSeedScore      :: !PeerScore
  , peerSeedGeneration :: !Word64
  }

data PeerDerivedUsage = PeerDerivedUsage {
    peerHasSubmitting    :: !Bool
  , peerHasRequestedTxs  :: !Bool
  }

-- Generate a shared tx state with distinct active and retained entries.
genSharedTxState :: Gen (SharedTxState PeerAddr TxId)
genSharedTxState = sized $ \n -> do
  let maxPeers = min 6 (n + 1)
      maxActiveTxs = min 8 (n + 2)
      maxRetainedTxs = min 6 (n + 2)

  numPeers <- chooseInt (0, maxPeers)
  peeraddrs <- genDistinctPositiveInts numPeers
  peerSeeds <- Map.fromList <$> mapM genPeerSeedEntry peeraddrs

  numActiveTxs <-
    if null peeraddrs
       then pure 0
       else chooseInt (0, maxActiveTxs)
  numRetainedTxs <- chooseInt (0, maxRetainedTxs)

  txids <- genDistinctPositiveInts (numActiveTxs + numRetainedTxs)
  let (activeTxIds, retainedTxIds) = splitAt numActiveTxs txids

  activeEntries <- mapM (genActiveTxEntry peeraddrs) activeTxIds
  retainedEntries <- mapM genRetainedEntry retainedTxIds
  sharedGeneration <- genSmallWord64

  pure $ buildSharedTxState peerSeeds activeEntries retainedEntries sharedGeneration
  where
    genPeerSeedEntry peeraddr = do
      peerScoreValue <- choose (0 :: Double, 100)
      peerScoreTs <- genSmallTime
      peerSeedGeneration <- genSmallWord64
      pure ( peeraddr
           , PeerSeed {
               peerSeedScore = PeerScore peerScoreValue peerScoreTs,
               peerSeedGeneration
             }
           )

    genRetainedEntry txid = do
      retainUntil <- genSharedExpiryTime
      pure (txid, retainUntil)

-- Generate one active tx entry using a mix of leased and claimable shapes.
genActiveTxEntry :: [PeerAddr] -> TxId -> Gen (TxId, TxEntry PeerAddr)
genActiveTxEntry peeraddrs txid = do
  txEntry <- frequency
    [ (5, genLeasedTxEntry peeraddrs txid)
    , (3, genClaimableTxEntry peeraddrs txid)
    ]
  pure (txid, txEntry)

-- Generate a leased entry where the owner may already be downloading, buffered, or submitting.
genLeasedTxEntry :: [PeerAddr] -> TxId -> Gen (TxEntry PeerAddr)
genLeasedTxEntry peeraddrs txid = do
  advertiserPeers <- genNonEmptySublist peeraddrs
  owner <- elements advertiserPeers
  txAdvertisers <- genOwnedAdvertisers advertiserPeers owner
  txLease <- TxLeased owner <$> genSharedExpiryTime
  ownerAttempt <- frequency
    [ (2, pure Nothing)
    , (2, Just <$> elements [TxDownloading, TxBuffered])
    , (1, pure (Just TxSubmitting))
    ]
  pure TxEntry {
      txLease,
      txAdvertisers,
      txTieBreakSalt = txid,
      txAttempts = maybe Map.empty (Map.singleton owner) ownerAttempt
    }

-- Generate a claimable entry advertised by one or more resolved peers.
genClaimableTxEntry :: [PeerAddr] -> TxId -> Gen (TxEntry PeerAddr)
genClaimableTxEntry peeraddrs txid = do
  advertiserPeers <- genNonEmptySublist peeraddrs
  txAdvertisers <- genResolvedAdvertisers advertiserPeers
  pure TxEntry {
      txLease = TxClaimable,
      txAdvertisers,
      txTieBreakSalt = txid,
      txAttempts = Map.empty
    }

-- Generate advertisers where only the chosen owner uses AckWhenBuffered.
genOwnedAdvertisers
  :: [PeerAddr]
  -> PeerAddr
  -> Gen (Map.Map PeerAddr TxAdvertiser)
genOwnedAdvertisers advertiserPeers owner =
  Map.fromList <$> mapM genAdvertiser advertiserPeers
  where
    genAdvertiser peeraddr =
      let txAckState
            | peeraddr == owner = AckWhenBuffered
            | otherwise = AckWhenResolved
      in pure (peeraddr, TxAdvertiser { txAckState })

-- Generate advertisers that all acknowledge on resolution.
genResolvedAdvertisers :: [PeerAddr] -> Gen (Map.Map PeerAddr TxAdvertiser)
genResolvedAdvertisers advertiserPeers =
  Map.fromList <$> mapM genAdvertiser advertiserPeers
  where
    genAdvertiser peeraddr =
      pure (peeraddr, TxAdvertiser { txAckState = AckWhenResolved })

-- Rebuild a shared state from tx-centric fixtures while preserving interned keys.
buildSharedTxState
  :: Map.Map PeerAddr PeerSeed
  -> [(TxId, TxEntry PeerAddr)]
  -> [(TxId, Time)]
  -> Word64
  -> SharedTxState PeerAddr TxId
buildSharedTxState peerSeeds activeEntries retainedEntries sharedGeneration =
  baseState {
      sharedPeers = deriveSharedPeers peerSeeds activeEntries,
      sharedTxTable =
        IntMap.fromList
          [ (unTxKey (lookupKeyOrFail txid baseState), txEntry)
          | (txid, txEntry) <- activeEntries
          ],
      sharedRetainedTxs =
        retainedFromList
          [ (unTxKey (lookupKeyOrFail txid baseState), retainUntil)
          | (txid, retainUntil) <- retainedEntries
          ],
      sharedGeneration
    }
  where
    baseState = mkSharedState (fmap fst activeEntries <> fmap fst retainedEntries)

-- Derive peer phases from the generated tx entries.
deriveSharedPeers
  :: Map.Map PeerAddr PeerSeed
  -> [(TxId, TxEntry PeerAddr)]
  -> Map.Map PeerAddr SharedPeerState
deriveSharedPeers peerSeeds activeEntries =
  Map.mapWithKey buildPeerState completePeerSeeds
  where
    completePeerSeeds =
      foldl' addMissingPeerSeed peerSeeds (concatMap (entryPeers . snd) activeEntries)

    peerUsages =
      foldl' accumulatePeerUsage Map.empty activeEntries

    addMissingPeerSeed acc peeraddr =
      Map.insertWith (\_ old -> old) peeraddr defaultPeerSeed acc

    buildPeerState peeraddr PeerSeed { peerSeedScore, peerSeedGeneration } =
      let PeerDerivedUsage {
            peerHasSubmitting,
            peerHasRequestedTxs
          } = Map.findWithDefault emptyPeerDerivedUsage peeraddr peerUsages
          sharedPeerPhase
            | peerHasSubmitting = PeerSubmittingToMempool
            | peerHasRequestedTxs = PeerWaitingTxs
            | otherwise = PeerIdle in
      SharedPeerState {
          sharedPeerPhase,
          sharedPeerScore = peerSeedScore,
          sharedPeerGeneration = peerSeedGeneration
        }

    defaultPeerSeed =
      PeerSeed {
        peerSeedScore = emptyPeerScore now,
        peerSeedGeneration = 0
      }

-- Default derived usage for a peer with no active work.
emptyPeerDerivedUsage :: PeerDerivedUsage
emptyPeerDerivedUsage =
  PeerDerivedUsage {
      peerHasSubmitting = False,
      peerHasRequestedTxs = False
    }

-- Fold one tx entry's attempts into the derived per-peer usage map.
accumulatePeerUsage
  :: Map.Map PeerAddr PeerDerivedUsage
  -> (TxId, TxEntry PeerAddr)
  -> Map.Map PeerAddr PeerDerivedUsage
accumulatePeerUsage acc (_, TxEntry { txAttempts }) =
  foldl' step acc (Map.toList txAttempts)
  where
    step acc' (peeraddr, attempt) =
      case attempt of
        TxDownloading ->
          updatePeerUsage peeraddr False True acc'
        TxSubmitting ->
          updatePeerUsage peeraddr True False acc'
        TxBuffered ->
          updatePeerUsage peeraddr False False acc'
        TxNoAttempt ->
          acc'

-- Merge one peer's submitting and inflight usage into the accumulator.
updatePeerUsage
  :: PeerAddr
  -> Bool
  -> Bool
  -> Map.Map PeerAddr PeerDerivedUsage
  -> Map.Map PeerAddr PeerDerivedUsage
updatePeerUsage peeraddr submitting hasRequestedTxs acc =
  Map.insert peeraddr usage' acc
  where
    usage =
      Map.findWithDefault emptyPeerDerivedUsage peeraddr acc
    usage' =
      usage {
        peerHasSubmitting = peerHasSubmitting usage || submitting,
        peerHasRequestedTxs = peerHasRequestedTxs usage || hasRequestedTxs
      }

-- Collect every peer mentioned by a tx entry.
entryPeers :: TxEntry PeerAddr -> [PeerAddr]
entryPeers TxEntry { txLease, txAdvertisers, txAttempts } =
  leaseOwner <> Map.keys txAdvertisers <> Map.keys txAttempts
  where
    leaseOwner =
      case txLease of
        TxLeased owner _ -> [owner]
        TxClaimable -> []

-- Shrink shared state by dropping active txs, retained txs, or unused peers.
shrinkSharedTxState
  :: SharedTxState PeerAddr TxId
  -> [SharedTxState PeerAddr TxId]
shrinkSharedTxState sharedState =
  nub $
    [ emptySharedTxState
    , buildSharedTxState peerSeeds [] retainedEntries 0
    , buildSharedTxState peerSeeds activeEntries [] 0
    , buildSharedTxState usedPeerSeeds activeEntries retainedEntries 0
    ] ++
    [ buildSharedTxState peerSeeds activeEntries' retainedEntries 0
    | activeEntries' <- smallerActiveEntries
    ] ++
    [ buildSharedTxState peerSeeds activeEntries retainedEntries' 0
    | retainedEntries' <- smallerRetainedEntries
    ]
  where
    activeEntries =
      [ (resolveTxKey sharedState (TxKey k), txEntry)
      | (k, txEntry) <- IntMap.toList (sharedTxTable sharedState)
      ]
    retainedEntries =
      [ (resolveTxKey sharedState (TxKey k), retainUntil)
      | (k, retainUntil) <- retainedToList (sharedRetainedTxs sharedState)
      ]
    peerSeeds =
      Map.map
        (\SharedPeerState { sharedPeerScore, sharedPeerGeneration } ->
          PeerSeed {
            peerSeedScore = sharedPeerScore,
            peerSeedGeneration = sharedPeerGeneration
          })
        (sharedPeers sharedState)
    usedPeers =
      foldl' (\peers (_, txEntry) -> peers <> entryPeers txEntry) [] activeEntries
    usedPeerSeeds =
      Map.filterWithKey (\peeraddr _ -> peeraddr `elem` usedPeers) peerSeeds
    smallerActiveEntries =
      take 6
        [ activeEntries'
        | activeEntries' <- shrinkList (const []) activeEntries
        , length activeEntries' < length activeEntries
        ]
    smallerRetainedEntries =
      take 6
        [ retainedEntries'
        | retainedEntries' <- shrinkList (const []) retainedEntries
        , length retainedEntries' < length retainedEntries
        ]

-- Partition requested keys into a small number of contiguous request batches.
genRequestedTxBatches
  :: IntMap.IntMap SizeInBytes
  -> [TxKey]
  -> Gen (StrictSeq.StrictSeq RequestedTxBatch, SizeInBytes)
genRequestedTxBatches _ [] =
  pure (StrictSeq.empty, 0)
genRequestedTxBatches peerAvailableTxIds requestedKeys = do
  batchCount <- chooseInt (1, min 3 (length requestedKeys))
  batchLengths <- genPositivePartition (length requestedKeys) batchCount
  let batches =
        [ mkRequestedTxBatch keys (sum [ lookupAvailableTxSize key | key <- keys ])
        | keys <- splitByLengths batchLengths requestedKeys
        ]
      peerRequestedTxsSize =
        sum [ requestedTxBatchSize batch | batch <- batches ]
  pure (StrictSeq.fromList batches, peerRequestedTxsSize)
  where
    lookupAvailableTxSize key =
      case IntMap.lookup (unTxKey key) peerAvailableTxIds of
        Just txSize -> txSize
        Nothing -> error "TxLogic.genRequestedTxBatches: missing requested tx size"

-- Split a positive total into a fixed number of positive parts.
genPositivePartition :: Int -> Int -> Gen [Int]
genPositivePartition totalCount 1 =
  pure [totalCount]
genPositivePartition totalCount parts = do
  n <- chooseInt (1, totalCount - parts + 1)
  (n :) <$> genPositivePartition (totalCount - n) (parts - 1)

-- Split a list according to a list of chunk lengths.
splitByLengths :: [Int] -> [a] -> [[a]]
splitByLengths [] [] = []
splitByLengths [] _ = []
splitByLengths (n : ns) xs =
  let (prefix, suffix) = splitAt n xs in
  prefix : splitByLengths ns suffix

-- Pick a sublist, falling back to a singleton when the input is non-empty.
genNonEmptySublist :: [a] -> Gen [a]
genNonEmptySublist [] =
  pure []
genNonEmptySublist xs = do
  ys <- sublistOf xs
  case ys of
    [] -> (: []) <$> elements xs
    _ -> pure ys

-- Generate distinct positive ints from a bounded shuffled range.
genDistinctPositiveInts :: Int -> Gen [Int]
genDistinctPositiveInts count
  | count <= 0 = pure []
  | otherwise = take count <$> shuffle [1 .. max count (count * 4 + 5)]

-- Generate expiry times near the shared test reference time.
genSharedExpiryTime :: Gen Time
genSharedExpiryTime =
  Time . fromIntegral <$> chooseInt (80, 120)

-- Generate a positive tx size
genPositiveSize :: Gen SizeInBytes
genPositiveSize =
  fromIntegral <$> chooseInt (1, fromIntegral $ getSizeInBytes max_TX_SIZE)

-- Generate a small test timestamp.
genSmallTime :: Gen Time
genSmallTime =
  Time . fromIntegral <$> chooseInt (0, 1000)

-- Generate a small test generation counter.
genSmallWord64 :: Gen Word64
genSmallWord64 =
  fromIntegral <$> chooseInt (0, 1000)

-- Recover the fixture txid associated with an interned key.
txIdForKey :: TxKey -> TxId
txIdForKey (TxKey key) = key + 1

-- Fixed reference time used by deterministic test fixtures.
now :: Time
now = Time 100

-- Convert a positive QuickCheck value into a bounded non-zero tx size.
mkSize :: Positive Int -> SizeInBytes
mkSize (Positive n) = fromIntegral ((n `mod` 65535) + 1)

-- Build a valid test transaction with matching body and advertised size.
mkTx :: TxId -> SizeInBytes -> Tx TxId
mkTx txid txSize = Tx
  { getTxId = txid
  , getTxSize = txSize
  , getTxAdvSize = txSize
  , getTxValid = True
  }

-- Construct a peer fixture with zeroed generation.
mkSharedPeerState :: PeerPhase -> PeerScore -> SharedPeerState
mkSharedPeerState sharedPeerPhase sharedPeerScore =
  SharedPeerState {
    sharedPeerPhase,
    sharedPeerScore,
    sharedPeerGeneration = 0
  }

-- Intern a list of txids into an otherwise empty shared state.
mkSharedState :: [TxId] -> SharedTxState PeerAddr TxId
mkSharedState txids = snd (internTxIds txids emptySharedTxState)

-- Construct a single advertiser entry.
mkAdvertiser :: TxOwnerAckState -> TxAdvertiser
mkAdvertiser txAckState = TxAdvertiser { txAckState }

-- Construct an advertiser map for a set of peers.
mkAdvertisers :: [(PeerAddr, TxOwnerAckState)] -> Map.Map PeerAddr TxAdvertiser
mkAdvertisers = Map.fromList . fmap (\(peeraddr, txAckState) -> (peeraddr, mkAdvertiser txAckState))

-- Construct a requested batch together with its cached key set.
mkRequestedTxBatch :: [TxKey] -> SizeInBytes -> RequestedTxBatch
mkRequestedTxBatch keys requestedTxBatchSize = RequestedTxBatch
  { requestedTxBatchSet = IntSet.fromList (map unTxKey keys)
  , requestedTxBatchSize
  }

-- Construct a leased tx entry owned by one peer.
mkTxEntry :: PeerAddr -> SizeInBytes -> Maybe TxAttemptState -> TxEntry PeerAddr
mkTxEntry peeraddr _txSize mAttempt = TxEntry
  { txLease = TxLeased peeraddr (addTime 10 now)
  , txAdvertisers = Map.singleton peeraddr (mkAdvertiser AckWhenBuffered)
  , txTieBreakSalt = 0
  , txAttempts = maybe Map.empty (Map.singleton peeraddr) mAttempt
  }

-- Look up an interned key and fail fast in test setup code.
lookupKeyOrFail :: TxId -> SharedTxState PeerAddr TxId -> TxKey
lookupKeyOrFail txid st =
  case lookupTxKey txid st of
    Just txKey -> txKey
    Nothing -> error "TxLogic.lookupKeyOrFail: missing tx key"

-- Look up an active tx entry and fail fast in test setup code.
lookupEntryOrFail :: TxKey -> SharedTxState PeerAddr TxId -> TxEntry PeerAddr
lookupEntryOrFail (TxKey k) st =
  case IntMap.lookup k (sharedTxTable st) of
    Just txEntry -> txEntry
    Nothing -> error "TxLogic.lookupEntryOrFail: missing tx entry"

-- Look up a shared peer and fail fast in test setup code.
lookupPeerOrFail :: PeerAddr -> SharedTxState PeerAddr TxId -> SharedPeerState
lookupPeerOrFail peeraddr st =
  case Map.lookup peeraddr (sharedPeers st) of
    Just sharedPeerState -> sharedPeerState
    Nothing -> error "TxLogic.lookupPeerOrFail: missing peer"

-- Drop duplicate txids while keeping the first proposed size.
dedupeBatch :: [(TxId, SizeInBytes)] -> [(TxId, SizeInBytes)]
dedupeBatch = nubBy ((==) `on` fst)

-- Shift proposed txids forward until the batch is disjoint from the shared intern table.
freshBatchAgainstSharedState :: SharedTxState PeerAddr TxId -> [(TxId, SizeInBytes)] -> [(TxId, SizeInBytes)]
freshBatchAgainstSharedState sharedState = reverse . snd . foldl' step (reserved, [])
  where
    reserved = IntSet.fromList (Map.keys (sharedTxIdToKey sharedState))

    step (used, acc) (txid, txSize) =
      let freshTxId = firstFreshTxId used txid in
      (IntSet.insert freshTxId used, (freshTxId, txSize) : acc)

-- Find the first txid not present in the reserved set.
firstFreshTxId :: IntSet.IntSet -> TxId -> TxId
firstFreshTxId used = go
  where
    go txid
      | IntSet.member txid used = go (txid + 1)
      | otherwise = txid
