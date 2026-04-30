{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Test.Ouroboros.Network.TxSubmission.TxLogic
  ( tests
  , ArbTxDecisionPolicy (..)
  , PeerAddr
  , ArbSharedTxState (..)
  , ArbSharedPeerState (..)
  , ArbPeerTxLocalState (..)
  , ReceiveDuplicateFixture
  , PeerActionFixture
  , FanoutFixture
  , mkReceiveDuplicateFixture
  , mkResolvedAckFixture
  , mkForeignRejectedFixture
  , mkFanoutFixture
  , runReceiveDuplicateLoop
  , runPeerActionLoop
  , runFanoutLoop
  , sharedTxStateInvariant
  , peerTxLocalStateInvariant
  , combinedStateInvariant
  , InvariantStrength (..)
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Control.Monad.Class.MonadTime.SI (Time (..), addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (nub, nubBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word64)

import NoThunks.Class (NoThunks, unsafeNoThunks)



import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Tx (HasRawTxId (..), RawTxId, getRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry (updatePeerPhase)
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
    [ testProperty "handleReceivedTxIds handles mixed new / retained / mempool txids" prop_handleReceivedTxIds
    , testCaseSteps "handleReceivedTxIds adds the current peer as an advertiser for active txs" unit_handleReceivedTxIds_addsAdvertiserForActiveTxs
    , testCaseSteps "nextPeerAction lets another peer claim a fresh tx when the first advertiser is full" unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull
    , testProperty "handleReceivedTxs handles mixed buffered / omitted / late-retained / late-mempool / pruned txids" prop_handleReceivedTxs
    , testProperty "handleSubmittedTxs retains accepted and drops rejected" prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    , testProperty "nextPeerAction prioritises submitting buffered owned txs" prop_nextPeerAction_prioritisesSubmit
    , testProperty "nextPeerAction claims claimable tx for best idle advertiser" prop_nextPeerAction_claimsClaimableTx
    , testProperty "nextPeerAction claims a released tx from another advertiser" prop_nextPeerAction_claimsRejectedTxFromOtherAdvertiser
    , testCaseSteps "nextPeerAction claims a tx once the score delay threshold has elapsed" unit_nextPeerAction_claimsAtScoreDelayThreshold
    , testCaseSteps "peerScore decays linearly over time at scoreRate" unit_peerScore_decaysOverTime
    , testCaseSteps "applyPeerRejections drains the existing score before adding the new rejection count" unit_applyPeerRejections_drainsThenAdds
    , testProperty "nextPeerAction steals expired lease for best idle advertiser" prop_nextPeerAction_claimsExpiredLease
    , testProperty "nextPeerAction requests an oversized first tx within the soft budget" prop_nextPeerAction_requestsOversizedFirstTx
    , testCaseSteps "nextPeerAction skips blocked available txs and requests later claimable ones" unit_nextPeerAction_skipsBlockedAvailableTxs
    , testProperty "nextPeerAction submits buffered owned txs before acking" prop_nextPeerAction_ownerSubmitsBuffered
    , testCaseSteps "nextPeerAction requests other txs despite a blocked buffered tx" unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx
    , testCaseSteps "nextPeerAction only acks the safe prefix before a blocked buffered tx" unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx
    , testProperty "nextPeerAction keeps non-owner txids unacked until resolved" prop_nextPeerAction_nonOwnerWaitsUntilResolved
    , testProperty "nextPeerActionPipelined suppresses ack-only txid requests" prop_nextPeerActionPipelined_requiresAckAndReq
    , testProperty "nextPeerActionPipelined requests txids when it can ack and request" prop_nextPeerActionPipelined_requestsTxIds
    , testCaseSteps "nextPeerActionPipelined keeps one txid unacked while body replies are in flight" unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply
    , testProperty "nextPeerActionPipelined opens a second outstanding body batch" prop_nextPeerActionPipelined_secondBodyBatch
    , testProperty "nextPeerActionPipelined does not open a third outstanding body batch" prop_nextPeerActionPipelined_noThirdBodyBatch
    , testProperty "nextPeerAction prunes expired retained txs" prop_nextPeerAction_prunesExpiredRetained
    , testProperty "nextPeerAction keeps retained txs before expiry" prop_nextPeerAction_keepsRetained
    , testProperty "PeerDoNothing waits for the earliest shared expiry" prop_nextPeerAction_earliestWakeDelay
    , testProperty "PeerDoNothing uses the current peer generation" prop_nextPeerAction_returnsPeerGeneration
    , testProperty "handleSubmittedTxs bumps advertiser generations" prop_handleSubmittedTxs_bumpsAdvertisers
    , testCaseSteps "advertisingPeersForTxExcept scans the authoritative peer key sets" unit_advertisingPeersForTxExcept_scansAuthoritativePeerSets
    , testCaseSteps "removeAdvertisingPeersForResolvedTx clears all advertising peers for a resolved key" unit_removeAdvertisingPeersForResolvedTx_clearsAllAdvertisingPeers
    , testCaseSteps "updatePeerPhase only wakes the peer becoming idle" unit_updatePeerPhase_wakesOnlyBecomingIdlePeer
    , testCaseSteps "updatePeerPhase wakes competing idle advertisers when a peer leaves idle" unit_updatePeerPhase_wakesCompetingAdvertisers
    ]

--
-- NoThunks invariant checks
--

-- | Check that a value has no thunks in its fields.
checkNoThunks :: NoThunks a => String -> a -> Property
checkNoThunks name val =
    let result = unsafeNoThunks val
    in counterexample (name ++ ": " ++ show result) $ property True

--
-- InboundState properties
--

type PeerAddr = Int

data ReceiveDuplicateFixture = ReceiveDuplicateFixture
  { rdfPeerAddr       :: !PeerAddr
  , rdfRequestedTxIds :: !NumTxIdsToReq
  , rdfTxidsAndSizes  :: ![(TxId, SizeInBytes)]
  , rdfPeerState      :: !(PeerTxLocalState (Tx TxId))
  , rdfSharedState    :: !(SharedTxState PeerAddr TxId)
  }

data PeerActionFixture = PeerActionFixture
  { pafPeerAddr    :: !PeerAddr
  , pafPeerState   :: !(PeerTxLocalState (Tx TxId))
  , pafSharedState :: !(SharedTxState PeerAddr TxId)
  }

data FanoutFixture = FanoutFixture
  { ffPeers              :: ![PeerAddr]
  , ffRequestedTxIds     :: !NumTxIdsToReq
  , ffTxidsAndSizes      :: ![(TxId, SizeInBytes)]
  , ffInitialSharedState :: !(SharedTxState PeerAddr TxId)
  }

instance NFData ReceiveDuplicateFixture where
  rnf ReceiveDuplicateFixture { rdfPeerAddr, rdfRequestedTxIds, rdfTxidsAndSizes
                              , rdfPeerState, rdfSharedState } =
       rnf rdfPeerAddr
    `seq` rnf rdfRequestedTxIds
    `seq` rnf rdfTxidsAndSizes
    `seq` rnf rdfPeerState
    `seq` rnf rdfSharedState

instance NFData PeerActionFixture where
  rnf PeerActionFixture { pafPeerAddr, pafPeerState, pafSharedState } =
       rnf pafPeerAddr
    `seq` rnf pafPeerState
    `seq` rnf pafSharedState

instance NFData FanoutFixture where
  rnf FanoutFixture { ffPeers, ffRequestedTxIds, ffTxidsAndSizes, ffInitialSharedState } =
       rnf ffPeers
    `seq` rnf ffRequestedTxIds
    `seq` rnf ffTxidsAndSizes
    `seq` rnf ffInitialSharedState

data InvariantStrength = WeakInvariant
                       | StrongInvariant
  deriving (Eq, Show)

-- | 'PeerTxLocalState' invariant.
--
-- Consistency constraints between the peer-local bookkeeping maps:
--
--   * Every available or downloaded key is still tracked in the
--     unacknowledged queue.
--   * Every requested key is also available (a key can only be requested
--     while its advertised size is still known).
--   * Downloaded keys are disjoint from available and from requested
--     (a key moves out of available/requested exactly when its body
--     lands in downloaded).
--   * @peerRequestedTxs@ equals the union of all @requestedTxBatchSet@s
--     and @peerRequestedTxsSize@ equals the sum of all batch sizes.
peerTxLocalStateInvariant
  :: forall tx.
     TxDecisionPolicy
  -> PeerTxLocalState tx
  -> Property
peerTxLocalStateInvariant TxDecisionPolicy { scoreMax }
                          PeerTxLocalState {
                            peerUnacknowledgedTxIds,
                            peerAvailableTxIds,
                            peerRequestedTxs,
                            peerRequestedTxBatches,
                            peerRequestedTxsSize,
                            peerDownloadedTxs,
                            peerScore
                          } =
  conjoin
    [ counterexample "requested keys are not all available"
        (property (peerRequestedTxs `IntSet.isSubsetOf` availableKeys))
    , counterexample "available keys are not all in the unacknowledged queue"
        (property (availableKeys `IntSet.isSubsetOf` unackKeys))
    , counterexample "downloaded keys are not all in the unacknowledged queue"
        (property (downloadedKeys `IntSet.isSubsetOf` unackKeys))
    , counterexample "downloaded and available key sets overlap"
        (IntSet.null (downloadedKeys `IntSet.intersection` availableKeys))
    , counterexample "downloaded and requested key sets overlap"
        (IntSet.null (downloadedKeys `IntSet.intersection` peerRequestedTxs))
    , counterexample "peerRequestedTxs does not match the batch key-set union"
        (peerRequestedTxs === batchKeyUnion)
    , counterexample "peerRequestedTxsSize does not match the sum of batch sizes"
        (peerRequestedTxsSize === batchSizeSum)
    , counterexample ("peerScoreValue is negative: " ++ show scoreVal)
        (property (scoreVal >= 0))
    , counterexample ("peerScoreValue exceeds scoreMax: "
                      ++ show scoreVal ++ " > " ++ show scoreMax)
        (property (scoreVal <= scoreMax))
    ]
  where
    scoreVal       = peerScoreValue peerScore
    unackKeys      = IntSet.fromList [ k | TxKey k <- toList peerUnacknowledgedTxIds ]
    availableKeys  = IntMap.keysSet peerAvailableTxIds
    downloadedKeys = IntMap.keysSet peerDownloadedTxs
    batchKeyUnion  =
      IntSet.unions (fmap requestedTxBatchSet (toList peerRequestedTxBatches))
    batchSizeSum   =
      sum (fmap requestedTxBatchSize (toList peerRequestedTxBatches))

-- | Combined 'SharedTxState' / 'PeerTxLocalState' invariant.
--
-- Runs the individual invariants on each piece and adds the cross-state
-- coherence constraints:
--
--   * The peer's 'sharedPeerAdvertisedTxKeys' (as recorded in the shared
--     state) are a subset of the peer's local unacknowledged queue — a peer
--     can only advertise keys it has actually received.
--   * Those advertised keys must have a matching entry in 'sharedTxTable':
--     an advertisement without an active tx entry is an orphan and would
--     leave 'txAdvertiserCount' out of sync with the peer key sets.
combinedStateInvariant
  :: forall peeraddr txid tx.
     ( Ord peeraddr
     , Ord txid
     , HasRawTxId txid
     , Show peeraddr
     , Show txid
     )
  => TxDecisionPolicy
  -> InvariantStrength
  -> peeraddr
  -> PeerTxLocalState tx
  -> SharedTxState peeraddr txid
  -> Property
combinedStateInvariant policy strength peeraddr peerState sharedState =
  conjoin
    [ peerTxLocalStateInvariant policy peerState
    , sharedTxStateInvariant strength sharedState
    , counterexample "advertised keys escape the peer's unacknowledged queue"
        (property (advertisedKeys `IntSet.isSubsetOf` unackKeys))
    , counterexample "advertised keys have no matching sharedTxTable entry"
        (property (advertisedKeys `IntSet.isSubsetOf` IntMap.keysSet (sharedTxTable sharedState)))
    ]
  where
    unackKeys =
      IntSet.fromList [ k | TxKey k <- toList (peerUnacknowledgedTxIds peerState) ]
    advertisedKeys =
      maybe IntSet.empty sharedPeerAdvertisedTxKeys
        (Map.lookup peeraddr (sharedPeers sharedState))



-- | 'InboundState` invariant.
--
sharedTxStateInvariant
  :: forall peeraddr txid.
     ( Ord peeraddr
     , Ord txid
     , HasRawTxId txid
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
      all (\(rawId, txKey) -> fmap getRawTxId (IntMap.lookup (unTxKey txKey) sharedKeyToTxId) == Just rawId)
          (Map.toList sharedTxIdToKey)

    keysRoundTripBackward =
      all (\(k, txid) -> Map.lookup (getRawTxId txid) sharedTxIdToKey == Just (TxKey k))
          (IntMap.toList sharedKeyToTxId)

    advertisersForKey k =
      Map.keysSet $
        Map.filter
          (\SharedPeerState { sharedPeerAdvertisedTxKeys } ->
             IntSet.member k sharedPeerAdvertisedTxKeys)
          sharedPeers

    activeEntryLive TxEntry { txLease, txAdvertiserCount, txAttempts } =
         leaseLive txLease
      || txAdvertiserCount > 0
      || not (Map.null txAttempts)

    leaseLive TxClaimable {} = False
    leaseLive TxLeased {}    = True

    checkTxEntry (k, txEntry@TxEntry { txLease, txAdvertiserCount, txAttempts }) =
      counterexample ("bad active tx entry " ++ show k ++ ": " ++ show txEntry) $
        let txAdvertisers = advertisersForKey k in
        conjoin
          [ property (txAdvertisers `Set.isSubsetOf` knownPeers)
          , txAdvertiserCount === Set.size txAdvertisers
          , property (Map.keysSet txAttempts `Set.isSubsetOf` txAdvertisers)
          , case txLease of
                 TxClaimable _ ->
                   property True
                 TxLeased owner _ ->
                   property (Map.member owner sharedPeers && Set.member owner txAdvertisers)
          ]

newtype ArbTxDecisionPolicy = ArbTxDecisionPolicy TxDecisionPolicy
  deriving Show

newtype ArbSharedTxState = ArbSharedTxState (SharedTxState PeerAddr TxId)
  deriving Show

newtype ArbSharedPeerState = ArbSharedPeerState SharedPeerState
  deriving Show

newtype ArbPeerTxLocalState = ArbPeerTxLocalState (PeerTxLocalState (Tx TxId))
  deriving Show

-- | Tag classifying how @handleReceivedTxIds@ should resolve each incoming
-- txid.
--
-- * 'TxIdNew' — fresh txid, hits the new-entry branch.
-- * 'TxIdRetained' — pre-seeded into 'sharedRetainedTxs', hits the retained
--   branch.
-- * 'TxIdMempool' — mempool-known and not previously in 'sharedTxTable',
--   hits the mempool branch with a @Nothing@ lookup.
-- * 'TxIdMempoolResolvesActive' — mempool-known and pre-seeded into
--   'sharedTxTable' as advertised by some other peer, hits the mempool
--   branch with a @Just@ lookup and therefore triggers
--   'removeAdvertisingPeersForResolvedTxExcept' + peer wake-up. Production
--   only hits this when a tx reached the mempool via the local
--   tx-submission interface after remote peers had already advertised the
--   txid, so it is deliberately rare in the generator.
data TxIdGroupTag
  = TxIdNew
  | TxIdRetained
  | TxIdMempool
  | TxIdMempoolResolvesActive
  deriving (Eq, Ord, Show)

instance Arbitrary TxIdGroupTag where
  arbitrary = frequency
    [ (12, pure TxIdNew)
    , (4,  pure TxIdRetained)
    , (4,  pure TxIdMempool)
    , (1,  pure TxIdMempoolResolvesActive)
    ]

-- | Per-requested-txid fate, driving the coherent pre-state for
-- 'handleReceivedTxs' properties:
--
-- * 'RfBuffered': body is in the reply, entry is active with peeraddr's
--   TxDownloading attempt. Expected: attempt flipped to TxBuffered, body
--   added to peerDownloadedTxs. @Bool@ = co-advertised by another peer.
-- * 'RfOmitted': body is not in the reply, entry is active with peeraddr's
--   TxDownloading attempt. Expected: lease released; entry survives if
--   co-advertised, otherwise reaped by dropDeadActiveKeys. @Bool@ =
--   co-advertised.
-- * 'RfLateRetained': body is in the reply, but the key is already in
--   sharedRetainedTxs (no sharedTxTable entry). Expected: body dropped,
--   lateCount incremented, no state change beyond the usual peer bookkeeping.
-- * 'RfLateMempool': body is in the reply, entry is active with peeraddr's
--   TxDownloading attempt, and the callback reports the tx as already in
--   the mempool. Expected: body dropped, entry moved from sharedTxTable
--   to sharedRetainedTxs, advertising stripped, any other advertiser woken.
-- * 'RfOmittedPruned': body is not in the reply, and the key is not in
--   sharedState at all (fully pruned by some concurrent cleanup before
--   the reply arrives). Expected: omittedCount incremented, no shared-state
--   change (@keyWasLive@ is False so 'handleOmitted' takes the count-only
--   branch).
data RequestedFate
  = RfBuffered     !Bool
  | RfOmitted      !Bool
  | RfLateRetained
  | RfLateMempool
  | RfOmittedPruned
  deriving (Eq, Show)

instance Arbitrary RequestedFate where
  arbitrary = frequency
    [ (4, pure (RfBuffered False))
    , (2, pure (RfBuffered True))
    , (3, pure (RfOmitted  False))
    , (2, pure (RfOmitted  True))
    , (1, pure RfLateRetained)
    , (1, pure RfLateMempool)
    , (1, pure RfOmittedPruned)
    ]

rfInReply :: RequestedFate -> Bool
rfInReply RfBuffered{}    = True
rfInReply RfOmitted{}     = False
rfInReply RfLateRetained  = True
rfInReply RfLateMempool   = True
rfInReply RfOmittedPruned = False

rfCoAdvertised :: RequestedFate -> Bool
rfCoAdvertised (RfBuffered c) = c
rfCoAdvertised (RfOmitted  c) = c
rfCoAdvertised _              = False

rfGoesToActive :: RequestedFate -> Bool
rfGoesToActive RfBuffered{}    = True
rfGoesToActive RfOmitted{}     = True
rfGoesToActive RfLateMempool   = True
rfGoesToActive RfLateRetained  = False
rfGoesToActive RfOmittedPruned = False

rfIsPruned :: RequestedFate -> Bool
rfIsPruned RfOmittedPruned = True
rfIsPruned _               = False

instance Arbitrary ArbTxDecisionPolicy where
    arbitrary =
      frequency
        [ (1, pure (ArbTxDecisionPolicy defaultTxDecisionPolicy))
        , (9, do
            interTxSpaceVal <- realToFrac <$> choose (0 :: Double, 1)
            offset          <- choose (0.01 :: Double, 10)
            let inflightTimeoutVal = interTxSpaceVal + realToFrac offset
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
                <*> pure interTxSpaceVal
                <*> pure inflightTimeoutVal))
        ]

    shrink (ArbTxDecisionPolicy a)
      | a == defaultTxDecisionPolicy = []
      | otherwise =
          ArbTxDecisionPolicy defaultTxDecisionPolicy
        : [ ArbTxDecisionPolicy a { maxNumTxIdsToRequest = NumTxIdsToReq x }
          | (Positive (Small x)) <- shrink (Positive (Small (getNumTxIdsToReq (maxNumTxIdsToRequest a))))
          ]
        ++ [ ArbTxDecisionPolicy a { maxUnacknowledgedTxIds = x }
           | (Positive (Small x)) <- shrink (Positive (Small (maxUnacknowledgedTxIds a)))
           ]
        ++ [ ArbTxDecisionPolicy a { txsSizeInflightPerPeer = SizeInBytes s }
           | Positive s <- shrink (Positive (getSizeInBytes (txsSizeInflightPerPeer a)))
           ]
        ++ [ ArbTxDecisionPolicy a { maxOutstandingTxBatchesPerPeer = x }
           | x <- shrink (maxOutstandingTxBatchesPerPeer a), x >= 1
           ]
        ++ [ ArbTxDecisionPolicy a { txInflightMultiplicity = x }
           | Positive (Small x) <- shrink (Positive (Small (txInflightMultiplicity a)))
           ]
        ++ [ ArbTxDecisionPolicy a { bufferedTxsMinLifetime = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (bufferedTxsMinLifetime a) :: Double))
           ]
        ++ [ ArbTxDecisionPolicy a { scoreRate = x }
           | NonNegative x <- shrink (NonNegative (scoreRate a))
           ]
        ++ [ ArbTxDecisionPolicy a { scoreMax = x }
           | NonNegative x <- shrink (NonNegative (scoreMax a))
           ]
        ++ [ ArbTxDecisionPolicy a { interTxSpace = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (interTxSpace a) :: Double))
           ]
        ++ [ ArbTxDecisionPolicy a { inflightTimeout = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (inflightTimeout a) :: Double))
           , realToFrac x > interTxSpace a
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
      defaultPeerState = mkSharedPeerState PeerIdle

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


-- Verifies that handleReceivedTxIds resolves each incoming txid according to
-- its state:
--
--   * 'TxIdNew' — new claimable entry for @peeraddr@ in sharedTxTable.
--   * 'TxIdRetained' — queued locally only; shared state unchanged.
--   * 'TxIdMempool' — interned and added to sharedRetainedTxs.
--   * 'TxIdMempoolResolvesActive' — active entry (advertised by another
--     peer) is removed from sharedTxTable, moved to sharedRetainedTxs, the
--     other peer's advertising for the key is cleared, and (if idle) its
--     generation is bumped.
--
-- Also asserts the peer's pre-existing state and unrelated shared state are
-- preserved, and that the combined invariant holds before and after.
prop_handleReceivedTxIds
  :: ArbTxDecisionPolicy
  -> ArbSharedTxState
  -> ArbPeerTxLocalState
  -> NonEmptyList (TxId, Positive Int, TxIdGroupTag)
  -> Positive Int
  -> Property
prop_handleReceivedTxIds
    (ArbTxDecisionPolicy policy)
    (ArbSharedTxState sharedState0)
    (ArbPeerTxLocalState peerStateGenerated)
    (NonEmpty taggedInput)
    (Positive extraRequested) =
  forAll (genPeerAddrBiased sharedState0) $ \peeraddr ->
  let sharedStateWithPeer = ensurePeerAdvertisesTxKeys peeraddr [] sharedState0

      -- Canonicalize input: normalise each (txid, size), then dedupe by txid
      -- while preserving the first-seen tag.
      dedupedTagged :: [((TxId, SizeInBytes), TxIdGroupTag)]
      dedupedTagged =
        nubBy ((==) `on` (fst . fst))
          [ ((abs txid + 1, mkSize txSize), tag)
          | (txid, txSize, tag) <- taggedInput
          ]

      -- Shift all txids forward so they are disjoint from sharedStateWithPeer's
      -- intern table and from each other. Preserves input order and tag mapping.
      freshenedTxids :: [(TxId, SizeInBytes)]
      freshenedTxids =
        freshBatchAgainstSharedState sharedStateWithPeer (fmap fst dedupedTagged)

      taggedFreshened :: [((TxId, SizeInBytes), TxIdGroupTag)]
      taggedFreshened = zip freshenedTxids (fmap snd dedupedTagged)

      (newGroup, retainedGroup, mempoolFreshGroup, resolveActiveCandidates) =
        foldr partitionByTag ([], [], [], []) taggedFreshened
        where
          partitionByTag (e, TxIdNew)                  (n, r, m, a) = (e:n, r, m, a)
          partitionByTag (e, TxIdRetained)             (n, r, m, a) = (n, e:r, m, a)
          partitionByTag (e, TxIdMempool)              (n, r, m, a) = (n, r, e:m, a)
          partitionByTag (e, TxIdMempoolResolvesActive) (n, r, m, a) = (n, r, m, e:a)

      -- Seed the retained group into the shared state first: intern the txids
      -- and add them to sharedRetainedTxs.
      sharedStateWithRetained = seedRetainedTxids policy retainedGroup sharedStateWithPeer

      -- Pick an advertiser peer for the resolve-active sub-group, if any peer
      -- other than @peeraddr@ exists. If none is available, demote the
      -- resolve-active candidates to fresh mempool entries so they still
      -- exercise the mempool branch.
      otherPeerOpt :: Maybe PeerAddr
      otherPeerOpt =
        case filter (/= peeraddr)
               (Map.keys (sharedPeers sharedStateWithRetained)) of
          []    -> Nothing
          (p:_) -> Just p

      (mempoolResolveActiveGroup, mempoolGroup, sharedStateBase) =
        case otherPeerOpt of
          Just p | not (null resolveActiveCandidates) ->
            ( resolveActiveCandidates
            , mempoolFreshGroup
            , seedActiveTxidsForOtherPeer p resolveActiveCandidates
                                          sharedStateWithRetained
            )
          _ ->
            ( []
            , mempoolFreshGroup ++ resolveActiveCandidates
            , sharedStateWithRetained
            )

      -- The full input list, in original (interleaved) order.
      txidsAndSizes :: [(TxId, SizeInBytes)]
      txidsAndSizes = freshenedTxids

      mempoolTxidSet :: Set.Set TxId
      mempoolTxidSet =
        Set.fromList
          (fmap fst mempoolGroup ++ fmap fst mempoolResolveActiveGroup)
      mempoolHasTx :: TxId -> Bool
      mempoolHasTx = (`Set.member` mempoolTxidSet)

      oldKeys = IntMap.keysSet (sharedKeyToTxId sharedStateBase)
      -- Keys that are in sharedStateBase's intern table AND whose sharedTxTable
      -- entry is about to be removed by the mempool branch. We exclude them
      -- from the "unchanged at old keys" assertions; their behaviour is
      -- covered explicitly by checkMempoolResolveActiveEntry.
      resolveActiveKeySet :: IntSet.IntSet
      resolveActiveKeySet =
        IntSet.fromList
          [ unTxKey (lookupKeyOrFail txid sharedStateBase)
          | (txid, _) <- mempoolResolveActiveGroup
          ]
      stableOldKeys = oldKeys `IntSet.difference` resolveActiveKeySet
      requestedToReply = fromIntegral (length txidsAndSizes)
      -- Shift generated peer-local keys past everything that
      -- handleReceivedTxIds touches, so the pre-existing peer-local keys stay
      -- disjoint from both the base state and the newly-allocated keys.
      peerKeyShift = sharedNextTxKey sharedStateBase + length txidsAndSizes
      preExistingAdvertised =
        sharedPeerAdvertisedTxKeys (lookupPeerOrFail peeraddr sharedStateBase)
      advertisedPrefix =
        StrictSeq.fromList [ TxKey k | k <- IntSet.toList preExistingAdvertised ]
      peerStateShifted = shiftPeerTxLocalStateKeys peerKeyShift peerStateGenerated
      peerState0 =
        peerStateShifted {
          peerUnacknowledgedTxIds =
            advertisedPrefix <> peerUnacknowledgedTxIds peerStateShifted,
          peerRequestedTxIds = requestedToReply + fromIntegral extraRequested
        }
      oldPeerAvailableKeys = IntMap.keysSet (peerAvailableTxIds peerState0)
      (peerState', sharedState') =
        handleReceivedTxIds mempoolHasTx now policy peeraddr requestedToReply txidsAndSizes peerState0 sharedStateBase

      expectedRetainUntil =
        addTime (bufferedTxsMinLifetime policy) now

      -- Only the new group extends the peer's advertised-key set.
      expectedAdvertisedKeys =
        preExistingAdvertised
          `IntSet.union`
        IntSet.fromList
          [ unTxKey (lookupKeyOrFail txid sharedState')
          | (txid, _) <- newGroup
          ]

      -- Keys newly interned during the call (new + mempool-fresh). Retained
      -- and resolve-active keys were pre-interned by the seed helpers.
      expectedNextTxKeyAdvance = length newGroup + length mempoolGroup

      -- The generation bumps iff the call actually changed shared state. Pure
      -- retained-only input leaves shared state untouched.
      expectedGenerationAdvance :: Word64
      expectedGenerationAdvance
        | null newGroup && null mempoolGroup && null mempoolResolveActiveGroup = 0
        | otherwise                                                            = 1

      -- Peers whose entry may differ between sharedStateBase and sharedState':
      -- always @peeraddr@, and also the chosen advertiser if the mempool
      -- branch resolved any active entry.
      affectedPeers :: Set.Set PeerAddr
      affectedPeers =
        Set.insert peeraddr $
          case otherPeerOpt of
            Just p | not (null mempoolResolveActiveGroup) -> Set.singleton p
            _                                             -> Set.empty

      checkNewEntry (txid, size) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        case IntMap.lookup k (sharedTxTable sharedState') of
          Nothing ->
            counterexample ("missing new tx entry for " ++ show txid) (property False)
          Just TxEntry { txLease, txAdvertiserCount, txAttempts } ->
            conjoin
              [ txLease === TxClaimable now
              , txAdvertiserCount === 1
              , txAttempts === (Map.empty :: Map.Map PeerAddr TxAttemptState)
              , counterexample "new txid missing from peerAvailableTxIds"
                  (IntMap.lookup k (peerAvailableTxIds peerState') === Just size)
              , counterexample "new txid missing from peer advertised keys"
                  (property (IntSet.member k
                              (sharedPeerAdvertisedTxKeys
                                (lookupPeerOrFail peeraddr sharedState'))))
              ]

      checkRetainedEntry (txid, _) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        conjoin
          [ counterexample "retained txid appears in sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "retained txid disappeared from sharedRetainedTxs"
              (property (retainedMember k (sharedRetainedTxs sharedState')))
          , counterexample "retained txid leaked into peerAvailableTxIds"
              (property (IntMap.notMember k (peerAvailableTxIds peerState')))
          , counterexample "retained txid leaked into peer advertised keys"
              (property (IntSet.notMember k
                          (sharedPeerAdvertisedTxKeys
                            (lookupPeerOrFail peeraddr sharedState'))))
          ]

      checkMempoolEntry (txid, _) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        conjoin
          [ counterexample "mempool txid leaked into sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "mempool txid missing or wrong retain-until in sharedRetainedTxs"
              (retainedLookup k (sharedRetainedTxs sharedState')
                 === Just expectedRetainUntil)
          , counterexample "mempool txid leaked into peerAvailableTxIds"
              (property (IntMap.notMember k (peerAvailableTxIds peerState')))
          , counterexample "mempool txid leaked into peer advertised keys"
              (property (IntSet.notMember k
                          (sharedPeerAdvertisedTxKeys
                            (lookupPeerOrFail peeraddr sharedState'))))
          ]

      -- A resolve-active entry was in sharedTxTable (advertised by an "other"
      -- peer) before the call. The mempool branch deletes it from
      -- sharedTxTable, inserts it into sharedRetainedTxs, and clears the
      -- "other" peer's advertising for that key. peeraddr never advertised
      -- the key (keys are freshened out of peeraddr's advertised range).
      checkMempoolResolveActiveEntry (txid, _) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        conjoin
          [ counterexample "resolve-active txid remained in sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "resolve-active txid missing or wrong retain-until"
              (retainedLookup k (sharedRetainedTxs sharedState')
                 === Just expectedRetainUntil)
          , counterexample "resolve-active txid leaked into peerAvailableTxIds"
              (property (IntMap.notMember k (peerAvailableTxIds peerState')))
          , counterexample "resolve-active txid leaked into peer advertised keys"
              (property (IntSet.notMember k
                          (sharedPeerAdvertisedTxKeys
                            (lookupPeerOrFail peeraddr sharedState'))))
          , case otherPeerOpt of
              Nothing -> property True
              Just op ->
                counterexample "other advertiser still lists resolve-active key"
                  (property (IntSet.notMember k
                              (sharedPeerAdvertisedTxKeys
                                (lookupPeerOrFail op sharedState'))))
          ]

      -- If the resolve-active group is non-empty, the chosen @otherPeer@'s
      -- post-call state should have the resolve-active keys stripped from its
      -- advertising (reverting to what it advertised in sharedStateWithRetained)
      -- and its generation bumped by 1 iff its phase is PeerIdle.
      checkOtherPeerState =
        case otherPeerOpt of
          Just op | not (null mempoolResolveActiveGroup) ->
            let original = lookupPeerOrFail op sharedStateWithRetained
                post     = lookupPeerOrFail op sharedState' in
            conjoin
                 [ counterexample "other peer's advertised keys not restored"
                     (sharedPeerAdvertisedTxKeys post
                        === sharedPeerAdvertisedTxKeys original)
                 , counterexample "other peer's phase changed unexpectedly"
                     (sharedPeerPhase post === sharedPeerPhase original)
                 , counterexample "other peer's generation bump mismatch"
                     (sharedPeerGeneration post
                        === sharedPeerGeneration original + 1)
                 ]
          _ -> property True

      checkExistingTxId (rawId, txKey) =
        Map.lookup rawId (sharedTxIdToKey sharedState') === Just txKey in
    classify (StrictSeq.null (peerUnacknowledgedTxIds peerStateGenerated))
             "generated peer-local state: empty unacknowledged queue" $
    classify (not (Map.member peeraddr (sharedPeers sharedState0)))
             "peeraddr: fresh (not in generated sharedState)" $
    classify (not (IntSet.null preExistingAdvertised))
             "peeraddr: has pre-existing advertised keys" $
    classify (length txidsAndSizes /= length taggedInput)
             "received txids: reduced by dedupe or fresh-shift" $
    classify (not (null newGroup))                   "txids include new"              $
    classify (not (null retainedGroup))              "txids include retained"         $
    classify (not (null mempoolGroup))               "txids include mempool"          $
    classify (not (null mempoolResolveActiveGroup))  "txids include resolve-active"   $
    tabulate "received txids"       [bucket (length txidsAndSizes)]                         $
    tabulate "new group"            [bucket (length newGroup)]                              $
    tabulate "retained group"       [bucket (length retainedGroup)]                         $
    tabulate "mempool group"        [bucket (length mempoolGroup)]                          $
    tabulate "resolve-active group" [bucket (length mempoolResolveActiveGroup)]             $
    tabulate "sharedState peers"    [bucket (Map.size (sharedPeers sharedStateBase))]       $
    tabulate "active txs"           [bucket (IntMap.size (sharedTxTable sharedStateBase))]  $
    tabulate "retained txs"         [bucket (retainedSize (sharedRetainedTxs sharedStateBase))] $
    conjoin
      [ peerRequestedTxIds peerState' === fromIntegral extraRequested
      , toList (peerUnacknowledgedTxIds peerState')
          === toList (peerUnacknowledgedTxIds peerState0)
                ++ fmap (\(txid, _) -> lookupKeyOrFail txid sharedState') txidsAndSizes
      , IntMap.size (peerAvailableTxIds peerState')
          === IntMap.size (peerAvailableTxIds peerState0) + length newGroup
      , IntMap.restrictKeys (peerAvailableTxIds peerState') oldPeerAvailableKeys
          === peerAvailableTxIds peerState0
      , peerRequestedTxs peerState' === peerRequestedTxs peerState0
      , peerRequestedTxBatches peerState' === peerRequestedTxBatches peerState0
      , peerRequestedTxsSize peerState' === peerRequestedTxsSize peerState0
      , peerDownloadedTxs peerState' === peerDownloadedTxs peerState0
      , peerDownloadStartTime peerState' === peerDownloadStartTime peerState0
      , peerScore peerState' === peerScore peerState0
      , Map.withoutKeys (sharedPeers sharedState') affectedPeers
          === Map.withoutKeys (sharedPeers sharedStateBase) affectedPeers
      , sharedPeerAdvertisedTxKeys (lookupPeerOrFail peeraddr sharedState')
          === expectedAdvertisedKeys
      , IntMap.restrictKeys (sharedTxTable sharedState') stableOldKeys
          === IntMap.restrictKeys (sharedTxTable sharedStateBase) stableOldKeys
      , retainedRestrictKeys (sharedRetainedTxs sharedState') stableOldKeys
          === retainedRestrictKeys (sharedRetainedTxs sharedStateBase) stableOldKeys
      , IntMap.restrictKeys (sharedKeyToTxId sharedState') oldKeys
          === sharedKeyToTxId sharedStateBase
      , sharedGeneration sharedState'
          === sharedGeneration sharedStateBase + expectedGenerationAdvance
      , sharedNextTxKey sharedState'
          === sharedNextTxKey sharedStateBase + expectedNextTxKeyAdvance
      , conjoin (fmap checkExistingTxId (Map.toList (sharedTxIdToKey sharedStateBase)))
      , conjoin (fmap checkNewEntry newGroup)
      , conjoin (fmap checkRetainedEntry retainedGroup)
      , conjoin (fmap checkMempoolEntry mempoolGroup)
      , conjoin (fmap checkMempoolResolveActiveEntry mempoolResolveActiveGroup)
      , checkOtherPeerState
      , counterexample "combined invariant violated before the call"
          (combinedStateInvariant policy StrongInvariant peeraddr peerState0 sharedStateBase)
      , counterexample "combined invariant violated after the call"
          (combinedStateInvariant policy StrongInvariant peeraddr peerState' sharedState')
      , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
      , checkNoThunks "sharedState'" sharedState'
      ]

unit_handleReceivedTxIds_addsAdvertiserForActiveTxs :: (String -> IO ()) -> Assertion
unit_handleReceivedTxIds_addsAdvertiserForActiveTxs step = do
  step "Run handleReceivedTxIds for a peer advertising txids that are already active via other peers"
  let (peerState', sharedState') =
        handleReceivedTxIds
          (const False)
          now
          defaultTxDecisionPolicy
          peeraddr
          requestedTxIds
          txidsAndSizes
          peerState0
          sharedState0
  step "Assert the local peer now tracks the txids as unacknowledged and available"
  toList (peerUnacknowledgedTxIds peerState') @?= txKeys
  peerAvailableTxIds peerState' @?= expectedAvailableTxs
  step "Assert the shared peer view and advertiser counts were updated once per tx"
  sharedPeerAdvertisedTxKeys (lookupPeerOrFail peeraddr sharedState') @?= expectedAdvertisedKeys
  map (txAdvertiserCount . (`lookupEntryOrFail` sharedState')) txKeys
    @?= map ((+ 1) . txAdvertiserCount . (`lookupEntryOrFail` sharedState0)) txKeys
  step "Assert unrelated peers and shared mappings are unchanged apart from the generation bump"
  Map.delete peeraddr (sharedPeers sharedState') @?= Map.delete peeraddr (sharedPeers sharedState0)
  sharedTxIdToKey sharedState' @?= sharedTxIdToKey sharedState0
  sharedKeyToTxId sharedState' @?= sharedKeyToTxId sharedState0
  sharedGeneration sharedState' @?= sharedGeneration sharedState0 + 1
  where
    ReceiveDuplicateFixture
      { rdfPeerAddr = peeraddr
      , rdfRequestedTxIds = requestedTxIds
      , rdfTxidsAndSizes = txidsAndSizes
      , rdfPeerState = peerState0
      , rdfSharedState = sharedState0
      } = mkReceiveDuplicateFixture 4 3

    txKeys = fmap (`lookupKeyOrFail` sharedState0) (fmap fst txidsAndSizes)
    expectedAvailableTxs =
      IntMap.fromList
        [ (unTxKey txKey, txSize)
        | (txid, txSize) <- txidsAndSizes
        , let txKey = lookupKeyOrFail txid sharedState0
        ]
    expectedAdvertisedKeys = IntSet.fromList (map unTxKey txKeys)

unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull step = do
  step "Receive a fresh txid from peer A while A is already at its inflight size limit"
  let (peerAState1, sharedState1) =
        handleReceivedTxIds
          (const False)
          now
          defaultTxDecisionPolicy
          peerA
          requestedToReply
          [(txid, txSize)]
          peerAState0
          sharedState0
  txLease (lookupEntryOrFail key sharedState1) @?= TxClaimable now
  let (peerAAction, _, _) =
        nextPeerAction now defaultTxDecisionPolicy peerA peerAState1 sharedState1
  step "Run nextPeerAction for peer A and confirm the fresh tx remains unclaimed because A is full"
  case peerAAction of
       PeerDoNothing _ _ -> pure ()
       other ->
         assertFailure ("unexpected peer A action: " ++ show other)
  step "Receive the same txid from peer B and run nextPeerAction for B"
  let (peerBState1, sharedState2) =
        handleReceivedTxIds
          (const False)
          now
          defaultTxDecisionPolicy
          peerB
          requestedToReply
          [(txid, txSize)]
          peerBState0
          sharedState1
      (peerBAction, peerBState2, sharedState3) =
        nextPeerAction now defaultTxDecisionPolicy peerB peerBState1 sharedState2
  case peerBAction of
       PeerRequestTxs txKeys -> do
         step "Assert peer B can claim and request the fresh tx immediately"
         txKeys @?= [key]
         peerRequestedTxs peerBState2 @?= IntSet.singleton k
         txLease (lookupEntryOrFail key sharedState3) @?=
           TxLeased peerB (addTime (interTxSpace defaultTxDecisionPolicy) now)
       other ->
         assertFailure ("unexpected peer B action: " ++ show other)
  where
    peerA = 7 :: PeerAddr
    peerB = 8 :: PeerAddr
    txid = 1
    txSize = mkSize (Positive 10)
    requestedToReply = 1
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peerA, mkSharedPeerState PeerIdle)
          , (peerB, mkSharedPeerState PeerIdle)
          ]
      }
    peerAState0 = emptyPeerTxLocalState
      { peerRequestedTxIds = requestedToReply
      , peerRequestedTxsSize = txsSizeInflightPerPeer defaultTxDecisionPolicy
      }
    peerBState0 = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply }

-- Verifies that handleReceivedTxs resolves each requested txid according to
-- its 'RequestedFate': 'RfBuffered' flips the attempt to 'TxBuffered' and
-- inserts the body; 'RfOmitted' releases peeraddr's lease with the entry
-- surviving iff co-advertised; 'RfLateRetained' drops the body and counts
-- a late reply with no further state change; 'RfLateMempool' drops the
-- body, moves the active entry to sharedRetainedTxs, and strips advertising;
-- 'RfOmittedPruned' counts a penalty even though the key has already been
-- fully pruned from shared state. Aggregate 'omittedCount' and 'lateCount'
-- match the group sizes, peer-local fields handleReceivedTxs does not touch
-- are preserved, and the combined invariant holds before and after.
prop_handleReceivedTxs
  :: ArbTxDecisionPolicy
  -> ArbSharedTxState
  -> ArbPeerTxLocalState
  -> NonEmptyList (TxId, Positive Int, RequestedFate)
  -> NonNegative Double
  -> Property
prop_handleReceivedTxs
    (ArbTxDecisionPolicy policy)
    (ArbSharedTxState sharedState0)
    (ArbPeerTxLocalState peerStateGenerated)
    (NonEmpty requestedInput)
    (NonNegative initialScore) =
  forAll (genPeerAddrBiased sharedState0) $ \peeraddr ->
  let sharedStateWithPeer = ensurePeerAdvertisesTxKeys peeraddr [] sharedState0

      dedupedTagged :: [((TxId, SizeInBytes), RequestedFate)]
      dedupedTagged =
        nubBy ((==) `on` (fst . fst))
          [ ((abs txid + 1, mkSize txSize), fate)
          | (txid, txSize, fate) <- requestedInput
          ]

      freshenedTxids :: [(TxId, SizeInBytes)]
      freshenedTxids =
        freshBatchAgainstSharedState sharedStateWithPeer (fmap fst dedupedTagged)

      taggedFreshened :: [((TxId, SizeInBytes), RequestedFate)]
      taggedFreshened = zip freshenedTxids (fmap snd dedupedTagged)

      otherPeerOpt :: Maybe PeerAddr
      otherPeerOpt =
        case filter (/= peeraddr) (Map.keys (sharedPeers sharedStateWithPeer)) of
          []    -> Nothing
          (p:_) -> Just p

      -- Partition by fate.
      bufferedGroup, omittedGroup, lateRetainedGroup, lateMempoolGroup, prunedGroup
        :: [((TxId, SizeInBytes), RequestedFate)]
      bufferedGroup     = [ e | e@(_, RfBuffered{})    <- taggedFreshened ]
      omittedGroup      = [ e | e@(_, RfOmitted{})     <- taggedFreshened ]
      lateRetainedGroup = [ e | e@(_, RfLateRetained)  <- taggedFreshened ]
      lateMempoolGroup  = [ e | e@(_, RfLateMempool)   <- taggedFreshened ]
      prunedGroup       = [ e | e@(_, RfOmittedPruned) <- taggedFreshened ]

      -- Entries that need an active sharedTxTable seed, tagged with whether
      -- the entry is co-advertised by otherPeer. LateMempool keys seed an
      -- active entry (single advertiser) so the mempool branch of handleOne
      -- hits the Just lookup and fires removeAdvertisingPeersForResolvedTx.
      activeSeedTagged :: [((TxId, SizeInBytes), Bool)]
      activeSeedTagged =
        [ (fst e, rfCoAdvertised (snd e))
        | e <- taggedFreshened
        , rfGoesToActive (snd e)
        ]

      -- Seed retained first (no sharedTxTable entry, no advertising), then
      -- active on top (leased to peeraddr with a TxDownloading attempt, plus
      -- otherPeer advertising for co-advertised ones). Pruned entries are
      -- deliberately not seeded: their keys live only in the peer's local
      -- bookkeeping.
      sharedStateWithLateRetained =
        seedRetainedTxids policy (fmap fst lateRetainedGroup) sharedStateWithPeer
      sharedStateBase =
        seedRequestedActiveTxids peeraddr otherPeerOpt activeSeedTagged
                                 sharedStateWithLateRetained

      -- Synthetic keys for pruned entries, chosen to land above every key
      -- that 'sharedStateBase' already uses so they don't collide with
      -- anything interned. These keys never appear in any shared-state map.
      prunedAllocations :: [(((TxId, SizeInBytes), RequestedFate), Int)]
      prunedAllocations =
        zip prunedGroup [ sharedNextTxKey sharedStateBase + i
                        | i <- [0 .. length prunedGroup - 1] ]

      -- Resolve each tagged entry to an Int key: interned entries look their
      -- key up in sharedStateBase; pruned entries use their synthetic key.
      prunedKeyByTxId :: Map.Map TxId Int
      prunedKeyByTxId =
        Map.fromList [ (txid, k) | (((txid, _), _), k) <- prunedAllocations ]

      keyIntOf :: ((TxId, SizeInBytes), RequestedFate) -> Int
      keyIntOf ((txid, _), fate)
        | rfIsPruned fate = prunedKeyByTxId Map.! txid
        | otherwise       = unTxKey (lookupKeyOrFail txid sharedStateBase)

      -- All requested keys, in input order.
      requestedKeyInts :: [Int]
      requestedKeyInts = [ keyIntOf e | e <- taggedFreshened ]
      requestedKeys :: [TxKey]
      requestedKeys = fmap TxKey requestedKeyInts
      requestedKeysSet :: IntSet.IntSet
      requestedKeysSet = IntSet.fromList requestedKeyInts
      requestedAvailableMap :: IntMap.IntMap SizeInBytes
      requestedAvailableMap =
        IntMap.fromList (zip requestedKeyInts (fmap (snd . fst) taggedFreshened))
      requestedTotalSize :: SizeInBytes
      requestedTotalSize = sum (fmap (snd . fst) taggedFreshened)
      requestedBatch = mkRequestedTxBatch requestedKeys requestedTotalSize

      -- Keys genuinely co-advertised by otherPeer (only Buffered/Omitted tags
      -- can be co-advertised and only if otherPeerOpt is Just).
      coAdvertisedKeys :: IntSet.IntSet
      coAdvertisedKeys =
        IntSet.fromList
          [ keyIntOf e
          | e@(_, fate) <- taggedFreshened
          , rfCoAdvertised fate
          , isJust otherPeerOpt
          ]

      -- Omitted entries that survive (co-advertised) vs get reaped (solo).
      omittedSurvivingKeys, omittedReapedKeys, lateMempoolKeys :: IntSet.IntSet
      omittedSurvivingKeys =
        IntSet.fromList [ keyIntOf e | e <- omittedGroup, keyIntOf e `IntSet.member` coAdvertisedKeys ]
      omittedReapedKeys =
        IntSet.fromList [ keyIntOf e | e <- omittedGroup, not (keyIntOf e `IntSet.member` coAdvertisedKeys) ]
      lateMempoolKeys =
        IntSet.fromList [ keyIntOf e | e <- lateMempoolGroup ]

      -- mempoolHasTx returns True exactly for the LateMempool group's txids.
      mempoolTxidSet :: Set.Set TxId
      mempoolTxidSet = Set.fromList (fmap (fst . fst) lateMempoolGroup)
      mempoolHasTxFn :: TxId -> Bool
      mempoolHasTxFn = (`Set.member` mempoolTxidSet)

      -- Body list submitted to handleReceivedTxs: every in-reply entry.
      receivedBodies :: [(TxId, Tx TxId)]
      receivedBodies =
        [ (txid, mkTx txid size)
        | ((txid, size), fate) <- taggedFreshened
        , rfInReply fate
        ]

      -- Peer-local state. Shift the generator's keys out of range, then
      -- prepend advertised-from-sharedState0 keys plus our requested keys to
      -- the unack queue, and overwrite the request bookkeeping with a single
      -- batch of our requested keys (handleReceivedTxs only processes the
      -- head batch). The shift must also land above the pruned keys, which
      -- were allocated starting at 'sharedNextTxKey sharedStateBase'.
      peerKeyShift =
        sharedNextTxKey sharedStateBase + length prunedGroup + 1
      preExistingAdvertised =
        sharedPeerAdvertisedTxKeys
          (lookupPeerOrFail peeraddr sharedStateWithPeer)
      unackPrefix :: [TxKey]
      unackPrefix =
        [ TxKey k | k <- IntSet.toList preExistingAdvertised ]
        ++ requestedKeys
      peerStateShifted = shiftPeerTxLocalStateKeys peerKeyShift peerStateGenerated
      peerState0 = peerStateShifted {
          peerUnacknowledgedTxIds =
            StrictSeq.fromList unackPrefix
              <> peerUnacknowledgedTxIds peerStateShifted,
          peerAvailableTxIds     = requestedAvailableMap,
          peerRequestedTxs       = requestedKeysSet,
          peerRequestedTxBatches = StrictSeq.singleton requestedBatch,
          peerRequestedTxsSize   = requestedTotalSize,
          peerScore              = PeerScore (min initialScore (scoreMax policy))
                                             (Time 0)
        }

      (omittedCount, lateCount, peerState', sharedState') =
        handleReceivedTxs mempoolHasTxFn now policy peeraddr
                          receivedBodies peerState0 sharedStateBase

      penaltyCount = omittedCount + lateCount
      peerState''  | penaltyCount == 0 = peerState'
                   | otherwise         =
                       snd (applyPeerRejections policy now penaltyCount peerState')

      expectedRetainUntil =
        addTime (bufferedTxsMinLifetime policy) now

      -- Per-fate assertions.
      checkBufferedEntry ((txid, size), _) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        conjoin
          [ counterexample "buffered: peeraddr attempt not TxBuffered"
              (fmap (\TxEntry { txAttempts } -> Map.lookup peeraddr txAttempts)
                    (IntMap.lookup k (sharedTxTable sharedState'))
                 === Just (Just TxBuffered))
          , counterexample "buffered: body missing from peerDownloadedTxs"
              (fmap getTxId (IntMap.lookup k (peerDownloadedTxs peerState'))
                 === Just txid)
          , counterexample "buffered: body has wrong size"
              (fmap getTxSize (IntMap.lookup k (peerDownloadedTxs peerState'))
                 === Just size)
          ]

      checkOmittedSurvivingEntry ((txid, _), _) =
        let k = unTxKey (lookupKeyOrFail txid sharedState') in
        case IntMap.lookup k (sharedTxTable sharedState') of
          Nothing ->
            counterexample ("co-adv omitted entry was reaped for " ++ show txid)
                           (property False)
          Just TxEntry { txLease, txAdvertiserCount, txAttempts } ->
            conjoin
              [ counterexample "co-adv omitted: lease not demoted"
                  (txLease === TxClaimable now)
              , counterexample "co-adv omitted: advertiser count not decremented"
                  (txAdvertiserCount === 1)
              , counterexample "co-adv omitted: peeraddr attempt not cleared"
                  (property (Map.notMember peeraddr txAttempts))
              ]

      checkOmittedReapedEntry ((txid, _), _) =
        let k = unTxKey (lookupKeyOrFail txid sharedStateBase)
            rawId = getRawTxId txid
        in conjoin
             [ counterexample "reaped omitted: still in sharedTxTable"
                 (property (IntMap.notMember k (sharedTxTable sharedState')))
             , counterexample "reaped omitted: leaked into sharedRetainedTxs"
                 (property (not (retainedMember k (sharedRetainedTxs sharedState'))))
             , counterexample "reaped omitted: still in sharedKeyToTxId"
                 (property (IntMap.notMember k (sharedKeyToTxId sharedState')))
             , counterexample "reaped omitted: still in sharedTxIdToKey"
                 (property (Map.notMember rawId (sharedTxIdToKey sharedState')))
             ]

      -- LateRetained: sharedRetainedTxs entry untouched; no sharedTxTable
      -- entry exists or is created; body was dropped.
      checkLateRetainedEntry ((txid, _), _) =
        let k = unTxKey (lookupKeyOrFail txid sharedStateBase) in
        conjoin
          [ counterexample "late-retained: leaked into sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "late-retained: retain-until mismatch"
              (retainedLookup k (sharedRetainedTxs sharedState')
                 === Just expectedRetainUntil)
          , counterexample "late-retained: leaked into peerDownloadedTxs"
              (property (IntMap.notMember k (peerDownloadedTxs peerState')))
          ]

      -- LateMempool: active entry moved from sharedTxTable to
      -- sharedRetainedTxs; peeraddr's advertising stripped.
      checkLateMempoolEntry ((txid, _), _) =
        let k = unTxKey (lookupKeyOrFail txid sharedStateBase) in
        conjoin
          [ counterexample "late-mempool: still in sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "late-mempool: retain-until mismatch"
              (retainedLookup k (sharedRetainedTxs sharedState')
                 === Just expectedRetainUntil)
          , counterexample "late-mempool: leaked into peerDownloadedTxs"
              (property (IntMap.notMember k (peerDownloadedTxs peerState')))
          , counterexample "late-mempool: still in peer advertised keys"
              (property (IntSet.notMember k
                          (sharedPeerAdvertisedTxKeys
                            (lookupPeerOrFail peeraddr sharedState'))))
          ]

      -- Pruned: synthetic key was never interned; handleOmitted takes the
      -- count-only branch and leaves shared state untouched.
      checkPrunedEntry (((txid, _), _), k) =
        let rawId = getRawTxId txid in
        conjoin
          [ counterexample "pruned: leaked into sharedTxTable"
              (property (IntMap.notMember k (sharedTxTable sharedState')))
          , counterexample "pruned: leaked into sharedRetainedTxs"
              (property (not (retainedMember k (sharedRetainedTxs sharedState'))))
          , counterexample "pruned: leaked into sharedKeyToTxId"
              (property (IntMap.notMember k (sharedKeyToTxId sharedState')))
          , counterexample "pruned: leaked into sharedTxIdToKey"
              (property (Map.notMember rawId (sharedTxIdToKey sharedState')))
          , counterexample "pruned: leaked into peerDownloadedTxs"
              (property (IntMap.notMember k (peerDownloadedTxs peerState')))
          ]

      -- Expected peeraddr advertised keys post-call: pre-existing plus only
      -- the buffered group's keys. Omitted-group advertising is stripped by
      -- removeOmittedAdvertisedKeys; LateMempool advertising is stripped by
      -- removeAdvertisingPeersForResolvedTx; LateRetained never advertised.
      expectedPeerAdvertisedPost =
        preExistingAdvertised
          `IntSet.union`
        IntSet.fromList
          [ unTxKey (lookupKeyOrFail txid sharedState')
          | ((txid, _), _) <- bufferedGroup
          ]

      -- otherPeer is added to wakePeers when any surviving omitted entry
      -- exists; its generation is bumped unconditionally. LateMempool is
      -- set up single-advertiser so it does not wake otherPeer here.
      checkOtherPeerState =
        case otherPeerOpt of
          Just op | not (IntSet.null omittedSurvivingKeys) ->
            let original = lookupPeerOrFail op sharedStateBase
                post     = lookupPeerOrFail op sharedState' in
            conjoin
                 [ counterexample "other peer's advertised keys changed"
                     (sharedPeerAdvertisedTxKeys post
                        === sharedPeerAdvertisedTxKeys original)
                 , counterexample "other peer's phase changed"
                     (sharedPeerPhase post === sharedPeerPhase original)
                 , counterexample "other peer's generation bump mismatch"
                     (sharedPeerGeneration post
                        === sharedPeerGeneration original + 1)
                 ]
          _ -> property True

      affectedPeers :: Set.Set PeerAddr
      affectedPeers =
        Set.insert peeraddr $
          case otherPeerOpt of
            Just op | not (IntSet.null omittedSurvivingKeys) -> Set.singleton op
            _                                                -> Set.empty

      -- Key sets used for the "unchanged on stable old keys" assertions.
      -- Every requested key has its sharedTxTable slot touched (added,
      -- removed, or modified), so they are all excluded from stableForTxTable.
      -- sharedKeyToTxId only loses reaped keys; sharedRetainedTxs only gains
      -- LateMempool keys (and keeps the LateRetained pre-seed entries
      -- unchanged).
      oldKeys = IntMap.keysSet (sharedKeyToTxId sharedStateBase)
      stableForKeyMaps  = oldKeys `IntSet.difference` omittedReapedKeys
      stableForTxTable  = oldKeys `IntSet.difference` requestedKeysSet
      stableForRetained = oldKeys `IntSet.difference` lateMempoolKeys

      -- Generated peerDownloadedTxs keys (shifted) should survive untouched.
      genDownloadedKeys =
        IntMap.keysSet (peerDownloadedTxs peerStateShifted) in
    classify (StrictSeq.null (peerUnacknowledgedTxIds peerStateGenerated))
             "generated peer-local state: empty unacknowledged queue" $
    classify (not (Map.member peeraddr (sharedPeers sharedState0)))
             "peeraddr: fresh (not in generated sharedState)" $
    classify (not (IntSet.null coAdvertisedKeys))
             "requested txs include co-advertised" $
    classify (not (null bufferedGroup))     "requested txs include buffered"      $
    classify (not (null omittedGroup))      "requested txs include omitted"       $
    classify (not (IntSet.null omittedSurvivingKeys))
             "requested txs include omitted + surviving" $
    classify (not (IntSet.null omittedReapedKeys))
             "requested txs include omitted + reaped" $
    classify (not (null lateRetainedGroup)) "requested txs include late-retained" $
    classify (not (null lateMempoolGroup))  "requested txs include late-mempool"  $
    classify (not (null prunedGroup))       "requested txs include pruned"        $
    tabulate "requested"         [bucket (length freshenedTxids)]                         $
    tabulate "buffered"          [bucket (length bufferedGroup)]                          $
    tabulate "omitted"           [bucket (length omittedGroup)]                           $
    tabulate "late-retained"     [bucket (length lateRetainedGroup)]                      $
    tabulate "late-mempool"      [bucket (length lateMempoolGroup)]                       $
    tabulate "pruned"            [bucket (length prunedGroup)]                            $
    tabulate "sharedState peers" [bucket (Map.size (sharedPeers sharedStateBase))]        $
    conjoin
      [ omittedCount === length omittedGroup + length prunedGroup
      , lateCount    === length lateRetainedGroup + length lateMempoolGroup
      , peerRequestedTxs peerState' === IntSet.empty
      , peerRequestedTxBatches peerState' === StrictSeq.empty
      , peerRequestedTxsSize peerState' === 0
      , peerAvailableTxIds peerState' === IntMap.empty
      , peerUnacknowledgedTxIds peerState' === peerUnacknowledgedTxIds peerState0
      , peerRequestedTxIds peerState' === peerRequestedTxIds peerState0
      , peerDownloadStartTime peerState' === peerDownloadStartTime peerState0
      , peerScore peerState' === peerScore peerState0
      , counterexample "generated peerDownloadedTxs entries not preserved"
          (IntMap.restrictKeys (peerDownloadedTxs peerState') genDownloadedKeys
             === peerDownloadedTxs peerStateShifted)
      , counterexample "buffered bodies not all inserted into peerDownloadedTxs"
          (IntMap.restrictKeys (peerDownloadedTxs peerState')
             (IntSet.fromList
                [ unTxKey (lookupKeyOrFail txid sharedState')
                | ((txid, _), _) <- bufferedGroup
                ])
             === IntMap.fromList
                   [ (unTxKey (lookupKeyOrFail txid sharedState'), mkTx txid size)
                   | ((txid, size), _) <- bufferedGroup
                   ])
      , Map.withoutKeys (sharedPeers sharedState') affectedPeers
          === Map.withoutKeys (sharedPeers sharedStateBase) affectedPeers
      , sharedPeerAdvertisedTxKeys (lookupPeerOrFail peeraddr sharedState')
          === expectedPeerAdvertisedPost
      , IntMap.restrictKeys (sharedTxTable sharedState') stableForTxTable
          === IntMap.restrictKeys (sharedTxTable sharedStateBase) stableForTxTable
      , IntMap.restrictKeys (sharedKeyToTxId sharedState') stableForKeyMaps
          === IntMap.restrictKeys (sharedKeyToTxId sharedStateBase) stableForKeyMaps
      , retainedRestrictKeys (sharedRetainedTxs sharedState') stableForRetained
          === retainedRestrictKeys (sharedRetainedTxs sharedStateBase) stableForRetained
      , sharedGeneration sharedState' === sharedGeneration sharedStateBase + 1
      , conjoin (fmap checkBufferedEntry      bufferedGroup)
      , conjoin [ checkOmittedSurvivingEntry e
                | e <- omittedGroup
                , keyIntOf e `IntSet.member` coAdvertisedKeys
                ]
      , conjoin [ checkOmittedReapedEntry e
                | e <- omittedGroup
                , not (keyIntOf e `IntSet.member` coAdvertisedKeys)
                ]
      , conjoin (fmap checkLateRetainedEntry lateRetainedGroup)
      , conjoin (fmap checkLateMempoolEntry  lateMempoolGroup)
      , conjoin (fmap checkPrunedEntry       prunedAllocations)
      , checkOtherPeerState
      , counterexample "combined invariant violated before the call"
          (combinedStateInvariant policy StrongInvariant peeraddr peerState0 sharedStateBase)
      , counterexample "combined invariant violated after the call"
          (combinedStateInvariant policy StrongInvariant peeraddr peerState' sharedState')
      , counterexample "score path: peerScoreValue not as expected"
          (if penaltyCount == 0
              then peerScoreValue (peerScore peerState'')
                     === peerScoreValue (peerScore peerState0)
              else peerScoreValue (peerScore peerState'')
                     === min (scoreMax policy)
                             (currentPeerScore policy now (peerScore peerState0)
                                + fromIntegral penaltyCount))
      , counterexample "score path: peerScoreTs not advanced"
          (if penaltyCount == 0
              then peerScoreTs (peerScore peerState'')
                     === peerScoreTs (peerScore peerState0)
              else peerScoreTs (peerScore peerState'') === now)
      , counterexample "combined invariant violated after score update"
          (combinedStateInvariant policy StrongInvariant peeraddr peerState'' sharedState')
      , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
      , checkNoThunks "sharedState'" sharedState'
      ]

-- Verifies that handleSubmittedTxs retains accepted txs and removes rejected
-- txs from the active table and tx-key maps. Generated over a non-empty
-- list of (txid, size, accepted-flag, co-advertised-flag): the
-- accepted-flag controls accept vs reject; the co-advertised-flag adds a
-- second peer as advertiser, so rejected co-advertised txs stay in
-- 'sharedTxTable' (only the calling peer's advertisement is removed)
-- while rejected solo txs are dropped from all maps.
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> NonEmptyList (TxId, Positive Int, Bool, Bool)
  -> Property
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    (ArbTxDecisionPolicy policy)
    (Positive peeraddr)
    (NonEmpty rawEntries) =
      tabulate "accepted count"      [bucket (length acceptedKeys)]
    . tabulate "rejected count"      [bucket (length rejectedKeys)]
    . tabulate "co-advertised count" [bucket (length [() | (_, _, _, True) <- entries])]
    $ conjoin $
      [ peerDownloadedTxs peerState' === IntMap.empty
      , sharedGeneration sharedState' === sharedGeneration sharedState0 + 1
      , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
      , checkNoThunks "sharedState'" sharedState'
      ]
      ++
      [ counterexample ("accepted txid=" ++ show txid) (acceptedAssertions txid)
      | (txid, _, True, _) <- entries
      ]
      ++
      [ counterexample ("rejected solo txid=" ++ show txid) (rejectedSoloAssertions txid)
      | (txid, _, False, False) <- entries
      ]
      ++
      [ counterexample ("rejected co-advertised txid=" ++ show txid) (rejectedCoAdvAssertions txid)
      | (txid, _, False, True) <- entries
      ]
  where
    -- Use a distinct address as the second advertiser. Adding @peeraddr + 1@
    -- guarantees they don't clash even if QC produces consecutive ids.
    otherPeer = peeraddr + 1

    -- Normalise: shift txids to >= 1, convert sizes, dedupe by shifted txid
    -- (first occurrence wins). NonEmpty input ensures at least one entry
    -- survives.
    entries :: [(TxId, SizeInBytes, Bool, Bool)]
    entries = nubBy ((==) `on` (\(t, _, _, _) -> t))
            $ map (\(t, sz, acc, co) -> (abs t + 1, mkSize sz, acc, co)) rawEntries

    sharedState0 =
      let st            = mkSharedState [ txid | (txid, _, _, _) <- entries ]
          peeraddrKeys  = [ lookupKeyOrFail txid st | (txid, _, _, _)    <- entries ]
          otherPeerKeys = [ lookupKeyOrFail txid st | (txid, _, _, True) <- entries ] in
      ensurePeerAdvertisesTxKeys otherPeer otherPeerKeys
      $ ensurePeerAdvertisesTxKeys peeraddr peeraddrKeys
      $ st { sharedTxTable = IntMap.fromList
              [ (unTxKey (lookupKeyOrFail txid st),
                 (mkTxEntry peeraddr sz (Just TxBuffered) policy)
                   { txAdvertiserCount = if co then 2 else 1 })
              | (txid, sz, _, co) <- entries
              ]
           }

    keyOf txid = lookupKeyOrFail txid sharedState0
    kOf        = unTxKey . keyOf

    acceptedKeys = [ keyOf txid | (txid, _, True,  _) <- entries ]
    rejectedKeys = [ keyOf txid | (txid, _, False, _) <- entries ]

    peerState0 = emptyPeerTxLocalState
                   { peerDownloadedTxs = IntMap.fromList
                       [ (kOf txid, mkTx txid sz)
                       | (txid, sz, _, _) <- entries
                       ]
                   }

    expectedRetainUntil = addTime (bufferedTxsMinLifetime policy) now

    (peerState', sharedState') =
      handleSubmittedTxs now policy peeraddr acceptedKeys rejectedKeys
                         peerState0 sharedState0

    advertisedKeysOf peer st =
      sharedPeerAdvertisedTxKeys (lookupPeerOrFail peer st)

    acceptedAssertions txid =
      let k   = kOf txid
          key = keyOf txid in
      conjoin
        [ IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
        , retainedLookup k (sharedRetainedTxs sharedState') === Just expectedRetainUntil
        , Map.lookup (getRawTxId txid) (sharedTxIdToKey sharedState') === Just key
        , IntMap.lookup k (sharedKeyToTxId sharedState') === Just txid
        ]

    rejectedSoloAssertions txid =
      let k = kOf txid in
      conjoin
        [ IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
        , retainedLookup k (sharedRetainedTxs sharedState') === Nothing
        , Map.lookup (getRawTxId txid) (sharedTxIdToKey sharedState') === Nothing
        , IntMap.lookup k (sharedKeyToTxId sharedState') === Nothing
        ]

    rejectedCoAdvAssertions txid =
      let k   = kOf txid
          key = keyOf txid in
      conjoin
        [ counterexample "entry should remain in sharedTxTable"
            $ isJust (IntMap.lookup k (sharedTxTable sharedState'))
        , counterexample "this peer's advertisement should be removed"
            $ not (IntSet.member k (advertisedKeysOf peeraddr  sharedState'))
        , counterexample "co-advertiser's advertisement should remain"
            $ IntSet.member k (advertisedKeysOf otherPeer sharedState')
        , Map.lookup (getRawTxId txid) (sharedTxIdToKey sharedState') === Just key
        , IntMap.lookup k (sharedKeyToTxId sharedState') === Just txid
        , retainedLookup k (sharedRetainedTxs sharedState') === Nothing
        ]

-- Verifies that nextPeerAction submits buffered txs owned by the peer before
-- taking any other action.
prop_nextPeerAction_prioritisesSubmit
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_prioritisesSubmit (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 txSize0 =
  case peerAction of
       PeerSubmitTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerState' === peerState0
             -- Submit selection atomically marks the chosen tx as TxSubmitting
             -- so concurrent peer decisions exclude it.
           , sharedState' === markSubmittingTxs peeraddr [key] sharedState0
           , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
           , checkNoThunks "sharedState'" sharedState'
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers =
          Map.singleton peeraddr
            (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased peeraddr (addTime 10 now)
          , txAdvertiserCount = 1
          , txAttempts = Map.singleton peeraddr TxBuffered
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest policy
      , peerDownloadedTxs = IntMap.singleton k tx
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peeraddr peerState0 sharedState0

-- Verifies that nextPeerAction leases a claimable tx to the best idle
-- advertiser and requests its body.
prop_nextPeerAction_claimsClaimableTx
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_claimsClaimableTx (ArbTxDecisionPolicy policy) (Positive peerA0) (Positive peerB0) (Positive peerC0) txid0 txSize0 =
  distinctPeers ==>
    peerTxLocalStateInvariant policy peerState0 .&&.
    case peerAction of
         PeerRequestTxs txKeys ->
           conjoin
             [ txKeys === [key]
             , peerRequestedTxs peerState' === IntSet.singleton k
             , txLease (lookupEntryOrFail key sharedState') ===
                 TxLeased peerA (addTime (interTxSpace policy) now)
             , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
             , checkNoThunks "sharedState'" sharedState'
             ]
         _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    peerA = peerA0
    peerAScore = PeerScore (min 1 (scoreMax policy)) now
    peerB = peerB0 + 1000
    peerC = peerC0 + 2000
    distinctPeers = peerA /= peerB && peerA /= peerC && peerB /= peerC
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peerA, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerB, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerC, withAdvertisedTxKeys [key] (mkSharedPeerState PeerWaitingTxs))
          ]
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable (Time 0)
          , txAdvertiserCount = 3
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds = IntMap.singleton k txSize
      , peerScore = peerAScore
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peerA peerState0 sharedState0

-- | A peer's score decays linearly at 'scoreRate' from its last
-- timestamped value, clamped to zero.
unit_peerScore_decaysOverTime :: (String -> IO ()) -> Assertion
unit_peerScore_decaysOverTime step = do
    step "After 50 seconds at scoreRate 0.1 a score of 10 should drain to 5"
    currentPeerScore policy (Time 50) score0 @?= 5
    step "After 200 seconds the score should be fully decayed (clamped to 0)"
    currentPeerScore policy (Time 200) score0 @?= 0
    step "Reading at the same instant as the last update returns the unchanged value"
    currentPeerScore policy (Time 0) score0 @?= peerScoreValue score0
  where
    policy = defaultTxDecisionPolicy
    score0 = PeerScore { peerScoreValue = 10, peerScoreTs = Time 0 }

-- | A new rejection drains the existing score at 'scoreRate' per second
-- since its last update before adding the rejection count.
unit_applyPeerRejections_drainsThenAdds :: (String -> IO ()) -> Assertion
unit_applyPeerRejections_drainsThenAdds step = do
    step "Starting score 10 at Time 0; one rejection 50s later: 10 - (50 * 0.1) + 1 = 6"
    fst (applyPeerRejections policy (Time 50) 1 peerState0) @?= 6
  where
    policy     = defaultTxDecisionPolicy
    peerState0 = emptyPeerTxLocalState
                   { peerScore = PeerScore { peerScoreValue = 10
                                           , peerScoreTs    = Time 0 } }

unit_nextPeerAction_claimsAtScoreDelayThreshold :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsAtScoreDelayThreshold step = do
  step "Run nextPeerAction for a peer whose score contributes exactly a 1 ms claim delay"
  case peerAction of
       PeerRequestTxs txKeys -> do
         step "Assert the tx becomes claimable once the peerScore / 20 ms threshold has elapsed"
         txKeys @?= [key]
         peerRequestedTxs peerState' @?= IntSet.singleton k
         txLease (lookupEntryOrFail key sharedState') @?=
           TxLeased peeraddr (addTime (interTxSpace defaultTxDecisionPolicy) now)
       _ ->
         assertFailure ("unexpected peer action: " ++ show peerAction)
  where
    peeraddr = 7
    txid = 1
    txSize = mkSize (Positive 10)
    key = TxKey 0
    k = unTxKey key
    claimableAt = Time 99.999
    sharedState0 = emptySharedTxState
      { sharedPeers =
          Map.singleton peeraddr
            (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable claimableAt
          , txAdvertiserCount = 1
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity defaultTxDecisionPolicy
          }
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds = IntMap.singleton k txSize
      , peerScore = PeerScore 20 now
      }
    (peerAction, peerState', sharedState') =
      nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- | A peer whose submission attempt has been cleared (e.g. after a
-- mempool rejection) must not prevent another advertiser from claiming
-- the same tx. After one peer's attempt is cleared and its lease
-- released back to 'TxClaimable', any other peer that still advertises
-- the tx should be able to claim it on its next 'nextPeerAction' pass.
--
-- Exercises the cross-peer retry invariant in the 'txSelectable' /
-- 'nextPeerAction' path: once no peer has an outstanding attempt on a
-- tx and its lease is claimable, a still-advertising peer is eligible
-- to re-claim.
prop_nextPeerAction_claimsRejectedTxFromOtherAdvertiser :: Property
prop_nextPeerAction_claimsRejectedTxFromOtherAdvertiser =
    counterexample "peerB should be able to claim the released tx"
  $ case action of
      PeerRequestTxs keys ->
        counterexample ("keys requested: " ++ show keys)
          $ TxKey txKeyInt `elem` keys
      _ ->
        counterexample ("peerB action: " ++ show action) False
  where
    peerA, peerB :: PeerAddr
    peerA = 1
    peerB = 2

    txid :: TxId
    txid = 4

    txKeyInt :: Int
    txKeyInt = 0

    txSize :: SizeInBytes
    txSize = 100

    txBody :: Tx TxId
    txBody = mkTx txid txSize

    sharedState0 :: SharedTxState PeerAddr TxId
    sharedState0 =
        ensurePeerAdvertisesTxKeys peerA [TxKey txKeyInt]
      $ ensurePeerAdvertisesTxKeys peerB [TxKey txKeyInt]
      $ emptySharedTxState
          { sharedTxIdToKey = Map.singleton (getRawTxId txid) (TxKey txKeyInt)
          , sharedKeyToTxId = IntMap.singleton txKeyInt txid
          , sharedNextTxKey = txKeyInt + 1
          , sharedTxTable   = IntMap.singleton txKeyInt TxEntry
              { txLease           = TxLeased peerA (addTime 1 now)
              , txAdvertiserCount = 2
              , txAttempts        = Map.singleton peerA TxBuffered
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity defaultTxDecisionPolicy
              }
          }

    peerAState :: PeerTxLocalState (Tx TxId)
    peerAState = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey txKeyInt)
      , peerDownloadedTxs       = IntMap.singleton txKeyInt txBody
      }

    peerBState :: PeerTxLocalState (Tx TxId)
    peerBState = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey txKeyInt)
      , peerAvailableTxIds      = IntMap.singleton txKeyInt txSize
      }

    (_peerAStateAfter, sharedStateAfterRejection) =
      handleSubmittedTxs now defaultTxDecisionPolicy peerA
        []
        [TxKey txKeyInt]
        peerAState
        sharedState0

    (action, _peerBStateAfter, _sharedStateFinal) =
      nextPeerAction now defaultTxDecisionPolicy peerB peerBState
        sharedStateAfterRejection

-- Verifies that nextPeerAction can steal an expired lease for the best idle
-- advertiser and request that tx.
prop_nextPeerAction_claimsExpiredLease
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_claimsExpiredLease (ArbTxDecisionPolicy policy) (Positive oldOwner0) (Positive peerA0) (Positive peerB0) txid0 txSize0 =
  distinctPeers ==>
    peerTxLocalStateInvariant policy peerState0 .&&.
    case peerAction of
         PeerRequestTxs txKeys ->
           conjoin
             [ txKeys === [key]
             , peerRequestedTxs peerState' === IntSet.singleton k
             , txLease (lookupEntryOrFail key sharedState') ===
                 TxLeased peerA (addTime (interTxSpace policy) now)
             , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
             , checkNoThunks "sharedState'" sharedState'
             ]
         _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    oldOwner = oldOwner0
    peerA = peerA0 + 1000
    peerAScore = PeerScore (min 1 (scoreMax policy)) now
    peerB = peerB0 + 2000
    distinctPeers = oldOwner /= peerA && oldOwner /= peerB && peerA /= peerB
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (oldOwner, withAdvertisedTxKeys [key] (mkSharedPeerState PeerWaitingTxs))
          , (peerA, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerB, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          ]
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased oldOwner (Time 0)
          , txAdvertiserCount = 3
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds = IntMap.singleton k txSize
      , peerScore = peerAScore
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peerA peerState0 sharedState0

-- Verifies that nextPeerAction still requests an oversized first tx when it
-- is the only available choice within the soft-budget policy.
prop_nextPeerAction_requestsOversizedFirstTx
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_requestsOversizedFirstTx (ArbTxDecisionPolicy basePolicy) (Positive peeraddr) txid0 (Positive txSize0) =
  peerTxLocalStateInvariant policy peerState0 .&&.
  case peerAction of
       PeerRequestTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerRequestedTxs peerState' === IntSet.singleton k
           , peerRequestedTxsSize peerState' === txSize
           , txLease (lookupEntryOrFail key sharedState') ===
               TxLeased peeraddr (addTime (interTxSpace policy) now)
           , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
           , checkNoThunks "sharedState'" sharedState'
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize (Positive (txSize0 + 1))
    key = TxKey 0
    k = unTxKey key
    policy = basePolicy
      { txsSizeInflightPerPeer = txSize - 1
      , maxOutstandingTxBatchesPerPeer = 1
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr
          (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k (mkTxEntry peeraddr txSize Nothing policy)
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds = IntMap.singleton k txSize
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
    blockedKey = TxKey 1
    claimableKey = TxKey 2
    kBlocked = 1
    kClaimable = 2
    peerState = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [blockedKey, claimableKey]
      , peerAvailableTxIds = IntMap.fromList [(kBlocked, 10), (kClaimable, 11)]
      }
    sharedState :: SharedTxState PeerAddr TxId
    sharedState = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, withAdvertisedTxKeys [blockedKey, claimableKey]
                         (mkSharedPeerState PeerIdle))
          , (otherPeer, withAdvertisedTxKeys [blockedKey]
                          (mkSharedPeerState PeerIdle))
          ]
      , sharedTxTable = IntMap.fromList
          [ (kBlocked, TxEntry
              { txLease = TxLeased otherPeer (addTime 10 testNow)
              , txAdvertiserCount = 2
              , txAttempts = Map.empty
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          , (kClaimable, TxEntry
              { txLease = TxClaimable (Time 0)
              , txAdvertiserCount = 1
              , txAttempts = Map.empty
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          ]
      }
    (peerAction, peerState', sharedState') =
      nextPeerAction testNow policy peeraddr peerState sharedState

-- Verifies that nextPeerAction submits buffered owned txs before
-- acknowledging their txids.
prop_nextPeerAction_ownerSubmitsBuffered
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_ownerSubmitsBuffered (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 txSize0 =
  case peerAction of
       PeerSubmitTxs [txKey] ->
         conjoin
           [ txKey === key
           , peerState' === peerState0
             -- Submit selection atomically marks the chosen tx as TxSubmitting
             -- so concurrent peer decisions exclude it.
           , sharedState' === markSubmittingTxs peeraddr [key] sharedState0
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    txSize = mkSize txSize0
    tx = mkTx txid txSize
    key = TxKey 0
    k = unTxKey key
    sharedState0 = emptySharedTxState
      { sharedPeers =
          Map.singleton peeraddr
            (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable (Time 0)
          , txAdvertiserCount = 1
          , txAttempts = Map.singleton peeraddr TxBuffered
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest policy
      , peerDownloadedTxs = IntMap.singleton k tx
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peeraddr peerState0 sharedState0

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
      , txAdvertiserCount = 2
      , txAttempts = Map.fromList
          [ (peeraddr, TxBuffered)
          , (submittingPeer, TxSubmitting)
          ]
      , currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, withAdvertisedTxKeys [blockedKey, claimableKey]
                         (mkSharedPeerState PeerIdle))
          , (submittingPeer, withAdvertisedTxKeys [blockedKey]
                              (mkSharedPeerState PeerSubmittingToMempool))
          ]
      , sharedTxTable = IntMap.fromList
          [ (kBlocked, blockedEntry)
          , (kClaimable, TxEntry
              { txLease = TxClaimable (Time 0)
              , txAdvertiserCount = 1
              , txAttempts = Map.empty
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity defaultTxDecisionPolicy
              })
          ]
      , sharedTxIdToKey = Map.fromList
          [ (getRawTxId blockedTxid, blockedKey)
          , (getRawTxId claimableTxid, claimableKey)
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
       PeerRequestTxIds _ txIdsToAcknowledge txIdsToReq -> do
         step "Assert only the safe prefix is acknowledged"
         txIdsToAcknowledge @?= 1
         assertBool ("expected positive txIdsToReq, got " ++ show txIdsToReq) (txIdsToReq > 0)
         peerUnacknowledgedTxIds peerState' @?= StrictSeq.singleton blockedKey
         peerRequestedTxIds peerState' @?= txIdsToReq
         txAdvertiserCount (lookupEntryOrFail blockedKey sharedState') @?=
           txAdvertiserCount blockedEntry
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
      , txAdvertiserCount = 2
      , txAttempts = Map.fromList
          [ (peeraddr, TxBuffered)
          , (submittingPeer, TxSubmitting)
          ]
      , currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ (peeraddr, withAdvertisedTxKeys [blockedKey]
                         (mkSharedPeerState PeerIdle))
          , (submittingPeer, withAdvertisedTxKeys [blockedKey]
                              (mkSharedPeerState PeerSubmittingToMempool))
          ]
      , sharedTxTable = IntMap.singleton kBlocked blockedEntry
      , sharedRetainedTxs = retainedSingleton kResolved (addTime 17 now)
      , sharedTxIdToKey = Map.fromList
          [ (getRawTxId resolvedTxid, resolvedKey)
          , (getRawTxId blockedTxid, blockedKey)
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
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Property
prop_nextPeerAction_nonOwnerWaitsUntilResolved (ArbTxDecisionPolicy policy) (Positive owner0) (Positive peeraddr0) txid0 =
  owner /= peeraddr ==>
    conjoin
      [ case unresolvedAction of
             PeerDoNothing _ _ ->
               unresolvedExpectations
             PeerRequestTxIds _ txIdsToAcknowledge _ ->
               conjoin
                 [ txIdsToAcknowledge === 0
                 , unresolvedExpectations
                 ]
             _ -> counterexample ("unexpected unresolved action: " ++ show unresolvedAction) False
      , case resolvedAction of
             PeerRequestTxIds _ txIdsToAcknowledge _ ->
               conjoin
                 [ txIdsToAcknowledge === 1
                 , peerUnacknowledgedTxIds resolvedPeerState' === StrictSeq.empty
                 , checkNoThunks "resolvedPeerState'" (resolvedPeerState' :: PeerTxLocalState (Tx TxId))
                 , checkNoThunks "resolvedSharedState'" resolvedSharedState'
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
      [ (owner, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      , (peeraddr, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
      ]
    unresolvedSharedState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable (Time 0)
          , txAdvertiserCount = 2
          , txAttempts = Map.singleton owner TxBuffered
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
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
    (unresolvedAction, unresolvedPeerState', unresolvedSharedState') = nextPeerAction now policy peeraddr peerState0 unresolvedSharedState
    unresolvedExpectations =
      conjoin
        [ peerUnacknowledgedTxIds unresolvedPeerState' === peerUnacknowledgedTxIds peerState0
        , txAdvertiserCount (lookupEntryOrFail key unresolvedSharedState') ===
            txAdvertiserCount (lookupEntryOrFail key unresolvedSharedState)
        , checkNoThunks "unresolvedPeerState'" (unresolvedPeerState' :: PeerTxLocalState (Tx TxId))
        , checkNoThunks "unresolvedSharedState'" unresolvedSharedState'
        ]
    (resolvedAction, resolvedPeerState', resolvedSharedState') = nextPeerAction now policy peeraddr peerState0 resolvedSharedState

-- Verifies that nextPeerActionPipelined does nothing when it can only
-- acknowledge txids and cannot request new ones in the same step.
prop_nextPeerActionPipelined_requiresAckAndReq
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_requiresAckAndReq (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ _ ->
         conjoin
           [ peerUnacknowledgedTxIds peerState' === peerUnacknowledgedTxIds peerState0
           , sharedState' === sharedState0
           , checkNoThunks "peerState'" (peerState' :: PeerTxLocalState (Tx TxId))
           , checkNoThunks "sharedState'" sharedState'
           ]
       _ -> counterexample ("unexpected pipelined action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = maxNumTxIdsToRequest policy
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle)
      , sharedRetainedTxs = retainedSingleton k (addTime 17 now)
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now policy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined requests txids once it can both
-- acknowledge old txids and ask for more.
prop_nextPeerActionPipelined_requestsTxIds
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_requestsTxIds (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerRequestTxIds _ txIdsToAcknowledge txIdsToReq ->
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
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle)
      , sharedRetainedTxs = retainedSingleton k (addTime 17 now)
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now policy peeraddr peerState0 sharedState0

unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply
  :: (String -> IO ())
  -> Assertion
unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply step = do
  step "Run nextPeerActionPipelined with three ackable txids and one outstanding body batch"
  case peerAction of
    PeerRequestTxIds _ txIdsToAcknowledge txIdsToReq -> do
      step "Assert pipelined txid requests keep one txid unacked while a body reply is still in flight"
      txIdsToAcknowledge @?= 2
      assertBool ("expected positive txIdsToReq, got " ++ show txIdsToReq) (txIdsToReq > 0)
      peerUnacknowledgedTxIds peerState' @?= StrictSeq.singleton keyC
      peerRequestedTxIds peerState' @?= txIdsToReq
      peerRequestedTxBatches peerState' @?= peerRequestedTxBatches peerState0
      sharedState' @?= sharedState0
    _ ->
      assertFailure ("unexpected pipelined action: " ++ show peerAction)
  where
    peeraddr :: PeerAddr
    peeraddr = 7
    txidA, txidB, txidC :: TxId
    txidA = 1
    txidB = 2
    txidC = 3
    keyA = TxKey 0
    keyB = TxKey 1
    keyC = TxKey 2
    requestedBatch = mkRequestedTxBatch [keyA] 11
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB, keyC]
      , peerRequestedTxs = IntSet.singleton (unTxKey keyA)
      , peerRequestedTxBatches = StrictSeq.singleton requestedBatch
      , peerRequestedTxsSize = requestedTxBatchSize requestedBatch
      }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle)
      , sharedRetainedTxs =
          retainedFromList
            [ (unTxKey keyA, addTime 17 now)
            , (unTxKey keyB, addTime 17 now)
            , (unTxKey keyC, addTime 17 now)
            ]
      , sharedTxIdToKey = Map.fromList [(getRawTxId txidA, keyA), (getRawTxId txidB, keyB), (getRawTxId txidC, keyC)]
      , sharedKeyToTxId =
          IntMap.fromList
            [ (unTxKey keyA, txidA)
            , (unTxKey keyB, txidB)
            , (unTxKey keyC, txidC)
            ]
      , sharedNextTxKey = 3
      }
    (peerAction, peerState', sharedState') =
      nextPeerActionPipelined now defaultTxDecisionPolicy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined opens a second outstanding body
-- batch when another downloadable tx is available.
prop_nextPeerActionPipelined_secondBodyBatch
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_secondBodyBatch (ArbTxDecisionPolicy basePolicy) (Positive peeraddr) txidA0 txidB0 txSizeA0 txSizeB0 =
  txidA /= txidB ==>
    peerTxLocalStateInvariant policy peerState0 .&&.
    case peerAction of
         PeerRequestTxs [txKey] ->
           conjoin
             [ txKey === keyB
             , peerRequestedTxs peerState' === IntSet.fromList [kA, kB]
             , StrictSeq.length (peerRequestedTxBatches peerState') === 2
             , peerRequestedTxsSize peerState' === txSizeA + txSizeB
             , fmap txLease (IntMap.lookup kB (sharedTxTable sharedState')) ===
                 Just (TxLeased peeraddr (addTime (interTxSpace policy) now))
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
      { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB]
      , peerAvailableTxIds = IntMap.fromList [(kA, txSizeA), (kB, txSizeB)]
      , peerRequestedTxs = IntSet.singleton kA
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [keyA] txSizeA)
      , peerRequestedTxsSize = txSizeA
      }
    sharedState0 = emptySharedTxState
      { sharedPeers =
          Map.singleton peeraddr
            (withAdvertisedTxKeys [keyA, keyB] (mkSharedPeerState PeerIdle))
      , sharedTxTable = IntMap.fromList
          [ (kA, mkTxEntry peeraddr txSizeA (Just TxDownloading) policy)
          , (kB, TxEntry
              { txLease = TxClaimable (Time 0)
              , txAdvertiserCount = 1
              , txAttempts = Map.empty
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          ]
      , sharedTxIdToKey = Map.fromList [(getRawTxId txidA, keyA), (getRawTxId txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(kA, txidA), (kB, txidB)]
      , sharedNextTxKey = 2
      }
    policy = basePolicy
      { maxOutstandingTxBatchesPerPeer = max 2 (maxOutstandingTxBatchesPerPeer basePolicy)
      , txsSizeInflightPerPeer         = max (txSizeA + txSizeB)
                                             (txsSizeInflightPerPeer basePolicy)
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now policy peeraddr peerState0 sharedState0

-- Verifies that nextPeerActionPipelined does not open a third outstanding
-- body batch once the per-peer batch limit is reached.
prop_nextPeerActionPipelined_noThirdBodyBatch
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerActionPipelined_noThirdBodyBatch (ArbTxDecisionPolicy basePolicy) (Positive peeraddr) txidA0 txidB0 txidC0 txSizeA0 txSizeB0 txSizeC0 =
  distinctTxIds ==>
    peerTxLocalStateInvariant policy peerState0 .&&.
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
      { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB, keyC]
      , peerAvailableTxIds = IntMap.fromList [(kA, txSizeA), (kB, txSizeB), (kC, txSizeC)]
      , peerRequestedTxs = IntSet.fromList [kA, kB]
      , peerRequestedTxBatches = StrictSeq.fromList
          [ mkRequestedTxBatch [keyA] txSizeA
          , mkRequestedTxBatch [keyB] txSizeB
          ]
      , peerRequestedTxsSize = txSizeA + txSizeB
      }
    sharedState0 = emptySharedTxState
      { sharedPeers =
          Map.singleton peeraddr
            (withAdvertisedTxKeys [keyA, keyB, keyC] (mkSharedPeerState PeerIdle))
      , sharedTxTable = IntMap.fromList
          [ (kA, mkTxEntry peeraddr txSizeA (Just TxDownloading) policy)
          , (kB, mkTxEntry peeraddr txSizeB (Just TxDownloading) policy)
          , (kC, TxEntry
              { txLease = TxClaimable (Time 0)
              , txAdvertiserCount = 1
              , txAttempts = Map.empty
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          ]
      , sharedTxIdToKey = Map.fromList [(getRawTxId txidA, keyA), (getRawTxId txidB, keyB), (getRawTxId txidC, keyC)]
      , sharedKeyToTxId = IntMap.fromList [(kA, txidA), (kB, txidB), (kC, txidC)]
      , sharedNextTxKey = 3
      }
    (peerAction, peerState', sharedState') = nextPeerActionPipelined now policy peeraddr peerState0 sharedState0
    policy = basePolicy { maxOutstandingTxBatchesPerPeer = 2 }

-- Verifies that nextPeerAction prunes expired retained txs and removes their
-- tx-key mappings while the peer is idle.
prop_nextPeerAction_prunesExpiredRetained
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_prunesExpiredRetained (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ Nothing ->
         conjoin
           [ peerState' === idlePeerState
           , IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
           , retainedLookup k (sharedRetainedTxs sharedState') === Nothing
           , Map.lookup (getRawTxId txid) (sharedTxIdToKey sharedState') === Nothing
           , IntMap.lookup k (sharedKeyToTxId sharedState') === Nothing
           , sharedGeneration sharedState' === sharedGeneration sharedState0 + 1
           ]
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    txid = abs txid0 + 1
    key = TxKey 0
    k = unTxKey key
    idlePeerState :: PeerTxLocalState (Tx TxId)
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest policy }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle)
      , sharedRetainedTxs = retainedSingleton k now
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = max 1 (k + 1)
      }
    -- The central counters thread sweeps expired retained entries; emulate
    -- that by calling the same helper before evaluating the peer decision.
    sweptState = sweepSharedState now sharedState0
    (peerAction, peerState', sharedState') = nextPeerAction now policy peeraddr idlePeerState sweptState

-- Verifies that nextPeerAction keeps unexpired retained txs and returns the
-- wake delay until their expiry.
prop_nextPeerAction_keepsRetained
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_nextPeerAction_keepsRetained (ArbTxDecisionPolicy policy) (Positive peeraddr) txid0 _txSize0 =
  case peerAction of
       PeerDoNothing _ (Just wakeDelay) ->
         conjoin
           [ peerState' === idlePeerState
           , IntMap.lookup k (sharedTxTable sharedState') === (Nothing :: Maybe (TxEntry PeerAddr))
           , retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil
           , Map.lookup (getRawTxId txid) (sharedTxIdToKey sharedState') === Just key
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
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest policy }
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.singleton peeraddr (mkSharedPeerState PeerIdle)
      , sharedRetainedTxs = retainedSingleton k retainUntil
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    (peerAction, peerState', sharedState') = nextPeerAction now policy peeraddr idlePeerState sharedState0

-- Verifies that PeerDoNothing waits until the earliest shared expiry, whether
-- it comes from a lease or a retained tx.
prop_nextPeerAction_earliestWakeDelay
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> TxId
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Property
prop_nextPeerAction_earliestWakeDelay (ArbTxDecisionPolicy policy) (Positive peeraddr) (Positive owner0) txidA0 txidB0 _txSizeA0 _txSizeB0 =
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
    idlePeerState = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest policy }
    sharedPeers0 = Map.fromList
      [ (peeraddr, withAdvertisedTxKeys [keyA] (mkSharedPeerState PeerIdle))
      , (owner, withAdvertisedTxKeys [keyA] (mkSharedPeerState PeerWaitingTxs))
      ]
    leaseFirstState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton (unTxKey keyA) TxEntry
          { txLease = TxLeased owner leaseUntil
          , txAdvertiserCount = 2
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedRetainedTxs = retainedSingleton (unTxKey keyB) retainUntilLater
      , sharedTxIdToKey = Map.fromList [(getRawTxId txidA, keyA), (getRawTxId txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(unTxKey keyA, txidA), (unTxKey keyB, txidB)]
      , sharedNextTxKey = 2
      }
    retainFirstState = emptySharedTxState
      { sharedPeers = sharedPeers0
      , sharedTxTable = IntMap.singleton (unTxKey keyA) TxEntry
          { txLease = TxLeased owner leaseUntilLater
          , txAdvertiserCount = 2
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedRetainedTxs = retainedSingleton (unTxKey keyB) retainUntilSoon
      , sharedTxIdToKey = Map.fromList [(getRawTxId txidA, keyA), (getRawTxId txidB, keyB)]
      , sharedKeyToTxId = IntMap.fromList [(unTxKey keyA, txidA), (unTxKey keyB, txidB)]
      , sharedNextTxKey = 2
      }
    (leaseFirstAction, _, _) = nextPeerAction now policy peeraddr idlePeerState leaseFirstState
    (retainFirstAction, _, _) = nextPeerAction now policy peeraddr idlePeerState retainFirstState

-- Verifies that PeerDoNothing reports the current generation of the acting
-- peer.
prop_nextPeerAction_returnsPeerGeneration
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Property
prop_nextPeerAction_returnsPeerGeneration (ArbTxDecisionPolicy policy) (Positive peeraddr) =
  case peerAction of
       PeerDoNothing generation Nothing -> generation === expectedGeneration
       _ -> counterexample ("unexpected peer action: " ++ show peerAction) False
  where
    expectedGeneration = 7
    sharedState0 :: SharedTxState PeerAddr TxId
    sharedState0 = emptySharedTxState
      { sharedPeers = Map.fromList
          [ ( peeraddr
            , (mkSharedPeerState PeerIdle)
                { sharedPeerGeneration = expectedGeneration
                }
            )
          , ( peeraddr + 1000
            , (mkSharedPeerState PeerIdle)
                { sharedPeerGeneration = 11
                }
            )
          ]
      }
    peerState0 = emptyPeerTxLocalState { peerRequestedTxIds = maxNumTxIdsToRequest policy }
    (peerAction, _, _) = nextPeerAction now policy peeraddr peerState0 sharedState0

-- Verifies that handleSubmittedTxs bumps the generation of every other
-- advertiser of the resolved tx, regardless of phase, while leaving the
-- submitting peer's own generation unchanged.
prop_handleSubmittedTxs_bumpsAdvertisers
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Property
prop_handleSubmittedTxs_bumpsAdvertisers (ArbTxDecisionPolicy policy) (Positive owner0) (Positive peerA0) (Positive peerB0) txid0 txSize0 =
  owner /= peerA && owner /= peerB && peerA /= peerB ==>
    conjoin
      [ sharedPeerGeneration (lookupPeerOrFail peerA sharedState') === 1
      , sharedPeerGeneration (lookupPeerOrFail peerB sharedState') === 1
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
          [ (owner, withAdvertisedTxKeys [key] (mkSharedPeerState PeerSubmittingToMempool))
          , (peerA, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerB, withAdvertisedTxKeys [key] (mkSharedPeerState PeerWaitingTxs))
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased owner (addTime 10 now)
          , txAdvertiserCount = 3
          , txAttempts = Map.singleton owner TxBuffered
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    peerState0 = emptyPeerTxLocalState { peerDownloadedTxs = IntMap.singleton k tx }
    (_, sharedState') = handleSubmittedTxs now policy owner [key] [] peerState0 sharedState0

unit_advertisingPeersForTxExcept_scansAuthoritativePeerSets :: (String -> IO ()) -> Assertion
unit_advertisingPeersForTxExcept_scansAuthoritativePeerSets step = do
  step "Build a shared state whose per-tx advertiser count is stale-low"
  let advertisingPeers =
        advertisingPeersForTxExcept owner key sharedState0
  step "Assert all peers advertising the key are found from the authoritative per-peer key sets"
  advertisingPeers @?= Set.fromList [peerA, peerB]
  where
    owner, peerA, peerB, unrelatedPeer :: PeerAddr
    owner = 1
    peerA = 2
    peerB = 3
    unrelatedPeer = 4
    txid :: TxId
    txid = 1
    baseState = mkSharedState [txid]
    key = lookupKeyOrFail txid baseState
    k = unTxKey key
    sharedState0 = baseState
      { sharedPeers = Map.fromList
          [ (owner, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerA, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerB, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (unrelatedPeer, mkSharedPeerState PeerIdle)
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased owner (addTime 10 now)
          , txAdvertiserCount = 1
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity defaultTxDecisionPolicy
          }
      }

unit_removeAdvertisingPeersForResolvedTx_clearsAllAdvertisingPeers :: (String -> IO ()) -> Assertion
unit_removeAdvertisingPeersForResolvedTx_clearsAllAdvertisingPeers step = do
  step "Remove a resolved tx key from all advertising peers"
  let (sharedState', advertisers) =
        removeAdvertisingPeersForResolvedTx key sharedState0
  step "Assert all advertising peers are returned even when the cached count is stale-low"
  advertisers @?= Set.fromList [owner, peerA, peerB]
  step "Assert the resolved key is cleared from every advertising peer and unrelated peers are unchanged"
  sharedPeerAdvertisedTxKeys (lookupPeerOrFail owner sharedState') @?= IntSet.empty
  sharedPeerAdvertisedTxKeys (lookupPeerOrFail peerA sharedState') @?= IntSet.empty
  sharedPeerAdvertisedTxKeys (lookupPeerOrFail peerB sharedState') @?= IntSet.empty
  sharedPeerAdvertisedTxKeys (lookupPeerOrFail unrelatedPeer sharedState') @?= IntSet.empty
  sharedTxTable sharedState' @?= sharedTxTable sharedState0
  where
    owner, peerA, peerB, unrelatedPeer :: PeerAddr
    owner = 1
    peerA = 2
    peerB = 3
    unrelatedPeer = 4
    txid :: TxId
    txid = 1
    baseState = mkSharedState [txid]
    key = lookupKeyOrFail txid baseState
    k = unTxKey key
    sharedState0 = baseState
      { sharedPeers = Map.fromList
          [ (owner, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerA, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (peerB, withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle))
          , (unrelatedPeer, mkSharedPeerState PeerIdle)
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxLeased owner (addTime 10 now)
          , txAdvertiserCount = 1
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity defaultTxDecisionPolicy
          }
      }

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
          [ (peer, (mkSharedPeerState PeerWaitingTxs) { sharedPeerGeneration = 5 })
          , (other, (mkSharedPeerState PeerIdle) { sharedPeerGeneration = 11 })
          ]
      }
    sharedState' = updatePeerPhase peer PeerIdle sharedState0

unit_updatePeerPhase_wakesCompetingAdvertisers :: (String -> IO ()) -> Assertion
unit_updatePeerPhase_wakesCompetingAdvertisers step = do
  step "Update an idle peer to a waiting phase"
  sharedPeerPhase (lookupPeerOrFail leavingPeer sharedState') @?= PeerWaitingTxs
  step "Assert no competing advertisers are woken by leaving idle under score-delay claiming"
  sharedPeerGeneration (lookupPeerOrFail leavingPeer sharedState') @?= 5
  sharedPeerGeneration (lookupPeerOrFail competingPeer sharedState') @?= 11
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
          [ (leavingPeer, (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle)) { sharedPeerGeneration = 5 })
          , (competingPeer, (withAdvertisedTxKeys [key] (mkSharedPeerState PeerIdle)) { sharedPeerGeneration = 11 })
          , (unrelatedPeer, (mkSharedPeerState PeerIdle) { sharedPeerGeneration = 17 })
          ]
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable (Time 0)
          , txAdvertiserCount = 2
          , txAttempts = Map.empty
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity defaultTxDecisionPolicy
          }
      }
    sharedState' = updatePeerPhase leavingPeer PeerWaitingTxs sharedState0

-- Generate a shared peer state.
genSharedPeerState :: Gen SharedPeerState
genSharedPeerState = do
  sharedPeerPhase <- elements [PeerIdle, PeerWaitingTxIds, PeerWaitingTxs, PeerSubmittingToMempool]
  sharedPeerGeneration <- genSmallWord64
  pure SharedPeerState {
      sharedPeerPhase,
      sharedPeerAdvertisedTxKeys = IntSet.empty,
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

  peerScoreTs <- genSmallTime
  -- Generated peer states default to a zero score.
  let peerScoreValue = 0
  pure PeerTxLocalState {
      peerUnacknowledgedTxIds,
      peerAvailableTxIds,
      peerRequestedTxs = requestedSet,
      peerRequestedTxBatches,
      peerRequestedTxsSize,
      peerRequestedTxIds,
      peerDownloadedTxs,
      peerDownloadStartTime = Nothing,
      peerScore = PeerScore peerScoreValue peerScoreTs
    }
  where
    genAvailableTx key = do
      txSize <- genPositiveSize
      pure (unTxKey key, txSize)

    genDownloadedTx key = do
      txSize <- genPositiveSize
      pure (unTxKey key, mkTx (txIdForKey key) txSize)

newtype PeerSeed = PeerSeed { peerSeedGeneration :: Word64 }

data PeerDerivedUsage = PeerDerivedUsage {
    peerHasSubmitting   :: !Bool
  , peerHasRequestedTxs :: !Bool
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
      peerSeedGeneration <- genSmallWord64
      pure (peeraddr, PeerSeed { peerSeedGeneration })

    genRetainedEntry txid = do
      retainUntil <- genSharedExpiryTime
      pure (txid, retainUntil)

-- Generate one active tx entry using a mix of leased and claimable shapes.
genActiveTxEntry :: [PeerAddr] -> TxId -> Gen (TxId, Set.Set PeerAddr, TxEntry PeerAddr)
genActiveTxEntry peeraddrs txid = do
  (txAdvertisers, txEntry) <- frequency
    [ (5, genLeasedTxEntry peeraddrs txid)
    , (3, genClaimableTxEntry peeraddrs txid)
    ]
  pure (txid, txAdvertisers, txEntry)

-- Generate a leased entry where the owner may already be downloading, buffered, or submitting.
genLeasedTxEntry :: [PeerAddr] -> TxId -> Gen (Set.Set PeerAddr, TxEntry PeerAddr)
genLeasedTxEntry peeraddrs _txid = do
  advertiserPeers <- genNonEmptySublist peeraddrs
  owner <- elements advertiserPeers
  txAdvertisers <- genOwnedAdvertisers advertiserPeers owner
  txLease <- TxLeased owner <$> genSharedExpiryTime
  ownerAttempt <- frequency
    [ (2, pure Nothing)
    , (2, Just <$> elements [TxDownloading, TxBuffered])
    , (1, pure (Just TxSubmitting))
    ]
  pure
    ( txAdvertisers
    , TxEntry {
        txLease,
        txAdvertiserCount = Set.size txAdvertisers,
        txAttempts = maybe Map.empty (Map.singleton owner) ownerAttempt,
        currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }
    )

-- Generate a claimable entry advertised by one or more resolved peers.
genClaimableTxEntry :: [PeerAddr] -> TxId -> Gen (Set.Set PeerAddr, TxEntry PeerAddr)
genClaimableTxEntry peeraddrs _txid = do
  advertiserPeers <- genNonEmptySublist peeraddrs
  txAdvertisers <- genResolvedAdvertisers advertiserPeers
  claimableAt <- genSharedExpiryTime
  pure
    ( txAdvertisers
    , TxEntry {
        txLease = TxClaimable claimableAt,
        txAdvertiserCount = Set.size txAdvertisers,
        txAttempts = Map.empty,
        currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }
    )

-- Generate the advertiser set for an entry owned by the chosen peer.
genOwnedAdvertisers
  :: [PeerAddr]
  -> PeerAddr
  -> Gen (Set.Set PeerAddr)
genOwnedAdvertisers advertiserPeers owner =
  pure (Set.fromList (owner : advertiserPeers))

-- Generate the advertiser set for a claimable entry.
genResolvedAdvertisers :: [PeerAddr] -> Gen (Set.Set PeerAddr)
genResolvedAdvertisers advertiserPeers =
  pure (Set.fromList advertiserPeers)

-- Rebuild a shared state from tx-centric fixtures while preserving interned keys.
buildSharedTxState
  :: Map.Map PeerAddr PeerSeed
  -> [(TxId, Set.Set PeerAddr, TxEntry PeerAddr)]
  -> [(TxId, Time)]
  -> Word64
  -> SharedTxState PeerAddr TxId
buildSharedTxState peerSeeds activeEntries retainedEntries sharedGeneration =
  baseState {
      sharedPeers = deriveSharedPeers baseState peerSeeds activeEntries,
      sharedTxTable =
        IntMap.fromList
          [ (unTxKey (lookupKeyOrFail txid baseState), txEntry)
          | (txid, _, txEntry) <- activeEntries
          ],
      sharedRetainedTxs =
        retainedFromList
          [ (unTxKey (lookupKeyOrFail txid baseState), retainUntil)
          | (txid, retainUntil) <- retainedEntries
          ],
      sharedGeneration
    }
  where
    baseState =
      mkSharedState ([ txid | (txid, _, _) <- activeEntries ] <> fmap fst retainedEntries)

-- Derive peer phases from the generated tx entries.
deriveSharedPeers
  :: SharedTxState PeerAddr TxId
  -> Map.Map PeerAddr PeerSeed
  -> [(TxId, Set.Set PeerAddr, TxEntry PeerAddr)]
  -> Map.Map PeerAddr SharedPeerState
deriveSharedPeers baseState peerSeeds activeEntries =
  Map.mapWithKey buildPeerState completePeerSeeds
  where
    completePeerSeeds =
      foldl' addMissingPeerSeed peerSeeds (concatMap entryPeers activeEntries)

    peerUsages =
      foldl' accumulatePeerUsage Map.empty activeEntries

    peerAdvertisedKeys =
      Map.fromListWith IntSet.union
        [ (peeraddr, IntSet.singleton (unTxKey (lookupKeyOrFail txid baseState)))
        | (txid, txAdvertisers, _) <- activeEntries
        , peeraddr <- Set.toList txAdvertisers
        ]

    addMissingPeerSeed acc peeraddr =
      Map.insertWith (\_ old -> old) peeraddr defaultPeerSeed acc

    buildPeerState peeraddr PeerSeed { peerSeedGeneration } =
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
          sharedPeerAdvertisedTxKeys =
            Map.findWithDefault IntSet.empty peeraddr peerAdvertisedKeys,
          sharedPeerGeneration = peerSeedGeneration
        }

    defaultPeerSeed =
      PeerSeed { peerSeedGeneration = 0 }

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
  -> (TxId, Set.Set PeerAddr, TxEntry PeerAddr)
  -> Map.Map PeerAddr PeerDerivedUsage
accumulatePeerUsage acc (_, _, TxEntry { txAttempts }) =
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
entryPeers :: (TxId, Set.Set PeerAddr, TxEntry PeerAddr) -> [PeerAddr]
entryPeers (_, txAdvertisers, TxEntry { txLease, txAttempts }) =
  leaseOwner <> Set.toList txAdvertisers <> Map.keys txAttempts
  where
    leaseOwner =
      case txLease of
        TxLeased owner _ -> [owner]
        TxClaimable _    -> []

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
      [ ( resolveTxKey sharedState (TxKey k)
        , advertisersForKey k
        , txEntry
        )
      | (k, txEntry) <- IntMap.toList (sharedTxTable sharedState)
      ]
    retainedEntries =
      [ (resolveTxKey sharedState (TxKey k), retainUntil)
      | (k, retainUntil) <- retainedToList (sharedRetainedTxs sharedState)
      ]
    peerSeeds =
      Map.map
        (\SharedPeerState { sharedPeerGeneration } ->
          PeerSeed { peerSeedGeneration = sharedPeerGeneration })
        (sharedPeers sharedState)
    usedPeers =
      foldl' (\peers activeEntry -> peers <> entryPeers activeEntry) [] activeEntries
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
    advertisersForKey k =
      Map.keysSet $
        Map.filter
          (\SharedPeerState { sharedPeerAdvertisedTxKeys } ->
             IntSet.member k sharedPeerAdvertisedTxKeys)
          (sharedPeers sharedState)

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
    _  -> pure ys

-- Generate distinct positive ints from a bounded shuffled range.
genDistinctPositiveInts :: Int -> Gen [Int]
genDistinctPositiveInts count
  | count <= 0 = pure []
  | otherwise = take count <$> shuffle [1 .. max count (count * 4 + 5)]

-- Pick a peer address biased toward existing peers in the shared state, so
-- the generator frequently exercises the "peeraddr already known" code
-- paths. Falls back to a fresh small-range address when the shared state
-- has no peers.
genPeerAddrBiased :: SharedTxState PeerAddr TxId -> Gen PeerAddr
genPeerAddrBiased sharedState =
  case Map.keys (sharedPeers sharedState) of
    []    -> genFresh
    peers -> frequency
               [ (3, elements peers)
               , (1, genFresh)
               ]
  where
    genFresh = chooseInt (1, 64)

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

-- Render a count into a coarse bucket label for QuickCheck's 'tabulate'.
bucket :: Int -> String
bucket n
  | n <= 0    = "0"
  | n == 1    = "1"
  | n <= 5    = "2-5"
  | n <= 10   = "6-10"
  | n <= 25   = "11-25"
  | n <= 100  = "26-100"
  | otherwise = "100+"

-- Build a valid test transaction with matching body and advertised size.
mkTx :: TxId -> SizeInBytes -> Tx TxId
mkTx txid txSize = Tx
  { getTxId = txid
  , getTxSize = txSize
  , getTxAdvSize = txSize
  , getTxValid = True
  , getTxParent = Nothing
  }

-- Construct a peer fixture with zeroed generation.
mkSharedPeerState :: PeerPhase -> SharedPeerState
mkSharedPeerState sharedPeerPhase =
  SharedPeerState {
    sharedPeerPhase,
    sharedPeerAdvertisedTxKeys = IntSet.empty,
    sharedPeerGeneration = 0
  }

withAdvertisedTxKeys :: [TxKey] -> SharedPeerState -> SharedPeerState
withAdvertisedTxKeys txKeys sharedPeerState =
  sharedPeerState {
    sharedPeerAdvertisedTxKeys = IntSet.fromList (map unTxKey txKeys)
  }

ensurePeerAdvertisesTxKeys
  :: PeerAddr
  -> [TxKey]
  -> SharedTxState PeerAddr TxId
  -> SharedTxState PeerAddr TxId
ensurePeerAdvertisesTxKeys peeraddr txKeys st@SharedTxState { sharedPeers } =
  st {
    sharedPeers =
      Map.alter updatePeer peeraddr sharedPeers
  }
  where
    advertisedKeys = IntSet.fromList (map unTxKey txKeys)

    updatePeer Nothing =
      Just (withAdvertisedTxKeys txKeys (mkSharedPeerState PeerIdle))
    updatePeer (Just sharedPeerState) =
      Just
        (sharedPeerState {
          sharedPeerAdvertisedTxKeys =
            sharedPeerAdvertisedTxKeys sharedPeerState `IntSet.union` advertisedKeys
        })

-- Shift every TxKey referenced by a peer-local state by a constant offset so
-- the state can be composed with a foreign SharedTxState without key
-- collisions.
shiftPeerTxLocalStateKeys :: Int -> PeerTxLocalState tx -> PeerTxLocalState tx
shiftPeerTxLocalStateKeys offset peerState = peerState {
    peerUnacknowledgedTxIds =
      fmap shiftTxKey (peerUnacknowledgedTxIds peerState),
    peerAvailableTxIds =
      IntMap.mapKeysMonotonic (+ offset) (peerAvailableTxIds peerState),
    peerRequestedTxs =
      IntSet.map (+ offset) (peerRequestedTxs peerState),
    peerRequestedTxBatches =
      fmap shiftBatch (peerRequestedTxBatches peerState),
    peerDownloadedTxs =
      IntMap.mapKeysMonotonic (+ offset) (peerDownloadedTxs peerState)
  }
  where
    shiftTxKey (TxKey k) = TxKey (k + offset)
    shiftBatch batch = batch {
        requestedTxBatchSet = IntSet.map (+ offset) (requestedTxBatchSet batch)
      }

-- Intern a list of txids into an otherwise empty shared state.
mkSharedState :: [TxId] -> SharedTxState PeerAddr TxId
mkSharedState txids = snd (internTxIds txids emptySharedTxState)

-- Construct a requested batch together with its cached key set.
mkRequestedTxBatch :: [TxKey] -> SizeInBytes -> RequestedTxBatch
mkRequestedTxBatch keys requestedTxBatchSize = RequestedTxBatch
  { requestedTxBatchSet = IntSet.fromList (map unTxKey keys)
  , requestedTxBatchSize
  }

-- Construct a leased tx entry owned by one peer.
mkTxEntry :: PeerAddr -> SizeInBytes -> Maybe TxAttemptState -> TxDecisionPolicy -> TxEntry PeerAddr
mkTxEntry peeraddr _txSize mAttempt policy = TxEntry
  { txLease = TxLeased peeraddr (addTime 10 now)
  , txAdvertiserCount = 1
  , txAttempts = maybe Map.empty (Map.singleton peeraddr) mAttempt
  , currentMaxInflightMultiplicity = txInflightMultiplicity policy
  }

-- Look up an interned key and fail fast in test setup code.
lookupKeyOrFail :: TxId -> SharedTxState PeerAddr TxId -> TxKey
lookupKeyOrFail txid st =
  case lookupTxKey txid st of
    Just txKey -> txKey
    Nothing    -> error "TxLogic.lookupKeyOrFail: missing tx key"

-- Look up an active tx entry and fail fast in test setup code.
lookupEntryOrFail :: TxKey -> SharedTxState PeerAddr TxId -> TxEntry PeerAddr
lookupEntryOrFail (TxKey k) st =
  case IntMap.lookup k (sharedTxTable st) of
    Just txEntry -> txEntry
    Nothing      -> error "TxLogic.lookupEntryOrFail: missing tx entry"

-- Look up a shared peer and fail fast in test setup code.
lookupPeerOrFail :: PeerAddr -> SharedTxState PeerAddr TxId -> SharedPeerState
lookupPeerOrFail peeraddr st =
  case Map.lookup peeraddr (sharedPeers st) of
    Just sharedPeerState -> sharedPeerState
    Nothing              -> error "TxLogic.lookupPeerOrFail: missing peer"

-- Shift proposed txids forward until the batch is disjoint from the shared intern table.
freshBatchAgainstSharedState :: SharedTxState PeerAddr TxId -> [(TxId, SizeInBytes)] -> [(TxId, SizeInBytes)]
freshBatchAgainstSharedState sharedState = reverse . snd . foldl' step (reserved, [])
  where
    reserved = Set.fromList (Map.keys (sharedTxIdToKey sharedState))

    step (used, acc) (txid, txSize) =
      let freshTxId = firstFreshTxId used txid in
      (Set.insert (getRawTxId freshTxId) used, (freshTxId, txSize) : acc)

-- Intern the given txids into the shared state and seed each into
-- sharedRetainedTxs.
seedRetainedTxids
  :: TxDecisionPolicy
  -> [(TxId, SizeInBytes)]
  -> SharedTxState PeerAddr TxId
  -> SharedTxState PeerAddr TxId
seedRetainedTxids policy entries st0 =
  stInterned {
    sharedRetainedTxs =
      foldl' (\r k -> retainedInsertMax k retainUntil r)
             (sharedRetainedTxs stInterned)
             retainedKeys
  }
  where
    retainUntil         = addTime (bufferedTxsMinLifetime policy) now
    (_, stInterned)     = internTxIds (fmap fst entries) st0
    retainedKeys        = [ unTxKey (lookupKeyOrFail txid stInterned)
                          | (txid, _) <- entries
                          ]

-- Intern the given txids and add an active sharedTxTable entry for each,
-- advertised by the given peer.
seedActiveTxidsForOtherPeer
  :: PeerAddr
  -> [(TxId, SizeInBytes)]
  -> SharedTxState PeerAddr TxId
  -> SharedTxState PeerAddr TxId
seedActiveTxidsForOtherPeer otherPeer entries st0 =
  stInterned {
    sharedTxTable =
      foldl' (\tbl k -> IntMap.insert k activeEntry tbl)
             (sharedTxTable stInterned)
             activeKeys,
    sharedPeers =
      Map.adjust augmentAdvertised otherPeer (sharedPeers stInterned)
  }
  where
    activeEntry = TxEntry {
        txLease = TxClaimable now,
        txAdvertiserCount = 1,
        txAttempts = Map.empty,
        currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }
    (_, stInterned) = internTxIds (fmap fst entries) st0
    activeKeys      = [ unTxKey (lookupKeyOrFail txid stInterned)
                      | (txid, _) <- entries
                      ]
    augmentAdvertised sps = sps {
        sharedPeerAdvertisedTxKeys =
          IntSet.union (sharedPeerAdvertisedTxKeys sps)
                       (IntSet.fromList activeKeys)
      }

-- Intern each requested txid and add an active sharedTxTable entry leased to
-- @peeraddr@ with a TxDownloading attempt. Entries whose Bool tag is True
-- (co-advertised) are also advertised by @otherPeer@, giving them
-- @txAdvertiserCount = 2@ so the omitted-and-released path leaves them
-- alive (instead of being reaped by dropDeadActiveKeys). Used by the
-- handleReceivedTxs property to build a coherent pre-state for one
-- outstanding request batch.
seedRequestedActiveTxids
  :: PeerAddr
  -> Maybe PeerAddr
  -> [((TxId, SizeInBytes), Bool)]
  -> SharedTxState PeerAddr TxId
  -> SharedTxState PeerAddr TxId
seedRequestedActiveTxids peeraddr otherPeerOpt tagged st0 =
  stFinal
  where
    entries         = fmap fst tagged
    (_, stInterned) = internTxIds (fmap fst entries) st0
    leaseUntil      = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now

    perKey :: [(Int, Bool)]
    perKey =
      [ ( unTxKey (lookupKeyOrFail txid stInterned)
        , coAdv && isJust otherPeerOpt
        )
      | ((txid, _), coAdv) <- tagged
      ]

    mkEntry coAdv = TxEntry {
        txLease = TxLeased peeraddr leaseUntil,
        txAdvertiserCount = if coAdv then 2 else 1,
        txAttempts = Map.singleton peeraddr TxDownloading,
        currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }

    stWithTable = stInterned {
        sharedTxTable =
          foldl' (\tbl (k, coAdv) -> IntMap.insert k (mkEntry coAdv) tbl)
                 (sharedTxTable stInterned)
                 perKey
      }

    peerAdvertisedAll  = IntSet.fromList (map fst perKey)
    otherAdvertisedAll = IntSet.fromList [ k | (k, True) <- perKey ]

    augmentWith addKeys sps = sps {
        sharedPeerAdvertisedTxKeys =
          IntSet.union (sharedPeerAdvertisedTxKeys sps) addKeys
      }

    stFinal = stWithTable {
        sharedPeers =
          let withPeer =
                Map.adjust (augmentWith peerAdvertisedAll) peeraddr
                           (sharedPeers stWithTable)
          in case otherPeerOpt of
               Just op | not (IntSet.null otherAdvertisedAll) ->
                 Map.adjust (augmentWith otherAdvertisedAll) op withPeer
               _ -> withPeer
      }

-- Find the first txid not present in the reserved set.
firstFreshTxId :: Set.Set RawTxId -> TxId -> TxId
firstFreshTxId used = go
  where
    go txid
      | Set.member (getRawTxId txid) used = go (txid + 1)
      | otherwise = txid

mkReceiveDuplicateFixture :: Int -> Int -> ReceiveDuplicateFixture
mkReceiveDuplicateFixture existingAdvertisers txidCount =
  ReceiveDuplicateFixture
    { rdfPeerAddr = targetPeer
    , rdfRequestedTxIds = fromIntegral txidCount
    , rdfTxidsAndSizes = txidsAndSizes
    , rdfPeerState =
        emptyPeerTxLocalState {
          peerRequestedTxIds = fromIntegral txidCount
        }
    , rdfSharedState =
        mkActiveSharedState allPeers ownerPeer existingPeers txidsAndSizes
    }
  where
    ownerPeer = 0
    targetPeer = existingAdvertisers
    existingPeers = [1 .. existingAdvertisers - 1]
    allPeers = ownerPeer : targetPeer : existingPeers
    txidsAndSizes = mkTxidsAndSizes txidCount

-- Prebuild an ack-only workload after all advertised txs have resolved into
-- retained entries.
mkResolvedAckFixture :: Int -> Int -> PeerActionFixture
mkResolvedAckFixture advertiserCount txidCount =
  PeerActionFixture
    { pafPeerAddr = targetPeer
    , pafPeerState =
        emptyPeerTxLocalState {
          peerUnacknowledgedTxIds = StrictSeq.fromList txKeys
        }
    , pafSharedState = sharedState
    }
  where
    ownerPeer = 0
    targetPeer = 1
    otherPeers = [2 .. advertiserCount - 1]
    allPeers = [0 .. advertiserCount - 1]
    txidsAndSizes = mkTxidsAndSizes txidCount
    sharedState0 =
      mkActiveSharedState allPeers ownerPeer (targetPeer : otherPeers) txidsAndSizes
    sharedState = retainAllActiveTxs sharedState0
    txKeys = fmap (`lookupKeyOrFail` sharedState0) (fmap fst txidsAndSizes)

-- | Compatibility alias for the previous benchmark helper name.
mkForeignRejectedFixture :: Int -> Int -> PeerActionFixture
mkForeignRejectedFixture = mkResolvedAckFixture

mkFanoutFixture :: Int -> Int -> FanoutFixture
mkFanoutFixture peerCount txidCount =
  FanoutFixture
    { ffPeers = peers
    , ffRequestedTxIds = fromIntegral txidCount
    , ffTxidsAndSizes = txidsAndSizes
    , ffInitialSharedState =
        mkActiveSharedState allPeers ownerPeer [] txidsAndSizes
    }
  where
    ownerPeer = 0
    peers = [1 .. peerCount]
    allPeers = ownerPeer : peers
    txidsAndSizes = mkTxidsAndSizes txidCount

runReceiveDuplicateLoop :: Int -> ReceiveDuplicateFixture -> IO ()
runReceiveDuplicateLoop iterations ReceiveDuplicateFixture
  { rdfPeerAddr
  , rdfRequestedTxIds
  , rdfTxidsAndSizes
  , rdfPeerState
  , rdfSharedState
  } =
    go iterations
  where
    go 0 = pure ()
    go n = do
      let result =
            handleReceivedTxIds
              (const False)
              now
              defaultTxDecisionPolicy
              rdfPeerAddr
              rdfRequestedTxIds
              rdfTxidsAndSizes
              rdfPeerState
              rdfSharedState
      _ <- evaluate (rnf result)
      go (n - 1)

runPeerActionLoop :: Int -> PeerActionFixture -> IO ()
runPeerActionLoop iterations PeerActionFixture
  { pafPeerAddr
  , pafPeerState
  , pafSharedState
  } =
    go iterations
  where
    go 0 = pure ()
    go n = do
      let result =
            nextPeerAction now defaultTxDecisionPolicy pafPeerAddr pafPeerState pafSharedState
      _ <- evaluate (rnf result)
      go (n - 1)

runFanoutLoop :: Int -> FanoutFixture -> IO ()
runFanoutLoop iterations FanoutFixture
  { ffPeers
  , ffRequestedTxIds
  , ffTxidsAndSizes
  , ffInitialSharedState
  } =
    go iterations
  where
    go 0 = pure ()
    go n = do
      _ <- evaluate (rnf roundResult)
      go (n - 1)

    roundResult =
      let (!peerStatesRev, !sharedStateAfterReceive) =
            foldl' receiveOne ([], ffInitialSharedState) ffPeers
          !sharedStateResolved = retainAllActiveTxs sharedStateAfterReceive
          (!ackResultsRev, !sharedStateAfterAck) =
            foldl' acknowledgeOne ([], sharedStateResolved) (reverse peerStatesRev)
      in (reverse peerStatesRev, reverse ackResultsRev, sharedStateAfterAck)

    receiveOne
      :: ([(PeerAddr, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
      -> PeerAddr
      -> ([(PeerAddr, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
    receiveOne (!peerStatesAcc, !sharedStateAcc) peeraddr =
      let peerState0 =
            emptyPeerTxLocalState {
              peerRequestedTxIds = ffRequestedTxIds
            }
          !(peerState', sharedStateAcc') =
            handleReceivedTxIds
              (const False)
              now
              defaultTxDecisionPolicy
              peeraddr
              ffRequestedTxIds
              ffTxidsAndSizes
              peerState0
              sharedStateAcc
      in ((peeraddr, peerState') : peerStatesAcc, sharedStateAcc')

    acknowledgeOne
      :: ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
      -> (PeerAddr, PeerTxLocalState (Tx TxId))
      -> ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
    acknowledgeOne (!ackResultsAcc, !sharedStateAcc) (peeraddr, peerState0) =
      let !(peerAction, peerState', sharedStateAcc') =
            nextPeerAction now defaultTxDecisionPolicy peeraddr peerState0 sharedStateAcc
      in ( (peeraddr, peerAction, peerState') : ackResultsAcc
         , sharedStateAcc'
         )

mkTxidsAndSizes :: Int -> [(TxId, SizeInBytes)]
mkTxidsAndSizes count =
  [ (txid, fromIntegral (128 + txid))
  | txid <- [1 .. count]
  ]

mkActiveSharedState
  :: [PeerAddr]
  -> PeerAddr
  -> [PeerAddr]
  -> [(TxId, SizeInBytes)]
  -> SharedTxState PeerAddr TxId
mkActiveSharedState allPeers ownerPeer resolvedAdvertisers txidsAndSizes =
  sharedState1 {
      sharedTxTable =
        IntMap.fromList
          [ (unTxKey txKey, mkEntry txKey)
          | (txid, _txSize) <- txidsAndSizes
          , let txKey = lookupKeyOrFail txid sharedState1
          ]
    , sharedPeers =
        Map.fromList
          [ (peeraddr, (mkSharedPeerState PeerIdle) {
                         sharedPeerAdvertisedTxKeys =
                           Map.findWithDefault IntSet.empty peeraddr peerAdvertisedKeys
                       })
          | peeraddr <- allPeers
          ]
    }
  where
    sharedState0 = emptySharedTxState
    sharedState1 = snd (internTxIds (fmap fst txidsAndSizes) sharedState0)

    advertisers = Set.fromList (ownerPeer : resolvedAdvertisers)
    peerAdvertisedKeys =
      Map.fromListWith IntSet.union
        [ (peeraddr, IntSet.singleton (unTxKey txKey))
        | (txid, _txSize) <- txidsAndSizes
        , let txKey = lookupKeyOrFail txid sharedState1
        , peeraddr <- Set.toList advertisers
        ]

    mkEntry _txKey = TxEntry
      { txLease = TxLeased ownerPeer (addTime 10 now)
      , txAdvertiserCount = Set.size advertisers
      , txAttempts = Map.empty
      , currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }

-- Resolve all active txs into retained entries so non-owner peers may safely
-- acknowledge their txids.
retainAllActiveTxs :: SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId
retainAllActiveTxs st@SharedTxState { sharedTxTable, sharedRetainedTxs, sharedGeneration } =
  st {
      sharedPeers =
        Map.map
          (\sharedPeerState ->
             sharedPeerState { sharedPeerAdvertisedTxKeys = IntSet.empty })
          (sharedPeers st),
      sharedTxTable = IntMap.empty,
      sharedRetainedTxs = IntMap.foldlWithKey' retainOne sharedRetainedTxs sharedTxTable,
      sharedGeneration = sharedGeneration + 1
    }
  where
    retainUntil = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now

    retainOne retainedAcc k _ =
      retainedInsertMax k retainUntil retainedAcc
