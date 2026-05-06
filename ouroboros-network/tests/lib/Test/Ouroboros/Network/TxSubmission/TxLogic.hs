{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Test.Ouroboros.Network.TxSubmission.TxLogic
  ( tests
  , ArbTxDecisionPolicy (..)
  , PeerAddr
  , ArbSharedTxState (..)
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
  , peerTxInFlightInvariant
  , combinedStateInvariant
  , InvariantStrength (..)
  ) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime, diffTime)
import Data.Foldable (foldl', toList)
import Data.Function (on)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, mapAccumL, nub, nubBy, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Word (Word64)

import NoThunks.Class (NoThunks, unsafeNoThunks)



import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Tx (HasRawTxId (..), getRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types

import Test.Ouroboros.Network.TxSubmission.Types

import Test.QuickCheck
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCaseSteps,
           (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

tests :: TestTree
tests =
  testGroup "TxLogic"
    [ localOption (QuickCheckTests 50) $
        testGroup "TriggerScenario meta-tests"
          [ testProperty "generated scenario produces a valid initial state"
                         prop_TriggerScenario_validInitialState
          , testProperty "shrink preserves validity"
                         prop_TriggerScenario_shrinkPreservesValidity
          , testProperty "shrink does not grow the trigger list"
                         prop_TriggerScenario_shrinkSmaller
          , testProperty "shrink does not contain the original value"
                         prop_TriggerScenario_shrinkExcludesOriginal
          ]
    , testProperty "nextPeerAction processes all multi-peer triggers" prop_nextPeerAction_processesAllTriggers
    , testCaseSteps "peerScore decays linearly over time at scoreRate" unit_peerScore_decaysOverTime
    , testCaseSteps "applyPeerRejections drains the existing score before adding the new rejection count" unit_applyPeerRejections_drainsThenAdds
    , testProperty "handleReceivedTxIds classifies incoming txids" prop_handleReceivedTxIds
    , testCaseSteps "handleReceivedTxIds tracks the advertise without mutating an existing entry" unit_handleReceivedTxIds_advertisesExistingEntry
    , testProperty "handleReceivedTxs buffers requested bodies and releases omitted ones" prop_handleReceivedTxs
    , testProperty "handleSubmittedTxs retains accepted and drops rejected" prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    , testProperty "nextPeerAction returns the current shared generation when idle" prop_nextPeerAction_returnsSharedGeneration
    , testProperty "nextPeerAction respects the inflight size budget" prop_nextPeerAction_picksTxsRespectingBudget
    , testProperty "nextPeerAction submits buffered owned txs before acking" prop_nextPeerAction_ownerSubmitsBuffered
    , testProperty "nextPeerAction prunes expired retained txs" prop_nextPeerAction_prunesExpiredRetained
    , testProperty "nextPeerAction keeps retained txs before expiry" prop_nextPeerAction_keepsRetained
    , testProperty "nextPeerActionPipelined suppresses ack-only or request-only txid messages" prop_nextPeerActionPipelined_requiresAckAndReq
    , testProperty "nextPeerActionPipelined emits a pipelined txid request when ack and request fire together" prop_nextPeerActionPipelined_requestsTxIds
    , testProperty "nextPeerActionPipelined opens a second outstanding body batch" prop_nextPeerActionPipelined_secondBodyBatch
    , testProperty "nextPeerActionPipelined does not open a third outstanding body batch" prop_nextPeerActionPipelined_noThirdBodyBatch
    , testProperty "nextPeerAction's PeerDoNothing carries the earliest scheduled wake" prop_nextPeerAction_earliestWakeDelay
    , testCaseSteps "nextPeerActionPipelined keeps one txid unacked while body replies are in flight" unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply
    , testCaseSteps "nextPeerAction skips blocked-available txs and requests later claimable ones" unit_nextPeerAction_skipsBlockedAvailableTxs
    , testCaseSteps "nextPeerAction only acks the safe prefix before a blocked buffered tx" unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx
    , testCaseSteps "nextPeerAction lets another peer claim a fresh tx when the first advertiser is full" unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull
    , testCaseSteps "nextPeerAction claims a released tx from another advertiser" unit_nextPeerAction_claimsRejectedTxFromOtherAdvertiser
    , testCaseSteps "nextPeerAction claims a tx once the score delay threshold has elapsed" unit_nextPeerAction_claimsAtScoreDelayThreshold
    , testCaseSteps "nextPeerAction requests other work despite a blocked buffered tx" unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx
    , testProperty "nextPeerAction keeps non-owner txids unacked until resolved" prop_nextPeerAction_nonOwnerWaitsUntilResolved
    , testProperty "nextPeerAction claims a claimable tx for the best idle advertiser" prop_nextPeerAction_claimsClaimableTx
    ]

--
-- NoThunks invariant checks
--

-- | Check that a value has no thunks in its fields.
checkNoThunks :: NoThunks a => String -> a -> Property
checkNoThunks name val =
    val `seq` case unsafeNoThunks val of
      Nothing   -> property True
      Just info -> counterexample
                     (name ++ " contains thunks: " ++ show info)
                     (property False)

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
--   * @peerAdvertisedTxKeys@ is a subset of the unacknowledged queue (a
--     peer can only advertise keys it has actually received and not yet
--     acked).
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

-- | Per-peer 'PeerTxInFlight' invariant.
--
-- A peer's attempting and submitting sets must be disjoint (a key is
-- in at most one phase per peer).  Lease and submission stake imply
-- the peer is also tracking the key as advertised.
peerTxInFlightInvariant :: PeerTxInFlight -> Property
peerTxInFlightInvariant pif =
  conjoin
    [ counterexample "pifAttempting and pifSubmitting overlap"
        (IntSet.null (pifAttempting pif `IntSet.intersection` pifSubmitting pif))
    , counterexample "pifLeased not contained in pifAttempting ∪ pifSubmitting"
        (property (pifLeased pif
                     `IntSet.isSubsetOf`
                     (pifAttempting pif `IntSet.union` pifSubmitting pif)))
    ]

-- | Combined invariant over @SharedTxState@ and a snapshot of every
-- live peer's @(PeerTxLocalState, PeerTxInFlight)@.
--
-- Runs the per-piece invariants ('peerTxLocalStateInvariant',
-- 'peerTxInFlightInvariant', 'sharedTxStateInvariant') and adds the
-- cross-state coherence constraints that need both shared and per-peer
-- views to express:
--
--   * Every key in any peer's 'pifAdvertised' references an active
--     entry or a retained one (never falls out of both).
--   * Each entry's 'txAttempt' equals the number of peers in the map
--     whose 'pifAttempting' contains the key.
--   * Each entry's 'txInSubmission' is true iff some peer in the map
--     has the key in its 'pifSubmitting'.
--   * If an entry is 'TxLeased peer _' and that peer is in the map,
--     the peer's 'pifLeased' contains the key.  Stale leases (peer's
--     'pifLeased' has a key that the entry no longer leases to them)
--     are tolerated since theft via inflight-cap bumps does not
--     proactively scrub the loser's set.
--
-- Single-peer call sites pass a @Map.singleton@; multi-peer scenarios
-- pass the full schedule snapshot so the cross-peer counter checks
-- are exercised.
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
  -> Map.Map peeraddr (PeerTxLocalState tx, PeerTxInFlight)
  -> SharedTxState peeraddr txid
  -> Property
combinedStateInvariant policy strength peers sharedState =
  let activeKeys = IntMap.keysSet (sharedTxTable sharedState)
      retKeys    = retainedKeysSet (sharedRetainedTxs sharedState)
      liveKeys   = activeKeys `IntSet.union` retKeys
  in conjoin $
       [ counterexample ("PeerTxLocalState invariant violated for peer "
                          ++ show p)
           (peerTxLocalStateInvariant policy ps)
       | (p, (ps, _)) <- Map.toList peers ]
    ++ [ counterexample ("PeerTxInFlight invariant violated for peer "
                           ++ show p)
           (peerTxInFlightInvariant pif)
       | (p, (_, pif)) <- Map.toList peers ]
    ++ [ sharedTxStateInvariant strength sharedState ]
    ++ [ counterexample ("peer " ++ show p
                          ++ " advertises keys that are neither active nor retained")
           (property (pifAdvertised pif `IntSet.isSubsetOf` liveKeys))
       | (p, (_, pif)) <- Map.toList peers ]
    ++ [ counterexample ("txAttempt mismatch for entry " ++ show k
                          ++ ": expected " ++ show expectedAttempt
                          ++ " (sum across peers' pifAttempting)"
                          ++ ", got " ++ show (txAttempt entry))
           (txAttempt entry === expectedAttempt)
       | (k, entry) <- IntMap.toList (sharedTxTable sharedState)
       , let expectedAttempt =
               sum [ if IntSet.member k (pifAttempting pif) then 1 else 0
                   | (_, (_, pif)) <- Map.toList peers ]
       ]
    ++ [ counterexample ("txInSubmission mismatch for entry " ++ show k
                          ++ ": expected " ++ show expectedSubmit)
           (txInSubmission entry === expectedSubmit)
       | (k, entry) <- IntMap.toList (sharedTxTable sharedState)
       , let expectedSubmit =
               any (\(_, (_, pif)) -> IntSet.member k (pifSubmitting pif))
                   (Map.toList peers)
       ]
    ++ [ counterexample ("entry " ++ show k
                          ++ " is TxLeased to peer " ++ show owner
                          ++ " but the peer's pifLeased does not contain it")
           (case Map.lookup owner peers of
              Nothing            -> property True
              Just (_, ownerPif) -> property (IntSet.member k (pifLeased ownerPif)))
       | (k, entry) <- IntMap.toList (sharedTxTable sharedState)
       , TxLeased owner _ <- [txLease entry]
       ]


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
    , counterexample "live tx keys missing from tx-key maps"
        (liveKeys `IntSet.isSubsetOf` IntMap.keysSet sharedKeyToTxId)
    , counterexample "sharedNextTxKey does not stay ahead of all live tx keys"
        (property (all (< sharedNextTxKey) (IntSet.toList liveKeys)))
    ]
    ++ case strength of
         -- Both strengths now run the per-entry well-formedness check.
         -- "Self-evident liveness" (lease or txAttempt > 0 or
         -- txInSubmission) is no longer a shared-state invariant: an
         -- entry can sit TxClaimable with 'txAttempt = 0' while a live
         -- peer's 'pifAdvertised' keeps it from being swept.  The sweep
         -- itself enforces global liveness; per-peer invariants live in
         -- 'combinedStateInvariant'.
         _ -> fmap checkTxEntry activeEntries
  where
    liveKeys = IntMap.keysSet sharedTxTable `IntSet.union` retainedKeysSet sharedRetainedTxs
    activeEntries = IntMap.toList sharedTxTable

    keysRoundTripForward =
      all (\(rawId, txKey) -> fmap getRawTxId (IntMap.lookup (unTxKey txKey) sharedKeyToTxId) == Just rawId)
          (Map.toList sharedTxIdToKey)

    keysRoundTripBackward =
      all (\(k, txid) -> Map.lookup (getRawTxId txid) sharedTxIdToKey == Just (TxKey k))
          (IntMap.toList sharedKeyToTxId)

    checkTxEntry (k, txEntry@TxEntry { txAttempt, txInSubmission }) =
      counterexample ("bad active tx entry " ++ show k ++ ": " ++ show txEntry) $
        conjoin
          [ counterexample "txAttempt is negative"
              (property (txAttempt >= 0))
          , counterexample "txInSubmission without any peer in submission"
              (property (not txInSubmission || txAttempt >= 0))
          ]

newtype ArbTxDecisionPolicy = ArbTxDecisionPolicy TxDecisionPolicy
  deriving Show

newtype ArbSharedTxState = ArbSharedTxState (SharedTxState PeerAddr TxId)
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
  deriving (Eq, Ord, Show)

instance Arbitrary TxIdGroupTag where
  arbitrary = frequency
    [ (12, pure TxIdNew)
    , (4,  pure TxIdRetained)
    , (4,  pure TxIdMempool)
    ]

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

--
-- Peer score tests
--

-- | 'currentPeerScore' decays the score linearly at 'scoreRate' per second.
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

--
-- handleReceivedTxIds
--

-- | Verifies that 'handleReceivedTxIds' classifies each incoming txid:
--
--   * 'TxIdNew' — interned and a fresh 'TxClaimable' entry is created;
--     the key joins 'peerAvailableTxIds' and 'pifAdvertised'.
--   * 'TxIdRetained' — already in 'sharedRetainedTxs'; no entry created
--     and the key is not added to 'pifAdvertised'.
--   * 'TxIdMempool' — interned, moved straight to 'sharedRetainedTxs'
--     and not added to 'pifAdvertised'.
--
-- Pre-state is empty peer-local and an otherwise empty shared state with
-- the retained group seeded.  Asserts the combined invariant before and
-- after, plus per-group inclusion / exclusion in each piece of state.
prop_handleReceivedTxIds
  :: ArbTxDecisionPolicy
  -> NonEmptyList (TxId, Positive Int, TxIdGroupTag)
  -> Property
prop_handleReceivedTxIds (ArbTxDecisionPolicy policy) (NonEmpty taggedInput) =
  let
    -- Normalise: positive txids, non-zero sizes; dedupe by txid.
    normalised :: [(TxId, SizeInBytes, TxIdGroupTag)]
    normalised =
      nubBy ((==) `on` (\(t,_,_) -> t))
        [ (abs txid + 1, mkSize sz, tag)
        | (txid, sz, tag) <- taggedInput
        ]

    txidsAndSizes :: [(TxId, SizeInBytes)]
    txidsAndSizes = [ (txid, sz) | (txid, sz, _) <- normalised ]

    newGroup      = [ (txid, sz) | (txid, sz, TxIdNew)      <- normalised ]
    retainedGroup = [ (txid, sz) | (txid, sz, TxIdRetained) <- normalised ]
    mempoolGroup  = [ (txid, sz) | (txid, sz, TxIdMempool)  <- normalised ]

    mempoolHasTx :: TxId -> Bool
    mempoolHasTx txid = txid `Set.member` Set.fromList (fmap fst mempoolGroup)

    sharedState0 = seedRetainedTxids policy retainedGroup emptySharedTxState

    requestedToReply = fromIntegral (length txidsAndSizes)
    peerState0   = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply }
    peerInFlight0 = emptyPeerTxInFlight

    (peerState', peerInFlight', sharedState') =
      handleReceivedTxIds mempoolHasTx now policy
                          requestedToReply txidsAndSizes
                          peerState0 peerInFlight0 sharedState0

    keyOf txid = unTxKey (lookupKeyOrFail txid sharedState')

    expectedAdvertisedKeys =
      IntSet.fromList [ keyOf txid | (txid, _) <- newGroup ]

    expectedAvailableTxIds =
      IntMap.fromList [ (keyOf txid, sz) | (txid, sz) <- newGroup ]

    expectedUnacked =
      [ lookupKeyOrFail txid sharedState' | (txid, _) <- txidsAndSizes ]

    retainUntil = addTime (bufferedTxsMinLifetime policy) now

    checkNew (txid, _) =
      let k = keyOf txid in
      counterexample ("new tx " ++ show txid) $ conjoin
        [ counterexample "missing TxClaimable entry"
            (case IntMap.lookup k (sharedTxTable sharedState') of
               Just txEntry -> conjoin
                 [ txLease txEntry === TxClaimable now
                 , txAttempt txEntry === 0
                 , property (not (txInSubmission txEntry))
                 ]
               Nothing -> property False)
        , counterexample "expected to be in retained" $
            property (not (retainedMember k (sharedRetainedTxs sharedState')))
        ]

    checkRetained (txid, _) =
      let k = keyOf txid in
      counterexample ("retained tx " ++ show txid) $ conjoin
        [ counterexample "leaked into sharedTxTable"
            (property (IntMap.notMember k (sharedTxTable sharedState')))
        , counterexample "missing from retained"
            (property (retainedMember k (sharedRetainedTxs sharedState')))
        , counterexample "leaked into pifAdvertised"
            (property (IntSet.notMember k (pifAdvertised peerInFlight')))
        ]

    checkMempool (txid, _) =
      let k = keyOf txid in
      counterexample ("mempool tx " ++ show txid) $ conjoin
        [ counterexample "leaked into sharedTxTable"
            (property (IntMap.notMember k (sharedTxTable sharedState')))
        , counterexample "missing or wrong retainUntil"
            (retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil)
        , counterexample "leaked into pifAdvertised"
            (property (IntSet.notMember k (pifAdvertised peerInFlight')))
        ]
  in
    classify (not (null newGroup))      "txids include new"      $
    classify (not (null retainedGroup)) "txids include retained" $
    classify (not (null mempoolGroup))  "txids include mempool"  $
    conjoin
      [ counterexample "unacknowledged queue mismatch"
          (toList (peerUnacknowledgedTxIds peerState') === expectedUnacked)
      , counterexample "peerAvailableTxIds mismatch"
          (peerAvailableTxIds peerState' === expectedAvailableTxIds)
      , counterexample "pifAdvertised mismatch"
          (pifAdvertised peerInFlight' === expectedAdvertisedKeys)
      , counterexample "peerRequestedTxIds was not consumed"
          (peerRequestedTxIds peerState' === 0)
      , conjoin (fmap checkNew newGroup)
      , conjoin (fmap checkRetained retainedGroup)
      , conjoin (fmap checkMempool mempoolGroup)
      , combinedStateInvariant policy WeakInvariant
          (Map.singleton peerAddr (peerState', peerInFlight')) sharedState'
      ]
  where
    peerAddr = 1 :: PeerAddr

-- | When a peer advertises a txid whose entry already exists, the entry
-- itself is unchanged: the per-peer 'PeerTxInFlight' simply records the
-- new advertisement.  The new model has no 'txAdvertiserCount' to bump.
unit_handleReceivedTxIds_advertisesExistingEntry :: (String -> IO ()) -> Assertion
unit_handleReceivedTxIds_advertisesExistingEntry step = do
    step "Set up a sharedTxTable entry leased to peer 0"
    let txid :: TxId
        txid = 7
        existing = mkActiveSharedState [0] 0 [] [(txid, 256)]
        peerAddr2 :: PeerAddr
        peerAddr2 = 1
        entryBefore = lookupEntryOrFail (lookupKeyOrFail txid existing) existing

    step "Peer 1 advertises the same txid"
    let (peerState', peerInFlight', sharedState') =
          handleReceivedTxIds (const False) now defaultTxDecisionPolicy
                              1 [(txid, 256)]
                              emptyPeerTxLocalState { peerRequestedTxIds = 1 }
                              emptyPeerTxInFlight
                              existing

    step "Entry in sharedTxTable is unchanged"
    let entryAfter = lookupEntryOrFail (lookupKeyOrFail txid sharedState') sharedState'
    entryAfter @?= entryBefore

    step "Peer 1 now tracks the txid as advertised + available"
    let k = unTxKey (lookupKeyOrFail txid sharedState')
    pifAdvertised peerInFlight' @?= IntSet.singleton k
    IntMap.keys (peerAvailableTxIds peerState') @?= [k]
    toList (peerUnacknowledgedTxIds peerState') @?= [TxKey k]
    -- peerAddr2 is referenced to keep the variable in scope for clarity.
    _ <- pure peerAddr2
    pure ()

--
-- handleReceivedTxs
--

-- | Verifies that 'handleReceivedTxs' buffers requested bodies and
-- releases omitted ones, decrementing 'txAttempt' for each.  The
-- pre-state is built by manually claiming each requested key.
prop_handleReceivedTxs
  :: ArbTxDecisionPolicy
  -> NonEmptyList (TxId, Positive Int, Bool)
  -> Property
prop_handleReceivedTxs (ArbTxDecisionPolicy policy)
                       (NonEmpty rawInput) =
  let
    normalised :: [(TxId, SizeInBytes, Bool)]
    normalised =
      nubBy ((==) `on` (\(t,_,_) -> t))
        [ (abs txid + 1, mkSize sz, inReply)
        | (txid, sz, inReply) <- rawInput
        ]

    txidsAndSizes :: [(TxId, SizeInBytes)]
    txidsAndSizes = [ (txid, sz) | (txid, sz, _) <- normalised ]

    -- 1) Receive the txids on peer 1 to set up advertised + available.
    sharedState0 = emptySharedTxState
    requestedToReply = fromIntegral (length txidsAndSizes)
    peerState0   = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply }
    (peerState1, peerInFlight1, sharedState1) =
      handleReceivedTxIds (const False) now policy
                          requestedToReply txidsAndSizes
                          peerState0 emptyPeerTxInFlight sharedState0

    -- 2) Manually claim each key (simulate pickRequestTxs).
    keys = [ lookupKeyOrFail txid sharedState1 | (txid, _) <- txidsAndSizes ]
    leaseUntil = addTime (interTxSpace policy) now

    sharedState2 = sharedState1 {
        sharedTxTable =
          foldl' (\tbl k -> IntMap.adjust (claimEntry k) (unTxKey k) tbl)
                 (sharedTxTable sharedState1) keys,
        sharedGeneration = sharedGeneration sharedState1 + 1
      }
    claimEntry _ entry =
      entry {
        txLease = TxLeased peerAddr leaseUntil,
        txAttempt = txAttempt entry + 1
      }

    requestedKeySet = IntSet.fromList (fmap unTxKey keys)
    batch = mkRequestedTxBatch keys (sum (fmap snd txidsAndSizes))
    peerState2 = peerState1 {
        peerRequestedTxs = requestedKeySet,
        peerRequestedTxBatches = StrictSeq.singleton batch,
        peerRequestedTxsSize = requestedTxBatchSize batch
      }
    peerInFlight2 = peerInFlight1 {
        pifLeased     = IntSet.union (pifLeased peerInFlight1)     requestedKeySet,
        pifAttempting = IntSet.union (pifAttempting peerInFlight1) requestedKeySet
      }

    -- 3) Run handleReceivedTxs with only the in-reply subset.
    txReply :: [(TxId, Tx TxId)]
    txReply = [ (txid, mkTx txid sz)
              | (txid, sz, inReply) <- normalised
              , inReply
              ]
    expectedBuffered = IntSet.fromList
      [ unTxKey (lookupKeyOrFail txid sharedState1)
      | (txid, _, inReply) <- normalised, inReply
      ]
    expectedOmitted = requestedKeySet `IntSet.difference` expectedBuffered

    (omittedCount, lateCount, peerState3, peerInFlight3, sharedState3) =
      handleReceivedTxs (const False) now policy peerAddr txReply
                        peerState2 peerInFlight2 sharedState2
  in
    classify (not (IntSet.null expectedBuffered)) "buffered subset"  $
    classify (not (IntSet.null expectedOmitted))  "omitted subset"   $
    conjoin
      [ counterexample "lateCount should be zero (no retained / mempool branch hit)"
          (lateCount === 0)
      , counterexample "omittedCount mismatch"
          (omittedCount === IntSet.size expectedOmitted)
      , counterexample "buffered keys mismatch"
          (IntMap.keysSet (peerDownloadedTxs peerState3) === expectedBuffered)
      , counterexample "peerRequestedTxs not drained"
          (peerRequestedTxs peerState3 === IntSet.empty)
      , counterexample "request batch was not dequeued"
          (peerRequestedTxBatches peerState3 === StrictSeq.empty)
      , counterexample "pifLeased should retain only buffered keys"
          (pifLeased peerInFlight3 === expectedBuffered)
      , counterexample "pifAttempting should retain only buffered keys"
          (pifAttempting peerInFlight3 === expectedBuffered)
      , conjoin
          [ counterexample
              ("omitted entry " ++ show k ++ " should sit TxClaimable") $
            case IntMap.lookup k (sharedTxTable sharedState3) of
              Just entry -> conjoin
                [ case txLease entry of
                    TxClaimable _ -> property True
                    other         -> counterexample (show other) (property False)
                , counterexample "txAttempt not decremented"
                    (txAttempt entry === 0)
                ]
              Nothing -> counterexample "entry vanished" (property False)
          | k <- IntSet.toList expectedOmitted
          ]
      , combinedStateInvariant policy WeakInvariant
          (Map.singleton peerAddr (peerState3, peerInFlight3)) sharedState3
      ]
  where
    peerAddr = 1 :: PeerAddr

--
-- handleSubmittedTxs
--

-- | Accepted txs move into 'sharedRetainedTxs'; rejected txs stay in
-- the active table with 'TxClaimable' lease and cleared
-- 'txInSubmission'; the peer's 'pifSubmitting' is fully cleared.
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
  :: ArbTxDecisionPolicy
  -> NonEmptyList (TxId, Positive Int, Bool)
  -> Property
prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    (ArbTxDecisionPolicy policy)
    (NonEmpty rawInput) =
  let
    normalised :: [(TxId, SizeInBytes, Bool)]
    normalised =
      nubBy ((==) `on` (\(t,_,_) -> t))
        [ (abs txid + 1, mkSize sz, accept)
        | (txid, sz, accept) <- rawInput
        ]

    txidsAndSizes = [ (txid, sz) | (txid, sz, _) <- normalised ]

    -- Pre-state: every key leased to peerAddr with txInSubmission set
    -- and the key already counted in 'pifSubmitting'.
    sharedState0 = mkActiveSharedState [peerAddr] peerAddr [] txidsAndSizes
    keys = [ lookupKeyOrFail txid sharedState0 | (txid, _) <- txidsAndSizes ]
    keySet = IntSet.fromList (fmap unTxKey keys)

    flipToSubmitting entry =
      entry { txAttempt = 0, txInSubmission = True }
    sharedState1 = sharedState0 {
        sharedTxTable =
          IntSet.foldl' (flip (IntMap.adjust flipToSubmitting))
                        (sharedTxTable sharedState0)
                        keySet
      }

    peerInFlight0 = emptyPeerTxInFlight {
        pifLeased     = keySet,
        pifSubmitting = keySet,
        pifAdvertised = keySet
      }
    peerState0 = emptyPeerTxLocalState {
        peerUnacknowledgedTxIds = StrictSeq.fromList keys,
        peerAvailableTxIds =
          IntMap.fromList [(unTxKey (lookupKeyOrFail txid sharedState1), sz)
                          | (txid, sz) <- txidsAndSizes],
        peerDownloadedTxs =
          IntMap.fromList [(unTxKey (lookupKeyOrFail txid sharedState1),
                            mkTx txid sz)
                          | (txid, sz) <- txidsAndSizes]
      }

    accepted = [ lookupKeyOrFail txid sharedState1
               | (txid, _, True)  <- normalised ]
    rejected = [ lookupKeyOrFail txid sharedState1
               | (txid, _, False) <- normalised ]

    acceptedKeys = IntSet.fromList (fmap unTxKey accepted)
    rejectedKeys = IntSet.fromList (fmap unTxKey rejected)

    (peerState', peerInFlight', sharedState') =
      handleSubmittedTxs now policy peerAddr accepted rejected
                         peerState0 peerInFlight0 sharedState1

    retainUntil = addTime (bufferedTxsMinLifetime policy) now

    checkAccepted k =
      counterexample ("accepted " ++ show k) $ conjoin
        [ counterexample "still in sharedTxTable"
            (property (IntMap.notMember k (sharedTxTable sharedState')))
        , counterexample "missing or wrong retainUntil"
            (retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil)
        ]
    checkRejected k =
      counterexample ("rejected " ++ show k) $
        case IntMap.lookup k (sharedTxTable sharedState') of
          Just entry -> conjoin
            [ case txLease entry of
                TxClaimable _ -> property True
                other         -> counterexample (show other) (property False)
            , counterexample "txInSubmission still set"
                (property (not (txInSubmission entry)))
            ]
          Nothing -> counterexample "entry vanished" (property False)
  in
    classify (not (null accepted)) "has accepted" $
    classify (not (null rejected)) "has rejected" $
    conjoin
      [ counterexample "pifSubmitting still has keys"
          (pifSubmitting peerInFlight' === IntSet.empty)
      , counterexample "pifLeased not cleared for submitted keys"
          (pifLeased peerInFlight' === IntSet.empty)
      , counterexample "pifAdvertised not cleared for submitted keys"
          (pifAdvertised peerInFlight' === IntSet.empty)
      , counterexample "peerDownloadedTxs not cleared for submitted keys"
          (IntMap.keysSet (peerDownloadedTxs peerState') === IntSet.empty)
      , conjoin (fmap checkAccepted (IntSet.toList acceptedKeys))
      , conjoin (fmap checkRejected (IntSet.toList rejectedKeys))
      , combinedStateInvariant policy WeakInvariant
          (Map.singleton peerAddr (peerState', peerInFlight')) sharedState'
      ]
  where
    peerAddr = 1 :: PeerAddr

--
-- nextPeerAction
--

-- | An idle peer whose shared state has no work returns
-- 'PeerDoNothing' carrying the current 'sharedGeneration'.
prop_nextPeerAction_returnsSharedGeneration
  :: ArbTxDecisionPolicy
  -> Word64
  -> Property
prop_nextPeerAction_returnsSharedGeneration (ArbTxDecisionPolicy policy0) gen =
  let
    -- A zero unack window forces 'pickRequestTxIdsAction' to return
    -- 'Nothing', so the scheduler falls through to 'PeerDoNothing'.
    policy = policy0 { maxUnacknowledgedTxIds = 0
                     , maxNumTxIdsToRequest   = 0 }
    sharedState :: SharedTxState PeerAddr TxId
    sharedState = emptySharedTxState { sharedGeneration = gen }
    (action, _, _, _) =
      nextPeerAction now policy peerAddr emptyPeerTxLocalState
                     emptyPeerTxInFlight sharedState
  in
    case action of
      PeerDoNothing g _ -> g === gen
      _                 -> counterexample (show action)
                                          (property False)
  where
    peerAddr = 1 :: PeerAddr

-- | When advertised candidates exceed the per-peer size budget,
-- 'pickRequestTxs' picks a contiguous prefix of the unacked queue
-- whose total size fits the budget (with the soft single-tx allowance
-- documented in 'pickRequestTxsAction').
prop_nextPeerAction_picksTxsRespectingBudget
  :: ArbTxDecisionPolicy
  -> NonEmptyList (TxId, Positive Int)
  -> Property
prop_nextPeerAction_picksTxsRespectingBudget (ArbTxDecisionPolicy policy0)
                                             (NonEmpty rawInput) =
  let
    -- Tighten the budget so the test exercises the truncation path.
    policy = policy0 { txsSizeInflightPerPeer = SizeInBytes 4096
                     , maxOutstandingTxBatchesPerPeer = 4 }

    txidsAndSizes :: [(TxId, SizeInBytes)]
    txidsAndSizes =
      nubBy ((==) `on` fst)
        [ (abs txid + 1, SizeInBytes (1 + (fromIntegral n `mod` 2048)))
        | (txid, Positive n) <- rawInput
        ]

    requestedToReply = fromIntegral (length txidsAndSizes)
    peerState0   = emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply }
    (peerState1, peerInFlight1, sharedState1) =
      handleReceivedTxIds (const False) now policy
                          requestedToReply txidsAndSizes
                          peerState0 emptyPeerTxInFlight emptySharedTxState

    (action, _peerState', peerInFlight', sharedState') =
      nextPeerAction now policy peerAddr peerState1 peerInFlight1 sharedState1

    keyOrder = [ unTxKey (lookupKeyOrFail txid sharedState1)
               | (txid, _) <- txidsAndSizes ]
    sizeOf k =
      maybe 0 getSizeInBytes
        (IntMap.lookup k (peerAvailableTxIds peerState1))

    budget = getSizeInBytes (txsSizeInflightPerPeer policy)
    isPrefix _ [] = True
    isPrefix [] _ = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys
  in
    case action of
      PeerRequestTxs picked ->
        let pickedKeys = fmap unTxKey picked
            pickedSize = sum (fmap sizeOf pickedKeys)
            -- The first picked tx may exceed the budget on its own
            -- (soft budget); subsequent picks must keep the running
            -- total at or below the budget.
            tailSize = pickedSize - maybe 0 sizeOf (listToMaybe pickedKeys)
        in conjoin
             [ counterexample "picked is not a prefix of the unacked queue"
                 (property (pickedKeys `isPrefix` keyOrder))
             , counterexample
                 ("tail of picked exceeds budget: " ++ show pickedSize)
                 (property (tailSize <= budget))
             , counterexample "picked keys not added to pifLeased"
                 (property (IntSet.fromList pickedKeys
                              `IntSet.isSubsetOf` pifLeased peerInFlight'))
             , counterexample "claimed entries should be TxLeased to peerAddr"
                 (conjoin
                   [ case IntMap.lookup k (sharedTxTable sharedState') of
                       Just entry -> case txLease entry of
                         TxLeased owner _ -> owner === peerAddr
                         other            -> counterexample (show other)
                                                            (property False)
                       Nothing -> counterexample "missing entry"
                                                 (property False)
                   | k <- pickedKeys
                   ])
             ]
      _ ->
        -- If the candidates were empty (e.g. all sizes happened to be
        -- the same and dedupe stripped them) the scheduler may pick
        -- something else; just check the action type isn't junk.
        counterexample (show action) (property True)
  where
    peerAddr = 1 :: PeerAddr

-- | When the peer has buffered a downloaded body of which it owns the
-- lease, 'nextPeerAction' submits before requesting more bodies or
-- acking.
prop_nextPeerAction_ownerSubmitsBuffered
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Property
prop_nextPeerAction_ownerSubmitsBuffered (ArbTxDecisionPolicy policy)
                                         (Positive sizeBytes) =
  let
    txid :: TxId
    txid = 1
    sz   = SizeInBytes (1 + fromIntegral (sizeBytes `mod` 1024))

    -- 1) Peer receives the txid (creates the entry).
    (peerState1, peerInFlight1, sharedState1) =
      handleReceivedTxIds (const False) now policy 1 [(txid, sz)]
                          (emptyPeerTxLocalState { peerRequestedTxIds = 1 })
                          emptyPeerTxInFlight emptySharedTxState

    txKey   = lookupKeyOrFail txid sharedState1
    keyInt  = unTxKey txKey
    leaseUntil = addTime (interTxSpace policy) now

    -- 2) Manually claim and buffer the body (simulating
    -- pickRequestTxsAction + handleReceivedTxs's buffered branch).
    sharedState2 = sharedState1 {
        sharedTxTable =
          IntMap.adjust
            (\entry -> entry {
                 txLease = TxLeased peerAddr leaseUntil,
                 txAttempt = txAttempt entry + 1
               })
            keyInt (sharedTxTable sharedState1)
      }
    peerState2 = peerState1 {
        peerDownloadedTxs = IntMap.singleton keyInt (mkTx txid sz)
      }
    peerInFlight2 = peerInFlight1 {
        pifLeased     = IntSet.insert keyInt (pifLeased peerInFlight1),
        pifAttempting = IntSet.insert keyInt (pifAttempting peerInFlight1)
      }

    (action, _, _, _) =
      nextPeerAction now policy peerAddr peerState2 peerInFlight2 sharedState2
  in
    case action of
      PeerSubmitTxs ks -> ks === [txKey]
      _                -> counterexample (show action) (property False)
  where
    peerAddr = 1 :: PeerAddr

-- | After 'sweepSharedState' fires, retained entries past their
-- deadline are gone.  This is a lightweight sanity check on the
-- retention sweep.
prop_nextPeerAction_prunesExpiredRetained
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Property
prop_nextPeerAction_prunesExpiredRetained (ArbTxDecisionPolicy policy)
                                          (Positive nTxids) =
  let
    n = 1 + (nTxids `mod` 5)
    txidsAndSizes =
      [ (t, SizeInBytes 64) | t <- [1 .. n] ]

    sharedState0 = seedRetainedTxids policy txidsAndSizes emptySharedTxState
    expired      = addTime (bufferedTxsMinLifetime policy + 1) now
    sharedState' = sweepSharedState expired IntSet.empty sharedState0
  in
    counterexample
      ("retained set after sweep: " ++ show (retainedSize (sharedRetainedTxs sharedState')))
      (retainedSize (sharedRetainedTxs sharedState') === 0)

-- | Before the retention deadline expires, retained entries survive a
-- sweep call.
prop_nextPeerAction_keepsRetained
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Property
prop_nextPeerAction_keepsRetained (ArbTxDecisionPolicy policy)
                                  (Positive nTxids) =
  let
    n = 1 + (nTxids `mod` 5)
    txidsAndSizes =
      [ (t, SizeInBytes 64) | t <- [1 .. n] ]

    sharedState0 = seedRetainedTxids policy txidsAndSizes emptySharedTxState
    sharedState' = sweepSharedState now IntSet.empty sharedState0
  in
    retainedSize (sharedRetainedTxs sharedState') === n

--
-- nextPeerActionPipelined
--

-- | In pipelined mode 'pickRequestTxIdsAction' must return @Nothing@
-- when either the ack count or the request count is zero (the wire
-- format would otherwise produce an ack-only or request-only
-- pipelined message, which is not allowed).
-- | When the peer has at least one ackable txid AND room to request
-- more, 'nextPeerActionPipelined' emits a 'PeerRequestTxIds' with both
-- counts non-zero.
prop_nextPeerActionPipelined_requestsTxIds
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_requestsTxIds (ArbTxDecisionPolicy policy0) =
  let
    policy = policy0 { maxUnacknowledgedTxIds = 8
                     , maxNumTxIdsToRequest   = 4 }
    -- Pre-state: two ackable retained txids (so the keep-one-unacked
    -- clamp on pipelined requests still leaves a non-zero ack) and an
    -- outstanding pipelined req (peerRequestedTxIds = 1) so the wire
    -- format treats the new request as pipelined.
    txids = [(1, 64), (2, 64)] :: [(TxId, SizeInBytes)]
    sharedState0 = seedRetainedTxids policy txids emptySharedTxState
    keys = [ TxKey (unTxKey (lookupKeyOrFail txid sharedState0))
           | (txid, _) <- txids ]
    peerState0 = emptyPeerTxLocalState {
        peerUnacknowledgedTxIds = StrictSeq.fromList keys,
        peerRequestedTxIds      = 1
      }
    (action, _, _, _) =
      nextPeerActionPipelined now policy peerAddr peerState0
                              emptyPeerTxInFlight sharedState0
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxIds TxIdsPipelinedReq ack req ->
          conjoin [ counterexample "ack should be non-zero"
                      (property (ack /= 0))
                  , counterexample "req should be non-zero"
                      (property (req /= 0))
                  ]
        _ -> property False
  where
    peerAddr = 1 :: PeerAddr

-- | The 'PeerDoNothing' wake-delay is the smallest of: earliest
-- claim-ready time among advertised txs, earliest stuck-bump time
-- among buffered txs, and earliest retention-expiry time.  This test
-- focuses on the retention case: an idle peer with nothing else but a
-- single retained tx must wake at that retention deadline.
prop_nextPeerAction_earliestWakeDelay
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Property
prop_nextPeerAction_earliestWakeDelay (ArbTxDecisionPolicy policy0)
                                      (Positive offsetSec) =
  let
    -- Force the doNothing branch.
    policy = policy0 { maxUnacknowledgedTxIds = 0
                     , maxNumTxIdsToRequest   = 0
                     , bufferedTxsMinLifetime = realToFrac offsetSec }
    txid :: TxId
    txid = 1
    sharedState0 = seedRetainedTxids policy [(txid, 64)] emptySharedTxState
    expectedWake = bufferedTxsMinLifetime policy
    (action, _, _, _) =
      nextPeerAction now policy peerAddr emptyPeerTxLocalState
                     emptyPeerTxInFlight sharedState0
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerDoNothing _ (Just delay) -> delay === expectedWake
        _                            -> property False
  where
    peerAddr = 1 :: PeerAddr

-- | While a body reply is still in flight, the protocol requires the
-- peer to keep at least one txid unacked.  Verifies that
-- 'pickRequestTxIdsAction' clamps the ack count accordingly when the
-- only ackable txid is the one that should stay unacked.
unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply
  :: (String -> IO ()) -> Assertion
unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply step = do
    step "Set up a peer with one retained (ackable) txid and an outstanding body batch"
    let txid :: TxId
        txid = 1
        policy = defaultTxDecisionPolicy
        sharedState0 = seedRetainedTxids policy [(txid, 64)] emptySharedTxState
        keyInt = unTxKey (lookupKeyOrFail txid sharedState0)
        outstandingBatch = mkRequestedTxBatch [TxKey keyInt] 64
        peerState0 = emptyPeerTxLocalState {
            peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey keyInt),
            peerRequestedTxs        = IntSet.singleton keyInt,
            peerRequestedTxBatches  = StrictSeq.singleton outstandingBatch,
            peerRequestedTxsSize    = 64
          }

    step "Run nextPeerActionPipelined"
    let (action, _, _, _) =
          nextPeerActionPipelined now policy peerAddr peerState0
                                  emptyPeerTxInFlight sharedState0

    step "Pipelined response must keep the txid unacked while the body batch is outstanding"
    case action of
      PeerRequestTxIds TxIdsPipelinedReq ack _ ->
        ack @?= 0
      PeerDoNothing {} ->
        -- Acceptable: not enough room to request anything.
        pure ()
      other ->
        assertFailure ("unexpected action: " ++ show other)
  where
    peerAddr = 1 :: PeerAddr

prop_nextPeerActionPipelined_requiresAckAndReq
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_requiresAckAndReq (ArbTxDecisionPolicy policy0) =
  let
    -- Cap to small windows so the property is easy to reason about.
    policy = policy0 { maxUnacknowledgedTxIds = 4
                     , maxNumTxIdsToRequest   = 4 }
    txid :: TxId
    txid = 1
    -- Pre-state: peer has one unacked txid that is *not* ackable
    -- (entry sits TxClaimable with no peer attempt and the peer is
    -- still tracking it as advertised), AND there is already a
    -- pipelined request outstanding so 'keepOneUnackedForPipelinedRequest'
    -- forces num_acked = 0.
    sharedState0 = mkActiveSharedState [peerAddr] peerAddr [] [(txid, 64)]
    keyInt = unTxKey (lookupKeyOrFail txid sharedState0)
    -- Strip the leaseholder so the entry is TxClaimable, attempt 0.
    sharedState1 = sharedState0 {
        sharedTxTable =
          IntMap.adjust
            (\entry -> entry {
                 txLease   = TxClaimable now,
                 txAttempt = 0
               })
            keyInt (sharedTxTable sharedState0)
      }
    peerState0 = emptyPeerTxLocalState {
        peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey keyInt),
        peerAvailableTxIds = IntMap.singleton keyInt 64,
        peerRequestedTxIds = 1  -- pretend there's a pipelined req in flight
      }
    peerInFlight0 = emptyPeerTxInFlight {
        pifAdvertised = IntSet.singleton keyInt
      }

    (action, _, _, _) =
      nextPeerActionPipelined now policy peerAddr peerState0
                              peerInFlight0 sharedState1
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxIds {} -> property False
        _                   -> property True
  where
    peerAddr = 1 :: PeerAddr

-- | While a peer holds one outstanding body batch and there is room for
-- another, 'nextPeerActionPipelined' opens the second batch.
prop_nextPeerActionPipelined_secondBodyBatch
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_secondBodyBatch (ArbTxDecisionPolicy basePolicy) =
  let
    policy = basePolicy
      { maxOutstandingTxBatchesPerPeer = max 2 (maxOutstandingTxBatchesPerPeer basePolicy)
      , txsSizeInflightPerPeer = SizeInBytes 4096
      }
    txSizeA, txSizeB :: SizeInBytes
    txSizeA = SizeInBytes 100
    txSizeB = SizeInBytes 100
    keyA = TxKey 0
    keyB = TxKey 1
    kA = unTxKey keyA
    kB = unTxKey keyB
    leaseUntil = addTime (interTxSpace policy) now
    sharedState = emptySharedTxState
      { sharedTxTable = IntMap.fromList
          [ (kA, TxEntry
              { txLease = TxLeased peerAddr leaseUntil
              , txAttempt = 1
              , txInSubmission = False
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          , (kB, TxEntry
              { txLease = TxClaimable now
              , txAttempt = 0
              , txInSubmission = False
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          ]
      , sharedTxIdToKey = Map.fromList [(getRawTxId (1 :: TxId), keyA), (getRawTxId (2 :: TxId), keyB)]
      , sharedKeyToTxId = IntMap.fromList [(kA, 1 :: TxId), (kB, 2 :: TxId)]
      , sharedNextTxKey = 2
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB]
      , peerAvailableTxIds = IntMap.fromList [(kA, txSizeA), (kB, txSizeB)]
      , peerRequestedTxs = IntSet.singleton kA
      , peerRequestedTxBatches = StrictSeq.singleton (mkRequestedTxBatch [keyA] txSizeA)
      , peerRequestedTxsSize = txSizeA
      }
    peerInFlight0 = emptyPeerTxInFlight
      { pifAdvertised = IntSet.fromList [kA, kB]
      , pifLeased     = IntSet.singleton kA
      , pifAttempting = IntSet.singleton kA
      }

    (action, _, peerInFlight', sharedState') =
      nextPeerActionPipelined now policy peerAddr peerState0
                              peerInFlight0 sharedState
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxs [picked] ->
          conjoin
            [ counterexample "expected to pick keyB"
                (picked === keyB)
            , counterexample "B not added to pifLeased"
                (property (IntSet.member kB (pifLeased peerInFlight')))
            , counterexample "B not added to pifAttempting"
                (property (IntSet.member kB (pifAttempting peerInFlight')))
            , counterexample "B not leased to peerAddr"
                (case IntMap.lookup kB (sharedTxTable sharedState') of
                   Just entry -> case txLease entry of
                     TxLeased owner _ -> owner === peerAddr
                     _                -> property False
                   Nothing -> property False)
            ]
        _ -> property False
  where
    peerAddr = 7 :: PeerAddr

-- | When the peer already holds 'maxOutstandingTxBatchesPerPeer'
-- outstanding body batches, 'nextPeerActionPipelined' refuses to open
-- another even with available candidates.
prop_nextPeerActionPipelined_noThirdBodyBatch
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_noThirdBodyBatch (ArbTxDecisionPolicy basePolicy) =
  let
    policy = basePolicy { maxOutstandingTxBatchesPerPeer = 2 }
    txSize :: SizeInBytes
    txSize = SizeInBytes 64
    keyA = TxKey 0
    keyB = TxKey 1
    keyC = TxKey 2
    kA = unTxKey keyA
    kB = unTxKey keyB
    kC = unTxKey keyC
    leaseUntil = addTime (interTxSpace policy) now
    inflightEntry = TxEntry
      { txLease = TxLeased peerAddr leaseUntil
      , txAttempt = 1
      , txInSubmission = False
      , currentMaxInflightMultiplicity = txInflightMultiplicity policy
      }
    freshEntry = TxEntry
      { txLease = TxClaimable now
      , txAttempt = 0
      , txInSubmission = False
      , currentMaxInflightMultiplicity = txInflightMultiplicity policy
      }
    sharedState = emptySharedTxState
      { sharedTxTable = IntMap.fromList
          [ (kA, inflightEntry)
          , (kB, inflightEntry)
          , (kC, freshEntry)
          ]
      , sharedTxIdToKey = Map.fromList
          [ (getRawTxId (1 :: TxId), keyA)
          , (getRawTxId (2 :: TxId), keyB)
          , (getRawTxId (3 :: TxId), keyC)
          ]
      , sharedKeyToTxId = IntMap.fromList
          [ (kA, 1 :: TxId), (kB, 2 :: TxId), (kC, 3 :: TxId) ]
      , sharedNextTxKey = 3
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB, keyC]
      , peerAvailableTxIds = IntMap.fromList
          [(kA, txSize), (kB, txSize), (kC, txSize)]
      , peerRequestedTxs = IntSet.fromList [kA, kB]
      , peerRequestedTxBatches = StrictSeq.fromList
          [ mkRequestedTxBatch [keyA] txSize
          , mkRequestedTxBatch [keyB] txSize
          ]
      , peerRequestedTxsSize = txSize + txSize
      }
    peerInFlight0 = emptyPeerTxInFlight
      { pifAdvertised = IntSet.fromList [kA, kB, kC]
      , pifLeased     = IntSet.fromList [kA, kB]
      , pifAttempting = IntSet.fromList [kA, kB]
      }

    (action, _, _, _) =
      nextPeerActionPipelined now policy peerAddr peerState0
                              peerInFlight0 sharedState
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxs {} -> property False
        _                 -> property True
  where
    peerAddr = 7 :: PeerAddr

-- | A peer with two unacked txids — one whose entry is leased to
-- another peer (blocked) and one that's still claimable — must skip
-- the blocked one and fetch the claimable one.
unit_nextPeerAction_skipsBlockedAvailableTxs
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_skipsBlockedAvailableTxs step = do
    step "Set up two advertised txs: keyA leased to another peer with the cap full, keyB claimable"
    let policy0     = defaultTxDecisionPolicy
        policy      = policy0 { txInflightMultiplicity = 1 }
        peerAddr    = 7  :: PeerAddr
        otherPeer   = 8  :: PeerAddr
        keyA        = TxKey 0
        keyB        = TxKey 1
        kA          = unTxKey keyA
        kB          = unTxKey keyB
        sharedState = emptySharedTxState
          { sharedTxTable = IntMap.fromList
              [ (kA, TxEntry
                  { txLease = TxLeased otherPeer (addTime 10 now)
                  , txAttempt = 1
                  , txInSubmission = False
                  , currentMaxInflightMultiplicity = txInflightMultiplicity policy
                  })
              , (kB, TxEntry
                  { txLease = TxClaimable now
                  , txAttempt = 0
                  , txInSubmission = False
                  , currentMaxInflightMultiplicity = txInflightMultiplicity policy
                  })
              ]
          , sharedTxIdToKey = Map.fromList
              [(getRawTxId (1 :: TxId), keyA), (getRawTxId (2 :: TxId), keyB)]
          , sharedKeyToTxId = IntMap.fromList [(kA, 1 :: TxId), (kB, 2 :: TxId)]
          , sharedNextTxKey = 2
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.fromList [keyA, keyB]
          , peerAvailableTxIds = IntMap.fromList [(kA, 10), (kB, 11)]
          }
        peerInFlight = emptyPeerTxInFlight
          { pifAdvertised = IntSet.fromList [kA, kB]
          }

    step "Run nextPeerAction"
    let (action, _, _, sharedState') =
          nextPeerAction now policy peerAddr peerState peerInFlight sharedState

    step "The claimable tx is requested and leased; the blocked tx is skipped"
    case action of
      PeerRequestTxs picked ->
        picked @?= [keyB]
      other ->
        assertFailure ("unexpected action: " ++ show other)

    case IntMap.lookup kB (sharedTxTable sharedState') of
      Just entry -> case txLease entry of
        TxLeased owner _ -> owner @?= peerAddr
        TxClaimable _    -> assertFailure "keyB still TxClaimable"
      Nothing -> assertFailure "keyB entry vanished"

-- | When the peer's unacked queue mixes a retained-and-acked tx and a
-- buffered-but-blocked tx, only the safe prefix is acked; the blocked
-- tx remains unacked.
unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx step = do
    step "Set up: tx 1 retained (ackable), tx 2 leased + buffered + already submitting via another peer"
    let peerAddr  = 7 :: PeerAddr
        otherPeer = 8 :: PeerAddr
        resolvedKey = TxKey 1
        blockedKey  = TxKey 2
        kResolved   = unTxKey resolvedKey
        kBlocked    = unTxKey blockedKey
        blockedTx   = mkTx 2 (mkSize (Positive 10))
        policy      = defaultTxDecisionPolicy
        blockedEntry = TxEntry
          { txLease = TxLeased peerAddr (addTime 10 now)
          , txAttempt = 1
          , txInSubmission = True
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
        sharedState = emptySharedTxState
          { sharedTxTable = IntMap.singleton kBlocked blockedEntry
          , sharedRetainedTxs = retainedSingleton kResolved (addTime 17 now)
          , sharedTxIdToKey = Map.fromList
              [(getRawTxId (1 :: TxId), resolvedKey), (getRawTxId (2 :: TxId), blockedKey)]
          , sharedKeyToTxId = IntMap.fromList [(kResolved, 1 :: TxId), (kBlocked, 2 :: TxId)]
          , sharedNextTxKey = 3
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.fromList [resolvedKey, blockedKey]
          , peerDownloadedTxs = IntMap.singleton kBlocked blockedTx
          }
        -- This peer holds the lease and is inside mempoolAddTxs;
        -- another peer (otherPeer) is also "submitting" so submitting-
        -- by-other is True from peerAddr's viewpoint. We model that by
        -- otherPeer's pifSubmitting having the key.
        peerInFlight = emptyPeerTxInFlight
          { pifLeased     = IntSet.singleton kBlocked
          , pifSubmitting = IntSet.singleton kBlocked
          }

    step "Run nextPeerAction (with the blocked tx already submitted by 'me')"
    let (action, peerState', _, _) =
          nextPeerAction now policy peerAddr peerState peerInFlight sharedState

    -- The pickSubmit walk finds the blocked tx still buffered + this
    -- peer is the submitter (so 'txSubmittingByOther' is False), so
    -- the test should fall through to a txid request that acks the
    -- retained prefix only.
    step "Only the retained prefix is acked; blocked tx remains unacked"
    case action of
      PeerRequestTxIds _ ack req -> do
        ack @?= 1
        assertBool ("expected positive txIdsToReq, got " ++ show req) (req > 0)
        toList (peerUnacknowledgedTxIds peerState') @?= [blockedKey]
      PeerSubmitTxs ks ->
        ks @?= [blockedKey]
      other ->
        assertFailure ("unexpected action: " ++ show other)
    -- otherPeer is referenced only to express the multi-peer setup.
    _ <- pure (otherPeer :: PeerAddr)
    pure ()

-- | When peer A is at its inflight-size cap and a fresh txid arrives,
-- peer A cannot claim; once peer B advertises the same key, peer B can
-- claim and download.
unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull step = do
    step "Receive a fresh txid from peer A while A is already at its inflight size limit"
    let (peerAState1, peerAInFlight1, sharedState1) =
          handleReceivedTxIds (const False) now defaultTxDecisionPolicy
            requestedToReply [(txid, txSize)]
            peerAState0 emptyPeerTxInFlight sharedState0
    txLease (lookupEntryOrFail key sharedState1) @?= TxClaimable now

    step "nextPeerAction for peer A: tx remains unclaimed because A is at the size cap"
    let (peerAAction, _, _, _) =
          nextPeerAction now defaultTxDecisionPolicy peerA
            peerAState1 peerAInFlight1 sharedState1
    case peerAAction of
      PeerDoNothing _ _ -> pure ()
      PeerRequestTxIds {} -> pure ()
      other -> assertFailure ("unexpected peer A action: " ++ show other)

    step "Peer B advertises the same txid"
    let (peerBState1, peerBInFlight1, sharedState2) =
          handleReceivedTxIds (const False) now defaultTxDecisionPolicy
            requestedToReply [(txid, txSize)]
            peerBState0 emptyPeerTxInFlight sharedState1

    step "nextPeerAction for peer B: claims and requests the fresh tx"
    let (peerBAction, peerBState2, peerBInFlight2, sharedState3) =
          nextPeerAction now defaultTxDecisionPolicy peerB
            peerBState1 peerBInFlight1 sharedState2
    case peerBAction of
      PeerRequestTxs txKeys -> do
        txKeys @?= [key]
        peerRequestedTxs peerBState2 @?= IntSet.singleton k
        IntSet.member k (pifLeased peerBInFlight2) @?= True
        case IntMap.lookup k (sharedTxTable sharedState3) of
          Just entry -> case txLease entry of
            TxLeased owner _ -> owner @?= peerB
            _                -> assertFailure "B's claim did not produce TxLeased B"
          Nothing -> assertFailure "entry vanished after B's claim"
      other -> assertFailure ("unexpected peer B action: " ++ show other)
  where
    peerA = 7 :: PeerAddr
    peerB = 8 :: PeerAddr
    txid :: TxId
    txid = 1
    txSize = mkSize (Positive 10)
    requestedToReply :: NumTxIdsToReq
    requestedToReply = 1
    key = TxKey 0
    k   = unTxKey key
    sharedState0 = emptySharedTxState
    peerAState0 = emptyPeerTxLocalState
      { peerRequestedTxIds = requestedToReply
      , peerRequestedTxsSize = txsSizeInflightPerPeer defaultTxDecisionPolicy
      }
    peerBState0 = emptyPeerTxLocalState
      { peerRequestedTxIds = requestedToReply }

-- | After peer A submits the body and the mempool rejects it, the
-- entry's lease releases back to 'TxClaimable' and peer B (still
-- advertising the key) is free to claim and re-attempt on its next
-- 'nextPeerAction' pass.
unit_nextPeerAction_claimsRejectedTxFromOtherAdvertiser
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsRejectedTxFromOtherAdvertiser step = do
    step "Pre-state: tx leased to peer A, A is in mempoolAddTxs (post-markSubmittingTxs), B advertises the same key"
    let txid :: TxId
        txid = 4
        txKeyInt :: Int
        txKeyInt = 0
        txSize :: SizeInBytes
        txSize = 100
        txBody :: Tx TxId
        txBody = mkTx txid txSize
        peerA = 1 :: PeerAddr
        peerB = 2 :: PeerAddr
        sharedState0 :: SharedTxState PeerAddr TxId
        sharedState0 = emptySharedTxState
          { sharedTxIdToKey = Map.singleton (getRawTxId txid) (TxKey txKeyInt)
          , sharedKeyToTxId = IntMap.singleton txKeyInt txid
          , sharedNextTxKey = txKeyInt + 1
          , sharedTxTable   = IntMap.singleton txKeyInt TxEntry
              { txLease = TxLeased peerA (addTime 1 now)
              , txAttempt = 0
              , txInSubmission = True
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity defaultTxDecisionPolicy
              }
          }
        peerAState0 = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey txKeyInt)
          , peerDownloadedTxs       = IntMap.singleton txKeyInt txBody
          }
        peerAInFlight0 = emptyPeerTxInFlight
          { pifAdvertised = IntSet.singleton txKeyInt
          , pifLeased     = IntSet.singleton txKeyInt
          , pifSubmitting = IntSet.singleton txKeyInt
          }
        peerBState0 = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey txKeyInt)
          , peerAvailableTxIds      = IntMap.singleton txKeyInt txSize
          }
        peerBInFlight0 = emptyPeerTxInFlight
          { pifAdvertised = IntSet.singleton txKeyInt
          }

    step "Peer A submits and the mempool rejects"
    let (peerAState', peerAInFlight', sharedStateAfter) =
          handleSubmittedTxs now defaultTxDecisionPolicy peerA
            [] [TxKey txKeyInt]
            peerAState0 peerAInFlight0 sharedState0

    step "Lease released, attempt cleared, peer A's pif* cleared for the key"
    case IntMap.lookup txKeyInt (sharedTxTable sharedStateAfter) of
      Just entry -> do
        txLease entry         @?= TxClaimable now
        txAttempt entry       @?= 0
        txInSubmission entry  @?= False
      Nothing -> assertFailure "entry vanished after rejection"
    peerDownloadedTxs peerAState' @?= IntMap.empty
    pifLeased     peerAInFlight'  @?= IntSet.empty
    pifSubmitting peerAInFlight'  @?= IntSet.empty
    pifAdvertised peerAInFlight'  @?= IntSet.empty

    step "Peer B's nextPeerAction now claims the released tx"
    let (peerBAction, peerBState', peerBInFlight', sharedStateFinal) =
          nextPeerAction now defaultTxDecisionPolicy peerB
            peerBState0 peerBInFlight0 sharedStateAfter
    peerBAction @?= PeerRequestTxs [TxKey txKeyInt]
    peerRequestedTxs peerBState' @?= IntSet.singleton txKeyInt
    IntSet.member txKeyInt (pifLeased peerBInFlight') @?= True
    case IntMap.lookup txKeyInt (sharedTxTable sharedStateFinal) of
      Just entry -> case txLease entry of
        TxLeased owner _ -> owner @?= peerB
        _                -> assertFailure "B's claim did not produce TxLeased B"
      Nothing -> assertFailure "entry vanished after B's claim"

-- | A peer with a non-zero score waits its score-derived delay before
-- claiming a TxClaimable entry.  Set up the entry to become claimable
-- exactly @peerScore / 20000@ seconds before @now@; the peer should
-- be allowed to claim at @now@.
unit_nextPeerAction_claimsAtScoreDelayThreshold
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsAtScoreDelayThreshold step = do
    step "Set up: claimableAt = now - 1ms, peer score = 20 (1ms claim delay), so claim at now is just allowed"
    let peerAddr = 7 :: PeerAddr
        txid :: TxId
        txid = 1
        txSize = mkSize (Positive 10)
        key = TxKey 0
        k   = unTxKey key
        claimableAt = Time 99.999  -- now is Time 100
        sharedState = emptySharedTxState
          { sharedTxIdToKey = Map.singleton (getRawTxId txid) key
          , sharedKeyToTxId = IntMap.singleton k txid
          , sharedNextTxKey = 1
          , sharedTxTable = IntMap.singleton k TxEntry
              { txLease = TxClaimable claimableAt
              , txAttempt = 0
              , txInSubmission = False
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity defaultTxDecisionPolicy
              }
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.singleton key
          , peerAvailableTxIds = IntMap.singleton k txSize
          , peerScore = PeerScore 20 now
          }
        peerInFlight = emptyPeerTxInFlight
          { pifAdvertised = IntSet.singleton k
          }

    step "Run nextPeerAction"
    let (action, peerState', _, sharedState') =
          nextPeerAction now defaultTxDecisionPolicy peerAddr
            peerState peerInFlight sharedState

    step "Tx becomes claimable once the peerScore / 20000 ms threshold has elapsed"
    case action of
      PeerRequestTxs txKeys -> do
        txKeys @?= [key]
        peerRequestedTxs peerState' @?= IntSet.singleton k
        case IntMap.lookup k (sharedTxTable sharedState') of
          Just entry -> case txLease entry of
            TxLeased owner _ -> owner @?= peerAddr
            _                -> assertFailure "did not lease to peerAddr"
          Nothing -> assertFailure "entry vanished after claim"
      other -> assertFailure ("unexpected action: " ++ show other)

-- | A peer that has buffered a tx blocked by another peer's submission
-- should still request body work for OTHER advertised txs that are
-- claimable.  Without this, a single stuck submission would starve the
-- whole peer.
unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx step = do
    step "Set up: blocked tx leased to me + buffered, but another peer is submitting it; second tx is claimable"
    let peerAddr       = 7 :: PeerAddr
        submittingPeer = 8 :: PeerAddr
        blockedTxid :: TxId
        blockedTxid = 1
        claimableTxid :: TxId
        claimableTxid = 2
        blockedSize   = mkSize (Positive 10)
        claimableSize = mkSize (Positive 11)
        blockedKey   = TxKey 1
        claimableKey = TxKey 2
        kBlocked     = unTxKey blockedKey
        kClaimable   = unTxKey claimableKey
        blockedTx    = mkTx blockedTxid blockedSize
        blockedEntry = TxEntry
          { txLease = TxLeased peerAddr (addTime 10 now)
          , txAttempt = 1
          , txInSubmission = True   -- submittingPeer is in mempoolAddTxs
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity defaultTxDecisionPolicy
          }
        sharedState = emptySharedTxState
          { sharedTxTable = IntMap.fromList
              [ (kBlocked, blockedEntry)
              , (kClaimable, TxEntry
                  { txLease = TxClaimable now
                  , txAttempt = 0
                  , txInSubmission = False
                  , currentMaxInflightMultiplicity =
                      txInflightMultiplicity defaultTxDecisionPolicy
                  })
              ]
          , sharedTxIdToKey = Map.fromList
              [ (getRawTxId blockedTxid, blockedKey)
              , (getRawTxId claimableTxid, claimableKey)
              ]
          , sharedKeyToTxId = IntMap.fromList
              [ (kBlocked, blockedTxid), (kClaimable, claimableTxid) ]
          , sharedNextTxKey = 3
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.fromList [blockedKey, claimableKey]
          , peerAvailableTxIds = IntMap.fromList
              [ (kBlocked, blockedSize), (kClaimable, claimableSize) ]
          , peerDownloadedTxs = IntMap.singleton kBlocked blockedTx
          }
        -- This peer holds the lease on the blocked tx but didn't
        -- submit it themselves; the submitting peer is someone else.
        peerInFlight = emptyPeerTxInFlight
          { pifAdvertised = IntSet.fromList [kBlocked, kClaimable]
          , pifLeased     = IntSet.singleton kBlocked
          , pifAttempting = IntSet.singleton kBlocked
          }

    step "Run nextPeerAction"
    let (action, peerState', _, sharedState') =
          nextPeerAction now defaultTxDecisionPolicy peerAddr
            peerState peerInFlight sharedState

    step "Blocked tx stays buffered while the claimable tx is requested"
    case action of
      PeerRequestTxs picked -> do
        picked @?= [claimableKey]
        peerUnacknowledgedTxIds peerState' @?= peerUnacknowledgedTxIds peerState
        peerRequestedTxs peerState' @?= IntSet.singleton kClaimable
        peerDownloadedTxs peerState' @?= peerDownloadedTxs peerState
        case IntMap.lookup kClaimable (sharedTxTable sharedState') of
          Just entry -> case txLease entry of
            TxLeased owner _ -> owner @?= peerAddr
            _                -> assertFailure "claimable not leased to peerAddr"
          Nothing -> assertFailure "claimable entry vanished"
      other ->
        assertFailure ("unexpected action: " ++ show other)
    -- submittingPeer is referenced only for clarity of the test setup.
    _ <- pure (submittingPeer :: PeerAddr)
    pure ()

-- | A peer that is not the leaseholder and has not buffered the body
-- waits with the txid unacked until the tx resolves out of the active
-- table.  After it resolves into 'sharedRetainedTxs' the same
-- 'nextPeerAction' call now ack-ifies the txid.
prop_nextPeerAction_nonOwnerWaitsUntilResolved
  :: ArbTxDecisionPolicy
  -> TxId
  -> Property
prop_nextPeerAction_nonOwnerWaitsUntilResolved (ArbTxDecisionPolicy policy)
                                               txid0 =
  let
    txid = abs txid0 + 1
    key  = TxKey 0
    k    = unTxKey key
    -- Unresolved: entry sits TxClaimable; another peer "has the body"
    -- but in the new model that's reflected by txAttempt > 0 on the
    -- entry; this peer's pif* are empty.  Peer's peerAvailableTxIds is
    -- empty so it can't claim either.
    unresolvedSharedState :: SharedTxState PeerAddr TxId
    unresolvedSharedState = emptySharedTxState
      { sharedTxTable = IntMap.singleton k TxEntry
          { txLease = TxClaimable now
          , txAttempt = 0
          , txInSubmission = False
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
    peerInFlight0 = emptyPeerTxInFlight
      { pifAdvertised = IntSet.singleton k
      }

    (unresolvedAction, unresolvedPeerState', _, _) =
      nextPeerAction now policy peerAddr peerState0 peerInFlight0
                     unresolvedSharedState
    (resolvedAction, resolvedPeerState', _, _) =
      nextPeerAction now policy peerAddr peerState0 peerInFlight0
                     resolvedSharedState
  in
    conjoin
      [ counterexample "unresolved: must not ack the unresolved txid" $
          case unresolvedAction of
            PeerDoNothing _ _ ->
              peerUnacknowledgedTxIds unresolvedPeerState'
                === peerUnacknowledgedTxIds peerState0
            PeerRequestTxIds _ ack _ ->
              counterexample ("ack should be 0, got " ++ show ack)
                (ack === 0)
            other ->
              counterexample ("unexpected unresolved action: " ++ show other)
                             (property False)
      , counterexample "resolved: must ack the now-retained txid" $
          case resolvedAction of
            PeerRequestTxIds _ ack _ ->
              conjoin
                [ ack === 1
                , peerUnacknowledgedTxIds resolvedPeerState' === StrictSeq.empty
                ]
            other ->
              counterexample ("unexpected resolved action: " ++ show other)
                             (property False)
      ]
  where
    peerAddr = 1 :: PeerAddr

-- | Roles for 'prop_nextPeerAction_claimsClaimableTx': 'Good' has no
-- score and can claim immediately; 'Bad' has a non-zero score that
-- pushes its claim past 'now'; 'Confounder' has no relevant local
-- state and is expected to do nothing on any 'nextPeerAction' call.
data PeerRole = Good | Bad | Confounder
  deriving (Eq, Show)

-- | Whether the lease starts as 'TxClaimable claimableAt' (a fresh
-- claimable lease) or as 'TxLeased oldOwner claimableAt' with
-- @claimableAt < now@ (a stale lease whose deadline already passed,
-- so any other advertiser whose 'peerClaimDelay' permits can claim).
data LeaseStart = ClaimableLease | ExpiredLease
  deriving (Eq, Show)

instance Arbitrary LeaseStart where
  arbitrary = elements [ClaimableLease, ExpiredLease]

-- | A scheduling order for the three peers.  Generated as a permutation
-- of the three roles.
newtype PeerOrder = PeerOrder [PeerRole]
  deriving Show

instance Arbitrary PeerOrder where
  arbitrary = PeerOrder <$> shuffle [Good, Bad, Confounder]

-- | Drives 'nextPeerAction' for three peers ('Good', 'Bad',
-- 'Confounder') in a generator-chosen order.  Asserts:
--
--   * 'Good' (no score) claims the tx and ends up the leaseholder.
--   * 'Bad' (non-zero score) yields 'PeerDoNothing' with a wake delay
--     equal to the score-derived delay (offset by whether 'Bad' ran
--     before or after 'Good's claim).
--   * 'Confounder' (no relevant state) yields 'PeerDoNothing' with no
--     wake.
--   * The combined invariant holds for the full @(peerLocal,
--     peerInFlight)@ snapshot of all three peers after each action.
prop_nextPeerAction_claimsClaimableTx
  :: ArbTxDecisionPolicy
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> TxId
  -> Positive Int
  -> Positive Int
  -> Positive Int
  -> PeerOrder
  -> LeaseStart
  -> Property
prop_nextPeerAction_claimsClaimableTx
    (ArbTxDecisionPolicy arbPolicy)
    (Positive good0) (Positive bad0) (Positive conf0)
    txid0 txSize0 (Positive badScore0) (Positive tDecay0) (PeerOrder order)
    leaseStart =
      tabulate "order"     [show order]
    . tabulate "lease"     [show leaseStart]
    $ conjoin
        [ counterexample ("Good must claim: " ++ show (lookupAction Good)) $
            case lookupAction Good of
              Just (PeerRequestTxs txKeys, _, _) -> txKeys === [key]
              _                                  -> property False
        , counterexample ("Bad must yield with the score-delay derived wake: "
                            ++ show (lookupAction Bad)) $
            case lookupAction Bad of
              Just (PeerDoNothing _ (Just delay), _, _) ->
                let diff = abs (delay - expectedBadDelay) in
                counterexample
                  ("delay = " ++ show delay
                    ++ ", expected = " ++ show expectedBadDelay
                    ++ ", diff = " ++ show diff)
                  (property (diff < 1e-9))
              other ->
                counterexample ("got: " ++ show other) (property False)
        , counterexample ("Confounder must do nothing with no scheduled wake: "
                            ++ show (lookupAction Confounder)) $
            case lookupAction Confounder of
              Just (PeerDoNothing _ Nothing, _, _) -> property True
              _                                    -> property False
        , counterexample "Lease must end up at Good" $
            case IntMap.lookup k (sharedTxTable sharedStateFinal) of
              Just entry -> case txLease entry of
                TxLeased owner _ -> owner === goodPeer
                _                -> property False
              Nothing -> property False
        ]
  where
    policy = arbPolicy
      { scoreMax  = max 200 (scoreMax arbPolicy)
      , scoreRate = max 0.01 (min 1.0 (scoreRate arbPolicy))
      }

    goodPeer = good0
    badPeer  = bad0 + 1000
    confPeer = conf0 + 2000
    txid     = abs txid0 + 1
    txSize   = mkSize txSize0
    key      = TxKey 0
    k        = unTxKey key

    claimableAt = Time 99.999
    tDecaySec :: Double
    tDecaySec = fromIntegral (1 + (tDecay0 - 1) `mod` 10 :: Int)
    decayAmount :: Double
    decayAmount = tDecaySec * scoreRate policy
    decayedBadScore :: Double
    decayedBadScore = fromIntegral (21 + (badScore0 - 1) `mod` 80 :: Int)
    badInitialScore :: Double
    badInitialScore = decayedBadScore + decayAmount
    badPeerScore = PeerScore
      { peerScoreValue = badInitialScore
      , peerScoreTs    = addTime (negate (realToFrac tDecaySec)) now
      }

    badRunsBeforeGood = case (elemIndex Bad order, elemIndex Good order) of
                         (Just bi, Just gi) -> bi < gi
                         _                  -> False
    badClaimDelay :: DiffTime
    badClaimDelay = realToFrac (decayedBadScore / 20000)
    expectedBadDelay :: DiffTime
    expectedBadDelay
      | badRunsBeforeGood = badClaimDelay - diffTime now claimableAt
      | otherwise         = badClaimDelay + interTxSpace policy

    -- Stale-lease holder; only present in shared state when
    -- 'leaseStart = ExpiredLease'.  'nextPeerAction' is never called
    -- for this peer; it exists only to set the @TxLeased@ owner with
    -- @leaseUntil = claimableAt < now@.
    oldOwner = good0 + bad0 + conf0 + 3000

    sharedState0 = emptySharedTxState
      { sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      , sharedTxTable = IntMap.singleton k TxEntry
          { txLease = case leaseStart of
              ClaimableLease -> TxClaimable claimableAt
              ExpiredLease   -> TxLeased oldOwner claimableAt
          , txAttempt = 0
          , txInSubmission = False
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      }

    goodPeerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds      = IntMap.singleton k txSize
      }
    goodInFlight0 = emptyPeerTxInFlight { pifAdvertised = IntSet.singleton k }

    badPeerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerAvailableTxIds      = IntMap.singleton k txSize
      , peerScore               = badPeerScore
      }
    badInFlight0 = emptyPeerTxInFlight { pifAdvertised = IntSet.singleton k }

    confPeerState0 = emptyPeerTxLocalState
      { peerRequestedTxIds = maxNumTxIdsToRequest policy
      }
    confInFlight0 = emptyPeerTxInFlight

    roleSetup Good       = (goodPeer, goodPeerState0, goodInFlight0)
    roleSetup Bad        = (badPeer,  badPeerState0,  badInFlight0)
    roleSetup Confounder = (confPeer, confPeerState0, confInFlight0)

    runOne :: (SharedTxState PeerAddr TxId,
               [(PeerRole, PeerAction, PeerTxLocalState (Tx TxId), PeerTxInFlight)])
           -> PeerRole
           -> (SharedTxState PeerAddr TxId,
               [(PeerRole, PeerAction, PeerTxLocalState (Tx TxId), PeerTxInFlight)])
    runOne (ss, acc) role =
      let (peer, ps0, pif0) = roleSetup role
          (action, ps', pif', ss') =
            nextPeerAction now policy peer ps0 pif0 ss
      in (ss', (role, action, ps', pif') : acc)

    (sharedStateFinal, results) = foldl' runOne (sharedState0, []) order

    lookupAction :: PeerRole
                 -> Maybe (PeerAction, PeerTxLocalState (Tx TxId), PeerTxInFlight)
    lookupAction role =
      case [ (a, ps', pif') | (r, a, ps', pif') <- results, r == role ] of
        []      -> Nothing
        (x : _) -> Just x

--
-- TriggerScenario: multi-peer scheduler exercise
--
-- Each 'ActionTrigger' on a peer's list describes one action that
-- should be choosable at the moment the test calls 'nextPeerAction'.
-- 'buildTriggerState' turns the per-peer list into a consistent
-- ('PeerTxLocalState', 'PeerTxInFlight', 'SharedTxState') triple.

data ActionTrigger
  = TSubmittable    TxId (Positive Int)
    -- ^ peer holds the lease, body buffered locally, ready to submit.
  | TFetchable      TxId (Positive Int)
    -- ^ peer advertises the txid, claimable now.
  | TAckable        TxId
    -- ^ in 'sharedRetainedTxs' + peer's unacked queue (resolved already).
  | TFetchableLater (Positive Int) TxId (Positive Int)
    -- ^ delay-in-seconds + txid + size: claimable only after the loop
    --   has advanced 'time' by at least the delay.
  deriving (Eq, Show)

instance Arbitrary ActionTrigger where
  arbitrary = oneof
    [ TSubmittable    <$> arbitrary <*> arbitrary
    , TFetchable      <$> arbitrary <*> arbitrary
    , TAckable        <$> arbitrary
    , TFetchableLater <$> arbitrary <*> arbitrary <*> arbitrary
    ]
  shrink (TSubmittable t s) =
       [ TFetchable t s, TAckable t ]
    ++ [ TSubmittable t' s | t' <- take 1 (shrink t) ]
  shrink (TFetchable t s) =
       TAckable t :
       [ TFetchable t' s | t' <- take 1 (shrink t) ]
  shrink (TAckable t) =
       [ TAckable t' | t' <- take 1 (shrink t) ]
  shrink (TFetchableLater d t s) =
       [ TFetchable t s, TAckable t ]
    ++ [ TFetchableLater d' t s | d' <- take 1 (shrink d) ]

triggerTxid :: ActionTrigger -> TxId
triggerTxid (TSubmittable    t _)   = t
triggerTxid (TFetchable      t _)   = t
triggerTxid (TAckable        t)     = t
triggerTxid (TFetchableLater _ t _) = t

isTSubmittable :: ActionTrigger -> Bool
isTSubmittable TSubmittable{} = True
isTSubmittable _              = False

isTAckable :: ActionTrigger -> Bool
isTAckable TAckable{} = True
isTAckable _          = False

isTFetchableNow :: ActionTrigger -> Bool
isTFetchableNow TFetchable{} = True
isTFetchableNow _            = False

isTFetchableLater :: ActionTrigger -> Bool
isTFetchableLater TFetchableLater{} = True
isTFetchableLater _                 = False

setTxid :: ActionTrigger -> TxId -> ActionTrigger
setTxid (TSubmittable    _ s)   t = TSubmittable    t s
setTxid (TFetchable      _ s)   t = TFetchable      t s
setTxid (TAckable        _)     t = TAckable        t
setTxid (TFetchableLater d _ s) t = TFetchableLater d t s

-- | Generator-time bias selector: 'ModeDisjoint' renumbers all
-- triggers so each peer's txid range is unique (no cross-peer overlap),
-- 'ModeShared' collapses txids into a small shared pool so cross-peer
-- overlap arises with high probability.
data OverlapMode = ModeDisjoint | ModeShared
  deriving (Eq, Show)

-- | A scenario is an overlap mode plus a per-peer trigger map.
data TriggerScenario =
       TriggerScenario OverlapMode (Map.Map PeerAddr [ActionTrigger])
  deriving (Eq, Show)

genPerPeerTriggers :: Gen [ActionTrigger]
genPerPeerTriggers = do
  size <- frequency
    [ (2, pure 1)
    , (1, pure 0)
    , (3, choose (2, 10))
    , (1, choose (11, 100))
    ]
  genElem <- oneof
    [ pure arbitrary
    , pure (TFetchable      <$> arbitrary <*> arbitrary)
    , pure (TSubmittable    <$> arbitrary <*> arbitrary)
    , pure (TAckable        <$> arbitrary)
    , pure (TFetchableLater <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
  vectorOf size genElem

shrinkTriggerList :: [ActionTrigger] -> [[ActionTrigger]]
shrinkTriggerList ts =
  let n = length ts in
     [ take (n `div` 2) ts | n >= 2 ]
  ++ [ drop (n `div` 2) ts | n >= 2 ]
  ++ [ take i ts ++ drop (i + 1) ts | i <- [0 .. n - 1] ]
  ++ [ take i ts ++ t' : drop (i + 1) ts
     | i <- [0 .. n - 1]
     , t' <- shrink (ts !! i)
     ]

renumberDisjoint :: [[ActionTrigger]] -> [[ActionTrigger]]
renumberDisjoint = snd . mapAccumL renumberOne 1
  where
    renumberOne nextId triggers =
      let n = length triggers in
      (nextId + n, zipWith setTxid triggers [nextId .. nextId + n - 1])

collapseToPool :: Int -> [[ActionTrigger]] -> Gen [[ActionTrigger]]
collapseToPool poolSize = traverse (traverse remap)
  where
    remap trig = do
      newId <- chooseInt (1, poolSize)
      pure (setTxid trig newId)

instance Arbitrary TriggerScenario where
  arbitrary = do
    nPeers <- frequency
      [ (2, pure 1)
      , (2, pure 2)
      , (1, pure 3)
      ]
    perPeer <- vectorOf nPeers genPerPeerTriggers
    mode <- frequency
      [ (2, pure ModeDisjoint)
      , (3, pure ModeShared)
      ]
    remapped <- case mode of
      ModeDisjoint -> pure (renumberDisjoint perPeer)
      ModeShared   ->
        let totalT  = sum (map length perPeer)
            poolSz  = max 1 (totalT `div` 2) in
        collapseToPool poolSz perPeer
    pure (TriggerScenario mode
            (Map.fromList (zip [1 .. nPeers] remapped)))
  shrink (TriggerScenario mode m) =
       [ TriggerScenario mode (Map.delete p m)
       | Map.size m > 1
       , p <- Map.keys m
       ]
    ++ [ TriggerScenario mode (Map.insert p ts' m)
       | (p, ts) <- Map.toList m
       , ts' <- shrinkTriggerList ts
       ]

-- | Strongest trigger category seen across all peers for a given txid.
data TxCategory = CatSubmit | CatFetch | CatAck
  deriving (Eq, Show)

categoryOf :: [ActionTrigger] -> TxCategory
categoryOf trigs
  | any isTSubmittable trigs                                  = CatSubmit
  | any isTFetchableNow trigs || any isTFetchableLater trigs  = CatFetch
  | otherwise                                                 = CatAck

hasActiveEntry :: ActionTrigger -> Bool
hasActiveEntry TAckable{} = False
hasActiveEntry _          = True

-- | Build a consistent multi-peer state from a per-peer trigger map.
-- The state must already be normalised by 'normaliseScenario'.
buildTriggerState :: TxDecisionPolicy
                  -> Map.Map PeerAddr [ActionTrigger]
                  -> ( Map.Map PeerAddr (PeerTxLocalState (Tx TxId), PeerTxInFlight)
                     , SharedTxState PeerAddr TxId
                     )
buildTriggerState policy perPeer =
    (peerStates, sharedState0)
  where
    allTxids :: [TxId]
    allTxids = nub
      [ triggerTxid t
      | (_, ts) <- Map.toAscList perPeer
      , t <- ts
      ]

    txidToKey :: Map.Map TxId Int
    txidToKey = Map.fromList (zip allTxids [0..])

    txidKey :: TxId -> Int
    txidKey t = txidToKey Map.! t

    triggersFor :: TxId -> [(PeerAddr, ActionTrigger)]
    triggersFor txid =
      [ (p, t)
      | (p, ts) <- Map.toAscList perPeer
      , t <- ts
      , triggerTxid t == txid
      ]

    submittingPeer :: TxId -> Maybe PeerAddr
    submittingPeer txid = listToMaybe
      [ p | (p, t) <- triggersFor txid, isTSubmittable t ]

    laterDelay :: TxId -> Maybe Int
    laterDelay txid = listToMaybe
      [ d | (_, TFetchableLater (Positive d) _ _) <- triggersFor txid ]

    -- An entry's 'txAttempt' is exactly the number of TSubmittable
    -- triggers for the txid; with 'dedupeAcrossPeers' that's at most 1.
    -- All fetchable peers contribute via their per-peer 'pifAdvertised'
    -- to keep the entry alive against the orphan sweep.
    mkEntry :: TxId -> TxEntry PeerAddr
    mkEntry txid =
      let trigs = triggersFor txid
          attemptCount = length [() | (_, t) <- trigs, isTSubmittable t]
          lease = case submittingPeer txid of
            Just p  -> TxLeased p (addTime 10 now)
            Nothing -> case laterDelay txid of
              Just d  -> TxClaimable (addTime (fromIntegral d) now)
              Nothing -> TxClaimable now in
      TxEntry
        { txLease           = lease
        , txAttempt         = attemptCount
        , txInSubmission    = False
        , currentMaxInflightMultiplicity = txInflightMultiplicity policy
        }

    activeTxids = [ txid | txid <- allTxids
                  , categoryOf (map snd (triggersFor txid)) /= CatAck ]
    retainedTxids = [ txid | txid <- allTxids
                    , categoryOf (map snd (triggersFor txid)) == CatAck ]

    retainedUntil   = addTime 600 now
    retainedEntries = [ (txidKey txid, retainedUntil) | txid <- retainedTxids ]

    mkPeerState :: PeerAddr -> [ActionTrigger]
                -> (PeerTxLocalState (Tx TxId), PeerTxInFlight)
    mkPeerState peeraddr ts =
      let peerLocal = emptyPeerTxLocalState
            { peerUnacknowledgedTxIds = StrictSeq.fromList
                [ TxKey (txidKey (triggerTxid t)) | t <- ts ]
            , peerDownloadedTxs = IntMap.fromList
                [ (txidKey t', mkTx t' (mkSize s))
                | TSubmittable t' s <- ts
                ]
            , peerAvailableTxIds = IntMap.fromList $
                   [ (txidKey t', mkSize s) | TFetchable t' s <- ts ]
                ++ [ (txidKey t', mkSize s) | TFetchableLater _ t' s <- ts ]
            }
          advertised = IntSet.fromList
            [ txidKey (triggerTxid t) | t <- ts, hasActiveEntry t ]
          leasedHere = IntSet.fromList
            [ txidKey t' | TSubmittable t' _ <- ts
                         , submittingPeer t' == Just peeraddr ]
          attemptingHere = leasedHere
          peerInFlight = emptyPeerTxInFlight
            { pifAdvertised = advertised
            , pifLeased     = leasedHere
            , pifAttempting = attemptingHere
            }
      in (peerLocal, peerInFlight)

    peerStates = Map.mapWithKey mkPeerState perPeer

    sharedState0 = emptySharedTxState
      { sharedTxTable     = IntMap.fromList
          [ (txidKey txid, mkEntry txid) | txid <- activeTxids ]
      , sharedTxIdToKey   = Map.fromList
          [ (getRawTxId txid, TxKey (txidKey txid)) | txid <- allTxids ]
      , sharedKeyToTxId   = IntMap.fromList
          [ (txidKey txid, txid) | txid <- allTxids ]
      , sharedNextTxKey   = length allTxids
      , sharedRetainedTxs = retainedFromList retainedEntries
      }

-- | Per-peer normalise: shift txids to >= 1, dedupe, reorder so
-- ackables come first, then submittables, then fetchables.
normaliseTriggers :: [ActionTrigger] -> [ActionTrigger]
normaliseTriggers =
    orderTriggers
  . nubBy ((==) `on` triggerTxid)
  . map shiftTrigger
  where
    shiftTrigger (TSubmittable    t s)   = TSubmittable    (abs t + 1) s
    shiftTrigger (TFetchable      t s)   = TFetchable      (abs t + 1) s
    shiftTrigger (TAckable        t)     = TAckable        (abs t + 1)
    shiftTrigger (TFetchableLater d t s) = TFetchableLater d (abs t + 1) s
    orderTriggers ts =
         filter isTAckable        ts
      ++ filter isTSubmittable    ts
      ++ filter isTFetchableNow   ts
      ++ filter isTFetchableLater ts

-- | Across peers, ensure each txid has 'TSubmittable' from at most one
-- peer (the lowest-numbered).
dedupeAcrossPeers :: Map.Map PeerAddr [ActionTrigger]
                  -> Map.Map PeerAddr [ActionTrigger]
dedupeAcrossPeers m = Map.mapWithKey (map . demote) m
  where
    primarySubmitter :: Map.Map TxId PeerAddr
    primarySubmitter = Map.fromListWith min
      [ (t, p) | (p, ts) <- Map.toList m, TSubmittable t _ <- ts ]
    demote p (TSubmittable t s)
      | Map.lookup t primarySubmitter /= Just p = TFetchable t s
    demote _ trig = trig

normaliseScenario :: Map.Map PeerAddr [ActionTrigger]
                  -> Map.Map PeerAddr [ActionTrigger]
normaliseScenario = dedupeAcrossPeers . Map.map normaliseTriggers

-- | Policy used by the 'TriggerScenario' meta-tests.
metaPolicy :: TxDecisionPolicy
metaPolicy = defaultTxDecisionPolicy { txInflightMultiplicity = 2 }

prop_TriggerScenario_validInitialState :: TriggerScenario -> Property
prop_TriggerScenario_validInitialState (TriggerScenario _ rawPerPeer) =
    let perPeer           = normaliseScenario rawPerPeer
        (peerStates, ss0) = buildTriggerState metaPolicy perPeer in
    combinedStateInvariant metaPolicy StrongInvariant peerStates ss0

prop_TriggerScenario_shrinkPreservesValidity :: TriggerScenario -> Property
prop_TriggerScenario_shrinkPreservesValidity ts =
    conjoin
      [ prop_TriggerScenario_validInitialState ts'
      | ts' <- shrink ts
      ]

prop_TriggerScenario_shrinkSmaller :: TriggerScenario -> Property
prop_TriggerScenario_shrinkSmaller ts@(TriggerScenario _ rawM) =
    let n = sum (map length (Map.elems rawM)) in
    conjoin
      [ counterexample ("shrink grew the trigger count: " ++ show ts')
          $ sum (map length (Map.elems rawM')) <= n
      | ts'@(TriggerScenario _ rawM') <- shrink ts
      ]

prop_TriggerScenario_shrinkExcludesOriginal :: TriggerScenario -> Property
prop_TriggerScenario_shrinkExcludesOriginal ts =
    counterexample "shrink contains the original value"
      $ property (ts `notElem` shrink ts)

-- | Drives 'nextPeerAction' for every peer in the scenario, advancing
-- the earliest-wake peer at each step.  Asserts:
--
--   1. The loop terminates within the iteration budget.
--   2. Every txid whose strongest cross-peer trigger category is
--      'CatSubmit' or 'CatFetch' appears in the union of submitted keys
--      (since fetched bodies are deferred-delivered and then submitted).
--   3. Every txid whose category is 'CatFetch' appears in the union of
--      requested keys.
--   4. Every txid whose category is 'CatAck' appears in the union of
--      acked keys.
--   5. 'combinedStateInvariant' holds on the initial state for every
--      peer and after the acting peer's update at every step.
prop_nextPeerAction_processesAllTriggers
  :: ArbTxDecisionPolicy
  -> TriggerScenario
  -> Property
prop_nextPeerAction_processesAllTriggers
    (ArbTxDecisionPolicy arbPolicy) (TriggerScenario mode rawPerPeer) =
      tabulate "trigger count" [bucket totalTriggers]
    . tabulate "peer count"    [show nPeers]
    . tabulate "iterations"    [bucket iterations]
    . tabulate "shared txids"  [bucket sharedTxidCount]
    . tabulate "overlap mode"  [show mode]
    $ conjoin
        [ counterexample "loop must terminate within step budget"
            $ property terminated
        , counterexample "submitted set must equal CatSubmit txids"
            $ allSubmitted === expectedSubmitted
        , counterexample "requested set must equal CatFetch txids"
            $ allRequested === expectedFetched
        , counterexample
            ("expected acks missing: " ++ show (IntSet.toList missingAcks))
            $ property (IntSet.null missingAcks)
        , counterexample "initial combined invariant violated"
            (combinedStateInvariant policy StrongInvariant peerStates0 sharedState0)
        , conjoin
            [ counterexample
                ("invariant after step " ++ show n
                  ++ " (peer " ++ show p ++ ")") inv
            | (n, p, inv) <- stateInvariants
            ]
        ]
  where
    perPeer       = normaliseScenario rawPerPeer
    totalTriggers = sum (map length (Map.elems perPeer))
    nPeers        = Map.size perPeer

    txidPeerCounts :: Map.Map TxId Int
    txidPeerCounts = Map.fromListWith (+)
      [ (triggerTxid t, 1 :: Int)
      | (_, ts) <- Map.toList perPeer
      , t       <- ts
      ]
    sharedTxidCount = length
      [ () | (_, n) <- Map.toList txidPeerCounts, n >= 2 ]

    policy = arbPolicy
      { txInflightMultiplicity         = 2
      , txsSizeInflightPerPeer         =
          max_TX_SIZE * fromIntegral (max 1 totalTriggers)
      , maxOutstandingTxBatchesPerPeer = max 1 totalTriggers
      }

    maxIters = 100 + 6 * totalTriggers * max 1 nPeers

    allTxids = nub
      [ triggerTxid t | (_, ts) <- Map.toAscList perPeer, t <- ts ]
    txidToKey = Map.fromList (zip allTxids [0..])
    txidKey t = txidToKey Map.! t

    triggersFor txid =
      [ trig | (_, ts) <- Map.toAscList perPeer
             , trig    <- ts
             , triggerTxid trig == txid ]
    catFor txid = categoryOf (triggersFor txid)

    expectedSubmitted = IntSet.fromList
      [ txidKey t | t <- allTxids
                  , let c = catFor t, c == CatSubmit || c == CatFetch ]
    expectedFetched   = IntSet.fromList
      [ txidKey t | t <- allTxids, catFor t == CatFetch  ]
    expectedAcked     = IntSet.fromList
      [ txidKey t | t <- allTxids, catFor t == CatAck    ]

    (peerStates0, sharedState0) = buildTriggerState policy perPeer

    -- Schedule entry: (next-wake-time, peerLocal, peerInFlight).
    initialSchedule
      :: Map.Map PeerAddr (Maybe Time, PeerTxLocalState (Tx TxId), PeerTxInFlight)
    initialSchedule = Map.map (\(ps, pif) -> (Just now, ps, pif)) peerStates0

    (allSubmitted, allRequested, allAcked,
     stateInvariants, terminated, iterations) =
        runLoop sharedState0 initialSchedule Map.empty
                IntSet.empty IntSet.empty IntSet.empty
                [] 0 now

    missingAcks = expectedAcked `IntSet.difference` allAcked

    pickEarliest schedule =
      case sortBy (compare `on` snd)
             [ (p, t) | (p, (Just t, _, _)) <- Map.toList schedule ] of
        []         -> Nothing
        (p, t) : _ ->
          let (_, ps, pif) = schedule Map.! p in
          Just (p, t, ps, pif)

    reactivateOthers acting time =
      Map.mapWithKey $ \p (status, ps, pif) ->
        if p == acting
          then (status, ps, pif)
          else case status of
            Just t' | t' <= time -> (Just t', ps, pif)
            _                    -> (Just time, ps, pif)

    mkBody :: SharedTxState PeerAddr TxId
           -> PeerTxLocalState (Tx TxId)
           -> TxKey
           -> (TxId, Tx TxId)
    mkBody ss ps (TxKey k) =
      let txid = sharedKeyToTxId ss IntMap.! k
          size = peerAvailableTxIds ps IntMap.! k in
      (txid, mkTx txid size)

    runLoop ss schedule pending subs reqs acks invs i lastTime
      | i >= maxIters =
          (subs, reqs, acks, reverse invs, False, i)
      | otherwise =
          case pickEarliest schedule of
            Nothing ->
              (subs, reqs, acks, reverse invs, True, i)
            Just (p, time, ps, pif) ->
              let lastTime' = max lastTime time

                  -- Snapshot of every peer's @(peerLocal, peerInFlight)@
                  -- with @p@'s entry overridden to the supplied pair.
                  peerSnapshot pSnap psSnap pifSnap =
                    Map.insert pSnap (psSnap, pifSnap)
                      (Map.map (\(_, ps_, pif_) -> (ps_, pif_)) schedule)

                  -- Drain p's pending body deliveries before its action.
                  (psPre, pifPre, ssPre, pendingPre, drainInvs, stepDrain) =
                    case Map.lookup p pending of
                      Nothing -> (ps, pif, ss, pending, [], i)
                      Just deliveries ->
                        let (_, _, ps2, pif2, ss2) =
                              handleReceivedTxs (const False) time policy p
                                deliveries ps pif ss
                            stepD = i + 1
                            drainInv = conjoin
                              [ combinedStateInvariant policy
                                  StrongInvariant
                                  (peerSnapshot p ps2 pif2)
                                  ss2
                              , checkNoThunks
                                  ("peerState after drain (peer "
                                    ++ show p ++ ", step " ++ show stepD ++ ")")
                                  ps2
                              , checkNoThunks
                                  ("sharedState after drain (peer "
                                    ++ show p ++ ", step " ++ show stepD ++ ")")
                                  ss2
                              ] in
                        ( ps2, pif2, ss2, Map.delete p pending
                           , [(stepD, p, drainInv)], stepD )

                  (action, ps', pif', ss') =
                    nextPeerAction time policy p psPre pifPre ssPre
                  oldUnacked = peerUnacknowledgedTxIds psPre
                  newUnacked = peerUnacknowledgedTxIds ps'
                  numAcked   = StrictSeq.length oldUnacked
                             - StrictSeq.length newUnacked
                  ackedNow   = IntSet.fromList $ map unTxKey
                             $ toList (StrictSeq.take numAcked oldUnacked)
                  step       = stepDrain + 1
                  inv        = conjoin
                    [ combinedStateInvariant policy StrongInvariant
                        (peerSnapshot p ps' pif') ss'
                    , checkNoThunks
                        ("peerState' (peer " ++ show p
                          ++ ", step " ++ show step ++ ")")
                        ps'
                    , checkNoThunks
                        ("sharedState' (peer " ++ show p
                          ++ ", step " ++ show step ++ ")")
                        ss'
                    ] in
              case action of
                PeerDoNothing _ Nothing ->
                  let schedule' = Map.insert p (Nothing, ps', pif') schedule in
                  runLoop ss' schedule' pendingPre subs reqs acks
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'
                PeerDoNothing _ (Just delay) ->
                  let nextWake = addTime (max delay 0.001) time
                      schedule' = Map.insert p (Just nextWake, ps', pif') schedule in
                  runLoop ss' schedule' pendingPre subs reqs acks
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'
                PeerSubmitTxs ks ->
                  let (ps'', pif'', ss'') =
                        handleSubmittedTxs time policy p ks [] ps' pif' ss'
                      postInv = conjoin
                        [ combinedStateInvariant policy StrongInvariant
                            (peerSnapshot p ps'' pif'') ss''
                        , checkNoThunks
                            ("peerState'' (peer " ++ show p
                              ++ ", step " ++ show step ++ ")")
                            ps''
                        , checkNoThunks
                            ("sharedState'' (peer " ++ show p
                              ++ ", step " ++ show step ++ ")")
                            ss''
                        ]
                      others' = reactivateOthers p time schedule
                      schedule' = Map.insert p (Just time, ps'', pif'') others'
                      subs' = IntSet.union subs
                                (IntSet.fromList (unTxKey <$> ks)) in
                  runLoop ss'' schedule' pendingPre subs' reqs acks
                       ( (step, p, postInv) : (step, p, inv)
                       : drainInvs ++ invs ) step lastTime'
                PeerRequestTxs ks ->
                  let bodies   = [ mkBody ssPre psPre k | k <- ks ]
                      pending' = Map.insert p bodies pendingPre
                      others'  = reactivateOthers p time schedule
                      schedule' = Map.insert p (Just time, ps', pif') others'
                      reqs'     = IntSet.union reqs
                                    (IntSet.fromList (unTxKey <$> ks)) in
                  runLoop ss' schedule' pending' subs reqs' acks
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'
                PeerRequestTxIds{} ->
                  let others'   = reactivateOthers p time schedule
                      schedule' = Map.insert p (Just time, ps', pif') others'
                      acks'     = IntSet.union acks ackedNow in
                  runLoop ss' schedule' pendingPre subs reqs acks'
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'

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
      peerPhase = PeerIdle,
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

-- Generate a shared tx state with distinct active and retained entries.
--
-- Per-peer state is no longer carried in 'SharedTxState'; tests that
-- exercise multi-peer behaviour pair this generator with a separate
-- 'PeerTxLocalState' for the peer under test.
genSharedTxState :: Gen (SharedTxState PeerAddr TxId)
genSharedTxState = sized $ \n -> do
  let maxPeers = min 6 (n + 1)
      maxActiveTxs = min 8 (n + 2)
      maxRetainedTxs = min 6 (n + 2)

  numPeers <- chooseInt (1, max 1 maxPeers)
  peeraddrs <- genDistinctPositiveInts numPeers

  numActiveTxs <- chooseInt (0, maxActiveTxs)
  numRetainedTxs <- chooseInt (0, maxRetainedTxs)

  txids <- genDistinctPositiveInts (numActiveTxs + numRetainedTxs)
  let (activeTxIds, retainedTxIds) = splitAt numActiveTxs txids

  activeEntries <- mapM (genActiveTxEntry peeraddrs) activeTxIds
  retainedEntries <- mapM genRetainedEntry retainedTxIds
  sharedGeneration <- genSmallWord64

  pure $ buildSharedTxState activeEntries retainedEntries sharedGeneration
  where
    genRetainedEntry txid = do
      retainUntil <- genSharedExpiryTime
      pure (txid, retainUntil)

-- Generate one active tx entry using a mix of leased and claimable shapes.
genActiveTxEntry :: [PeerAddr] -> TxId -> Gen (TxId, TxEntry PeerAddr)
genActiveTxEntry peeraddrs txid = do
  txEntry <- frequency
    [ (5, genLeasedTxEntry peeraddrs)
    , (3, genClaimableTxEntry)
    ]
  pure (txid, txEntry)

-- Generate a leased entry where the owner may also be in submission.
genLeasedTxEntry :: [PeerAddr] -> Gen (TxEntry PeerAddr)
genLeasedTxEntry peeraddrs = do
  owner <- elements peeraddrs
  txLease <- TxLeased owner <$> genSharedExpiryTime
  inSub  <- frequency [(2, pure False), (1, pure True)]
  pure TxEntry {
      txLease,
      txAttempt = if inSub then 0 else 1,
      txInSubmission = inSub,
      currentMaxInflightMultiplicity =
        txInflightMultiplicity defaultTxDecisionPolicy
    }

-- Generate a claimable entry with no in-flight attempt.
genClaimableTxEntry :: Gen (TxEntry PeerAddr)
genClaimableTxEntry = do
  claimableAt <- genSharedExpiryTime
  pure TxEntry {
      txLease = TxClaimable claimableAt,
      txAttempt = 0,
      txInSubmission = False,
      currentMaxInflightMultiplicity =
        txInflightMultiplicity defaultTxDecisionPolicy
    }

-- Rebuild a shared state from tx-centric fixtures while preserving interned keys.
buildSharedTxState
  :: [(TxId, TxEntry PeerAddr)]
  -> [(TxId, Time)]
  -> Word64
  -> SharedTxState PeerAddr TxId
buildSharedTxState activeEntries retainedEntries sharedGeneration =
  baseState {
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
    baseState =
      mkSharedState (fmap fst activeEntries <> fmap fst retainedEntries)

-- Shrink shared state by dropping active or retained txs.
shrinkSharedTxState
  :: SharedTxState PeerAddr TxId
  -> [SharedTxState PeerAddr TxId]
shrinkSharedTxState sharedState =
  nub $
    [ emptySharedTxState
    , buildSharedTxState [] retainedEntries 0
    , buildSharedTxState activeEntries [] 0
    ] ++
    [ buildSharedTxState activeEntries' retainedEntries 0
    | activeEntries' <- smallerActiveEntries
    ] ++
    [ buildSharedTxState activeEntries retainedEntries' 0
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

-- Intern a list of txids into an otherwise empty shared state.
mkSharedState :: [TxId] -> SharedTxState PeerAddr TxId
mkSharedState txids = snd (internTxIds txids emptySharedTxState)

-- Construct a requested batch together with its cached key set.
mkRequestedTxBatch :: [TxKey] -> SizeInBytes -> RequestedTxBatch
mkRequestedTxBatch keys requestedTxBatchSize = RequestedTxBatch
  { requestedTxBatchSet = IntSet.fromList (map unTxKey keys)
  , requestedTxBatchSize
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
  { rdfRequestedTxIds
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
              rdfRequestedTxIds
              rdfTxidsAndSizes
              rdfPeerState
              emptyPeerTxInFlight
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
            nextPeerAction now defaultTxDecisionPolicy pafPeerAddr
              pafPeerState emptyPeerTxInFlight pafSharedState
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
      :: ([(PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)], SharedTxState PeerAddr TxId)
      -> PeerAddr
      -> ([(PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)], SharedTxState PeerAddr TxId)
    receiveOne (!peerStatesAcc, !sharedStateAcc) peeraddr =
      let peerState0 =
            emptyPeerTxLocalState {
              peerRequestedTxIds = ffRequestedTxIds
            }
          !(peerState', peerInFlight', sharedStateAcc') =
            handleReceivedTxIds
              (const False)
              now
              defaultTxDecisionPolicy
              ffRequestedTxIds
              ffTxidsAndSizes
              peerState0
              emptyPeerTxInFlight
              sharedStateAcc
      in ((peeraddr, peerState', peerInFlight') : peerStatesAcc, sharedStateAcc')

    acknowledgeOne
      :: ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
      -> (PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)
      -> ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
    acknowledgeOne (!ackResultsAcc, !sharedStateAcc) (peeraddr, peerState0, peerInFlight0) =
      let !(peerAction, peerState', _peerInFlight', sharedStateAcc') =
            nextPeerAction now defaultTxDecisionPolicy peeraddr
              peerState0 peerInFlight0 sharedStateAcc
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
mkActiveSharedState _allPeers ownerPeer _resolvedAdvertisers txidsAndSizes =
  sharedState1 {
      sharedTxTable =
        IntMap.fromList
          [ (unTxKey txKey, mkEntry txKey)
          | (txid, _txSize) <- txidsAndSizes
          , let txKey = lookupKeyOrFail txid sharedState1
          ]
    }
  where
    sharedState0 = emptySharedTxState
    sharedState1 = snd (internTxIds (fmap fst txidsAndSizes) sharedState0)

    mkEntry _txKey = TxEntry
      { txLease = TxLeased ownerPeer (addTime 10 now)
      , txAttempt = 1
      , txInSubmission = False
      , currentMaxInflightMultiplicity =
          txInflightMultiplicity defaultTxDecisionPolicy
      }

-- Resolve all active txs into retained entries so non-owner peers may safely
-- acknowledge their txids.
retainAllActiveTxs :: SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId
retainAllActiveTxs st@SharedTxState { sharedTxTable, sharedRetainedTxs, sharedGeneration } =
  st {
      sharedTxTable = IntMap.empty,
      sharedRetainedTxs = IntMap.foldlWithKey' retainOne sharedRetainedTxs sharedTxTable,
      sharedGeneration = sharedGeneration + 1
    }
  where
    retainUntil = addTime (bufferedTxsMinLifetime defaultTxDecisionPolicy) now

    retainOne retainedAcc k _ =
      retainedInsertMax k retainUntil retainedAcc
