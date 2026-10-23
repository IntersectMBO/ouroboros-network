{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
  , mkReceiveRetainedFixture
  , mkResolvedAckFixture
  , mkForeignRejectedFixture
  , mkFanoutFixture
  , runReceiveDuplicateLoop
  , swept
  , runPeerActionLoop
  , runFanoutLoop
  , sharedTxStateInvariant
  , peerTxLocalStateInvariant
  , peerTxInFlightInvariant
  , combinedStateInvariant
  , combinedStateInvariantStep
  , sharedGenerationBumpInvariant
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Control.Monad (filterM, foldM, forM, forM_)
import Control.Monad.Class.MonadAsync (withAsync)
import Control.Monad.Class.MonadAsync qualified as Async
import Control.Monad.Class.MonadThrow (try)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), addTime, diffTime,
           getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (IOSim, SimEventType (..), runSimOrThrow,
           runSimTrace, traceEvents, traceM, traceResult)
import Data.Dynamic (fromDynamic)
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Function (on)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, isPrefixOf, mapAccumL, nub, nubBy, sort, sortBy,
           sortOn, unfoldr)
import Data.List as List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Word (Word16, Word64)

import Control.Tracer (Tracer (..), emit, nullTracer)
import NoThunks.Class (NoThunks, unsafeNoThunks)


import Network.TypedProtocol.Codec (AnyMessage)

import Ouroboros.Network.Hashable (hashWithSalt, mkSalt)
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.RegisteredDelay qualified as RegisteredDelay
import Ouroboros.Network.Tx (HasRawTxId (..), getRawTxId)
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry (CounterMirrors (..),
           PeerRegistration, PeerTierEntry (..), PeerTxAPI (..),
           PeerTxRegistry (..), SharedTxStateVar, TxSubmissionCountersVar,
           awaitMirrorChange, drainPause, joinTier, leaveTier,
           newPeerTxRegistry, newSharedTxStateVar, newTxSubmissionCountersVar,
           refreshTierRoots, registerPeerCounters, registerPeerTier,
           retirePeerCounters, rotateTiers, scrubFromPeerInFlight,
           snapshotCountersWith, sweepBatch, sweepInterval, tierCensus,
           txCountersThreadV2, withPeer, withTier, writeSharedStateIfChanged)
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Tier (censusOf, effectiveTier,
           isSource, nextJitter, orderFast, recordNovel, slowWait, sourceUntil,
           standingOf, tierStanding)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
import Test.Ouroboros.Network.TxSubmission.SizedBytes (SizedBytes,
           TxSubmission2Bounds (..), genBoundedMessage, isReplyTxs,
           isRequestTxs, messageSize, shrinkBoundedMessage, withinBounds)

import Test.Ouroboros.Network.TxSubmission.Types

import System.Random (StdGen, mkStdGen)
import System.Random.SplitMix (mkSMGen, nextWord64)
import Test.QuickCheck
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase,
           testCaseSteps, (@?=))
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
    , testGroup "TxDecisionPolicy meta-tests"
          [ testProperty "generator produces sane policies"
                         prop_ArbTxDecisionPolicy_generatesSane
          , testProperty "shrinker produces sane policies"
                         prop_ArbTxDecisionPolicy_shrinkSane
          , testProperty "default policy is sane up to the config bounds"
                         prop_mkTxDecisionPolicy_saneAtBounds
          , testProperty "sane policy accepts exactly the policies within the bounds"
                         prop_saneTxDecisionPolicy_exactBounds
          , testProperty "sane messages generator is valid"
                         prop_ArbSaneMessages_generatesValid
          , testProperty "sane messages shrinker is valid"
                         prop_ArbSaneMessages_shrinkValid
          , testProperty "messages within a sane policy fit within the size limits"
                         prop_txSubmission2Messages_fitByteLimits
          ]
    , testProperty "nextPeerAction processes all multi-peer triggers" prop_nextPeerAction_processesAllTriggers
    , testCaseSteps "peerScore decays linearly over time at scoreRate" unit_peerScore_decaysOverTime
    , testCaseSteps "applyPeerEvents drains the existing score before adding the new rejection count" unit_applyPeerEvents_drainsThenAdds
    , testProperty "handleReceivedTxIds classifies incoming txids" prop_handleReceivedTxIds
    , testCaseSteps "handleReceivedTxIds tracks the advertise without mutating an existing entry" unit_handleReceivedTxIds_advertisesExistingEntry
    , testProperty "handleReceivedTxIds counts a peer advertising an entry once however often the txid comes" prop_handleReceivedTxIds_repeatCountsOnce
    , testProperty "handleReceivedTxs answers a batch by its keys' fates" prop_handleReceivedTxs_keyFates
    , testProperty "a write of the shared state advances the revision by one" prop_writeSharedStateIfChanged_countsOne
    , testProperty "handleSubmittedTxs retains the accepted keys still interned, and a dead one's txid afresh" prop_handleSubmittedTxs_deadKeys
    , testProperty "handleReceivedTxs buffers requested bodies and releases omitted ones" prop_handleReceivedTxs
    , testProperty "handleSubmittedTxs retains accepted and drops rejected" prop_handleSubmittedTxs_retainsAcceptedAndDropsRejected
    , testProperty "handleSubmittedTxs records novelty only off the fast tier" prop_handleSubmittedTxs_novelOffFastOnly
    , testProperty "an idle peer with no work parks on the wake counter" prop_nextPeerAction_parksOnWakeCounter
    , testProperty "nextPeerAction respects the inflight size budget" prop_nextPeerAction_picksTxsRespectingBudget
    , testProperty "nextPeerAction caps each body batch independently of the inflight window" prop_nextPeerAction_capsBodyBatches
    , testProperty "nextPeerAction submits buffered owned txs before acking" prop_nextPeerAction_ownerSubmitsBuffered
    , testCase     "nextPeerAction claims a duplicated advertised txid only once" unit_nextPeerAction_dedupsBodyRequestOnDuplicateTxIds
    , testCase     "nextPeerAction submits a duplicated buffered txid only once"  unit_nextPeerAction_dedupsSubmitOnDuplicateTxIds
    , testCase     "nextPeerAction clears in-flight tracking when an orphaned body is pruned" unit_nextPeerAction_clearsInFlightForPrunedBody
    , testProperty "nextPeerAction prunes expired retained txs" prop_nextPeerAction_prunesExpiredRetained
    , testProperty "nextPeerAction keeps retained txs before expiry" prop_nextPeerAction_keepsRetained
    , testProperty "bounded sweep expires the earliest, at most its limit" prop_sweepUpTo_bounded
    , testProperty "bounded sweeps repeated equal one unbounded sweep" prop_sweepUpTo_converges
    , testProperty "sweep reports a remaining backlog iff one remains" prop_sweepUpTo_reportsBacklog
    , testProperty "sweep keeps a lookup entry iff its key is resident" prop_sweep_lookupEntries
    , testProperty "sweep drops exactly the entries no peer advertises, leases, attempts or submits" prop_sweep_dropsAbandoned
    , testProperty "no sweep drops an entry a peer still advertises" prop_sweep_keepsAdvertised
    , testProperty "an advertised key whose retention expired is acknowledged" prop_sweep_expiredAdvertised
    , testProperty "a leaving peer takes one advertiser off each entry it advertised, writing for nothing else" prop_scrub_advertisers
    , testProperty "bounded sweeps round the table equal one unbounded sweep" prop_sweep_rotation
    , testProperty "one bounded sweep does at most its limit of each kind of work" prop_sweepUpTo_boundedWork
    , testProperty "a walk takes its limit of entries in cyclic key order" prop_walkFrom_cyclic
    , testProperty "expiry alone drops the expired keys with their lookup entries" prop_expireUpTo_lookupEntries
    , testProperty "the counters thread's first sweep is the unbounded sweep at that time" prop_thread_rotation
    , testProperty "the counters thread emits on its cadence through drains" prop_thread_emissionCadence
    , localOption (QuickCheckTests 20) $ testProperty "under a backlog the counters thread's sweeps are at most an interval apart" prop_thread_drainCap
    , localOption (QuickCheckTests 20) $ testProperty "an emission does not delay the counters thread's next sweep" prop_thread_emissionCost
    , testProperty "the shared state's counts are traced under every field's name" prop_countFields_everyField
    , testProperty "nanosecond deadlines never expire early, and expire within 1 ns" prop_retainDeadline_rounding
    , testProperty "counters snapshot counts every peer exactly once" prop_snapshotCounters_exactlyOnce
    , testProperty "the reply-size counters bucket each txid reply by the txids it carried" prop_replySizeCounters
    , testProperty "a release removes its own registration, not another under its address" prop_registry_releaseOwn
    , testProperty "nextPeerActionPipelined rejects request-only (ack=0, req=N) messages" prop_nextPeerActionPipelined_rejectsRequestOnly
    , testCaseSteps "nextPeerActionPipelined rejects pure-ack (ack=N, req=0) messages" unit_nextPeerActionPipelined_rejectsPureAck
    , testProperty "nextPeerActionPipelined emits a pipelined txid request when ack and request fire together" prop_nextPeerActionPipelined_requestsTxIds
    , testProperty "nextPeerActionPipelined opens a second outstanding body batch" prop_nextPeerActionPipelined_secondBodyBatch
    , testProperty "nextPeerActionPipelined does not open a third outstanding body batch" prop_nextPeerActionPipelined_noThirdBodyBatch
    , testCaseSteps "nextPeerActionPipelined keeps one txid unacked while body replies are in flight" unit_nextPeerActionPipelined_keepsOneUnackedWithOutstandingBodyReply
    , testCaseSteps "nextPeerAction skips blocked-available txs and requests later claimable ones" unit_nextPeerAction_skipsBlockedAvailableTxs
    , testCaseSteps "nextPeerAction only acks the safe prefix before a blocked buffered tx" unit_nextPeerAction_acksSafePrefixBeforeBlockedBufferedTx
    , testCaseSteps "nextPeerAction lets another peer claim a fresh tx when the first advertiser is full" unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull
    , testCaseSteps "nextPeerAction claims a released tx from another advertiser" unit_nextPeerAction_claimsRejectedTxFromOtherAdvertiser
    , testCaseSteps "nextPeerAction claims a tx once the score delay threshold has elapsed" unit_nextPeerAction_claimsAtScoreDelayThreshold
    , testCaseSteps "nextPeerAction requests other work despite a blocked buffered tx" unit_nextPeerAction_requestsOtherWorkDespiteBlockedBufferedTx
    , testCaseSteps "nextPeerAction submits buffered bodies across a resolved-elsewhere gap" unit_nextPeerAction_submitsBufferedAcrossResolvedGap
    , testProperty "nextPeerAction keeps non-owner txids unacked until resolved" prop_nextPeerAction_nonOwnerWaitsUntilResolved
    , testProperty "a txid request without acks comes only on an empty unacked queue" prop_nextPeerAction_blockingOnlyWhenEmpty
    , testProperty "nextPeerAction claims a claimable tx for the best idle advertiser" prop_nextPeerAction_claimsClaimableTx
    , testGroup "Tiers"
        [ testProperty "a slow peer's txid requests are never closer than the cadence less its jitter" prop_tier_slowRequestSpacing
        , testProperty "a fast peer is served as under a zero cadence" prop_tier_fastIgnoresCadence
        , testProperty "a slow peer's txid request sets the gate within the jitter band" prop_tier_slowSetsGate
        , testProperty "isSource agrees with the count of accepts inside the window" prop_tier_sourceOracle
        , testProperty "the source deadline agrees with the source rule at every later time" prop_tier_sourceUntil
        , testProperty "recordNovel keeps one entry per accepting submission inside the window, the newest that reach the count" prop_tier_recordNovelTrim
        , localOption (QuickCheckTests 1000) $ testProperty "the sanity check refuses a window at or below zero or over an hour, or a negative source count, and passes the rest" prop_policy_sourceSane
        , testProperty "the order places exactly the first k by salted rank after every join, leave and rotation" prop_tier_orderOracle
        , localOption (QuickCheckTests 20) $ testProperty "the salted order is fair" prop_tier_orderUniform
        , testProperty "the slow wait lies in the jitter band and the generator moves" prop_tier_jitterBand
        , testProperty "a leaving fast peer's slot goes to the next in order at once; a peer holds none before its first decision" prop_tier_registry
        , testProperty "a gated txid request parks on the claimable counter read then" prop_tier_gateParksOnClaimable
        , testProperty "the gate holds txid requests and parks an idle slow peer; a buffered body keeps the generation" prop_tier_gateHoldsOnlyTxIdRequests
        , testProperty "local roots are fast by standing and hold no coin slot" prop_tier_rootsOutsideCoin
        , testProperty "a peer leaving withPeer frees its fast slot at once" prop_withPeer_leaveFreesSlot
        , testProperty "a gated park is on the claimable counter at its value then; its bump wakes it, the other's does not" prop_withPeer_gatedParkOnClaimable
        , testProperty "a fast peer's park is on the wake counter at its value then; its bump wakes it, the other's does not" prop_withPeer_fastParkOnWake
        , testProperty "the census counts each standing once, root before coin before source" prop_tier_censusOracle
        , testProperty "a decision's tier read writes until when it is a source; the standing lapses with the window" prop_tier_withTierStanding
        , testProperty "the tier states and the census follow the flags as the joins, refreshes and decisions left them" prop_tier_censusFollowsFlags
        , testProperty "the request tiers are a queue: a reply takes the oldest request's tier" prop_requestTiers_fifo
        , testProperty "a reply's txids are counted on the tier of the oldest outstanding request" prop_requestTiers_counted
        , testProperty "a txid request records the tier it is served on" prop_requestTier_recorded
        , testProperty "the counters thread rotates the order once a period, not before" prop_countersThread_rotates
        , testProperty "the counters thread follows a root flip within a sweep interval and places" prop_countersThread_refreshesRoots
        , testProperty "a decision is one transaction" prop_decision_oneTransaction
        , testProperty "the counters thread refuses a policy that fails the sanity check" prop_countersThread_refusesInsane
        ]
    , testGroup "Claimable"
        [ testProperty "the claimable counter moves on an omitted body or a rejection, and only then" prop_claimable_releases
        , testProperty "a stuck entry's cap bump moves the claimable counter" prop_claimable_stuckBump
        , testProperty "a leaving peer's scrub moves the claimable counter iff it held work" prop_claimable_scrub
        , testProperty "the slow park wakes on the claimable counter or the delay, not on the generation" prop_claimable_park
        ]
    ]

-- | The 'ArbTxDecisionPolicy' generator only produces policies that pass
-- 'saneTxDecisionPolicy'.
prop_ArbTxDecisionPolicy_generatesSane :: ArbTxDecisionPolicy -> ArbTxSubmissionConfig -> Property
prop_ArbTxDecisionPolicy_generatesSane (ArbTxDecisionPolicy p) (ArbTxSubmissionConfig c) =
    saneTxDecisionPolicy p c === Right ()

-- | Shrinking an 'ArbTxDecisionPolicy' or an 'ArbTxSubmissionConfig' only
-- yields policies and configs that pass 'saneTxDecisionPolicy'.  QuickCheck
-- shrinks one argument at a time, so each shrink is checked with the other
-- argument unchanged.
prop_ArbTxDecisionPolicy_shrinkSane :: ArbTxDecisionPolicy -> ArbTxSubmissionConfig -> Property
prop_ArbTxDecisionPolicy_shrinkSane policy@(ArbTxDecisionPolicy p)
                                    config@(ArbTxSubmissionConfig c) =
    let policies = [ p' | ArbTxDecisionPolicy p'   <- shrink policy ]
        configs  = [ c' | ArbTxSubmissionConfig c' <- shrink config ]
    in checkCoverage
     . cover 80 (not (null policies)) "policy shrinks"
     . cover 80 (not (null configs))  "config shrinks"
     . conjoin
     $ [ sane p' c  | p' <- policies ]
    ++ [ sane p  c' | c' <- configs ]
  where
    sane p' c' = counterexample (show (p', c'))
                $ saneTxDecisionPolicy p' c' === Right ()

-- | The largest window for which a `MsgRequestTxs` with txids of size
-- `max_TXID_SIZE` fits within the `StIdle` size limit:
-- @(65535 - 4) `div` max_TXID_SIZE@.
--
maxSaneNumUnacknowledgedTxIds :: Word16
maxSaneNumUnacknowledgedTxIds = 1489

-- | The largest `maxNumTxIdsToRequest` for which
-- @max_TX_SIZE * maxNumTxIdsToRequest@ fits in `SizeInBytes`:
-- @(2^32 - 1) `div` 65540@.
--
maxSaneNumTxIdsToRequest :: Word16
maxSaneNumTxIdsToRequest = 65532

-- | A `TxSubmissionConfig` with large values, mostly close to the bounds
-- checked by 'saneTxDecisionPolicy'.
--
newtype ArbLargeTxSubmissionConfig =
    ArbLargeTxSubmissionConfig TxSubmissionConfig
  deriving Show

instance Arbitrary ArbLargeTxSubmissionConfig where
  arbitrary =
    fmap ArbLargeTxSubmissionConfig $
    TxSubmissionConfig
      <$> frequency
            [ (1, pure (NumTxIdsToAck maxSaneNumUnacknowledgedTxIds))
            , (1, pure (NumTxIdsToAck (maxSaneNumUnacknowledgedTxIds + 1)))
            , (3, NumTxIdsToAck <$> choose ( maxSaneNumUnacknowledgedTxIds - 10
                                           , maxSaneNumUnacknowledgedTxIds + 10))
            , (1, NumTxIdsToAck <$> arbitraryBoundedIntegral)
            ]
      <*> frequency
            [ (1, pure (NumTxIdsToReq maxSaneNumTxIdsToRequest))
            , (1, pure (NumTxIdsToReq (maxSaneNumTxIdsToRequest + 1)))
            , (3, NumTxIdsToReq <$> choose ( maxSaneNumTxIdsToRequest - 10
                                           , maxBound))
            , (1, NumTxIdsToReq <$> arbitraryBoundedIntegral)
            ]

  -- shrink each field independently; the configs have no invariants
  shrink (ArbLargeTxSubmissionConfig c) =
       [ ArbLargeTxSubmissionConfig c { maxNumUnacknowledgedTxIds = NumTxIdsToAck w }
       | w <- shrink (getNumTxIdsToAck (maxNumUnacknowledgedTxIds c)) ]
    ++ [ ArbLargeTxSubmissionConfig c { maxNumTxIdsToRequest = NumTxIdsToReq r }
       | r <- shrink (getNumTxIdsToReq (maxNumTxIdsToRequest c)) ]

-- | 'mkTxDecisionPolicy' returns a (sane) policy for exactly those
-- configs whose window and `maxNumTxIdsToRequest` are positive and within
-- the bounds.  In
-- particular the reply to the largest batch, with the
-- `const_MAX_TX_SIZE_DISCREPANCY` tolerance for every txid in the window,
-- fits within the `StTxs` size limit, and the default
-- `txsSizeInflightPerPeer` doesn't saturate.
--
prop_mkTxDecisionPolicy_saneAtBounds :: ArbLargeTxSubmissionConfig
                                          -> Property
prop_mkTxDecisionPolicy_saneAtBounds (ArbLargeTxSubmissionConfig c) =
    let window = getNumTxIdsToAck (maxNumUnacknowledgedTxIds c)
        reqNo  = getNumTxIdsToReq (maxNumTxIdsToRequest c)
        result = mkTxDecisionPolicy c
        inBounds =
             window >= 1 && window <= maxSaneNumUnacknowledgedTxIds
          && reqNo  >= 1 && reqNo  <= maxSaneNumTxIdsToRequest
    in checkCoverage
     $ cover 1 (window == maxSaneNumUnacknowledgedTxIds)     "window at the bound"
     $ cover 1 (window == maxSaneNumUnacknowledgedTxIds + 1) "window above the bound"
     $ cover 1 (reqNo  == maxSaneNumTxIdsToRequest)          "request at the bound"
     $ cover 1 (reqNo  == maxSaneNumTxIdsToRequest + 1)      "request above the bound"
     $ cover 20 inBounds "within the bounds"
     $ counterexample (show result)
     $ case result of
         Left _  -> counterexample "mkTxDecisionPolicy rejects the config"
                      (not inBounds)
         Right p ->
           conjoin
             [ counterexample "mkTxDecisionPolicy accepts the config"
                 inBounds
             , counterexample "txsSizeInflightPerPeer saturated"
                 (   toInteger (txsSizeInflightPerPeer p)
                  == toInteger max_TX_SIZE * toInteger reqNo)
             ]

-- | The largest `txsSizePerBatch` for which the reply to a batch with
-- a full window of txs, each up to `const_MAX_TX_SIZE_DISCREPANCY` larger
-- than advertised, fits within the `StTxs` size limit:
-- @2500000 - 4 - 32 * window@.
--
maxSaneTxsSizePerBatch :: Word16 -> Integer
maxSaneTxsSizePerBatch window = 2499996 - 32 * toInteger window

-- | A policy field which `perturbPolicy` puts out of its range.
--
data PolicyField =
    TxsSizeInflightPerPeer
  | TxsSizePerBatch
  | MaxOutstandingTxBatchesPerPeer
  | TxInflightMultiplicity
  | BufferedTxsMinLifetime
  | ScoreRate
  | ScoreMax
  | ScoreAcceptDecrement
  | InterTxSpace
  | InflightTimeout
  | MaxPeerClaimDelay
  | TxFastPeers
  | TxSlowCadence
  | TxSlowJitter
  | TxTierRotation
  | TxSourceCount
  | TxSourceWindow
  deriving (Show, Eq, Enum, Bounded)

perturbPolicy :: PolicyField -> TxDecisionPolicy -> TxDecisionPolicy
perturbPolicy field p = case field of
    TxsSizeInflightPerPeer         -> p { txsSizeInflightPerPeer = 0 }
    TxsSizePerBatch                -> p { txsSizePerBatch = 0 }
    MaxOutstandingTxBatchesPerPeer -> p { maxOutstandingTxBatchesPerPeer = 0 }
    TxInflightMultiplicity         -> p { txInflightMultiplicity = 0 }
    BufferedTxsMinLifetime         -> p { bufferedTxsMinLifetime = -1 }
    ScoreRate                      -> p { scoreRate = -1 }
    ScoreMax                       -> p { scoreMax = -1 }
    ScoreAcceptDecrement           -> p { scoreAcceptDecrement = -1 }
    InterTxSpace                   -> p { interTxSpace = -1 }
    InflightTimeout                -> p { inflightTimeout = interTxSpace p - 0.001 }
    MaxPeerClaimDelay              -> p { maxPeerClaimDelay = -1 }
    TxFastPeers                    -> p { txFastPeers = -1 }
    TxSlowCadence                  -> p { txSlowCadence = -1 }
    TxSlowJitter                   -> p { txSlowJitter = 1 }
    TxTierRotation                 -> p { txTierRotation = 0 }
    TxSourceCount                  -> p { txSourceCount = -1 }
    TxSourceWindow                 -> p { txSourceWindow = maxSourceWindow + 1 }

-- | A policy and a config, mostly close to the bounds checked by
-- 'saneTxDecisionPolicy'.  The policy is built by 'nearBoundsPolicy' from
-- a base policy, a `txsSizePerBatch` and sometimes a field put out of its
-- range, so that each part shrinks on its own.
--
data ArbPolicyNearBounds = ArbPolicyNearBounds {
    apnBasePolicy      :: TxDecisionPolicy,
    apnConfig          :: TxSubmissionConfig,
    apnTxsSizePerBatch :: SizeInBytes,
    apnPerturbed       :: Maybe PolicyField
  }
  deriving Show

nearBoundsPolicy :: ArbPolicyNearBounds -> TxDecisionPolicy
nearBoundsPolicy ArbPolicyNearBounds { apnBasePolicy, apnTxsSizePerBatch,
                                       apnPerturbed } =
    maybe id perturbPolicy apnPerturbed
      apnBasePolicy { txsSizePerBatch = apnTxsSizePerBatch }

nearBoundsSane :: ArbPolicyNearBounds -> Bool
nearBoundsSane a = isRight (saneTxDecisionPolicy (nearBoundsPolicy a) (apnConfig a))

instance Arbitrary ArbPolicyNearBounds where
  arbitrary = do
    ArbTxDecisionPolicy apnBasePolicy <- arbitrary
    apnConfig <- frequency
      [ (1, (\(ArbTxSubmissionConfig c) -> c) <$> arbitrary)
      , (3, (\(ArbLargeTxSubmissionConfig c) -> c) <$> arbitrary)
      ]
    let maxBatch = maxSaneTxsSizePerBatch
                     (getNumTxIdsToAck (maxNumUnacknowledgedTxIds apnConfig))
        discrepancy = toInteger const_MAX_TX_SIZE_DISCREPANCY
    apnTxsSizePerBatch <- frequency
      [ (2, pure (txsSizePerBatch apnBasePolicy))
      , (1, pure (fromIntegral (max 1 maxBatch)))
      , (1, pure (fromIntegral (max 1 (maxBatch + 1))))
      , (4, fromIntegral <$> choose ( max 1 (maxBatch - discrepancy)
                                    , maxBatch + discrepancy))
      ]
    apnPerturbed <- frequency
      [ (4, pure Nothing)
      , (1, Just <$> arbitraryBoundedEnum)
      ]
    pure ArbPolicyNearBounds { apnBasePolicy, apnConfig,
                               apnTxsSizePerBatch, apnPerturbed }

  shrink a@ArbPolicyNearBounds { apnBasePolicy, apnConfig,
                                 apnTxsSizePerBatch, apnPerturbed } =
       [ a { apnPerturbed = Nothing } | Just _ <- [apnPerturbed] ]
    ++ [ a { apnConfig = c }
       | ArbLargeTxSubmissionConfig c <- shrink (ArbLargeTxSubmissionConfig apnConfig) ]
    ++ [ a { apnTxsSizePerBatch = SizeInBytes s }
       | s <- shrink (getSizeInBytes apnTxsSizePerBatch) ]
    ++ [ a { apnBasePolicy = p }
       | ArbTxDecisionPolicy p <- shrink (ArbTxDecisionPolicy apnBasePolicy) ]

-- | 'saneTxDecisionPolicy' accepts exactly the policies and configs within
-- the bounds stated numerically here:
--
-- * window in @[1, 1489]@ (`MsgRequestTxs` within the `StIdle` size limit);
-- * `maxNumTxIdsToRequest` in @[1, 65532]@ (no overflow of the in-flight
--   size);
-- * `txsSizePerBatch` in @[1, 2500000 - 4 - 32 * window]@ (`MsgReplyTxs`
--   within the `StTxs` size limit);
-- * the remaining fields positive or non-negative, @inflightTimeout >=
--   interTxSpace@, the jitter below one and the source window at most
--   `maxSourceWindow`.
--
-- 'prop_txSubmission2Messages_fitByteLimits' checks that these bounds keep
-- the messages within the size limits.
--
prop_saneTxDecisionPolicy_exactBounds :: ArbPolicyNearBounds -> Property
prop_saneTxDecisionPolicy_exactBounds a@ArbPolicyNearBounds { apnConfig = c,
                                                              apnPerturbed } =
    let p      = nearBoundsPolicy a
        window = getNumTxIdsToAck (maxNumUnacknowledgedTxIds c)
        reqNo  = getNumTxIdsToReq (maxNumTxIdsToRequest c)
        batch  = toInteger (txsSizePerBatch p)
        windowInBounds = window >= 1 && window <= maxSaneNumUnacknowledgedTxIds
        inBounds =
             windowInBounds
          && reqNo  >= 1 && reqNo  <= maxSaneNumTxIdsToRequest
          && batch  >= 1 && batch  <= maxSaneTxsSizePerBatch window
          && txsSizeInflightPerPeer         p >  0
          && maxOutstandingTxBatchesPerPeer p >= 1
          && txInflightMultiplicity         p >= 1
          && bufferedTxsMinLifetime         p >= 0
          && scoreRate                      p >= 0
          && scoreMax                       p >= 0
          && scoreAcceptDecrement           p >= 0
          && interTxSpace                   p >= 0
          && inflightTimeout                p >= interTxSpace p
          && maxPeerClaimDelay              p >= 0
          && txFastPeers                    p >= 0
          && txSlowCadence                  p >= 0
          && txSlowJitter                   p >= 0
          && txSlowJitter                   p <  1
          && txTierRotation                 p >  0
          && txSourceCount                  p >= 0
          && txSourceWindow                 p >  0
          && txSourceWindow                 p <= maxSourceWindow
    in checkCoverage
     $ cover 1 (window == maxSaneNumUnacknowledgedTxIds)     "window at the bound"
     $ cover 1 (window == maxSaneNumUnacknowledgedTxIds + 1) "window above the bound"
     $ cover 1 (reqNo  == maxSaneNumTxIdsToRequest)          "request at the bound"
     $ cover 1 (reqNo  == maxSaneNumTxIdsToRequest + 1)      "request above the bound"
     $ cover 1 (windowInBounds && batch == maxSaneTxsSizePerBatch window)
               "batch at the bound"
     $ cover 1 (windowInBounds && batch == maxSaneTxsSizePerBatch window + 1)
               "batch above the bound"
     $ cover 10 (apnPerturbed /= Nothing) "another field out of range"
     $ cover 20 inBounds "within the bounds"
     $ tabulate "out of range" (maybe [] (pure . show) apnPerturbed)
     $ counterexample (show p)
     $ counterexample (show (saneTxDecisionPolicy p c))
     $ isRight (saneTxDecisionPolicy p c) === inBounds

-- | The bounds on messages which a policy and a config allow:
--
-- * at most `maxNumUnacknowledgedTxIds` txids of up to `max_TXID_SIZE` in
--   a message;
-- * a `MsgReplyTxs` with a single tx of up to `max_TX_SIZE` (which can exceed
--   `txsSizePerBatch`), or with txs advertised with up to `txsSizePerBatch`
--   bytes in total.  Each tx can be up to `const_MAX_TX_SIZE_DISCREPANCY`
--   larger than advertised.
--
policyBounds :: TxDecisionPolicy -> TxSubmissionConfig -> TxSubmission2Bounds
policyBounds p c =
    TxSubmission2Bounds {
      tbMaxNumTxIds = window,
      tbMaxTxIdSize = fromIntegral max_TXID_SIZE,
      tbMaxTxSize   = fromIntegral max_TX_SIZE + discrepancy,
      tbMaxTxsSize  = fromIntegral (txsSizePerBatch p)
                    + discrepancy * fromIntegral window
    }
  where
    window = getNumTxIdsToAck (maxNumUnacknowledgedTxIds c)
    discrepancy = fromIntegral const_MAX_TX_SIZE_DISCREPANCY :: Word

-- | A sane policy and config, and messages within the bounds they allow
-- (see 'policyBounds').
--
data ArbSaneMessages =
    ArbSaneMessages ArbPolicyNearBounds
                    [AnyMessage (TxSubmission2 SizedBytes SizedBytes)]
  deriving Show

saneMessagesBounds :: ArbPolicyNearBounds -> TxSubmission2Bounds
saneMessagesBounds a = policyBounds (nearBoundsPolicy a) (apnConfig a)

instance Arbitrary ArbSaneMessages where
  arbitrary = do
    a <- arbitrary `suchThat` nearBoundsSane
    ArbSaneMessages a <$> vectorOf 5 (genBoundedMessage (saneMessagesBounds a))

  -- shrink the policy and config only while they stay sane and the messages
  -- stay within their bounds
  shrink (ArbSaneMessages a msgs) =
       [ ArbSaneMessages a' msgs
       | a' <- shrink a
       , nearBoundsSane a'
       , all (withinBounds (saneMessagesBounds a')) msgs
       ]
    ++ [ ArbSaneMessages a msgs'
       | msgs' <- shrinkList shrinkBoundedMessage msgs
       , not (null msgs')
       ]

-- | The 'ArbSaneMessages' generator only produces sane policies and messages
-- within their bounds.
--
prop_ArbSaneMessages_generatesValid :: ArbSaneMessages -> Property
prop_ArbSaneMessages_generatesValid (ArbSaneMessages a msgs) =
         counterexample "not sane" (nearBoundsSane a)
    .&&. counterexample "messages not within the bounds"
           (all (withinBounds (saneMessagesBounds a)) msgs)

-- | Shrinking an 'ArbSaneMessages' preserves its invariants.  Only the first
-- 100 shrinks are checked: a message with many txids has many shrinks.
--
prop_ArbSaneMessages_shrinkValid :: ArbSaneMessages -> Property
prop_ArbSaneMessages_shrinkValid x =
    conjoin
      [ prop_ArbSaneMessages_generatesValid x'
      | x' <- take 100 (shrink x)
      ]

-- | Messages within the bounds which a sane policy and config allow (see
-- 'policyBounds') fit within the size limits of 'byteLimitsTxSubmission2'
-- when encoded with 'codecTxSubmission2'.
--
prop_txSubmission2Messages_fitByteLimits :: ArbSaneMessages -> Property
prop_txSubmission2Messages_fitByteLimits (ArbSaneMessages _ msgs) =
    let sizes = zip msgs (map messageSize msgs)
    in checkCoverage
     $ cover 5 (or [ isReplyTxs msg
                     && limit - size < toInteger const_MAX_TX_SIZE_DISCREPANCY
                   | (msg, (size, limit)) <- sizes ])
               "MsgReplyTxs within const_MAX_TX_SIZE_DISCREPANCY of the limit"
     $ cover 1 (or [ isRequestTxs msg
                     && limit - size < toInteger max_TXID_SIZE
                   | (msg, (size, limit)) <- sizes ])
               "MsgRequestTxs within a txid of the limit"
     $ conjoin
         [ counterexample (show size ++ " > " ++ show limit)
         $ size <= limit
         | (_, (size, limit)) <- sizes
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
                     False

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
  { pafPeerAddr     :: !PeerAddr
  , pafPeerState    :: !(PeerTxLocalState (Tx TxId))
  , pafPeerInFlight :: !PeerTxInFlight
  , pafSharedState  :: !(SharedTxState PeerAddr TxId)
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
  rnf PeerActionFixture { pafPeerAddr, pafPeerState, pafPeerInFlight, pafSharedState } =
       rnf pafPeerAddr
    `seq` rnf pafPeerState
    `seq` rnf pafPeerInFlight
    `seq` rnf pafSharedState

instance NFData FanoutFixture where
  rnf FanoutFixture { ffPeers, ffRequestedTxIds, ffTxidsAndSizes, ffInitialSharedState } =
       rnf ffPeers
    `seq` rnf ffRequestedTxIds
    `seq` rnf ffTxidsAndSizes
    `seq` rnf ffInitialSharedState

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
     NoThunks tx
  => TxDecisionPolicy
  -> PeerTxLocalState tx
  -> Property
peerTxLocalStateInvariant TxDecisionPolicy { scoreMax }
                          ps@PeerTxLocalState {
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
        (peerRequestedTxs `IntSet.isSubsetOf` availableKeys)
    , counterexample "available keys are not all in the unacknowledged queue"
        (availableKeys `IntSet.isSubsetOf` unackKeys)
    , counterexample "downloaded keys are not all in the unacknowledged queue"
        (downloadedKeys `IntSet.isSubsetOf` unackKeys)
    , counterexample "downloaded and available key sets overlap"
        (IntSet.null (downloadedKeys `IntSet.intersection` availableKeys))
    , counterexample "downloaded and requested key sets overlap"
        (IntSet.null (downloadedKeys `IntSet.intersection` peerRequestedTxs))
    , counterexample "peerRequestedTxs does not match the batch key-set union"
        (peerRequestedTxs === batchKeyUnion)
    , counterexample "peerRequestedTxsSize does not match the sum of batch sizes"
        (peerRequestedTxsSize === batchSizeSum)
    , counterexample ("peerScoreValue is negative: " ++ show scoreVal)
        (scoreVal >= 0)
    , counterexample ("peerScoreValue exceeds scoreMax: "
                      ++ show scoreVal ++ " > " ++ show scoreMax)
        (scoreVal <= scoreMax)
    , checkNoThunks "PeerTxLocalState" ps
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
-- in at most one phase per peer); 'pifLeased' is a subset of their
-- union.
peerTxInFlightInvariant :: PeerTxInFlight -> Property
peerTxInFlightInvariant pif =
  conjoin
    [ counterexample "pifAttempting and pifSubmitting overlap"
        (IntSet.null (pifAttempting pif `IntSet.intersection` pifSubmitting pif))
    , counterexample "pifLeased not contained in pifAttempting `union` pifSubmitting"
        (pifLeased pif
           `IntSet.isSubsetOf`
           (pifAttempting pif `IntSet.union` pifSubmitting pif))
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
--     entry or a retained one, the keys given as expired aside
--     ('combinedStateInvariantExpired').
--   * Each entry's 'txAdvertisers' equals the number of peers in the map
--     whose 'pifAdvertised' contains the key, and its 'txAttempt' the
--     number whose 'pifAttempting' does.
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
     , NoThunks tx
     )
  => TxDecisionPolicy
  -> Map.Map peeraddr (PeerTxLocalState tx, PeerTxInFlight)
  -> SharedTxState peeraddr txid
  -> Property
combinedStateInvariant = combinedStateInvariantExpired IntSet.empty

-- | 'combinedStateInvariant' with the given keys allowed in advertised
-- sets though neither active nor retained: keys whose retention expired
-- before their advertisers acknowledged them, the one way an advertised
-- key loses its residence ('prop_sweep_expiredAdvertised').
combinedStateInvariantExpired
  :: forall peeraddr txid tx.
     ( Ord peeraddr
     , Ord txid
     , HasRawTxId txid
     , Show peeraddr
     , Show txid
     , NoThunks tx
     )
  => IntSet.IntSet
  -> TxDecisionPolicy
  -> Map.Map peeraddr (PeerTxLocalState tx, PeerTxInFlight)
  -> SharedTxState peeraddr txid
  -> Property
combinedStateInvariantExpired expired policy peers sharedState =
  conjoin $
       [ counterexample ("PeerTxLocalState invariant violated for peer "
                          ++ show p)
           (peerTxLocalStateInvariant policy ps)
       | (p, (ps, _)) <- Map.toList peers ]
    ++ [ counterexample ("PeerTxInFlight invariant violated for peer "
                           ++ show p)
           (peerTxInFlightInvariant pif)
       | (p, (_, pif)) <- Map.toList peers ]
    ++ [ sharedTxStateInvariant sharedState ]
    ++ [ counterexample ("peer " ++ show p
                          ++ " advertises keys that are neither active nor retained: "
                          ++ show (IntSet.toList stray))
           (IntSet.null stray)
       | (p, (_, pif)) <- Map.toList peers
       , let stray = pifAdvertised pif `IntSet.difference` (liveKeys `IntSet.union` expired) ]
    ++ [ counterexample ("txAdvertisers mismatch for entry " ++ show k
                          ++ ": expected " ++ show expectedAdvertisers
                          ++ " (peers whose pifAdvertised contains the key)"
                          ++ ", got " ++ show (txAdvertisers entry))
           (txAdvertisers entry === expectedAdvertisers)
       | (k, entry) <- IntMap.toList (sharedTxTable sharedState)
       , let expectedAdvertisers =
               length [ ()
                      | (_, (_, pif)) <- Map.toList peers
                      , IntSet.member k (pifAdvertised pif)
                      ]
       ]
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
              Nothing            -> True
              Just (_, ownerPif) -> IntSet.member k (pifLeased ownerPif))
       | (k, entry) <- IntMap.toList (sharedTxTable sharedState)
       , TxLeased owner _ <- [txLease entry]
       ]
  where
    liveKeys = IntMap.keysSet (sharedTxTable sharedState)
                 `IntSet.union` retainedKeysSet (sharedRetainedTxs sharedState)

-- | Cross-step invariant on shared state: any change to fields other
-- than the two counters ('sharedGeneration' and 'sharedRevision') must
-- advance at least one of them.
--
-- 'writeSharedStateIfChanged' (Registry.hs) treats the revision as a
-- dirty bit: a pure transition whose new state matches the old revision
-- is silently dropped at commit time.  Forgetting to bump the revision
-- in a transition that mutates shared state therefore
-- loses the update in production while still passing the structural
-- single-state 'sharedTxStateInvariant'.
--
-- The check is one-directional: a counter may also bump when no other
-- field changed (e.g. 'handleReceivedTxs' bumps unconditionally).
-- Spurious bumps only cost wake-ups and are not asserted against.  A
-- bump of 'sharedGeneration' is also one of 'sharedRevision', which
-- counts every write.
sharedGenerationBumpInvariant
  :: (Eq peeraddr, Eq txid, HasRawTxId txid)
  => SharedTxState peeraddr txid  -- ^ before
  -> SharedTxState peeraddr txid  -- ^ after
  -> Property
sharedGenerationBumpInvariant before after =
  conjoin
    [ counterexample
        ("sharedGeneration decreased: " ++ show g0 ++ " -> " ++ show g1)
        (g1 >= g0)
    , counterexample
        ("sharedRevision decreased: " ++ show r0 ++ " -> " ++ show r1)
        (r1 >= r0)
    , counterexample
        ("revision not bumped despite state change "
          ++ "(generation " ++ show g0 ++ " -> " ++ show g1
          ++ ", revision " ++ show r0 ++ " -> " ++ show r1 ++ ")")
        (not otherFieldsChanged || r1 > r0)
    , counterexample
        ("sharedGeneration bumped without sharedRevision (generation "
          ++ show g0 ++ " -> " ++ show g1 ++ ", revision " ++ show r0 ++ " -> " ++ show r1 ++ ")")
        (g1 == g0 || r1 > r0)
    ]
  where
    g0 = sharedGeneration before
    g1 = sharedGeneration after
    r0 = sharedRevision   before
    r1 = sharedRevision   after
    -- Align both counters so the derived Eq picks up only the other-field
    -- differences.  Using a record update means any new field added to
    -- 'SharedTxState' is automatically included in the comparison.
    otherFieldsChanged =
      before { sharedGeneration = g1, sharedRevision = r1 } /= after

-- | Step-level variant of 'combinedStateInvariant': runs the standard
-- invariant on the after-state and adds the 'sharedGeneration' bump
-- check across the step.  Use at any call site where a pure transition
-- produces both a before and an after state.
combinedStateInvariantStep
  :: forall peeraddr txid tx.
     ( Ord peeraddr
     , Ord txid
     , HasRawTxId txid
     , Show peeraddr
     , Show txid
     , NoThunks tx
     )
  => TxDecisionPolicy
  -> Map.Map peeraddr (PeerTxLocalState tx, PeerTxInFlight)
  -> SharedTxState peeraddr txid  -- ^ before
  -> SharedTxState peeraddr txid  -- ^ after
  -> Property
combinedStateInvariantStep policy peers before after =
  conjoin
    [ combinedStateInvariant policy peers after
    , sharedGenerationBumpInvariant before after
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
  => SharedTxState peeraddr txid
  -> Property
sharedTxStateInvariant SharedTxState {
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
        (keysRoundTripForward && keysRoundTripBackward)
    , counterexample "tx-key map keys are not the live tx keys"
        (IntMap.keysSet sharedKeyToTxId === liveKeys)
    , counterexample "sharedNextTxKey does not stay ahead of all live tx keys"
        (all (< sharedNextTxKey) (IntSet.toList liveKeys))
    ]
    ++ fmap checkTxEntry activeEntries
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
              (txAttempt >= 0)
          , counterexample "txInSubmission without any peer in submission"
              (not txInSubmission || txAttempt >= 0)
          , counterexample "txAdvertisers is negative"
              (txAdvertisers txEntry >= 0)
          ]

newtype ArbTxDecisionPolicy = ArbTxDecisionPolicy TxDecisionPolicy
  deriving (Eq, Show)

newtype ArbSharedTxState = ArbSharedTxState (SharedTxState PeerAddr TxId)
  deriving Show

newtype ArbPeerTxLocalState = ArbPeerTxLocalState (PeerTxLocalState (Tx TxId))
  deriving Show

-- | Tag classifying how @handleReceivedTxIds@ should resolve each incoming
-- txid.
--
-- * 'TxIdNew' - fresh txid, hits the new-entry branch.
-- * 'TxIdRetained' - pre-seeded into 'sharedRetainedTxs', hits the retained
--   branch.
-- * 'TxIdMempool' - mempool-known and not previously in 'sharedTxTable',
--   hits the mempool branch with a @Nothing@ lookup.
-- * 'TxIdMempoolResolvesActive' - mempool-known and pre-seeded into
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
        [ (1, pure (ArbTxDecisionPolicy (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)))
        , (9, do
            interTxSpaceVal <- realToFrac <$> choose (0 :: Double, 1)
            offset          <- choose (0.01 :: Double, 10)
            let inflightTimeoutVal = interTxSpaceVal + realToFrac offset
            ArbTxDecisionPolicy <$> (
              TxDecisionPolicy
                <$> (SizeInBytes . getPositive <$> arbitrary)
                -- biased towards values below `max_TX_SIZE`, for which
                -- a larger tx is requested in a batch on its own
                <*> (SizeInBytes <$> frequency
                      [ (1, choose (1, getSizeInBytes max_TX_SIZE - 1))
                      , (1, choose (getSizeInBytes max_TX_SIZE,
                                    getSizeInBytes (txsSizePerBatch (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2))))
                      ])
                <*> choose (1, 10)
                <*> (getSmall . getPositive <$> arbitrary)
                <*> (realToFrac <$> choose (0 :: Double, 2))
                <*> choose (0, 1)
                <*> choose (0, 1800)
                <*> choose (0, 5)
                <*> pure interTxSpaceVal
                <*> pure inflightTimeoutVal
                <*> (realToFrac <$> choose (0.1, 1 :: Double))
                -- fast slots often below the peer counts the simulations
                -- draw, so fast and slow peers run together
                <*> frequency [(1, choose (0, 50)), (1, choose (0, 10))]
                <*> (realToFrac <$> choose (0 :: Double, 2))
                <*> choose (0, 0.9)
                <*> (realToFrac <$> choose (1 :: Double, 600))
                <*> frequency [(1, pure 0), (3, choose (1, 20))]
                <*> (realToFrac <$> choose (0.1 :: Double, 60))))
        ]

    shrink (ArbTxDecisionPolicy a)
      | a == unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2 = []
      | otherwise = nub $
          ArbTxDecisionPolicy (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
        :  [ ArbTxDecisionPolicy a { txsSizeInflightPerPeer = SizeInBytes s }
           | Positive s <- shrink (Positive (getSizeInBytes (txsSizeInflightPerPeer a)))
           ]
        ++ [ ArbTxDecisionPolicy a { txsSizePerBatch = SizeInBytes s }
           | Positive s <- shrink (Positive (getSizeInBytes (txsSizePerBatch a)))
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
        ++ [ ArbTxDecisionPolicy a { scoreAcceptDecrement = x }
           | NonNegative x <- shrink (NonNegative (scoreAcceptDecrement a))
           ]
        ++ [ ArbTxDecisionPolicy a { interTxSpace = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (interTxSpace a) :: Double))
           ]
        ++ [ ArbTxDecisionPolicy a { inflightTimeout = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (inflightTimeout a) :: Double))
           , realToFrac x > interTxSpace a
           ]
        ++ [ ArbTxDecisionPolicy a { txFastPeers = x }
           | NonNegative x <- shrink (NonNegative (txFastPeers a))
           ]
        ++ [ ArbTxDecisionPolicy a { txSlowCadence = realToFrac x }
           | NonNegative x <- shrink (NonNegative (realToFrac (txSlowCadence a) :: Double))
           ]
        ++ [ ArbTxDecisionPolicy a { txSlowJitter = x }
           | NonNegative x <- shrink (NonNegative (txSlowJitter a)), x < 1
           ]
        ++ [ ArbTxDecisionPolicy a { txTierRotation = realToFrac x }
           | Positive x <- shrink (Positive (realToFrac (txTierRotation a) :: Double))
           ]
        ++ [ ArbTxDecisionPolicy a { txSourceCount = x }
           | NonNegative x <- shrink (NonNegative (txSourceCount a))
           ]
        ++ [ ArbTxDecisionPolicy a { txSourceWindow = realToFrac x }
           | Positive x <- shrink (Positive (realToFrac (txSourceWindow a) :: Double))
           ]

instance Arbitrary ArbPeerTxLocalState where
  arbitrary = ArbPeerTxLocalState <$> genPeerTxLocalState

  -- a candidate equal to the input would shrink forever
  shrink (ArbPeerTxLocalState peerState)
    | peerState == emptyPeerTxLocalState = []
    | otherwise =
        [ ArbPeerTxLocalState peerState'
        | peerState' <-
            [ emptyPeerTxLocalState
            , peerState
                { peerRequestedTxs = IntSet.empty
                , peerRequestedTxBatches = StrictSeq.empty
                , peerRequestedTxsSize = 0
                , peerRequestedTxIds = 0
                , peerDownloadedTxs = IntMap.empty
                }
            ]
        , peerState' /= peerState
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
    -- Pin scoreRate to a value that makes the test arithmetic obvious;
    -- the production default may be tuned without affecting this
    -- formula-level invariant.
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2) { scoreRate = 0.1 }
    score0 = PeerScore { peerScoreValue = 10, peerScoreTs = Time 0 }

-- | A new rejection drains the existing score at 'scoreRate' per second
-- since its last update before adding the rejection count.
unit_applyPeerEvents_drainsThenAdds :: (String -> IO ()) -> Assertion
unit_applyPeerEvents_drainsThenAdds step = do
    step "Starting score 10 at Time 0; one rejection 50s later: 10 - (50 * 0.1) + 1 = 6"
    fst (applyPeerEvents policy (Time 50) 0 1 peerState0) @?= 6
  where
    -- See 'unit_peerScore_decaysOverTime' for why the test pins
    -- scoreRate explicitly.
    policy     = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2) { scoreRate = 0.1 }
    peerState0 = emptyPeerTxLocalState
                   { peerScore = PeerScore { peerScoreValue = 10
                                           , peerScoreTs    = Time 0 } }

--
-- handleReceivedTxIds
--

-- | Verifies that 'handleReceivedTxIds' classifies each incoming txid:
--
--   * 'TxIdNew' - interned and a fresh 'TxClaimable' entry is created;
--     the key joins 'peerAvailableTxIds' and 'pifAdvertised'.
--   * 'TxIdRetained' - already in 'sharedRetainedTxs'; no entry created
--     and the key is not added to 'pifAdvertised'.
--   * 'TxIdMempool' - interned, moved straight to 'sharedRetainedTxs'
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
    peerState0 :: PeerTxLocalState (Tx TxId)
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

    retainUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)

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
            not (retainedMember k (sharedRetainedTxs sharedState'))
        ]

    checkRetained (txid, _) =
      let k = keyOf txid in
      counterexample ("retained tx " ++ show txid) $ conjoin
        [ counterexample "leaked into sharedTxTable"
            (IntMap.notMember k (sharedTxTable sharedState'))
        , counterexample "missing from retained"
            (retainedMember k (sharedRetainedTxs sharedState'))
        , counterexample "leaked into pifAdvertised"
            (IntSet.notMember k (pifAdvertised peerInFlight'))
        ]

    checkMempool (txid, _) =
      let k = keyOf txid in
      counterexample ("mempool tx " ++ show txid) $ conjoin
        [ counterexample "leaked into sharedTxTable"
            (IntMap.notMember k (sharedTxTable sharedState'))
        , counterexample "missing or wrong retainUntil"
            (retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil)
        , counterexample "leaked into pifAdvertised"
            (IntSet.notMember k (pifAdvertised peerInFlight'))
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
      , combinedStateInvariantStep policy
          (Map.singleton peerAddr (peerState', peerInFlight'))
          sharedState0 sharedState'
      ]
  where
    peerAddr = 1 :: PeerAddr

-- | When a peer advertises a txid whose entry already exists, the entry
-- gains an advertiser and nothing else: the per-peer 'PeerTxInFlight'
-- records the advertisement, the entry counts it.
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
          handleReceivedTxIds (const False) now (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                              1 [(txid, 256)]
                              emptyPeerTxLocalState { peerRequestedTxIds = 1 }
                              emptyPeerTxInFlight
                              existing

    step "Entry in sharedTxTable gains an advertiser and nothing else"
    let entryAfter = lookupEntryOrFail (lookupKeyOrFail txid sharedState') sharedState'
    entryAfter @?= entryBefore { txAdvertisers = txAdvertisers entryBefore + 1 }

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

-- | A peer coming to advertise an active entry is one more advertiser,
-- once: a txid repeated within the reply, or advertised again while still
-- advertised, adds nothing more.
prop_handleReceivedTxIds_repeatCountsOnce :: Positive Int -> Bool -> Property
prop_handleReceivedTxIds_repeatCountsOnce (Positive repeats) secondReply =
  let txid :: TxId
      txid = 7
      existing = mkActiveSharedState [0] 0 [] [(txid, 256)]
      k = unTxKey (lookupKeyOrFail txid existing)
      before = txAdvertisers (lookupEntryOrFail (TxKey k) existing)
      reply = replicate repeats (txid, 256)
      receive pif st =
        case handleReceivedTxIds (const False) now
                                   (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                                 (fromIntegral (length reply)) reply
                                 emptyPeerTxLocalState { peerRequestedTxIds = fromIntegral (length reply) }
                                 pif st of
          (_, pif', st') -> (pif', st')
      (pif1, st1) = receive emptyPeerTxInFlight existing
      -- The same peer advertising the txid again in a later reply.
      (pif2, st2) = if secondReply then receive pif1 st1 else (pif1, st1)
  in counterexample (show (st1, st2)) $
          counterexample "advertisers" (txAdvertisers (lookupEntryOrFail (TxKey k) st2) === before + 1)
     .&&. counterexample "advertised" (pifAdvertised pif2 === IntSet.singleton k)

-- | What became of a key this peer requested by the time the reply comes:
-- still live; dead, its lookup entries gone; re-interned under a fresh
-- key another peer advertises; or re-interned under a fresh key this peer
-- requested again in a later batch.
data KeyFate = LiveKey | DeadKey | Reinterned | RequestedAgain
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A key of the batch answered: its fate, and whether its body is in the
-- reply.
data RequestedKey = RequestedKey KeyFate Bool
  deriving Show

instance Arbitrary RequestedKey where
  arbitrary = RequestedKey <$> elements [minBound .. maxBound] <*> arbitrary
  shrink (RequestedKey fate arrives) =
       [ RequestedKey fate' arrives | fate' <- [minBound .. maxBound], fate' < fate ]
    ++ [ RequestedKey fate True | not arrives ]

-- | A reply answers its batch key by key.  A live key's body is buffered,
-- and its absence an omission that releases the lease and the advertiser.
-- A body for a key dead or re-interned since is late, not omitted, and
-- answers that request only: it is buffered against no other, a later
-- request for the same txid stays outstanding; its absence is an
-- omission.  The peer is done with the batch but for the buffered keys,
-- and the shared state changes only where a lease was released.
prop_handleReceivedTxs_keyFates :: NonEmptyList RequestedKey -> Property
prop_handleReceivedTxs_keyFates (NonEmpty requested) =
  let txids = zip [1 :: TxId ..] requested
      -- The live keys' entries, leased to this peer, and the fresh entries
      -- of the re-interned txids: leased to this peer where it requested
      -- the txid again, claimable and another peer's otherwise.
      shared0 = mkActiveSharedState [peerAddr, other] peerAddr []
                  [ (txid, 64) | (txid, RequestedKey fate _) <- txids, fate /= DeadKey ]
      shared1 = shared0 {
          sharedTxTable =
            IntMap.fromList
              [ (k, if fate == Reinterned then e { txLease = TxClaimable now, txAttempt = 0 } else e)
              | (txid, RequestedKey fate _) <- txids
              , fate /= DeadKey
              , let k = unTxKey (lookupKeyOrFail txid shared0)
              , Just e <- [IntMap.lookup k (sharedTxTable shared0)]
              ]
        }
      freshKey txid = unTxKey (lookupKeyOrFail txid shared1)
      -- The key the batch holds: the live key, or a key no txid maps to.
      batchKey (txid, RequestedKey LiveKey _) = freshKey txid
      batchKey (txid, _)                      = 1000 + txid
      batchKeys = map batchKey txids
      laterKeys = [ freshKey txid | (txid, RequestedKey RequestedAgain _) <- txids ]
      batchOf ks = RequestedTxBatch { requestedTxBatchSet = IntSet.fromList ks
                                    , requestedTxBatchSize = 64 * fromIntegral (length ks) }
      peerState = (emptyPeerTxLocalState :: PeerTxLocalState (Tx TxId)) {
          peerUnacknowledgedTxIds = StrictSeq.fromList (map TxKey (batchKeys ++ laterKeys)),
          peerAvailableTxIds      = IntMap.fromList [ (k, 64) | k <- batchKeys ++ laterKeys ],
          peerRequestedTxs        = IntSet.fromList (batchKeys ++ laterKeys),
          peerRequestedTxBatches  = StrictSeq.fromList (batchOf batchKeys : [ batchOf laterKeys | not (null laterKeys) ]),
          peerRequestedTxsSize    = 64 * fromIntegral (length batchKeys + length laterKeys)
        }
      pif = emptyPeerTxInFlight {
          pifAttempting = IntSet.fromList (batchKeys ++ laterKeys),
          pifLeased     = IntSet.fromList (batchKeys ++ laterKeys),
          pifAdvertised = IntSet.fromList (batchKeys ++ laterKeys)
        }
      otherPif = emptyPeerTxInFlight {
          pifAdvertised = IntSet.fromList [ freshKey txid | (txid, RequestedKey Reinterned _) <- txids ]
        }
      bodies = [ (txid, mkTx txid 64) | (txid, RequestedKey _ True) <- txids ]
      (omitted, late, peerState', pif', shared') =
        handleReceivedTxs (const False) now (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                            peerAddr bodies peerState pif shared1
      buffered = IntSet.fromList [ freshKey txid | (txid, RequestedKey LiveKey True) <- txids ]
      kept     = buffered `IntSet.union` IntSet.fromList laterKeys
      liveOmitted = or [ True | (_, RequestedKey LiveKey False) <- txids ]
  in counterexample (show (peerState', pif', shared')) $
       classify liveOmitted "omits a live key" $
       classify (any (\(RequestedKey fate arrives) -> fate /= LiveKey && arrives) requested) "has a late body" $
       counterexample "omitted"
         (omitted === length [ () | RequestedKey _ False <- requested ])
       .&&. counterexample "late"
              (late === length [ () | RequestedKey fate True <- requested, fate /= LiveKey ])
       .&&. counterexample "buffered"
              (IntMap.keysSet (peerDownloadedTxs peerState') === buffered)
       .&&. counterexample "requests still outstanding"
              (peerRequestedTxs peerState' === IntSet.fromList laterKeys)
       .&&. counterexample "the peer's keys in flight"
              (pifAttempting pif' === kept .&&. pifLeased pif' === kept .&&. pifAdvertised pif' === kept)
       .&&. counterexample "the shared state changes exactly when a live key was omitted"
              ((sharedRevision shared' > sharedRevision shared1) === liveOmitted
               .&&. (sharedGeneration shared' > sharedGeneration shared1) === liveOmitted)
       .&&. combinedStateInvariantStep (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
              (Map.fromList [ (peerAddr, (peerState', pif')), (other, (emptyPeerTxLocalState, otherPif)) ])
              shared1 shared'
  where
    peerAddr, other :: PeerAddr
    peerAddr = 0
    other    = 1

-- | A write of the shared state advances the revision by exactly one past
-- the revision read, whatever the step bumped it by, and brings the
-- counters' mirrors up to the state's; a step that moved neither counter
-- writes nothing, the mirrors included.
prop_writeSharedStateIfChanged_countsOne :: Word64 -> Positive Word64 -> Word64 -> Word64 -> Property
prop_writeSharedStateIfChanged_countsOne revision0 (Positive bumps) generation claimable = runSimOrThrow $ do
    let st0 = (emptySharedTxState :: SharedTxState PeerAddr TxId) { sharedRevision = revision0 }
        st1 = st0 { sharedRevision   = sharedRevision st0 + bumps
                  , sharedGeneration = generation
                  , sharedClaimable  = claimable }
    mirrors <- CounterMirrors <$> newTVarIO 0 <*> newTVarIO 0
    var <- newSharedTxStateVar st0
    atomically $ writeSharedStateIfChanged mirrors var (sharedRevision st0) st1
    written <- readTVarIO var
    mirrored <- readMirrors mirrors
    same <- newSharedTxStateVar st0
    atomically $ writeSharedStateIfChanged mirrors same (sharedRevision st0) st0
    unwritten <- readTVarIO same
    mirroredAfter <- readMirrors mirrors
    pure $ counterexample (show written) $
           counterexample "revision after a write" (sharedRevision written === sharedRevision st0 + 1)
      .&&. counterexample "generation after a write" (sharedGeneration written === generation)
      .&&. counterexample "the mirrors follow a write" (mirrored === (generation, claimable))
      .&&. counterexample "no write without a bump" (sharedRevision unwritten === sharedRevision st0)
      .&&. counterexample "the mirrors are untouched without a write" (mirroredAfter === (generation, claimable))
  where
    readMirrors CounterMirrors { generationMirror, claimableMirror } =
      (,) <$> readTVarIO generationMirror <*> readTVarIO claimableMirror

-- | What became of a dead key's txid meanwhile: nothing more, another
-- peer advertised it again (an active entry under a fresh key, claimable
-- or leased to that peer), or another peer's submission retained it
-- again, until a deadline so many seconds from now.
data DeadFate = Gone | ActiveAgain AgainLease | RetainedAgain Int
  deriving (Eq, Show)

-- | The lease of an entry active again: none, or another peer's, with
-- the attempt a lease comes with.
data AgainLease = AgainClaimable | AgainLeased
  deriving (Eq, Show)

instance Arbitrary AgainLease where
  arbitrary = elements [AgainClaimable, AgainLeased]
  shrink AgainLeased    = [AgainClaimable]
  shrink AgainClaimable = []

instance Arbitrary DeadFate where
  arbitrary = frequency [ (2, pure Gone), (1, ActiveAgain <$> arbitrary)
                        , (1, RetainedAgain <$> choose (-5, 600)) ]
  shrink Gone              = []
  shrink (ActiveAgain l)   = Gone : [ ActiveAgain l' | l' <- shrink l ]
  shrink (RetainedAgain d) = Gone : ActiveAgain AgainClaimable : [ RetainedAgain d' | d' <- shrink d ]

-- | The lease a live key's entry carries when the submission completes:
-- this peer's still, another peer's (taken once this peer's expired), or
-- none (expired and released).
data LeaseState = OwnLease | OtherLease | NoLease
  deriving (Eq, Show)

instance Arbitrary LeaseState where
  arbitrary = frequency [ (2, pure OwnLease), (1, pure OtherLease), (1, pure NoLease) ]
  shrink OwnLease = []
  shrink _        = [OwnLease]

-- | A key of a submission's outcome: still interned, or dead, its lookup
-- entries gone, another peer having retained the tx meanwhile and that
-- retention having expired, with what became of its txid since; whether
-- the mempool accepted it; and, live, the lease its entry carries.
data SubmittedKey = SubmittedKey { skLive :: Bool, skAccepted :: Bool, skFate :: DeadFate, skLease :: LeaseState }
  deriving Show

instance Arbitrary SubmittedKey where
  arbitrary = SubmittedKey <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (SubmittedKey live accepted fate lease) =
       [ SubmittedKey True accepted fate lease | not live ]
    ++ [ SubmittedKey live True fate lease | not accepted ]
    ++ [ SubmittedKey live accepted fate' lease | fate' <- shrink fate ]
    ++ [ SubmittedKey live accepted fate lease' | lease' <- shrink lease ]

-- | A submission completing retains each accepted key still interned,
-- under the retention deadline, keeps each rejected one active, claimable
-- and with this peer no longer advertising it, and retains an accepted
-- dead key's txid afresh, under a key allocated now, so that a lagging
-- advertisement of it is acknowledged rather than fetched again; unless
-- the txid is interned again meanwhile: active under another key, that
-- entry is retained under it too, as after a mempool hit, whoever leases
-- it; retained again, the retention is extended to the later deadline.
-- A rejected dead key retains nothing and changes nothing.  The peer is
-- done with all of them.
prop_handleSubmittedTxs_deadKeys :: ArbTxDecisionPolicy -> NonEmptyList SubmittedKey -> Property
prop_handleSubmittedTxs_deadKeys (ArbTxDecisionPolicy policy) (NonEmpty submitted) =
  let txids = zip [1 :: TxId ..] submitted
      -- The live keys' entries, in submission by this peer, each with the
      -- lease its key says: this peer's, another's, or none.
      shared0 = mkActiveSharedState [peerAddr] peerAddr []
                  [ (txid, 64) | (txid, SubmittedKey True _ _ _) <- txids ]
      leaseOfTxid = Map.fromList [ (txid, skLease sk) | (txid, sk) <- txids ]
      shared1 = shared0 {
          sharedTxTable = IntMap.mapWithKey release (sharedTxTable shared0)
        }
      release k e =
        e { txAttempt = 0, txInSubmission = True
          , txLease = case leaseOfTxid Map.! (sharedKeyToTxId shared0 IntMap.! k) of
                        OwnLease   -> txLease e
                        OtherLease -> TxLeased other (addTime 10 now)
                        NoLease    -> TxClaimable now }
      -- The key submitted: the live key, or a key no txid maps to.
      keyOf (txid, SubmittedKey True _ _ _) = unTxKey (lookupKeyOrFail txid shared1)
      keyOf (txid, _)                       = 1000 + txid
      keyed = [ (keyOf t, txid, sk) | t@(txid, sk) <- txids ]
      keys  = [ k | (k, _, _) <- keyed ]
      -- The dead txids interned again meanwhile, by another peer: active
      -- under a fresh key, or retained until a deadline of their own.
      broughtBack = [ (txid, fate) | (txid, SubmittedKey False _ fate _) <- txids, fate /= Gone ]
      shared2 = List.foldl' bringBack shared1 broughtBack
      bringBack st (txid, fate) =
        let (_, TxKey k, st') = internTxId txid st in
        case fate of
          ActiveAgain l   -> st' { sharedTxTable = IntMap.insert k (againEntry l) (sharedTxTable st') }
          RetainedAgain d -> st' { sharedRetainedTxs = retainedInsertMax k (deadlineAt d) (sharedRetainedTxs st') }
          Gone            -> st'
      againEntry l = TxEntry {
          txLease = case l of
                      AgainClaimable -> TxClaimable now
                      AgainLeased    -> TxLeased againOwner (addTime 10 now),
          txAttempt = case l of
                        AgainClaimable -> 0
                        AgainLeased    -> 1,
          txAdvertisers = case l of
                            AgainClaimable -> 0
                            AgainLeased    -> 1,
          txInSubmission = False,
          currentMaxInflightMultiplicity = txInflightMultiplicity policy
        }
      deadlineAt d = retainDeadline (addTime (fromIntegral (d :: Int)) now)
      againKey txid = unTxKey (lookupKeyOrFail txid shared2)
      accepted = [ (TxKey k, txid) | (k, txid, SubmittedKey _ True _ _) <- keyed ]
      rejected = [ TxKey k | (k, _, SubmittedKey _ False _ _) <- keyed ]
      pif = emptyPeerTxInFlight {
          pifSubmitting = IntSet.fromList keys,
          pifLeased     = IntSet.fromList [ k | (k, _, SubmittedKey True _ _ OwnLease) <- keyed ],
          pifAdvertised = IntSet.fromList keys
        }
      peerState = (emptyPeerTxLocalState :: PeerTxLocalState (Tx TxId)) {
          peerUnacknowledgedTxIds = StrictSeq.fromList (map TxKey keys),
          peerDownloadedTxs       = IntMap.fromList [ (k, mkTx txid 64) | (k, txid, _) <- keyed ]
        }
      (peerState', pif', shared') =
        handleSubmittedTxs now policy peerAddr accepted rejected peerState pif shared2
      retainUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)
      -- every accepted txid is retained, the live ones under their key, the
      -- dead ones under keys allocated by this step, or under the key they
      -- are active again under; one retained again stays retained
      -- whatever the outcome, until the later of the two deadlines when
      -- accepted
      retained = retainedToList (sharedRetainedTxs shared')
      txidOfKey k = IntMap.lookup k (sharedKeyToTxId shared')
      retainedTxIds = sort [ txid | (k, _) <- retained, Just txid <- [txidOfKey k] ]
      expectRetainedTxIds =
        sort [ txid | (_, txid, SubmittedKey live acc fate _) <- keyed
                    , acc || isRetainedAgain (live, fate) ]
      isRetainedAgain (False, RetainedAgain _) = True
      isRetainedAgain _                        = False
      expectDeadline (SubmittedKey False acc (RetainedAgain d) _)
        | acc       = max (deadlineAt d) retainUntil
        | otherwise = deadlineAt d
      expectDeadline _ = retainUntil
      deadlinesOf txid = [ until' | (k, until') <- retained, txidOfKey k == Just txid ]
      freshKeys = [ k | (k, _) <- retained, k >= sharedNextTxKey shared2 ]
      deadAccepted = length [ () | (_, _, SubmittedKey False True Gone _) <- keyed ]
      -- the accepted txids' entries active again are retained; a rejected
      -- dead key's stays, the rejection changing nothing
      acceptedTxIds  = [ txid | (_, txid, SubmittedKey _ True _ _) <- keyed ]
      rejectedLive   = IntSet.fromList [ k | (k, _, SubmittedKey True False _ _) <- keyed ]
      expectActive   = rejectedLive `IntSet.union`
                       IntSet.fromList [ againKey txid | (txid, ActiveAgain _) <- broughtBack
                                                       , txid `notElem` acceptedTxIds ]
      -- a third peer, apart from the one the live keys' other leases
      -- name: leasing, attempting and advertising the entries active
      -- again under its lease, before and after
      againOwner  = peerAddr + 2
      leasedAgain = IntSet.fromList [ againKey txid | (txid, ActiveAgain AgainLeased) <- broughtBack ]
      ownerPif    = emptyPeerTxInFlight {
          pifLeased     = leasedAgain,
          pifAttempting = leasedAgain,
          pifAdvertised = leasedAgain
        }
      peers'      = Map.fromList [ (peerAddr, (peerState', pif'))
                                 , (againOwner, (emptyPeerTxLocalState, ownerPif)) ]
      -- a rejected entry is out of submission; this peer's lease is gone,
      -- another peer's stays
      rejectedEntry e =
        counterexample (show e) $
          property (not (txInSubmission e))
          .&&. case txLease e of
                 TxLeased owner _ -> counterexample "still leased by this peer" (owner =/= peerAddr)
                 TxClaimable _    -> property True
  in counterexample (show shared') $
       classify (any (not . skLive) submitted) "has dead keys" $
       classify (not (null broughtBack)) "has txids interned again" $
       counterexample "every accepted txid retained, nothing else"
         (retainedTxIds === expectRetainedTxIds)
       .&&. counterexample "retained until the deadline"
              (conjoin [ counterexample (show txid) (deadlinesOf txid === [expectDeadline sk])
                       | (_, txid, sk) <- keyed, txid `elem` expectRetainedTxIds ])
       .&&. counterexample "the dead accepted ones under fresh keys"
              (length freshKeys === deadAccepted)
       .&&. counterexample "active after"
              (IntMap.keysSet (sharedTxTable shared') === expectActive)
       .&&. counterexample "an accepted txid's entry active again is retained under its key"
              (conjoin [ counterexample (show txid) (property (againKey txid `elem` map fst retained))
                       | (txid, ActiveAgain _) <- broughtBack, txid `elem` acceptedTxIds ])
       .&&. counterexample "a rejected dead key's entry active again is left alone"
              (conjoin [ IntMap.lookup (againKey txid) (sharedTxTable shared') === Just (againEntry l)
                       | (txid, ActiveAgain l) <- broughtBack, txid `notElem` acceptedTxIds ])
       .&&. counterexample "rejected entries"
              (conjoin [ rejectedEntry e | (k, e) <- IntMap.toList (sharedTxTable shared')
                                         , k `IntSet.member` rejectedLive ])
       .&&. counterexample "the peer is done with them"
              (pifSubmitting pif' === IntSet.empty
               .&&. pifLeased pif' === IntSet.empty
               .&&. pifAdvertised pif' === IntSet.empty)
       -- only a live key's rejection reopens an entry (out of submission,
       -- this peer's lease released): a wake, whatever lease the entry
       -- carried; a dead key has no entry to reopen
       .&&. counterexample "the claimable counter moves iff a live key was rejected"
              (sharedClaimable shared' === sharedClaimable shared2
                 + (if any (\(_, _, SubmittedKey live acc _ _) -> live && not acc) keyed then 1 else 0))
       .&&. combinedStateInvariantStep policy peers' shared2 shared'
       .&&. peerTxLocalStateInvariant policy peerState'
  where
    peerAddr, other :: PeerAddr
    peerAddr = 1
    other    = 2

-- | A submission's accepts record novelty only off the fast tier: a peer
-- the coin or its standing has fast wins the races and would make a
-- source of itself; one served on the slow tier records what the mempool
-- accepted, up to the source count.
prop_handleSubmittedTxs_novelOffFastOnly
  :: ArbTxDecisionPolicy -> Bool -> Positive Int -> Property
prop_handleSubmittedTxs_novelOffFastOnly (ArbTxDecisionPolicy policy0) fast (Positive n0) =
  let n       = n0 `mod` 12 + 1
      policy  = policy0 { txSourceCount = max 1 (txSourceCount policy0) }
      txids   = [1 .. n] :: [TxId]
      shared0 = mkActiveSharedState [peerAddr] peerAddr [] [ (txid, 64) | txid <- txids ]
      shared1 = shared0 {
          sharedTxTable = IntMap.map (\e -> e { txAttempt = 0, txInSubmission = True }) (sharedTxTable shared0)
        }
      accepted  = [ (lookupKeyOrFail txid shared1, txid) | txid <- txids ]
      keys      = [ unTxKey k | (k, _) <- accepted ]
      pif       = emptyPeerTxInFlight {
          pifSubmitting = IntSet.fromList keys,
          pifLeased     = IntSet.fromList keys,
          pifAdvertised = IntSet.fromList keys
        }
      peerState = (emptyPeerTxLocalState :: PeerTxLocalState (Tx TxId)) {
          peerTier                = if fast then FastTier else SlowTier,
          peerUnacknowledgedTxIds = StrictSeq.fromList (map TxKey keys),
          peerDownloadedTxs       = IntMap.fromList [ (unTxKey k, mkTx txid 64) | (k, txid) <- accepted ]
        }
      (peerState', _, _) = handleSubmittedTxs now policy peerAddr accepted [] peerState pif shared1
      expected = if fast then StrictSeq.empty else StrictSeq.singleton (now, n)
  in counterexample (show (peerNovelAt peerState')) $
       cover 30 fast "fast" $
       cover 30 (not fast) "slow" $
       peerNovelAt peerState' === expected
  where
    peerAddr :: PeerAddr
    peerAddr = 1

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
          List.foldl' (\tbl k -> IntMap.adjust (claimEntry k) (unTxKey k) tbl)
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
                    other         -> counterexample (show other) False
                , counterexample "txAttempt not decremented"
                    (txAttempt entry === 0)
                ]
              Nothing -> counterexample "entry vanished" False
          | k <- IntSet.toList expectedOmitted
          ]
      , combinedStateInvariantStep policy
          (Map.singleton peerAddr (peerState3, peerInFlight3))
          sharedState2 sharedState3
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

    accepted = [ (lookupKeyOrFail txid sharedState1, txid)
               | (txid, _, True)  <- normalised ]
    rejected = [ lookupKeyOrFail txid sharedState1
               | (txid, _, False) <- normalised ]

    acceptedKeys = IntSet.fromList (fmap (unTxKey . fst) accepted)
    rejectedKeys = IntSet.fromList (fmap unTxKey rejected)

    (peerState', peerInFlight', sharedState') =
      handleSubmittedTxs now policy peerAddr accepted rejected
                         peerState0 peerInFlight0 sharedState1

    -- Mirror the production pipeline in 'submitBufferedTxs': after
    -- 'handleSubmittedTxs', the V2 harness runs 'applyPeerEvents' on
    -- the resulting peer state.  Running 'peerTxLocalStateInvariant'
    -- on the output guards both its fast path
    -- (rejectedCount == 0 && score == 0) and its slow path against
    -- future strictness regressions via the invariant's embedded
    -- 'checkNoThunks'.
    (_, peerStateAfterEvents) =
      applyPeerEvents policy now (length accepted) (length rejected) peerState'

    retainUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)

    checkAccepted k =
      counterexample ("accepted " ++ show k) $ conjoin
        [ counterexample "still in sharedTxTable"
            (IntMap.notMember k (sharedTxTable sharedState'))
        , counterexample "missing or wrong retainUntil"
            (retainedLookup k (sharedRetainedTxs sharedState') === Just retainUntil)
        ]
    checkRejected k =
      counterexample ("rejected " ++ show k) $
        case IntMap.lookup k (sharedTxTable sharedState') of
          Just entry -> conjoin
            [ case txLease entry of
                TxClaimable _ -> property True
                other         -> counterexample (show other) False
            , counterexample "txInSubmission still set"
                (not (txInSubmission entry))
            ]
          Nothing -> counterexample "entry vanished" False
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
      , combinedStateInvariantStep policy
          (Map.singleton peerAddr (peerState', peerInFlight'))
          sharedState1 sharedState'
      , peerTxLocalStateInvariant policy peerStateAfterEvents
      ]
  where
    peerAddr = 1 :: PeerAddr

--
-- nextPeerAction
--

-- | An idle peer whose shared state has no work returns a
-- 'PeerDoNothing' parked on the wake counter, whatever its value.
prop_nextPeerAction_parksOnWakeCounter
  :: ArbTxDecisionPolicy
  -> Word64
  -> Property
prop_nextPeerAction_parksOnWakeCounter (ArbTxDecisionPolicy policy) gen =
  let
    -- A zero unack window forces 'pickRequestTxIdsAction' to return
    -- 'Nothing', so the scheduler falls through to 'PeerDoNothing'.  The
    -- config is deliberately out of the range 'saneTxDecisionPolicy' accepts:
    -- this tests 'pickRequestTxIdsAction' in isolation.
    config = TxSubmissionConfig { maxNumUnacknowledgedTxIds = 0
                                , maxNumTxIdsToRequest      = 0 }
    sharedState :: SharedTxState PeerAddr TxId
    sharedState = emptySharedTxState { sharedGeneration = gen }
    (action, _, _, _) =
      nextPeerAction now policy config peerAddr emptyPeerTxLocalState
                     emptyPeerTxInFlight sharedState
  in
    case action of
      PeerDoNothing _ counter -> counter === WakeCounter
      _                       -> counterexample (show action)
                                                False
  where
    peerAddr = 1 :: PeerAddr

-- | When advertised candidates exceed the per-peer size budget,
-- 'pickRequestTxs' picks a contiguous prefix of the unacked queue
-- whose total size fits the budget (with the soft single-tx allowance
-- documented in 'pickRequestTxsAction').
prop_nextPeerAction_picksTxsRespectingBudget
  :: ArbTxDecisionPolicy
  -> ArbTxSubmissionConfig
  -> NonEmptyList (TxId, Positive Int)
  -> Property
prop_nextPeerAction_picksTxsRespectingBudget (ArbTxDecisionPolicy policy0)
                                             (ArbTxSubmissionConfig config)
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
      nextPeerAction now policy config peerAddr peerState1 peerInFlight1 sharedState1

    keyOrder = [ unTxKey (lookupKeyOrFail txid sharedState1)
               | (txid, _) <- txidsAndSizes ]
    sizeOf k =
      maybe 0 getSizeInBytes
        (IntMap.lookup k (peerAvailableTxIds peerState1))

    budget = getSizeInBytes (txsSizeInflightPerPeer policy)
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
                 (pickedKeys `isPrefixOf` keyOrder)
             , counterexample
                 ("tail of picked exceeds budget: " ++ show pickedSize)
                 (tailSize <= budget)
             , counterexample "picked keys not added to pifLeased"
                 (IntSet.fromList pickedKeys
                    `IntSet.isSubsetOf` pifLeased peerInFlight')
             , counterexample "claimed entries should be TxLeased to peerAddr"
                 (conjoin
                   [ case IntMap.lookup k (sharedTxTable sharedState') of
                       Just entry -> case txLease entry of
                         TxLeased owner _ -> owner === peerAddr
                         other            -> counterexample (show other)
                                                            False
                       Nothing -> counterexample "missing entry"
                                                 False
                   | k <- pickedKeys
                   ])
             ]
      _ ->
        -- If the candidates were empty (e.g. all sizes happened to be
        -- the same and dedupe stripped them) the scheduler may pick
        -- something else; just check the action type isn't junk.
        counterexample (show action) True
  where
    peerAddr = 1 :: PeerAddr

-- | Advertised transaction sizes and a policy covering either a large
-- inflight window, or a first transaction at the hard batch limit with
-- a smaller soft inflight limit. Sizes are at least two so all policy limits
-- can remain positive.
--
data ArbBodyBatchScenario = ArbBodyBatchScenario {
      bbsTxSizes :: NonEmpty SizeInBytes,
      bbsPolicy  :: TxDecisionPolicy
    }
  deriving Show

instance Arbitrary ArbBodyBatchScenario where
    arbitrary = do
      otherCount <- choose (1, 199)
      let genSize = chooseEnum (2, max_TX_SIZE)
      sizes@(firstSize :| _) <-
        (:|) <$> genSize <*> vectorOf otherCount genSize
      -- Bound the count and sizes to keep their Word32 sum in range.
      (txsSizePerBatch, txsSizeInflightPerPeer) <- oneof
        [ (,) <$> frequency
                    [ (1, pure (maximum sizes))
                    , (1, pure (sum sizes - 1))
                    , (3, chooseEnum (maximum sizes, sum sizes - 1))
                    ]
              <*> chooseEnum (sum sizes, 2 * sum sizes)
        , (,) <$> pure firstSize <*> chooseEnum (1, firstSize - 1)
          -- the batch limit is below the first tx's size
        , (,) <$> chooseEnum (1, firstSize - 1)
              <*> chooseEnum (sum sizes, 2 * sum sizes)
        ]
      pure ArbBodyBatchScenario {
          bbsTxSizes = sizes,
          bbsPolicy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
            { txsSizePerBatch
            , txsSizeInflightPerPeer
            , maxOutstandingTxBatchesPerPeer = length sizes
            }
        }

    shrink scenario@ArbBodyBatchScenario { bbsTxSizes, bbsPolicy } =
        let TxDecisionPolicy { txsSizePerBatch, txsSizeInflightPerPeer } =
              bbsPolicy
        in filter validScenario $
           [ scenario { bbsTxSizes = sizes }
           | sizes <- shrinkMap (fmap SizeInBytes) (fmap getSizeInBytes)
                                bbsTxSizes
           , all (>= 2) sizes
           ]
        ++ [ scenario { bbsPolicy = bbsPolicy
                          { txsSizePerBatch = limit } }
           | limit <- shrinkIntegral txsSizePerBatch
           ]
        ++ [ scenario { bbsPolicy = bbsPolicy
                          { txsSizeInflightPerPeer = limit } }
           | limit <- shrinkIntegral txsSizeInflightPerPeer
           ]
      where
        validScenario ArbBodyBatchScenario { bbsTxSizes = sizes@(firstSize :| _)
                                          , bbsPolicy = policy
                                          } =
          let TxDecisionPolicy { txsSizePerBatch, txsSizeInflightPerPeer } =
                policy
          in txsSizePerBatch > 0 && txsSizeInflightPerPeer > 0
          && (   (maximum sizes <= txsSizePerBatch
               && txsSizePerBatch < sum sizes
               && sum sizes <= txsSizeInflightPerPeer)
               || (   txsSizePerBatch == firstSize
                   && txsSizeInflightPerPeer < firstSize)
               || (   txsSizePerBatch < firstSize
                   && sum sizes <= txsSizeInflightPerPeer))


-- | A large inflight window may span several batches. Each batch respects
-- its hard limit, while all advertised transactions can still be requested
-- in order. The first transaction is requested even when it exceeds the soft
-- inflight limit.  A transaction above the hard limit is requested in a batch
-- on its own.
--
prop_nextPeerAction_capsBodyBatches :: ArbBodyBatchScenario -> Property
prop_nextPeerAction_capsBodyBatches ArbBodyBatchScenario {
    bbsTxSizes = txSizes@(firstSize :| _),
    bbsPolicy  = policy@TxDecisionPolicy { txsSizePerBatch, txsSizeInflightPerPeer }
  } =
    checkCoverage $
    let sizes = toList txSizes
        txCount = length sizes
        config = defaultTxSubmissionConfigV2
          { maxNumUnacknowledgedTxIds = fromIntegral txCount
          , maxNumTxIdsToRequest = fromIntegral txCount
          }
        txidsAndSizes ::  [(TxId, SizeInBytes)]
        txidsAndSizes = zip [1 ..] sizes

        initialState@(peerState, _, sharedState) =
          handleReceivedTxIds (const False) now policy
            (fromIntegral txCount) txidsAndSizes
            (emptyPeerTxLocalState
              { peerRequestedTxIds = fromIntegral txCount })
            emptyPeerTxInFlight emptySharedTxState

        advertisedKeys :: [TxKey]
        advertisedKeys =
          [ lookupKeyOrFail txid sharedState | (txid, _) <- txidsAndSizes ]

        expectedKeys :: [TxKey]
        expectedKeys
          | firstSize > txsSizeInflightPerPeer = take 1 advertisedKeys
          | otherwise                          = advertisedKeys

        sizeOf :: TxKey -> SizeInBytes
        sizeOf key = peerAvailableTxIds peerState IntMap.! unTxKey key

        step
          :: (PeerTxLocalState tx, PeerTxInFlight, SharedTxState PeerAddr txid)
          -> (  Time
             -> TxDecisionPolicy
             -> TxSubmissionConfig
             -> PeerAddr
             -> PeerTxLocalState tx
             -> PeerTxInFlight
             -> SharedTxState PeerAddr txid
             -> ( PeerAction
                , PeerTxLocalState tx
                , PeerTxInFlight
                , SharedTxState PeerAddr txid
                )
             )
          -> ( (PeerTxLocalState tx, PeerTxInFlight, SharedTxState PeerAddr txid)
             , PeerAction
             )
        step (ps, pif, ss) next =
          let (action, ps', pif', ss') =
                next now policy config peerAddr ps pif ss
          in ((ps', pif', ss'), action)

        -- At most one request per transaction is needed. Keep the run
        -- bounded so a failure to make progress produces a counterexample.
        ((finalPeerState, _, _), actions) =
          mapAccumL step initialState
            (nextPeerAction
              : replicate (txCount - 1) nextPeerActionPipelined)

        batches :: [[TxKey]]
        batches = [keys | PeerRequestTxs keys <- actions]

        batchSizes :: [SizeInBytes]
        batchSizes = map (sum . map sizeOf) batches

    in cover 20 (length batches > 1) "more than one batch"
     $ cover 5  (length batches > 2) "more than two batches"
     $ cover 20
         (firstSize == txsSizePerBatch && firstSize > txsSizeInflightPerPeer)
         "first transaction at the hard limit"
     $ cover 20 (firstSize > txsSizePerBatch)
         "first transaction above the hard limit"
     $ cover 50 (length (nub sizes) > 1) "mixed transaction sizes"
     $ conjoin
         [ counterexample "batch exceeds the hard size limit"
             (and [ batchSize <= txsSizePerBatch || length batch == 1
                  | (batch, batchSize) <- zip batches batchSizes
                  ])
         , counterexample "empty body request"
             (all (not . null) batches)
         , counterexample "batch left room for the next transaction"
             (and [ batchSize + sizeOf nextKey > txsSizePerBatch
                  | (batchSize, nextKey : _) <-
                      zip batchSizes (drop 1 batches)
                  ])
         , counterexample "requests must match the policy's allowed prefix"
             (concat batches === expectedKeys)
         , counterexample "incorrect total inflight size"
             (peerRequestedTxsSize finalPeerState
                === sum (map sizeOf expectedKeys))
         ]
  where
    peerAddr = 1 :: PeerAddr

-- | When the peer has buffered a downloaded body of which it owns the
-- lease, 'nextPeerAction' submits before requesting more bodies or
-- acking.
prop_nextPeerAction_ownerSubmitsBuffered
  :: ArbTxDecisionPolicy
  -> ArbTxSubmissionConfig
  -> Positive Int
  -> Property
prop_nextPeerAction_ownerSubmitsBuffered (ArbTxDecisionPolicy policy)
                                         (ArbTxSubmissionConfig config)
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
      nextPeerAction now policy config peerAddr peerState2 peerInFlight2 sharedState2
  in
    case action of
      PeerSubmitTxs ks -> ks === [txKey]
      _                -> counterexample (show action) False
  where
    peerAddr = 1 :: PeerAddr

-- | A sender may legitimately re-advertise an unacked txid (e.g. fork
-- switch on the sender re-adds the tx to its mempool under a new idx;
-- the outbound side tracks by idx, not by hash).  When the same key
-- appears more than once in 'peerUnacknowledgedTxIds',
-- 'nextPeerAction' must still claim and request the body only once
-- per scheduler invocation, with 'txAttempt' incremented by exactly
-- one and 'peerRequestedTxsSize' charged for a single tx body.
unit_nextPeerAction_dedupsBodyRequestOnDuplicateTxIds :: Assertion
unit_nextPeerAction_dedupsBodyRequestOnDuplicateTxIds = do
    let n  = 3
        sz = SizeInBytes 512
        txid :: TxId
        txid = 1

        -- Widen the unack window so N copies fit; pin the size budget
        -- to two tx bodies so the test catches the inflated-size bug
        -- (a per-occurrence charge would block the second occurrence).
        policy = (unsafeMkTxDecisionPolicy config)
          { txsSizeInflightPerPeer         = SizeInBytes (getSizeInBytes sz * 2)
          , maxOutstandingTxBatchesPerPeer = 4
          }
        config = defaultTxSubmissionConfigV2
          { maxNumUnacknowledgedTxIds      = NumTxIdsToAck (fromIntegral (n + 1))
          , maxNumTxIdsToRequest           = NumTxIdsToReq (fromIntegral (n + 1))
          }

        duplicated       = replicate n (txid, sz)
        requestedToReply = fromIntegral n

        -- Simulate the wire delivering N copies of the same txid in one
        -- 'MsgReplyTxIds' batch.
        (peerState1, peerInFlight1, sharedState1) =
          handleReceivedTxIds (const False) now policy
                              requestedToReply duplicated
                              (emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply })
                              emptyPeerTxInFlight emptySharedTxState

        txKey  = lookupKeyOrFail txid sharedState1
        keyInt = unTxKey txKey

        (action, peerState', peerInFlight', sharedState') =
          nextPeerAction now policy config peerAddr peerState1 peerInFlight1 sharedState1

    -- The queue must keep all N duplicates so the ack-by-count
    -- handshake stays in lockstep with the sender.
    StrictSeq.length (peerUnacknowledgedTxIds peerState1) @?= n

    case action of
      PeerRequestTxs picked -> do
        picked @?= [txKey]
        assertBool "pifLeased must contain the key"
          (IntSet.member keyInt (pifLeased peerInFlight'))
        case IntMap.lookup keyInt (sharedTxTable sharedState') of
          Just e  -> txAttempt e @?= 1
          Nothing -> assertFailure "missing entry after request"
        peerRequestedTxsSize peerState' @?= sz
        StrictSeq.length (peerRequestedTxBatches peerState') @?= 1
        peerRequestedTxs peerState' @?= IntSet.singleton keyInt
      _ ->
        assertFailure ("expected PeerRequestTxs, got: " ++ show action)
  where
    peerAddr = 1 :: PeerAddr

-- | When the same key appears more than once in
-- 'peerUnacknowledgedTxIds' and we have its body buffered,
-- 'nextPeerAction' must submit the key only once.  Otherwise the
-- mempool would receive duplicate copies, and a mempool that
-- deduplicates internally would have its duplicate-rejected entries
-- misclassified as accepted by 'classifySubmittedTxs' in V2.
unit_nextPeerAction_dedupsSubmitOnDuplicateTxIds :: Assertion
unit_nextPeerAction_dedupsSubmitOnDuplicateTxIds = do
    let n  = 3
        sz = SizeInBytes 512
        txid :: TxId
        txid = 1

        config = defaultTxSubmissionConfigV2
          { maxNumUnacknowledgedTxIds = NumTxIdsToAck (fromIntegral (n + 1))
          , maxNumTxIdsToRequest      = NumTxIdsToReq (fromIntegral (n + 1))
          }
        policy = unsafeMkTxDecisionPolicy config

        duplicated       = replicate n (txid, sz)
        requestedToReply = fromIntegral n

        (peerState1, peerInFlight1, sharedState1) =
          handleReceivedTxIds (const False) now policy
                              requestedToReply duplicated
                              (emptyPeerTxLocalState { peerRequestedTxIds = requestedToReply })
                              emptyPeerTxInFlight emptySharedTxState

        txKey      = lookupKeyOrFail txid sharedState1
        keyInt     = unTxKey txKey
        leaseUntil = addTime (interTxSpace policy) now

        -- Manually claim the lease and buffer the body, simulating a
        -- completed body fetch.  The buffered body covers every
        -- duplicate in the queue since they all share the same key.
        sharedState2 = sharedState1 {
            sharedTxTable =
              IntMap.adjust
                (\entry -> entry {
                     txLease   = TxLeased peerAddr leaseUntil,
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
          nextPeerAction now policy config peerAddr peerState2 peerInFlight2 sharedState2

    StrictSeq.length (peerUnacknowledgedTxIds peerState2) @?= n
    case action of
      PeerSubmitTxs ks -> ks @?= [txKey]
      _ -> assertFailure ("expected PeerSubmitTxs, got: " ++ show action)
  where
    peerAddr = 1 :: PeerAddr

-- | When another peer resolves a tx into the mempool while this peer
-- still has its body buffered, the scheduler's 'mkPeerActionContext'
-- drops the orphaned body from 'peerDownloadedTxs' (it can no longer
-- be submitted: there is no active 'sharedTxTable' entry to mark
-- 'txInSubmission' on).  The matching 'pifLeased' / 'pifAttempting'
-- entries must also be cleared, otherwise:
--
-- * 'txSelectable' wrongly vetoes a re-claim on re-advertisement via
--   'txPeerHasAttempt' (the same 'TxKey' is returned when the txid is
--   re-interned), so this peer becomes silently unproductive for that
--   txid; and
-- * on bracket exit, 'scrubFromPeerInFlight.decAttempt' decrements
--   'txAttempt' on a freshly re-interned entry that this peer never
--   claimed, corrupting another peer's accounting.
unit_nextPeerAction_clearsInFlightForPrunedBody :: Assertion
unit_nextPeerAction_clearsInFlightForPrunedBody = do
    let sz = SizeInBytes 512
        txid :: TxId
        txid = 1
        config = defaultTxSubmissionConfigV2
        policy = unsafeMkTxDecisionPolicy config

        -- Peer A receives the txid advertisement.
        (peerState1, peerInFlight1, sharedState1) =
          handleReceivedTxIds (const False) now policy 1 [(txid, sz)]
                              (emptyPeerTxLocalState { peerRequestedTxIds = 1 })
                              emptyPeerTxInFlight emptySharedTxState

        txKey      = lookupKeyOrFail txid sharedState1
        keyInt     = unTxKey txKey
        leaseUntil = addTime (interTxSpace policy) now

        -- Peer A has claimed the lease and buffered the downloaded body.
        sharedState2 = sharedState1 {
            sharedTxTable =
              IntMap.adjust
                (\entry -> entry {
                     txLease   = TxLeased peerAddr leaseUntil,
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

        -- Simulate a sibling advertiser submitting the same tx to the
        -- mempool.  The active entry moves to 'sharedRetainedTxs'; peer
        -- A's local body buffer is now orphaned.
        retainUntil = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)
        sharedState3 = sharedState2 {
            sharedTxTable     = IntMap.delete keyInt (sharedTxTable sharedState2),
            sharedRetainedTxs =
              retainedInsertMax keyInt retainUntil (sharedRetainedTxs sharedState2),
            sharedGeneration  = sharedGeneration sharedState2 + 1
          }

        (_action, peerState', peerInFlight', _sharedState') =
          nextPeerAction now policy config peerAddr peerState2 peerInFlight2 sharedState3

    -- The orphaned body is pruned from the local buffer.
    assertBool "peerDownloadedTxs must drop the orphaned body"
      (IntMap.notMember keyInt (peerDownloadedTxs peerState'))
    -- The matching in-flight tracking entries must be cleared in lockstep.
    assertBool "pifLeased must drop the orphaned key"
      (IntSet.notMember keyInt (pifLeased peerInFlight'))
    assertBool "pifAttempting must drop the orphaned key"
      (IntSet.notMember keyInt (pifAttempting peerInFlight'))
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
    sharedState' = sweepSharedState expired sharedState0
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
    sharedState' = sweepSharedState now sharedState0
  in
    retainedSize (sharedRetainedTxs sharedState') === n

-- | What happens between two of the counters snapshot's transactions: a
-- live peer counts once more, a live peer retires, or a new peer registers
-- with a fresh cell.  Ops on a peer in the wrong state do nothing.
data CounterOp = Bump Int | Retire Int | Register Int
  deriving Show

-- | How many peers are registered at the start, and the ops to run before
-- each of the snapshot's transactions, in order.
--
-- Every count is a base-3 digit of its own in the total: peer @p@ starts
-- at @3^p@ and the @i@-th 'Bump' of the script adds
-- @3^('initialDigits' + i)@, so the snapshot's total says how many times
-- it took each count.  The digits fit a 'Word64', which bounds the bumps.
data CounterScript = CounterScript Int [[CounterOp]]
  deriving Show

-- | Digits reserved for the peers registered at the start.
initialDigits :: Int
initialDigits = 6

-- | Digits in all: @3^40 - 1@ fits a 'Word64'.
counterDigits :: Int
counterDigits = 40

instance Arbitrary CounterScript where
  arbitrary = do
    peers <- choose (0, initialDigits)
    CounterScript peers . capBumps
      <$> listOf (listOf (oneof [ Bump <$> peer
                                , Retire <$> peer
                                , Register <$> choose (initialDigits, 9) ]))
    where
      peer = choose (0, 9)

      -- Only as many bumps as there are digits for them.
      capBumps :: [[CounterOp]] -> [[CounterOp]]
      capBumps = snd . mapAccumL batch 0

      batch :: Int -> [CounterOp] -> (Int, [CounterOp])
      batch n [] = (n, [])
      batch n (Bump p : ops)
        | n >= counterDigits - initialDigits = batch n ops
        | otherwise                          = (Bump p :) <$> batch (n + 1) ops
      batch n (op : ops) = (op :) <$> batch n ops
  shrink (CounterScript peers bs) =
       [ CounterScript peers' bs | peers' <- shrink peers ]
    ++ [ CounterScript peers bs' | bs' <- shrinkList (shrinkList (const [])) bs ]

-- | The variables a counters script runs against: the snapshot's own
-- retired accumulator and registry, every cell a peer ever had, the bumps
-- seen so far, the transactions each applied bump ran after, by its digit,
-- the transactions run so far, and the ops still to run.
data CounterEnv s = CounterEnv {
    ceRetired  :: TxSubmissionCountersVar (IOSim s),
    ceRegistry :: PeerTxRegistry (IOSim s) Int,
    ceAllCells :: StrictTVar (IOSim s) (Map.Map Int (TxSubmissionCountersVar (IOSim s))),
    ceBumps    :: StrictTVar (IOSim s) Int,
    ceApplied  :: StrictTVar (IOSim s) (Map.Map Int Int),
    ceTxs      :: StrictTVar (IOSim s) Int,
    cePending  :: StrictTVar (IOSim s) [[CounterOp]]
  }

-- | However peers count, retire and register between its transactions, a
-- snapshot takes every count there was before it started exactly once,
-- every count made during it at most once, and nothing else: no peer is
-- missed or counted twice.
prop_snapshotCounters_exactlyOnce :: CounterScript -> Property
prop_snapshotCounters_exactlyOnce (CounterScript peers batches) = runSimOrThrow $ do
    ceRetired  <- newTVarIO mempty
    cells      <- mapM (newTVarIO . counted . (3 ^)) [0 .. peers - 1]
    ceRegistry <- noRootsRegistry (mkStdGen 7)
    mapM_ (\(p, c) -> atomically (registerPeerCounters ceRegistry p c)) (zip [0 ..] cells)
    ceAllCells <- newTVarIO (Map.fromList (zip [0 ..] cells))
    ceBumps    <- newTVarIO 0
    ceApplied  <- newTVarIO Map.empty
    ceTxs      <- newTVarIO 0
    cePending  <- newTVarIO batches
    let env = CounterEnv { ceRetired, ceRegistry, ceAllCells, ceBumps,
                           ceApplied, ceTxs, cePending }
    s       <- txsReceived <$> snapshotCountersWith (runTx env) ceRetired ceRegistry
    applied <- readTVarIO ceApplied
    pure $ counterexample (show (s, applied)) $ conjoin
      [ counterexample ("digit " ++ show i ++ ", " ++ what
                        ++ ", taken " ++ show times)
          (lo <= times && times <= hi)
      | i <- [0 .. counterDigits - 1]
      , let times           = digit i s
            (lo, hi, what)  = expected applied i
      ]
  where
    digit :: Int -> Word64 -> Int
    digit i s = fromIntegral ((s `div` 3 ^ i) `mod` 3)

    -- How many times the snapshot must have taken the count of a digit:
    -- the initial counts and those made before its first transaction
    -- once, those made during it at most once, the rest not at all.
    expected :: Map.Map Int Int -> Int -> (Int, Int, String)
    expected applied i
      | i < peers                      = (1, 1, "an initial count")
      | Map.lookup i applied == Just 0 = (1, 1, "a count before the snapshot")
      | Map.member i applied           = (0, 1, "a count during the snapshot")
      | otherwise                      = (0, 0, "no count")

    -- A live peer's registration and cell.
    registrationOf :: PeerTxRegistry (IOSim s) Int -> Int
                   -> STM (IOSim s) (Maybe (PeerRegistration Int, TxSubmissionCountersVar (IOSim s)))
    registrationOf PeerTxRegistry { registeredPeers } p =
      listToMaybe . filter ((== p) . fst . fst) . Map.toList <$> readTVar registeredPeers

    -- Run the ops due before this transaction, then the transaction.
    runTx :: CounterEnv s -> STM (IOSim s) a -> IOSim s a
    runTx env@CounterEnv { cePending, ceTxs } tx = do
      next <- atomically $ do
        bs <- readTVar cePending
        case bs of
          b : rest -> writeTVar cePending rest >> pure b
          []       -> pure []
      mapM_ (apply env) next
      atomically $ modifyTVar ceTxs (+ 1)
      atomically tx

    apply :: CounterEnv s -> CounterOp -> IOSim s ()
    apply CounterEnv { ceRegistry, ceBumps, ceApplied, ceTxs } (Bump p) =
      atomically $ do
        i <- readTVar ceBumps
        writeTVar ceBumps (i + 1)
        live <- registrationOf ceRegistry p
        case live of
          Just (_, c) -> do
            modifyTVar c (<> counted (3 ^ (initialDigits + i)))
            txs <- readTVar ceTxs
            modifyTVar ceApplied (Map.insert (initialDigits + i) txs)
          Nothing     -> pure ()
    apply CounterEnv { ceRetired, ceRegistry } (Retire p) = atomically $ do
      live <- registrationOf ceRegistry p
      case live of
        Just (registration, c) -> retirePeerCounters ceRetired ceRegistry registration c
        Nothing                -> pure ()
    apply CounterEnv { ceRegistry, ceAllCells } (Register p) = do
      known <- Map.member p <$> readTVarIO ceAllCells
      if known then pure () else do
        c <- newTVarIO mempty
        atomically $ do
          modifyTVar ceAllCells (Map.insert p c)
          _ <- registerPeerCounters ceRegistry p c
          pure ()

-- | Two peers registered under one address, the first retiring: the second
-- stays registered, and a snapshot counts both, the first through the
-- accumulator.
prop_registry_releaseOwn :: Word64 -> Word64 -> Property
prop_registry_releaseOwn a b = runSimOrThrow $ do
    retired  <- newTVarIO mempty
    registry <- noRootsRegistry (mkStdGen 7)
    cellA    <- newTVarIO (counted a)
    cellB    <- newTVarIO (counted b)
    keyA     <- atomically (registerPeerCounters registry (1 :: Int) cellA)
    keyB     <- atomically (registerPeerCounters registry 1 cellB)
    atomically $ retirePeerCounters retired registry keyA cellA
    still    <- Map.keys <$> readTVarIO (registeredPeers registry)
    s        <- txsReceived <$> snapshotCountersWith atomically retired registry
    pure $ counterexample (show (keyA, keyB, still, s)) $
           counterexample "registered after the first release" (still === [keyB])
      .&&. counterexample "counted" (s === a + b)

-- | A counters accumulator with that many txs received.
counted :: Word64 -> TxSubmissionCountersAcc
counted k = mempty { txsReceived = k }

-- | Expiry alone drops the expired keys with their lookup entries and
-- keeps the others; the tx table is left alone, and the backlog flag is
-- right.
prop_expireUpTo_lookupEntries :: SweepState -> Property
prop_expireUpTo_lookupEntries ss@SweepState { ssLimit } =
  let st   = sweepStateOf ss
      (st', more) = expireSharedStateUpTo ssLimit now st
      expired = retainedKeysSet (sharedRetainedTxs st)
                  `IntSet.difference` retainedKeysSet (sharedRetainedTxs st')
  in counterexample (show st') $
       counterexample "tx table changed" (sharedTxTable st' === sharedTxTable st)
       .&&. counterexample "lookup entries"
              (IntMap.keysSet (sharedKeyToTxId st')
                 === IntMap.keysSet (sharedKeyToTxId st) `IntSet.difference` expired)
       .&&. counterexample "backlog flag"
              (more === any (\(_, t) -> t <= retainNow now) (retainedToList (sharedRetainedTxs st')))
       .&&. sharedTxStateInvariant st'

-- | The counters thread over a shared state, beside an experiment with the
-- state's variable; the shared-state traces go to the tracer.
withCountersThread :: TxDecisionPolicy
                   -> Tracer (IOSim s) (TraceTxLogic PeerAddr TxId ())
                   -> SharedTxState PeerAddr TxId
                   -> (SharedTxStateVar (IOSim s) PeerAddr TxId -> IOSim s a)
                   -> IOSim s a
withCountersThread policy tracer st0 experiment = do
    sharedStateVar <- newSharedTxStateVar st0
    registry       <- noRootsRegistry (mkStdGen 7)
    retired        <- newTxSubmissionCountersVar mempty
    withAsync (txCountersThreadV2 policy defaultTxSubmissionConfigV2 nullTracer tracer
                                  retired sharedStateVar registry) $ \_ ->
      experiment sharedStateVar

-- | A backlog of @n@ expired retained entries, keys @0 .. n-1@ due a
-- nanosecond apart in key order, with their lookup entries.
retainedBacklog :: Int -> SharedTxState PeerAddr TxId
retainedBacklog n =
  emptySharedTxState {
      sharedRetainedTxs = retainedFromList [ (k, due k) | k <- keys ],
      sharedTxIdToKey   = Map.fromList [ (k, TxKey k) | k <- keys ],
      sharedKeyToTxId   = IntMap.fromList [ (k, k) | k <- keys ],
      sharedNextTxKey   = n
    }
  where
    keys = [0 .. n - 1]

    due :: Int -> RetainTime
    due k = retainDeadline (Time (fromIntegral k / 1000000000))

-- | The state with @n@ more expired retained entries, fresh keys, due at
-- once: a backlog arriving while the counters thread runs.
addBacklog :: Int -> SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId
addBacklog n st =
  st { sharedRetainedTxs = List.foldl' (\r k -> retainedInsertMax k due r) (sharedRetainedTxs st) keys,
       sharedTxIdToKey   = Map.union (sharedTxIdToKey st) (Map.fromList [ (k, TxKey k) | k <- keys ]),
       sharedKeyToTxId   = IntMap.union (sharedKeyToTxId st) (IntMap.fromList [ (k, k) | k <- keys ]),
       sharedNextTxKey   = sharedNextTxKey st + n,
       sharedRevision    = sharedRevision st + 1 }
  where
    keys = [ sharedNextTxKey st .. sharedNextTxKey st + n - 1 ]

    due :: RetainTime
    due = retainDeadline (Time 0)

-- | The counters thread's first sweep, at 'sweepInterval', is the unbounded
-- sweep at that time, revision aside: what the state has due by then
-- expires, its orphans go, as do the lookup entries no peer holds, and the
-- rest stays.
prop_thread_rotation :: SweepState -> Property
prop_thread_rotation ss@SweepState { ssRetained } =
  classify (any ((<= 0) . snd) ssRetained) "expires something" $
  runSimOrThrow $
    withCountersThread (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2) nullTracer st0 $
      \sharedStateVar -> do
    threadDelay (tick + drainPause / 2)
    st <- readTVarIO sharedStateVar
    pure $ counterexample (show st) $
           counterexample "state after the first sweep"
             (st { sharedRevision = 0 } === (sweepSharedState (Time tick) st0) { sharedRevision = 0 })
      .&&. sharedTxStateInvariant st
  where
    tick :: DiffTime
    tick = sweepInterval (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)

    st0 :: SharedTxState PeerAddr TxId
    st0 = sweepStateAt (Time tick) ss

-- | Backlogs arriving while the counters thread runs: each a number of
-- expired entries, taking the thread most of an interval to drain, and the
-- multiple of the emission cadence it arrives up to 300 ms before, so that
-- a drain is under way when an emission is due.
newtype Backlogs = Backlogs [(Int, Int, Int)]
  deriving Show

instance Arbitrary Backlogs where
  arbitrary = do
    n <- choose (0, 5)
    Backlogs <$> vectorOf n ((,,) <$> choose (30 * sweepBatch, 50 * sweepBatch)
                                  <*> choose (1, 4)
                                  <*> choose (0, 300))
  shrink (Backlogs bs) = Backlogs <$> shrinkList (const []) bs

-- | A retention lifetime, from 0.4 s to 4 s: sweep intervals from the
-- shortest to the longest, multiples of 'drainPause' and not.
newtype Lifetime = Lifetime DiffTime
  deriving Show

instance Arbitrary Lifetime where
  arbitrary = Lifetime . (/ 1000) . fromIntegral <$> choose (400, 4000 :: Int)
  shrink (Lifetime t) =
    [ Lifetime (fromIntegral ms / 1000)
    | ms <- shrink (round (t * 1000) :: Int), ms >= 400 ]

-- | However the drains delay it, every shared-state trace the counters
-- thread emits falls within two sweep intervals after a multiple of its
-- cadence: an emission delayed by a drain does not delay the next, and a
-- drain never runs past the interval.
prop_thread_emissionCadence :: Lifetime -> Backlogs -> Property
prop_thread_emissionCadence (Lifetime lifetime) (Backlogs backlogs) = runSimOrThrow $ do
    emissions <- newTVarIO []
    let tracer = Tracer $ emit $ \_ -> do
          t <- getMonotonicTime
          atomically (modifyTVar emissions (t :))
        policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                   { bufferedTxsMinLifetime = lifetime }
    withCountersThread policy tracer emptySharedTxState $ \sharedStateVar -> do
      forM_ (sortOn snd arrivals) $ \(n, at) -> do
        t <- getMonotonicTime
        threadDelay (at `diffTime` t)
        atomically $ modifyTVar sharedStateVar (addBacklog n)
      threadDelay 35
      ts <- readTVarIO emissions
      -- every backlog changes the state, so the cadence point after each
      -- arrival emits, by the end of the first tick at or past it: the
      -- tick whose drain crosses the point, or that first tick, emits
      -- after its drain, which ends two pauses before the next tick, never
      -- skips to it
      let interval = sweepInterval policy
          firstTickFrom d = interval * fromIntegral (ceiling (d / interval) :: Int)
          dues = nub [ 7 * fromIntegral (ceiling (at / 7) :: Int) | (_, Time at) <- arrivals ]
      pure $ counterexample (show ts) $
             conjoin [ counterexample ("no emission for the cadence point " ++ show due)
                         (any (\(Time t) -> t >= due && t < firstTickFrom due + interval) ts)
                     | due <- dues ]
        .&&. conjoin [ counterexample (show t) (offset t <= 2 * interval)
                     | t <- ts ]
  where
    arrivals :: [(Int, Time)]
    arrivals = [ (n, Time (7 * fromIntegral k - fromIntegral lead / 1000))
               | (n, k, lead) <- backlogs ]

    -- How long after the last multiple of the cadence.
    offset :: Time -> DiffTime
    offset (Time t) = t - 7 * fromIntegral (floor (t / 7) :: Int)

-- | The state with @n@ more table entries, fresh keys, unleased, unattempted
-- and unheld: orphans for the sweeps' walk, a limit of them per sweep.
withOrphans :: Int -> SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId
withOrphans n st =
  st { sharedTxTable   = IntMap.union (sharedTxTable st) (IntMap.fromList [ (k, entry) | k <- keys ]),
       sharedTxIdToKey = Map.union (sharedTxIdToKey st) (Map.fromList [ (k, TxKey k) | k <- keys ]),
       sharedKeyToTxId = IntMap.union (sharedKeyToTxId st) (IntMap.fromList [ (k, k) | k <- keys ]),
       sharedNextTxKey = sharedNextTxKey st + n }
  where
    keys = [ sharedNextTxKey st .. sharedNextTxKey st + n - 1 ]
    -- No advertiser: abandoned from the first sweep on.
    entry = TxEntry {
        txLease = TxClaimable (Time 0),
        txAttempt = 0,
        txAdvertisers = 0,
        txInSubmission = False,
        currentMaxInflightMultiplicity = 1
      }

-- | While a backlog lasts the counters thread's sweeps come at most an
-- interval apart, and its writes, batches and sweeps, at least a pause
-- apart: a drain stops while a batch and the pause before the next sweep
-- still fit in the interval after its sweep, and the next sweep comes at
-- the interval's end.  A sweep shows as the table shrinking, its walk
-- tearing down a limit of orphans, which no batch does.
prop_thread_drainCap :: Lifetime -> Property
prop_thread_drainCap (Lifetime lifetime) = runSimOrThrow $
    withCountersThread policy nullTracer st0 $ \sharedStateVar -> do
      (writes, sweeps) <- watchWrites st0 sharedStateVar (3 * interval + drainPause / 2)
      final <- readTVarIO sharedStateVar
      let gaps = zipWith diffTime (drop 1 sweeps) sweeps
          spacing = zipWith diffTime (drop 1 writes) writes
          expired = retainedSize (sharedRetainedTxs st0) - retainedSize (sharedRetainedTxs final)
          -- what the drains alone expire in the three full intervals
          -- watched, at a batch a pause apart and two pauses left for the
          -- sweep: the sweeps' own expiry, half a batch each, is far below
          drainedAtLeast = 2 * (ceiling (interval / drainPause) - 2) * sweepBatch
      pure $ counterexample (show (sweeps, gaps, spacing, expired)) $
             counterexample "fewer than three sweeps seen" (length sweeps >= 3)
        .&&. counterexample "sweeps more than an interval apart" (all (<= interval) gaps)
        .&&. counterexample "writes less than a pause apart" (all (>= drainPause) spacing)
        .&&. counterexample ("the drains expired " ++ show expired ++ ", fewer than " ++ show drainedAtLeast)
               (expired >= drainedAtLeast)
  where
    policy :: TxDecisionPolicy
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
               { bufferedTxsMinLifetime = lifetime }

    interval :: DiffTime
    interval = sweepInterval policy

    -- Expired entries to outlast the three intervals watched, and orphans
    -- for the sweeps meanwhile.
    st0 :: SharedTxState PeerAddr TxId
    st0 = withOrphans (4 * sweepBatch)
            (retainedBacklog (4 * (ceiling (interval / drainPause) + 1) * sweepBatch))

-- | Watch a running counters thread through the state's writes for the
-- given time: the time of every write, and of every sweep, which shows as
-- the table shrinking, its walk tearing down a limit of orphans, which no
-- batch does.
watchWrites :: SharedTxState PeerAddr TxId
            -> SharedTxStateVar (IOSim s) PeerAddr TxId
            -> DiffTime
            -> IOSim s ([Time], [Time])
watchWrites st0 sharedStateVar for = do
    observations <- newTVarIO [(Time 0, IntMap.size (sharedTxTable st0))]
    let watch rev = do
          (rev', size) <- atomically $ do
            st <- readTVar sharedStateVar
            check (sharedRevision st /= rev)
            pure (sharedRevision st, IntMap.size (sharedTxTable st))
          t <- getMonotonicTime
          atomically (modifyTVar observations ((t, size) :))
          watch rev'
    withAsync (watch (sharedRevision st0)) $ \_ -> threadDelay for
    obs <- reverse <$> readTVarIO observations
    pure ( map fst (drop 1 obs)
         , [ t | ((t, size), (_, before)) <- zip (drop 1 obs) obs, size < before ]
         )

-- | What each of an emission's traces takes, as a share of the sweep
-- interval: up to a third of it, which the interval has room for with the
-- three traces an emission carries at most (counters, census, shared
-- state).
newtype EmissionShare = EmissionShare Int
  deriving Show

instance Arbitrary EmissionShare where
  arbitrary = EmissionShare <$> choose (1, 33)
  shrink (EmissionShare p) = [ EmissionShare p' | p' <- shrink p, p' >= 1 ]

-- | An emission, however long its traces take, does not delay the sweep
-- after it: the counters thread's sweeps stay at most an interval apart
-- across the first emission.  Orphans make every sweep show.
prop_thread_emissionCost :: Lifetime -> EmissionShare -> Property
prop_thread_emissionCost (Lifetime lifetime) (EmissionShare share) = runSimOrThrow $ do
    emissions <- newTVarIO []
    let tracer = Tracer $ emit $ \_ -> do
          t <- getMonotonicTime
          atomically (modifyTVar emissions (t :))
          threadDelay cost
    withCountersThread policy tracer st0 $ \sharedStateVar -> do
      (_, sweeps) <- watchWrites st0 sharedStateVar (7 + 2 * interval + drainPause / 2)
      es <- readTVarIO emissions
      let gaps = zipWith diffTime (drop 1 sweeps) sweeps
      pure $ counterexample (show (es, sweeps, gaps)) $
             counterexample "no emission in the window" (not (null es))
        .&&. counterexample "sweeps more than an interval apart" (all (<= interval) gaps)
  where
    policy :: TxDecisionPolicy
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
               { bufferedTxsMinLifetime = lifetime }

    interval :: DiffTime
    interval = sweepInterval policy

    cost :: DiffTime
    cost = interval * fromIntegral share / 100

    -- Orphans for every sweep's walk through the window, and some over.
    st0 :: SharedTxState PeerAddr TxId
    st0 = withOrphans ((ceiling ((7 + 2 * interval) / interval) + 2) * sweepBatch) emptySharedTxState

-- | The counts are traced under every field's name, each with its own
-- value: a field added to the record is traced or fails, never shifted
-- onto another's name.
prop_countFields_everyField :: SweepState -> Property
prop_countFields_everyField ss =
  let counts = sharedTxStateCounts (sweepStateOf ss)
  in      counterexample "the traced list against the record"
            (sharedTxStateCountFields counts
               === [ ("activeTxCount",     activeTxCount     counts)
                   , ("retainedTxCount",   retainedTxCount   counts)
                   , ("internedTxCount",   internedTxCount   counts)
                   , ("leasedTxCount",     leasedTxCount     counts)
                   , ("claimableTxCount",  claimableTxCount  counts)
                   , ("totalAttemptCount", totalAttemptCount counts)
                   , ("submittingTxCount", submittingTxCount counts)
                   , ("advertiserCount",   advertiserCount   counts)
                   ])
     .&&. counterexample "the docs against the traced names"
            (map fst sharedTxStateCountDocs === map fst (sharedTxStateCountFields counts))

-- | A deadline and a current time, in picoseconds: the deadline anywhere
-- from before the clock's origin (a non-positive lifetime) through the
-- first nanoseconds to weeks of uptime, or about the end of the 'Word64'
-- nanosecond range; the current time within two nanoseconds of it either
-- side, never negative.
data DeadlineCase = DeadlineCase Integer Integer
  deriving Show

instance Arbitrary DeadlineCase where
  arbitrary = DeadlineCase <$> oneof [ choose (-10000, 10000)
                                     , choose (0, 10 ^ (18 :: Int))
                                     , choose (rangeEnd - 10000, rangeEnd + 10000) ]
                           <*> choose (-2000, 2000)
    where rangeEnd = 1000 * 2 ^ (64 :: Int)
  shrink (DeadlineCase r g) =
       [ DeadlineCase r' g | r' <- shrink r ]
    ++ [ DeadlineCase r g' | g' <- shrink g ]

-- | Deadlines round up and the current time rounds down, so an entry
-- expires only once its deadline has passed, and at most one nanosecond
-- late; past the end of the range, where the clock itself saturates, a
-- deadline is due.
prop_retainDeadline_rounding :: DeadlineCase -> Property
prop_retainDeadline_rounding (DeadlineCase r gap) =
  let deadline = Time (picosecondsToDiffTime r)
      current  = Time (picosecondsToDiffTime (max 0 (r + gap)))
      expired  = retainDeadline deadline <= retainNow current
  in counterexample (show (deadline, current, expired)) $
       counterexample "expired early"
         (not expired || deadline <= current || retainNow current == maxBound)
       .&&. counterexample "expired late" (current < addTime 1e-9 deadline || expired)

-- | Retained entries alone: distinct keys, each due a few seconds either
-- side of 'now', so a sweep at 'now' finds some expired and some not, ties
-- included.
newtype RetainedOnly = RetainedOnly [(Int, RetainTime)]
  deriving Show

instance Arbitrary RetainedOnly where
  arbitrary = do
    keys <- nub <$> listOf (choose (0, 200))
    RetainedOnly <$> mapM (\k -> (,) k . offset <$> choose (-5, 5)) keys
    where offset :: Int -> RetainTime
          offset d = retainDeadline (addTime (fromIntegral d) now)
  shrink (RetainedOnly kts) = RetainedOnly <$> shrinkList (const []) kts

retainedOnlyState :: RetainedOnly -> SharedTxState PeerAddr TxId
retainedOnlyState (RetainedOnly kts) =
  emptySharedTxState { sharedRetainedTxs = retainedFromList kts }

-- | The state a bounded sweep returns.
swept :: (SharedTxState peeraddr txid, Bool, SweepCursor) -> SharedTxState peeraddr txid
swept (st, _, _) = st

-- | One bounded sweep removes at most its limit, and the ones it removes are
-- expired and due no later than any expired entry it leaves.
prop_sweepUpTo_bounded :: Int -> RetainedOnly -> Property
prop_sweepUpTo_bounded limit ro =
  let before  = retainedToList (sharedRetainedTxs (retainedOnlyState ro))
      after   = retainedToList (sharedRetainedTxs
                  (swept (sweepSharedStateUpTo limit now sweepStart (retainedOnlyState ro))))
      removed = [ t | (k, t) <- before, k `notElem` map fst after ]
      leftExpired = [ t | (_, t) <- after, t <= retainNow now ]
  in counterexample (show (removed, leftExpired)) $
       length removed <= expiryLimit
       .&&. all (<= retainNow now) removed
       .&&. (null leftExpired || null removed || maximum removed <= minimum leftExpired)
       .&&. (length removed == expiryLimit || null leftExpired)
  where
    -- the expiry's share of the limit: half of it, rounded up
    expiryLimit = max 0 (limit - limit `div` 2)

-- | The sweep reports expired entries remaining exactly when some remain,
-- whatever its limit: a sweep left no budget still asks for the drain.
prop_sweepUpTo_reportsBacklog :: Int -> RetainedOnly -> Property
prop_sweepUpTo_reportsBacklog limit ro =
  let (st', more, _) = sweepSharedStateUpTo limit now sweepStart (retainedOnlyState ro)
  in classify (limit <= 0) "no budget" $
     more === any (\(_, t) -> t <= retainNow now) (retainedToList (sharedRetainedTxs st'))

-- | Sweeping with a limit until nothing changes leaves the retained entries
-- one unbounded sweep leaves.
prop_sweepUpTo_converges :: Positive Int -> RetainedOnly -> Property
prop_sweepUpTo_converges (Positive limit) ro =
  let st0   = retainedOnlyState ro
      final = step st0
      step st = case sweepSharedStateUpTo limit now sweepStart st of
                  (st', True, _)  -> step st'
                  (st', False, _) -> st'
  in retainedToList (sharedRetainedTxs final)
     === retainedToList (sharedRetainedTxs (sweepSharedState now st0))

-- | A shared state as the sweep finds it: retained entries due a few
-- seconds either side of a time, and active entries of every kind with
-- their advertisers, some none.  Keys are distinct across the two
-- residences.
data SweepState = SweepState {
    ssRetained :: [(Int, Int)],             -- ^ key, seconds from the time to its deadline
    ssActive   :: [(Int, Int, ActiveKind)], -- ^ key, advertisers, kind
    ssLimit    :: Int
  }
  deriving Show

-- | What an active entry is doing: nothing, attempted by a peer, leased
-- to one, or being added to the mempool.  Only the first can be abandoned.
data ActiveKind = Claimable | Attempted | Leased | Submitting
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary ActiveKind where
  arbitrary = frequency [ (3, pure Claimable), (1, pure Attempted)
                        , (1, pure Leased), (1, pure Submitting) ]
  shrink Claimable = []
  shrink _         = [Claimable]

instance Arbitrary SweepState where
  arbitrary = do
    keys <- nub <$> listOf (choose (0, 60))
    (r, a) <- splitAtRandom keys
    SweepState <$> mapM (\k -> (,) k <$> choose (-5, 5)) r
               <*> mapM (\k -> (,,) k <$> frequency [ (1, pure 0), (2, choose (1, 3)) ] <*> arbitrary) a
               <*> choose (1, 8)
    where
      splitAtRandom xs = (`splitAt` xs) <$> choose (0, length xs)
  shrink SweepState { ssRetained, ssActive, ssLimit } =
       [ SweepState r ssActive ssLimit | r <- shrinkList (const []) ssRetained ]
    ++ [ SweepState ssRetained a ssLimit
       | a <- shrinkList (\(k, n, kind) -> [ (k, n', kind) | n' <- shrink n, n' >= 0 ]
                                           ++ [ (k, n, kind') | kind' <- shrink kind ]) ssActive ]

-- | The state of a 'SweepState' with its deadlines and leases relative to
-- the given time.
sweepStateAt :: Time -> SweepState -> SharedTxState PeerAddr TxId
sweepStateAt t SweepState { ssRetained, ssActive } =
  emptySharedTxState {
      sharedTxTable     = IntMap.fromList [ (k, entry n kind) | (k, n, kind) <- ssActive ],
      sharedRetainedTxs = retainedFromList [ (k, retainDeadline (addTime (fromIntegral d) t)) | (k, d) <- ssRetained ],
      sharedTxIdToKey   = Map.fromList [ (k, TxKey k) | k <- keys ],
      sharedKeyToTxId   = IntMap.fromList [ (k, k) | k <- keys ],
      sharedNextTxKey   = maximum (0 : map (+ 1) keys)
    }
  where
    keys = map fst ssRetained ++ [ k | (k, _, _) <- ssActive ]
    entry n kind = TxEntry {
        txLease = case kind of
                    Leased -> TxLeased 1 (addTime 10 t)
                    _      -> TxClaimable t,
        txAttempt = case kind of
                      Attempted -> 1
                      Leased    -> 1
                      _         -> 0,
        txInSubmission = kind == Submitting,
        txAdvertisers = n,
        currentMaxInflightMultiplicity = 1
      }

sweepStateOf :: SweepState -> SharedTxState PeerAddr TxId
sweepStateOf = sweepStateAt now

-- | After an unbounded sweep a lookup entry remains exactly for the keys
-- that still have a residence; and the state's invariants hold before and
-- after.
prop_sweep_lookupEntries :: SweepState -> Property
prop_sweep_lookupEntries ss =
  let st   = sweepStateOf ss
      st'  = sweepSharedState now st
      resident' = IntMap.keysSet (sharedTxTable st')
                    `IntSet.union` retainedKeysSet (sharedRetainedTxs st')
  in counterexample (show st') $
       counterexample "lookup entries after the sweep"
         (IntMap.keysSet (sharedKeyToTxId st') === resident')
       .&&. sharedTxStateInvariant st
       .&&. sharedTxStateInvariant st'

-- | After an unbounded sweep the tx table keeps exactly the entries some
-- peer advertises, leases, attempts or submits; the rest go with their
-- lookup entries.
prop_sweep_dropsAbandoned :: SweepState -> Property
prop_sweep_dropsAbandoned ss@SweepState { ssActive } =
  let st  = sweepStateOf ss
      st' = sweepSharedState now st
      kept = IntSet.fromList [ k | (k, n, kind) <- ssActive, kind /= Claimable || n > 0 ]
      dropped = IntMap.keysSet (sharedTxTable st) `IntSet.difference` kept
  in counterexample (show st') $
       classify (not (IntSet.null dropped)) "drops something" $
       counterexample "tx table after the sweep"
         (IntMap.keysSet (sharedTxTable st') === kept)
       .&&. counterexample "lookup entries of the dropped"
              (IntSet.null (IntMap.keysSet (sharedKeyToTxId st') `IntSet.intersection` dropped))

-- | A peer advertising some of the entries: one advertiser more each.
advertising :: IntSet.IntSet -> SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId
advertising keys st =
  st { sharedTxTable =
         IntSet.foldl' (\tbl k -> IntMap.adjust (\e -> e { txAdvertisers = txAdvertisers e + 1 }) k tbl)
                       (sharedTxTable st) keys }

-- | No sweep drops an entry a registered peer still advertises, whatever
-- else the entry is doing: the peer would acknowledge its txid without the
-- tx ever reaching the mempool.
prop_sweep_keepsAdvertised :: SweepState -> [Bool] -> Property
prop_sweep_keepsAdvertised ss@SweepState { ssActive } flags =
  let st  = sweepStateOf ss
      advertised = IntSet.fromList [ k | ((k, _, _), True) <- zip ssActive (flags ++ repeat False) ]
      st' = sweepSharedState now (advertising advertised st)
  in counterexample (show st') $
       classify (not (IntSet.null advertised)) "advertises something" $
       counterexample "an advertised entry was dropped"
         (advertised `IntSet.isSubsetOf` IntMap.keysSet (sharedTxTable st'))

-- | A peer's advertised set may hold a key whose retention expired before
-- the peer acknowledged it, the one exception to its keys being live.
-- The sweep leaves the peer's state alone and the combined invariant
-- holds; the peer's next action acknowledges every such key; and were
-- the peer to leave instead, the keys would cost no write.
prop_sweep_expiredAdvertised :: SweepState -> [Bool] -> Property
prop_sweep_expiredAdvertised ss@SweepState { ssRetained } flags =
  let st0   = sweepStateOf ss { ssActive = [] }
      mine  = [ k | ((k, _), True) <- zip ssRetained (flags ++ repeat False) ]
      later = addTime 6 now
      peerState = (emptyPeerTxLocalState :: PeerTxLocalState (Tx TxId)) {
          peerUnacknowledgedTxIds = StrictSeq.fromList (map TxKey mine),
          peerAvailableTxIds      = IntMap.fromList [ (k, 64) | k <- mine ]
        }
      pif = emptyPeerTxInFlight { pifAdvertised = IntSet.fromList mine }
      st1 = sweepSharedState later st0
      (action, peerState', pif', st2) =
        nextPeerAction later (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                       defaultTxSubmissionConfigV2 peerAddr peerState pif st1
      st3 = scrubFromPeerInFlight peerAddr later pif st1
  in counterexample (show (st1, action)) $
       classify (not (null mine)) "advertises something expired" $
       counterexample "retained entries after the sweep"
         (retainedSize (sharedRetainedTxs st1) === 0)
       .&&. combinedStateInvariantExpired (IntSet.fromList mine)
                 (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
              (Map.singleton peerAddr (peerState, pif)) st1
       .&&. counterexample "the action acknowledges every expired key"
              (case action of
                 PeerRequestTxIds acked _ -> fromIntegral acked === length mine
                 _                        -> counterexample (show action) False)
       .&&. counterexample "the advertised set after the acknowledgement"
              (pifAdvertised pif' === IntSet.empty)
       .&&. combinedStateInvariant (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
                                   (Map.singleton peerAddr (peerState', pif')) st2
       .&&. counterexample "leaving with expired keys writes nothing"
              (sharedRevision st3 === sharedRevision st1)
  where
    peerAddr :: PeerAddr
    peerAddr = 0

-- | A leaving peer takes one advertiser off each active entry it
-- advertised and nothing else off them, a structural write; its keys no
-- longer in the table, advertised, leased or attempted, cost nothing, a
-- write included; the entries that leaves with nobody go with the next
-- sweep, the others stay.
prop_scrub_advertisers :: SweepState -> [Bool] -> [Bool] -> Property
prop_scrub_advertisers ss@SweepState { ssActive } flags gone =
  let st   = sweepStateOf ss
      mine = IntSet.fromList [ k | ((k, _, _), True) <- zip ssActive (flags ++ repeat False) ]
      -- Keys above any the state has.
      absent = IntSet.fromList [ 100 + i | (i, True) <- zip [0 :: Int ..] gone ]
      st0  = advertising mine st
      pif  = emptyPeerTxInFlight {
          pifAdvertised = mine `IntSet.union` absent,
          pifLeased     = absent,
          pifAttempting = absent
        }
      st1  = scrubFromPeerInFlight (0 :: PeerAddr) now pif st0
      st2  = sweepSharedState now st1
      kept = IntSet.fromList [ k | (k, n, kind) <- ssActive, kind /= Claimable || n > 0 ]
  in counterexample (show (st1, st2)) $
       classify (not (IntSet.null mine)) "advertises something" $
       classify (not (IntSet.null absent)) "holds absent keys" $
       counterexample "entries after the scrub"
         (sharedTxTable st1 === sharedTxTable st)
       .&&. counterexample "the scrub writes exactly when it advertised an entry"
              ((sharedRevision st1 > sharedRevision st0) === not (IntSet.null mine))
       .&&. counterexample "the scrub grants no option"
              (sharedGeneration st1 === sharedGeneration st0)
       .&&. counterexample "entries after the sweep"
              (IntMap.keysSet (sharedTxTable st2) === kept)

-- | Bounded sweeps from the start, each going on from the cursor the last
-- returned, one per limit of table entries and on until no expired entry
-- remains, leave what one unbounded sweep leaves, revision aside.
prop_sweep_rotation :: SweepState -> Property
prop_sweep_rotation ss@SweepState { ssLimit } =
  let st0 = sweepStateOf ss
      -- The sweeps that cover the table once: entries only leave it
      -- meanwhile, and each sweep walks what its expiry left of the limit.
      (final, sweeps) = rotate st0 sweepStart 0 0
      unbounded = sweepSharedState now st0
  in counterexample (show (sweeps, final)) $
       counterexample "state after the rotation"
         (final { sharedRevision = 0 } === unbounded { sharedRevision = 0 })
       .&&. sharedTxStateInvariant final
  where
    tableSize = IntMap.size (sharedTxTable (sweepStateOf ss))

    rotate :: SharedTxState PeerAddr TxId -> SweepCursor -> Int -> Int
           -> (SharedTxState PeerAddr TxId, Int)
    rotate st cursor walked n =
      let (st', more, cursor') = sweepSharedStateUpTo ssLimit now cursor st
          expired = retainedSize (sharedRetainedTxs st) - retainedSize (sharedRetainedTxs st')
          walked' = walked + (ssLimit - expired)
      in if (walked' >= tableSize && not more) || n >= 1000
            then (st', n + 1)
            else rotate st' cursor' walked' (n + 1)

-- | One bounded sweep expires at most half its limit of retained entries,
-- rounded up, tears down at most its limit of table entries, at most its
-- limit of both together, and drops at most its limit of lookup entries:
-- the expired ones' and the orphans'.
prop_sweepUpTo_boundedWork :: SweepState -> Property
prop_sweepUpTo_boundedWork ss@SweepState { ssLimit } =
  let st = sweepStateOf ss
      (st', _, _) = sweepSharedStateUpTo ssLimit now sweepStart st
      shrank f = f st - f st'
      expired = shrank (retainedSize . sharedRetainedTxs)
      tornDown = shrank (IntMap.size . sharedTxTable)
  in counterexample (show st') $
          counterexample "retained" (expired <= ssLimit - ssLimit `div` 2)
     .&&. counterexample "tx table" (tornDown <= ssLimit)
     .&&. counterexample "both together" (expired + tornDown <= ssLimit)
     .&&. counterexample "lookup entries" (shrank (IntMap.size . sharedKeyToTxId) <= ssLimit)

-- | Walks from any key on, each going on from where the last stopped,
-- take the map's keys in cyclic order, the limit of them each time or the
-- whole map.
prop_walkFrom_cyclic :: [NonNegative Int] -> Positive Int -> NonNegative Int -> Property
prop_walkFrom_cyclic ks (Positive limit) (NonNegative from) =
  let m = IntMap.fromList [ (k, ()) | NonNegative k <- ks ]
      n = IntMap.size m
      walks = take 5 (go from)
      rotated = [ k | k <- IntMap.keys m, k >= from ] ++ [ k | k <- IntMap.keys m, k < from ]
      expected | n == 0    = []
               | otherwise = take (5 * min limit n) (cycle rotated)
  in counterexample (show walks) $ concat walks === expected
  where
    go :: Int -> [[Int]]
    go f = let (taken, f') = walkFrom limit f (IntMap.fromList [ (k, ()) | NonNegative k <- ks ])
           in map fst taken : go f'

--
-- nextPeerActionPipelined
--

-- | When the peer has at least one ackable txid AND room to request
-- more, 'nextPeerActionPipelined' emits a 'PeerRequestTxIds' with both
-- counts non-zero.
prop_nextPeerActionPipelined_requestsTxIds
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_requestsTxIds (ArbTxDecisionPolicy policy) =
  let
    config = TxSubmissionConfig { maxNumUnacknowledgedTxIds = 8
                                , maxNumTxIdsToRequest      = 4 }
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
      nextPeerActionPipelined now policy config peerAddr peerState0
                              emptyPeerTxInFlight sharedState0
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxIds ack req ->
          conjoin [ counterexample "ack should be non-zero"
                      (ack /= 0)
                  , counterexample "req should be non-zero"
                      (req /= 0)
                  ]
        _ -> property False
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
        config = defaultTxSubmissionConfigV2
        policy = unsafeMkTxDecisionPolicy config
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
          nextPeerActionPipelined now policy config peerAddr peerState0
                                  emptyPeerTxInFlight sharedState0

    step "Pipelined response must keep the txid unacked while the body batch is outstanding"
    case action of
      PeerDoNothing {} ->
        -- the only ackable txid must stay unacked, and a pipelined request
        -- has to ack as well as request: the peer waits
        pure ()
      other ->
        assertFailure ("unexpected action: " ++ show other)
  where
    peerAddr = 1 :: PeerAddr

-- | Request-only pipelined messages ('ack=0, req=N') poll the peer for
-- txids without letting its unack window grow, so the pipelined picker
-- must not emit them: a pipelined txid request has to both ack and
-- request.  When the peer has an unacked txid that is *not* ackable (so
-- the ack prefix is empty), the picker parks instead, even though the
-- unack window still has room.
prop_nextPeerActionPipelined_rejectsRequestOnly
  :: ArbTxDecisionPolicy
  -> Property
prop_nextPeerActionPipelined_rejectsRequestOnly (ArbTxDecisionPolicy policy) =
  let
    -- Small windows, so the property is easy to reason about.
    config = TxSubmissionConfig { maxNumUnacknowledgedTxIds = 4
                                , maxNumTxIdsToRequest      = 4 }
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
    -- Leave 'peerAvailableTxIds' empty so 'pickRequestTxsAction' finds no
    -- body candidate and the scheduler falls through to the txid picker.
    peerState0 = emptyPeerTxLocalState {
        peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey keyInt),
        peerRequestedTxIds = 1  -- pretend there's a pipelined req in flight
      }
    peerInFlight0 = emptyPeerTxInFlight {
        pifAdvertised = IntSet.singleton keyInt
      }

    (action, _, _, _) =
      nextPeerActionPipelined now policy config peerAddr peerState0
                              peerInFlight0 sharedState1
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerDoNothing {} -> property True
        _                -> property False
  where
    peerAddr = 1 :: PeerAddr

-- | Pure-ack pipelined messages (ack=N, req=0) burn a pipeline slot for
-- an empty reply and shrink the txid window without growing it.  The
-- pipelined picker must defer such acks until they can be bundled with
-- a request.
unit_nextPeerActionPipelined_rejectsPureAck
  :: (String -> IO ()) -> Assertion
unit_nextPeerActionPipelined_rejectsPureAck step = do
    step "Set up: two retained (ackable) txids in the unacked queue, request cap saturated"
    -- Make the per-message request cap small so we can saturate it with
    -- 'peerRequestedTxIds' and force the request count to zero.
    let config = defaultTxSubmissionConfigV2
                  { maxNumUnacknowledgedTxIds = 8
                  , maxNumTxIdsToRequest      = 2 }
        policy = unsafeMkTxDecisionPolicy config
        txids = [(1, 64), (2, 64)] :: [(TxId, SizeInBytes)]
        sharedState0 = seedRetainedTxids policy txids emptySharedTxState
        keys = [ TxKey (unTxKey (lookupKeyOrFail txid sharedState0))
               | (txid, _) <- txids ]
        -- 'peerRequestedTxIds' equals 'maxNumTxIdsToRequest' so the
        -- request cap is saturated and 'txIdsToRequest' evaluates to 0.
        -- Two ackable txids in the queue with the keep-one-unacked clamp
        -- means the ack count would be 1 - i.e. ack=1, req=0.
        peerState0 = emptyPeerTxLocalState {
            peerUnacknowledgedTxIds = StrictSeq.fromList keys,
            peerRequestedTxIds      = 2
          }

    step "Run nextPeerActionPipelined"
    let (action, _, _, _) =
          nextPeerActionPipelined now policy config peerAddr peerState0
                                  emptyPeerTxInFlight sharedState0

    step "Pure-ack pipelined message must not be emitted"
    case action of
      PeerRequestTxIds {} ->
        assertFailure ("unexpected pure-ack request: " ++ show action)
      _ ->
        pure ()
  where
    peerAddr = 1 :: PeerAddr

-- | While a peer holds one outstanding body batch and there is room for
-- another, 'nextPeerActionPipelined' opens the second batch.
prop_nextPeerActionPipelined_secondBodyBatch
  :: ArbTxDecisionPolicy
  -> ArbTxSubmissionConfig
  -> Property
prop_nextPeerActionPipelined_secondBodyBatch (ArbTxDecisionPolicy basePolicy) (ArbTxSubmissionConfig config) =
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
              , txAdvertisers = 1
              , txInSubmission = False
              , currentMaxInflightMultiplicity = txInflightMultiplicity policy
              })
          , (kB, TxEntry
              { txLease = TxClaimable now
              , txAttempt = 0
              , txAdvertisers = 1
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
      nextPeerActionPipelined now policy config peerAddr peerState0
                              peerInFlight0 sharedState
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxs [picked] ->
          conjoin
            [ counterexample "expected to pick keyB"
                (picked === keyB)
            , counterexample "B not added to pifLeased"
                (IntSet.member kB (pifLeased peerInFlight'))
            , counterexample "B not added to pifAttempting"
                (IntSet.member kB (pifAttempting peerInFlight'))
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
  -> ArbTxSubmissionConfig
  -> Property
prop_nextPeerActionPipelined_noThirdBodyBatch (ArbTxDecisionPolicy basePolicy) (ArbTxSubmissionConfig config) =
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
      , txAdvertisers = 1
      , txInSubmission = False
      , currentMaxInflightMultiplicity = txInflightMultiplicity policy
      }
    freshEntry = TxEntry
      { txLease = TxClaimable now
      , txAttempt = 0
      , txAdvertisers = 1
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
      nextPeerActionPipelined now policy config peerAddr peerState0
                              peerInFlight0 sharedState
  in
    counterexample ("got: " ++ show action) $
      case action of
        PeerRequestTxs {} -> False
        _                 -> True
  where
    peerAddr = 7 :: PeerAddr

-- | A peer with two unacked txids - one whose entry is leased to
-- another peer (blocked) and one that's still claimable - must skip
-- the blocked one and fetch the claimable one.
unit_nextPeerAction_skipsBlockedAvailableTxs
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_skipsBlockedAvailableTxs step = do
    step "Set up two advertised txs: keyA leased to another peer with the cap full, keyB claimable"
    let config      = defaultTxSubmissionConfigV2
        policy0     = unsafeMkTxDecisionPolicy config
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
                  , txAdvertisers = 1
                  , txInSubmission = False
                  , currentMaxInflightMultiplicity = txInflightMultiplicity policy
                  })
              , (kB, TxEntry
                  { txLease = TxClaimable now
                  , txAttempt = 0
                  , txAdvertisers = 1
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
          nextPeerAction now policy config peerAddr peerState peerInFlight sharedState

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
        resolvedKey = TxKey 1
        blockedKey  = TxKey 2
        kResolved   = unTxKey resolvedKey
        kBlocked    = unTxKey blockedKey
        blockedTx   = mkTx 2 (mkSize (Positive 10))
        config      = defaultTxSubmissionConfigV2
        policy      = unsafeMkTxDecisionPolicy config
        blockedEntry = TxEntry
          { txLease = TxLeased peerAddr (addTime 10 now)
          , txAttempt = 1
          , txAdvertisers = 1
          , txInSubmission = True
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
        sharedState = emptySharedTxState
          { sharedTxTable = IntMap.singleton kBlocked blockedEntry
          , sharedRetainedTxs = retainedSingleton kResolved (retainDeadline (addTime 17 now))
          , sharedTxIdToKey = Map.fromList
              [(getRawTxId (1 :: TxId), resolvedKey), (getRawTxId (2 :: TxId), blockedKey)]
          , sharedKeyToTxId = IntMap.fromList [(kResolved, 1 :: TxId), (kBlocked, 2 :: TxId)]
          , sharedNextTxKey = 3
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.fromList [resolvedKey, blockedKey]
          , peerDownloadedTxs = IntMap.singleton kBlocked blockedTx
          }
        -- Another peer (not in this single-peer snapshot) is mid-submission,
        -- so the entry's 'txInSubmission' flag is set.  peerAddr holds the
        -- lease and has the body buffered, but must skip submitting while
        -- the flag is set; it can still ack the safe retained prefix.
        peerInFlight = emptyPeerTxInFlight
          { pifLeased     = IntSet.singleton kBlocked
          , pifAttempting = IntSet.singleton kBlocked
          }

    step "Run nextPeerAction (with the blocked tx in submission by another peer)"
    let (action, peerState', _, _) =
          nextPeerAction now policy config peerAddr peerState peerInFlight sharedState

    -- 'pickBufferedTxsToSubmit' finds the blocked tx buffered but in
    -- submission ('txInSubmission' set), so it skips it; the walk falls
    -- through to a txid request that acks the retained prefix only.
    step "Only the retained prefix is acked; blocked tx remains unacked"
    case action of
      PeerRequestTxIds ack req -> do
        ack @?= 1
        assertBool ("expected positive txIdsToReq, got " ++ show req) (req > 0)
        toList (peerUnacknowledgedTxIds peerState') @?= [blockedKey]
      other ->
        assertFailure ("unexpected action: " ++ show other)

-- | When a peer's unack queue has a buffered body, then a
-- resolved-elsewhere gap (the txid has no entry in 'sharedTxTable'),
-- then another buffered body, 'pickBufferedTxsToSubmit' must skip the
-- gap and emit both buffered bodies in one 'PeerSubmitTxs' batch.
--
-- Regression guard for the Nothing-vs-Just distinction in
-- 'pickBufferedTxsToSubmit': collapsing the 'Nothing' branch into
-- "stop" leaves the second buffered body unsubmitted, breaking
-- progress when an intermediate txid was resolved by another peer.
unit_nextPeerAction_submitsBufferedAcrossResolvedGap
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_submitsBufferedAcrossResolvedGap step = do
    step "Set up: 3 unack txids, bodies buffered for 1st and 3rd; 2nd resolved elsewhere"
    let peerAddr  = 7 :: PeerAddr
        key1      = TxKey 1
        key2      = TxKey 2
        key3      = TxKey 3
        k1        = unTxKey key1
        k3        = unTxKey key3
        body1     = mkTx 1 (mkSize (Positive 10))
        body3     = mkTx 3 (mkSize (Positive 10))
        config    = defaultTxSubmissionConfigV2
        policy    = unsafeMkTxDecisionPolicy config
        leaseTime = addTime 10 now
        entry     = TxEntry
          { txLease                        = TxLeased peerAddr leaseTime
          , txAttempt                      = 1
          , txAdvertisers = 1
          , txInSubmission                 = False
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
        sharedState = emptySharedTxState
          { sharedTxTable   = IntMap.fromList [(k1, entry), (k3, entry)]
          , sharedTxIdToKey = Map.fromList
              [ (getRawTxId (1 :: TxId), key1)
              , (getRawTxId (2 :: TxId), key2)
              , (getRawTxId (3 :: TxId), key3)
              ]
          , sharedKeyToTxId = IntMap.fromList
              [(k1, 1 :: TxId), (unTxKey key2, 2), (k3, 3)]
          , sharedNextTxKey = 4
          }
        peerState = emptyPeerTxLocalState
          { peerUnacknowledgedTxIds = StrictSeq.fromList [key1, key2, key3]
          , peerDownloadedTxs       = IntMap.fromList [(k1, body1), (k3, body3)]
          }
        peerInFlight = emptyPeerTxInFlight
          { pifLeased     = IntSet.fromList [k1, k3]
          , pifAttempting = IntSet.fromList [k1, k3]
          }

    step "Run nextPeerAction"
    let (action, _, _, _) =
          nextPeerAction now policy config peerAddr peerState peerInFlight sharedState

    step "Both buffered bodies submitted in one batch"
    case action of
      PeerSubmitTxs ks -> ks @?= [key1, key3]
      other            -> assertFailure ("unexpected action: " ++ show other)

-- | When peer A is at its inflight-size cap and a fresh txid arrives,
-- peer A cannot claim; once peer B advertises the same key, peer B can
-- claim and download.
unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsFreshTxWhenFirstAdvertiserIsFull step = do
    step "Receive a fresh txid from peer A while A is already at its inflight size limit"
    let (peerAState1, peerAInFlight1, sharedState1) =
          handleReceivedTxIds (const False) now policy
            requestedToReply [(txid, txSize)]
            peerAState0 emptyPeerTxInFlight sharedState0
    txLease (lookupEntryOrFail key sharedState1) @?= TxClaimable now

    step "nextPeerAction for peer A: tx remains unclaimed because A is at the size cap"
    let (peerAAction, _, _, _) =
          nextPeerAction now policy config peerA
            peerAState1 peerAInFlight1 sharedState1
    case peerAAction of
      PeerDoNothing _ _ -> pure ()
      PeerRequestTxIds {} -> pure ()
      other -> assertFailure ("unexpected peer A action: " ++ show other)

    step "Peer B advertises the same txid"
    let (peerBState1, peerBInFlight1, sharedState2) =
          handleReceivedTxIds (const False) now policy
            requestedToReply [(txid, txSize)]
            peerBState0 emptyPeerTxInFlight sharedState1

    step "nextPeerAction for peer B: claims and requests the fresh tx"
    let (peerBAction, peerBState2, peerBInFlight2, sharedState3) =
          nextPeerAction now policy config peerB
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
    config = defaultTxSubmissionConfigV2
    policy = unsafeMkTxDecisionPolicy config
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
      , peerRequestedTxsSize = txsSizeInflightPerPeer policy
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
    let config = defaultTxSubmissionConfigV2
        policy = unsafeMkTxDecisionPolicy config
        txid :: TxId
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
              , txAdvertisers = 1
              , txInSubmission = True
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity policy
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
          handleSubmittedTxs now policy peerA
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
          nextPeerAction now policy config peerB
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
-- claiming a TxClaimable entry.  With any non-zero score the floor is
-- 10 ms (see 'peerClaimDelay'); set up the entry to become claimable
-- exactly 10 ms before @now@ so the claim is just allowed.
unit_nextPeerAction_claimsAtScoreDelayThreshold
  :: (String -> IO ()) -> Assertion
unit_nextPeerAction_claimsAtScoreDelayThreshold step = do
    step "Set up: claimableAt = now - 10ms, peer score = 20 (10ms floor claim delay), so claim at now is just allowed"
    let config = defaultTxSubmissionConfigV2
        policy = unsafeMkTxDecisionPolicy config
        peerAddr = 7 :: PeerAddr
        txid :: TxId
        txid = 1
        txSize = mkSize (Positive 10)
        key = TxKey 0
        k   = unTxKey key
        claimableAt = Time 99.990  -- now is Time 100; 10 ms in the past
        sharedState = emptySharedTxState
          { sharedTxIdToKey = Map.singleton (getRawTxId txid) key
          , sharedKeyToTxId = IntMap.singleton k txid
          , sharedNextTxKey = 1
          , sharedTxTable = IntMap.singleton k TxEntry
              { txLease = TxClaimable claimableAt
              , txAttempt = 0
              , txAdvertisers = 1
              , txInSubmission = False
              , currentMaxInflightMultiplicity =
                  txInflightMultiplicity policy
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
          nextPeerAction now policy config peerAddr
            peerState peerInFlight sharedState

    step "Tx becomes claimable once the score-derived 10ms floor has elapsed"
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
    let config = defaultTxSubmissionConfigV2
        policy = unsafeMkTxDecisionPolicy config
        peerAddr       = 7 :: PeerAddr
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
          , txAdvertisers = 1
          , txInSubmission = True   -- submittingPeer is in mempoolAddTxs
          , currentMaxInflightMultiplicity =
              txInflightMultiplicity policy
          }
        sharedState = emptySharedTxState
          { sharedTxTable = IntMap.fromList
              [ (kBlocked, blockedEntry)
              , (kClaimable, TxEntry
                  { txLease = TxClaimable now
                  , txAttempt = 0
                  , txAdvertisers = 1
                  , txInSubmission = False
                  , currentMaxInflightMultiplicity =
                      txInflightMultiplicity policy
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
          nextPeerAction now policy config peerAddr
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
  -> ArbTxSubmissionConfig
  -> TxId
  -> Property
prop_nextPeerAction_nonOwnerWaitsUntilResolved (ArbTxDecisionPolicy policy)
                                               (ArbTxSubmissionConfig config)
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
          , txAdvertisers = 1
          , txInSubmission = False
          , currentMaxInflightMultiplicity = txInflightMultiplicity policy
          }
      , sharedTxIdToKey = Map.singleton (getRawTxId txid) key
      , sharedKeyToTxId = IntMap.singleton k txid
      , sharedNextTxKey = 1
      }
    resolvedSharedState = unresolvedSharedState
      { sharedTxTable = IntMap.empty
      , sharedRetainedTxs = retainedSingleton k (retainDeadline (addTime 17 now))
      }
    peerState0 = emptyPeerTxLocalState
      { peerUnacknowledgedTxIds = StrictSeq.singleton key
      , peerRequestedTxIds = 0
      }
    peerInFlight0 = emptyPeerTxInFlight
      { pifAdvertised = IntSet.singleton k
      }

    (unresolvedAction, unresolvedPeerState', _, _) =
      nextPeerAction now policy config peerAddr peerState0 peerInFlight0
                     unresolvedSharedState
    (resolvedAction, resolvedPeerState', _, _) =
      nextPeerAction now policy config peerAddr peerState0 peerInFlight0
                     resolvedSharedState
  in
    conjoin
      [ counterexample "unresolved: must wait, the txid unacked and no request sent" $
          case unresolvedAction of
            PeerDoNothing _ _ ->
              peerUnacknowledgedTxIds unresolvedPeerState'
                === peerUnacknowledgedTxIds peerState0
            other ->
              counterexample ("unexpected unresolved action: " ++ show other)
                             False
      , counterexample "resolved: must ack the now-retained txid" $
          case resolvedAction of
            PeerRequestTxIds ack _ ->
              conjoin
                [ ack === 1
                , peerUnacknowledgedTxIds resolvedPeerState' === StrictSeq.empty
                ]
            other ->
              counterexample ("unexpected resolved action: " ++ show other)
                             False
      ]
  where
    peerAddr = 1 :: PeerAddr

-- | In non-pipelined mode a txid request without acks is the blocking poll
-- of an empty unacked queue; while txids sit unacked with nothing ackable
-- the peer waits rather than polling with @ack=0, req=N@.
prop_nextPeerAction_blockingOnlyWhenEmpty :: ArbTxDecisionPolicy -> NonNegative Int -> Property
prop_nextPeerAction_blockingOnlyWhenEmpty (ArbTxDecisionPolicy policy) (NonNegative n0) =
  let n = n0 `mod` 7
      txids = [1 .. n] :: [TxId]
      -- the entries leased to another peer and not downloaded here: nothing
      -- for this peer to ack, claim or submit
      shared0 = mkActiveSharedState [leaseholder] leaseholder [] [ (txid, 64) | txid <- txids ]
      keys = [ lookupKeyOrFail txid shared0 | txid <- txids ]
      peerState0 = emptyPeerTxLocalState { peerUnacknowledgedTxIds = StrictSeq.fromList keys }
      pif0 = emptyPeerTxInFlight { pifAdvertised = IntSet.fromList (map unTxKey keys) }
      (action, _, _, _) = nextPeerAction now policy defaultTxSubmissionConfigV2 peerAddr
                                         peerState0 pif0 shared0
  in counterexample (show action) $
       classify (n == 0) "empty queue" $
       case action of
         PeerRequestTxIds ack req ->
           counterexample "a request without acks while txids sit unacked"
             (n === 0 .&&. ack === 0 .&&. property (req > 0))
         PeerDoNothing {} ->
           counterexample "waiting on an empty queue" (property (n > 0))
         unexpected ->
           counterexample ("unexpected " ++ show unexpected) False
  where
    peerAddr    = 1 :: PeerAddr
    leaseholder = 2 :: PeerAddr

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
  -> ArbTxSubmissionConfig
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
    (ArbTxSubmissionConfig config)
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
              Just (PeerDoNothing (Just delay) _, _, _) ->
                let diff = abs (delay - expectedBadDelay) in
                counterexample
                  ("delay = " ++ show delay
                    ++ ", expected = " ++ show expectedBadDelay
                    ++ ", diff = " ++ show diff)
                  (diff < 1e-9)
              other ->
                counterexample ("got: " ++ show other) False
        , counterexample ("Confounder must do nothing with no scheduled wake: "
                            ++ show (lookupAction Confounder)) $
            case lookupAction Confounder of
              Just (PeerDoNothing Nothing _, _, _) -> True
              _                                    -> False
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
    badClaimDelay = peerClaimDelay policy now badPeerScore
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
          , txAdvertisers = 1
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
        -- Saturate the request cap so the txid picker has no work for
        -- Bad; the only relevant action is the score-derived wake from
        -- 'PeerDoNothing'.
      , peerRequestedTxIds      = maxNumTxIdsToRequest config
      }
    badInFlight0 = emptyPeerTxInFlight { pifAdvertised = IntSet.singleton k }

    confPeerState0 = emptyPeerTxLocalState
      { peerRequestedTxIds = maxNumTxIdsToRequest config
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
            nextPeerAction now policy config peer ps0 pif0 ss
      in (ss', (role, action, ps', pif') : acc)

    (sharedStateFinal, results) = List.foldl' runOne (sharedState0, []) order

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
          advertiserCount = length [() | (_, t) <- trigs, hasActiveEntry t]
          lease = case submittingPeer txid of
            Just p  -> TxLeased p (addTime 10 now)
            Nothing -> case laterDelay txid of
              Just d  -> TxClaimable (addTime (fromIntegral d) now)
              Nothing -> TxClaimable now in
      TxEntry
        { txLease           = lease
        , txAttempt         = attemptCount
        , txInSubmission    = False
        , txAdvertisers     = advertiserCount
        , currentMaxInflightMultiplicity = txInflightMultiplicity policy
        }

    activeTxids = [ txid | txid <- allTxids
                  , categoryOf (map snd (triggersFor txid)) /= CatAck ]
    retainedTxids = [ txid | txid <- allTxids
                    , categoryOf (map snd (triggersFor txid)) == CatAck ]

    retainedUntil   = retainDeadline (addTime 600 now)
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
            { pifAdvertised  = advertised
            , pifLeased      = leasedHere
            , pifAttempting  = attemptingHere
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
metaPolicy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2) { txInflightMultiplicity = 2 }

prop_TriggerScenario_validInitialState :: TriggerScenario -> Property
prop_TriggerScenario_validInitialState (TriggerScenario _ rawPerPeer) =
    let perPeer           = normaliseScenario rawPerPeer
        (peerStates, ss0) = buildTriggerState metaPolicy perPeer in
    combinedStateInvariant metaPolicy peerStates ss0

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
      $ ts `notElem` shrink ts

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
  -> ArbTxSubmissionConfig
  -> TriggerScenario
  -> Property
prop_nextPeerAction_processesAllTriggers
    (ArbTxDecisionPolicy arbPolicy)
    (ArbTxSubmissionConfig config)
    (TriggerScenario mode rawPerPeer) =
      tabulate "trigger count" [bucket totalTriggers]
    . tabulate "peer count"    [show nPeers]
    . tabulate "iterations"    [bucket iterations]
    . tabulate "shared txids"  [bucket sharedTxidCount]
    . tabulate "overlap mode"  [show mode]
    $ conjoin
        [ counterexample "loop must terminate within step budget"
            $ terminated
        , counterexample "submitted set must equal CatSubmit txids"
            $ allSubmitted === expectedSubmitted
        , counterexample "requested set must equal CatFetch txids"
            $ allRequested === expectedFetched
        , counterexample
            ("expected acks missing: " ++ show (IntSet.toList missingAcks))
            $ IntSet.null missingAcks
        , counterexample "initial combined invariant violated"
            (combinedStateInvariant policy peerStates0 sharedState0)
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
                              [ combinedStateInvariantStep policy
                                  (peerSnapshot p ps2 pif2)
                                  ss ss2
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
                    nextPeerAction time policy config p psPre pifPre ssPre
                  oldUnacked = peerUnacknowledgedTxIds psPre
                  newUnacked = peerUnacknowledgedTxIds ps'
                  numAcked   = StrictSeq.length oldUnacked
                             - StrictSeq.length newUnacked
                  ackedNow   = IntSet.fromList $ map unTxKey
                             $ toList (StrictSeq.take numAcked oldUnacked)
                  step       = stepDrain + 1
                  inv        = conjoin
                    [ combinedStateInvariantStep policy
                        (peerSnapshot p ps' pif') ssPre ss'
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
                PeerDoNothing Nothing _ ->
                  let schedule' = Map.insert p (Nothing, ps', pif') schedule in
                  runLoop ss' schedule' pendingPre subs reqs acks
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'
                PeerDoNothing (Just delay) _ ->
                  let nextWake = addTime (max delay 0.001) time
                      schedule' = Map.insert p (Just nextWake, ps', pif') schedule in
                  runLoop ss' schedule' pendingPre subs reqs acks
                       ((step, p, inv) : drainInvs ++ invs) step lastTime'
                PeerSubmitTxs ks ->
                  let (ps'', pif'', ss'') =
                        handleSubmittedTxs time policy p (withTxIds ss' ks) [] ps' pif' ss'
                      postInv = conjoin
                        [ combinedStateInvariantStep policy
                            (peerSnapshot p ps'' pif'') ss' ss''
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
      peerScore = PeerScore peerScoreValue peerScoreTs,
      peerTier = FastTier,
      peerNovelAt = StrictSeq.empty,
      peerSlowGate = Nothing,
      peerJitterSeed = 0,
      peerRequestTiers = StrictSeq.empty
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

  buildSharedTxState activeEntries retainedEntries <$> genSmallWord64
  where
    genRetainedEntry txid = do
      retainUntil <- genSharedExpiryTime
      pure (txid, retainDeadline retainUntil)

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
      txAdvertisers = 1,
      txInSubmission = inSub,
      currentMaxInflightMultiplicity =
        txInflightMultiplicity (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
    }

-- Generate a claimable entry with no in-flight attempt.
genClaimableTxEntry :: Gen (TxEntry PeerAddr)
genClaimableTxEntry = do
  claimableAt <- genSharedExpiryTime
  pure TxEntry {
      txLease = TxClaimable claimableAt,
      txAttempt = 0,
      txAdvertisers = 1,
      txInSubmission = False,
      currentMaxInflightMultiplicity =
        txInflightMultiplicity (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
    }

-- Rebuild a shared state from tx-centric fixtures while preserving interned keys.
buildSharedTxState
  :: [(TxId, TxEntry PeerAddr)]
  -> [(TxId, RetainTime)]
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
  -- rebuilding with the counters at zero reproduces an input whose
  -- counters are zero already; such a candidate would shrink forever
  filter (/= sharedState) $ nub $
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

-- | Keys with their txids, as a submission reports its accepted txs; a key
-- no txid maps to is left out.
withTxIds :: SharedTxState PeerAddr TxId -> [TxKey] -> [(TxKey, TxId)]
withTxIds st ks =
  [ (k, txid) | k@(TxKey i) <- ks, Just txid <- [IntMap.lookup i (sharedKeyToTxId st)] ]

-- | A registry with no local roots.
noRootsRegistry :: MonadSTM m => StdGen -> m (PeerTxRegistry m peeraddr)
noRootsRegistry g = newPeerTxRegistry g (pure (const False))

-- | A registry for the tests' 'PeerAddr' peers, the address type pinned
-- where nothing else fixes it.
newPeerAddrRegistry :: MonadSTM m => StdGen -> m (PeerTxRegistry m PeerAddr)
newPeerAddrRegistry = noRootsRegistry

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
      List.foldl' (\r k -> retainedInsertMax k retainUntil r)
                  (sharedRetainedTxs stInterned)
                  retainedKeys
  }
  where
    retainUntil         = retainDeadline (addTime (bufferedTxsMinLifetime policy) now)
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

-- | 'mkReceiveDuplicateFixture' after the advertised txs resolved into
-- retained entries: the standard case, txids the node already has, which
-- must write nothing shared.
mkReceiveRetainedFixture :: Int -> Int -> ReceiveDuplicateFixture
mkReceiveRetainedFixture existingAdvertisers txidCount =
  fixture { rdfSharedState = retainAllActiveTxs (rdfSharedState fixture) }
  where
    fixture = mkReceiveDuplicateFixture existingAdvertisers txidCount

-- Prebuild an ack-only workload after all advertised txs have resolved into
-- retained entries.
mkResolvedAckFixture :: Int -> Int -> PeerActionFixture
mkResolvedAckFixture advertiserCount txidCount =
  PeerActionFixture
    { pafPeerAddr = targetPeer
    , pafPeerState =
        emptyPeerTxLocalState {
          peerUnacknowledgedTxIds = StrictSeq.fromList txKeys,
          peerAvailableTxIds =
            IntMap.fromList
              [ (unTxKey txKey, txSize)
              | (txKey, (_, txSize)) <- zip txKeys txidsAndSizes
              ]
        }
      -- The peer received the txids while their entries were active, so
      -- it still lists them as available and advertised.
    , pafPeerInFlight =
        emptyPeerTxInFlight {
          pifAdvertised = IntSet.fromList (unTxKey <$> txKeys)
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
    txKeys = fmap ((`lookupKeyOrFail` sharedState0) . fst) txidsAndSizes

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

-- | Per-iteration timestamp for the benchmark loops below.
--
-- The offset (one picosecond per loop index) is far below any policy
-- timescale, so every iteration exercises the same code path, but it makes
-- each iteration's workload a distinct expression.  Without it GHC shares a
-- single result across all iterations (directly via a where-binding, or via
-- let-floating of a loop-invariant body), and the loop degenerates into
-- compute-once-then-'rnf'.
iterationTime :: Int -> Time
iterationTime n = addTime (fromIntegral n * 1e-12) now

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
              (iterationTime n)
              (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
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
  , pafPeerInFlight
  , pafSharedState
  } =
    go iterations
  where
    config = defaultTxSubmissionConfigV2
    policy = unsafeMkTxDecisionPolicy config
    go 0 = pure ()
    go n = do
      let result =
            nextPeerAction (iterationTime n) policy config pafPeerAddr
              pafPeerState pafPeerInFlight pafSharedState
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
    config = defaultTxSubmissionConfigV2
    policy = unsafeMkTxDecisionPolicy config
    go 0 = pure ()
    go n = do
      _ <- evaluate (rnf (roundResult (iterationTime n)))
      go (n - 1)

    roundResult iterNow =
      let (!peerStatesRev, !sharedStateAfterReceive) =
            List.foldl' (receiveOne iterNow) ([], ffInitialSharedState) ffPeers
          !sharedStateResolved = retainAllActiveTxs sharedStateAfterReceive
          (!ackResultsRev, !sharedStateAfterAck) =
            List.foldl' (acknowledgeOne iterNow) ([], sharedStateResolved) (reverse peerStatesRev)
      in (reverse peerStatesRev, reverse ackResultsRev, sharedStateAfterAck)

    receiveOne
      :: Time
      -> ([(PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)], SharedTxState PeerAddr TxId)
      -> PeerAddr
      -> ([(PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)], SharedTxState PeerAddr TxId)
    receiveOne iterNow (!peerStatesAcc, !sharedStateAcc) peeraddr =
      let peerState0 =
            emptyPeerTxLocalState {
              peerRequestedTxIds = ffRequestedTxIds
            }
          !(peerState', peerInFlight', sharedStateAcc') =
            handleReceivedTxIds
              (const False)
              iterNow
              policy
              ffRequestedTxIds
              ffTxidsAndSizes
              peerState0
              emptyPeerTxInFlight
              sharedStateAcc
      in ((peeraddr, peerState', peerInFlight') : peerStatesAcc, sharedStateAcc')

    acknowledgeOne
      :: Time
      -> ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
      -> (PeerAddr, PeerTxLocalState (Tx TxId), PeerTxInFlight)
      -> ([(PeerAddr, PeerAction, PeerTxLocalState (Tx TxId))], SharedTxState PeerAddr TxId)
    acknowledgeOne iterNow (!ackResultsAcc, !sharedStateAcc) (peeraddr, peerState0, peerInFlight0) =
      let !(peerAction, peerState', _peerInFlight', sharedStateAcc') =
            nextPeerAction iterNow policy config peeraddr
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
      , txAdvertisers = 1
      , txInSubmission = False
      , currentMaxInflightMultiplicity =
          txInflightMultiplicity (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
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
    retainUntil = retainDeadline (addTime retainLifetime now)
    retainLifetime = bufferedTxsMinLifetime (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)

    retainOne retainedAcc k _ =
      retainedInsertMax k retainUntil retainedAcc

--
-- Fetch tiers
--

-- | A slow peer with a stream of txids the node already holds, so nothing
-- but the gate ever spaces its txid requests: the policy, the peer's jitter
-- seed and the steps of the clock between decisions, in milliseconds.
data SlowStream = SlowStream ArbTxDecisionPolicy Word64 [Positive Int]
  deriving Show

instance Arbitrary SlowStream where
  arbitrary =
    SlowStream <$> arbitrary <*> arbitrary
               <*> listOf1 (Positive <$> frequency [ (3, choose (1, 50))
                                                   , (2, choose (51, 400))
                                                   , (1, choose (401, 3000)) ])
  shrink (SlowStream p s steps) =
       [ SlowStream p' s steps | p' <- shrink p ]
    ++ [ SlowStream p s steps' | steps' <- shrink steps, not (null steps') ]

-- | Drive one peer through the non-pipelined decision loop against a
-- stream of txids the mempool already holds.  Each txid request is
-- answered at once with as many fresh ids as it asked for.  Returns the
-- times of the txid requests and the final local state.
runStream :: TxDecisionPolicy
          -> PeerTxLocalState (Tx TxId)
          -> [Positive Int]
          -> ([Time], PeerTxLocalState (Tx TxId))
runStream policy peerState0 steps =
    ([ t | (t, PeerRequestTxIds {}, _, _) <- trace ], final)
  where
    (trace, final) = runStreamTrace policy peerState0 steps

-- | On the slow tier two txid requests are never closer than the cadence
-- less its jitter, blocking ones included: the stream is all duplicates,
-- so every request would otherwise be blocking and immediate.
prop_tier_slowRequestSpacing :: SlowStream -> Property
prop_tier_slowRequestSpacing (SlowStream (ArbTxDecisionPolicy policy0) seed steps) =
      cover 40 (length requests >= 2) "two or more requests"
    . cover 20 (length requests >= 5) "five or more requests"
    $ conjoin
        [ counterexample ("requests at " ++ show t0 ++ " and " ++ show t1
                          ++ ", least " ++ show least)
            (t1 `diffTime` t0 >= least - 1e-9)
        | (t0, t1) <- zip requests (drop 1 requests)
        ]
  where
    policy = policy0 { txSlowCadence = max 0.05 (txSlowCadence policy0)
                     , txSourceCount = 0 }
    least  = txSlowCadence policy * realToFrac (1 - txSlowJitter policy)
    (requests, _) =
      runStream policy emptyPeerTxLocalState { peerTier = SlowTier
                                             , peerJitterSeed = seed } steps

-- | A fast peer is served as if there were no cadence: the same requests
-- at the same times as under a zero cadence, and no gate is ever set.
prop_tier_fastIgnoresCadence :: SlowStream -> Property
prop_tier_fastIgnoresCadence (SlowStream (ArbTxDecisionPolicy policy) seed steps) =
      cover 40 (length requests >= 2) "two or more requests"
    $ requests === requests0
 .&&. peerSlowGate final === Nothing
  where
    fast = emptyPeerTxLocalState { peerTier = FastTier, peerJitterSeed = seed }
    (requests, final) = runStream policy fast steps
    (requests0, _)    = runStream policy { txSlowCadence = 0, txSlowJitter = 0 } fast steps

-- | A slow peer's txid request sets the gate within the jittered cadence.
prop_tier_slowSetsGate :: SlowStream -> Property
prop_tier_slowSetsGate (SlowStream (ArbTxDecisionPolicy policy0) seed steps) =
      cover 40 (not (null requests)) "a request"
    $ case (requests, peerSlowGate final) of
        ([], _)         -> property True
        (_, Nothing)    -> counterexample "no gate after a request" False
        (rs, Just gate) ->
          let wait = gate `diffTime` last rs in
          counterexample ("wait " ++ show wait ++ " outside " ++ show (lo, hi))
            (wait >= lo - 1e-9 && wait <= hi + 1e-9)
  where
    policy = policy0 { txSourceCount = 0 }
    lo = txSlowCadence policy * realToFrac (1 - txSlowJitter policy)
    hi = txSlowCadence policy * realToFrac (1 + txSlowJitter policy)
    (requests, final) =
      runStream policy emptyPeerTxLocalState { peerTier = SlowTier
                                             , peerJitterSeed = seed } steps

-- | Accepted txs from a peer: gaps between them and how many each time, in
-- milliseconds, with the window in the same units; and when to ask, either
-- some time after the last accept or exactly a window after one of them,
-- so the boundary is hit.
data Accepts = Accepts ArbTxDecisionPolicy [(Positive Int, Positive Int)] AskAt
  deriving Show

data AskAt = AfterLast (Positive Int) | WindowAfter (NonNegative Int)
  deriving Show

instance Arbitrary AskAt where
  arbitrary = oneof [ AfterLast <$> arbitrary, WindowAfter <$> arbitrary ]
  shrink (AfterLast l)   = [ AfterLast l' | l' <- shrink l ]
  shrink (WindowAfter i) = [ WindowAfter i' | i' <- shrink i ]

instance Arbitrary Accepts where
  arbitrary = do
    p <- arbitrary
    n <- choose (0, 30)
    events <- vectorOf n ((,) <$> (Positive <$> choose (1, 2000)) <*> (Positive <$> choose (1, 4)))
    Accepts p events <$> arbitrary
  shrink (Accepts p es a) =
       [ Accepts p' es a | p' <- shrink p ]
    ++ [ Accepts p es' a | es' <- shrink es ]
    ++ [ Accepts p es a' | a' <- shrink a ]

-- | A policy's source window rounded to whole milliseconds, so that the
-- window boundary is reachable by the accept times below.
wholeMsWindow :: TxDecisionPolicy -> TxDecisionPolicy
wholeMsWindow policy =
    policy { txSourceWindow = fromIntegral (max 1 ms) / 1000 }
  where
    ms = round (txSourceWindow policy * 1000) :: Int

-- | The accepting submissions of an 'Accepts' history as (time, count),
-- the gaps in milliseconds summed, up to the asking time, and that time:
-- so many milliseconds after the last submission, or exactly a window
-- after one of them.
acceptsAt :: TxDecisionPolicy -> [(Positive Int, Positive Int)] -> AskAt -> (Time, [(Time, Int)])
acceptsAt policy events askAt = (at, [ (t, c) | (t, c) <- accepts, t <= at ])
  where
    times   = scanl1 (+) [ g | (Positive g, _) <- events ] :: [Int]
    accepts = zip (map ms times) [ c | (_, Positive c) <- events ]
    at = case askAt of
      AfterLast (Positive lag) -> ms (lag + (if null times then 0 else last times))
      WindowAfter (NonNegative i)
        | null times -> ms 0
        | otherwise  -> addTime (txSourceWindow policy) (ms (times !! (i `mod` length times)))
    ms t = Time (fromIntegral (t :: Int) / 1000)

-- | 'isSource' over the states 'recordNovel' builds agrees with the count
-- over the whole history up to the asking time: a source has at least
-- 'txSourceCount' accepts inside the window, and the count is at least
-- one.
prop_tier_sourceOracle :: Accepts -> Property
prop_tier_sourceOracle (Accepts (ArbTxDecisionPolicy policy0) events askAt) =
      cover 20 oracle "source"
    . cover 20 (not oracle && n >= 1) "not a source"
    . cover 10 onBoundary "asked exactly a window after an accept"
    $ isSource policy at folded === oracle
  where
    policy = wholeMsWindow policy0
    n      = txSourceCount policy
    (at, history) = acceptsAt policy events askAt
    onBoundary = case askAt of
      WindowAfter _ -> not (null history)
      AfterLast _   -> False
    folded   = foldl (\acc (t, c) -> recordNovel policy t c acc) StrictSeq.empty history
    inWindow = sum [ c | (t, c) <- history, addTime (txSourceWindow policy) t >= at ]
    oracle   = n >= 1 && inWindow >= n

-- | When a source's record is asked about again, after the asking time:
-- at the deadline itself, or so many milliseconds later, up to half a
-- minute, beyond any window the policies draw.
data AskAgain = AtDeadline | MsLater Int
  deriving Show

instance Arbitrary AskAgain where
  arbitrary = frequency [ (1, pure AtDeadline), (4, MsLater <$> choose (0, 30000)) ]
  shrink AtDeadline   = []
  shrink (MsLater ms) = AtDeadline : [ MsLater ms' | ms' <- shrink ms ]

-- | The deadline 'sourceUntil' gives at the asking time agrees with
-- 'isSource' at every later time, with no accept in between: a source up
-- to the deadline and not after, and never one without a deadline.
prop_tier_sourceUntil :: Accepts -> AskAgain -> Property
prop_tier_sourceUntil (Accepts (ArbTxDecisionPolicy policy0) events askAt) askAgain =
      cover 20 (isJust deadline) "a source when asked"
    $ cover 10 (maybe False (< later) deadline) "lapsed by the later time"
    $ cover 5 (isJust deadline && Just later == deadline) "asked at the deadline"
    $ counterexample ("deadline " ++ show deadline ++ " later " ++ show later)
    $ isSource policy later folded === maybe False (>= later) deadline
  where
    policy = wholeMsWindow policy0
    (at, history) = acceptsAt policy events askAt
    folded   = foldl (\acc (t, c) -> recordNovel policy t c acc) StrictSeq.empty history
    deadline = sourceUntil policy at folded
    later = case askAgain of
      AtDeadline | Just d <- deadline -> d
      AtDeadline                      -> at
      MsLater ms                      -> addTime (fromIntegral ms / 1000) at

-- | The record 'recordNovel' builds over a history of accepting
-- submissions is what the whole history says: one entry per submission
-- time, inside the window at the last one, the newest that still reach
-- 'txSourceCount' and no older; a zero source count records nothing.
prop_tier_recordNovelTrim :: Accepts -> Property
prop_tier_recordNovelTrim (Accepts (ArbTxDecisionPolicy policy) events _) =
  let n       = txSourceCount policy
      times   = scanl1 (+) [ g | (Positive g, _) <- events ] :: [Int]
      history = zip (map ms times) [ c | (_, Positive c) <- events ]
      ms t    = Time (fromIntegral (t :: Int) / 1000)
      folded  = foldl (\acc (t, c) -> recordNovel policy t c acc) StrictSeq.empty history
      -- the oracle: the history merged by time, cut to the window at the
      -- last submission, then to the shortest suffix reaching the count
      merged  = Map.toAscList (Map.fromListWith (+) history)
      inWindow = case history of
        [] -> []
        _  -> [ (t, c) | (t, c) <- merged, addTime (txSourceWindow policy) t >= fst (last history) ]
      suffix xs | sum (map snd (drop 1 xs)) >= n, not (null xs) = suffix (drop 1 xs)
                | otherwise = xs
      expected | n <= 0    = StrictSeq.empty
               | otherwise = StrictSeq.fromList (suffix inWindow)
  in counterexample (show (n, history, folded)) $
       classify (n > 0 && length inWindow < length merged) "cut by the window" $
       classify (n > 0 && StrictSeq.length expected < length inWindow) "cut to the count" $
       folded === expected

-- | A source window: from a tenth of a second to an hour, up to a day, one
-- given in the wrong unit and beyond, or, three times in ten, at the edges
-- the sanity check turns on: zero, a little below it, the bound itself and
-- a picosecond either side of it.
newtype SourceWindow = SourceWindow DiffTime
  deriving Show

instance Arbitrary SourceWindow where
  arbitrary = SourceWindow <$> frequency
    [ (3, realToFrac <$> choose (0.1, 3600 :: Double))
    , (2, realToFrac <$> choose (3600, 86400 :: Double))
    , (1, realToFrac <$> choose (1e7, 1e9 :: Double))
    , (1, realToFrac <$> choose (-10, 0 :: Double))
    , (3, elements [ 0, maxSourceWindow, maxSourceWindow - picosecond, maxSourceWindow + picosecond ])
    ]
    where
      picosecond = picosecondsToDiffTime 1
  shrink (SourceWindow w) =
    [ SourceWindow (realToFrac w') | w' <- shrink (realToFrac w :: Double) ]

-- | The sanity check refuses a source window that is not positive or over
-- 'maxSourceWindow', as one given in the wrong unit is, and a negative
-- source count, and passes a window in @(0, maxSourceWindow]@, the bound
-- itself included, with a count of zero or more.
prop_policy_sourceSane :: ArbTxDecisionPolicy -> SourceWindow -> Int -> Property
prop_policy_sourceSane (ArbTxDecisionPolicy policy0) (SourceWindow window) count =
  let policy = policy0 { txSourceCount = count, txSourceWindow = window }
  in cover 20 (window > maxSourceWindow) "over the window bound" $
     cover 20 (window > 0 && window <= maxSourceWindow) "within the window bound" $
     cover 5 (window <= 0) "a window at or below zero" $
     cover 5 (window == maxSourceWindow) "the bound itself" $
     cover 10 (count < 0) "a negative count" $
     counterexample (show (window, count)) $
       saneTxDecisionPolicy policy defaultTxSubmissionConfigV2
         === (if count < 0 then Left (TxSourceCountTooSmall count)
              else if window <= 0 then Left (TxSourceWindowTooSmall window)
              else if window > maxSourceWindow then Left (TxSourceWindowTooLarge window)
              else Right ())

-- | The counters thread refuses a policy and config that fail the sanity
-- check before it does anything, with the check's own error, and runs on
-- ones that pass.  Each case says what it expects on its own, so the check
-- under test is not the oracle: a generated sane policy runs, each listed
-- corruption of it is refused, and a request cap above the window, which
-- the window's room bounds at use, runs.
prop_countersThread_refusesInsane :: ArbTxDecisionPolicy -> ArbTxSubmissionConfig -> Property
prop_countersThread_refusesInsane (ArbTxDecisionPolicy policy) (ArbTxSubmissionConfig config) =
    forAllBlind (elements (("sane", id, False) : corruptions)) $ \(name, corrupt, refused) ->
      let (policy', config') = corrupt (policy, config)
          outcome = runSimOrThrow $ do
            registry  <- newPeerAddrRegistry (mkStdGen 7)
            sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
            retired   <- newTxSubmissionCountersVar mempty
            try (timeout 1 (txCountersThreadV2 policy' config' nullTracer nullTracer
                                               retired sharedVar registry))
      in label name $ case outcome of
           Left (err :: TxConfigurationError) ->
                  counterexample "refused, but this case expects it to run" refused
             .&&. counterexample "the exception is not the check's own"
                    (Left err === saneTxDecisionPolicy policy' config')
           Right _ ->
             counterexample "ran, but this case expects a refusal" (not refused)
  where
    corruptions :: [( String
                    , (TxDecisionPolicy, TxSubmissionConfig)
                      -> (TxDecisionPolicy, TxSubmissionConfig)
                    , Bool )]
    corruptions =
      [ ("jitter one",       onPolicy (\p -> p { txSlowJitter = 1 }),                        True)
      , ("jitter above one", onPolicy (\p -> p { txSlowJitter = 1.5 }),                      True)
      , ("negative cadence", onPolicy (\p -> p { txSlowCadence = -1 }),                      True)
      , ("no rotation",      onPolicy (\p -> p { txTierRotation = 0 }),                      True)
      , ("no source window", onPolicy (\p -> p { txSourceWindow = 0 }),                      True)
      , ("negative source count", onPolicy (\p -> p { txSourceCount = -1 }),                 True)
      , ("source window in the wrong unit", onPolicy (\p -> p { txSourceWindow = 10000000 }), True)
      , ("empty window",     onConfig (\c -> c { maxNumUnacknowledgedTxIds = 0 }),           True)
      , ("no cap",           onConfig (\c -> c { maxNumTxIdsToRequest = 0 }),                True)
      , ("timeout below the space",
         onPolicy (\p -> p { inflightTimeout = interTxSpace p - 0.001 }), True)
      , ("cap above window", onConfig (\c -> c { maxNumTxIdsToRequest =
                                         NumTxIdsToReq
                                           (getNumTxIdsToAck (maxNumUnacknowledgedTxIds c) + 1) })
                             , False)
      ]
    onPolicy f (p, c) = (f p, c)
    onConfig f (p, c) = (p, f c)

-- | A peer leaving frees its fast slot at once, through the release of
-- 'withPeer': with one coin slot and two peers in the order, after either
-- one leaves the other decides on the fast tier, whichever of them the
-- order had put first; while both are in, exactly one is fast.
prop_withPeer_leaveFreesSlot :: Int -> Property
prop_withPeer_leaveFreesSlot seed = runSimOrThrow $ do
    outcomes <- forM [(1, 2), (2, 1)] $ \(outer, inner) -> do
      registry  <- newPeerAddrRegistry (mkStdGen seed)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      let run addr = withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
                              sharedVar registry retired (addr :: PeerAddr)
          tierOf api = peerTier . snd <$> runNextPeerAction api (Time 1) Nothing
                                             emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed api }
      run outer $ \apiOuter -> do
        joinTiers apiOuter
        both <- run inner $ \apiInner -> do
          joinTiers apiInner
          (,) <$> tierOf apiOuter <*> tierOf apiInner
        -- the inner peer has left: its release took it out of the order
        afterLeave <- tierOf apiOuter
        pure (outer, both, afterLeave)
    pure $ conjoin
      [ counterexample (show o) $
             counterexample "exactly one fast while both are in"
               (length [ () | FastTier <- [tOuter, tInner] ] === 1)
        .&&. counterexample "the remaining peer is fast after the other left" (after === FastTier)
      | o@(_, (tOuter, tInner), after) <- outcomes ]
  where
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
               { txFastPeers = 1, txSourceCount = 0 }

-- | A script of bumps of a counter of the shared state, each a write
-- through the registry, which moves the counter and its mirror together.
newtype BumpScript = BumpScript [Positive Word64]
  deriving Show

instance Arbitrary BumpScript where
  arbitrary = BumpScript <$> listOf (Positive <$> choose (1, 100))
  shrink (BumpScript bumps) = BumpScript <$> shrinkList shrink bumps

-- | Run a script against a counter of the shared state, given how a bump
-- moves it.
runBumpScript :: PeerTxRegistry (IOSim s) PeerAddr
              -> SharedTxStateVar (IOSim s) PeerAddr TxId
              -> (Word64 -> SharedTxState PeerAddr TxId -> SharedTxState PeerAddr TxId)
              -> [Positive Word64]
              -> IOSim s ()
runBumpScript registry sharedVar bump = mapM_ step
  where
    step (Positive n) = atomically $ do
      st <- readTVar sharedVar
      writeSharedStateIfChanged (registryMirrors registry) sharedVar (sharedRevision st)
        (bump n st) { sharedRevision = sharedRevision st + 1 }

-- | A bump of one counter of the shared state by one, through the
-- registry.
bumpCounter :: PeerTxRegistry (IOSim s) PeerAddr
            -> SharedTxStateVar (IOSim s) PeerAddr TxId
            -> ParkCounter
            -> IOSim s ()
bumpCounter registry sharedVar counter = runBumpScript registry sharedVar bump [Positive 1]
  where
    bump n st = case counter of
      WakeCounter      -> st { sharedGeneration = sharedGeneration st + n }
      ClaimableCounter -> st { sharedClaimable  = sharedClaimable  st + n }

-- | Whether a park taken at a value wakes before its delay of a second
-- when, half a second in, a counter is bumped through the registry: it
-- should on a bump of its own counter, and not on one of the other.
wakesOnBump :: PeerTxAPI (IOSim s) TxId (Tx TxId)
            -> PeerTxRegistry (IOSim s) PeerAddr
            -> SharedTxStateVar (IOSim s) PeerAddr TxId
            -> ParkCounter
            -> Word64
            -> ParkCounter
            -> IOSim s Bool
wakesOnBump api registry sharedVar parkedOn seen bumped =
    withAsync park $ \parked -> do
      threadDelay 0.5
      bumpCounter registry sharedVar bumped
      Async.wait parked
  where
    park = do
      start <- getMonotonicTime
      delay <- RegisteredDelay.new 1
      atomically (awaitPark api parkedOn seen (Just delay))
      woke <- getMonotonicTime
      pure (woke `diffTime` start < 1)

-- | The counter a bump is of: the park's own, or the other.
data BumpOf = OwnCounter | OtherCounter
  deriving (Eq, Show)

instance Arbitrary BumpOf where
  arbitrary = elements [OwnCounter, OtherCounter]
  shrink OtherCounter = [OwnCounter]
  shrink OwnCounter   = []

-- | A gated slow peer parks on the claimable counter, past its value as
-- the deciding transaction read it, which after any script of writes
-- through the registry, the initial value included, is the counter's; a
-- bump of that counter wakes the park at once, a bump of the wake counter
-- leaves it parked until its delay.
prop_withPeer_gatedParkOnClaimable :: Int -> Word64 -> BumpScript -> BumpOf -> Property
prop_withPeer_gatedParkOnClaimable seed claimable0 (BumpScript script) bumpOf =
    cover 30 (bumpOf == OtherCounter) "the other counter bumped" $
    runSimOrThrow $ do
      registry  <- newPeerAddrRegistry (mkStdGen seed)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      let bumpBy n st = st { sharedClaimable = sharedClaimable st + n }
      runBumpScript registry sharedVar bumpBy [ Positive claimable0 | claimable0 > 0 ]
      withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
               sharedVar registry retired (1 :: PeerAddr) $ \api -> do
        joinTiers api
        let ps0 = emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed api }
        (first, ps1) <- runNextPeerAction api (Time 1) Nothing ps0
        runBumpScript registry sharedVar bumpBy script
        (second, _) <- runNextPeerAction api (Time 1.001) Nothing ps1
        counter <- sharedClaimable <$> readTVarIO sharedVar
        case (first, second) of
          (ResolvedRequestTxIds 0 _, ResolvedDoNothing (Just _) ClaimableCounter seen) -> do
            woke <- wakesOnBump api registry sharedVar ClaimableCounter seen
                      (if bumpOf == OwnCounter then ClaimableCounter else WakeCounter)
            pure $ counterexample (unwords ["first", describeResolved first, "second", describeResolved second]) $
                   counterexample "the park's value is the counter's" (seen === counter)
              .&&. counterexample ("woke " ++ show woke) (woke === (bumpOf == OwnCounter))
          _ -> pure $ counterexample (unwords ["first", describeResolved first, "second", describeResolved second])
                    $ counterexample "expected a blocking request, then a gated park" False
  where
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2)
               { txFastPeers = 0, txSourceCount = 0, txSlowCadence = 10, txSlowJitter = 0 }

describeResolved :: ResolvedPeerAction TxId (Tx TxId) -> String
describeResolved (ResolvedRequestTxIds a r) = "request" ++ show (a, r)
describeResolved (ResolvedDoNothing d c s)  = "doNothing" ++ show (d, c, s)
describeResolved (ResolvedRequestTxs m)     = "requestTxs" ++ show (Map.keys m)
describeResolved (ResolvedSubmitTxs txs)    = "submit" ++ show (length txs)

-- | A fast peer with nothing to do parks on the wake counter, past its
-- value as the deciding transaction read it, which after any script of
-- writes through the registry, the initial value included, is the
-- counter's; a bump of that counter wakes the park at once, a bump of the
-- claimable counter leaves it parked until its delay.
prop_withPeer_fastParkOnWake :: Int -> Word64 -> BumpScript -> BumpOf -> Property
prop_withPeer_fastParkOnWake seed generation0 (BumpScript script) bumpOf =
    cover 30 (bumpOf == OtherCounter) "the other counter bumped" $
    runSimOrThrow $ do
      -- a local root: fast by standing, so its parks are on the wake counter
      registry  <- newPeerTxRegistry (mkStdGen seed) (pure (const True))
      sharedVar <- newSharedTxStateVar st0
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      let bumpBy n st = st { sharedGeneration = sharedGeneration st + n }
      runBumpScript registry sharedVar bumpBy [ Positive generation0 | generation0 > 0 ]
      withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
               sharedVar registry retired (1 :: PeerAddr) $ \api -> do
        joinTiers api
        -- it advertises the one txid, whose entry is leased elsewhere:
        -- nothing to ack, no body to request, a park
        ps0 <- applyReceivedTxIds api (Time 1) Nothing 1 [(7, 100)]
                 emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed api, peerRequestedTxIds = 1 }
        (first, ps1) <- runNextPeerAction api (Time 1) Nothing ps0
        runBumpScript registry sharedVar bumpBy script
        (second, _) <- runNextPeerAction api (Time 1.001) Nothing ps1
        counter <- sharedGeneration <$> readTVarIO sharedVar
        case (first, second) of
          (ResolvedDoNothing _ WakeCounter seen0, ResolvedDoNothing _ WakeCounter seen) -> do
            woke <- wakesOnBump api registry sharedVar WakeCounter seen
                      (if bumpOf == OwnCounter then WakeCounter else ClaimableCounter)
            pure $ counterexample (unwords ["first", describeResolved first, "second", describeResolved second]) $
                   counterexample "the first park's value is the counter's then" (seen0 === generation0)
              .&&. counterexample "the second park's value is the counter's" (seen === counter)
              .&&. counterexample ("woke " ++ show woke) (woke === (bumpOf == OwnCounter))
          _ -> pure $ counterexample (unwords ["first", describeResolved first, "second", describeResolved second])
                    $ counterexample "expected two parks on the wake counter" False
  where
    policy = (unsafeMkTxDecisionPolicy defaultTxSubmissionConfigV2) { txSourceCount = 0 }
    st0 = emptySharedTxState
      { sharedTxTable = IntMap.singleton 0 TxEntry
          { txLease = TxLeased 2 (Time 100), txAttempt = 1, txAdvertisers = 0
          , txInSubmission = False, currentMaxInflightMultiplicity = 1 }
      , sharedTxIdToKey = Map.singleton (getRawTxId (7 :: TxId)) (TxKey 0)
      , sharedKeyToTxId = IntMap.singleton 0 7
      , sharedNextTxKey = 1
      } :: SharedTxState PeerAddr TxId

-- | One step of a tier script over a few peer ids: a peer registering and
-- taking its first decision, registering only, deciding later, leaving,
-- or a rotation.  Ids repeat, so leaves find peers.
data TierEvent
  = TierJoin Int Bool
  | TierRegister Int Bool
  | TierStart Int
  | TierLeave Int
  | TierRotate
  deriving (Eq, Show)

newtype TierScript = TierScript [TierEvent]
  deriving Show

instance Arbitrary TierScript where
  arbitrary = TierScript <$> listOf event
    where
      peer  = choose (0, 7 :: Int)
      root  = frequency [(3, pure False), (1, pure True)]
      event = frequency
        [ (4, TierJoin <$> peer <*> root)
        , (1, TierRegister <$> peer <*> root)
        , (1, TierStart <$> peer)
        , (3, TierLeave <$> peer)
        , (1, pure TierRotate)
        ]
  shrink (TierScript es) = TierScript <$> shrinkList shrinkEvent es
    where
      shrinkEvent (TierJoin p r)     = [ TierJoin p' r | p' <- shrink p ] ++ [ TierJoin p False | r ]
      shrinkEvent (TierRegister p r) = [ TierRegister p' r | p' <- shrink p ] ++ [ TierRegister p False | r ]
      shrinkEvent (TierStart p)      = [ TierStart p' | p' <- shrink p ]
      shrinkEvent (TierLeave p)      = [ TierLeave p' | p' <- shrink p ]
      shrinkEvent TierRotate         = []

-- | The placement against the exact oracle after every step of a script:
-- the order holds exactly the peers that have decided; every root in it
-- is fast; of the others exactly the first k by salted rank are, the rank
-- recomputed here from the salt the registry holds; a peer that has not
-- decided is slow; and a rotation changes the salt.
prop_tier_orderOracle :: ArbTxDecisionPolicy -> NonNegative Int -> TierScript -> Int -> Property
prop_tier_orderOracle (ArbTxDecisionPolicy policy0) (NonNegative k0) (TierScript events) seed =
    runSimOrThrow $ do
      registry <- noRootsRegistry (mkStdGen seed)
      (checks, backfills) <- go registry Map.empty events
      pure $ cover 30 (backfills > 0) "a fast peer left a full order"
           $ conjoin checks
  where
    k      = k0 `mod` 5
    policy = policy0 { txFastPeers = k }

    -- live peers by id: registration, entry, root flag, decided yet
    go _ _ [] = pure ([], 0 :: Int)
    go registry live (ev : rest) = do
      (live', backfill, extra) <- step registry live ev
      verdict <- oracle registry live'
      (checks, backfills) <- go registry live' rest
      pure (counterexample (show ev) (extra .&&. verdict) : checks, backfill + backfills)

    step registry live ev = case ev of
      TierJoin p root     -> register registry live p root >>= start registry p
      TierRegister p root -> (\live' -> (live', 0, property True)) <$> register registry live p root
      TierStart p         -> start registry p live
      TierLeave p         -> case Map.lookup p live of
        Nothing -> pure (live, 0, property True)
        Just (r, e, _, _) -> do
          wasFast <- (== FastTier) . ptsCoin <$> readTVarIO (pteState e)
          inOrder <- Map.size <$> readTVarIO (peerTiers registry)
          atomically (leaveTier policy registry r)
          pure (Map.delete p live, if wasFast && inOrder > k then 1 else 0, property True)
      TierRotate -> do
        before <- saltKey registry
        atomically (rotateTiers policy registry)
        after <- saltKey registry
        pure (live, 0, counterexample "salt unchanged" (before =/= after))

    register registry live p root
      | Map.member p live = pure live
      | otherwise = do
          c <- newTVarIO mempty
          (r, e) <- atomically $ do
            r <- registerPeerCounters registry p c
            (e, _) <- registerPeerTier registry
            pure (r, e)
          pure (Map.insert p (r, e, root, False) live)

    start registry p live = case Map.lookup p live of
      Just (r, e, root, False) -> do
        atomically (joinTier policy registry r e root)
        pure (Map.insert p (r, e, root, True) live, 0, property True)
      _ -> pure (live, 0, property True)

    -- the salt, as the hash it gives a fixed key: distinct salts hash it apart
    saltKey registry = (\salt -> hashWithSalt salt (0 :: Int)) <$> readTVarIO (registrySalt registry)

    oracle registry live = do
      tiers <- readTVarIO (peerTiers registry)
      salt  <- readTVarIO (registrySalt registry)
      rows  <- forM (Map.toList live) $ \(p, (r, e, root, decided)) -> do
        -- the tier a decision is served on, read as a decision reads it
        served <- peerTier <$> atomically (withTier policy (Time 1) e (emptyPeerTxLocalState :: PeerTxLocalState ()))
        coin <- ptsCoin <$> readTVarIO (pteState e)
        pure (p, r, root, decided, coin, served)
      let decidedSet = Set.fromList [ r | (_, r, _, True, _, _) <- rows ]
          others     = [ r | (_, r, False, True, _, _) <- rows ]
          rank (addr, n) = (fst (nextWord64 (mkSMGen (fromIntegral (hashWithSalt salt addr)))), n)
          fastSet    = Set.fromList (take k (sortOn rank others))
          -- the coin is the order's verdict alone: a root is outside the order
          expectedCoin (_, r, root, decided, _, _)
            | not decided || root  = SlowTier
            | Set.member r fastSet = FastTier
            | otherwise            = SlowTier
          -- served: a root fast by standing once its join read the flag,
          -- the others by their coin
          expectedServed row@(_, _, root, decided, _, _)
            | root && decided = FastTier
            | otherwise       = expectedCoin row
      pure $ conjoin
        [ counterexample "the order holds the decided peers" (Map.keysSet tiers === decidedSet)
        , conjoin [ counterexample ("peer " ++ show p ++ " root " ++ show root ++ " decided " ++ show decided ++ " coin")
                                   (coin === expectedCoin row)
                    .&&. counterexample ("peer " ++ show p ++ " root " ++ show root ++ " decided " ++ show decided ++ " served")
                                   (served === expectedServed row)
                  | row@(p, _, root, decided, coin, served) <- rows ]
        ]

-- | The salted order is fair: over many salts every key is among the first
-- k about k/n of the time, whatever the keys' own hashes do.  Two thousand
-- salts put the standard error below 0.012, so the 0.1 band is nine of
-- them.
prop_tier_orderUniform :: Int -> Property
prop_tier_orderUniform seed0 =
    forAll (choose (2, 6)) $ \n ->
    forAll (choose (1, n - 1)) $ \k ->
      let keys   = [ (key, 0) | key <- [0 .. n - 1] ] :: [(Int, Word64)]
          draws  = 2000 :: Int
          salts  = take draws (unfoldr (Just . mkSalt) (mkStdGen seed0))
          counts = foldl (\acc salt -> foldr (\(key, _) -> Map.insertWith (+) key (1 :: Int)) acc
                                            (orderFast k salt keys))
                         Map.empty salts
          expected = fromIntegral k / fromIntegral n :: Double
      in conjoin
           [ counterexample ("key " ++ show key ++ " fast " ++ show c ++ " of " ++ show draws)
               (abs (fromIntegral c / fromIntegral draws - expected) < 0.1)
           | (key, _) <- keys
           , let c = Map.findWithDefault 0 key counts
           ]

-- | The slow wait lies within the jitter band, and the generator moves.
prop_tier_jitterBand :: ArbTxDecisionPolicy -> Word64 -> Property
prop_tier_jitterBand (ArbTxDecisionPolicy policy) seed =
    conjoin
      [ counterexample ("u " ++ show u) (u >= 0 && u < 1)
      , counterexample "seed unchanged" (seed' =/= seed)
      , counterexample ("wait " ++ show wait) (wait >= lo - 1e-9 && wait <= hi + 1e-9)
      ]
  where
    (u, seed') = nextJitter seed
    wait = slowWait policy u
    lo = txSlowCadence policy * realToFrac (1 - txSlowJitter policy)
    hi = txSlowCadence policy * realToFrac (1 + txSlowJitter policy)

-- | The first decisions fill the fast tier to 'txFastPeers' and no
-- further and a rotation keeps exactly min(k, n) peers fast; a peer
-- leaving a fast slot hands it to the next in the order at once, before
-- any arrival; a peer that registered but has not decided holds nothing,
-- and takes its place at its first decision.
prop_tier_registry :: ArbTxDecisionPolicy -> NonNegative Int -> Int -> Property
prop_tier_registry (ArbTxDecisionPolicy policy) (NonNegative n0) seed =
    runSimOrThrow $ do
      registry <- noRootsRegistry (mkStdGen seed)
      regs <- mapM (joinPeer registry) [1 .. n]
      placed <- fastCount registry
      atomically (rotateTiers policy registry)
      rotated <- fastCount registry
      fastRegs <- filterM (\(_, v) -> (== FastTier) . ptsCoin <$> readTVarIO v) regs
      churn <- case fastRegs of
        [] -> pure []
        ((r, _) : _) -> do
          atomically (leaveTier policy registry r)
          refilled <- fastCount registry
          (r', e') <- register registry (n + 1)
          waiting <- fastCount registry
          atomically (joinTier policy registry r' e' False)
          decided <- fastCount registry
          pure [ counterexample ("refilled " ++ show refilled) (refilled === min k (n - 1))
               , counterexample ("waiting "  ++ show waiting)  (waiting  === min k (n - 1))
               , counterexample ("decided "  ++ show decided)  (decided  === min k n)
               ]
      pure $ conjoin $
        [ counterexample ("placed " ++ show placed)   (placed   === min k n)
        , counterexample ("rotated " ++ show rotated) (rotated  === min k n)
        ] ++ churn
  where
    n = n0 `mod` 50
    k = txFastPeers policy
    register registry p = do
      c <- newTVarIO mempty
      atomically $ do
        r <- registerPeerCounters registry (p :: Int) c
        (e, _) <- registerPeerTier registry
        pure (r, e)
    joinPeer registry p = do
      (r, e) <- register registry p
      atomically (joinTier policy registry r e False)
      pure (r, pteState e)
    fastCount registry = do
      tiers <- readTVarIO (peerTiers registry)
      length . filter ((== FastTier) . ptsCoin)
        <$> mapM (readTVarIO . pteState) (Map.elems tiers)


-- | Every decision of a stream run: the time, the action, the local state
-- after it and the shared counter it saw; and the final local state.
runStreamTrace :: TxDecisionPolicy
               -> PeerTxLocalState (Tx TxId)
               -> [Positive Int]
               -> ([(Time, PeerAction, PeerTxLocalState (Tx TxId), Word64)], PeerTxLocalState (Tx TxId))
runStreamTrace policy peerState0 = go (Time 0) peerState0 emptyPeerTxInFlight emptySharedTxState 1
  where
    peer = 0 :: PeerAddr
    go _ ps _ _ _ [] = ([], ps)
    go t ps pif ss nextId (Positive ms : rest) =
      let later = addTime (fromIntegral ms / 1000) t
          (action, ps', pif', ss') =
            nextPeerAction t policy defaultTxSubmissionConfigV2 peer ps pif ss
          step = (t, action, ps', sharedClaimable ss')
      in case action of
           PeerRequestTxIds _ req ->
             let n   = fromIntegral req :: Int
                 ids = [ (txid, 100) | txid <- [nextId .. nextId + n - 1] ]
                 (ps'', pif'', ss'') = handleReceivedTxIds (const True) t policy req ids ps' pif' ss'
             in step `onto` go later ps'' pif'' ss'' (nextId + n) rest
           _ -> step `onto` go later ps' pif' ss' nextId rest

    onto :: a -> ([a], b) -> ([a], b)
    onto s (steps, final) = (s : steps, final)

-- | A 'PeerDoNothing' chosen while the gate holds a txid request parks on
-- the claimable counter; any other park is on the wake counter.  The
-- stream never buffers a body, so every gated park is a claimable one.
prop_tier_gateParksOnClaimable :: SlowStream -> Property
prop_tier_gateParksOnClaimable (SlowStream (ArbTxDecisionPolicy policy0) seed steps) =
      cover 30 (any gated trace) "a gated park"
    $ conjoin
        [ counterexample (show (t, action, c)) $
            case action of
              PeerDoNothing _ park | gateHolds t ps -> park === ClaimableCounter
              PeerDoNothing _ park                  -> park === WakeCounter
              _                                     -> property True
        | (t, action, ps, c) <- trace
        ]
  where
    policy = policy0 { txSlowCadence = max 0.05 (txSlowCadence policy0), txSourceCount = 0 }
    (trace, _) = runStreamTrace policy emptyPeerTxLocalState { peerTier = SlowTier, peerJitterSeed = seed } steps
    gateHolds t ps = maybe False (> t) (peerSlowGate ps)
    gated (t, PeerDoNothing {}, ps, _) = gateHolds t ps
    gated _                            = False

-- | The gate holds txid requests, and parks a slow peer with nothing to
-- do.  With the gate open the decision is whatever it is; with it
-- holding, a txid request becomes a park until the gate opens or an
-- earlier wake, on the claimable counter when the peer buffers no body
-- after the decision (bodies of resolved entries are dropped on the way
-- in) and on the generation when it does; a peer with nothing to do and no
-- buffered body parks the same way, rather than on the generation that
-- moves with every accept; and every other decision, a peer holding a
-- buffered body with nothing to do included, is the same as with the gate
-- open.
prop_tier_gateHoldsOnlyTxIdRequests
  :: ArbTxDecisionPolicy
  -> ArbPeerTxLocalState
  -> ArbSharedTxState
  -> Positive Int
  -> Property
prop_tier_gateHoldsOnlyTxIdRequests (ArbTxDecisionPolicy policy0)
                                    (ArbPeerTxLocalState ps0)
                                    (ArbSharedTxState ss)
                                    (Positive ms) =
      cover 20 (isRequest open) "gate held a txid request"
    $ cover 5 (isRequest open && buffered) "held with a buffered body"
    $ cover 5 (isIdle open && not buffered) "idle without a body, parked"
    $ cover 5 (isIdle open && buffered) "idle with a body, kept the generation"
    $ counterexample ("open:  " ++ show open ++ "\ngated: " ++ show gated)
    $ case open of
        PeerRequestTxIds {}             -> parked
        PeerDoNothing {} | not buffered -> parked
        _                               -> gated === open
  where
    parked = case gated of
      PeerDoNothing (Just d) park ->
        conjoin
          [ counterexample ("wake " ++ show d ++ " past the gate " ++ show gateDelay)
              (d <= gateDelay + 1e-9)
          , counterexample "park"
              (park === if buffered then WakeCounter else ClaimableCounter)
          ]
      _ -> counterexample "not a park" False
    isIdle PeerDoNothing {} = True
    isIdle _                = False
    policy    = policy0 { txSourceCount = 0 }
    t0        = Time 10
    gateDelay = fromIntegral ms / 1000
    ps        = ps0 { peerTier     = SlowTier
                    , peerNovelAt  = StrictSeq.empty
                    , peerSlowGate = Just (gateDelay `addTime` t0)
                    }
    buffered  = not (IntMap.null (peerDownloadedTxs psGated))
    peer      = 1 :: PeerAddr
    (open,  _, _, _)       = nextPeerAction t0 policy defaultTxSubmissionConfigV2 peer
                                            ps { peerSlowGate = Nothing } emptyPeerTxInFlight ss
    (gated, psGated, _, _) = nextPeerAction t0 policy defaultTxSubmissionConfigV2 peer
                                            ps emptyPeerTxInFlight ss
    isRequest PeerRequestTxIds {} = True
    isRequest _                   = False

-- | Local roots are fast by standing and hold none of the coin's slots:
-- after the joins and after a rotation every root is fast and exactly
-- min(k, n) of the others are; roots that lose their standing are placed
-- at the next refresh, like the rest from then on, before any rotation
-- and without a decision of their own.  The registry's census says the
-- same at each step, with no source among peers that never delivered.
prop_tier_rootsOutsideCoin :: ArbTxDecisionPolicy -> [Bool] -> Int -> Property
prop_tier_rootsOutsideCoin (ArbTxDecisionPolicy policy) roots0 seed =
    cover 30 (nRoots > 0 && n > k) "roots beside a full coin" $
    runSimOrThrow $ do
      rootsVar <- newTVarIO (Set.fromList [ p | (p, True) <- zip [1 ..] roots ])
      registry <- newPeerTxRegistry (mkStdGen seed) (flip Set.member <$> readTVar rootsVar)
      regs <- forM (zip [1 ..] roots) $ \(p, isRoot) -> do
        c <- newTVarIO mempty
        atomically $ do
          r <- registerPeerCounters registry (p :: Int) c
          (e, _) <- registerPeerTier registry
          joinTier policy registry r e isRoot
          pure (p, e)
      placed <- census rootsVar regs
      placedCensus <- atomically (tierCensus t0 registry)
      atomically (rotateTiers policy registry)
      rotated <- census rootsVar regs
      rotatedCensus <- atomically (tierCensus t0 registry)
      -- the roots stop being roots: the next refresh places the tiers
      -- with them among the candidates, so the coin's slots go to the
      -- first k of all n, before any rotation
      atomically (writeTVar rootsVar Set.empty)
      refreshTierRoots policy registry
      replaced <- census rootsVar regs
      replacedCensus <- atomically (tierCensus t0 registry)
      atomically (rotateTiers policy registry)
      demoted <- census rootsVar regs
      demotedCensus <- atomically (tierCensus t0 registry)
      pure $ conjoin
        [ counterexample ("placed "  ++ show placed)  (placed  === (nRoots, min k nOthers))
        , counterexample ("rotated " ++ show rotated) (rotated === (nRoots, min k nOthers))
        , counterexample ("replaced " ++ show replaced) (replaced === (0, min k n))
        , counterexample ("demoted " ++ show demoted) (demoted === (0, min k n))
        , counterexample "placed census"  (placedCensus  === expectedCensus nRoots nOthers)
        , counterexample "rotated census" (rotatedCensus === expectedCensus nRoots nOthers)
        , counterexample "replaced census" (replacedCensus === expectedCensus 0 n)
        , counterexample "demoted census" (demotedCensus === expectedCensus 0 n)
        ]
  where
    expectedCensus r o = TierCensus { censusFastByRoot   = r
                                    , censusFastByCoin   = min k o
                                    , censusFastBySource = 0
                                    , censusSlow         = o - min k o }
    t0      = Time 1
    roots   = take 50 roots0
    n       = length roots
    nRoots  = length (filter id roots)
    nOthers = n - nRoots
    k       = txFastPeers policy
    -- (fast roots, fast others) by the tier each peer is served on at a
    -- decision, against what the governor says now
    census rootsVar regs = do
      roots' <- readTVarIO rootsVar
      pairs <- forM regs $ \(p, e) -> do
        ps <- atomically (withTier policy t0 e (emptyPeerTxLocalState :: PeerTxLocalState ()))
        pure (Set.member p roots', peerTier ps)
      pure ( length [ () | (True,  FastTier) <- pairs ]
           , length [ () | (False, FastTier) <- pairs ] )

-- | A push of a request's tier or a pop by a reply.
data TierOp = Push PeerTier | Pop
  deriving Show

instance Arbitrary TierOp where
  arbitrary = frequency [ (2, Push <$> elements [FastTier, SlowTier]), (1, pure Pop) ]
  shrink (Push SlowTier) = [Push FastTier, Pop]
  shrink (Push FastTier) = [Pop]
  shrink Pop             = []

-- | The request tiers are a queue: a reply takes the tiers in the order the
-- requests recorded them, a fast tier when none is outstanding, and what is
-- left is what was pushed and not taken.
prop_requestTiers_fifo :: [TierOp] -> Property
prop_requestTiers_fifo ops =
    cover 30 (length [ () | Pop <- ops ] >= 2) "two replies" $
    go ops [] (emptyPeerTxLocalState :: PeerTxLocalState ())
  where
    go [] model st = counterexample "left over" (toList (peerRequestTiers st) === model)
    go (Push t : rest) model st = go rest (model ++ [t]) (recordRequestTier t st)
    go (Pop : rest) model st =
      let (t, st') = takeReplyTier st
          (expected, model') = case model of
            m : ms -> (m, ms)
            []     -> (FastTier, [])
      in counterexample ("took " ++ show t ++ ", expected " ++ show expected) (t === expected)
         .&&. go rest model' st'

-- | Each reply's txids are counted on the tier of the oldest outstanding
-- request, whatever the later requests went out on, and every reply takes
-- its request's tier off the queue.
prop_requestTiers_counted :: ArbTxDecisionPolicy -> NonEmptyList (Bool, NonNegative Int) -> Property
prop_requestTiers_counted (ArbTxDecisionPolicy policy0) (NonEmpty replies0) =
    cover 30 (length replies >= 2 && any fst replies && not (all fst replies)) "slow and fast outstanding" $
    runSimOrThrow $ do
      registry  <- noRootsRegistry (mkStdGen 5)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      final <- withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
                        sharedVar registry retired (0 :: PeerAddr) $ \api ->
        foldM (\ps (i, (_, n)) ->
                 applyReceivedTxIds api (Time (fromIntegral i)) Nothing (fromIntegral n) (idsOf i n)
                                    ps { peerRequestedTxIds = fromIntegral n })
              emptyPeerTxLocalState { peerRequestTiers = StrictSeq.fromList tiers }
              (zip [1 :: Int ..] replies)
      c <- snapshotCountersWith atomically retired registry
      pure $ counterexample ("slow ids " ++ show (txIdsReceivedSlow c)) (txIdsReceivedSlow c === expectedSlow)
        .&&. counterexample ("tiers left " ++ show (peerRequestTiers final)) (StrictSeq.null (peerRequestTiers final))
  where
    policy       = policy0 { txSourceCount = 0 }
    replies      = [ (slow, n `mod` 13) | (slow, NonNegative n) <- take 8 replies0 ]
    tiers        = [ if slow then SlowTier else FastTier | (slow, _) <- replies ]
    idsOf i n    = [ (i * 100 + j, 100) | j <- [1 .. n] ] :: [(TxId, SizeInBytes)]
    expectedSlow = sum [ fromIntegral n | (True, n) <- replies ]

-- | A decision that sends a txid request records the tier it is served on:
-- a local root's as fast, a lone peer's as slow when the coin has no slots.
prop_requestTier_recorded :: ArbTxDecisionPolicy -> Bool -> Property
prop_requestTier_recorded (ArbTxDecisionPolicy policy0) isRoot =
    runSimOrThrow $ do
      registry  <- newPeerTxRegistry (mkStdGen 7) (pure (const isRoot))
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
               sharedVar registry retired (0 :: PeerAddr) $ \api -> do
        joinTiers api
        (action, ps) <- runNextPeerAction api (Time 1) Nothing
                          emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed api }
        let requested = case action of
              ResolvedRequestTxIds {} -> True
              _                       -> False
        pure $ counterexample "not a txid request" requested
          .&&. counterexample ("tiers " ++ show (peerRequestTiers ps))
                 (peerRequestTiers ps === StrictSeq.fromList [if isRoot then FastTier else SlowTier])
  where
    policy = policy0 { txFastPeers = 0, txSourceCount = 0 }

-- | The counters thread runs the coin: the salt is the boot one until a
-- rotation period has passed, and another one a sweep interval after.
prop_countersThread_rotates :: ArbTxDecisionPolicy -> Positive Int -> Int -> Property
prop_countersThread_rotates (ArbTxDecisionPolicy policy0) (Positive tenths) seed =
    runSimOrThrow $ do
      registry  <- noRootsRegistry (mkStdGen seed)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      salt0 <- saltKey registry
      withAsync (txCountersThreadV2 policy defaultTxSubmissionConfigV2 nullTracer nullTracer
                                    retired sharedVar registry) $ \_ -> do
        threadDelay (period / 2)
        salt1 <- saltKey registry
        threadDelay (period / 2 + sweepInterval policy + 0.001)
        salt2 <- saltKey registry
        pure $ counterexample "rotated before a period" (salt1 === salt0)
          .&&. counterexample "not rotated a period and a sweep later" (salt2 =/= salt0)
  where
    period = fromIntegral (tenths `mod` 50 + 1) / 10 :: DiffTime
    policy = policy0 { txTierRotation = period }
    saltKey registry = (\salt -> hashWithSalt salt (0 :: Int)) <$> readTVarIO (registrySalt registry)

-- | The counters thread keeps the root flags: the flag is as the join
-- read it until the next sweep, whatever the governor says meanwhile;
-- a sweep later a peer that stopped being a root is served by its coin
-- and one that became a root fast by standing, with no decision of the
-- peer's own in between.
prop_countersThread_refreshesRoots :: ArbTxDecisionPolicy -> Bool -> Int -> Property
prop_countersThread_refreshesRoots (ArbTxDecisionPolicy policy0) rootAtJoin seed =
    runSimOrThrow $ do
      rootVar   <- newTVarIO rootAtJoin
      registry  <- newPeerTxRegistry (mkStdGen seed) (const <$> readTVar rootVar)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      withAsync (txCountersThreadV2 policy defaultTxSubmissionConfigV2 nullTracer nullTracer
                                    retired sharedVar registry) $ \_ ->
        withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
                 sharedVar registry retired (0 :: PeerAddr) $ \api -> do
          joinTiers api
          atomically (writeTVar rootVar (not rootAtJoin))
          joined <- flag registry
          threadDelay (sweepInterval policy + 0.001)
          refreshed <- flag registry
          (_, ps) <- runNextPeerAction api (Time 2) Nothing
                       emptyPeerTxLocalState { peerJitterSeed = apiJitterSeed api }
          pure $ conjoin
            [ counterexample "as the join read it, until the sweep" (joined === rootAtJoin)
            , counterexample "as the governor says, a sweep later" (refreshed === not rootAtJoin)
            , counterexample ("served " ++ show (peerTier ps))
                (peerTier ps === if rootAtJoin then SlowTier else FastTier)
            ]
  where
    -- no coin slot and no source rule: a root is fast, the rest slow
    policy = policy0 { txFastPeers = 0, txSourceCount = 0 }
    -- the one peer's root flag as the registry keeps it
    flag registry = do
      tiers <- readTVarIO (peerTiers registry)
      case Map.elems tiers of
        [e] -> ptsRoot <$> readTVarIO (pteState e)
        es  -> error ("peers in the order: " ++ show (length es))

-- | A mark in the sim trace around a decision.
data DecisionMark = DecisionMark
  deriving (Eq, Show)

-- | A decision is one transaction, the tier read and its write inside it:
-- between the marks around a decision the deciding thread commits exactly
-- once, pipelined or not, root or not, a source or not, with however many
-- advertised txids to work on; and the entry carries a source deadline
-- after it exactly when the peer is a source.
prop_decision_oneTransaction
  :: ArbTxDecisionPolicy -> Bool -> Bool -> Bool -> NonNegative Int -> Property
prop_decision_oneTransaction (ArbTxDecisionPolicy policy0) isRoot pipelined source
                             (NonNegative advertised0) =
      cover 20 source "a source: the decision writes its deadline"
    $ cover 20 (advertised > 0) "txids advertised"
    $ counterexample (show commits) (commits === Just 1)
      .&&. counterexample ("deadline " ++ show deadline) (isJust deadline === source)
  where
    advertised = advertised0 `mod` 4
    -- a source by the count the policy asks for, recorded at the
    -- decision's time
    policy | source    = policy0 { txSourceCount = max 1 (txSourceCount policy0) }
           | otherwise = policy0
    trace = runSimTrace $ do
      registry  <- newPeerTxRegistry (mkStdGen 7) (pure (const isRoot))
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
               sharedVar registry retired (0 :: PeerAddr) $ \api -> do
        joinTiers api
        let ps0 = emptyPeerTxLocalState
                    { peerJitterSeed = apiJitterSeed api
                    , peerNovelAt    = if source
                                         then recordNovel policy (Time 2) (txSourceCount policy) StrictSeq.empty
                                         else StrictSeq.empty }
        ps <- if advertised > 0
                then applyReceivedTxIds api (Time 1) Nothing (fromIntegral advertised)
                       [ (7 + i, 100) | i <- [1 .. advertised] ]
                       ps0 { peerRequestedTxIds = fromIntegral advertised }
                else pure ps0
        traceM DecisionMark
        _ <- if pipelined then runNextPeerActionPipelined api (Time 2) ps
                          else runNextPeerAction api (Time 2) Nothing ps
        traceM DecisionMark
        -- the entry's deadline after the decision
        tiers <- readTVarIO (peerTiers registry)
        case Map.elems tiers of
          [e] -> ptsSourceUntil <$> readTVarIO (pteState e)
          es  -> error ("peers in the order: " ++ show (length es))
    deadline = either (error . show) id (traceResult False trace)
    events = traceEvents trace
    -- the commits between the two marks, by the thread that made them
    commits = case [ (i, tid) | (i, (_, tid, _, EventLog dyn)) <- zip [0 :: Int ..] events
                              , fromDynamic dyn == Just DecisionMark ] of
      [(i, tid), (j, _)] ->
        Just (length [ () | (_, tid', _, EventTxCommitted {}) <- take (j - i - 1) (drop (i + 1) events)
                          , tid' == tid ])
      _ -> Nothing

-- | The census counts each standing once and classifies root before coin
-- before source, against explicit counts over generated peers.
prop_tier_censusOracle :: [(Bool, Bool, Bool)] -> Property
prop_tier_censusOracle peers =
      cover 20 (any (\(r, c, s) -> not r && not c && s) peers) "a source"
    $ cover 20 (any (\(r, _, _) -> r) peers) "a root"
    $      censusOf standings === TierCensus
             { censusFastByRoot   = length [ () | (True, _, _) <- peers ]
             , censusFastByCoin   = length [ () | (False, True, _) <- peers ]
             , censusFastBySource = length [ () | (False, False, True) <- peers ]
             , censusSlow         = length [ () | (False, False, False) <- peers ]
             }
      .&&. counterexample "the docs against the traced names"
             (map fst tierCensusDocs === map fst (tierCensusFields (censusOf standings)))
  where
    standings = [ standingOf root coin (if root || coinFast || source then FastTier else SlowTier)
                | (root, coinFast, source) <- peers
                , let coin = if coinFast then FastTier else SlowTier ]

-- | A decision's tier read puts the peer on root-or-coin, as its entry
-- has them, and writes until when it is a source to the entry, whose
-- standing is then the one the peer is served on, source rule included:
-- a source up to a window after its accepts and not after, with no
-- decision in between; reading again changes nothing.
prop_tier_withTierStanding
  :: ArbTxDecisionPolicy -> Bool -> Bool -> Bool -> NonNegative Int -> Property
prop_tier_withTierStanding (ArbTxDecisionPolicy policy0) root coinFast promote (NonNegative extra) =
      cover 10 (not root && not coinFast && source) "promoted source"
    $ cover 10 (not root && not coinFast && not source) "slow"
    $ runSimOrThrow $ do
        entry <- PeerTierEntry <$> newTVarIO (PeerTierState coin root Nothing)
        ps'  <- atomically (withTier policy t0 entry ps)
        s1   <- readTVarIO (pteState entry)
        ps'' <- atomically (withTier policy t0 entry ps')
        s2   <- readTVarIO (pteState entry)
        pure $ conjoin
          [ counterexample "tier" (peerTier ps' === if root then FastTier else coin)
          , counterexample "flags"
              (s1 === PeerTierState coin root (if source then Just edge else Nothing))
          , counterexample "standing"
              (tierStanding t0 s1 === standingOf root coin (effectiveTier policy t0 ps'))
          , counterexample "source" ((tierStanding t0 s1 == StandingSource) === (not root && not coinFast && source))
          , counterexample "at the window's edge" (tierStanding edge s1 === tierStanding t0 s1)
          , counterexample "lapsed" (tierStanding (addTime 0.001 edge) s1 === standingOf root coin coin)
          , counterexample "second read" (ps'' === ps' .&&. s2 === s1)
          ]
  where
    policy = policy0 { txSourceCount = max 1 (txSourceCount policy0) }
    t0     = Time 100
    edge   = addTime (txSourceWindow policy) t0
    coin   = if coinFast then FastTier else SlowTier
    -- accepts at or above the source count, or short of it
    accepts | promote   = txSourceCount policy + extra
            | otherwise = max 0 (txSourceCount policy - 1 - extra)
    ps     = emptyPeerTxLocalState { peerNovelAt = recordNovel policy t0 accepts StrictSeq.empty }
               :: PeerTxLocalState (Tx TxId)
    source = isSource policy t0 (peerNovelAt ps)

-- | One step of a flag script over a few peer ids: a peer joining with a
-- root flag, deciding with a count of novel accepts, the governor's answer
-- for it changing, the counters thread's refresh of every flag, leaving,
-- or a rotation.  Ids repeat, so later events find peers.
data FlagEvent
  = FlagJoin Int Bool
  | FlagDecide Int Int
  | FlagRoot Int Bool
  | FlagRefresh
  | FlagLeave Int
  | FlagRotate
  deriving (Eq, Show)

newtype FlagScript = FlagScript [FlagEvent]
  deriving Show

instance Arbitrary FlagScript where
  arbitrary = FlagScript <$> listOf event
    where
      peer  = choose (0, 7 :: Int)
      root  = frequency [(3, pure False), (1, pure True)]
      event = frequency
        [ (4, FlagJoin <$> peer <*> root)
        , (4, FlagDecide <$> peer <*> frequency [(1, pure 0), (1, choose (1, 20))])
        , (2, FlagRoot <$> peer <*> root)
        , (2, pure FlagRefresh)
        , (2, FlagLeave <$> peer)
        , (1, pure FlagRotate)
        ]
  shrink (FlagScript es) = FlagScript <$> shrinkList shrinkEvent es
    where
      shrinkEvent (FlagJoin p r)   = [ FlagJoin p' r | p' <- shrink p ] ++ [ FlagJoin p False | r ]
      shrinkEvent (FlagDecide p n) = [ FlagDecide p' n | p' <- shrink p ] ++ [ FlagDecide p n' | n' <- shrink n ]
      shrinkEvent (FlagRoot p r)   = [ FlagRoot p' r | p' <- shrink p ] ++ [ FlagRoot p False | r ]
      shrinkEvent FlagRefresh      = []
      shrinkEvent (FlagLeave p)    = [ FlagLeave p' | p' <- shrink p ]
      shrinkEvent FlagRotate       = []

-- | A live peer of the flag model: its registration and entry, and what
-- the registry should know of it: the root flag as its join or the last
-- refresh read it, until when it is a source as its last decision wrote
-- it, and the coin the last placement gave it.
data FlagPeer m = FlagPeer {
    fpRegistration :: PeerRegistration Int,
    fpEntry        :: PeerTierEntry m,
    fpState        :: PeerTierState
  }

-- | The registry's tier states and census follow the flags as the joins,
-- the refreshes and the peers' decisions left them, after every step of
-- a script: a placement keeps a root out of the candidates by what its
-- join or the last refresh read, not by what the governor says now; a
-- decision writes until when the peer is a source and is served on
-- root-or-coin, placing nothing; a source keeps its standing through a
-- placement that takes its coin slot, and loses it a window after its
-- accepts.  The model keeps the flags and places the coins from the salt
-- itself.
prop_tier_censusFollowsFlags :: ArbTxDecisionPolicy -> NonNegative Int -> FlagScript -> Int -> Property
prop_tier_censusFollowsFlags (ArbTxDecisionPolicy policy0) (NonNegative k0) (FlagScript events) seed =
    runSimOrThrow $ do
      rootsVar <- newTVarIO Map.empty
      registry <- newPeerTxRegistry (mkStdGen seed)
                    ((\roots p -> Map.findWithDefault False p roots) <$> readTVar rootsVar)
      (checks, demoted) <- go registry rootsVar Map.empty events
      pure $ cover 10 (demoted > 0) "a source lost its coin slot"
           $ conjoin checks
  where
    k      = k0 `mod` 5
    policy = policy0 { txFastPeers = k, txSourceCount = max 1 (txSourceCount policy0) }
    t0     = Time 100
    -- a window and more after the decisions: every source has lapsed
    t1     = addTime (txSourceWindow policy + 1) t0

    go _ _ _ [] = pure ([], 0 :: Int)
    go registry rootsVar live (ev : rest) = do
      (live', demoted, extra) <- step registry rootsVar live ev
      verdict <- oracle registry live'
      (checks, demotedMore) <- go registry rootsVar live' rest
      pure (counterexample (show ev) (extra .&&. verdict) : checks, demoted + demotedMore)

    step registry rootsVar live ev = case ev of
      FlagJoin p root
        | Map.member p live -> pure (live, 0, property True)
        | otherwise -> do
            c <- newTVarIO mempty
            atomically (modifyTVar rootsVar (Map.insert p root))
            (r, e) <- atomically $ do
              r <- registerPeerCounters registry p c
              (e, _) <- registerPeerTier registry
              joinTier policy registry r e root
              pure (r, e)
            place registry (Map.insert p (FlagPeer r e (PeerTierState SlowTier root Nothing)) live)
      FlagDecide p accepts -> case Map.lookup p live of
        Nothing -> pure (live, 0, property True)
        Just fp -> do
          let ps = emptyPeerTxLocalState { peerNovelAt = recordNovel policy t0 accepts StrictSeq.empty }
                     :: PeerTxLocalState ()
          ps' <- atomically (withTier policy t0 (fpEntry fp) ps)
          let s      = fpState fp
              s'     = s { ptsSourceUntil = if accepts >= txSourceCount policy
                                              then Just (addTime (txSourceWindow policy) t0)
                                              else Nothing }
              served = if ptsRoot s then FastTier else ptsCoin s
          pure ( Map.insert p fp { fpState = s' } live
               , 0
               , counterexample ("served " ++ show (peerTier ps')) (peerTier ps' === served) )
      FlagRoot p root -> do
        atomically (modifyTVar rootsVar (Map.insert p root))
        pure (live, 0, property True)
      FlagRefresh -> do
        refreshTierRoots policy registry
        roots <- readTVarIO rootsVar
        let rootOf p = Map.findWithDefault False p roots
            live'    = Map.mapWithKey (\p fp -> fp { fpState = (fpState fp) { ptsRoot = rootOf p } }) live
        -- a flag that changed places the tiers, in the model as in the
        -- registry
        if Map.map (ptsRoot . fpState) live' /= Map.map (ptsRoot . fpState) live
           then place registry live'
           else pure (live', 0, property True)
      FlagLeave p -> case Map.lookup p live of
        Nothing -> pure (live, 0, property True)
        Just fp -> do
          atomically (leaveTier policy registry (fpRegistration fp))
          place registry (Map.delete p live)
      FlagRotate -> do
        atomically (rotateTiers policy registry)
        place registry live

    -- the model's placement from the salt the registry holds: the first k
    -- of the non-roots by salted rank are fast; how many sources lost
    -- their slot to it
    place registry live = do
      salt <- readTVarIO (registrySalt registry)
      let rank (addr, n) = (fst (nextWord64 (mkSMGen (fromIntegral (hashWithSalt salt addr)))), n)
          others  = [ fpRegistration fp | fp <- Map.elems live, not (ptsRoot (fpState fp)) ]
          fastSet = Set.fromList (take k (sortOn rank others))
          coinOf fp | Set.member (fpRegistration fp) fastSet = FastTier
                    | otherwise                              = SlowTier
          placed  = Map.map (\fp -> fp { fpState = (fpState fp) { ptsCoin = coinOf fp } }) live
          demoted = length [ ()
                           | fp <- Map.elems live
                           , isJust (ptsSourceUntil (fpState fp))
                           , ptsCoin (fpState fp) == FastTier
                           , coinOf fp == SlowTier
                           ]
      pure (placed, demoted, property True)

    oracle registry live = do
      census0 <- atomically (tierCensus t0 registry)
      census1 <- atomically (tierCensus t1 registry)
      states <- forM (Map.toList live) $ \(p, fp) -> (,) p <$> readTVarIO (pteState (fpEntry fp))
      pure $ conjoin
        [ counterexample "census"
            (census0 === censusOf [ tierStanding t0 (fpState fp) | fp <- Map.elems live ])
        , counterexample "census a window later"
            (census1 === censusOf [ tierStanding t1 (fpState fp) | fp <- Map.elems live ])
        , counterexample "a source a window later" (censusFastBySource census1 === 0)
        , conjoin [ counterexample ("peer " ++ show p ++ " state") (s === fpState (live Map.! p))
                  | (p, s) <- states ]
        ]

-- | The reply-size counters through the registry: two txid replies of
-- the same @n@ txids bump the bucket of @n@ twice and no other bucket,
-- for @n@ across every bucket and its boundaries.
prop_replySizeCounters :: ArbTxDecisionPolicy -> NonNegative Int -> Property
prop_replySizeCounters (ArbTxDecisionPolicy policy0) (NonNegative n0) =
    runSimOrThrow $ do
      registry  <- noRootsRegistry (mkStdGen 3)
      sharedVar <- newSharedTxStateVar (emptySharedTxState :: SharedTxState PeerAddr TxId)
      retired   <- newTxSubmissionCountersVar mempty
      mempool   <- emptyMempool
      _ <- withPeer policy defaultTxSubmissionConfigV2 (getMempoolReader mempool)
                    sharedVar registry retired (0 :: PeerAddr) $ \api -> do
        -- each reply answers a request for n ids
        ps1 <- applyReceivedTxIds api t1 Nothing (fromIntegral n) ids
                 emptyPeerTxLocalState { peerRequestedTxIds = fromIntegral n }
        applyReceivedTxIds api t2 Nothing (fromIntegral n) ids
          ps1 { peerRequestedTxIds = fromIntegral n }
      c <- snapshotCountersWith atomically retired registry
      let buckets = (txIdReplies0 c, txIdReplies1 c, txIdReplies2to4 c, txIdReplies5to9 c, txIdReplies10 c)
      pure $ conjoin
        [ counterexample ("replies " ++ show (txIdRepliesReceived c)) (txIdRepliesReceived c === 2)
        , counterexample ("received " ++ show (txIdsReceived c)) (txIdsReceived c === 2 * fromIntegral n)
        , counterexample ("buckets " ++ show buckets) (buckets === expectedBuckets)
        ]
  where
    policy = policy0 { txSourceCount = 0 }
    n      = n0 `mod` 13
    t1     = Time 11
    t2     = Time 12
    ids    = [ (i, 100) | i <- [1 .. n] ] :: [(TxId, SizeInBytes)]
    expectedBuckets :: (Word64, Word64, Word64, Word64, Word64)
    expectedBuckets
      | n == 0    = (2, 0, 0, 0, 0)
      | n == 1    = (0, 2, 0, 0, 0)
      | n <= 4    = (0, 0, 2, 0, 0)
      | n <= 9    = (0, 0, 0, 2, 0)
      | otherwise = (0, 0, 0, 0, 2)

-- | One fetch cycle for a peer: txids it advertises, which of the requested
-- bodies it delivers, and which of the delivered ones the mempool rejects.
data FetchCycle = FetchCycle ArbTxDecisionPolicy (Positive Int) [Bool] [Bool]
  deriving Show

instance Arbitrary FetchCycle where
  arbitrary = do
    n <- choose (1, 8)
    FetchCycle <$> arbitrary <*> pure (Positive n) <*> vectorOf n arbitrary <*> vectorOf n arbitrary
  shrink (FetchCycle p (Positive n) ds rs) =
       [ FetchCycle p' (Positive n) ds rs | p' <- shrink p ]
    ++ [ FetchCycle p (Positive n') (take n' ds) (take n' rs) | n' <- [1 .. n - 1] ]
    ++ [ FetchCycle p (Positive n) ds' rs | ds' <- shrink ds, length ds' == n ]
    ++ [ FetchCycle p (Positive n) ds rs' | rs' <- shrink rs, length rs' == n ]

-- | The claimable counter moves exactly once when a reply omits a requested
-- body (a lease released) and exactly once when a submission is rejected
-- (a lease released), and not when txids arrive, bodies are requested,
-- bodies arrive or accepts happen; the generation moves on accepts too.
prop_claimable_releases :: FetchCycle -> Property
prop_claimable_releases (FetchCycle (ArbTxDecisionPolicy policy0) (Positive n) delivers rejects) =
      cover 30 (omitted > 0) "a body omitted"
    . cover 25 (rejected > 0) "a submission rejected"
    . cover 30 (accepted > 0) "a submission accepted"
    $ conjoin
        [ counterexample "txids arriving moved the claimable counter" (c1 === c0)
        , counterexample "requesting bodies moved the claimable counter" (c2 === c1)
        , counterexample ("bodies arriving: omitted " ++ show omitted) (c3 === c2 + (if omitted > 0 then 1 else 0))
        , counterexample ("submission: rejected " ++ show rejected) (c4 === c3 + (if rejected > 0 then 1 else 0))
        , counterexample "an accept must still move the generation"
            (not submittedAny || sharedGeneration ss4 > sharedGeneration ss3)
        ]
  where
    policy = policy0 { txsSizeInflightPerPeer = max_TX_SIZE * fromIntegral (max 1 n)
                     , maxOutstandingTxBatchesPerPeer = max 1 n
                     , txInflightMultiplicity = max 1 (txInflightMultiplicity policy0) }
    peer = 0 :: PeerAddr; at = Time 1
    ids = [ (txid, 100) | txid <- [1 .. n] ]
    ps0 = emptyPeerTxLocalState { peerRequestedTxIds = fromIntegral n }
    ss0 = emptySharedTxState; c0 = sharedClaimable ss0
    (ps1, pif1, ss1) = handleReceivedTxIds (const False) at policy (fromIntegral n) ids ps0 emptyPeerTxInFlight ss0
    c1 = sharedClaimable ss1
    (act2, ps2, pif2, ss2) = nextPeerAction at policy defaultTxSubmissionConfigV2 peer ps1 pif1 ss1
    requested = case act2 of PeerRequestTxs ks -> ks; _ -> []
    c2 = sharedClaimable ss2
    txidOf (TxKey k) = sharedKeyToTxId ss2 IntMap.! k
    delivered = [ (txidOf k, mkTx (txidOf k) 100) | (k, True) <- zip requested delivers ]
    omitted   = length requested - length delivered
    (_, _, ps3, pif3, ss3) = handleReceivedTxs (const False) at policy peer delivered ps2 pif2 ss2
    c3 = sharedClaimable ss3
    (act4, ps4, pif4, ss4a) = nextPeerAction at policy defaultTxSubmissionConfigV2 peer ps3 pif3 ss3
    submitted = case act4 of PeerSubmitTxs ks -> ks; _ -> []
    submittedAny = not (null submitted)
    (acceptedKs, rejectedKs) = ( [ k | (k, False) <- zip submitted rejects ], [ k | (k, True) <- zip submitted rejects ] )
    accepted = length acceptedKs; rejected = length rejectedKs
    (_, _, ss4) | submittedAny = handleSubmittedTxs at policy peer (withTxIds ss4a acceptedKs) rejectedKs ps4 pif4 ss4a
                | otherwise    = (ps4, pif4, ss4a)
    c4 = sharedClaimable ss4

-- | A stuck entry: leased by another peer past its attempt cap while this
-- peer holds the body.  The decision bumps the cap, and the claimable
-- counter with it, exactly when the lease is old enough.
-- | How long ago a lease was taken, in milliseconds, ranging over the
-- inflight timeouts the policy generator produces (up to about 11 s).
newtype LeaseAge = LeaseAge Int
  deriving Show

instance Arbitrary LeaseAge where
  arbitrary = LeaseAge <$> frequency [ (1, choose (0, 300)), (3, choose (0, 15000)) ]
  shrink (LeaseAge a) = [ LeaseAge a' | NonNegative a' <- shrink (NonNegative a) ]

prop_claimable_stuckBump :: ArbTxDecisionPolicy -> Positive Int -> LeaseAge -> Bool -> Property
prop_claimable_stuckBump (ArbTxDecisionPolicy policy) (Positive capN) (LeaseAge ageMs) inSubmission =
      cover 30 eligible "bump due"
    . cover 30 (not eligible) "no bump due"
    $ counterexample (show (eligible, sharedClaimable ss0, sharedClaimable ss1))
        (sharedClaimable ss1 === sharedClaimable ss0 + (if eligible then 1 else 0))
 .&&. counterexample "the cap must move iff the counter does"
        ((currentMaxInflightMultiplicity entry1 > cap) === eligible)
  where
    cap = (capN `mod` 4) + 1
    at = Time 1000
    leaseUntil = addTime (negate (fromIntegral ageMs / 1000)) at
    entry0 = TxEntry { txLease = TxLeased (1 :: PeerAddr) leaseUntil, txAttempt = cap, txAdvertisers = 2
                     , txInSubmission = inSubmission, currentMaxInflightMultiplicity = cap }
    ss0 = (mkSharedState [7]) { sharedTxTable = IntMap.singleton 0 entry0 }
    ps0 = emptyPeerTxLocalState { peerUnacknowledgedTxIds = StrictSeq.singleton (TxKey 0)
                                , peerDownloadedTxs = IntMap.singleton 0 (mkTx 7 100) }
    pif0 = emptyPeerTxInFlight { pifAdvertised = IntSet.singleton 0 }
    (_, _, _, ss1) = nextPeerAction at policy defaultTxSubmissionConfigV2 (0 :: PeerAddr)
                                    ps0 pif0 ss0
    entry1 = sharedTxTable ss1 IntMap.! 0
    eligible = not inSubmission && at >= addTime (inflightTimeout policy - interTxSpace policy) leaseUntil

-- | A leaving peer's scrub moves the claimable counter iff it released a
-- lease, an attempt or a submission on a present entry; advertising alone
-- releases nothing.
-- | Four sparse flag lists: which of four entries a leaving peer advertises,
-- leases, attempts and submits.
data ScrubFlags = ScrubFlags [Bool] [Bool] [Bool] [Bool]
  deriving Show

instance Arbitrary ScrubFlags where
  arbitrary = ScrubFlags <$> flags <*> flags <*> flags <*> flags
    where flags = vectorOf 4 (frequency [ (9, pure False), (1, pure True) ])
  shrink (ScrubFlags a l t u) =
    [ ScrubFlags a' l' t' u' | (a', l', t', u') <- shrink (a, l, t, u)
                             , all ((== 4) . length) [a', l', t', u'] ]

prop_claimable_scrub :: ScrubFlags -> Property
prop_claimable_scrub (ScrubFlags adv lea att sub) =
      cover 25 work "work released"
    . cover 25 (not work) "nothing released"
    $ sharedClaimable ss1 === sharedClaimable ss0 + (if work then 1 else 0)
  where
    n = 4
    keys flags = IntSet.fromList [ k | (k, True) <- zip [0 .. n - 1] (take n (flags ++ repeat False)) ]
    pif = PeerTxInFlight { pifAdvertised = keys adv, pifLeased = keys lea, pifAttempting = keys att, pifSubmitting = keys sub }
    entry = TxEntry { txLease = TxLeased (0 :: PeerAddr) (Time 5), txAttempt = 1, txAdvertisers = 1
                    , txInSubmission = False, currentMaxInflightMultiplicity = 2 }
    ss0 = (mkSharedState [1 .. n]) { sharedTxTable = IntMap.fromList [ (k, entry) | k <- [0 .. n - 1] ] }
    ss1 = scrubFromPeerInFlight (0 :: PeerAddr) (Time 10) pif ss0
    work = not (IntSet.null (keys lea `IntSet.union` keys att `IntSet.union` keys sub))

-- | The slow tier's park returns when the claimable counter's mirror has
-- moved or its delay has expired, and not when only the shared state
-- moved, its generation included: the park reads the mirror alone.
prop_claimable_park :: Word64 -> Word64 -> Positive Word64 -> Bool -> Bool -> Property
prop_claimable_park claimable0 generation0 (Positive bump) moveClaimable withDelay =
    runSimOrThrow $ do
      var <- newTVarIO emptySharedTxState { sharedClaimable = claimable0, sharedGeneration = generation0 }
      mirror <- newTVarIO claimable0
      -- the move happens half a second in; the delay, when given, a second in
      let move = do
            threadDelay 0.5
            atomically $
              if moveClaimable
                 then do modifyTVar var $ \st -> st { sharedClaimable = sharedClaimable st + bump }
                         writeTVar mirror (claimable0 + bump)
                 else modifyTVar var $ \st -> st { sharedGeneration = sharedGeneration st + bump }
      withAsync move $ \_ -> do
        mDelay <- if withDelay then Just <$> RegisteredDelay.new 1 else pure Nothing
        woke <- timeout 2 (atomically (awaitMirrorChange mirror claimable0 mDelay))
        t <- getMonotonicTime
        pure $ counterexample (show (moveClaimable, withDelay, woke, t)) $
          case (moveClaimable, withDelay) of
            (True, _)      -> woke === Just () .&&. counterexample "woke by the move" (t < Time 1)
            (False, True)  -> woke === Just () .&&. counterexample "woke by the delay" (t >= Time 1 && t < Time 2)
            (False, False) -> woke === Nothing
