{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Types
  ( -- * Shared state
    SharedTxState (..)
    -- * RetainedTxs with helper functions
  , RetainedTxs
  , retainedEmpty
  , retainedSingleton
  , retainedFromList
  , retainedToList
  , retainedSize
  , retainedLookup
  , retainedMember
  , retainedInsertMax
  , retainedDeleteKeys
  , retainedKeysSet
  , retainedRestrictKeys
  , retainedExpiredKeys
    -- * Traces
  , TraceTxLogic (..)
  , TxSubmissionInitDelay (..)
  , defaultTxSubmissionInitDelay
  , const_MAX_TX_SIZE_DISCREPANCY
    -- * Types shared with V1
    -- ** Various
  , ProcessedTxCount (..)
  , TxSubmissionLogicVersion (..)
    -- ** Mempool API
  , TxSubmissionMempoolWriter (..)
    -- ** Traces
  , TraceTxSubmissionInbound (..)
  , TxSubmissionCounters (..)
    -- ** Protocol Error
  , TxSubmissionProtocolError (..)
  , RequestedTxBatch (..)
  , TxLease (..)
  , TxEntry (..)
  , PeerAction (..)
  , PeerPhase (..)
  , PeerScore (..)
  , PeerTxLocalState (..)
  , PeerTxInFlight (..)
  , emptyPeerTxInFlight
    -- TxKey with helper functions
  , TxKey (..)
  , lookupTxKey
  , resolveTxKey
  , internTxId
  , internTxIds
  , emptyPeerScore
  , emptyPeerTxLocalState
  , emptySharedTxState
  , diffTimeToMilliseconds
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..))
import Control.Monad.Class.MonadTime.SI
import NoThunks.Class (NoThunks)
import NoThunks.Class.Orphans ()

import Data.List as List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Typeable (Typeable, eqT, (:~:) (Refl))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.Tx (HasRawTxId (..), RawTxId)

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntPSQ (IntPSQ)
import Data.IntPSQ qualified as IntPSQ
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Word (Word64)


-- | We check advertised sizes in a fuzzy way. The advertised and received
-- sizes need to agree up to this discrepancy.
const_MAX_TX_SIZE_DISCREPANCY :: SizeInBytes
const_MAX_TX_SIZE_DISCREPANCY = 32

-- | Compact internal transaction key used by V2 state.
newtype TxKey = TxKey { unTxKey :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Enum, NFData, NoThunks)

-- | The current download lease for a tx body.
--
-- A tx is either currently leased to a peer until a deadline or it is
-- unowned and became claimable at a specific time. Peers use their decayed
-- score as an additional per-peer delay after that claimable time before
-- attempting to steal the lease.
data TxLease peeraddr = TxLeased !peeraddr !Time
                      | TxClaimable !Time
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Shared per-tx state.
--
-- V2 keeps all cross-peer coordination at tx granularity. The first
-- peer that advertises a new txid becomes its initial owner. If the
-- lease expires without a successful outcome, the tx becomes claimable
-- and another eligible advertiser may atomically claim it.
--
-- Per-peer attempt detail is tracked in each peer's 'PeerTxInFlight'
-- TVar (owned by 'withPeer').  The shared entry only carries
-- aggregate counts: how many peers are currently attempting this body
-- and whether any peer is mid mempool submission.
data TxEntry peeraddr = TxEntry {
    -- | Current owner lease for downloading the tx body.  When
    -- 'TxClaimable', the embedded 'Time' also doubles as the
    -- last-activity stamp used by the orphan sweep.
    txLease                        :: !(TxLease peeraddr),

    -- | Number of peers currently attempting the body (claimed lease
    -- and/or buffered locally).  Incremented on claim, decremented on
    -- omit / lease release / 'PeerSubmitTxs' transition (the
    -- submitting peer no longer counts as an attempter).
    txAttempt                      :: !Int,

    -- | At least one peer is currently inside @mempoolAddTxs@ for this
    -- body.  Other peers use this flag, combined with their own
    -- 'pifSubmitting' set, to skip 'PeerSubmitTxs' for the same key.
    -- STM serialisation guarantees at most one peer is the submitter
    -- at any moment.
    txInSubmission                 :: !Bool,

    -- | Effective per-tx inflight multiplicity cap.
    --
    -- Initialised from 'txInflightMultiplicity' of the policy when the
    -- entry is created, and bumped by one when a peer's attempt sits past
    -- 'inflightTimeout' without reaching submission, allowing another
    -- peer to attempt in parallel.
    currentMaxInflightMultiplicity :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Per-peer in-flight tracking, owned by 'withPeer'.
--
-- The peer thread updates this TVar in the same STM transaction as
-- the shared 'TxEntry' so the two stay coherent.  Two readers:
--
-- * On exception, 'withPeer's bracket finalizer reads it and reverses
--   each per-peer contribution: releases any held lease, decrements
--   'txAttempt' for each attempted key, clears 'txInSubmission' for
--   any in-flight submission.
--
-- * The sweep walks every live peer's TVar to compute the union of
--   advertised keys and skips those entries when looking for orphans
--   to retire (the entry is still wanted by an active peer that
--   may simply be slow to claim).
data PeerTxInFlight = PeerTxInFlight {
    -- | Keys this peer currently has in its advertised window.  Used
    -- by the orphan sweep to know the entry is still wanted, and by
    -- 'nextWakeDelay' to scan for the earliest claim wake.
    pifAdvertised  :: !IntSet,

    -- | Keys this peer currently holds 'TxLeased' on.  Cleared on
    -- omit, accept, reject, lease-loss and bracket exit.
    pifLeased      :: !IntSet,

    -- | Keys this peer currently counts toward 'txAttempt'.  Spans
    -- the @claim -> download -> buffer@ phases; the key moves to
    -- 'pifSubmitting' on the @submit@ transition.
    pifAttempting  :: !IntSet,

    -- | Keys this peer currently counts toward 'txInSubmission'.
    -- Set on 'PeerSubmitTxs' / 'markSubmittingTxs', cleared on accept
    -- or reject.
    pifSubmitting  :: !IntSet,

    -- | Keys this peer holds in 'peerUnacknowledgedTxIds'.  A superset
    -- of 'pifAdvertised': also tracks keys the peer learned about via
    -- the retained-or-mempool path of 'handleReceivedTxIds' that are
    -- not advertised for fetch but still pending an ack.  Used by the
    -- sweep to know whether the txid lookup tables can be reclaimed.
    pifAcksPending :: !IntSet
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

emptyPeerTxInFlight :: PeerTxInFlight
emptyPeerTxInFlight = PeerTxInFlight {
    pifAdvertised = IntSet.empty,
    pifLeased     = IntSet.empty,
    pifAttempting = IntSet.empty,
    pifSubmitting = IntSet.empty,
    pifAcksPending = IntSet.empty
  }

-- | The next peer-local action chosen by the V2 worker thread.
--
-- V2 drives progress from the peer thread itself. Shared state only decides
-- whether a peer is allowed to perform an action; the peer thread then carries
-- it out directly.
data PeerAction
  = -- | No immediate work is available.  The peer should wait for either a
    -- shared-state generation change or the optional timeout.
    PeerDoNothing !Word64 !(Maybe DiffTime)
  | -- | Send a txid protocol message acknowledging the first argument and
    -- requesting the second argument.
    PeerRequestTxIds !NumTxIdsToAck !NumTxIdsToReq
  | -- | Request tx bodies for the given internally keyed txs from this peer.
    PeerRequestTxs ![TxKey]
  | -- | Submit the buffered txs identified by the given keys to the local
    -- mempool.
    PeerSubmitTxs ![TxKey]
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData


-- | A batch of transaction body requests sent to a peer.
--
-- Tracks the set of requested txids and the total expected
-- size in bytes for the batch. Used to manage inflight requests and match
-- responses to the original request.
data RequestedTxBatch = RequestedTxBatch {
    -- | The set of transaction keys requested in this batch.
    requestedTxBatchSet      :: !IntSet

    -- | Total expected size in bytes for all tx bodies in this batch.
  ,     requestedTxBatchSize :: !SizeInBytes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Coarse phase of a peer worker thread.
--
-- The phase is used when deciding whether a peer is currently eligible
-- to claim an expired tx lease.
data PeerPhase
  = -- | The peer worker is idle and may claim work for advertised txs.
    PeerIdle
  | -- | The peer worker is waiting for a txid reply from the remote peer.
    PeerWaitingTxIds
  | -- | The peer worker is waiting for a tx-body reply from the remote peer.
    PeerWaitingTxs
  | -- | The peer worker is submitting buffered txs to the local mempool.
    PeerSubmittingToMempool
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

instance NoThunks PeerPhase

-- | Peer usefulness score.
--
-- Lower is better. The current score is also interpreted as milliseconds of
-- extra delay before an idle peer may steal a claimable or expired tx lease.
data PeerScore = PeerScore {
    peerScoreValue :: !Double,
    peerScoreTs    :: !Time
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Low-cost monotonic counters for the V2 peer protocol and coordination path.
--
-- These counters are updated incrementally at the points where V2 performs
-- protocol sends, receives replies, or cheaply classifies received bodies.
-- They are kept separate from 'SharedTxState' so that emission can read a
-- small dedicated counter cell without scanning protocol state.
data TxSubmissionCounters = TxSubmissionCounters {
    txIdMessagesSent      :: !Word64,
    -- ^ Number of txid request messages sent (@MsgRequestTxIds@, blocking or
    -- pipelined).
    txIdsRequested        :: !Word64,
    -- ^ Total number of txids requested across all txid request messages (sum
    -- of @NumTxIdsToReq@ values).
    txIdRepliesReceived   :: !Word64,
    -- ^ Number of txid reply messages received (@MsgReplyTxIds@).
    txIdsReceived         :: !Word64,
    -- ^ Total number of txid-size pairs received across all txid replies.
    txMessagesSent        :: !Word64,
    -- ^ Number of tx body request messages sent (@MsgRequestTxs@).
    txsRequested          :: !Word64,
    -- ^ Total number of tx bodies requested across all body request messages.
    txRepliesReceived     :: !Word64,
    -- ^ Number of tx body reply messages received (@MsgReplyTxs@).
    txsReceived           :: !Word64,
    -- ^ Total number of tx bodies received across all body replies.
    txsOmitted            :: !Word64,
    -- ^ Number of requested tx bodies the peer omitted from its reply.
    lateBodies            :: !Word64,
    -- ^ Number of tx bodies received after the local state had already
    -- resolved them (txid was found in the mempool before the body arrived).
    txsAccepted           :: !Word64,
    -- ^ Tx bodies resolved into the mempool (includes txs found already present
    -- before attempting submission).
    txsRejected           :: !Word64,
    -- ^ Tx bodies rejected by the mempool.
    txIdBlockingReqsSent  :: !Word64,
    -- ^ Txid request messages sent as blocking requests.
    txIdPipelinedReqsSent :: !Word64,
    -- ^ Txid request messages sent as pipelined (non-blocking) requests.
    txIdBlockingWaitMs    :: !Word64,
    -- ^ Cumulative milliseconds spent waiting for replies to blocking txid
    -- requests.  High values indicate the system is mostly idle (no new
    -- transactions available from peers).
    txPipelineWaitMs      :: !Word64,
    -- ^ Cumulative milliseconds the pipeline was active, measured from the
    -- first 'MsgRequestTxs' send until all pipelined requests (both body and
    -- txid) have been replied to and the pipeline fully drains.  Proxy for
    -- the "loading" state where the peer is actively downloading transactions.
    txSubmissionWaitMs    :: !Word64
    -- ^ Cumulative milliseconds spent inside 'mempoolAddTxs'.  Covers both
    -- normal submission latency and time blocked due to a full mempool.
    -- High values relative to the other duration fields indicate mempool
    -- backpressure.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance Semigroup TxSubmissionCounters where
  a <> b = TxSubmissionCounters {
    txIdMessagesSent    = txIdMessagesSent a + txIdMessagesSent b,
    txIdsRequested      = txIdsRequested a + txIdsRequested b,
    txIdRepliesReceived = txIdRepliesReceived a + txIdRepliesReceived b,
    txIdsReceived      = txIdsReceived a + txIdsReceived b,
    txMessagesSent = txMessagesSent a + txMessagesSent b,
    txsRequested = txsRequested a + txsRequested b,
    txRepliesReceived = txRepliesReceived a + txRepliesReceived b,
    txsReceived = txsReceived a + txsReceived b,
    txsOmitted = txsOmitted a + txsOmitted b,
    lateBodies = lateBodies a + lateBodies b,
    txsAccepted = txsAccepted a + txsAccepted b,
    txsRejected = txsRejected a + txsRejected b,
    txIdBlockingReqsSent  = txIdBlockingReqsSent  a + txIdBlockingReqsSent  b,
    txIdPipelinedReqsSent = txIdPipelinedReqsSent a + txIdPipelinedReqsSent b,
    txIdBlockingWaitMs    = txIdBlockingWaitMs    a + txIdBlockingWaitMs    b,
    txPipelineWaitMs      = txPipelineWaitMs      a + txPipelineWaitMs      b,
    txSubmissionWaitMs    = txSubmissionWaitMs    a + txSubmissionWaitMs    b
    }

instance Monoid TxSubmissionCounters where
  mempty = TxSubmissionCounters {
    txIdMessagesSent = 0,
    txIdsRequested = 0,
    txIdRepliesReceived = 0,
    txIdsReceived = 0,
    txMessagesSent = 0,
    txsRequested = 0,
    txRepliesReceived = 0,
    txsReceived = 0,
    txsOmitted = 0,
    lateBodies = 0,
    txsAccepted = 0,
    txsRejected = 0,
    txIdBlockingReqsSent  = 0,
    txIdPipelinedReqsSent = 0,
    txIdBlockingWaitMs    = 0,
    txPipelineWaitMs      = 0,
    txSubmissionWaitMs    = 0
  }

-- | Convert a non-negative 'DiffTime' to whole milliseconds (truncated).
--
-- Works directly on the underlying picosecond 'Integer' to avoid the
-- 'realToFrac' detour through 'Rational' and 'Double'.
diffTimeToMilliseconds :: DiffTime -> Word64
diffTimeToMilliseconds = fromInteger . (`div` 1_000_000_000) . diffTimeToPicoseconds

emptyPeerScore :: Time -> PeerScore
emptyPeerScore scoreTs = PeerScore {
    peerScoreValue = 0,
    peerScoreTs    = scoreTs
  }

-- | Per-peer protocol state.
--
-- Owned by the worker thread for one peer. Includes the peer's coarse
-- protocol phase ('peerPhase'), which is purely peer-local: only this
-- peer's own scheduler reads and updates it, so it doesn't belong in
-- shared state.
data PeerTxLocalState tx = PeerTxLocalState {
    -- | Coarse phase of this peer's worker thread. Updated by
    -- 'nextPeerAction' / 'nextPeerActionPipelined' when an action is
    -- chosen and inspected in 'serverIdle' to decide whether to skip
    -- 'awaitSharedChange' on an Active->Idle transition.
    peerPhase                 :: !PeerPhase,

    -- | Unacknowledged txids in the order advertised by the peer.
    peerUnacknowledgedTxIds   :: !(StrictSeq TxKey),

    -- | Txids this peer currently advertises and that may be requested from
    -- it if the peer becomes the owner.
    peerAvailableTxIds        :: !(IntMap SizeInBytes),

    -- | Requested txids that have not yet been replied to.
    peerRequestedTxs          :: !IntSet,
    peerRequestedTxBatches    :: !(StrictSeq RequestedTxBatch),
    peerRequestedTxsSize      :: !SizeInBytes,
    peerRequestedTxIds        :: !NumTxIdsToReq,

    -- | Tx bodies downloaded from this peer and buffered locally until they
    -- are either submitted to the mempool or superseded by shared-state
    -- resolution.
    peerDownloadedTxs         :: !(IntMap tx),

    -- | Time at which the first outstanding body-request batch was
    -- sent in the current download episode.
    peerDownloadStartTime     :: !(Maybe Time),

    -- | Usefulness score for this peer, tracking rejection penalties and
    -- time-based decay.
    peerScore                 :: !PeerScore,

    -- | Set on a txid reply that returned zero new txids; cleared on a
    -- non-empty txid reply or when the mempool accepts at least one tx
    -- delivered by this peer.
    peerLastTxIdReplyWasEmpty :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

emptyPeerTxLocalState :: PeerTxLocalState tx
emptyPeerTxLocalState = PeerTxLocalState {
    peerPhase                 = PeerIdle,
    peerUnacknowledgedTxIds   = StrictSeq.empty,
    peerAvailableTxIds        = IntMap.empty,
    peerRequestedTxs          = IntSet.empty,
    peerRequestedTxBatches    = StrictSeq.empty,
    peerRequestedTxsSize      = 0,
    peerRequestedTxIds        = 0,
    peerDownloadedTxs         = IntMap.empty,
    peerDownloadStartTime     = Nothing,
    peerScore                 = emptyPeerScore (Time 0),
    peerLastTxIdReplyWasEmpty = False
  }

-- | Shared V2 state.
--
-- There is no global decision thread. Peer worker threads coordinate by
-- atomically reading and updating this shared state.
data SharedTxState peeraddr txid = SharedTxState {
    -- | Active unresolved txs that still participate in leasing, buffering,
    -- submission and advertiser tracking.
    sharedTxTable     :: !(IntMap (TxEntry peeraddr)),
    -- | Accepted txs retained locally for a bounded time so later txid
    -- advertisements can be acked without re-requesting the body.
    sharedRetainedTxs :: !RetainedTxs,
    sharedTxIdToKey   :: !(Map (RawTxId txid) TxKey),
    sharedKeyToTxId   :: !(IntMap txid),
    sharedNextTxKey   :: !Int,
    -- | Wake counter.  Bumped on transitions that may grant other peers a
    -- new option (lease release, new entry accepted into the mempool, cap
    -- bump, ...).  Parked peers watch this via 'awaitSharedChange'.
    sharedGeneration  :: !Word64,
    -- | Structural dirty bit.  Bumped on most structural changes; used by
    -- 'writeSharedStateIfChanged' (paired with 'sharedGeneration') to skip
    -- redundant TVar writes.  A change to either counter triggers a write.
    sharedRevision    :: !Word64
  }
  deriving stock Generic

deriving stock instance (Eq peeraddr, Eq txid, HasRawTxId txid) => Eq (SharedTxState peeraddr txid)
deriving stock instance (Show peeraddr, Show txid, HasRawTxId txid) => Show (SharedTxState peeraddr txid)
deriving anyclass instance (NFData peeraddr, NFData txid, HasRawTxId txid) => NFData (SharedTxState peeraddr txid)
deriving anyclass instance (NoThunks peeraddr, NoThunks txid, HasRawTxId txid) => NoThunks (SharedTxState peeraddr txid)

-- | Retained tx-key set with two indexes:
--
-- * 'retainedQueue' is keyed on 'Time' for cheap earliest-expiry queries.
-- * 'retainedSet'   shadows the keys for O(min(n, W)) 'retainedMember'
--   queries (the hot path: every received txid is checked here).
--
-- Both are kept in lockstep by the 'retained*' helpers below.
data RetainedTxs = RetainedTxs {
    retainedQueue :: !(IntPSQ Time ()),
    retainedSet   :: !IntSet
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

emptySharedTxState :: SharedTxState peeraddr txid
emptySharedTxState = SharedTxState {
    sharedTxTable          = IntMap.empty,
    sharedRetainedTxs      = retainedEmpty,
    sharedTxIdToKey        = Map.empty,
    sharedKeyToTxId        = IntMap.empty,
    sharedNextTxKey        = 0,
    sharedGeneration       = 0,
    sharedRevision         = 0
  }

retainedEmpty :: RetainedTxs
retainedEmpty = RetainedTxs IntPSQ.empty IntSet.empty

retainedSingleton :: Int -> Time -> RetainedTxs
retainedSingleton k retainUntil =
    RetainedTxs (IntPSQ.insert k retainUntil () IntPSQ.empty)
                (IntSet.singleton k)

retainedFromList :: [(Int, Time)] -> RetainedTxs
retainedFromList =
    List.foldl' (\retained (k, retainUntil) -> retainedInsertMax k retainUntil retained)
                retainedEmpty

retainedToList :: RetainedTxs -> [(Int, Time)]
retainedToList =
    sortOn fst
  . fmap (\(k, retainUntil, ()) -> (k, retainUntil))
  . IntPSQ.toList
  . retainedQueue

retainedSize :: RetainedTxs -> Int
retainedSize = IntSet.size . retainedSet
{-# INLINE retainedSize #-}

retainedLookup :: Int -> RetainedTxs -> Maybe Time
retainedLookup k retained =
    fmap fst (IntPSQ.lookup k (retainedQueue retained))
{-# INLINE retainedLookup #-}

retainedMember :: Int -> RetainedTxs -> Bool
retainedMember k = IntSet.member k . retainedSet
{-# INLINE retainedMember #-}

retainedInsertMax :: Int -> Time -> RetainedTxs -> RetainedTxs
retainedInsertMax k retainUntil (RetainedTxs queue keys) =
    RetainedTxs (IntPSQ.insert k retainUntil' () queue)
                (IntSet.insert k keys)
  where
    retainUntil' =
      case IntPSQ.lookup k queue of
        Just (existing, ()) -> max existing retainUntil
        Nothing             -> retainUntil
{-# INLINE retainedInsertMax #-}

retainedDeleteKeys :: IntSet -> RetainedTxs -> RetainedTxs
retainedDeleteKeys ks (RetainedTxs queue keys) =
    RetainedTxs (IntSet.foldl' (flip IntPSQ.delete) queue ks)
                (keys `IntSet.difference` ks)
{-# INLINE retainedDeleteKeys #-}

retainedKeysSet :: RetainedTxs -> IntSet
retainedKeysSet = retainedSet
{-# INLINE retainedKeysSet #-}

retainedRestrictKeys :: RetainedTxs -> IntSet -> RetainedTxs
retainedRestrictKeys (RetainedTxs queue keys) ks =
    RetainedTxs (IntPSQ.fold' keep IntPSQ.empty queue)
                (IntSet.intersection keys ks)
  where
    keep k retainUntil _
      | IntSet.member k ks = IntPSQ.insert k retainUntil ()
      | otherwise          = id
{-# INLINE retainedRestrictKeys #-}

retainedExpiredKeys :: Time -> RetainedTxs -> IntSet
retainedExpiredKeys currentTime retained =
    -- Quick exit if no TX has expired.
    case IntPSQ.findMin queue of
      Just (_, earliest, _) | earliest <= currentTime -> go IntSet.empty queue
      _                                               -> IntSet.empty
  where
    queue = retainedQueue retained
    go expired r =
      case IntPSQ.minView r of
        Just (k, retainUntil, (), r')
          | retainUntil <= currentTime ->
              go (IntSet.insert k expired) r'
          | otherwise ->
              expired
        Nothing ->
          expired
{-# INLINE retainedExpiredKeys #-}

lookupTxKey :: HasRawTxId txid
            => txid
            -> SharedTxState peeraddr txid
            -> Maybe TxKey
lookupTxKey txid SharedTxState { sharedTxIdToKey } =
    Map.lookup (getRawTxId txid) sharedTxIdToKey

resolveTxKey :: HasCallStack
             => SharedTxState peeraddr txid
             -> TxKey
             -> txid
resolveTxKey SharedTxState { sharedKeyToTxId } (TxKey k) =
    case IntMap.lookup k sharedKeyToTxId of
         Just txid -> txid
         Nothing   -> error $
           "TxSubmission.V2.resolveTxKey: missing tx key " ++ show k

internTxId :: HasRawTxId txid
           => txid
           -> SharedTxState peeraddr txid
           -> (RawTxId txid, TxKey, SharedTxState peeraddr txid)
internTxId txid st@SharedTxState { sharedTxIdToKey, sharedKeyToTxId, sharedNextTxKey }
  | Just key <- Map.lookup rawId sharedTxIdToKey = (rawId, key, st)
  | otherwise =
      let key = TxKey sharedNextTxKey in
      ( rawId
         , key
         , st { sharedTxIdToKey = Map.insert rawId key sharedTxIdToKey
              , sharedKeyToTxId = IntMap.insert sharedNextTxKey txid sharedKeyToTxId
              , sharedNextTxKey = sharedNextTxKey + 1
              }
         )
  where
    rawId = getRawTxId txid

internTxIds :: (Foldable f, HasRawTxId txid)
            => f txid
            -> SharedTxState peeraddr txid
            -> (Map (RawTxId txid) TxKey, SharedTxState peeraddr txid)
internTxIds txids st0 = List.foldl' step (Map.empty, st0) txids
  where
    step (acc, st) txid =
      let (rawId, key, st') = internTxId txid st in
      (Map.insert rawId key acc, st')

-- | Flag to enable/disable the usage of the new tx-submission logic.
--
data TxSubmissionLogicVersion =
      -- | the legacy `Ouroboros.Network.TxSubmission.Inbound.V1`
      TxSubmissionLogicV1
      -- | the new `Ouroboros.Network.TxSubmission.Inbound.V2`
    | TxSubmissionLogicV2
    deriving (Eq, Show, Enum, Bounded)

-- | TxLogic tracer.
--
data TraceTxLogic peeraddr txid tx =
    TraceSharedTxState (SharedTxState peeraddr txid)
  deriving Show


data ProcessedTxCount = ProcessedTxCount {
      -- | Just accepted this many transactions.
      ptxcAccepted :: Int
      -- | Just rejected this many transactions.
    , ptxcRejected :: Int
    , ptxcScore    :: Double
    }
  deriving (Eq, Show)


-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m err =
     TxSubmissionMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId          :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- Return list of valid tx's added to the mempool and a list of invalid
       -- tx's with the corresponding error.
       --
       mempoolAddTxs :: [tx] -> m ([txid], [(txid, err)])
    }


data TraceTxSubmissionInbound txid tx =
    -- | Number of transactions just about to be inserted.
    TraceTxSubmissionCollected [txid]
    -- | Just processed transaction pass/fail breakdown.
  | TraceTxSubmissionProcessed ProcessedTxCount
  | TraceTxInboundCanRequestMoreTxs Int
  | TraceTxInboundCannotRequestMoreTxs Int
  | TraceTxInboundAddedToMempool [txid] DiffTime
  | TraceTxInboundRejectedFromMempool [txid] DiffTime
  | TraceTxInboundError TxSubmissionProtocolError
  | TraceTxInboundRequestTxs [txid]

  --
  -- messages emitted by the new implementation of the server in
  -- "Ouroboros.Network.TxSubmission.Inbound.Server"; some of them are also
  -- used in this module.
  --

  -- | Server received 'MsgDone'
  | TraceTxInboundTerminated
  deriving (Eq, Show)


data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested
     | ProtocolErrorTxIdsNotRequested
     | forall txid. (Typeable txid, Show txid, Eq txid)
       => ProtocolErrorTxSizeError [(txid, SizeInBytes, SizeInBytes)]
     -- ^ a list of txid for which the received size and advertised size didn't
     -- match.

instance Eq   TxSubmissionProtocolError where
    ProtocolErrorTxNotRequested    == ProtocolErrorTxNotRequested      = True
    ProtocolErrorTxNotRequested    == _                                = False
    ProtocolErrorTxIdsNotRequested == ProtocolErrorTxIdsNotRequested   = True
    ProtocolErrorTxIdsNotRequested == _                                = False
    ProtocolErrorTxSizeError (as :: [(a, SizeInBytes, SizeInBytes)])
      == ProtocolErrorTxSizeError (as' :: [(a', SizeInBytes, SizeInBytes)]) =
        case eqT @a @a' of
          Nothing   -> False
          Just Refl -> as == as'
    ProtocolErrorTxSizeError {} == _ = False

deriving instance Show TxSubmissionProtocolError

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."
  displayException ProtocolErrorTxIdsNotRequested =
      "The peer replied with more txids than we asked for."
  displayException (ProtocolErrorTxSizeError txids) =
      "The peer received txs with wrong sizes " ++ show txids

data TxSubmissionInitDelay =
     TxSubmissionInitDelay DiffTime
 | NoTxSubmissionInitDelay
 deriving (Eq, Show)

defaultTxSubmissionInitDelay :: TxSubmissionInitDelay
defaultTxSubmissionInitDelay = TxSubmissionInitDelay 60
