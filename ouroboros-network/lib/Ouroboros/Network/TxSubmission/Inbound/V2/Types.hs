{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
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
  , RetainTime
  , retainTimeSeconds
  , retainDeadline
  , retainNow
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
  , retainedExpireUpTo
  , retainedDueBy
    -- ** Sweep cursor
  , SweepCursor (..)
  , sweepStart
    -- ** Counts for tracing
  , SharedTxStateCounts (..)
  , sharedTxStateCounts
  , sharedTxStateCountFields
  , tierCensusFields
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
  , TxSubmissionCountersOf (..)
  , TxSubmissionCounters
  , TxSubmissionCountersAcc
  , emittedCounters
    -- ** Protocol Error
  , TxSubmissionProtocolError (..)
  , RequestedTxBatch (..)
  , TxLease (..)
  , TxDoom (..)
  , TxEntry (..)
  , PeerAction (..)
  , ResolvedPeerAction (..)
  , PeerPhase (..)
  , PeerTier (..)
  , recordRequestTier
  , takeReplyTier
  , TierStanding (..)
  , TierCensus (..)
  , PeerTierState (..)
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

-- | The sweep's review of an active entry for abandonment.  An entry
-- claimable for a grace with nobody attempting or submitting it is a
-- candidate: one sweep dooms it, and the counters thread then asks every
-- live peer's advertised set, one peer at a time, before the next review
-- drops it.  An advertisement or a claim of a doomed entry meanwhile
-- clears the doom, a holder found spares it, and a spared entry is a
-- candidate again a grace later.  So advertising an existing entry writes
-- nothing shared unless the entry is under review, and the review never
-- drops an entry a peer still advertises: its reply committed before the
-- doom, and its set shows the key, or after, and cleared it.
data TxDoom
  = Undoomed
  | Doomed
    -- ^ under review: dropped by the next review unless a holder is found
  | Spared !Time
    -- ^ a holder was found at this time; a candidate again after the grace
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
    --
    -- Invariant: while 'TxLeased' to a registered peer, that peer's
    -- 'pifLeased' contains this key (stale leases left behind by
    -- inflight-cap theft are tolerated).
    txLease                        :: !(TxLease peeraddr),

    -- | Number of peers currently attempting the body (claimed lease
    -- and/or buffered locally).  Incremented on claim, decremented on
    -- omit / lease release / 'PeerSubmitTxs' transition (the
    -- submitting peer no longer counts as an attempter).
    --
    -- Invariant: equals the number of registered peers whose
    -- 'pifAttempting' contains this key.
    txAttempt                      :: !Int,

    -- | At least one peer is currently inside @mempoolAddTxs@ for this
    -- body.  Other peers skip 'PeerSubmitTxs' for the key while it is
    -- set.  STM serialisation guarantees at most one peer is the
    -- submitter at any moment.
    --
    -- Invariant: true iff some registered peer's 'pifSubmitting'
    -- contains this key.
    txInSubmission                 :: !Bool,

    -- | The sweep's review of the entry for abandonment, see 'TxDoom'.
    --
    -- Invariant: a 'Doomed' entry is claimable with no attempt and no
    -- submission; a claim clears the doom with the lease it takes.
    txDoom                         :: !TxDoom,

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
-- the shared 'TxEntry' so the two stay coherent.  Its other readers are
-- 'withPeer's bracket finalizer, which on exit reverses each per-peer
-- contribution: releases any held lease, decrements 'txAttempt' for each
-- attempted key, clears 'txInSubmission' for any in-flight submission;
-- and the counters thread's review of doomed entries, which reads the
-- advertised set of one peer per transaction, never inside the sweep's.
--
-- Invariant: 'pifAttempting' and 'pifSubmitting' are disjoint; a key
-- moves from the former to the latter atomically on the submit
-- transition.  'pifLeased' is a subset of their union.
data PeerTxInFlight = PeerTxInFlight {
    -- | Keys this peer currently has in its advertised window.  Used by
    -- 'nextWakeDelay' to scan for the earliest claim wake.
    --
    -- Invariant: a subset of the live keys (active 'sharedTxTable'
    -- union retained 'sharedRetainedTxs'), but for a key whose retention
    -- expired before this peer acknowledged it; the acknowledgement
    -- follows ('txIdAckable').
    pifAdvertised :: !IntSet,

    -- | Keys this peer currently holds 'TxLeased' on.  Cleared on
    -- omit, accept, reject, lease-loss and bracket exit.
    pifLeased     :: !IntSet,

    -- | Keys this peer currently counts toward 'txAttempt'.  Spans
    -- the @claim -> download -> buffer@ phases; the key moves to
    -- 'pifSubmitting' on the @submit@ transition.
    pifAttempting :: !IntSet,

    -- | Keys this peer currently counts toward 'txInSubmission'.
    -- Set on 'PeerSubmitTxs' / 'markSubmittingTxs', cleared on accept
    -- or reject.
    pifSubmitting :: !IntSet
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

emptyPeerTxInFlight :: PeerTxInFlight
emptyPeerTxInFlight = PeerTxInFlight {
    pifAdvertised  = IntSet.empty,
    pifLeased      = IntSet.empty,
    pifAttempting  = IntSet.empty,
    pifSubmitting  = IntSet.empty
  }

-- | The next peer-local action chosen by the V2 worker thread.
--
-- V2 drives progress from the peer thread itself. Shared state only decides
-- whether a peer is allowed to perform an action; the peer thread then carries
-- it out directly.
data PeerAction
  = -- | No immediate work is available.  The peer should wait for either a
    -- shared-state generation change or the optional timeout; or, when the
    -- third field is set, for the 'sharedClaimable' counter to move past it
    -- instead of the generation: the slow fetch tier's park while its gate
    -- holds a txid request.
    PeerDoNothing !Word64 !(Maybe DiffTime) !(Maybe Word64)
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

-- | A 'PeerAction' with what carrying it out takes from the shared state,
-- resolved in the deciding transaction: the txids and advertised sizes of
-- the bodies to request, the buffered bodies to submit with their txids.
-- A key that leaves the state once the decision is made, by another peer's
-- submission of the same tx and the retention's expiry, cannot fail the
-- resolution afterwards.
data ResolvedPeerAction txid tx
  = ResolvedDoNothing !Word64 !(Maybe DiffTime) !(Maybe Word64)
  | ResolvedRequestTxIds !NumTxIdsToAck !NumTxIdsToReq
  | ResolvedRequestTxs !(Map txid SizeInBytes)
  | ResolvedSubmitTxs ![(TxKey, txid, tx)]


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

-- | A peer's fetch tier (see "Ouroboros.Network.TxSubmission.Inbound.V2.Tier"):
-- fast peers are polled for txids as soon as there is something to
-- acknowledge, slow peers at 'txSlowCadence'.
data PeerTier
  = FastTier
  | SlowTier
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

instance NoThunks PeerTier

-- | Why a peer is served on the tier it is: the standing the registry keeps
-- per peer for the tier census.  A peer with several claims to the fast
-- tier counts once, root first, coin before source, since the coin's
-- slot is its whatever the source rule says.
data TierStanding
  = StandingRoot
    -- ^ fast by standing: a local root
  | StandingCoin
    -- ^ fast by the coin: among the first in the salted order
  | StandingSource
    -- ^ fast by the source rule while the coin left it slow
  | StandingSlow
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

instance NoThunks TierStanding

-- | How many live peers are served on each standing.
data TierCensus = TierCensus {
    censusFastByRoot   :: !Int,
    censusFastByCoin   :: !Int,
    censusFastBySource :: !Int,
    censusSlow         :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- | A peer's place in the tiers, as the registry keeps it: the coin's
-- verdict, written by a placement; whether it is a local root and whether
-- it is a source, written by its decisions.  Its standing follows from the
-- three (see 'Ouroboros.Network.TxSubmission.Inbound.V2.Tier.tierStanding').
data PeerTierState = PeerTierState {
    ptsCoin   :: !PeerTier,
    ptsRoot   :: !Bool,
    ptsSource :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

instance NoThunks PeerTierState

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
--
-- @t@ is the type of the three duration fields: 'TxSubmissionCountersAcc'
-- accumulates them as 'DiffTime', and 'emittedCounters' converts to the
-- whole milliseconds of 'TxSubmissionCounters'. Accumulating a duration
-- in its own type keeps the peer threads free of per-sample conversion
-- and rounding.
data TxSubmissionCountersOf t = TxSubmissionCounters {
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
    txIdSlowReqsSent      :: !Word64,
    -- ^ Txid request messages sent to peers on the slow tier (a subset of
    -- 'txIdMessagesSent').
    txIdsReceivedSlow     :: !Word64,
    -- ^ Txids received in replies to requests sent on the slow tier (a
    -- subset of 'txIdsReceived').
    txIdReplies0          :: !Word64,
    txIdReplies1          :: !Word64,
    txIdReplies2to4       :: !Word64,
    txIdReplies5to9       :: !Word64,
    txIdReplies10         :: !Word64,
    -- ^ Txid replies by the number of txids they carried: none, one, two
    -- to four, five to nine, ten or more (a partition of
    -- 'txIdRepliesReceived').
    txIdBlockingWait      :: !t,
    -- ^ Cumulative time spent waiting for replies to blocking txid
    -- requests.  High values indicate the system is mostly idle (no new
    -- transactions available from peers).
    txPipelineWait        :: !t,
    -- ^ Cumulative time the pipeline was active, measured from the first
    -- 'MsgRequestTxs' send until all pipelined requests (both body and
    -- txid) have been replied to and the pipeline fully drains.  Proxy for
    -- the "loading" state where the peer is actively downloading transactions.
    txSubmissionWait      :: !t
    -- ^ Cumulative time spent inside 'mempoolAddTxs'. Covers both normal
    -- submission latency and time blocked due to a full mempool. High
    -- values relative to the other duration fields indicate mempool
    -- backpressure.
  }
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass NFData

-- | The counters as traced and published: durations in whole milliseconds.
type TxSubmissionCounters = TxSubmissionCountersOf Word64

-- | The counters as accumulated by each peer and by the retired-peers
-- accumulator: durations exact, so no sample is lost to rounding and no
-- field can overflow.
type TxSubmissionCountersAcc = TxSubmissionCountersOf DiffTime

instance Num t => Semigroup (TxSubmissionCountersOf t) where
  a <> b = TxSubmissionCounters {
    txIdMessagesSent      = txIdMessagesSent a + txIdMessagesSent b,
    txIdsRequested        = txIdsRequested a + txIdsRequested b,
    txIdRepliesReceived   = txIdRepliesReceived a + txIdRepliesReceived b,
    txIdsReceived         = txIdsReceived a + txIdsReceived b,
    txMessagesSent        = txMessagesSent a + txMessagesSent b,
    txsRequested          = txsRequested a + txsRequested b,
    txRepliesReceived     = txRepliesReceived a + txRepliesReceived b,
    txsReceived           = txsReceived a + txsReceived b,
    txsOmitted            = txsOmitted a + txsOmitted b,
    lateBodies            = lateBodies a + lateBodies b,
    txsAccepted           = txsAccepted a + txsAccepted b,
    txsRejected           = txsRejected a + txsRejected b,
    txIdBlockingReqsSent  = txIdBlockingReqsSent  a + txIdBlockingReqsSent  b,
    txIdPipelinedReqsSent = txIdPipelinedReqsSent a + txIdPipelinedReqsSent b,
    txIdSlowReqsSent      = txIdSlowReqsSent      a + txIdSlowReqsSent      b,
    txIdsReceivedSlow     = txIdsReceivedSlow     a + txIdsReceivedSlow     b,
    txIdReplies0          = txIdReplies0          a + txIdReplies0          b,
    txIdReplies1          = txIdReplies1          a + txIdReplies1          b,
    txIdReplies2to4       = txIdReplies2to4       a + txIdReplies2to4       b,
    txIdReplies5to9       = txIdReplies5to9       a + txIdReplies5to9       b,
    txIdReplies10         = txIdReplies10         a + txIdReplies10         b,
    txIdBlockingWait      = txIdBlockingWait      a + txIdBlockingWait      b,
    txPipelineWait        = txPipelineWait        a + txPipelineWait        b,
    txSubmissionWait      = txSubmissionWait      a + txSubmissionWait      b
    }

instance Num t => Monoid (TxSubmissionCountersOf t) where
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
    txIdSlowReqsSent = 0,
    txIdsReceivedSlow = 0,
    txIdReplies0 = 0,
    txIdReplies1 = 0,
    txIdReplies2to4 = 0,
    txIdReplies5to9 = 0,
    txIdReplies10 = 0,
    txIdBlockingWait      = 0,
    txPipelineWait        = 0,
    txSubmissionWait      = 0
  }

-- | Convert accumulated counters for emission: the duration fields to
-- whole milliseconds, the counts unchanged.
--
-- This is the only place a duration is rounded, so the loss is at most a
-- millisecond per field on a cumulative total rather than every
-- sub-millisecond sample.
emittedCounters :: TxSubmissionCountersAcc -> TxSubmissionCounters
emittedCounters = fmap diffTimeToMilliseconds

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
--
-- Invariants (see @peerTxLocalStateInvariant@):
--
-- * 'peerRequestedTxs' is a subset of the 'peerAvailableTxIds' keys,
--   which are a subset of the 'peerUnacknowledgedTxIds' keys.
-- * 'peerDownloadedTxs' keys are a subset of the unacknowledged queue
--   and disjoint from both 'peerAvailableTxIds' and 'peerRequestedTxs':
--   a key leaves available/requested exactly when its body lands in
--   downloaded.
-- * 'peerRequestedTxs' equals the union of every batch's
--   'requestedTxBatchSet', and 'peerRequestedTxsSize' the sum of their
--   'requestedTxBatchSize'.
-- * the 'peerScore' value stays in @[0, scoreMax]@.
data PeerTxLocalState tx = PeerTxLocalState {
    -- | Coarse phase of this peer's worker thread. Updated by
    -- 'nextPeerAction' / 'nextPeerActionPipelined' when an action is
    -- chosen and inspected in 'serverIdle' to decide whether to skip
    -- 'awaitSharedChange' on an Active->Idle transition.
    peerPhase               :: !PeerPhase,

    -- | Unacknowledged txids in the order advertised by the peer.
    peerUnacknowledgedTxIds :: !(StrictSeq TxKey),

    -- | Txids this peer currently advertises and that may be requested from
    -- it if the peer becomes the owner.
    peerAvailableTxIds      :: !(IntMap SizeInBytes),

    -- | Requested txids that have not yet been replied to.
    peerRequestedTxs        :: !IntSet,

    -- | Outstanding tx-body request batches, in request order.  Their
    -- 'requestedTxBatchSet's union to 'peerRequestedTxs' and their
    -- 'requestedTxBatchSize's sum to 'peerRequestedTxsSize'.
    peerRequestedTxBatches  :: !(StrictSeq RequestedTxBatch),

    -- | Total serialized size of all outstanding requested tx bodies.
    peerRequestedTxsSize    :: !SizeInBytes,

    -- | Number of txids requested from this peer but not yet replied to
    -- (the in-flight txid-request window).
    peerRequestedTxIds      :: !NumTxIdsToReq,

    -- | Tx bodies downloaded from this peer and buffered locally until they
    -- are either submitted to the mempool or superseded by shared-state
    -- resolution.
    peerDownloadedTxs       :: !(IntMap tx),

    -- | Time at which the first outstanding body-request batch was
    -- sent in the current download episode.
    peerDownloadStartTime   :: !(Maybe Time),

    -- | Usefulness score for this peer, tracking rejection penalties and
    -- time-based decay.
    peerScore               :: !PeerScore,

    -- | The tier the coin or the peer's standing put it on, written by the
    -- registry before each decision.  A tx source is fast whatever this
    -- says (see 'peerNovelAt').
    peerTier                :: !PeerTier,

    -- | When the mempool last accepted txs this peer delivered, newest
    -- last, at most as many as 'txSourceRate' over 'txSourceWindow'
    -- amounts to: the peer is a source while the oldest is inside the
    -- window.
    peerNovelAt             :: !(StrictSeq Time),

    -- | On the slow tier, the time before which no further txid request
    -- goes out, set when one is sent.
    peerSlowGate            :: !(Maybe Time),

    -- | The generator state the slow tier's jitter is drawn from, seeded by
    -- the registry at registration.
    peerJitterSeed          :: !Word64,

    -- | The tiers the outstanding txid requests went out on, the oldest
    -- first: replies come in that order, and each reply's txids are counted
    -- on the tier that asked for them, whatever the tier is when they
    -- arrive or became while an earlier request was still out.
    peerRequestTiers        :: !(StrictSeq PeerTier)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | Record the tier a txid request goes out on, behind the outstanding
-- ones ('peerRequestTiers').
recordRequestTier :: PeerTier -> PeerTxLocalState tx -> PeerTxLocalState tx
recordRequestTier tier peerState =
  peerState { peerRequestTiers = peerRequestTiers peerState StrictSeq.|> tier }

-- | The tier the oldest outstanding txid request went out on, the one a
-- reply answers, and the state without it; 'FastTier' when none is out.
takeReplyTier :: PeerTxLocalState tx -> (PeerTier, PeerTxLocalState tx)
takeReplyTier peerState =
  case peerRequestTiers peerState of
    tier StrictSeq.:<| rest -> (tier, peerState { peerRequestTiers = rest })
    _                       -> (FastTier, peerState)

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
    peerTier                  = FastTier,
    peerNovelAt               = StrictSeq.empty,
    peerSlowGate              = Nothing,
    peerJitterSeed            = 0,
    peerRequestTiers          = StrictSeq.empty
  }

-- | Shared V2 state.
--
-- There is no global decision thread. Peer worker threads coordinate by
-- atomically reading and updating this shared state.
--
-- Invariants (see @sharedTxStateInvariant@):
--
-- * the active ('sharedTxTable') and retained ('sharedRetainedTxs') key
--   sets are disjoint.
-- * 'sharedTxIdToKey' and 'sharedKeyToTxId' are mutual inverses (equal
--   size, round-tripping in both directions).
-- * the keys of those two maps are exactly the live keys (active union
--   retained): a lookup entry lives with its residence and goes with it.
-- * 'sharedNextTxKey' is strictly greater than every live key, so a
--   freshly interned key never collides with a live one.
--
-- Cross-step (see @sharedGenerationBumpInvariant@): both counters are
-- monotone non-decreasing, any transition that changes some other field
-- must advance 'sharedRevision', and one that advances 'sharedGeneration'
-- advances 'sharedRevision' too; @writeSharedStateIfChanged@ silently
-- drops a commit that leaves the revision where it read it.
data SharedTxState peeraddr txid = SharedTxState {
    -- | Active unresolved txs that still participate in leasing, buffering
    -- and submission, and that the sweep reviews for abandonment.
    sharedTxTable     :: !(IntMap (TxEntry peeraddr)),
    -- | Accepted txs retained locally for a bounded time so later txid
    -- advertisements can be acked without re-requesting the body.
    sharedRetainedTxs :: !RetainedTxs,
    -- | Forward txid interning table: maps a raw txid to its compact
    -- 'TxKey'.  Inverse of 'sharedKeyToTxId'.
    sharedTxIdToKey   :: !(Map (RawTxId txid) TxKey),
    -- | Reverse interning table: maps a 'TxKey' back to its txid.
    -- Inverse of 'sharedTxIdToKey'.
    sharedKeyToTxId   :: !(IntMap txid),
    -- | Next fresh 'TxKey' to allocate; stays strictly greater than every
    -- live key.
    sharedNextTxKey   :: !Int,
    -- | Wake counter.  Bumped on transitions that may grant other peers a
    -- new option (lease release, new entry accepted into the mempool, cap
    -- bump, ...).  Parked peers watch this via 'awaitSharedChange'.
    sharedGeneration  :: !Word64,
    -- | Write counter.  Bumped by every step that changes the state, the
    -- ones that bump 'sharedGeneration' included; 'writeSharedStateIfChanged'
    -- writes the TVar exactly when it moved, so it counts committed writes.
    sharedRevision    :: !Word64,
    -- | Bumped when an entry becomes claimable for another peer: a lease
    -- released by a rejection or an omitted body, a stuck entry's cap
    -- raised, a leaving peer's leases scrubbed.  The slow fetch tier parks
    -- on it instead of 'sharedGeneration', which also moves on every
    -- accept, so a held txid request costs no re-evaluation per accept but
    -- a re-fetch chain still runs at round-trip speed.
    sharedClaimable   :: !Word64
  }
  deriving stock Generic

deriving stock instance (Eq peeraddr, Eq txid, HasRawTxId txid) => Eq (SharedTxState peeraddr txid)
deriving stock instance (Show peeraddr, Show txid, HasRawTxId txid) => Show (SharedTxState peeraddr txid)
deriving anyclass instance (NFData peeraddr, NFData txid, HasRawTxId txid) => NFData (SharedTxState peeraddr txid)
deriving anyclass instance (NoThunks peeraddr, NoThunks txid, HasRawTxId txid) => NoThunks (SharedTxState peeraddr txid)

-- | Keys of transactions tx-submission knows are in the mempool, kept for a
-- a limited time.
--
-- A key enters whenever we learns that the tx is in the mempool:
--
-- * a body we submitted is accepted, or
-- * a peer announces a txid, or delivers a body, that the mempool
--   already holds. Could be due to earlier fetches, or the TX may have
--   arrived through the LocalTxSubmission protocol.
--
-- In each case the key is dropped from the active 'sharedTxTable' and
-- inserted here with deadline @now + bufferedTxsMinLifetime@ (re-insert
-- keeps the later deadline).  A key leaves only when that deadline passes
-- and the shared-state sweep removes it, independent of whether peers
-- have acked it yet.
--
-- While retained a key counts as resolved: its txid may be acked to the
-- announcing peers, and a re-announced txid or late-arriving body is
-- treated as already handled rather than re-fetched.  A rejected tx is
-- not retained, it stays claimable in 'sharedTxTable' so peers retry it.
--
-- Backed by two indexes:
--
-- * 'retainedQueue' is keyed on the deadline, a 'RetainTime', for cheap
--   earliest-expiry queries: machine-word comparisons, where 'Time'
--   compares 'Integer' picoseconds.
-- * 'retainedSet'   shadows the keys for O(min(n, W)) 'retainedMember'
--   queries (the hot path: every received txid is checked here).
--
-- Both are kept in lockstep by the 'retained*' helpers below.
data RetainedTxs = RetainedTxs {
    retainedQueue :: !(IntPSQ RetainTime ()),
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
    sharedRevision         = 0,
    sharedClaimable        = 0
  }

retainedEmpty :: RetainedTxs
retainedEmpty = RetainedTxs IntPSQ.empty IntSet.empty

-- | An instant on the retention clock: whole nanoseconds of monotonic
-- time, a deadline rounded up ('retainDeadline') or a current time rounded
-- down ('retainNow').  Only ever compared, never computed with, so that a
-- revision or a count cannot stand in for it.
newtype RetainTime = RetainTime Word64
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, Bounded, NFData, NoThunks)

-- | The instant in seconds, for traces.
retainTimeSeconds :: RetainTime -> Double
retainTimeSeconds (RetainTime ns) = fromIntegral ns / 1e9

-- | A retention deadline: 'Time' in nanoseconds, rounded up, so that with
-- 'retainNow' rounding down an entry never expires before its deadline.
-- 'Word64' nanoseconds of monotonic time last 584 years; a negative time,
-- as a non-positive lifetime gives, is due at once, and a time past the
-- range is due at its end, rather than wrapping.
retainDeadline :: Time -> RetainTime
retainDeadline (Time t) =
  RetainTime (clampNanoseconds ((diffTimeToPicoseconds t + 999) `div` 1000))

-- | The current time for expiry: 'Time' in nanoseconds, rounded down.
retainNow :: Time -> RetainTime
retainNow (Time t) = RetainTime (clampNanoseconds (diffTimeToPicoseconds t `div` 1000))

-- | Nanoseconds as a 'Word64', saturating at either end.
clampNanoseconds :: Integer -> Word64
clampNanoseconds = fromInteger . max 0 . min (toInteger (maxBound :: Word64))

retainedSingleton :: Int -> RetainTime -> RetainedTxs
retainedSingleton k retainUntil =
    RetainedTxs (IntPSQ.insert k retainUntil () IntPSQ.empty)
                (IntSet.singleton k)

retainedFromList :: [(Int, RetainTime)] -> RetainedTxs
retainedFromList =
    List.foldl' (\retained (k, retainUntil) -> retainedInsertMax k retainUntil retained)
                retainedEmpty

retainedToList :: RetainedTxs -> [(Int, RetainTime)]
retainedToList =
    sortOn fst
  . fmap (\(k, retainUntil, ()) -> (k, retainUntil))
  . IntPSQ.toList
  . retainedQueue

retainedSize :: RetainedTxs -> Int
retainedSize = IntSet.size . retainedSet
{-# INLINE retainedSize #-}

retainedLookup :: Int -> RetainedTxs -> Maybe RetainTime
retainedLookup k retained =
    fmap fst (IntPSQ.lookup k (retainedQueue retained))
{-# INLINE retainedLookup #-}

retainedMember :: Int -> RetainedTxs -> Bool
retainedMember k = IntSet.member k . retainedSet
{-# INLINE retainedMember #-}

retainedInsertMax :: Int -> RetainTime -> RetainedTxs -> RetainedTxs
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

-- | The keys whose retention has expired by @currentTime@, earliest
-- first, at most @limit@ of them (none for a limit of zero or less), and
-- the retained entries without them.  Bounds the work of one sweep, so a
-- backlog of expired entries is drained over several sweeps rather than in
-- one transaction that grows with it; the queue is unwound once, for the
-- keys and the remainder together.
retainedExpireUpTo :: Int -> RetainTime -> RetainedTxs -> (IntSet, RetainedTxs)
retainedExpireUpTo limit currentTime retained@(RetainedTxs queue keys)
  | limit > 0, retainedDueBy currentTime retained = go limit IntSet.empty queue
  | otherwise                                     = (IntSet.empty, retained)
  where
    go !n !expired r
      | n <= 0 = done expired r
      | otherwise =
          case IntPSQ.minView r of
            Just (k, retainUntil, (), r')
              | retainUntil <= currentTime ->
                  go (n - 1) (IntSet.insert k expired) r'
            _ -> done expired r
    done expired r = (expired, RetainedTxs r (keys `IntSet.difference` expired))
{-# INLINE retainedExpireUpTo #-}

-- | Where a bounded sweep goes on from: the next key to examine in
-- 'sharedTxTable'.
newtype SweepCursor = SweepCursor { sweepFrom :: Int }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, NoThunks)

-- | The cursor at the start of the table.
sweepStart :: SweepCursor
sweepStart = SweepCursor 0

-- | The sizes a 'SharedTxState' is traced by, each under its field's name
-- ('sharedTxStateCountFields').
data SharedTxStateCounts = SharedTxStateCounts {
    -- | Entries of 'sharedTxTable'.
    activeTxCount     :: !Int,
    -- | Entries of 'sharedRetainedTxs'.
    retainedTxCount   :: !Int,
    -- | Lookup entries.
    internedTxCount   :: !Int,
    leasedTxCount     :: !Int,
    claimableTxCount  :: !Int,
    totalAttemptCount :: !Int,
    submittingTxCount :: !Int,
    -- | Active entries under review for abandonment ('Doomed').
    doomedTxCount     :: !Int
  }
  deriving stock (Eq, Show, Generic)

sharedTxStateCounts :: SharedTxState peeraddr txid -> SharedTxStateCounts
sharedTxStateCounts SharedTxState { sharedTxTable, sharedRetainedTxs,
                                    sharedTxIdToKey } =
  SharedTxStateCounts {
      activeTxCount     = active,
      retainedTxCount   = retained,
      internedTxCount   = interned,
      leasedTxCount     = leased,
      claimableTxCount  = claimable,
      totalAttemptCount = attempts,
      submittingTxCount = submitting,
      doomedTxCount     = doomed
    }
  where
    active   = IntMap.size sharedTxTable
    retained = retainedSize sharedRetainedTxs
    interned = Map.size sharedTxIdToKey

    (leased, claimable, attempts, submitting, doomed) =
      IntMap.foldl' count (0, 0, 0, 0, 0) sharedTxTable

    count (!l, !c, !a, !s, !d) TxEntry { txLease, txAttempt, txInSubmission, txDoom } =
      ( case txLease of TxLeased {}    -> l + 1
                        TxClaimable {} -> l
      , case txLease of TxClaimable {} -> c + 1
                        TxLeased {}    -> c
      , a + txAttempt
      , if txInSubmission then s + 1 else s
      , if txDoom == Doomed then d + 1 else d
      )

-- | The counts by their fields' names, the names they are traced under,
-- listed by hand: a field added to the record without a line here is
-- missing from the traces, not a failure inside the counters thread.
sharedTxStateCountFields :: SharedTxStateCounts -> [(String, Int)]
sharedTxStateCountFields counts =
  [ ("activeTxCount",     activeTxCount     counts)
  , ("retainedTxCount",   retainedTxCount   counts)
  , ("internedTxCount",   internedTxCount   counts)
  , ("leasedTxCount",     leasedTxCount     counts)
  , ("claimableTxCount",  claimableTxCount  counts)
  , ("totalAttemptCount", totalAttemptCount counts)
  , ("submittingTxCount", submittingTxCount counts)
  , ("doomedTxCount",     doomedTxCount     counts)
  ]

-- | The tier census counts by their fields' names, the names they are
-- traced under, listed by hand like 'sharedTxStateCountFields'.
tierCensusFields :: TierCensus -> [(String, Int)]
tierCensusFields census =
  [ ("fastByRoot",   censusFastByRoot   census)
  , ("fastByCoin",   censusFastByCoin   census)
  , ("fastBySource", censusFastBySource census)
  , ("slow",         censusSlow         census)
  ]

-- | Whether any retained entry is due by @currentTime@.
retainedDueBy :: RetainTime -> RetainedTxs -> Bool
retainedDueBy currentTime retained =
    case IntPSQ.findMin (retainedQueue retained) of
      Just (_, earliest, _) -> earliest <= currentTime
      Nothing               -> False
{-# INLINE retainedDueBy #-}

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

-- | Intern a txid into the shared lookup tables.  Returns the txid's
-- raw form, its stable 'TxKey' (existing if already interned, freshly
-- allocated otherwise), and the resulting shared state.
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
    step (!acc, !st) txid =
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
    TraceSharedTxState (SharedTxState peeraddr txid) SharedTxStateCounts
    -- ^ the state and its counts, folded once by the counters thread for
    -- every renderer
  | TraceTxTiers TierCensus
    -- ^ the live peers by the standing they are served on
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
