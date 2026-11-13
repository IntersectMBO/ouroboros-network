{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Policy
  ( TxDecisionPolicy (..)
  , mkTxDecisionPolicy
  , saneTxDecisionPolicy
  , TxConfigurationError (..)
  , TxSubmissionConfig (..)
  , defaultTxSubmissionConfigV2
  , maxSourceWindow
  , max_TX_SIZE
  , max_TXID_SIZE
    -- * Re-exports
  , NumTxIdsToReq (..)
  ) where

import Control.DeepSeq
import Control.Exception (Exception)
import Control.Monad (unless)
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.Limits (sizeLimitForState)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (byteLimitsTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..), SingTxSubmission (SingIdle, SingTxs))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (const_MAX_TX_SIZE_DISCREPANCY)


-- | Maximal tx size.
--
-- It bounds the advertised tx size on every node-to-node version: the V2
-- inbound disconnects from a peer which advertises a larger tx
-- (`ProtocolErrorTxSizeTooLarge`).  The bound is far above the ledger's
-- maximal tx size, and every version was already limited by it through the
-- ingress queue of the mini-protocol (see below).  Increasing it requires
-- a new node-to-node version, since nodes running older versions disconnect
-- from peers which advertise larger txs.
--
-- Affects:
--
-- * `TxDecisionPolicy`
-- * `maximumIngressQueue` for `tx-submission` mini-protocol, see
--   `Ouroboros.Network.NodeToNode.txSubmissionProtocolLimits`
--
max_TX_SIZE :: SizeInBytes
max_TX_SIZE = 65_540


-- | Upper bound on the size of an encoded txid, the same as used by
-- `Cardano.Network.NodeToNode.txSubmissionProtocolLimits`.
--
-- It bounds `maxNumUnacknowledgedTxIds`: a `MsgRequestTxs` can carry up to
-- `maxNumUnacknowledgedTxIds` txids and must fit within the `StIdle` size
-- limit, see `saneTxDecisionPolicy`.
--
max_TXID_SIZE :: SizeInBytes
max_TXID_SIZE = 44


-- | Policy for making decisions
--
data TxDecisionPolicy = TxDecisionPolicy {
      --
      -- Configuration of tx decision logic.
      --

      txsSizeInflightPerPeer         :: !SizeInBytes,
      -- ^ a limit of tx size in-flight from a single peer.
      -- It can be exceed by max tx size.

      txsSizePerBatch                :: !SizeInBytes,
      -- ^ Hard limit on the total advertised transaction size in one body
      -- request batch, except for the first tx in a batch, which is
      -- requested even if it is larger.  Hence a batch is at most
      -- `max txsSizePerBatch max_TX_SIZE`.  The default reserves space for
      -- reply framing and for the `const_MAX_TX_SIZE_DISCREPANCY` tolerance
      -- of each tx.  It MUST be positive, and the reply to the largest batch
      -- MUST fit within the `StTxs` size limit (see `saneTxDecisionPolicy`).
      -- It can be smaller than `max_TX_SIZE`: a larger tx is requested in
      -- a batch on its own.

      maxOutstandingTxBatchesPerPeer :: !Int,
      -- ^ a limit of outstanding tx-body request batches from a single peer.

      txInflightMultiplicity         :: !Int,
      -- ^ from how many peers download the `txid` simultaneously

      bufferedTxsMinLifetime         :: !DiffTime,
      -- ^ how long TXs that have been added to the mempool will be
      -- kept in the `bufferedTxs` cache.

      scoreRate                      :: !Double,
      -- ^ rate at which "rejected" TXs drain. Unit: TX/seconds.

      scoreMax                       :: !Double,
      -- ^ Maximum number of "rejections". Unit: seconds

      scoreAcceptDecrement           :: !Double,
      -- ^ amount subtracted from the peer's score for each tx body the peer delivered that the mempool accepted.

      interTxSpace                   :: !DiffTime,
      -- ^ space between actual requests for the same TX. This the time a peer has soul ownership
      -- of their TX lease. When it expires other peers may issue additional requests.

      inflightTimeout                :: !DiffTime,
      -- ^ Maximum time a peer's attempt may sit between claim and
      -- entering submission before the per-entry inflight-multiplicity
      -- cap is bumped, allowing another peer to attempt in parallel.
      maxPeerClaimDelay              :: !DiffTime,
      -- ^ Maximum delay penalty for poor performing peers.

      -- Fetch tiers: see 'Ouroboros.Network.TxSubmission.Inbound.V2.Tier'.
      --
      txFastPeers                    :: !Int,
      -- ^ how many inbound peers the salted order keeps on the fast tier,
      -- polled for txids as soon as there is something to acknowledge.
      -- Local roots and tx sources are fast besides.
      txSlowCadence                  :: !DiffTime,
      -- ^ the least time between two txid requests to a slow-tier peer,
      -- blocking ones included, before jitter.
      txSlowJitter                   :: !Double,
      -- ^ the fraction of 'txSlowCadence' each wait is jittered by, drawn
      -- afresh per request, so slow peers never fire together.  In @[0, 1)@.
      txTierRotation                 :: !DiffTime,
      -- ^ how often the coin draws a new salt, reordering the fast tier.
      txSourceCount                  :: !Int,
      -- ^ a peer that delivered this many novel accepted txs inside
      -- 'txSourceWindow' is a tx source and fast while it lasts.  Zero
      -- disables the rule.
      txSourceWindow                 :: !DiffTime
      -- ^ the window 'txSourceCount' is counted over.
    }
  deriving (Eq, Show)

instance NFData TxDecisionPolicy where
  rnf TxDecisionPolicy{} = ()

-- | The default policy for the given config.  Returns a `TxConfigurationError`
-- if it isn't sane (see `saneTxDecisionPolicy`); the node should then report
-- it and halt.
--
mkTxDecisionPolicy
  :: TxSubmissionConfig
  -> Either TxConfigurationError TxDecisionPolicy
mkTxDecisionPolicy config =
    policy <$ saneTxDecisionPolicy policy config
  where
    policy = TxDecisionPolicy {
      -- Saturates rather than wraps around if `maxNumTxIdsToRequest` is too
      -- large (such a config does not pass `saneTxDecisionPolicy`).
      txsSizeInflightPerPeer =
        fromIntegral @Integer @SizeInBytes
          $ min (toInteger (maxBound :: SizeInBytes))
                (toInteger max_TX_SIZE * toInteger (maxNumTxIdsToRequest config)),
      txsSizePerBatch =
          safeLimit
        $ fromIntegral stTxsSizeLimit
        - msgReplyTxsFraming,
      maxOutstandingTxBatchesPerPeer = 4,
      txInflightMultiplicity = 2,
      bufferedTxsMinLifetime = 2,
      scoreRate              = 0.001,
      scoreMax               = 15 * 60,
      scoreAcceptDecrement   = 3,
      interTxSpace           = 0.250,
      inflightTimeout        = 0.600,
      maxPeerClaimDelay      = 0.250,
      txFastPeers            = 20,
      txSlowCadence          = 0.500,
      txSlowJitter           = 0.2,
      txTierRotation         = 293,
      txSourceCount          = 10,
      txSourceWindow         = 10
    }
    -- for `~2.5MB`, leave 50KB room
    safeLimit :: SizeInBytes -> SizeInBytes
    safeLimit limit = limit - limit `div` 50

-- | `MsgReplyTxs` has four bytes of CBOR framing: the outer list, message
-- tag, indefinite transaction list and break marker.
--
msgReplyTxsFraming :: SizeInBytes
msgReplyTxsFraming = 4

-- | `MsgRequestTxs` has four bytes of CBOR framing: the outer list, message
-- tag, indefinite txid list and break marker.
--
msgRequestTxsFraming :: SizeInBytes
msgRequestTxsFraming = 4

-- | Size limit of the `StTxs` state, i.e. of a `MsgReplyTxs` message.
--
stTxsSizeLimit :: Word
stTxsSizeLimit =
    sizeLimitForState
      (byteLimitsTxSubmission2 (error "invariant violation!"))
      SingTxs

-- | Size limit of the `StIdle` state, i.e. of a `MsgRequestTxs` message.
--
stIdleSizeLimit :: Word
stIdleSizeLimit =
    sizeLimitForState
      (byteLimitsTxSubmission2 (error "invariant violation!"))
      SingIdle

-- | Shared options between the inbound and outbound side.
--
data TxSubmissionConfig =
    TxSubmissionConfig {
      maxNumUnacknowledgedTxIds :: !NumTxIdsToAck,
      -- ^ maximum number of unacknowledged txids; it MUST be positive, and
      -- small enough for a `MsgRequestTxs` with that many txids to fit within
      -- the `StIdle` size limit (see `max_TXID_SIZE`).  Before
      -- `NodeToNodeV_16` it is a protocol parameter and must not be changed;
      -- since `NodeToNodeV_16` it is a local setting.
      maxNumTxIdsToRequest      :: !NumTxIdsToReq
      -- ^ maximum number of txids an inbound client can request at a time;
      -- a request is sized by the window's room as well, so a cap above
      -- the window never binds.  Best kept at the window while a slow
      -- fetch tier exists, so a slow peer hands over what accumulated
      -- during its wait in one exchange; below it the peer catches up only
      -- as blocks clear its mempool.
    }
  deriving (Eq, Show)

instance NFData TxSubmissionConfig where
  rnf TxSubmissionConfig{} = ()

-- | `TxSubmissionConfig` for `TxSubmissionLogicV2`.
--
defaultTxSubmissionConfigV2 :: TxSubmissionConfig
defaultTxSubmissionConfigV2 = TxSubmissionConfig {
    maxNumUnacknowledgedTxIds = NumTxIdsToAck 10,
    maxNumTxIdsToRequest      = NumTxIdsToReq 10
  }

-- | Why a `TxDecisionPolicy` with a `TxSubmissionConfig` isn't sane, see
-- `saneTxDecisionPolicy`.  Each constructor carries the offending values.
--
data TxConfigurationError
  -- | `maxNumUnacknowledgedTxIds` must be positive
  = NumUnacknowledgedTxIdsTooSmall NumTxIdsToAck
  -- | a `MsgRequestTxs` with `maxNumUnacknowledgedTxIds` txids of size
  -- `max_TXID_SIZE` doesn't fit within the `StIdle` size limit
  | NumUnacknowledgedTxIdsTooLarge NumTxIdsToAck
  -- | `maxNumTxIdsToRequest` must be positive
  | MaxNumTxIdsToRequestTooSmall NumTxIdsToReq
  -- | @max_TX_SIZE * maxNumTxIdsToRequest@ doesn't fit in `SizeInBytes`
  | MaxNumTxIdsToRequestTooLarge NumTxIdsToReq
  -- | `txsSizeInflightPerPeer` must be positive
  | TxsSizeInflightPerPeerTooSmall SizeInBytes
  -- | `txsSizePerBatch` must be positive
  | TxsSizePerBatchTooSmall SizeInBytes
  -- | the reply to the largest batch, with `maxNumUnacknowledgedTxIds` txs
  -- each up to `const_MAX_TX_SIZE_DISCREPANCY` larger than advertised,
  -- doesn't fit within the `StTxs` size limit
  | TxsSizePerBatchTooLarge SizeInBytes NumTxIdsToAck
  -- | `maxOutstandingTxBatchesPerPeer` must be positive
  | MaxOutstandingTxBatchesPerPeerTooSmall Int
  -- | `txInflightMultiplicity` must be positive
  | TxInflightMultiplicityTooSmall Int
  -- | `bufferedTxsMinLifetime` must be non-negative
  | BufferedTxsMinLifetimeTooSmall DiffTime
  -- | `scoreRate` must be non-negative
  | ScoreRateTooSmall Double
  -- | `scoreMax` must be non-negative
  | ScoreMaxTooSmall Double
  -- | `scoreAcceptDecrement` must be non-negative
  | ScoreAcceptDecrementTooSmall Double
  -- | `interTxSpace` must be non-negative
  | InterTxSpaceTooSmall DiffTime
  -- | `inflightTimeout` (the first field) must be at least `interTxSpace`
  -- (the second field)
  | InflightTimeoutTooSmall DiffTime DiffTime
  -- | `maxPeerClaimDelay` must be non-negative
  | MaxPeerClaimDelayTooSmall DiffTime
  -- | `txFastPeers` must be non-negative
  | TxFastPeersTooSmall Int
  -- | `txSlowCadence` must be non-negative
  | TxSlowCadenceTooSmall DiffTime
  -- | `txSlowJitter` must be non-negative
  | TxSlowJitterTooSmall Double
  -- | `txSlowJitter` must be below one
  | TxSlowJitterTooLarge Double
  -- | `txTierRotation` must be positive
  | TxTierRotationTooSmall DiffTime
  -- | `txSourceCount` must be non-negative
  | TxSourceCountTooSmall Int
  -- | `txSourceWindow` must be positive
  | TxSourceWindowTooSmall DiffTime
  -- | `txSourceWindow` must be at most `maxSourceWindow`
  | TxSourceWindowTooLarge DiffTime
  deriving (Eq, Show)

instance Exception TxConfigurationError

-- | Sanity check for a 'TxDecisionPolicy': 'Right' when every field lies
-- in the range the decision logic assumes, otherwise the first violated
-- bound as a 'TxConfigurationError'.
--
-- All window and size limits must be positive (otherwise the peer can make
-- no progress), the rate, score and delay parameters must
-- be non-negative, and 'inflightTimeout' must be at least 'interTxSpace'
-- (their difference is used as a non-negative delay when bumping a stuck
-- entry's inflight-multiplicity cap).  'mkTxDecisionPolicy' satisfies
-- it.  The reply to the largest batch MUST fit within the `StTxs` size
-- limit.  A batch can exceed `txsSizePerBatch` by being a single tx (of at
-- most `max_TX_SIZE`), so its advertised size is at most `max txsSizePerBatch
-- max_TX_SIZE`.  It holds at most `maxNumUnacknowledgedTxIds` txs, and each
-- received tx can be larger than advertised by up to
-- `const_MAX_TX_SIZE_DISCREPANCY`. A `MsgRequestTxs` with
-- `maxNumUnacknowledgedTxIds` txids of size `max_TXID_SIZE` MUST fit within
-- the `StIdle` size limit. `max_TX_SIZE * maxNumTxIdsToRequest` MUST fit in
-- `SizeInBytes`.  The tier fields must be non-negative, the jitter below one
-- (a wait at or above the cadence's whole would turn negative and the slow
-- gate open), the rotation and source window positive and the window at
-- most 'maxSourceWindow'.  'txCountersThreadV2' refuses a policy and config
-- that fail it.
saneTxDecisionPolicy :: TxDecisionPolicy
                     -> TxSubmissionConfig
                     -> Either TxConfigurationError ()
saneTxDecisionPolicy TxDecisionPolicy {
                       txsSizeInflightPerPeer,
                       txsSizePerBatch,
                       maxOutstandingTxBatchesPerPeer,
                       txInflightMultiplicity,
                       bufferedTxsMinLifetime,
                       scoreRate,
                       scoreMax,
                       scoreAcceptDecrement,
                       interTxSpace,
                       inflightTimeout,
                       maxPeerClaimDelay,
                       txFastPeers,
                       txSlowCadence,
                       txSlowJitter,
                       txTierRotation,
                       txSourceCount,
                       txSourceWindow
                     }
                     TxSubmissionConfig {
                       maxNumUnacknowledgedTxIds,
                       maxNumTxIdsToRequest
                     } = do
    unless (maxNumUnacknowledgedTxIds >= 1)
      $ Left (NumUnacknowledgedTxIdsTooSmall maxNumUnacknowledgedTxIds)
    unless (   toInteger msgRequestTxsFraming
             + toInteger max_TXID_SIZE * toInteger maxNumUnacknowledgedTxIds
            <= toInteger stIdleSizeLimit)
      $ Left (NumUnacknowledgedTxIdsTooLarge maxNumUnacknowledgedTxIds)
    unless (maxNumTxIdsToRequest >= 1)
      $ Left (MaxNumTxIdsToRequestTooSmall maxNumTxIdsToRequest)
    unless (    toInteger max_TX_SIZE * toInteger maxNumTxIdsToRequest
             <= toInteger (maxBound :: SizeInBytes))
      $ Left (MaxNumTxIdsToRequestTooLarge maxNumTxIdsToRequest)
    unless (txsSizeInflightPerPeer > 0)
      $ Left (TxsSizeInflightPerPeerTooSmall txsSizeInflightPerPeer)
    unless (txsSizePerBatch > 0)
      $ Left (TxsSizePerBatchTooSmall txsSizePerBatch)
    unless (   toInteger msgReplyTxsFraming
             + toInteger (max txsSizePerBatch max_TX_SIZE)
             + toInteger const_MAX_TX_SIZE_DISCREPANCY
               * toInteger maxNumUnacknowledgedTxIds
            <= toInteger stTxsSizeLimit)
      $ Left (TxsSizePerBatchTooLarge txsSizePerBatch maxNumUnacknowledgedTxIds)
    unless (maxOutstandingTxBatchesPerPeer >= 1)
      $ Left (MaxOutstandingTxBatchesPerPeerTooSmall maxOutstandingTxBatchesPerPeer)
    unless (txInflightMultiplicity >= 1)
      $ Left (TxInflightMultiplicityTooSmall txInflightMultiplicity)
    unless (bufferedTxsMinLifetime >= 0)
      $ Left (BufferedTxsMinLifetimeTooSmall bufferedTxsMinLifetime)
    unless (scoreRate >= 0)
      $ Left (ScoreRateTooSmall scoreRate)
    unless (scoreMax >= 0)
      $ Left (ScoreMaxTooSmall scoreMax)
    unless (scoreAcceptDecrement >= 0)
      $ Left (ScoreAcceptDecrementTooSmall scoreAcceptDecrement)
    unless (interTxSpace >= 0)
      $ Left (InterTxSpaceTooSmall interTxSpace)
    unless (inflightTimeout >= interTxSpace)
      $ Left (InflightTimeoutTooSmall inflightTimeout interTxSpace)
    unless (maxPeerClaimDelay >= 0)
      $ Left (MaxPeerClaimDelayTooSmall maxPeerClaimDelay)
    unless (txFastPeers >= 0)
      $ Left (TxFastPeersTooSmall txFastPeers)
    unless (txSlowCadence >= 0)
      $ Left (TxSlowCadenceTooSmall txSlowCadence)
    unless (txSlowJitter >= 0)
      $ Left (TxSlowJitterTooSmall txSlowJitter)
    unless (txSlowJitter < 1)
      $ Left (TxSlowJitterTooLarge txSlowJitter)
    unless (txTierRotation > 0)
      $ Left (TxTierRotationTooSmall txTierRotation)
    unless (txSourceCount >= 0)
      $ Left (TxSourceCountTooSmall txSourceCount)
    unless (txSourceWindow > 0)
      $ Left (TxSourceWindowTooSmall txSourceWindow)
    unless (txSourceWindow <= maxSourceWindow)
      $ Left (TxSourceWindowTooLarge txSourceWindow)

-- | The longest source window 'saneTxDecisionPolicy' accepts: an hour.
-- A source window is seconds to minutes; a longer one is a unit error
-- (ten seconds given in microseconds), and the record a peer keeps of its
-- accepts ('peerNovelAt') grows with the window.
maxSourceWindow :: DiffTime
maxSourceWindow = 3600
