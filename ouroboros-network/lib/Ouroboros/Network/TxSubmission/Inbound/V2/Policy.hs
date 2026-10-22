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
  , max_TX_SIZE
    -- * Re-exports
  , NumTxIdsToReq (..)
  ) where

import Control.DeepSeq
import Control.Exception (Exception, assert)
import Control.Monad (unless)
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.Limits (sizeLimitForState)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (byteLimitsTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..), SingTxSubmission (SingTxs))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))


-- | Maximal tx size.
--
-- Affects:
--
-- * `TxDecisionPolicy`
-- * `maximumIngressQueue` for `tx-submission` mini-protocol, see
--   `Ouroboros.Network.NodeToNode.txSubmissionProtocolLimits`
--
max_TX_SIZE :: SizeInBytes
max_TX_SIZE = 65_540


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
      -- request batch. The default reserves space for reply framing.
      -- MUST be equal or larger than a maximum tx size (see `max_TX_SIZE`).

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

      disablePipelinedTxIdRequests   :: !Bool
      -- ^ When 'True', the txid picker never issues pipelined
      -- @MsgRequestTxIds@ messages; only blocking requests fire (and
      -- only when the unack window has been fully drained). Used
      -- by some benchmarks.
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
      txsSizeInflightPerPeer =
        assert (    fromIntegral max_TX_SIZE * fromIntegral (maxNumTxIdsToRequest config)
                 <= fromIntegral @SizeInBytes @Integer maxBound
               )
        max_TX_SIZE * (fromIntegral (maxNumTxIdsToRequest config)),
      -- MsgReplyTxs has four bytes of CBOR framing: the outer list, message
      -- tag, indefinite transaction list and break marker.
      txsSizePerBatch =
        let sizePerBatch =
                safeLimit
              $ fromIntegral
                  (sizeLimitForState
                    (byteLimitsTxSubmission2 (error "invariant violation!"))
                    SingTxs)
              - 4
        in assert (sizePerBatch >= max_TX_SIZE)
           sizePerBatch,
      maxOutstandingTxBatchesPerPeer = 4,
      txInflightMultiplicity = 2,
      bufferedTxsMinLifetime = 2,
      scoreRate              = 0.001,
      scoreMax               = 15 * 60,
      scoreAcceptDecrement   = 3,
      interTxSpace           = 0.250,
      inflightTimeout        = 0.600,
      maxPeerClaimDelay      = 0.250,
      disablePipelinedTxIdRequests = False
    }
    -- for `~2.5MB`, leave 50KB room
    safeLimit :: SizeInBytes -> SizeInBytes
    safeLimit limit = limit - limit `div` 50

-- | Shared options between the inbound and outbound side.
--
data TxSubmissionConfig =
    TxSubmissionConfig {
      maxNumUnacknowledgedTxIds :: !NumTxIdsToAck,
      -- ^ maximum number of unacknowledged txids; it MUST be positive.
      -- Before `NodeToNodeV_16` it is a protocol parameter and must not be
      -- changed; since `NodeToNodeV_16` it is a local setting.
      maxNumTxIdsToRequest      :: !NumTxIdsToReq
      -- ^ maximum number of txids an inbound client can request at a time.
    }
  deriving (Eq, Show)

instance NFData TxSubmissionConfig where
  rnf TxSubmissionConfig{} = ()

-- | `TxSubmissionConfig` for `TxSubmissionLogicV2`.
--
defaultTxSubmissionConfigV2 :: TxSubmissionConfig
defaultTxSubmissionConfigV2 = TxSubmissionConfig {
    maxNumUnacknowledgedTxIds = NumTxIdsToAck 10,
    maxNumTxIdsToRequest      = NumTxIdsToReq 6
  }

-- | Why a `TxDecisionPolicy` with a `TxSubmissionConfig` isn't sane, see
-- `saneTxDecisionPolicy`.  Each constructor carries the offending values.
--
data TxConfigurationError
  -- | `maxNumUnacknowledgedTxIds` must be positive
  = NumUnacknowledgedTxIdsTooSmall NumTxIdsToAck
  -- | `maxNumTxIdsToRequest` must be positive
  | MaxNumTxIdsToRequestTooSmall NumTxIdsToReq
  -- | @max_TX_SIZE * maxNumTxIdsToRequest@ doesn't fit in `SizeInBytes`
  | MaxNumTxIdsToRequestTooLarge NumTxIdsToReq
  -- | `txsSizeInflightPerPeer` must be positive
  | TxsSizeInflightPerPeerTooSmall SizeInBytes
  -- | `txsSizePerBatch` must be positive
  | TxsSizePerBatchTooSmall SizeInBytes
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
-- it.  `txsSizePerBatch` MUST be equal or greater than `max_TX_SIZE`.
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
                       maxPeerClaimDelay
                     }
                     TxSubmissionConfig {
                       maxNumUnacknowledgedTxIds,
                       maxNumTxIdsToRequest
                     } = do
    unless (maxNumUnacknowledgedTxIds >= 1)
      $ Left (NumUnacknowledgedTxIdsTooSmall maxNumUnacknowledgedTxIds)
    unless (maxNumTxIdsToRequest >= 1)
      $ Left (MaxNumTxIdsToRequestTooSmall maxNumTxIdsToRequest)
    unless (    toInteger max_TX_SIZE * toInteger maxNumTxIdsToRequest
             <= toInteger (maxBound :: SizeInBytes))
      $ Left (MaxNumTxIdsToRequestTooLarge maxNumTxIdsToRequest)
    unless (txsSizeInflightPerPeer > 0)
      $ Left (TxsSizeInflightPerPeerTooSmall txsSizeInflightPerPeer)
    unless (txsSizePerBatch > 0)
      $ Left (TxsSizePerBatchTooSmall txsSizePerBatch)
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
