{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Policy
  ( TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  , saneTxDecisionPolicy
  , TxSubmissionConfig (..)
  , defaultTxSubmissionConfigV2
  , max_TX_SIZE
  , max_TXID_SIZE
    -- * Re-exports
  , NumTxIdsToReq (..)
  ) where

import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.Limits (sizeLimitForState)
import Ouroboros.Network.Protocol.TxSubmission2.Codec (byteLimitsTxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..), SingTxSubmission (SingIdle, SingTxs))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))


-- | Maximal tx size.
--
-- It bounds the advertised tx size: the inbound side disconnects from
-- a peer which advertises a larger tx (`ProtocolErrorTxSizeTooLarge`).
-- Increasing it requires a new node-to-node version.
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

defaultTxDecisionPolicy :: TxSubmissionConfig -> TxDecisionPolicy
defaultTxDecisionPolicy config =
    TxDecisionPolicy {
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
  where
    -- for `~2.5MB`, leave 50KB room
    safeLimit :: SizeInBytes -> SizeInBytes
    safeLimit limit = limit - limit `div` 50

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

-- | Sanity check for a 'TxDecisionPolicy': 'True' when every field lies
-- in the range the decision logic assumes.
--
-- All window and size limits must be positive (otherwise the peer can make
-- no progress), the rate, score and delay parameters must
-- be non-negative, and 'inflightTimeout' must be at least 'interTxSpace'
-- (their difference is used as a non-negative delay when bumping a stuck
-- entry's inflight-multiplicity cap).  'defaultTxDecisionPolicy' satisfies
-- it.  `txsSizePerBatch` MUST be equal or greater than `max_TX_SIZE`.
-- A `MsgRequestTxs` with `maxNumUnacknowledgedTxIds` txids of size
-- `max_TXID_SIZE` MUST fit within the `StIdle` size limit.
saneTxDecisionPolicy :: TxDecisionPolicy -> TxSubmissionConfig -> Bool
saneTxDecisionPolicy p c =
       maxNumUnacknowledgedTxIds      c >= 1
       -- `MsgRequestTxs` has four bytes of CBOR framing: the outer list,
       -- message tag, indefinite txid list and break marker.
    &&    4 + fromIntegral max_TXID_SIZE * fromIntegral (maxNumUnacknowledgedTxIds c)
       <= toInteger (sizeLimitForState
                      (byteLimitsTxSubmission2 (error "invariant violation!"))
                      SingIdle)
    &&    fromIntegral  max_TX_SIZE * fromIntegral (maxNumTxIdsToRequest c)
       <= fromIntegral @SizeInBytes @Integer maxBound
    && maxNumTxIdsToRequest           c >= 1
    && txsSizeInflightPerPeer         p >  0
    && txsSizePerBatch                p >= max_TX_SIZE
    && maxOutstandingTxBatchesPerPeer p >= 1
    && txInflightMultiplicity         p >= 1
    && bufferedTxsMinLifetime         p >= 0
    && scoreRate                      p >= 0
    && scoreMax                       p >= 0
    && scoreAcceptDecrement           p >= 0
    && interTxSpace                   p >= 0
    && inflightTimeout                p >= interTxSpace p
    && maxPeerClaimDelay              p >= 0
