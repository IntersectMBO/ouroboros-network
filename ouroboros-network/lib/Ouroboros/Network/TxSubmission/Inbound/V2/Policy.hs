{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Policy
  ( TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  , saneTxDecisionPolicy
  , TxSubmissionConfig (..)
  , TxSubmissionProtocolVersion (..)
  , defaultTxSubmissionConfigV2
  , max_TX_SIZE
    -- * Re-exports
  , NumTxIdsToReq (..)
  ) where

import Control.DeepSeq
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..),
           NumTxIdsToReq (..), NumTxsToReq (..))
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
    txsSizeInflightPerPeer = max_TX_SIZE * (fromIntegral (maxNumTxIdsToRequest config)),
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

-- | Shared options between the inbound and outbound side.
--
data TxSubmissionConfig =
    TxSubmissionConfig {
      maxNumUnacknowledgedTxIds :: !NumTxIdsToAck,
      -- ^ this is a protocol parameter which sets how large the buffers could
      -- be; must not be changed without a new node-to-node version
      maxNumTxIdsToRequest      :: !NumTxIdsToReq,
      -- ^ number of txids an outbound server can respond with and at the same
      -- time number of txids an inbound client can request.
      maxNumTxsToRequest        ::  NumTxsToReq
      -- ^ number of txs a `TxSubmissionLogicV1` inbound client can request at a time
      -- TODO: remove when the `TxSubmissionLogicV1` is removed
    }
  deriving (Eq, Show)

instance NFData TxSubmissionConfig where
  rnf TxSubmissionConfig{} = ()

-- | `TxSubmissionConfig` for `TxSubmissionLogicV2`.
--
defaultTxSubmissionConfigV2 :: TxSubmissionConfig
defaultTxSubmissionConfigV2 = TxSubmissionConfig {
    maxNumUnacknowledgedTxIds = NumTxIdsToAck 10,
    maxNumTxIdsToRequest      = NumTxIdsToReq 6,
    maxNumTxsToRequest        = error "TxLogic.V2 should not use this value"
  }

-- | Protocol version.
data TxSubmissionProtocolVersion =
    TxSubmisionProtocolVersion_1
    -- The outbound side is enforcing fixed `TxSubmissionConfig`
  | TxSubmissionProtocolVersion_2
    -- The outbound side responds according to `TxSubmissionConfig`

-- | Sanity check for a 'TxDecisionPolicy': 'True' when every field lies
-- in the range the decision logic assumes.
--
-- The window and per-peer limit fields must be positive (otherwise the
-- peer can make no progress), the rate, score and delay parameters must
-- be non-negative, and 'inflightTimeout' must be at least 'interTxSpace'
-- (their difference is used as a non-negative delay when bumping a stuck
-- entry's inflight-multiplicity cap).  'defaultTxDecisionPolicy' satisfies
-- it.
saneTxDecisionPolicy :: TxDecisionPolicy -> TxSubmissionConfig -> Bool
saneTxDecisionPolicy p c =
       maxNumUnacknowledgedTxIds      c >= 1
    && maxNumTxIdsToRequest           c >= 1
    && maxNumTxsToRequest             c >= 1
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
