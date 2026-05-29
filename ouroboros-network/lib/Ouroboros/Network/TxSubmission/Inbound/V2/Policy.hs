{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Policy
  ( TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  , max_TX_SIZE
    -- * Re-exports
  , NumTxIdsToReq (..)
  ) where

import Control.DeepSeq
import Control.Monad.Class.MonadTime.SI
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToReq (..))
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
      maxNumTxIdsToRequest   :: !NumTxIdsToReq,
      -- ^ a maximal number of txids requested at once.

      maxUnacknowledgedTxIds :: !NumTxIdsToReq,
      -- ^ maximal number of unacknowledgedTxIds.  Measured in `NumTxIdsToReq`
      -- since we enforce this policy by requesting not more txids than what
      -- this limit allows.

      --
      -- Configuration of tx decision logic.
      --

      txsSizeInflightPerPeer :: !SizeInBytes,
      -- ^ a limit of tx size in-flight from a single peer.
      -- It can be exceed by max tx size.

      maxOutstandingTxBatchesPerPeer :: !Int,
      -- ^ a limit of outstanding tx-body request batches from a single peer.

      txInflightMultiplicity :: !Int,
      -- ^ from how many peers download the `txid` simultaneously

      bufferedTxsMinLifetime :: !DiffTime,
      -- ^ how long TXs that have been added to the mempool will be
      -- kept in the `bufferedTxs` cache.

      scoreRate              :: !Double,
      -- ^ rate at which "rejected" TXs drain. Unit: TX/seconds.

      scoreMax               :: !Double,
      -- ^ Maximum number of "rejections". Unit: seconds

      scoreAcceptDecrement   :: !Double,
      -- ^ amount subtracted from the peer's score for each tx body the peer delivered that the mempool accepted.

      interTxSpace           :: !DiffTime,
      -- ^ space between actual requests for the same TX.

      inflightTimeout        :: !DiffTime,
      -- ^ Maximum time a peer's attempt may sit between claim and
      -- entering submission before the per-entry inflight-multiplicity
      -- cap is bumped, allowing another peer to attempt in parallel.
      maxPeerClaimDelay      :: !DiffTime
      -- ^ Maximum delay penalty for poor performing peers.
    }
  deriving (Eq, Show)

instance NFData TxDecisionPolicy where
  rnf TxDecisionPolicy{} = ()

defaultTxDecisionPolicy :: TxDecisionPolicy
defaultTxDecisionPolicy =
  TxDecisionPolicy {
    maxNumTxIdsToRequest   = 6,
    maxUnacknowledgedTxIds = 10, -- must be the same as txSubmissionMaxUnacked
    txsSizeInflightPerPeer = max_TX_SIZE * 6,
    maxOutstandingTxBatchesPerPeer = 4,
    txInflightMultiplicity = 2,
    bufferedTxsMinLifetime = 2,
    scoreRate              = 0.001,
    scoreMax               = 15 * 60,
    scoreAcceptDecrement   = 3,
    interTxSpace           = 0.250,
    inflightTimeout        = 0.500,
    maxPeerClaimDelay      = 0.250
  }
