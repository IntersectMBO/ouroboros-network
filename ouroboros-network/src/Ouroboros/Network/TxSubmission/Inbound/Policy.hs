{-# LANGUAGE NumericUnderscores #-}

module Ouroboros.Network.TxSubmission.Inbound.Policy
  ( TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  , max_TX_SIZE
  ) where

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

      maxTxsSizeInflight     :: !SizeInBytes,
      -- ^ a limit of tx size in-flight from all peers.
      -- It can be exceed by max tx size.

      txInflightMultiplicity :: !Int,
      -- ^ from how many peers download the `txid` simultaneously

      bufferedTxsMinLifetime :: !DiffTime,
      -- ^ how long TXs that have been added to the mempool will be
      -- kept in the `bufferedTxs` cache.

      scoreRate              :: !Double,
      -- ^ rate at which "rejected" TXs drain. Unit: TX/seconds.

      scoreMax               :: !Double
      -- ^ Maximum number of "rejections". Unit: seconds

    }
  deriving Show

defaultTxDecisionPolicy :: TxDecisionPolicy
defaultTxDecisionPolicy =
  TxDecisionPolicy {
    maxNumTxIdsToRequest   = 3,
    maxUnacknowledgedTxIds = 10, -- must be the same as txSubmissionMaxUnacked
    txsSizeInflightPerPeer = max_TX_SIZE * 6,
    maxTxsSizeInflight     = max_TX_SIZE * 20,
    txInflightMultiplicity = 2,
    bufferedTxsMinLifetime = 2,
    scoreRate              = 0.1,
    scoreMax               = 15 * 60
  }
