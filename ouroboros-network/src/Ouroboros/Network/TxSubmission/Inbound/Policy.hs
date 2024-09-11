module Ouroboros.Network.TxSubmission.Inbound.Policy
  ( TxDecisionPolicy (..)
  , defaultTxDecisionPolicy
  ) where

import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToReq (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes (..))

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

      txInflightMultiplicity :: !Int
      -- ^ from how many peers download the `txid` simultaneously
    }
  deriving Show

defaultTxDecisionPolicy :: TxDecisionPolicy
defaultTxDecisionPolicy =
  TxDecisionPolicy {
    maxNumTxIdsToRequest   = 1,
    maxUnacknowledgedTxIds = 2,
    txsSizeInflightPerPeer = 2,
    maxTxsSizeInflight     = maxBound,
    txInflightMultiplicity = 2
  }
