module Ouroboros.Consensus.Node.ExitPolicy (
    NodeToNodeInitiatorResult (..)
  , returnPolicy
    -- * Re-exports
  , ReturnPolicy
  ) where

import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Network.ExitPolicy


-- | Result of any of the `node-to-node` mini-protocols.  We ignore all but
-- `chain-sync` results.
--
data NodeToNodeInitiatorResult =
    ChainSyncInitiatorResult !ChainSyncClientResult
  | NoInitiatorResult


returnPolicy :: ReturnPolicy NodeToNodeInitiatorResult
returnPolicy NoInitiatorResult = ReconnectDelay 0
returnPolicy (ChainSyncInitiatorResult result) = case result of
  -- TODO: it would be nice to have additional context to predict when we will
  -- be ready to reconnect.
  ForkTooDeep      _ _ourTip _theirTip -> ReconnectDelay 120
  NoMoreIntersection _ourTip _theirTip -> ReconnectDelay 120
  RolledBackPastIntersection
                   _ _ourTip _theirTip -> ReconnectDelay 180
  -- the outbound-governor asked for hot to warm demotion; it's up to the
  -- governor to decide to promote the peer to hot.
  AskedToTerminate                     -> ReconnectDelay 0
