{-# LANGUAGE NamedFieldPuns #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- nb. the module Ouroboros.Network.Diffusion.Governor should be imported qualified as PeerSelection by convention to aid comprehension

module Ouroboros.Network.Diffusion.Configuration
  ( DefaultNumBootstrapPeers (..)
  , MinBigLedgerPeersForTrustedState (..)
  , defaultNumBootstrapPeers
  , defaultAcceptedConnectionsLimit
  , defaultDiffusionMode
  , defaultPeerSharing
  , defaultBlockFetchConfiguration
  , defaultChainSyncTimeout
  , defaultDeadlineTargets
  , defaultSyncTargets
  , defaultDeadlineChurnInterval
  , defaultBulkChurnInterval
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , BlockFetchConfiguration (..)
  , ChainSyncTimeout (..)
  , DiffusionMode (..)
  , MiniProtocolParameters (..)
  , P2P (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
  , ConsensusMode (..)
  , defaultConsensusMode
  , defaultMinBigLedgerPeersForTrustedState
  , defaultMiniProtocolParameters
  , deactivateTimeout
  , closeConnectionTimeout
  , peerMetricsConfiguration
  , defaultTimeWaitTimeout
  , defaultProtocolIdleTimeout
  , defaultResetTimeout
  , handshake_QUERY_SHUTDOWN_DELAY
  , ps_POLICY_PEER_SHARE_STICKY_TIME
  , ps_POLICY_PEER_SHARE_MAX_PEERS
  ) where

import Control.Monad.Class.MonadTime.SI
import System.Random (randomRIO)

import Cardano.Node.ConsensusMode
import Cardano.Node.Types (MinBigLedgerPeersForTrustedState (..))
import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion (P2P (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, maxChainSyncTimeout, minChainSyncTimeout,
           peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode (DiffusionMode (..),
           MiniProtocolParameters (..), defaultMiniProtocolParameters)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSharing (ps_POLICY_PEER_SHARE_MAX_PEERS,
           ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Ouroboros.Network.Protocol.Handshake (handshake_QUERY_SHUTDOWN_DELAY)
import Ouroboros.Network.Protocol.Limits (shortWait)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))


-- | Default number of bootstrap peers
--
newtype DefaultNumBootstrapPeers = DefaultNumBootstrapPeers { getDefaultNumBootstrapPeers :: Int }
  deriving (Eq, Show)

defaultNumBootstrapPeers :: DefaultNumBootstrapPeers
defaultNumBootstrapPeers = DefaultNumBootstrapPeers 30

-- |Outbound governor targets
-- Targets may vary depending on whether a node is operating in
-- Genesis mode.

-- | Default peer targets in Praos mode
--
defaultDeadlineTargets :: PeerSelectionTargets
defaultDeadlineTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 60,
    targetNumberOfKnownPeers                = 85,
    targetNumberOfEstablishedPeers          = 40,
    targetNumberOfActivePeers               = 15,
    targetNumberOfKnownBigLedgerPeers       = 15,
    targetNumberOfEstablishedBigLedgerPeers = 10,
    targetNumberOfActiveBigLedgerPeers      = 5 }

-- | These targets are established when Genesis mode is enabled
-- in node configuration and when the node is syncing up
--
defaultSyncTargets :: PeerSelectionTargets
defaultSyncTargets =
  defaultDeadlineTargets {
    targetNumberOfActivePeers               = 0,
    targetNumberOfKnownBigLedgerPeers       = 100,
    targetNumberOfEstablishedBigLedgerPeers = 50,
    targetNumberOfActiveBigLedgerPeers      = 30 }

-- | This parameter controls the minimum number of active connections
--   with big ledger peers that must be maintained when syncing in
--   Genesis mode such that trusted state can be signalled to Consensus.
--   Exiting syncing / entering deadline mode is predicated on this
--   condition. This should be below `targetNumberOfActiveBigLedgerPeers`
--   in `syncTargets` otherwise untrusted state will never be departed.
--   This value is lower than the target, because in Genesis we may
--   demote a big ledger peer for underperformance and not immediately
--   promote one from the warm set in case there are adversaries
--   whom are intentionally trying to slow us down.
--
defaultMinBigLedgerPeersForTrustedState :: MinBigLedgerPeersForTrustedState
defaultMinBigLedgerPeersForTrustedState = MinBigLedgerPeersForTrustedState 5

-- | Inbound governor targets
--
defaultAcceptedConnectionsLimit :: AcceptedConnectionsLimit
defaultAcceptedConnectionsLimit =
  AcceptedConnectionsLimit {
    acceptedConnectionsHardLimit = 512,
    acceptedConnectionsSoftLimit = 384,
    acceptedConnectionsDelay     = 5 }

-- | Principal mode of network operation
--
defaultDiffusionMode :: P2P
defaultDiffusionMode = NonP2P

-- | Node's peer sharing participation flag
--
defaultPeerSharing :: PeerSharing
defaultPeerSharing = PeerSharingDisabled

-- | Configuration for FetchDecisionPolicy.
--
defaultBlockFetchConfiguration :: Int -> BlockFetchConfiguration
defaultBlockFetchConfiguration bfcSalt =
  BlockFetchConfiguration {
    bfcMaxConcurrencyBulkSync = 1,
    bfcMaxConcurrencyDeadline = 1,
    bfcMaxRequestsInflight    = fromIntegral $ blockFetchPipeliningMax defaultMiniProtocolParameters,
    bfcDecisionLoopInterval   = 0.01, -- 10ms
    bfcSalt }

defaultChainSyncTimeout :: IO ChainSyncTimeout
defaultChainSyncTimeout = do
    -- These values approximately correspond to false positive
    -- thresholds for streaks of empty slots with 99% probability,
    -- 99.9% probability up to 99.999% probability.
    -- t = T_s [log (1-Y) / log (1-f)]
    -- Y = [0.99, 0.999...]
    -- T_s = slot length of 1s.
    -- f = 0.05
    -- The timeout is randomly picked per bearer to avoid all bearers
    -- going down at the same time in case of a long streak of empty
    -- slots.
    -- To avoid global synchronosation the timeout is picked uniformly
    -- from the interval 135 - 269, corresponds to the a 99.9% to
    -- 99.9999% thresholds.
    -- TODO: The timeout should be drawn at random everytime chainsync
    --       enters the must reply state. A static per connection timeout
    --       leads to selection preassure for connections with a large
    --       timeout, see #4244.
    mustReplyTimeout <- Just . realToFrac <$> randomRIO ( realToFrac minChainSyncTimeout :: Double
                                                        , realToFrac maxChainSyncTimeout :: Double
                                                        )
    return ChainSyncTimeout { canAwaitTimeout  = shortWait,
                              intersectTimeout = shortWait,
                              mustReplyTimeout,
                              idleTimeout      = Just 3673 }

defaultDeadlineChurnInterval :: DiffTime
defaultDeadlineChurnInterval = 3300

defaultBulkChurnInterval :: DiffTime
defaultBulkChurnInterval = 900
