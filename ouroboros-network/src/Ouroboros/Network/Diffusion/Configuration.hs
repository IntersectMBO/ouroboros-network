{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Diffusion.Configuration
  ( DefaultNumBootstrapPeers (..)
  , defaultNumBootstrapPeers
  , defaultPeerTargets
  , defaultGenesisSyncPeerTargets
  , defaultAcceptedConnectionsLimit
  , defaultDiffusionMode
  , defaultPeerSharing
  , defaultBlockFetchConfiguration
  , defaultChainSyncTimeout
  , defaultGenesisPeerTargetConfiguration
  , defaultLegacyPeerTargetConfiguration
  , defaultPeerSelectionTargetsBuilder
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , BlockFetchConfiguration (..)
  , ChainSyncTimeout (..)
  , ConfigurationTargets (..)
  , MiniProtocolParameters (..)
  , P2P (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
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

import System.Random (randomRIO)

import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Common (P2P (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
           defaultMiniProtocolParameters)
import Ouroboros.Network.PeerSelection.Governor.Types
           (ConfigurationTargets (..), PeerSelectionTargets (..),
           mkTargetsSelector, PeerSelectionState (activePeers))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerStateJudgement)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSharing (ps_POLICY_PEER_SHARE_MAX_PEERS,
           ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Ouroboros.Network.Protocol.Handshake (handshake_QUERY_SHUTDOWN_DELAY)
import Ouroboros.Network.Protocol.Limits (shortWait)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.ConsensusMode

-- | Default number of bootstrap peers
--
newtype DefaultNumBootstrapPeers = DefaultNumBootstrapPeers { getDefaultNumBootstrapPeers :: Int }
  deriving (Eq, Show)

defaultNumBootstrapPeers :: DefaultNumBootstrapPeers
defaultNumBootstrapPeers = DefaultNumBootstrapPeers 30

-- |Outbound governor targets
-- Targets may vary depending on whether a node is operating in
-- Genesis mode.

-- | Default peer targets when Genesis mode is disabled
--
defaultPeerTargets :: PeerSelectionTargets
defaultPeerTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 85,
    targetNumberOfKnownPeers                = 85,
    targetNumberOfEstablishedPeers          = 40,
    targetNumberOfActivePeers               = 15,
    targetNumberOfKnownBigLedgerPeers       = 15,
    targetNumberOfEstablishedBigLedgerPeers = 10,
    targetNumberOfActiveBigLedgerPeers      = 5 }

-- | These targets are established when Genesis mode is enabled
-- and consensus determines that we are 'far' from the tip
-- of the best chain
--
defaultGenesisSyncPeerTargets :: PeerSelectionTargets
defaultGenesisSyncPeerTargets =
  defaultPeerTargets {
    targetNumberOfActivePeers               = governor_GENESIS_ACTIVE_PEERS,
    targetNumberOfKnownBigLedgerPeers       = governor_GENESIS_KNOWN_BIG_LEDGER_PEERS,
    targetNumberOfEstablishedBigLedgerPeers = governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS,
    targetNumberOfActiveBigLedgerPeers      = governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS }

defaultGenesisPeerTargetConfiguration :: ConfigurationTargets
defaultGenesisPeerTargetConfiguration =
  ConfigurationTargets {
    confDefaultPeerTargets     = defaultPeerTargets,
    confGenesisSyncPeerTargets = defaultGenesisSyncPeerTargets }

defaultLegacyPeerTargetConfiguration :: ConfigurationTargets
defaultLegacyPeerTargetConfiguration =
  ConfigurationTargets {
    confDefaultPeerTargets     = defaultPeerTargets,
    confGenesisSyncPeerTargets = defaultPeerTargets }

-- | Peer targets selector for currentTargets field in 'PeerSelectionActions'
-- that uses defaults defined here. This accounts for different targets depending
-- on whether the node is configured to use Genesis or not.
--
defaultPeerSelectionTargetsBuilder :: LedgerStateJudgement -> ConsensusMode -> PeerSelectionTargets
defaultPeerSelectionTargetsBuilder = mkTargetsSelector defaultGenesisPeerTargetConfiguration

-- | Target number of active peers in Genesis sync mode
--
governor_GENESIS_ACTIVE_PEERS :: Int
governor_GENESIS_ACTIVE_PEERS = 0

-- | Target number of known big ledger peers in Genesis sync mode
--
governor_GENESIS_KNOWN_BIG_LEDGER_PEERS :: Int
governor_GENESIS_KNOWN_BIG_LEDGER_PEERS = 100

-- | Target number of established big ledger peers in Genesis sync mode
--
governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS :: Int
governor_GENESIS_ESTABLISHED_BIG_LEDGER_PEERS = 50

-- | Target number of active big ledger peers in Genesis sync mode
--
governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS :: Int
governor_GENESIS_ACTIVE_BIG_LEDGER_PEERS = 30

-- |Inbound governor targets
--
defaultAcceptedConnectionsLimit :: AcceptedConnectionsLimit
defaultAcceptedConnectionsLimit =
  AcceptedConnectionsLimit {
    acceptedConnectionsHardLimit = 512,
    acceptedConnectionsSoftLimit = 384,
    acceptedConnectionsDelay     = 5 }

-- |Principal mode of network operation
--
defaultDiffusionMode :: P2P
defaultDiffusionMode = NonP2P

-- |Node's peer sharing participation flag
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
    mustReplyTimeout <- Just . realToFrac <$> randomRIO (135,269 :: Double)
    return ChainSyncTimeout { canAwaitTimeout  = shortWait,
                              intersectTimeout = shortWait,
                              mustReplyTimeout,
                              idleTimeout      = Just 3673 }
