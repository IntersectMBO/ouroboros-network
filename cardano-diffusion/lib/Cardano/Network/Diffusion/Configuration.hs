{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- This module contains Cardano specific configuration parameters

module Cardano.Network.Diffusion.Configuration
  ( DefaultNumBootstrapPeers (..)
  , NumberOfBigLedgerPeers (..)
  , defaultNumBootstrapPeers
  , defaultSyncTargets
  , defaultNumberOfBigLedgerPeers
  , defaultChainSyncIdleTimeout
  , mkDefaultBlockFetchConfiguration
  , mkDefaultMiniProtocolParameters
  , TxSubmissionLogicVersion (..)
  , defaultTxSubmissionLogicVersion
  , defaultNodeTxSubmissionConfig
  , defaultEgressScheduling
  , srvPrefix
    -- * Re-exports
  , ChainSyncIdleTimeout (..)
  , BlockFetchConfiguration (..)
  , MiniProtocolParameters (..)
  , NodeTxSubmissionConfig (..)
  , Salt
  , mkSalt
  , mkSaltIO
  ) where

import Cardano.Network.NodeToNode (MiniProtocolParameters (..),
           NodeTxSubmissionConfig (..), keepAliveMiniProtocolNum,
           mkDefaultMiniProtocolParameters, txSubmissionMiniProtocolNum)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (NumberOfBigLedgerPeers (..))
import Network.Mux qualified as Mx
import Network.Mux.Types (MiniProtocolDir (..))
import Ouroboros.Network.Diffusion.Configuration qualified as Diffusion
import Ouroboros.Network.Diffusion.Types (EgressScheduling (..))

import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
           GenesisBlockFetchConfiguration (..))
import Ouroboros.Network.Hashable (Salt, mkSalt, mkSaltIO)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (SRVPrefix)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncIdleTimeout (..))
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy qualified as V2
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
           (TxSubmissionLogicVersion (..))

-- | Scheduled egress ('Diffusion.defaultEgressSchedulingWith'):
--
-- Responder traffic is ranked in the budget's bucket, its own requests go
-- direct, and the tx bodies its peers pull go in the reserved slice. In
-- tx-submission the responder is the side that asks, so its requests go
-- direct too, and the initiator's replies, the bodies, are the slice's.
--
-- Under a saturated budget the ranked order serves the rest tier, peers
-- with neither standing nor credit, only with what the others leave, so a
-- reply there can outwait the remote's time limit (chain-sync's is the
-- shortest) and cost the connection: under load the node sheds the peers
-- it neither depends on nor has credited, by their own timeouts.
defaultEgressScheduling :: EgressScheduling
defaultEgressScheduling = (Diffusion.defaultEgressSchedulingWith laneOf) {
    -- one small reply per request, nothing a client can drive to volume
    esUnchargedProtocols = [keepAliveMiniProtocolNum]
  }
  where
    laneOf num InitiatorDir | num == txSubmissionMiniProtocolNum = Mx.Slice
    laneOf num ResponderDir | num == txSubmissionMiniProtocolNum = Mx.Direct
    laneOf num dir = Mx.directionSplit num dir

-- | Default number of bootstrap peers
--
newtype DefaultNumBootstrapPeers =
  DefaultNumBootstrapPeers { getDefaultNumBootstrapPeers :: Int }
  deriving (Eq, Show)

defaultNumBootstrapPeers :: DefaultNumBootstrapPeers
defaultNumBootstrapPeers = DefaultNumBootstrapPeers 30

-- | These targets are established when Genesis mode is enabled
-- in node configuration and when the node is syncing up
--
defaultSyncTargets :: PeerSelectionTargets
defaultSyncTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 0,
    targetNumberOfKnownPeers                = 150,
    targetNumberOfEstablishedPeers          = 10,
    targetNumberOfActivePeers               = 5,
    targetNumberOfKnownBigLedgerPeers       = 100,
    targetNumberOfEstablishedBigLedgerPeers = 40,
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
defaultNumberOfBigLedgerPeers :: NumberOfBigLedgerPeers
defaultNumberOfBigLedgerPeers = NumberOfBigLedgerPeers 5

defaultChainSyncIdleTimeout :: ChainSyncIdleTimeout
defaultChainSyncIdleTimeout = ChainSyncIdleTimeout 3373

srvPrefix :: SRVPrefix
srvPrefix = "_cardano._tcp"


-- | Configuration for FetchDecisionPolicy.  The maximal number of requests in
-- flight is the `blockFetchPipeliningMax` of the given
-- `MiniProtocolParameters`.
--
mkDefaultBlockFetchConfiguration :: MiniProtocolParameters
                                 -> Salt
                                 -> BlockFetchConfiguration
mkDefaultBlockFetchConfiguration MiniProtocolParameters { blockFetchPipeliningMax }
                                 bfcSalt =
  BlockFetchConfiguration {
    bfcMaxConcurrencyBulkSync = 1,
    bfcMaxConcurrencyDeadline = 1,
    bfcMaxRequestsInflight    = fromIntegral blockFetchPipeliningMax,
    bfcDecisionLoopIntervalGenesis = 0.04,  -- 40ms
    bfcDecisionLoopIntervalPraos = 0.01,  -- 10ms
    bfcGenesisBFConfig        = GenesisBlockFetchConfiguration
      { gbfcGracePeriod = 10 },  -- seconds
    bfcSalt }

-- | The default logic version is the legacy one, the new one is still
-- experimental.
defaultTxSubmissionLogicVersion :: TxSubmissionLogicVersion
defaultTxSubmissionLogicVersion = TxSubmissionLogicV1


-- | The default `NodeTxSubmissionConfig`: the settings of
-- `defaultTxSubmissionConfigV2`.
--
-- It will be overwritten if `TxSubmissionLogicV1` is used or negotiated version
-- is before `NodeToNodeV_16`, see `txSubmissionConfigForVersion`.
--
defaultNodeTxSubmissionConfig :: NodeTxSubmissionConfig
defaultNodeTxSubmissionConfig =
    NodeTxSubmissionConfig {
      maxNumUnacknowledgedTxIds =
        V2.maxNumUnacknowledgedTxIds V2.defaultTxSubmissionConfigV2,
      maxNumTxIdsToRequest =
        V2.maxNumTxIdsToRequest V2.defaultTxSubmissionConfigV2
    }
