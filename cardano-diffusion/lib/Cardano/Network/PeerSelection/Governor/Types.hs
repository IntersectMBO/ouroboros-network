{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ViewPatterns             #-}

module Cardano.Network.PeerSelection.Governor.Types
  ( empty
  , outboundConnectionsState
  , cardanoPeerSelectionGovernorArgs
  , readAssociationMode
  , SupportsPeerSelectionState (..)
  , Cardano.NumberOfBigLedgerPeers (..)
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadTimer.SI
import Data.Maybe (fromJust)
import Data.Set qualified as Set

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.LedgerStateJudgement
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Monitor
           (monitorBootstrapPeersFlag, monitorLedgerStateJudgement,
           waitForSystemToQuiesce)
import Cardano.Network.PeerSelection.Governor.Monitor qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Cardano.Network.PeerSelection.PeerTrustable
           (PeerTrustable (IsNotTrustable))
import Cardano.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

import Ouroboros.Network.PeerSelection (PeerSharing (..), UseLedgerPeers (..))
import Ouroboros.Network.PeerSelection.Governor.Types (AssociationMode (..),
           BootstrapPeersCriticalTimeoutError (..), ExtraGuardedDecisions (..),
           PeerSelectionActions (..), PeerSelectionGovernorArgs (..),
           PeerSelectionInterfaces (..), PeerSelectionSetsWithSizes,
           PeerSelectionState (..), PeerSelectionView (..), SupportsPeerSelectionState (..))
import Ouroboros.Network.PeerSelection.LedgerPeers
           (LedgerPeersConsensusInterface (lpExtraAPI))


empty :: (Cardano.ViewExtraPeers (Cardano.ExtraPeers peeraddr))
empty = Cardano.ExtraPeerSelectionSetsWithSizes {
    viewKnownBootstrapPeers           = (Set.empty, 0)
  , viewColdBootstrapPeersPromotions  = (Set.empty, 0)
  , viewEstablishedBootstrapPeers     = (Set.empty, 0)
  , viewWarmBootstrapPeersDemotions   = (Set.empty, 0)
  , viewWarmBootstrapPeersPromotions  = (Set.empty, 0)
  , viewActiveBootstrapPeers          = (Set.empty, 0)
  , viewActiveBootstrapPeersDemotions = (Set.empty, 0)
  }

outboundConnectionsState
    :: Ord peeraddr
    => AssociationMode
    -> PeerSelectionSetsWithSizes (Cardano.ViewExtraPeers (Cardano.ExtraPeers peeraddr)) peeraddr
    -> PeerSelectionState Cardano.ExtraState PeerTrustable extraPeers peeraddr peerconn
    -> OutboundConnectionsState
outboundConnectionsState
    associationMode
    PeerSelectionView {
      viewEstablishedPeers       = (viewEstablishedPeers, _),
        viewActiveBigLedgerPeers = (_, activeNumBigLedgerPeers),
      viewExtraViews = fromJust -> Cardano.ExtraPeerSelectionSetsWithSizes {
        viewEstablishedBootstrapPeers = (viewEstablishedBootstrapPeers, _),
        viewActiveBootstrapPeers      = (viewActiveBootstrapPeers, _)
      }
    }
    PeerSelectionState {
      localRootPeers,
      extraState = Cardano.ExtraState {
        Cardano.consensusMode,
        Cardano.bootstrapPeersFlag,
        Cardano.minNumberOfBigLedgerPeers
      }
    }
    =
    case (associationMode, bootstrapPeersFlag, consensusMode) of
      (LocalRootsOnly, _, _)
        |  -- we are only connected to trusted local root
           -- peers
           viewEstablishedPeers `Set.isSubsetOf` trustableLocalRootSet
        -> TrustedStateWithExternalPeers

        |  otherwise
        -> UntrustedState

       -- bootstrap mode
      (Unrestricted, UseBootstrapPeers {}, _)
        |  -- we are only connected to trusted local root
           -- peers or bootstrap peers
           viewEstablishedPeers `Set.isSubsetOf` (viewEstablishedBootstrapPeers <> trustableLocalRootSet)
           -- there's at least one active bootstrap peer
        ,  not (Set.null viewActiveBootstrapPeers)
        -> TrustedStateWithExternalPeers

        |  otherwise
        -> UntrustedState

       -- praos mode with public roots
      (Unrestricted, DontUseBootstrapPeers, PraosMode)
        -> UntrustedState

      -- Genesis mode
      (Unrestricted, DontUseBootstrapPeers, GenesisMode)
        |  activeNumBigLedgerPeers >= Cardano.getNumberOfBigLedgerPeers minNumberOfBigLedgerPeers
        -> TrustedStateWithExternalPeers

        |  otherwise
        -> UntrustedState
  where
    trustableLocalRootSet = LocalRootPeers.trustableKeysSet localRootPeers


cardanoPeerSelectionGovernorArgs
  :: ( MonadTimer m
     , Alternative (STM m)
     , Ord peeraddr
     )
  => Cardano.ExtraPeerSelectionActions m
  -> PeerSelectionGovernorArgs
       Cardano.ExtraState
       extraDebugState
       PeerTrustable
       (Cardano.ExtraPeers peeraddr)
       (Cardano.LedgerPeersConsensusInterface m)
       peeraddr
       peerconn
       BootstrapPeersCriticalTimeoutError
       m
cardanoPeerSelectionGovernorArgs extraActions =
  PeerSelectionGovernorArgs {
    -- If by any chance the node takes more than 15 minutes to converge to a
    -- clean state, we crash the node. This could happen in very rare
    -- conditions such as a global network issue, DNS, or a bug in the code.
    -- In any case crashing the node will force the node to be restarted,
    -- starting in the correct state for it to make progress.
    abortGovernor   = \blockedAt st ->
      case Cardano.bootstrapPeersTimeout (extraState st) of
        Nothing -> Nothing
        Just t
          | blockedAt >= t -> Just BootstrapPeersCriticalTimeoutError
          | otherwise      -> Nothing
  , updateWithState = \PeerSelectionInterfaces { readUseLedgerPeers }
                       PeerSelectionActions { getLedgerStateCtx,
                                              peerSharing }
                       psv st -> do
      associationMode <- readAssociationMode readUseLedgerPeers
                                             peerSharing
                                             (Cardano.bootstrapPeersFlag (extraState st))
      Cardano.updateOutboundConnectionsState
        (lpExtraAPI getLedgerStateCtx)
        (outboundConnectionsState associationMode psv st)
  , extraDecisions  =
      ExtraGuardedDecisions {
        preBlocking     = \_ psa pst ->
             monitorBootstrapPeersFlag   extraActions psa pst
          <> monitorLedgerStateJudgement psa pst
          <> waitForSystemToQuiesce          pst
      , postBlocking    = mempty
      , postNonBlocking = mempty
      , customTargetsAction         = Just $ \_ -> Cardano.targetPeers extraActions
      , customLocalRootsAction      = Just $ \_ -> Cardano.localRoots
      , enableProgressMakingActions = \st ->
          not (requiresBootstrapPeers (Cardano.bootstrapPeersFlag st) (Cardano.ledgerStateJudgement st))
      , ledgerPeerSnapshotExtraStateChange = \st ->
          st { Cardano.ledgerStateJudgement = YoungEnough }
      }
  , defaultExtraFlags = IsNotTrustable
  }


-- | Classify if a node is in promiscuous mode.
--
-- A node is not in promiscuous mode only if: it doesn't use ledger peers, peer
-- sharing, the set of bootstrap peers is empty.
--
readAssociationMode
  :: MonadSTM m
  => STM m UseLedgerPeers
  -> PeerSharing
  -> UseBootstrapPeers
  -> STM m AssociationMode
readAssociationMode
  readUseLedgerPeers
  peerSharing
  useBootstrapPeers
  =
  do useLedgerPeers <- readUseLedgerPeers
     pure $
       case (useLedgerPeers, peerSharing, useBootstrapPeers) of
         (DontUseLedgerPeers, PeerSharingDisabled, DontUseBootstrapPeers)
           -> LocalRootsOnly
         (DontUseLedgerPeers, PeerSharingDisabled, UseBootstrapPeers config)
           |  null config
           -> LocalRootsOnly
         _ -> Unrestricted
