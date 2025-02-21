{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Ouroboros.Cardano.Network.PeerSelection.Governor.Types where

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.Types (LedgerStateJudgement (..),
           getNumberOfBigLedgerPeers)
import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM
import Data.Set (Set)
import Data.Set qualified as Set
import Ouroboros.Cardano.Network.ExtraRootPeers qualified as Cardano
import Ouroboros.Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.Monitor
           (monitorBootstrapPeersFlag, monitorLedgerStateJudgement,
           waitForSystemToQuiesce)
import Ouroboros.Cardano.Network.PeerSelection.Governor.Monitor qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionActions qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Ouroboros.Cardano.Network.PublicRootPeers qualified as Cardano.PublicRootPeers
import Ouroboros.Network.PeerSelection.Governor (readAssociationMode)
import Ouroboros.Network.PeerSelection.Governor.Types (AssociationMode (..),
           BootstrapPeersCriticalTimeoutError (..), ExtraGuardedDecisions (..),
           PeerSelectionActions (..), PeerSelectionGovernorArgs (..),
           PeerSelectionInterfaces (..), PeerSelectionSetsWithSizes,
           PeerSelectionState (..), PeerSelectionView (..))
import Ouroboros.Network.PeerSelection.LedgerPeers
           (LedgerPeersConsensusInterface (lpExtraAPI))
import Ouroboros.Network.PeerSelection.PublicRootPeers (getBigLedgerPeers)
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

-- | Peer selection view.
--
-- This is a functor which is used to hold computation of various peer sets and
-- their sizes.  See `peerSelectionStateToView`, `peerSelectionStateToCounters`.
--
data ExtraPeerSelectionSetsWithSizes peeraddr =
  ExtraPeerSelectionSetsWithSizes {
    viewKnownBootstrapPeers           :: (Set peeraddr, Int)
  , viewColdBootstrapPeersPromotions  :: (Set peeraddr, Int)
  , viewEstablishedBootstrapPeers     :: (Set peeraddr, Int)
  , viewWarmBootstrapPeersDemotions   :: (Set peeraddr, Int)
  , viewWarmBootstrapPeersPromotions  :: (Set peeraddr, Int)
  , viewActiveBootstrapPeers          :: (Set peeraddr, Int)
  , viewActiveBootstrapPeersDemotions :: (Set peeraddr, Int)
  } deriving (Eq, Show)

empty :: ExtraPeerSelectionSetsWithSizes peeraddr
empty = ExtraPeerSelectionSetsWithSizes {
    viewKnownBootstrapPeers           = (Set.empty, 0)
  , viewColdBootstrapPeersPromotions  = (Set.empty, 0)
  , viewEstablishedBootstrapPeers     = (Set.empty, 0)
  , viewWarmBootstrapPeersDemotions   = (Set.empty, 0)
  , viewWarmBootstrapPeersPromotions  = (Set.empty, 0)
  , viewActiveBootstrapPeers          = (Set.empty, 0)
  , viewActiveBootstrapPeersDemotions = (Set.empty, 0)
  }


cardanoPeerSelectionStatetoCounters
  :: Ord peeraddr
  => PeerSelectionState
      extraState
      extraFlags
      (Cardano.ExtraPeers peeraddr)
      peeraddr
      peerconn
  -> ExtraPeerSelectionSetsWithSizes peeraddr
cardanoPeerSelectionStatetoCounters
  PeerSelectionState {
    establishedPeers,
    activePeers,
    publicRootPeers,
    inProgressPromoteCold,
    inProgressPromoteWarm,
    inProgressDemoteWarm,
    inProgressDemoteHot
  } =
  ExtraPeerSelectionSetsWithSizes {
    viewKnownBootstrapPeers           = size   knownBootstrapPeersSet
  , viewColdBootstrapPeersPromotions  = size $ knownBootstrapPeersSet
                                        `Set.intersection` inProgressPromoteCold
  , viewEstablishedBootstrapPeers     = size   establishedBootstrapPeersSet
  , viewWarmBootstrapPeersDemotions   = size $ establishedBootstrapPeersSet
                                        `Set.intersection` inProgressDemoteWarm
  , viewWarmBootstrapPeersPromotions  = size $ establishedBootstrapPeersSet
                                        `Set.intersection` inProgressPromoteWarm
  , viewActiveBootstrapPeers          = size   activeBootstrapPeersSet
  , viewActiveBootstrapPeersDemotions = size $ activeBootstrapPeersSet
                                        `Set.intersection` inProgressDemoteHot
  }
  where
    size s = (s, Set.size s)

    -- common sets
    establishedSet = EstablishedPeers.toSet establishedPeers
    bigLedgerSet   = getBigLedgerPeers publicRootPeers

    -- non big ledger peers
    establishedPeersSet = establishedSet Set.\\ establishedBigLedgerPeersSet
    activePeersSet      = activePeers Set.\\ activeBigLedgerPeersSet

    -- big ledger peers
    establishedBigLedgerPeersSet = establishedSet `Set.intersection` bigLedgerSet
    activeBigLedgerPeersSet      = establishedBigLedgerPeersSet `Set.intersection` activePeers

    -- bootstrap peers
    bootstrapSet                 = Cardano.PublicRootPeers.getBootstrapPeers publicRootPeers
    -- bootstrap peers and big ledger peers are disjoint, hence we can use
    -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
    knownBootstrapPeersSet       = bootstrapSet
    establishedBootstrapPeersSet = establishedPeersSet `Set.intersection` bootstrapSet
    activeBootstrapPeersSet      = activePeersSet `Set.intersection` bootstrapSet


outboundConnectionsState
    :: Ord peeraddr
    => AssociationMode
    -> PeerSelectionSetsWithSizes (ExtraPeerSelectionSetsWithSizes peeraddr) peeraddr
    -> PeerSelectionState Cardano.ExtraState PeerTrustable extraPeers peeraddr peerconn
    -> OutboundConnectionsState
outboundConnectionsState
    associationMode
    PeerSelectionView {
      viewEstablishedPeers       = (viewEstablishedPeers, _),
        viewActiveBigLedgerPeers = (_, activeNumBigLedgerPeers),
      viewExtraViews = ExtraPeerSelectionSetsWithSizes {
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
        |  activeNumBigLedgerPeers >= getNumberOfBigLedgerPeers minNumberOfBigLedgerPeers
        -> TrustedStateWithExternalPeers

        |  otherwise
        -> UntrustedState
  where
    trustableLocalRootSet = LocalRootPeers.trustableKeysSet localRootPeers


cardanoPeerSelectionGovernorArgs
  :: ( MonadSTM m
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
       (ExtraPeerSelectionSetsWithSizes peeraddr)
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
  }
