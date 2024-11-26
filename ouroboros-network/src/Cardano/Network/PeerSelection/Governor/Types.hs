{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PeerSelection.Governor.Types where

import Cardano.Network.ConsensusMode (ConsensusMode (..))
import Cardano.Network.LedgerPeerConsensusInterface
           (CardanoLedgerPeersConsensusInterface (..))
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Cardano.Network.PeerSelection.Governor.Monitor (localRoots,
           monitorBootstrapPeersFlag, monitorLedgerStateJudgement, targetPeers,
           waitForSystemToQuiesce)
import Cardano.Network.PeerSelection.Governor.PeerSelectionActions
           (CardanoPeerSelectionActions)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (CardanoPeerSelectionState (..))
import Cardano.Network.PeerSelection.LocalRootPeers
           (OutboundConnectionsState (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers)
import Cardano.Network.Types (LedgerStateJudgement (..),
           getMinBigLedgerPeersForTrustedState)
import Control.Concurrent.Class.MonadSTM (MonadSTM, STM)
import Data.Set (Set)
import Data.Set qualified as Set
import Ouroboros.Network.PeerSelection.Governor (readAssociationMode)
import Ouroboros.Network.PeerSelection.Governor.Types (AssociationMode (..),
           BootstrapPeersCriticalTimeoutError (..), ExtraGuardedDecisions (..),
           PeerSelectionGovernorArgs (..), PeerSelectionSetsWithSizes,
           PeerSelectionState (..), PeerSelectionView (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..), UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

-- | Peer selection view.
--
-- This is a functor which is used to hold computation of various peer sets and
-- their sizes.  See `peerSelectionStateToView`, `peerSelectionStateToCounters`.
--
data CardanoPeerSelectionView peeraddr =
  CardanoPeerSelectionView {
    viewKnownBootstrapPeers           :: (Set peeraddr, Int)
  , viewColdBootstrapPeersPromotions  :: (Set peeraddr, Int)
  , viewEstablishedBootstrapPeers     :: (Set peeraddr, Int)
  , viewWarmBootstrapPeersDemotions   :: (Set peeraddr, Int)
  , viewWarmBootstrapPeersPromotions  :: (Set peeraddr, Int)
  , viewActiveBootstrapPeers          :: (Set peeraddr, Int)
  , viewActiveBootstrapPeersDemotions :: (Set peeraddr, Int)
  } deriving (Eq, Show)

empty :: CardanoPeerSelectionView peeraddr
empty = CardanoPeerSelectionView {
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
  => PeerSelectionState extraState extraFlags (CardanoPublicRootPeers peeraddr) peeraddr peerconn
  -> CardanoPeerSelectionView peeraddr
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
  CardanoPeerSelectionView {
    viewKnownBootstrapPeers                = size   knownBootstrapPeersSet
  , viewColdBootstrapPeersPromotions       = size $ knownBootstrapPeersSet
                                            `Set.intersection` inProgressPromoteCold
  , viewEstablishedBootstrapPeers          = size   establishedBootstrapPeersSet
  , viewWarmBootstrapPeersDemotions        = size $ establishedBootstrapPeersSet
                                             `Set.intersection` inProgressDemoteWarm
  , viewWarmBootstrapPeersPromotions       = size $ establishedBootstrapPeersSet
                                             `Set.intersection` inProgressPromoteWarm
  , viewActiveBootstrapPeers               = size   activeBootstrapPeersSet
  , viewActiveBootstrapPeersDemotions      = size $ activeBootstrapPeersSet
                                             `Set.intersection` inProgressDemoteHot
  }
  where
    size s = (s, Set.size s)

    -- common sets
    establishedSet = EstablishedPeers.toSet establishedPeers
    bigLedgerSet   = PublicRootPeers.getBigLedgerPeers publicRootPeers

    -- non big ledger peers
    establishedPeersSet = establishedSet Set.\\ establishedBigLedgerPeersSet
    activePeersSet      = activePeers Set.\\ activeBigLedgerPeersSet

    -- big ledger peers
    establishedBigLedgerPeersSet = establishedSet `Set.intersection` bigLedgerSet
    activeBigLedgerPeersSet      = establishedBigLedgerPeersSet `Set.intersection` activePeers

    -- bootstrap peers
    bootstrapSet                 = PublicRootPeers.getBootstrapPeers publicRootPeers
    -- bootstrap peers and big ledger peers are disjoint, hence we can use
    -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
    knownBootstrapPeersSet       = bootstrapSet
    establishedBootstrapPeersSet = establishedPeersSet `Set.intersection` bootstrapSet
    activeBootstrapPeersSet      = activePeersSet `Set.intersection` bootstrapSet

outboundConnectionsState
    :: Ord peeraddr
    => AssociationMode
    -> PeerSelectionSetsWithSizes (CardanoPeerSelectionView peeraddr) peeraddr
    -> PeerSelectionState CardanoPeerSelectionState PeerTrustable extraPeers peeraddr peerconn
    -> OutboundConnectionsState
outboundConnectionsState
    associationMode
    PeerSelectionView {
      viewEstablishedPeers       = (viewEstablishedPeers, _),
        viewActiveBigLedgerPeers = (_, activeNumBigLedgerPeers),
      viewExtraViews = CardanoPeerSelectionView {
        viewEstablishedBootstrapPeers = (viewEstablishedBootstrapPeers, _),
        viewActiveBootstrapPeers      = (viewActiveBootstrapPeers, _)
      }
    }
    PeerSelectionState {
      localRootPeers,
      extraState = CardanoPeerSelectionState {
        cpstConsensusMode,
        cpstBootstrapPeersFlag,
        cpstMinBigLedgerPeersForTrustedState
      }
    }
    =
    case (associationMode, cpstBootstrapPeersFlag, cpstConsensusMode) of
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
        |  activeNumBigLedgerPeers >= getMinBigLedgerPeersForTrustedState cpstMinBigLedgerPeersForTrustedState
        -> TrustedStateWithExternalPeers

        |  otherwise
        -> UntrustedState
  where
    trustableLocalRootSet = LocalRootPeers.trustableKeysSet localRootPeers

cardanoPeerSelectionGovernorArgs
  :: ( MonadSTM m
    , Ord peeraddr
     )
  => STM m UseLedgerPeers
  -> PeerSharing
  -> LedgerPeersConsensusInterface (CardanoLedgerPeersConsensusInterface m) m
  -> PeerSelectionGovernorArgs
       CardanoPeerSelectionState
       (CardanoPeerSelectionActions m)
       (CardanoPublicRootPeers peeraddr)
       (CardanoLedgerPeersConsensusInterface m)
       PeerTrustable
       (CardanoPeerSelectionView peeraddr)
       peeraddr
       peerconn
       BootstrapPeersCriticalTimeoutError
       m
cardanoPeerSelectionGovernorArgs readUseLedgerPeers peerSharing lpsci =
  PeerSelectionGovernorArgs {
    -- If by any chance the node takes more than 15 minutes to converge to a
    -- clean state, we crash the node. This could happen in very rare
    -- conditions such as a global network issue, DNS, or a bug in the code.
    -- In any case crashing the node will force the node to be restarted,
    -- starting in the correct state for it to make progress.
    abortGovernor   = \st ->
      case cpstBootstrapPeersTimeout (extraState st) of
        Nothing -> Nothing
        Just t
          | cpstBlockedAt (extraState st) >= t -> Just BootstrapPeersCriticalTimeoutError
          | otherwise                         -> Nothing
  , updateWithState = \psv st -> do
      associationMode <- readAssociationMode readUseLedgerPeers
                                             peerSharing
                                             (cpstBootstrapPeersFlag (extraState st))
      clpciUpdateOutboundConnectionsState (lpExtraAPI lpsci)
        (outboundConnectionsState associationMode psv st)
  , extraDecisions  =
      ExtraGuardedDecisions {
        preBlocking     =
          [ \_ psa pst -> monitorBootstrapPeersFlag   psa pst
          , \_ psa pst -> monitorLedgerStateJudgement psa pst
          , \_ _   pst -> waitForSystemToQuiesce          pst
          ]
      , postBlocking    = []
      , preNonBlocking  = []
      , postNonBlocking = []
      , requiredTargetsAction              = \_ -> targetPeers
      , requiredLocalRootsAction           = \_ -> localRoots
      , enableProgressMakingActions        = \st ->
          not (requiresBootstrapPeers (cpstBootstrapPeersFlag st) (cpstLedgerStateJudgement st))
      , ledgerPeerSnapshotExtraStateChange = \st ->
          st { cpstLedgerStateJudgement = YoungEnough }
      }
  }
