{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.PeerSelection.Governor.Types where

import Cardano.Network.PublicRootPeers (CardanoPublicRootPeers)
import Data.Set (Set)
import Data.Set qualified as Set
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers

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
