{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Network.PeerSelection.ExtraRootPeers
  ( ExtraPeers (..)
  , ToExtraTrace (..)
  , ViewExtraPeers (..)
  , toAdvertisePeersMap
  , intersection
  , difference
  , size
  , toSet
  , member
  , nullAll
  , nullBootstrap
  , nullPublicConfig
  , empty
  , fromMapAndSet
  , invariant
  , cardanoPublicRootPeersAPI
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Cardano.Network.LedgerStateJudgement
import Cardano.Network.PeerSelection.Bootstrap
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionState (..),
           SupportsPeerSelectionState (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers (..))
import Ouroboros.Network.PeerSelection.State.EstablishedPeers qualified as EstablishedPeers
import Ouroboros.Network.PeerSelection.Types

data ExtraPeers peeraddr =
  ExtraPeers
    { publicConfigPeers :: !(Map peeraddr PeerAdvertise)
    , bootstrapPeers    :: !(Set peeraddr)
    }
  deriving (Eq, Show)


instance ( Ord peeraddr
         , Show peeraddr
         ) => SupportsPeerSelectionState (ExtraPeers peeraddr) peeraddr where
  data ToExtraTrace (ExtraPeers peeraddr) =
           TraceLedgerStateJudgementChanged LedgerStateJudgement
         | TraceUseBootstrapPeersChanged UseBootstrapPeers
    deriving Show

  data ViewExtraPeers (ExtraPeers peeraddr) = ExtraPeerSelectionSetsWithSizes {
      viewKnownBootstrapPeers           :: (Set peeraddr, Int)
    , viewColdBootstrapPeersPromotions  :: (Set peeraddr, Int)
    , viewEstablishedBootstrapPeers     :: (Set peeraddr, Int)
    , viewWarmBootstrapPeersDemotions   :: (Set peeraddr, Int)
    , viewWarmBootstrapPeersPromotions  :: (Set peeraddr, Int)
    , viewActiveBootstrapPeers          :: (Set peeraddr, Int)
    , viewActiveBootstrapPeersDemotions :: (Set peeraddr, Int)
    } deriving (Eq, Show)

  publicExtraPeersAPI = cardanoPublicRootPeersAPI

  mkViewExtraPeers PeerSelectionState {..} =
      ExtraPeerSelectionSetsWithSizes {
          viewKnownBootstrapPeers           = size'   knownBootstrapPeersSet
        , viewColdBootstrapPeersPromotions  = size' $ knownBootstrapPeersSet
                                              `Set.intersection` inProgressPromoteCold
        , viewEstablishedBootstrapPeers     = size'   establishedBootstrapPeersSet
        , viewWarmBootstrapPeersDemotions   = size' $ establishedBootstrapPeersSet
                                              `Set.intersection` inProgressDemoteWarm
        , viewWarmBootstrapPeersPromotions  = size' $ establishedBootstrapPeersSet
                                              `Set.intersection` inProgressPromoteWarm
        , viewActiveBootstrapPeers          = size'   activeBootstrapPeersSet
        , viewActiveBootstrapPeersDemotions = size' $ activeBootstrapPeersSet
                                              `Set.intersection` inProgressDemoteHot
        }
    where
      size' s = (s, Set.size s)

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
      bootstrapSet = bootstrapPeers $ getExtraPeers publicRootPeers
      -- bootstrap peers and big ledger peers are disjoint, hence we can use
      -- `knownPeersSet`, `establishedPeersSet` and `activePeersSet` below.
      knownBootstrapPeersSet       = bootstrapSet
      establishedBootstrapPeersSet = establishedPeersSet `Set.intersection` bootstrapSet
      activeBootstrapPeersSet      = activePeersSet `Set.intersection` bootstrapSet


instance Ord peeraddr => Semigroup (ExtraPeers peeraddr) where
  (ExtraPeers a b) <> (ExtraPeers a' b') =
    let -- Combine the sets, prioritizing bootstrapPeers
        combinedSet = b `Set.union` b'
        -- Combine the maps and remove any peers that are now in the set
        combinedMap = (a `Map.union` a') `Map.withoutKeys` combinedSet
    in ExtraPeers combinedMap combinedSet

instance Ord peeraddr => Monoid (ExtraPeers peeraddr) where
  mempty = empty

-- Cardano Public Root Peers Actions
cardanoPublicRootPeersAPI :: Ord peeraddr => PublicExtraPeersAPI (ExtraPeers peeraddr) peeraddr
cardanoPublicRootPeersAPI =
  PublicExtraPeersAPI {
    nullExtraPeers         = nullAll
  , invariantExtraPeers    = invariant
  , memberExtraPeers       = member
  , extraPeersToSet        = toSet
  , sizeExtraPeers         = size
  , differenceExtraPeers   = difference
  , intersectionExtraPeers = intersection
  , toAdvertise            = toAdvertisePeersMap
  }

-- Map and Set are disjoint
invariant :: Ord peeraddr => ExtraPeers peeraddr -> Bool
invariant (ExtraPeers a b) = all (`Map.notMember` a) b

fromMapAndSet :: Ord peeraddr
              => Map peeraddr PeerAdvertise
              -> Set peeraddr
              -> ExtraPeers peeraddr
fromMapAndSet pp bsp =
  let newPP = pp `Map.withoutKeys` bsp
   in ExtraPeers newPP bsp

empty :: ExtraPeers peeraddr
empty = ExtraPeers Map.empty Set.empty

nullPublicConfig :: ExtraPeers peeraddr -> Bool
nullPublicConfig ExtraPeers { publicConfigPeers } =
  Map.null publicConfigPeers

nullBootstrap :: ExtraPeers peeraddr -> Bool
nullBootstrap ExtraPeers { bootstrapPeers } =
  Set.null bootstrapPeers

nullAll :: ExtraPeers peeraddr -> Bool
nullAll cprp = nullPublicConfig cprp && nullBootstrap cprp

member :: Ord peeraddr => peeraddr -> ExtraPeers peeraddr -> Bool
member addr (ExtraPeers a b) =
  Map.member addr a || Set.member addr b

toSet :: Ord peeraddr => ExtraPeers peeraddr -> Set peeraddr
toSet (ExtraPeers a b) = Map.keysSet a <> b

size :: ExtraPeers peeraddr -> Int
size (ExtraPeers a b) = Map.size a + Set.size b

difference :: Ord peeraddr
           => ExtraPeers peeraddr
           -> Set peeraddr
           -> ExtraPeers peeraddr
difference (ExtraPeers a b) addrs =
  ExtraPeers (Map.withoutKeys a addrs)
                         (Set.difference b addrs)

intersection :: Ord peeraddr
             => ExtraPeers peeraddr
             -> Set peeraddr
             -> ExtraPeers peeraddr
intersection (ExtraPeers a b) addrs =
  ExtraPeers (Map.restrictKeys a addrs)
                         (Set.intersection b addrs)

toAdvertisePeersMap :: Ord peeraddr
                    => ExtraPeers peeraddr
                    -> Map peeraddr PeerAdvertise
toAdvertisePeersMap (ExtraPeers a b) =
  a <> Set.foldl (\m p -> Map.insert p DoNotAdvertisePeer m) Map.empty b
