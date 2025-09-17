{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Network.Diffusion.Topology where

import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..))
import Ouroboros.Network.Diffusion.Topology

type CardanoNetworkTopology =
  NetworkTopology UseBootstrapPeers PeerTrustable

-- | This function returns false if non-trustable peers are configured
--
isValidTrustedPeerConfiguration :: CardanoNetworkTopology -> Bool
isValidTrustedPeerConfiguration
  NetworkTopology { localRootPeersGroups = LocalRootPeersGroups lprgs
                  , extraConfig          = ubp
                  } =
    case ubp of
      DontUseBootstrapPeers   -> True
      UseBootstrapPeers []    -> anyTrustable
      UseBootstrapPeers (_:_) -> True
  where
    anyTrustable =
      any (\LocalRootPeersGroup { localRoots
                                , extraFlags = trustable
                                } ->
            case trustable of
              IsNotTrustable -> False
              IsTrustable    -> not
                              . null
                              . rootAccessPoints
                              $ localRoots
          ) lprgs
