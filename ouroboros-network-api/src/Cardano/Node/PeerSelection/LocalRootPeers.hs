{-# LANGUAGE DeriveGeneric #-}

module Cardano.Node.PeerSelection.LocalRootPeers (OutboundConnectionsState (..)) where

import GHC.Generics
import NoThunks.Class

data OutboundConnectionsState =
    TrustedStateWithExternalPeers
    -- ^
    -- * /in the Praos mode/: connected only to trusted local
    --   peers and at least one bootstrap peer or public root;
    -- * /in the Genesis mode/: meeting target of active big ledger peers;
    -- * or it is in `LocalRootsOnly` mode and indeed only connected to
    --   trusted local root peers (see
    --   `Ouroboros.Network.PeerSelection.Governor.AssociationMode`).

  | UntrustedState
    -- ^ catch all other cases
  deriving (Eq, Show, Generic)

instance NoThunks OutboundConnectionsState
