{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs  #-}

module Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..)) where

import           GHC.Generics (Generic)

-- | Is a peer willing to participate in Peer Sharing? If yes are others allowed
-- to share this peer's address?
-- Information about the node comes from the configuration file, while information about
-- other nodes is received via handshake.
--
-- NOTE: This information is only useful if P2P flag is enabled.
--
data PeerSharing = PeerSharingDisabled -- ^ Peer does not participate in Peer Sharing
                                       -- at all
                 | PeerSharingEnabled -- ^ Peer participates in Peer Sharing
  deriving  (Eq, Show, Read, Generic)

-- | The combination of two 'PeerSharing' values forms a Monoid where the unit
-- is 'PeerSharingEnabled'.
--
-- This operation is used in the connection handshake.
--
instance Semigroup PeerSharing where
  (<>) :: PeerSharing -> PeerSharing -> PeerSharing
  PeerSharingDisabled <> _                   = PeerSharingDisabled
  _                   <> PeerSharingDisabled = PeerSharingDisabled
  _                   <> _                   = PeerSharingEnabled

-- | The Monoid laws are witnessed by the following denotation function:
--
-- ⟦_⟧ :: PeerSharing -> All
-- ⟦ PeerSharingDisabled ⟧ = All False
-- ⟦ PeerSharingEnabled ⟧  = All True
--
instance Monoid PeerSharing where
  mempty :: PeerSharing
  mempty = PeerSharingEnabled
