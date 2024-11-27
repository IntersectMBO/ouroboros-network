{-# LANGUAGE DeriveGeneric #-}

module Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable (..)) where

import GHC.Generics (Generic)

-- | Is this Peer trustable as a bootstrap peer?
--
-- This trustability flag is used on local root peers (pre-genesis) to
-- distinguish which locally configured peer is considered safe to trust for
-- bootstrap purposes
--
data PeerTrustable = IsTrustable
                   | IsNotTrustable
  deriving (Eq, Show, Ord, Generic)
