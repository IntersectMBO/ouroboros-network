{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.PeerSelection.Types (
    PeerSource(..),
    PeerAdvertise(..),
  ) where

import           GHC.Generics (Generic)


-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourceGossip
--              | PeerSource -- it requested us to advertise it
  deriving (Eq, Show, Enum)


-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise = DoAdvertisePeer
                   | DoNotAdvertisePeer
  deriving (Eq, Show, Generic)

