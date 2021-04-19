{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Network.PeerSelection.Types (
    PeerSource(..),
    PeerAdvertise(..),
    PeerStatus(..),
  ) where

import           GHC.Generics (Generic)
import           Data.Aeson


-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourceGossip
                | PeerSourceStaleRoot
--              | PeerSource -- it requested us to advertise it
  deriving (Eq, Ord, Show, Enum)


-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise = DoAdvertisePeer
                   | DoNotAdvertisePeer
  deriving (Eq, Show, Generic)

instance FromJSON PeerAdvertise where
  parseJSON = withObject "PeerAdvertise" $ \v -> do
    advertise <- v .: "advertise"
    if advertise then return DoAdvertisePeer
                 else return DoNotAdvertisePeer

instance ToJSON PeerAdvertise where
  toJSON DoAdvertisePeer =
    object
      [ "advertise" .= True ]
  toJSON DoNotAdvertisePeer =
    object
      [ "advertise" .= False ]

data PeerStatus =
       PeerCold
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

