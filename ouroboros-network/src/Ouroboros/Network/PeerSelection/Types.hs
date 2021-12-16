{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Network.PeerSelection.Types
  ( PeerSource (..)
  , PeerAdvertise (..)
  , PeerStatus (..)
  ) where

import           Data.Aeson
import           Data.Bool (bool)
import           GHC.Generics (Generic)


-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourceGossip
  deriving (Eq, Ord, Show, Enum)


-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise = DoAdvertisePeer
                   | DoNotAdvertisePeer
  deriving (Eq, Show, Generic)

instance FromJSON PeerAdvertise where
  parseJSON = withBool "PeerAdvertise" $
      return . bool DoNotAdvertisePeer DoAdvertisePeer

instance ToJSON PeerAdvertise where
  toJSON DoAdvertisePeer    = Bool True
  toJSON DoNotAdvertisePeer = Bool False

data PeerStatus =
       PeerCold
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

