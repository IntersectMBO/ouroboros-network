{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..)) where

import           Data.Aeson
import           Data.Bool (bool)
import           GHC.Generics (Generic)

-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise = DoNotAdvertisePeer
                   | DoAdvertisePeer
  deriving (Eq, Show, Ord, Generic)

instance FromJSON PeerAdvertise where
  parseJSON = withBool "PeerAdvertise" $
      return . bool DoNotAdvertisePeer DoAdvertisePeer

instance ToJSON PeerAdvertise where
  toJSON DoAdvertisePeer    = Bool True
  toJSON DoNotAdvertisePeer = Bool False
