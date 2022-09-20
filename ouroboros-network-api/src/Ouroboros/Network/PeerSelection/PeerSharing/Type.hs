{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.PeerSelection.PeerSharing.Type
  ( PeerSharing (..)
  , combinePeerInformation
  ) where

import           Data.Aeson
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Ouroboros.Network.PeerSelection.PeerAdvertise.Type
                     (PeerAdvertise (..))
import           Text.Read (readMaybe)

-- | Is a peer willing to participate in Peer Sharing? If yes are others allowed
-- to share this peer's address?
-- Information about the node comes from the configuration file, while information about
-- other nodes is received via handshake.
--
-- NOTE: This information is only useful if P2P flag is enabled.
--
data PeerSharing = NoPeerSharing -- ^ Peer does not participate in Peer Sharing
                                 -- at all
                 | PeerSharingPrivate -- ^ Peer participates in Peer Sharing but
                                      -- its address should be private
                 | PeerSharingPublic -- ^ Peer participates in Peer Sharing
  deriving  (Eq, Show, Read, Generic)

instance FromJSON PeerSharing where
  parseJSON = withText "PeerSharing" $ \t ->
    case readMaybe (Text.unpack t) of
      Nothing -> fail ("PeerSharing.parseJSON: could not parse value: "
                     ++ Text.unpack t)
      Just ps -> return ps

instance ToJSON PeerSharing where
  toJSON = String . Text.pack . show

-- Combine a 'PeerSharing' value and a 'PeerAdvertise' value into a
-- resulting 'PeerSharing' that can be used to decide if we should
-- share or not the given Peer. According to the following rules:
--
-- - If no PeerSharing value is known then there's nothing we can assess
-- - If a peer is not participating in Peer Sharing ignore all other information
-- - If a peer said it wasn't okay to share its address, respect that no matter what.
-- - If a peer was privately configured with DoNotAdvertisePeer respect that no matter
-- what.
--
combinePeerInformation :: PeerSharing -> PeerAdvertise -> PeerSharing
combinePeerInformation NoPeerSharing      _                  = NoPeerSharing
combinePeerInformation PeerSharingPrivate _                  = PeerSharingPrivate
combinePeerInformation PeerSharingPublic  DoNotAdvertisePeer = PeerSharingPrivate
combinePeerInformation _                  _                  = PeerSharingPublic
