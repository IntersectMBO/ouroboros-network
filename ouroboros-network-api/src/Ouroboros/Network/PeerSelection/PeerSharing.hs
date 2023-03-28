{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.PeerSharing
  ( PeerSharing (..)
  , combinePeerInformation
  , encodePortNumber
  , decodePortNumber
  , encodeRemoteAddress
  , decodeRemoteAddress
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),
                     withText)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Network.Socket (PortNumber, SockAddr (..))
import           Ouroboros.Network.PeerSelection.PeerAdvertise
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
combinePeerInformation _                         _           = PeerSharingPublic

encodePortNumber :: PortNumber -> CBOR.Encoding
encodePortNumber = CBOR.encodeWord16 . fromIntegral

decodePortNumber :: CBOR.Decoder s PortNumber
decodePortNumber = fromIntegral <$> CBOR.decodeWord16


-- | This encoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
--
encodeRemoteAddress :: SockAddr -> CBOR.Encoding
encodeRemoteAddress (SockAddrInet pn w) = CBOR.encodeListLen 3
                           <> CBOR.encodeWord 0
                           <> CBOR.encodeWord32 w
                           <> encodePortNumber pn
encodeRemoteAddress (SockAddrInet6 pn fi (w1, w2, w3, w4) si) = CBOR.encodeListLen 8
                                                <> CBOR.encodeWord 1
                                                <> CBOR.encodeWord32 w1
                                                <> CBOR.encodeWord32 w2
                                                <> CBOR.encodeWord32 w3
                                                <> CBOR.encodeWord32 w4
                                                <> CBOR.encodeWord32 fi
                                                <> CBOR.encodeWord32 si
                                                <> encodePortNumber pn
encodeRemoteAddress (SockAddrUnix _) = error "Should never be encoding a SockAddrUnix!"

-- | This decoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
--
decodeRemoteAddress :: CBOR.Decoder s SockAddr
decodeRemoteAddress = do
  _ <- CBOR.decodeListLen
  tok <- CBOR.decodeWord
  case tok of
    0 -> do
      w <- CBOR.decodeWord32
      pn <- decodePortNumber
      return (SockAddrInet pn w)
    1 -> do
      w1 <- CBOR.decodeWord32
      w2 <- CBOR.decodeWord32
      w3 <- CBOR.decodeWord32
      w4 <- CBOR.decodeWord32
      fi <- CBOR.decodeWord32
      si <- CBOR.decodeWord32
      pn <- decodePortNumber
      return (SockAddrInet6 pn fi (w1, w2, w3, w4) si)
    _ -> fail ("Serialise.decode.SockAddr unexpected tok " ++ show tok)
