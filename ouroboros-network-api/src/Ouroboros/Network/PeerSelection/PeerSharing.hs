{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.PeerSelection.PeerSharing
  ( PeerSharing (..)
  , combinePeerSharing
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
import           Text.Read (readMaybe)

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

instance FromJSON PeerSharing where
  parseJSON = withText "PeerSharing" $ \t ->
    case readMaybe (Text.unpack t) of
      Nothing -> fail ("PeerSharing.parseJSON: could not parse value: "
                     ++ Text.unpack t)
      Just ps -> return ps

instance ToJSON PeerSharing where
  toJSON = String . Text.pack . show

-- | Combine two 'PeerSharing' values
--
-- 'PeerSharingDisabled' is the absorbing element
combinePeerSharing :: PeerSharing -> PeerSharing -> PeerSharing
combinePeerSharing PeerSharingDisabled _ = PeerSharingDisabled
combinePeerSharing _ PeerSharingDisabled = PeerSharingDisabled
combinePeerSharing _ _                   = PeerSharingEnabled

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
encodeRemoteAddress (SockAddrInet6 pn _ (w1, w2, w3, w4) _) = CBOR.encodeListLen 6
                                                <> CBOR.encodeWord 1
                                                <> CBOR.encodeWord32 w1
                                                <> CBOR.encodeWord32 w2
                                                <> CBOR.encodeWord32 w3
                                                <> CBOR.encodeWord32 w4
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
      pn <- decodePortNumber
      return (SockAddrInet6 pn 0 (w1, w2, w3, w4) 0)
    _ -> fail ("Serialise.decode.SockAddr unexpected tok " ++ show tok)
