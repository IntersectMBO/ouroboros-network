{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Ouroboros.Network.RemoteAddress.Codec
  ( encodePortNumber
  , decodePortNumber
  , encodeRemoteAddress
  , decodeRemoteAddress
  , RemoteAddressEncoding (..)
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.Serialise (Serialise (..))
import Data.Aeson qualified as Aeson
import GHC.Generics

import Network.Socket (PortNumber, SockAddr (..))

encodePortNumber :: PortNumber -> CBOR.Encoding
encodePortNumber = CBOR.encodeWord16 . fromIntegral

decodePortNumber :: CBOR.Decoder s PortNumber
decodePortNumber = fromIntegral <$> CBOR.decodeWord16


-- | This encoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
---
-- /Invariant:/ not a unix socket address type.
---
encodeRemoteAddress :: SockAddr -> CBOR.Encoding
encodeRemoteAddress = \case
  SockAddrInet pn w -> CBOR.encodeListLen 3
                    <> CBOR.encodeWord 0
                    <> CBOR.encodeWord32 w
                    <> encodePortNumber pn
  SockAddrInet6 pn _ (w1, w2, w3, w4) _ -> CBOR.encodeListLen 6
                                        <> CBOR.encodeWord 1
                                        <> CBOR.encodeWord32 w1
                                        <> CBOR.encodeWord32 w2
                                        <> CBOR.encodeWord32 w3
                                        <> CBOR.encodeWord32 w4
                                        <> encodePortNumber pn
  SockAddrUnix _ -> error "Should never be encoding a SockAddrUnix!"

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


-- | A newtype wrapper which provides `Serialise` instance.
--
newtype RemoteAddressEncoding addr =
    RemoteAddressEncoding { getRemoteAddressEncoding :: addr }
  deriving (Eq, Ord, Generic)

-- | This instance is used by `LocalStateQuery` mini-protocol codec in
-- `ouroboros-consensus-diffusion`.
--
instance Serialise (RemoteAddressEncoding SockAddr) where
  encode = encodeRemoteAddress . getRemoteAddressEncoding
  decode = RemoteAddressEncoding <$> decodeRemoteAddress

instance Aeson.ToJSON (RemoteAddressEncoding SockAddr) where
  toJSON     (RemoteAddressEncoding addr) = Aeson.toJSON (show addr)
  toEncoding (RemoteAddressEncoding addr) = Aeson.toEncoding (show addr)
