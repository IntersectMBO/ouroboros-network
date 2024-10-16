{-# LANGUAGE LambdaCase #-}

module Ouroboros.Network.PeerSelection.PeerSharing.Codec
  ( encodePortNumber
  , decodePortNumber
  , encodeRemoteAddress
  , decodeRemoteAddress
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR

import Network.Socket (PortNumber, SockAddr (..))
import Ouroboros.Network.NodeToNode.Version qualified as NodeToNode

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
encodeRemoteAddress :: NodeToNode.Version -> SockAddr -> CBOR.Encoding
encodeRemoteAddress =
  \case
    NodeToNode.V_13 -> sockAddr
    NodeToNode.V_14 -> sockAddr

  where
    sockAddr = \case
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
decodeRemoteAddress :: NodeToNode.Version -> CBOR.Decoder s SockAddr
decodeRemoteAddress =
  \case
    NodeToNode.V_13 -> decoder13
    NodeToNode.V_14 -> decoder13

  where
    decoder13 = do
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
