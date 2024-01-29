module Ouroboros.Network.PeerSelection.PeerSharing.Codec
  ( encodePortNumber
  , decodePortNumber
  , encodeRemoteAddress
  , decodeRemoteAddress
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR

import Network.Socket (PortNumber, SockAddr (..))
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion (..))

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
encodeRemoteAddress :: NodeToNodeVersion -> SockAddr -> CBOR.Encoding
encodeRemoteAddress ntnVersion sockAddr
  | ntnVersion >= NodeToNodeV_13 =
    case sockAddr of
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
  | otherwise =
    case sockAddr of
      SockAddrInet pn w -> CBOR.encodeListLen 3
                        <> CBOR.encodeWord 0
                        <> CBOR.encodeWord32 w
                        <> encodePortNumber pn
      SockAddrInet6 pn fi (w1, w2, w3, w4) si -> CBOR.encodeListLen 8
                                              <> CBOR.encodeWord 1
                                              <> CBOR.encodeWord32 w1
                                              <> CBOR.encodeWord32 w2
                                              <> CBOR.encodeWord32 w3
                                              <> CBOR.encodeWord32 w4
                                              <> CBOR.encodeWord32 fi
                                              <> CBOR.encodeWord32 si
                                              <> encodePortNumber pn
      SockAddrUnix _ -> error "Should never be encoding a SockAddrUnix!"

-- | This decoder should be faithful to the PeerSharing
-- CDDL Specification.
--
-- See the network design document for more details
--
decodeRemoteAddress :: NodeToNodeVersion -> CBOR.Decoder s SockAddr
decodeRemoteAddress ntnVersion
  | ntnVersion >= NodeToNodeV_13 = do
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
  | otherwise = do
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
        _fi <- CBOR.decodeWord32
        _si <- CBOR.decodeWord32
        pn <- decodePortNumber
        return (SockAddrInet6 pn 0 (w1, w2, w3, w4) 0)
      _ -> fail ("Serialise.decode.SockAddr unexpected tok " ++ show tok)

