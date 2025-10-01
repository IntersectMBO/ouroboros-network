{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}

module Ouroboros.Network.CodecCBORTerm
  ( CodecCBORTerm (..)
  , VersionDataCodec
  , VersionedCodecCBORTerm (.., VersionDataCodec, encodeData, decodeData)
  , mkVersionedCodecCBORTerm
  , unVersionCodecCBORTerm
  ) where

import Codec.CBOR.Term qualified as CBOR
import Data.Text (Text)


-- | A pure codec which encodes to / decodes from 'CBOR.Term'.  This is useful
-- if one expects a valid @cbor@ encoding, which one might not know how to
-- decode like in the 'Handshake' protocol.
--
data CodecCBORTerm fail a = CodecCBORTerm
  { encodeTerm :: a -> CBOR.Term
  , decodeTerm :: CBOR.Term -> Either fail a
  }


-- | A pure codec which encodes to / decodes from `CBOR.Term` which can
-- depend on a version.
--
data VersionedCodecCBORTerm fail v a = VersionedCodecCBORTerm {
    encodeVersionedTerm :: v -> a -> CBOR.Term,
    decodeVersionedTerm :: v -> CBOR.Term -> Either fail a
  }

mkVersionedCodecCBORTerm :: (vNumber -> CodecCBORTerm fail vData)
                         -> VersionedCodecCBORTerm  fail vNumber vData
mkVersionedCodecCBORTerm codec = VersionedCodecCBORTerm {
      encodeVersionedTerm = encodeTerm . codec,
      decodeVersionedTerm = decodeTerm . codec
    }

unVersionCodecCBORTerm :: VersionedCodecCBORTerm fail vNumber vData
                       -> vNumber -> CodecCBORTerm fail vData
unVersionCodecCBORTerm VersionedCodecCBORTerm{encodeVersionedTerm, decodeVersionedTerm} v =
    CodecCBORTerm {
      encodeTerm = encodeVersionedTerm v,
      decodeTerm = decodeVersionedTerm v
    }

--
-- A specialised VersionedCodecCBORTerm used for encoding / decoding
-- handshake's version data
--

type VersionDataCodec versionNumber versionData =
    VersionedCodecCBORTerm Text versionNumber versionData

-- | Codec for version data exchanged by the handshake protocol.
--
pattern VersionDataCodec :: (v -> a -> CBOR.Term)
                         -> (v -> CBOR.Term -> Either Text a)
                         -> VersionDataCodec v a
pattern VersionDataCodec { encodeData, decodeData } =
    VersionedCodecCBORTerm encodeData decodeData
{-# COMPLETE VersionDataCodec #-}
