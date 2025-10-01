module Ouroboros.Network.CodecCBORTerm
  ( CodecCBORTerm (..)
  , VersionDataCodec (..)
  , cborTermVersionDataCodec
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

-- | Codec for version data exchanged by the handshake protocol.
--
data VersionDataCodec vNumber vData = VersionDataCodec {
    encodeData :: vNumber -> vData -> CBOR.Term,
    -- ^ encoder of 'vData' which has access to 'extra vData' which can bring
    -- extra instances into the scope (by means of pattern matching on a GADT).
    decodeData :: vNumber -> CBOR.Term -> Either Text vData
    -- ^ decoder of 'vData'.
  }

cborTermVersionDataCodec :: (vNumber -> CodecCBORTerm Text vData)
                         -> VersionDataCodec vNumber vData
cborTermVersionDataCodec codec = VersionDataCodec {
      encodeData = encodeTerm . codec,
      decodeData = decodeTerm . codec
    }
