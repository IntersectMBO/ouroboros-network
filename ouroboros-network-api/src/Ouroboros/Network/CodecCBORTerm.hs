module Ouroboros.Network.CodecCBORTerm where

import qualified Codec.CBOR.Term as CBOR


-- | A pure codec which encodes to / decodes from 'CBOR.Term'.  This is useful
-- if one expects a valid @cbor@ encoding, which one might not know how to
-- decode like in the 'Handshake' protocol.
--
data CodecCBORTerm fail a = CodecCBORTerm
  { encodeTerm :: a -> CBOR.Term
  , decodeTerm :: CBOR.Term -> Either fail a
  }
