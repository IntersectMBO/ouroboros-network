module Codec.SerialiseTerm where

import           Codec.CBOR.Term as CBOR

data CodecCBORTerm fail a = CodecCBORTerm
  { encodeTerm :: a -> CBOR.Term
  , decodeTerm :: CBOR.Term -> Either fail a
  }
