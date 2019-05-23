module Codec.SerialiseTerm where

import           Data.Text (Text)
import           Codec.CBOR.Term as CBOR

-- |
-- Serialisation and de-serialisation for CBOR-in-CBOR encoding using
-- @'CBOR.Term'@.
--
class SerialiseTerm a where
  encodeTerm :: a -> CBOR.Term
  decodeTerm :: CBOR.Term -> Either Text a
