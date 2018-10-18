
module Serialise (
      -- * Class
      Serialise(..)
    , prop_serialise
    , prop_serialise_valid
    , prop_serialise_roundtrip

      -- * Functions
    , serialise
    , deserialise

      -- * Re-exports
    , module Codec.CBOR.Encoding
    , module Codec.CBOR.Decoding
    , (<>)
    )
    where

import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import Codec.CBOR.Encoding hiding (Encoding(..), Tokens(..))
import Codec.CBOR.Encoding        (Encoding)
import Codec.CBOR.Decoding hiding (DecodeAction(..), TokenType(..))
import Codec.CBOR.FlatTerm
import Codec.CBOR.Write (toLazyByteString)
import Codec.CBOR.Read  (deserialiseFromBytes, DeserialiseFailure)


class Serialise a where
    encode  :: a -> Encoding
    decode  :: Decoder s a

-- Class properties
--
prop_serialise :: (Serialise a, Eq a) => a -> Bool
prop_serialise x = prop_serialise_valid x
                && prop_serialise_roundtrip x

prop_serialise_valid :: Serialise a => a -> Bool
prop_serialise_valid = validFlatTerm . toFlatTerm . encode

prop_serialise_roundtrip :: (Serialise a, Eq a) => a -> Bool
prop_serialise_roundtrip x = deserialise (serialise x) == Right (mempty, x)

---------------------------------------

serialise :: Serialise a => a -> LBS.ByteString
serialise = toLazyByteString . encode

deserialise :: Serialise a
            => LBS.ByteString -> Either DeserialiseFailure (LBS.ByteString, a)
deserialise = deserialiseFromBytes decode

---------------------------------------

instance Serialise a => Serialise [a] where
  encode xs = encodeListLen (fromIntegral $ length xs)
           <> foldr (\x r -> encode x <> r) mempty xs

  decode =  do
    n  <- decodeListLen
    decodeSequenceLenN (flip (:)) [] reverse n decode


