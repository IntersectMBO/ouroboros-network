{-# LANGUAGE CPP #-}
module Serialise (
      -- * Class
      Serialise(..)
    , prop_serialise
    , prop_serialise_valid
    , prop_serialise_roundtrip


      -- * Re-exports
    , module Codec.CBOR.Encoding
    , module Codec.CBOR.Decoding
    , (<>)
    , serialise
    , deserialise
    , deserialiseOrFail
    , toLazyByteString
    )
    where

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid
#endif
import Codec.CBOR.Decoding      hiding (DecodeAction (..), TokenType (..))
import Codec.CBOR.Encoding      hiding (Encoding (..), Tokens (..))
import Codec.CBOR.Encoding      (Encoding)
import Codec.CBOR.FlatTerm
import Codec.CBOR.Write         (toLazyByteString)
import Codec.Serialise
import Codec.Serialise.Decoding (decodeBytes)

-- Class properties
--
prop_serialise :: (Serialise a, Eq a) => a -> Bool
prop_serialise x = prop_serialise_valid x
                && prop_serialise_roundtrip x

prop_serialise_valid :: Serialise a => a -> Bool
prop_serialise_valid = validFlatTerm . toFlatTerm . encode

prop_serialise_roundtrip :: (Serialise a, Eq a) => a -> Bool
prop_serialise_roundtrip x = deserialiseOrFail (serialise x) == Right x
