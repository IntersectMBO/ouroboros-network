{-# LANGUAGE CPP #-}
module Ouroboros.Network.Serialise (
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

import           Codec.CBOR.Decoding hiding (DecodeAction (..), TokenType (..))
import           Codec.CBOR.Encoding hiding (Encoding (..), Tokens (..))
import           Codec.CBOR.Encoding (Encoding)
import           Codec.CBOR.FlatTerm
import           Codec.CBOR.Write (toLazyByteString)
import           Codec.Serialise
import           Codec.Serialise.Decoding (decodeBytes)
import           Test.QuickCheck (Property, counterexample, (.&&.), (===))

-- Class properties
--

prop_serialise :: (Serialise a, Eq a, Show a) => a -> Property
prop_serialise x =     prop_serialise_valid x
                  .&&. prop_serialise_roundtrip x

prop_serialise_valid :: Serialise a => a -> Property
prop_serialise_valid a =
    let e = encode a
        f = toFlatTerm e
        s = "invalid flat term " ++ show f ++ " from encoding " ++ show e
    in  counterexample s $ validFlatTerm f

prop_serialise_roundtrip :: (Serialise a, Eq a, Show a) => a -> Property
prop_serialise_roundtrip x = deserialiseOrFail (serialise x) === Right x
