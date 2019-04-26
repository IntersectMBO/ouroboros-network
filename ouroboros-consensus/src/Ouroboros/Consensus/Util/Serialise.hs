{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.Serialise (
      encodeBA
    , decodeBA
    , toBS
    ) where

import qualified Codec.CBOR.Write as CBOR.Write
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import           Crypto.Error (CryptoFailable (..))
import           Data.ByteArray (ByteArrayAccess, convert)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)

encodeBA :: ByteArrayAccess ba => ba -> Encoding
encodeBA ba = let bs = convert ba :: ByteString in encode bs

decodeBA :: forall a s. (ByteString -> CryptoFailable a) -> Decoder s a
decodeBA f = do
    bs <- decode :: Decoder s ByteString
    case f bs of
        CryptoPassed a -> return a
        CryptoFailed e -> fail $ "decodeBA: " ++ show e

toBS :: Encoding -> ByteString
toBS = toStrict . CBOR.Write.toLazyByteString
