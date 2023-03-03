{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.CBOR where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.Serialise as CBOR

data CBORCodec' a b = CBORCodec {
    encodeCBOR :: a -> CBOR.Encoding,
    decodeCBOR :: forall s. CBOR.Decoder s b
  }

type CBORCodec a = CBORCodec' a a

serialiseCodec :: ( CBOR.Serialise a
                  , CBOR.Serialise b
                  )
               => CBORCodec' a b
serialiseCodec = CBORCodec CBOR.encode CBOR.decode
