{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Protocol.Codec.Cbor where

import           Control.Monad.ST

import           Data.ByteString (ByteString)
import qualified Data.Text as T (pack)

import qualified Codec.CBOR.Decoding as CBOR hiding (DecodeAction(Fail, Done))
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR

import           Protocol.Codec

-- | Convert a CBOR decoder type into a Protocol.Codec.Decoder.
cborDecoder
  :: forall s tr state .
     CBOR.Decoder s (Decoded tr state (Codec (ST s) CBOR.Encoding ByteString tr))
  -> Decoder (ST s) ByteString (Decoded tr state (Codec (ST s) CBOR.Encoding ByteString tr))
cborDecoder decoder = Decoder (idecode <$> CBOR.deserialiseIncremental decoder)
  where
  idecode
    :: CBOR.IDecode s (Decoded tr state (Codec (ST s) CBOR.Encoding ByteString tr))
    -> DecoderStep (ST s) ByteString (Decoded tr state (Codec (ST s) CBOR.Encoding ByteString tr))
  idecode term = case term of
    CBOR.Fail bs _ (CBOR.DeserialiseFailure _ str) -> Fail (Just bs) (T.pack str)
    CBOR.Done bs _ it -> Done (Just bs) it
    CBOR.Partial k -> Partial $ Decoder . fmap idecode . k
