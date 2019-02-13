{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Codec
  ( cborCodec
  ) where

import           Control.Monad.ST
import           Control.Monad.Class.MonadST (MonadST (..))

import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read  as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec


-- | Construct a 'Codec' for a CBOR based serialisation format.
--
-- Takes encode and decode functions for the protocol messages that use the
-- CBOR library encoder and decoder.
--
-- Or to put it another way: this is an adaptor between the @cborg@ CBOR
-- library and the 'Codec' abstraction.
--
cborCodec
  :: forall ps m. MonadST m

  => (forall (pr :: PeerRole) (st :: ps) (st' :: ps).
             PeerHasAgency pr st
          -> Message ps st st' -> CBOR.Encoding)

  -> (forall (pr :: PeerRole) (st :: ps) s.
             PeerHasAgency pr st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps CBOR.DeserialiseFailure m ByteString
cborCodec cborEncode cborDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder (cborEncode stok) msg,
      decode = \stok     -> convertCborDecoder (cborDecode stok)
    }

convertCborEncoder :: (a -> CBOR.Encoding) -> a -> ByteString
convertCborEncoder cborEncode =
    CBOR.toStrictByteString
  . cborEncode

convertCborDecoder
  :: MonadST m
  => (forall s. CBOR.Decoder s a)
  -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder cborDecode =
    withLiftST (convertCborDecoder' cborDecode)

convertCborDecoder'
  :: forall s m a. Functor m
  => (CBOR.Decoder s a)
  -> (forall b. ST s b -> m b)
  -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder' cborDecode liftST =
    go <$> liftST (CBOR.deserialiseIncremental cborDecode)
  where
    go (CBOR.Done  trailing _ x)
      | BS.null trailing       = DecodeDone x Nothing
      | otherwise              = DecodeDone x (Just trailing)
    go (CBOR.Fail _ _ failure) = DecodeFail failure
    go (CBOR.Partial k)        = DecodePartial (fmap go . liftST . k)

