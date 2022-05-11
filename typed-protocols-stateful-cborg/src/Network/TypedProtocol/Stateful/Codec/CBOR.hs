{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Stateful.Codec.CBOR
  ( module Network.TypedProtocol.Stateful.Codec
  , DeserialiseFailure
  , mkCodecCborLazyBS
  , mkCodecCborStrictBS
  ) where

import           Control.Monad.Class.MonadST (MonadST (..))

import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import           Data.Singletons

import           Network.TypedProtocol.Stateful.Codec
import           Network.TypedProtocol.Codec.CBOR (DeserialiseFailure,
                   convertCborDecoderBS, convertCborDecoderLBS)
import           Network.TypedProtocol.Core


-- | Construct a 'Codec' for a CBOR based serialisation format, using strict
-- 'BS.ByteString's.
--
-- This is an adaptor between the @cborg@ library and the 'Codec' abstraction.
--
-- It takes encode and decode functions for the protocol messages that use the
-- CBOR library encoder and decoder.
--
-- Note that this is /less/ efficient than the 'mkCodecCborLazyBS' variant
-- because it has to copy and concatenate the result of the encoder (which
-- natively produces chunks).
--
mkCodecCborStrictBS
  :: forall ps f m. MonadST m

  => (forall (st :: ps) (st' :: ps).
             SingI st
          => ActiveState st
          => f st' -> Message ps st st' -> CBOR.Encoding)

  -> (forall (st :: ps) s.
             ActiveState st
          => Sing st
          -> f st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps DeserialiseFailure f m BS.ByteString
mkCodecCborStrictBS cborMsgEncode cborMsgDecode =
    Codec {
      encode = \f msg  -> convertCborEncoder (cborMsgEncode f) msg,
      decode = \stok f -> convertCborDecoder (cborMsgDecode stok f)
    }
  where
    convertCborEncoder :: (a -> CBOR.Encoding) -> a -> BS.ByteString
    convertCborEncoder cborEncode =
        CBOR.toStrictByteString
      . cborEncode

    convertCborDecoder
      :: (forall s. CBOR.Decoder s a)
      -> m (DecodeStep BS.ByteString DeserialiseFailure m a)
    convertCborDecoder cborDecode =
        withLiftST (convertCborDecoderBS cborDecode)

-- | Construct a 'Codec' for a CBOR based serialisation format, using lazy
-- 'BS.ByteString's.
--
-- This is an adaptor between the @cborg@ library and the 'Codec' abstraction.
--
-- It takes encode and decode functions for the protocol messages that use the
-- CBOR library encoder and decoder.
--
mkCodecCborLazyBS
  :: forall ps f m. MonadST m

  => (forall (st :: ps) (st' :: ps).
             SingI st
          => ActiveState st
          => f st'
          -> Message ps st st' -> CBOR.Encoding)

  -> (forall (st :: ps) s.
             ActiveState st
          => Sing st
          -> f st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps CBOR.DeserialiseFailure f m LBS.ByteString
mkCodecCborLazyBS cborMsgEncode cborMsgDecode =
    Codec {
      encode = \f msg  -> convertCborEncoder (cborMsgEncode f) msg,
      decode = \stok f -> convertCborDecoder (cborMsgDecode stok f)
    }
  where
    convertCborEncoder :: (a -> CBOR.Encoding) -> a -> LBS.ByteString
    convertCborEncoder cborEncode =
        toLazyByteString
      . CBOR.toBuilder
      . cborEncode

    convertCborDecoder
      :: (forall s. CBOR.Decoder s a)
      -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
    convertCborDecoder cborDecode =
        withLiftST (convertCborDecoderLBS cborDecode)

{-# NOINLINE toLazyByteString #-}
toLazyByteString :: BS.Builder -> LBS.ByteString
toLazyByteString = BS.toLazyByteStringWith strategy LBS.empty
  where
    -- Buffer strategy and sizes better tuned to our network protocol situation.
    --
    -- The LBS.smallChunkSize is 4k - heap object overheads, so that
    -- it does fit in a 4k overall.
    --
    strategy = BS.untrimmedStrategy 800 LBS.smallChunkSize

