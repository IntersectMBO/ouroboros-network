{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.Codec.CBOR
  ( module Network.TypedProtocol.Codec
  , DeserialiseFailure
  , mkCodecCborLazyBS
  , mkCodecCborStrictBS
  ) where

import           Control.Monad.ST
import           Control.Monad.Class.MonadST (MonadST (..))

import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read  as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec


type DeserialiseFailure = CBOR.DeserialiseFailure

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
  :: forall ps m. MonadST m

  => (forall (pr :: PeerRole) (st :: ps) (st' :: ps).
             PeerHasAgency pr st
          -> Message ps st st' -> CBOR.Encoding)

  -> (forall (pr :: PeerRole) (st :: ps) s.
             PeerHasAgency pr st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps DeserialiseFailure m BS.ByteString
mkCodecCborStrictBS cborMsgEncode cborMsgDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder (cborMsgEncode stok) msg,
      decode = \stok     -> convertCborDecoder (cborMsgDecode stok)
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

convertCborDecoderBS
  :: forall s m a. Functor m
  => (CBOR.Decoder s a)
  -> (forall b. ST s b -> m b)
  -> m (DecodeStep BS.ByteString DeserialiseFailure m a)
convertCborDecoderBS cborDecode liftST =
    go <$> liftST (CBOR.deserialiseIncremental cborDecode)
  where
    go :: CBOR.IDecode s a
       -> DecodeStep BS.ByteString DeserialiseFailure m a
    go (CBOR.Done  trailing _ x)
      | BS.null trailing       = DecodeDone x Nothing
      | otherwise              = DecodeDone x (Just trailing)
    go (CBOR.Fail _ _ failure) = DecodeFail failure
    go (CBOR.Partial k)        = DecodePartial (fmap go . liftST . k)


-- | Construct a 'Codec' for a CBOR based serialisation format, using lazy
-- 'BS.ByteString's.
--
-- This is an adaptor between the @cborg@ library and the 'Codec' abstraction.
--
-- It takes encode and decode functions for the protocol messages that use the
-- CBOR library encoder and decoder.
--
mkCodecCborLazyBS
  :: forall ps m. MonadST m

  => (forall (pr :: PeerRole) (st :: ps) (st' :: ps).
             PeerHasAgency pr st
          -> Message ps st st' -> CBOR.Encoding)

  -> (forall (pr :: PeerRole) (st :: ps) s.
             PeerHasAgency pr st
          -> CBOR.Decoder s (SomeMessage st))

  -> Codec ps CBOR.DeserialiseFailure m LBS.ByteString
mkCodecCborLazyBS  cborMsgEncode cborMsgDecode =
    Codec {
      encode = \stok msg -> convertCborEncoder (cborMsgEncode stok) msg,
      decode = \stok     -> convertCborDecoder (cborMsgDecode stok)
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

convertCborDecoderLBS
  :: forall s m a. Monad m
  => (CBOR.Decoder s a)
  -> (forall b. ST s b -> m b)
  -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
convertCborDecoderLBS cborDecode liftST =
    go [] =<< liftST (CBOR.deserialiseIncremental cborDecode)
  where
    -- Have to mediate between a CBOR decoder that consumes strict bytestrings
    -- and our choice here that consumes lazy bytestrings.
    go :: [BS.ByteString] -> CBOR.IDecode s a
       -> m (DecodeStep LBS.ByteString CBOR.DeserialiseFailure m a)
    go [] (CBOR.Done  trailing _ x)
      | BS.null trailing    = return (DecodeDone x Nothing)
      | otherwise           = return (DecodeDone x (Just trailing'))
                                where trailing' = LBS.fromStrict trailing
    go cs (CBOR.Done  trailing _ x) = return (DecodeDone x (Just trailing'))
                                where trailing' = LBS.fromChunks (trailing : cs)
    go _  (CBOR.Fail _ _ e) = return (DecodeFail e)

    -- We keep a bunch of chunks and supply the CBOR decoder with them
    -- until we run out, when we go get another bunch.
    go (c:cs) (CBOR.Partial  k) = go cs =<< liftST (k (Just c))
    go []     (CBOR.Partial  k) = return $ DecodePartial $ \mbs -> case mbs of
                                    Nothing -> go [] =<< liftST (k Nothing)
                                    Just bs -> go cs (CBOR.Partial k)
                                      where cs = LBS.toChunks bs

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

