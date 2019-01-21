{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Network.TypedProtocol.Codec where

import           Control.Monad.ST (ST)
import           Ouroboros.Network.MonadClass.MonadST

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import qualified Codec.CBOR.Write    as CBOR
import qualified Codec.Serialise     as Serialise

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy.Internal as LBS (smallChunkSize)
import qualified Data.ByteString.Lazy          as LBS


data Codec bytes failure m a = Codec {
       encode    :: a -> [bytes],
       decode    :: m (DecodeStep bytes failure m a),
       chunkNull :: bytes -> Bool
     }

data DecodeStep bytes failure m a =
    -- | The decoder has consumed the available input and needs more
    -- to continue. Provide @'Just'@ if more input is available and
    -- @'Nothing'@ otherwise, and you will get a new @'DecodeStep'@.
    Partial (Maybe bytes -> m (DecodeStep bytes failure m a))

    -- | The decoder has successfully finished. Except for the output
    -- value you also get any unused input as well as the number of
    -- bytes consumed.
  | Done a !bytes

    -- | The decoder ran into an error. The decoder either used
    -- @'fail'@ or was not provided enough input. Contains any
    -- unconsumed input, the number of bytes consumed, and a
    -- @'DeserialiseFailure'@ exception describing the reason why the
    -- failure occurred.
  | Fail failure



serialiseCodec :: (MonadST m, Serialise.Serialise a)
               => Codec ByteString CBOR.DeserialiseFailure m a
serialiseCodec = cborCodec Serialise.encode Serialise.decode 

cborCodec :: MonadST m
          => (a -> CBOR.Encoding)
          -> (forall s. CBOR.Decoder s a)
          -> Codec ByteString CBOR.DeserialiseFailure m a
cborCodec cborEncode cborDecode =
    Codec {
      encode = convertCborEncoder  cborEncode,
      decode = convertCborDecoder' cborDecode,
      chunkNull = BS.null
    }

convertCborEncoder :: (a -> CBOR.Encoding) -> a -> [ByteString]
convertCborEncoder cborEncode =
    LBS.toChunks
  . toLazyByteString
  . CBOR.toBuilder
  . cborEncode

{-# NOINLINE toLazyByteString #-}
toLazyByteString :: BS.Builder -> LBS.ByteString
toLazyByteString = BS.toLazyByteStringWith strategy LBS.empty
  where
    strategy = BS.untrimmedStrategy 800 LBS.smallChunkSize

convertCborDecoder' :: MonadST m
                    => (forall s. CBOR.Decoder s a)
                    -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder' cborDecode =
    withLiftST (convertCborDecoder cborDecode)

convertCborDecoder :: forall s m a. Functor m
                   => CBOR.Decoder s a
                   -> (forall b. ST s b -> m b)
                   -> m (DecodeStep ByteString CBOR.DeserialiseFailure m a)
convertCborDecoder cborDecode liftST =
    go <$> liftST (CBOR.deserialiseIncremental cborDecode)
  where
    go (CBOR.Done  trailing _ x)          = Done x trailing
    go (CBOR.Fail _trailing _off failure) = Fail failure
    go (CBOR.Partial k)                   = Partial (fmap go . liftST . k)

