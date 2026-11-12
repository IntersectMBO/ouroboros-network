{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Network.Protocol.Codec.Utils
  ( WithBytes (..)
  , encodeBytes
  , encodeWithBytes
  , WithByteSpan (..)
  , decodeWithByteSpan
  , bytesBetweenOffsets
  , runWithByteSpan
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import GHC.Generics (Generic)
import NoThunks.Class
import Ouroboros.Network.Util.ShowProxy


data WithBytes a = WithBytes {
      cborBytes   :: ByteString,
      -- ^ cbor encoding
      cborPayload :: a
      -- ^ decoded structure
    }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (ShowProxy, NoThunks)

encodeBytes :: ByteString -> CBOR.Encoding
encodeBytes =
    -- this should be equivalent to
    -- `CBOR.encodePreEncoded . BSL.toStrict . cborBytes`
    -- but it doesn't copy the bytes
    foldMap CBOR.encodePreEncoded . BSL.toChunks

encodeWithBytes :: WithBytes a -> CBOR.Encoding
encodeWithBytes = encodeBytes . cborBytes

-- | A decoded value and its half-open byte offsets in the decoder's input.
-- Offsets are signed 'CBOR.ByteOffset' values; validate externally supplied
-- offsets before narrowing them. Fresh staged decoders have a fresh origin, so
-- rebase their spans before using them against the enclosing message's bytes.
--
newtype WithByteSpan a = WithByteSpan (a, CBOR.ByteOffset, CBOR.ByteOffset)
  deriving stock (Eq, Show)

decodeWithByteSpan :: CBOR.Decoder s a -> CBOR.Decoder s (WithByteSpan a)
decodeWithByteSpan = fmap WithByteSpan . CBOR.decodeWithByteSpan

bytesBetweenOffsets :: CBOR.ByteOffset -> CBOR.ByteOffset -> ByteString -> ByteString
bytesBetweenOffsets start end bytes = BSL.take (end - start) $ BSL.drop start bytes

-- | Retain the element's exact bytes while applying its annotator to the full
-- input: nested annotation offsets can be relative to that full buffer.
runWithByteSpan :: ByteString -> WithByteSpan (ByteString -> a) -> WithBytes a
runWithByteSpan bytes (WithByteSpan (a, start, end)) = WithBytes {
    cborBytes   = bytesBetweenOffsets start end bytes,
    cborPayload = a bytes
  }
