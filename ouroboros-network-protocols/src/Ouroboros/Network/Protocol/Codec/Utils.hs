module Ouroboros.Network.Protocol.Codec.Utils
  ( WithBytes (..)
  , encodeBytes
  , encodeWithBytes
  , WithByteSpan (..)
  , decodeWithByteSpan
  , bytesBetweenOffsets
  ) where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL


data WithBytes a = WithBytes {
      cborBytes   :: ByteString,
      -- ^ cbor encoding
      cborPayload :: a
      -- ^ decoded structure
    }
  deriving (Show, Eq)

encodeBytes :: ByteString -> CBOR.Encoding
encodeBytes =
    -- this should be equivalent to
    -- `CBOR.encodePreEncoded . BSL.toStrict . cborBytes`
    -- but it doesn't copy the bytes
    foldMap CBOR.encodePreEncoded . BSL.toChunks

encodeWithBytes :: WithBytes a -> CBOR.Encoding
encodeWithBytes = encodeBytes . cborBytes

-- | A bytespan functor.
--
newtype WithByteSpan a = WithByteSpan (a, CBOR.ByteOffset, CBOR.ByteOffset)

decodeWithByteSpan :: CBOR.Decoder s a -> CBOR.Decoder s (WithByteSpan a)
decodeWithByteSpan = fmap WithByteSpan . CBOR.decodeWithByteSpan

bytesBetweenOffsets :: CBOR.ByteOffset -> CBOR.ByteOffset -> ByteString -> ByteString
bytesBetweenOffsets start end bytes = BSL.take (end - start) $ BSL.drop start bytes
