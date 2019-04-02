module Ouroboros.Network.Server.Version.CBOR
  ( Number
  , Blob
  , codec
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Word (Word32)

import Control.Monad.Class.MonadST (MonadST)
import Network.TypedProtocol.Codec (Codec)

import qualified Ouroboros.Network.Server.Version.Protocol as Version

type Number = Word32
type Blob = ByteString

-- A common version encoding scheme where the version number is `Word32` and
-- the version data for every version is always encoded as a length-prefixed
-- `ByteString`.

codec :: ( MonadST m ) => Codec (Version.StVersion Number Blob) CBOR.DeserialiseFailure m Lazy.ByteString
codec = Version.cborCodec numEncoder numDecoder blobEncoder blobDecoder
  where
  numEncoder :: Number -> CBOR.Encoding
  numEncoder = CBOR.encodeWord32
  numDecoder :: CBOR.Decoder s Number
  numDecoder = CBOR.decodeWord32
  blobEncoder :: Blob -> CBOR.Encoding
  blobEncoder = CBOR.encodeBytes
  blobDecoder :: CBOR.Decoder s Blob
  blobDecoder = CBOR.decodeBytes
