{-# LANGUAGE OverloadedStrings #-}

module Ouroboros.Byron.Proxy.ChainSync.Types
  ( Block
  , Point (..)
  , codec
  , encodeBlock
  , decodeBlock
  , encodePoint
  , decodePoint
  ) where

import Control.Monad.Class.MonadST (MonadST)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Codec.Serialise (Serialise (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString, fromStrict)
import Network.TypedProtocol.Codec (Codec)

import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Cardano
import Cardano.Chain.Slotting (EpochSlots)

import Ouroboros.Network.Protocol.ChainSync.Codec (codecChainSync)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Block (SlotNo (..))

type Block = Binary.Annotated (Cardano.ABlockOrBoundary ByteString) ByteString

data Point = Point
  { pointSlot :: !SlotNo
  , pointHash :: !Cardano.HeaderHash
  }
  deriving (Show, Eq)

-- | A `Block` has a `ByteString` annotation which is assumed to be its
-- CBOR encoding, so we drop that into a "CBOR-in-CBOR" (tag 24).
encodeBlock :: Block -> CBOR.Encoding
encodeBlock = Binary.encodeUnknownCborDataItem . Lazy.fromStrict . Binary.annotation

-- | FIXME length limit must be imposed. Can it be done here? I don't see any
-- types in cborg that would make it possible.
decodeBlock :: EpochSlots -> CBOR.Decoder s Block
decodeBlock epochSlots = do
  bytes <- Binary.decodeUnknownCborDataItem
  case Binary.decodeFullAnnotatedBytes "Block or boundary" internalDecoder (Lazy.fromStrict bytes) of
    Right it  -> pure $ Binary.Annotated it bytes
    -- FIXME
    --   err :: Binary.DecoderError
    -- but AFAICT the only way to make the decoder fail is to give a `String`
    -- to `fail`...
    Left  err -> fail (show err)
  where
  internalDecoder :: Binary.Decoder s (Cardano.ABlockOrBoundary Binary.ByteSpan)
  internalDecoder = Cardano.fromCBORABlockOrBoundary epochSlots

encodePoint :: Point -> CBOR.Encoding
encodePoint point =
     CBOR.encodeListLen 2
  -- FIXME `SlotNo` encoding uses the `Serialise` class, but `ToCBOR` class
  -- is used for `HeaderHash`.
  <> encode (pointSlot point)
  <> Binary.toCBOR (pointHash point)

decodePoint :: CBOR.Decoder s Point
decodePoint = do
  n <- CBOR.decodeListLen
  case n of
    -- FIXME `SlotNo` decoding uses the `Serialise` class, but `ToCBOR` class
    -- is used for `HeaderHash`.
    2 -> Point <$> decode <*> Binary.fromCBOR
    _ -> fail "Point: invalid list length"

codec
  :: (MonadST m)
  => EpochSlots
  -> Codec (ChainSync Block Point) CBOR.DeserialiseFailure m Lazy.ByteString
codec epochSlots = codecChainSync
  encodeBlock
  (decodeBlock epochSlots)
  encodePoint
  decodePoint
