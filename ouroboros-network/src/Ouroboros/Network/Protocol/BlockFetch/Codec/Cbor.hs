{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
module Ouroboros.Network.Protocol.BlockFetch.Codec.Cbor where

import Prelude
import Control.Exception (Exception, throw)
import Control.Monad.ST

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
import Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import Protocol.Codec

import Ouroboros.Network.Protocol.BlockFetch.Type
import Ouroboros.Network.Protocol.Codec.Cbor (cborDecoder)


{-------------------------------------------------------------------------------
 @'BlockFetchClientProtocol'@ codec
-------------------------------------------------------------------------------}

-- | Codec errors of the @'BlockFetchClientProtocol'@
--
data BlockFetchClientCodecError
  = BFClientCodecErrUnexpectedTag
  | BFClientCodecErrNoTransitionFromDone
  | BFClientCodecErrCbor String
  | BFClientCodecErrInputLeft String
  deriving (Show, Eq)

instance Exception BlockFetchClientCodecError

blockFetchClientCodec
  :: forall s range .
     ( Serialise range )
  => Codec (ST s) BlockFetchClientCodecError Encoding ByteString (BlockRequestClientMessage range) StClientIdle
blockFetchClientCodec = blockFetchClientCodecIdle

blockFetchClientCodecIdle
  :: forall s range .
     ( Serialise range )
  => Codec (ST s) BlockFetchClientCodecError Encoding ByteString (BlockRequestClientMessage range) StClientIdle
blockFetchClientCodecIdle = Codec
  { encode = cborEncodeIdle
  , decode = cborDecoder BFClientCodecErrCbor cborDecodeIdle
  }
 where
  cborEncodeIdle
    :: Encoder (BlockRequestClientMessage range) StClientIdle (Encoded Encoding (Codec (ST s) BlockFetchClientCodecError Encoding ByteString (BlockRequestClientMessage range)))
  cborEncodeIdle = Encoder $ \tr -> case tr of
    MessageRequestRange range -> Encoded (encodeListLen 2 <> encodeWord 0 <> CBOR.encode range) blockFetchClientCodecIdle
    MessageDone -> Encoded (encodeListLen 1 <> encodeWord 1) blockFetchClientCodecDone

  cborDecodeIdle
    :: CBOR.Decoder s (Decoded (BlockRequestClientMessage range) StClientIdle (Codec (ST s) BlockFetchClientCodecError Encoding ByteString (BlockRequestClientMessage range)))
  cborDecodeIdle = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      0 -> do
        range <- CBOR.decode
        pure $ Decoded (MessageRequestRange range) blockFetchClientCodecIdle
      1 -> pure $ Decoded MessageDone blockFetchClientCodecDone
      _ -> throw BFClientCodecErrUnexpectedTag

blockFetchClientCodecDone
  :: forall s range .
     ( Serialise range )
  => Codec (ST s) BlockFetchClientCodecError Encoding ByteString (BlockRequestClientMessage range) StClientDone
blockFetchClientCodecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left $ BFClientCodecErrNoTransitionFromDone
  }

{-------------------------------------------------------------------------------
 @'BlockFetchServerProtocol'@ codec
-------------------------------------------------------------------------------}

-- | Codec errors of the @'BlockFetchServerProtocol'@
--
data BlockFetchServerCodecError
  = BFServerCodecErrCbor String
  | BFServerCodecErrUnknownTag ServerState Word
  | BFServerCodecErrNoTransitionFromDone
  | BFServerCodecErrInputLeft String
  deriving (Show, Eq)

instance Exception BlockFetchServerCodecError

blockFetchServerCodec
  :: forall s block .
     ( Serialise block )
  => Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block) StServerAwaiting
blockFetchServerCodec = blockFetchServerCodecAwaiting

blockFetchServerCodecAwaiting
  :: forall s block .
     ( Serialise block
     )
  => Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block) StServerAwaiting
blockFetchServerCodecAwaiting = Codec
  { encode = cborEncodeIdle
  , decode = cborDecoder BFServerCodecErrCbor cborDecodeIdle
  }
 where
  cborEncodeIdle
    :: Encoder (BlockRequestServerMessage block) StServerAwaiting (Encoded Encoding (Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block)))
  cborEncodeIdle = Encoder $ \tr -> case tr of
    MessageStartBatch -> Encoded (encodeListLen 1 <> encodeWord 0) blockFetchServerCodecSending
    MessageNoBlocks   -> Encoded (encodeListLen 1 <> encodeWord 1) blockFetchServerCodecAwaiting
    MessageServerDone -> Encoded (encodeListLen 1 <> encodeWord 2) blockFetchServerCodecDone

  cborDecodeIdle
    :: CBOR.Decoder s (Decoded (BlockRequestServerMessage block) StServerAwaiting (Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block)))
  cborDecodeIdle = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      0 -> pure $ Decoded MessageStartBatch blockFetchServerCodecSending
      1 -> pure $ Decoded MessageNoBlocks blockFetchServerCodecAwaiting
      2 -> pure $ Decoded MessageServerDone blockFetchServerCodecDone
      _ -> throw $ BFServerCodecErrUnknownTag StServerAwaiting key

blockFetchServerCodecSending
  :: forall s block .
     ( Serialise block )
  => Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block)
    StServerSending
blockFetchServerCodecSending = Codec
  { encode = cborEncodeSending
  , decode = cborDecoder BFServerCodecErrCbor cborDecodeSending
  }
 where
  cborEncodeSending
    :: Encoder (BlockRequestServerMessage block) StServerSending (Encoded Encoding (Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block)))
  cborEncodeSending = Encoder $ \tr -> case tr of
    MessageBlock block -> Encoded (encodeListLen 2 <> encodeWord 0 <> CBOR.encode block) blockFetchServerCodecSending
    MessageBatchDone   -> Encoded (encodeListLen 1 <> encodeWord 1) blockFetchServerCodecAwaiting
    MessageServerError -> Encoded (encodeListLen 1 <> encodeWord 2) blockFetchServerCodecAwaiting

  cborDecodeSending
    :: CBOR.Decoder s (Decoded (BlockRequestServerMessage block) StServerSending (Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block)))
  cborDecodeSending = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      0 -> do
        block <- CBOR.decode
        return $ Decoded (MessageBlock block) blockFetchServerCodecSending
      1 -> pure $ Decoded MessageBatchDone blockFetchServerCodecAwaiting
      2 -> pure $ Decoded MessageServerError blockFetchServerCodecAwaiting
      _ -> throw $ BFServerCodecErrUnknownTag StServerSending key

blockFetchServerCodecDone
  :: forall s block .
     ( Serialise block )
  => Codec (ST s) BlockFetchServerCodecError Encoding ByteString (BlockRequestServerMessage block) StServerDone
blockFetchServerCodecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Fold $ pure $ Complete [] $ pure $ Left BFServerCodecErrNoTransitionFromDone
  }
