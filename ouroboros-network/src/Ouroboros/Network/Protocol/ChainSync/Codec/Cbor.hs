{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.ChainSync.Codec.Cbor where

import Prelude hiding (fail)
import Control.Monad.Fail (fail)
import Control.Monad.ST

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Codec.CBOR.Encoding (Encoding, encodeListLen, encodeWord)
import Codec.CBOR.Decoding (decodeListLen, decodeWord)
import qualified Codec.CBOR.Decoding as CBOR (Decoder)
import Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import Protocol.Codec (Codec (..), Encoder (..), Decoder (..), DecoderStep (..), Encoded (..), Decoded (..))

import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.Codec.Cbor (cborDecoder)

codecChainSync
  :: forall s header point .
     ( Serialise header, Serialise point )
  => Codec (ST s) Encoding ByteString (ChainSyncMessage header point) 'StIdle
codecChainSync = codecIdle

codecIdle
  :: forall s header point .
     ( Serialise header, Serialise point )
  => Codec (ST s) Encoding ByteString (ChainSyncMessage header point) 'StIdle
codecIdle = Codec
  { encode = cborEncodeIdle
  , decode = cborDecoder cborDecodeIdle
  }
  where

  cborEncodeIdle
    :: Encoder (ChainSyncMessage header point) 'StIdle (Encoded Encoding (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborEncodeIdle = Encoder $ \tr -> case tr of
    MsgRequestNext -> Encoded (encodeListLen 1 <> encodeWord 0) codecNext_CanAwait
    MsgFindIntersect pts -> Encoded (encodeListLen 2 <> encodeWord 4 <> CBOR.encode pts) codecIntersect
    MsgDone -> Encoded (encodeListLen 1 <> encodeWord 7) codecDone

  cborDecodeIdle
    :: CBOR.Decoder s (Decoded (ChainSyncMessage header point) 'StIdle (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborDecodeIdle = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      0 -> pure $ Decoded MsgRequestNext codecNext_CanAwait
      4 -> do
        pts <- CBOR.decode
        pure $ Decoded (MsgFindIntersect pts) codecIntersect
      7 -> pure $ Decoded MsgDone codecDone
      _ -> fail "ChainSyncMessage.idle: unexpected key"

-- | We need to specify 'StCanAwait and do this independently of
-- 'codecNext_MustReply'. If we tried to leave it free, the decoder
-- would not work for 'MsgAwaitReply', since it picks 'StCanAwait.
--
-- Sadly, it means the encoding of the other 2 next message (roll forwards or
-- backwards) is duplicated. With some refactoring the duplication could be
-- minimized. NB: their encodings do not need to agree. They could be
-- completely different, and the protocol would still work over it, as long
-- as they are locally consistent.
codecNext_CanAwait
  :: forall s header point .
     ( Serialise header, Serialise point )
  => Codec (ST s) Encoding ByteString (ChainSyncMessage header point) ('StNext 'StCanAwait)
codecNext_CanAwait = Codec
  { encode = cborEncodeNext
  , decode = cborDecoder cborDecodeNext
  }
  where

  cborEncodeNext
    :: Encoder (ChainSyncMessage header point) ('StNext 'StCanAwait) (Encoded Encoding (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborEncodeNext = Encoder $ \tr -> case tr of
    MsgAwaitReply -> Encoded (encodeListLen 1 <> encodeWord 1) codecNext_MustReply
    MsgRollForward h p -> Encoded (encodeListLen 3 <> encodeWord 2 <> CBOR.encode h <> CBOR.encode p) codecIdle
    MsgRollBackward p p' -> Encoded (encodeListLen 3 <> encodeWord 3 <> CBOR.encode p <> CBOR.encode p') codecIdle

  cborDecodeNext
    :: CBOR.Decoder s (Decoded (ChainSyncMessage header point) ('StNext 'StCanAwait) (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborDecodeNext = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      1 -> pure $ Decoded MsgAwaitReply codecNext_MustReply
      2 -> do
        h <- CBOR.decode
        p <- CBOR.decode
        pure $ Decoded (MsgRollForward h p) codecIdle
      3 -> do
        p <- CBOR.decode
        p' <- CBOR.decode
        pure $ Decoded (MsgRollBackward p p') codecIdle
      _ -> fail "ChainSyncMessage.next: unexpected key"

codecNext_MustReply
  :: forall s header point .
     ( Serialise header, Serialise point )
  => Codec (ST s) Encoding ByteString (ChainSyncMessage header point) ('StNext 'StMustReply)
codecNext_MustReply = Codec
  { encode = cborEncodeNext
  , decode = cborDecoder cborDecodeNext
  }
  where

  cborEncodeNext
    :: Encoder (ChainSyncMessage header point) ('StNext 'StMustReply) (Encoded Encoding (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborEncodeNext = Encoder $ \tr -> case tr of
    MsgRollForward h p -> Encoded (encodeListLen 3 <> encodeWord 2 <> CBOR.encode h <> CBOR.encode p) codecIdle
    MsgRollBackward p p' -> Encoded (encodeListLen 3 <> encodeWord 3 <> CBOR.encode p <> CBOR.encode p') codecIdle

  cborDecodeNext
    :: CBOR.Decoder s (Decoded (ChainSyncMessage header point) ('StNext 'StMustReply) (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborDecodeNext = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      2 -> do
        h <- CBOR.decode
        p <- CBOR.decode
        pure $ Decoded (MsgRollForward h p) codecIdle
      3 -> do
        p <- CBOR.decode
        p' <- CBOR.decode
        pure $ Decoded (MsgRollBackward p p') codecIdle
      _ -> fail "ChainSyncMessage.next: unexpected key"

codecIntersect
  :: forall s header point .
     ( Serialise header, Serialise point )
  => Codec (ST s) Encoding ByteString (ChainSyncMessage header point) 'StIntersect
codecIntersect = Codec
  { encode = cborEncodeIntersect
  , decode = cborDecoder cborDecodeIntersect
  }
  where

  cborEncodeIntersect
    :: Encoder (ChainSyncMessage header point) 'StIntersect (Encoded Encoding (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborEncodeIntersect = Encoder $ \tr -> case tr of
    MsgIntersectImproved p p' -> Encoded (encodeListLen 3 <> encodeWord 5 <> CBOR.encode p <> CBOR.encode p') codecIdle
    MsgIntersectUnchanged p -> Encoded (encodeListLen 2 <> encodeWord 6 <> CBOR.encode p) codecIdle

  cborDecodeIntersect
    :: CBOR.Decoder s (Decoded (ChainSyncMessage header point) 'StIntersect (Codec (ST s) Encoding ByteString (ChainSyncMessage header point)))
  cborDecodeIntersect = do
    _ <- decodeListLen
    key <- decodeWord
    case key of
      5 -> do
        p <- CBOR.decode
        p' <- CBOR.decode
        pure $ Decoded (MsgIntersectImproved p p') codecIdle
      6 -> do
        p <- CBOR.decode
        pure $ Decoded (MsgIntersectUnchanged p) codecIdle
      _ -> fail "ChainSyncMessage.intersect: unexpected key"


codecDone :: Codec (ST s) Encoding ByteString (ChainSyncMessage header point) 'StDone
codecDone = Codec
  { encode = Encoder $ \tr -> case tr of { }
  , decode = Decoder $ pure $ Fail mempty (T.pack "no transitions from done")
  }
