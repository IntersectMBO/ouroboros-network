{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.BlockFetch.Codec
  ( codecBlockFetch
  , codecBlockFetchSerialised
  , codecBlockFetchId
  ) where

import           Control.Monad (when)
import           Control.Monad.Class.MonadST

import qualified Data.ByteString.Lazy as LBS

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Write    as CBOR

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Cbor

import           Ouroboros.Network.Block (HeaderHash, Point, Serialised (..))
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.BlockFetch.Type

-- | 'codecBlockFetch' but without the CBOR-in-CBOR trick
codecBlockFetchUnwrapped
  :: forall block m.
     MonadST m
  => (block            -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s block)
  -> (HeaderHash block -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (HeaderHash block))
  -> Codec (BlockFetch block) CBOR.DeserialiseFailure m LBS.ByteString
codecBlockFetchUnwrapped encodeBlock     decodeBlock
                         encodeBlockHash decodeBlockHash =
    mkCodecCborLazyBS encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (BlockFetch block) st st'
         -> CBOR.Encoding
  encode (ClientAgency TokIdle) (MsgRequestRange (ChainRange from to)) =
    CBOR.encodeListLen 3 <> CBOR.encodeWord 0 <> encodePoint from
                                              <> encodePoint to
  encode (ClientAgency TokIdle) MsgClientDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 1
  encode (ServerAgency TokBusy) MsgStartBatch =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2
  encode (ServerAgency TokBusy) MsgNoBlocks =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 3
  encode (ServerAgency TokStreaming) (MsgBlock block) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 4 <> encodeBlock block
  encode (ServerAgency TokStreaming) MsgBatchDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 5

  decode :: forall (pr :: PeerRole) s (st :: BlockFetch block).
            PeerHasAgency pr st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) -> do
        from <- decodePoint
        to   <- decodePoint
        return $ SomeMessage $ MsgRequestRange (ChainRange from to)
      (ClientAgency TokIdle, 1) -> return $ SomeMessage MsgClientDone
      (ServerAgency TokBusy, 2) -> return $ SomeMessage MsgStartBatch
      (ServerAgency TokBusy, 3) -> return $ SomeMessage MsgNoBlocks
      (ServerAgency TokStreaming, 4) -> SomeMessage . MsgBlock
                                          <$> decodeBlock
      (ServerAgency TokStreaming, 5) -> return $ SomeMessage MsgBatchDone

      -- TODO proper exceptions
      (ClientAgency TokIdle, _)      -> fail "codecBlockFetch.Idle: unexpected key"
      (ServerAgency TokBusy, _)      -> fail "codecBlockFetch.Busy: unexpected key"
      (ServerAgency TokStreaming, _) -> fail "codecBlockFetch.Streaming: unexpected key"


  encodePoint :: Point block -> CBOR.Encoding
  encodePoint = Block.encodePoint encodeBlockHash

  decodePoint :: forall s. CBOR.Decoder s (Point block)
  decodePoint = Block.decodePoint decodeBlockHash

codecBlockFetch
  :: forall block m.
     MonadST m
  => (block            -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (LBS.ByteString -> block))
  -> (HeaderHash block -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (HeaderHash block))
  -> Codec (BlockFetch block) CBOR.DeserialiseFailure m LBS.ByteString
codecBlockFetch encodeBlock decodeBlock =
    codecBlockFetchUnwrapped encodeBlockWrapped decodeBlockWrapped
 where
  encodeBlockWrapped :: block -> CBOR.Encoding
  encodeBlockWrapped block =
    --TODO: replace with encodeEmbeddedCBOR from cborg-0.2.4 once
    -- it is available, since that will be faster.
      CBOR.encodeTag 24
   <> CBOR.encodeBytes (CBOR.toStrictByteString (encodeBlock block))

  decodeBlockWrapped :: forall s. CBOR.Decoder s block
  decodeBlockWrapped = do
    --TODO: replace this with decodeEmbeddedCBOR from cborg-0.2.4 once
    -- it is available, since that will be faster.
    tag <- CBOR.decodeTag
    when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
    payload <- LBS.fromStrict <$> CBOR.decodeBytes
    case CBOR.deserialiseFromBytes decodeBlock payload of
      Left (CBOR.DeserialiseFailure _ reason) -> fail reason
      Right (trailing, block)
        | not (LBS.null trailing) -> fail "trailing bytes in CBOR-in-CBOR"
        | otherwise               -> return (block payload)

codecBlockFetchSerialised
  :: forall block m.
     MonadST m
  => (HeaderHash block -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (HeaderHash block))
  -> Codec (BlockFetch (Serialised block)) CBOR.DeserialiseFailure m LBS.ByteString
codecBlockFetchSerialised =
  codecBlockFetchUnwrapped encodeBlockWrapped decodeBlock
 where
  -- Avoid converting to a strict ByteString, as that requires copying O(n)
  -- in case the lazy ByteString consists of more than one chunks.
  encodeBlockWrapped :: Serialised block -> CBOR.Encoding
  encodeBlockWrapped (Serialised bytes) =
    --TODO: replace with encodeEmbeddedCBOR from cborg-0.2.4 once
    -- it is available, since that will be faster.
      CBOR.encodeTag 24
      -- TODO can this be improved?
   <> CBOR.encodeBytes (LBS.toStrict bytes)

  decodeBlock :: forall s. CBOR.Decoder s (Serialised block)
  decodeBlock = do
    --TODO: replace this with decodeEmbeddedCBOR from cborg-0.2.4 once
    -- it is available, since that will be faster.
    tag <- CBOR.decodeTag
    when (tag /= 24) $ fail "expected tag 24 (CBOR-in-CBOR)"
    Serialised . LBS.fromStrict <$> CBOR.decodeBytes

codecBlockFetchId
  :: forall block m. Monad m
  => Codec (BlockFetch block) CodecFailure m (AnyMessage (BlockFetch block))
codecBlockFetchId = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (BlockFetch block) st st'
         -> AnyMessage (BlockFetch block)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) st.
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (BlockFetch block))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
    (_, Nothing) -> return $ DecodeFail CodecFailureOutOfInput
    (ClientAgency TokIdle,      Just (AnyMessage msg@(MsgRequestRange {}))) -> return (DecodeDone (SomeMessage msg) Nothing)
    (ClientAgency TokIdle,      Just (AnyMessage msg@(MsgClientDone {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
    (ServerAgency TokBusy,      Just (AnyMessage msg@(MsgStartBatch {})))   -> return (DecodeDone (SomeMessage msg) Nothing)
    (ServerAgency TokBusy,      Just (AnyMessage msg@(MsgNoBlocks {})))     -> return (DecodeDone (SomeMessage msg) Nothing)
    (ServerAgency TokStreaming, Just (AnyMessage msg@(MsgBlock {})))        -> return (DecodeDone (SomeMessage msg) Nothing)
    (ServerAgency TokStreaming, Just (AnyMessage msg@(MsgBatchDone {})))    -> return (DecodeDone (SomeMessage msg) Nothing)
    (_, _) -> return $ DecodeFail (CodecFailure "codecBlockFetchId: no matching message")
