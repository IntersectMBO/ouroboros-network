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
  , codecBlockFetchId
  ) where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen, decodeWord)

import           Network.TypedProtocol.Codec
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Block (HeaderHash, Point)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.Protocol.BlockFetch.Type

codecBlockFetch
  :: forall header body m.
     ( Monad m
     , MonadST m
     )
  => (body              -> CBOR.Encoding)
  -> (HeaderHash header -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s body)
  -> (forall s. CBOR.Decoder s (HeaderHash header))
  -> Codec (BlockFetch header body) CBOR.DeserialiseFailure m ByteString
codecBlockFetch encodeBody encodeHeaderHash
                decodeBody decodeHeaderHash =
    mkCodecCborLazyBS encode decode
 where
  encodePoint' :: Point header -> CBOR.Encoding
  encodePoint' = Block.encodePoint $ Block.encodeChainHash encodeHeaderHash

  decodePoint' :: forall s. CBOR.Decoder s (Point header)
  decodePoint' = Block.decodePoint $ Block.decodeChainHash decodeHeaderHash

  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (BlockFetch header body) st st'
         -> CBOR.Encoding
  encode (ClientAgency TokIdle) (MsgRequestRange (ChainRange from to)) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> encodePoint' from <> encodePoint' to
  encode (ClientAgency TokIdle) MsgClientDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 1
  encode (ServerAgency TokBusy) MsgStartBatch =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2
  encode (ServerAgency TokBusy) MsgNoBlocks =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 3
  encode (ServerAgency TokStreaming) (MsgBlock body) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 4 <> encodeBody body
  encode (ServerAgency TokStreaming) MsgBatchDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 5

  decode :: forall (pr :: PeerRole) s (st :: BlockFetch header body).
            PeerHasAgency pr st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) -> do
        from <- decodePoint'
        to   <- decodePoint'
        return $ SomeMessage $ MsgRequestRange (ChainRange from to)
      (ClientAgency TokIdle, 1) -> return $ SomeMessage MsgClientDone
      (ServerAgency TokBusy, 2) -> return $ SomeMessage MsgStartBatch
      (ServerAgency TokBusy, 3) -> return $ SomeMessage MsgNoBlocks
      (ServerAgency TokStreaming, 4) -> SomeMessage . MsgBlock <$> decodeBody
      (ServerAgency TokStreaming, 5) -> return $ SomeMessage MsgBatchDone

      -- TODO proper exceptions
      (ClientAgency TokIdle, _)      -> fail "codecBlockFetch.Idle: unexpected key"
      (ServerAgency TokBusy, _)      -> fail "codecBlockFetch.Busy: unexpected key"
      (ServerAgency TokStreaming, _) -> fail "codecBlockFetch.Streaming: unexpected key"


codecBlockFetchId
  :: forall header body m. Monad m
  => Codec (BlockFetch header body) CodecFailure m (AnyMessage (BlockFetch header body))
codecBlockFetchId = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (BlockFetch header body) st st'
         -> AnyMessage (BlockFetch header body)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) st.
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (BlockFetch header body))
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
