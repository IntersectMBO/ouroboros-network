{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.BlockFetch.Codec where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen, decodeWord)
import           Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import           Network.TypedProtocol.Codec
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.BlockFetch.Type

codecBlockFetch
  :: forall header body m.
     ( Monad m
     , MonadST m
     , Serialise header
     , Serialise body
     , Serialise (HeaderHash header)
     )
  => Codec (BlockFetch header body) CBOR.DeserialiseFailure m ByteString
codecBlockFetch = mkCodecCborLazyBS encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (BlockFetch header body) st st'
         -> CBOR.Encoding
  encode (ClientAgency TokIdle) (MsgRequestRange (ChainRange from to)) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encode from <> CBOR.encode to
  encode (ClientAgency TokIdle) MsgClientDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 1
  encode (ServerAgency TokBusy) MsgStartBatch =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2
  encode (ServerAgency TokBusy) MsgNoBlocks =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 3
  encode (ServerAgency TokStreaming) (MsgBlock body) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 4 <> CBOR.encode body
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
        from <- CBOR.decode
        to   <- CBOR.decode
        return $ SomeMessage $ MsgRequestRange (ChainRange from to)
      (ClientAgency TokIdle, 1) -> return $ SomeMessage MsgClientDone
      (ServerAgency TokBusy, 2) -> return $ SomeMessage MsgStartBatch
      (ServerAgency TokBusy, 3) -> return $ SomeMessage MsgNoBlocks
      (ServerAgency TokStreaming, 4) -> SomeMessage . MsgBlock <$> CBOR.decode
      (ServerAgency TokStreaming, 5) -> return $ SomeMessage MsgBatchDone

      -- TODO proper exceptions
      (ClientAgency TokIdle, _)      -> fail "codecBlockFetch.Idle: unexpected key"
      (ServerAgency TokBusy, _)      -> fail "codecBlockFetch.Busy: unexpected key"
      (ServerAgency TokStreaming, _) -> fail "codecBlockFetch.Streaming: unexpected key"
