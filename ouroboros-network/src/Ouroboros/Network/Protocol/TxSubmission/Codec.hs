{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.TxSubmission.Codec where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen, decodeWord)
import           Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import           Network.TypedProtocol.Codec
import           Ouroboros.Network.Codec

import           Ouroboros.Network.Protocol.TxSubmission.Type

codecTxSubmission
  :: forall hash tx m.
     ( Monad m
     , MonadST m
     , Serialise hash
     , Serialise tx
     )
  => Codec (TxSubmission hash tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission = mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (TxSubmission hash tx) st st'
           -> CBOR.Encoding
    encode (ServerAgency TokIdle) (MsgGetHashes n) =
      CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encode n
    encode (ClientAgency TokSendHashes) (MsgSendHashes hs) =
      CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encode hs
    encode (ServerAgency TokIdle) (MsgGetTx hash) =
      CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> CBOR.encode hash
    encode (ClientAgency TokSendTx) (MsgTx tx) =
      CBOR.encodeListLen 2 <> CBOR.encodeWord 3 <> CBOR.encode tx
    encode (ServerAgency TokIdle) MsgDone =
      CBOR.encodeListLen 1 <> CBOR.encodeWord 4

    decode :: forall (pr :: PeerRole) s (st :: TxSubmission hash tx).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ServerAgency TokIdle,       2, 0) -> SomeMessage . MsgGetHashes  <$> CBOR.decode
        (ClientAgency TokSendHashes, 2, 1) -> SomeMessage . MsgSendHashes <$> CBOR.decode
        (ServerAgency TokIdle,       2, 2) -> SomeMessage . MsgGetTx      <$> CBOR.decode
        (ClientAgency TokSendTx,     2, 3) -> SomeMessage . MsgTx         <$> CBOR.decode
        (ServerAgency TokIdle,       1, 4) -> return (SomeMessage MsgDone)

        (ServerAgency TokIdle,       _, _) -> fail "codecTxSubmission.Idle: unexpected key"
        (ClientAgency TokSendHashes, _, _) -> fail "codecTxSubmission.SendHashes: unexpected key"
        (ClientAgency TokSendTx,     _, _) -> fail "codecTxSubmission.SendTx: unexpected key"
