{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.TxSubmission.Codec (
    codecTxSubmission
  ) where

import           Control.Monad.Class.MonadST
import qualified Data.List.NonEmpty as NonEmpty

import           Data.ByteString.Lazy (ByteString)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.Cbor

import           Ouroboros.Network.Protocol.TxSubmission.Type


codecTxSubmission
  :: forall txid tx m.
     MonadST m
  => (txid -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s tx)
  -> Codec (TxSubmission txid tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission encodeTxId decodeTxId
                  encodeTx   decodeTx =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (TxSubmission txid tx) st st'
           -> CBOR.Encoding
    encode (ServerAgency TokIdle) (MsgRequestTxIds blocking ackNo reqNo) =
        CBOR.encodeListLen 4
     <> CBOR.encodeWord 0
     <> CBOR.encodeBool (case blocking of
                           TokBlocking    -> True
                           TokNonBlocking -> False)
     <> CBOR.encodeWord16 ackNo
     <> CBOR.encodeWord16 reqNo

    encode (ClientAgency (TokTxIds _)) (MsgReplyTxIds txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 1
     <> CBOR.encodeListLenIndef
     <> foldr (\(txid, sz) r -> CBOR.encodeListLen 2
                             <> encodeTxId txid
                             <> CBOR.encodeWord32 sz
                             <> r)
              CBOR.encodeBreak
              txids'
      where
        txids' :: [(txid, TxSizeInBytes)]
        txids' = case txids of
                   BlockingReply    xs -> NonEmpty.toList xs
                   NonBlockingReply xs -> xs

    encode (ServerAgency TokIdle) (MsgRequestTxs txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTxId txid <> r) CBOR.encodeBreak txids

    encode (ClientAgency TokTxs)  (MsgReplyTxs txs) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTx txid <> r) CBOR.encodeBreak txs

    encode (ClientAgency (TokTxIds TokBlocking)) MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 4


    decode :: forall (pr :: PeerRole) s (st :: TxSubmission txid tx).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of

        (ServerAgency TokIdle,       4, 0) -> do
          blocking <- CBOR.decodeBool
          ackNo    <- CBOR.decodeWord16
          reqNo    <- CBOR.decodeWord16
          return $! case blocking of
            True  -> SomeMessage (MsgRequestTxIds TokBlocking    ackNo reqNo)
            False -> SomeMessage (MsgRequestTxIds TokNonBlocking ackNo reqNo)

        (ClientAgency (TokTxIds b),  2, 1) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef
                     (flip (:)) [] reverse
                     (do CBOR.decodeListLenOf 2
                         txid <- decodeTxId
                         sz   <- CBOR.decodeWord32
                         return (txid, sz))
          case (b, txids) of
            (TokBlocking, t:ts) ->
              return $
                SomeMessage (MsgReplyTxIds (BlockingReply (t NonEmpty.:| ts)))

            (TokNonBlocking, ts) ->
              return $
                SomeMessage (MsgReplyTxIds (NonBlockingReply ts))

            (TokBlocking, []) ->
              fail "codecTxSubmission: MsgReplyTxIds: empty list not permitted"


        (ServerAgency TokIdle,       2, 2) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTxId
          return (SomeMessage (MsgRequestTxs txids))

        (ClientAgency TokTxs,     2, 3) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTx
          return (SomeMessage (MsgReplyTxs txids))

        (ClientAgency (TokTxIds TokBlocking), 1, 4) ->
          return (SomeMessage MsgDone)

        (ServerAgency TokIdle,    _, _) ->
          fail "codecTxSubmission.Idle: unexpected key"
        (ClientAgency TokTxIds{}, _, _) ->
          fail "codecTxSubmission.TxIds: unexpected key"
        (ClientAgency TokTxs,     _, _) ->
          fail "codecTxSubmission.Tx: unexpected message key"

