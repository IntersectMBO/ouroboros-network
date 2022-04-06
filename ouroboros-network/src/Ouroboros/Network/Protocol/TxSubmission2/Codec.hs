{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.TxSubmission2.Codec
  ( codecTxSubmission2
  , codecTxSubmission2Id
  , encodeTxSubmission2
  , decodeTxSubmission2
  , byteLimitsTxSubmission2
  , timeLimitsTxSubmission2
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime
import qualified Data.List.NonEmpty as NonEmpty

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Protocol.TxSubmission2.Type

-- | Byte Limits.
byteLimitsTxSubmission2 :: forall bytes txid tx.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (TxSubmission2 txid tx) bytes
byteLimitsTxSubmission2 = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TxSubmission2 txid tx).
                    PeerHasAgency pr st -> Word
    stateToLimit (ClientAgency TokInit)                   = smallByteLimit
    stateToLimit (ClientAgency (TokTxIds TokBlocking))    = largeByteLimit
    stateToLimit (ClientAgency (TokTxIds TokNonBlocking)) = largeByteLimit
    stateToLimit (ClientAgency TokTxs)                    = largeByteLimit
    stateToLimit (ServerAgency TokIdle)                   = smallByteLimit


-- | Time Limits.
--
-- `TokTxIds TokBlocking` No timeout
-- `TokTxIds TokNonBlocking` `shortWait` timeout
-- `TokTxs` `shortWait` timeout
-- `TokIdle` `shortWait` timeout
timeLimitsTxSubmission2 :: forall txid tx. ProtocolTimeLimits (TxSubmission2 txid tx)
timeLimitsTxSubmission2 = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: TxSubmission2 txid tx).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokInit)                   = waitForever
    stateToLimit (ClientAgency (TokTxIds TokBlocking))    = waitForever
    stateToLimit (ClientAgency (TokTxIds TokNonBlocking)) = shortWait
    stateToLimit (ClientAgency TokTxs)                    = shortWait
    stateToLimit (ServerAgency TokIdle)                   = waitForever


codecTxSubmission2
  :: forall txid tx m.
     MonadST m
  => (txid -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s tx)
  -> Codec (TxSubmission2 txid tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission2 encodeTxId decodeTxId
                   encodeTx   decodeTx =
    mkCodecCborLazyBS
      (encodeTxSubmission2 encodeTxId encodeTx)
      decode
  where
    decode :: forall (pr :: PeerRole) (st :: TxSubmission2 txid tx).
              PeerHasAgency pr st
           -> forall s. CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      decodeTxSubmission2 decodeTxId decodeTx stok len key

encodeTxSubmission2
    :: forall txid tx.
       (txid -> CBOR.Encoding)
    -> (tx -> CBOR.Encoding)
    -> (forall (pr :: PeerRole) (st :: TxSubmission2 txid tx) (st' :: TxSubmission2 txid tx).
               PeerHasAgency pr st
            -> Message (TxSubmission2 txid tx) st st'
            -> CBOR.Encoding)
encodeTxSubmission2 encodeTxId encodeTx = encode
  where
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (TxSubmission2 txid tx) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokInit)  MsgInit =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 6
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


decodeTxSubmission2
    :: forall txid tx.
       (forall s . CBOR.Decoder s txid)
    -> (forall s . CBOR.Decoder s tx)
    -> (forall (pr :: PeerRole) (st :: TxSubmission2 txid tx) s.
               PeerHasAgency pr st
            -> Int
            -> Word
            -> CBOR.Decoder s (SomeMessage st))
decodeTxSubmission2 decodeTxId decodeTx = decode
  where
    decode :: forall (pr :: PeerRole) s (st :: TxSubmission2 txid tx).
              PeerHasAgency pr st
           -> Int
           -> Word
           -> CBOR.Decoder s (SomeMessage st)
    decode stok len key = do
      case (stok, len, key) of
        (ClientAgency TokInit,       1, 6) ->
          return (SomeMessage MsgInit)
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

        --
        -- failures per protocol state
        --

        (ClientAgency TokInit, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (ClientAgency (TokTxIds TokBlocking), _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (ClientAgency (TokTxIds TokNonBlocking), _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (ClientAgency TokTxs, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (ServerAgency TokIdle, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)

codecTxSubmission2Id
  :: forall txid tx m. Monad m
  => Codec (TxSubmission2 txid tx) CodecFailure m (AnyMessage (TxSubmission2 txid tx))
codecTxSubmission2Id = Codec encode decode
 where
  encode :: forall (pr :: PeerRole) st st'.
            PeerHasAgency pr st
         -> Message (TxSubmission2 txid tx) st st'
         -> AnyMessage (TxSubmission2 txid tx)
  encode _ = AnyMessage

  decode :: forall (pr :: PeerRole) (st :: TxSubmission2 txid tx).
            PeerHasAgency pr st
         -> m (DecodeStep (AnyMessage (TxSubmission2 txid tx))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> return $ case (stok, bytes) of
    (ClientAgency TokInit,      Just (AnyMessage msg@MsgInit))              -> DecodeDone (SomeMessage msg) Nothing
    (ServerAgency TokIdle,      Just (AnyMessage msg@(MsgRequestTxIds {}))) -> DecodeDone (SomeMessage msg) Nothing
    (ServerAgency TokIdle,      Just (AnyMessage msg@(MsgRequestTxs {})))   -> DecodeDone (SomeMessage msg) Nothing
    (ClientAgency TokTxs,       Just (AnyMessage msg@(MsgReplyTxs {})))     -> DecodeDone (SomeMessage msg) Nothing
    (ClientAgency (TokTxIds b), Just (AnyMessage msg)) -> case (b, msg) of
      (TokBlocking,    MsgReplyTxIds (BlockingReply {}))    -> DecodeDone (SomeMessage msg) Nothing
      (TokNonBlocking, MsgReplyTxIds (NonBlockingReply {})) -> DecodeDone (SomeMessage msg) Nothing
      (TokBlocking,    MsgDone {})                          -> DecodeDone (SomeMessage msg) Nothing
      (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
    (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
