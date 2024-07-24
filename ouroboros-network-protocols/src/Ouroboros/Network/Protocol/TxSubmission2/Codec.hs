{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
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

import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.List.NonEmpty qualified as NonEmpty

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Text.Printf

import Network.TypedProtocol.Codec.CBOR

import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.TxSubmission2.Type

-- | Byte Limits.
byteLimitsTxSubmission2 :: forall bytes txid tx.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (TxSubmission2 txid tx) bytes
byteLimitsTxSubmission2 = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    ActiveState st => StateToken st -> Word
    stateToLimit SingInit                    = smallByteLimit
    stateToLimit (SingTxIds SingBlocking)    = largeByteLimit
    stateToLimit (SingTxIds SingNonBlocking) = largeByteLimit
    stateToLimit SingTxs                     = largeByteLimit
    stateToLimit SingIdle                    = smallByteLimit
    stateToLimit a@SingDone                  = notActiveState a


-- | Time Limits.
--
-- `SingTxIds SingBlocking` No timeout
-- `SingTxIds SingNonBlocking` `shortWait` timeout
-- `SingTxs` `shortWait` timeout
-- `SingIdle` `shortWait` timeout
timeLimitsTxSubmission2 :: forall txid tx. ProtocolTimeLimits (TxSubmission2 txid tx)
timeLimitsTxSubmission2 = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingInit                    = waitForever
    stateToLimit (SingTxIds SingBlocking)    = waitForever
    stateToLimit (SingTxIds SingNonBlocking) = shortWait
    stateToLimit SingTxs                     = shortWait
    stateToLimit SingIdle                    = waitForever
    stateToLimit a@SingDone                  = notActiveState a


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
    decode :: forall (st :: TxSubmission2 txid tx).
              ActiveState st
           => StateToken st
           -> forall s. CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      decodeTxSubmission2 decodeTxId decodeTx stok len key

encodeTxSubmission2
    :: forall txid tx.
       (txid -> CBOR.Encoding)
    -> (tx -> CBOR.Encoding)
    -> (forall (st :: TxSubmission2 txid tx) (st' :: TxSubmission2 txid tx).
               Message (TxSubmission2 txid tx) st st'
            -> CBOR.Encoding)
encodeTxSubmission2 encodeTxId encodeTx = encode
  where
    encode :: forall st st'.
              Message (TxSubmission2 txid tx) st st'
           -> CBOR.Encoding
    encode MsgInit =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 6
    encode (MsgRequestTxIds blocking (NumTxIdsToAck ackNo) (NumTxIdsToReq reqNo)) =
        CBOR.encodeListLen 4
     <> CBOR.encodeWord 0
     <> CBOR.encodeBool (case blocking of
                           SingBlocking    -> True
                           SingNonBlocking -> False)
     <> CBOR.encodeWord16 ackNo
     <> CBOR.encodeWord16 reqNo

    encode (MsgReplyTxIds txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 1
     <> CBOR.encodeListLenIndef
     <> foldr (\(txid, SizeInBytes sz) r ->
                                CBOR.encodeListLen 2
                             <> encodeTxId txid
                             <> CBOR.encodeWord32 sz
                             <> r)
              CBOR.encodeBreak
              txids'
      where
        txids' :: [(txid, SizeInBytes)]
        txids' = case txids of
                   BlockingReply    xs -> NonEmpty.toList xs
                   NonBlockingReply xs -> xs

    encode (MsgRequestTxs txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTxId txid <> r) CBOR.encodeBreak txids

    encode (MsgReplyTxs txs) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTx txid <> r) CBOR.encodeBreak txs

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 4


decodeTxSubmission2
    :: forall txid tx.
       (forall s . CBOR.Decoder s txid)
    -> (forall s . CBOR.Decoder s tx)
    -> (forall (st :: TxSubmission2 txid tx) s.
               ActiveState st
            => StateToken st
            -> Int
            -> Word
            -> CBOR.Decoder s (SomeMessage st))
decodeTxSubmission2 decodeTxId decodeTx = decode
  where
    decode :: forall s (st :: TxSubmission2 txid tx).
              ActiveState st
           => StateToken st
           -> Int
           -> Word
           -> CBOR.Decoder s (SomeMessage st)
    decode stok len key = do
      case (stok, len, key) of
        (SingInit,       1, 6) ->
          return (SomeMessage MsgInit)
        (SingIdle,       4, 0) -> do
          blocking <- CBOR.decodeBool
          ackNo    <- NumTxIdsToAck <$> CBOR.decodeWord16
          reqNo    <- NumTxIdsToReq <$> CBOR.decodeWord16
          return $! case blocking of
            True  -> SomeMessage (MsgRequestTxIds SingBlocking    ackNo reqNo)
            False -> SomeMessage (MsgRequestTxIds SingNonBlocking ackNo reqNo)

        (SingTxIds b,  2, 1) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef
                     (flip (:)) [] reverse
                     (do CBOR.decodeListLenOf 2
                         txid <- decodeTxId
                         sz   <- CBOR.decodeWord32
                         return (txid, SizeInBytes sz))
          case (b, txids) of
            (SingBlocking, t:ts) ->
              return $
                SomeMessage (MsgReplyTxIds (BlockingReply (t NonEmpty.:| ts)))

            (SingNonBlocking, ts) ->
              return $
                SomeMessage (MsgReplyTxIds (NonBlockingReply ts))

            (SingBlocking, []) ->
              fail "codecTxSubmission: MsgReplyTxIds: empty list not permitted"


        (SingIdle,       2, 2) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTxId
          return (SomeMessage (MsgRequestTxs txids))

        (SingTxs,     2, 3) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTx
          return (SomeMessage (MsgReplyTxs txids))

        (SingTxIds SingBlocking, 1, 4) ->
          return (SomeMessage MsgDone)

        (SingDone, _, _) -> notActiveState stok

        --
        -- failures per protocol state
        --

        (SingInit, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingTxIds SingBlocking, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingTxIds SingNonBlocking, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingTxs, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)
        (SingIdle, _, _) ->
            fail (printf "codecTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)

codecTxSubmission2Id
  :: forall txid tx m. Monad m
  => Codec (TxSubmission2 txid tx) CodecFailure m (AnyMessage (TxSubmission2 txid tx))
codecTxSubmission2Id = Codec { encode, decode }
 where
  encode :: forall st st'.
            ActiveState st
         => StateTokenI st
         => Message (TxSubmission2 txid tx) st st'
         -> AnyMessage (TxSubmission2 txid tx)
  encode = AnyMessage

  decode :: forall (st :: TxSubmission2 txid tx).
            ActiveState st
         => StateToken st
         -> m (DecodeStep (AnyMessage (TxSubmission2 txid tx))
                          CodecFailure m (SomeMessage st))
  decode stok = return $ DecodePartial $ \bytes -> return $ case (stok, bytes) of
    (SingInit,      Just (AnyMessage msg@MsgInit))              -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingBlocking _ _)))    -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingNonBlocking _ _))) -> DecodeDone (SomeMessage msg) Nothing
    (SingIdle,      Just (AnyMessage msg@(MsgRequestTxs {})))   -> DecodeDone (SomeMessage msg) Nothing
    (SingTxs,       Just (AnyMessage msg@(MsgReplyTxs {})))     -> DecodeDone (SomeMessage msg) Nothing
    (SingTxIds b, Just (AnyMessage msg)) -> case (b, msg) of
      (SingBlocking,    MsgReplyTxIds (BlockingReply {}))    -> DecodeDone (SomeMessage msg) Nothing
      (SingNonBlocking, MsgReplyTxIds (NonBlockingReply {})) -> DecodeDone (SomeMessage msg) Nothing
      (SingBlocking,    MsgDone {})                          -> DecodeDone (SomeMessage msg) Nothing
      (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
    (SingDone, _) -> notActiveState stok
    (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
