{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Codec
  ( codecLocalTxSubmission
  , codecLocalTxSubmissionId
  ) where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Text.Printf

import Network.TypedProtocol.Codec.CBOR

import Ouroboros.Network.Protocol.LocalTxSubmission.Type


codecLocalTxSubmission
  :: forall tx reject m.
     MonadST m
  => (tx -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s tx)
  -> (reject -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s reject)
  -> Codec (LocalTxSubmission tx reject) CBOR.DeserialiseFailure m ByteString
codecLocalTxSubmission encodeTx decodeTx encodeReject decodeReject =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall st st'.
              Message (LocalTxSubmission tx reject) st st'
           -> CBOR.Encoding
    encode (MsgSubmitTx tx) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodeTx tx

    encode MsgAcceptTx =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encode (MsgRejectTx reject) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeReject reject

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 3


    decode :: forall s (st :: LocalTxSubmission tx reject).
              ActiveState st
           => StateToken st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (SingIdle, 2, 0) -> do
          tx <- decodeTx
          return (SomeMessage (MsgSubmitTx tx))

        (SingBusy, 1, 1) ->
          return (SomeMessage MsgAcceptTx)

        (SingBusy, 2, 2) -> do
          reject <- decodeReject
          return (SomeMessage (MsgRejectTx reject))

        (SingIdle, 1, 3) ->
          return (SomeMessage MsgDone)

        (SingDone, _, _) -> notActiveState stok

        (_, _, _) -> fail (printf "codecLocalTxSubmission (%s, %s) unexpected key (%d, %d)"
                                  (show (activeAgency :: ActiveAgency st)) (show stok) key len)

codecLocalTxSubmissionId
  :: forall tx reject m.
     Monad m
  => Codec (LocalTxSubmission tx reject)
            CodecFailure m
           (AnyMessage (LocalTxSubmission tx reject))
codecLocalTxSubmissionId =
    Codec encode decode
  where
    encode :: forall st st'.
              ActiveState st
           => StateTokenI st
           => Message (LocalTxSubmission tx reject) st st'
           -> AnyMessage (LocalTxSubmission tx reject)
    encode = AnyMessage

    decode :: forall (st :: LocalTxSubmission tx reject).
              ActiveState st
           => StateToken st
           -> m (DecodeStep (AnyMessage (LocalTxSubmission tx reject))
                            CodecFailure m (SomeMessage st))
    decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
      (SingIdle, Just (AnyMessage msg@(MsgSubmitTx{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgAcceptTx{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgRejectTx{}))) -> res msg
      (SingIdle, Just (AnyMessage msg@(MsgDone{})))     -> res msg
      (SingDone, _)                                     -> notActiveState stok
      (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
      (_, _)       -> return (DecodeFail (CodecFailure failmsg))
    res msg = return (DecodeDone (SomeMessage msg) Nothing)
    failmsg = "codecLocalTxSubmissionId: no matching message"
