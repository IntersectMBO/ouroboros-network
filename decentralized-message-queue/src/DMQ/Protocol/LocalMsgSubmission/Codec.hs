{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module DMQ.Protocol.LocalMsgSubmission.Codec where

import Control.Monad.Class.MonadST

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Data.ByteString.Lazy (ByteString)
import Text.Printf

import Network.TypedProtocol.Codec.CBOR

import DMQ.Protocol.LocalMsgSubmission.Type


codecLocalMsgSubmission
  :: forall msg reject m.
     MonadST m
  => (msg -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s msg)
  -> (reject -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s reject)
  -> Codec (LocalMsgSubmission msg reject) CBOR.DeserialiseFailure m ByteString
codecLocalMsgSubmission encodeMsg decodeMsg encodeReject decodeReject =
    mkCodecCborLazyBS encode decode
  where
    encode :: forall st st'.
              Message (LocalMsgSubmission msg reject) st st'
           -> CBOR.Encoding
    encode (MsgSubmit msg) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 10
     <> encodeMsg msg

    encode MsgAccept =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 11

    encode (MsgReject reject) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 12
     <> encodeReject reject

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 13


    decode :: forall s (st :: LocalMsgSubmission msg reject).
              ActiveState st
           => StateToken st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (SingIdle, 2, 10) -> SomeMessage . MsgSubmit <$> decodeMsg

        (SingBusy, 1, 11) ->
          return (SomeMessage MsgAccept)

        (SingBusy, 2, 12) -> SomeMessage . MsgReject <$> decodeReject

        (SingIdle, 1, 13) ->
          return (SomeMessage MsgDone)

        (SingDone, _, _) -> notActiveState stok

        (_, _, _) -> fail (printf "codecLocalMsgSubmission (%s, %s) unexpected key (%d, %d)"
                                  (show (activeAgency :: ActiveAgency st)) (show stok) key len)

codecLocalMsgSubmissionId
  :: forall msg reject m.
     Monad m
  => Codec (LocalMsgSubmission msg reject)
           CodecFailure m
           (AnyMessage (LocalMsgSubmission msg reject))
codecLocalMsgSubmissionId =
    Codec encode decode
  where
    encode :: forall st st'.
              ActiveState st
           => StateTokenI st
           => Message (LocalMsgSubmission msg reject) st st'
           -> AnyMessage (LocalMsgSubmission msg reject)
    encode = AnyMessage

    decode :: forall (st :: LocalMsgSubmission msg reject).
              ActiveState st
           => StateToken st
           -> m (DecodeStep (AnyMessage (LocalMsgSubmission msg reject))
                            CodecFailure m (SomeMessage st))
    decode stok = return $ DecodePartial $ \bytes -> case (stok, bytes) of
      (SingIdle, Just (AnyMessage msg@(MsgSubmit{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgAccept{}))) -> res msg
      (SingBusy, Just (AnyMessage msg@(MsgReject{}))) -> res msg
      (SingIdle, Just (AnyMessage msg@(MsgDone{})))   -> res msg
      (SingDone, _)                                   -> notActiveState stok
      (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
      (_, _)       -> return (DecodeFail (CodecFailure failmsg))
    res msg = return (DecodeDone (SomeMessage msg) Nothing)
    failmsg = "codecLocalMsgSubmissionId: no matching message"
