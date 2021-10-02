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

import           Control.Monad.Class.MonadST

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Singletons
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type


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
              SingI (PeerHasAgency st)
           => CBOR.Decoder s (SomeMessage st)
    decode = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (sing :: Sing (PeerHasAgency st), len, key) of
        (SingClientHasAgency SingIdle, 2, 0) -> do
          tx <- decodeTx
          return (SomeMessage (MsgSubmitTx tx))

        (SingServerHasAgency SingBusy, 1, 1) ->
          return (SomeMessage MsgAcceptTx)

        (SingServerHasAgency SingBusy, 2, 2) -> do
          reject <- decodeReject
          return (SomeMessage (MsgRejectTx reject))

        (SingClientHasAgency SingIdle, 1, 3) ->
          return (SomeMessage MsgDone)

        (stok, _, _) -> fail (printf "codecLocalTxSubmission (%s) unexpected key (%d, %d)" (show stok) key len)

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
              SingI (PeerHasAgency st)
           => Message (LocalTxSubmission tx reject) st st'
           -> AnyMessage (LocalTxSubmission tx reject)
    encode = AnyMessage

    decode :: forall (st :: LocalTxSubmission tx reject).
              SingI (PeerHasAgency st)
           => m (DecodeStep (AnyMessage (LocalTxSubmission tx reject))
                            CodecFailure m (SomeMessage st))
    decode = return $ DecodePartial $ \bytes -> case (sing :: Sing (PeerHasAgency st), bytes) of
      (SingClientHasAgency SingIdle, Just (AnyMessage msg@(MsgSubmitTx{}))) -> res msg
      (SingServerHasAgency SingBusy, Just (AnyMessage msg@(MsgAcceptTx{}))) -> res msg
      (SingServerHasAgency SingBusy, Just (AnyMessage msg@(MsgRejectTx{}))) -> res msg
      (SingClientHasAgency SingIdle, Just (AnyMessage msg@(MsgDone{})))     -> res msg
      (_, Nothing) -> return (DecodeFail CodecFailureOutOfInput)
      (_, _)       -> return (DecodeFail (CodecFailure failmsg))
    res msg = return (DecodeDone (SomeMessage msg) Nothing)
    failmsg = "codecLocalTxSubmissionId: no matching message"
