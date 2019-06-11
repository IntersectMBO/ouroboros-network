{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Codec (
    codecLocalTxSubmission
  ) where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read     as CBOR

import           Ouroboros.Network.Codec

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
    encode :: forall (pr :: PeerRole) st st'.
              PeerHasAgency pr st
           -> Message (LocalTxSubmission tx reject) st st'
           -> CBOR.Encoding
    encode (ClientAgency TokIdle) (MsgSubmitTx tx) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 0
     <> encodeTx tx

    encode (ServerAgency TokBusy) MsgAcceptTx =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 1

    encode (ServerAgency TokBusy) (MsgRejectTx reject) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> encodeReject reject

    encode (ClientAgency TokIdle) MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 3


    decode :: forall (pr :: PeerRole) s (st :: LocalTxSubmission tx reject).
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decode stok = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      case (stok, len, key) of
        (ClientAgency TokIdle, 2, 0) -> do
          tx <- decodeTx
          return (SomeMessage (MsgSubmitTx tx))

        (ServerAgency TokBusy, 1, 1) ->
          return (SomeMessage MsgAcceptTx)

        (ServerAgency TokBusy, 2, 2) -> do
          reject <- decodeReject
          return (SomeMessage (MsgRejectTx reject))

        (ClientAgency TokIdle, 1, 3) ->
          return (SomeMessage MsgDone)

        (ClientAgency TokIdle, _, _) ->
          fail "codecLocalTxSubmission.Idle: unexpected key"
        (ServerAgency TokBusy, _, _) ->
          fail "codecLocalTxSubmission.Busy: unexpected key"

