{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.ReqResp.Codec.CBOR where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen, encodeWord)
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen, decodeWord)
import           Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.ReqResp.Type

codecReqResp
  :: forall req resp m.
     ( MonadST m
     , Serialise req
     , Serialise resp
     )
  => Codec (ReqResp req resp) CBOR.DeserialiseFailure m ByteString
codecReqResp = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg :: forall (pr :: PeerRole) st st'.
                PeerHasAgency pr st
            -> Message (ReqResp req resp) st st'
            -> CBOR.Encoding
  encodeMsg (ClientAgency TokIdle) (MsgReq req) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encode req
  encodeMsg (ServerAgency TokBusy) (MsgResp resp) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encode resp
  encodeMsg (ClientAgency TokIdle) MsgDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2

  decodeMsg :: forall (pr :: PeerRole) s (st :: ReqResp req resp).
               PeerHasAgency pr st
            -> CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) -> SomeMessage . MsgReq  <$> CBOR.decode
      (ServerAgency TokBusy, 1) -> SomeMessage . MsgResp <$> CBOR.decode
      (ClientAgency TokIdle, 2) -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (ClientAgency TokIdle, _) -> fail "codecReqResp.StIdle: unexpected key"
      (ServerAgency TokBusy, _) -> fail "codecReqResp.StBusy: unexpected key"

