{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.ReqResp.Codec.CBOR where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)
import           Data.Singletons

import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeListLen,
                     decodeWord)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeListLen,
                     encodeWord)
import qualified Codec.CBOR.Read as CBOR
import           Codec.Serialise.Class (Serialise)
import qualified Codec.Serialise.Class as CBOR

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core
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
  encodeMsg :: forall st st'.
               Message (ReqResp req resp) st st'
            -> CBOR.Encoding
  encodeMsg (MsgReq req) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encode req
  encodeMsg (MsgResp resp) =
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encode resp
  encodeMsg MsgDone =
    CBOR.encodeListLen 1 <> CBOR.encodeWord 2

  decodeMsg :: forall s (st :: ReqResp req resp).
               SingI st
            => CBOR.Decoder s (SomeMessage st)
  decodeMsg = do
    _ <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (sing :: Sing st, key) of
      (SingIdle, 0) -> SomeMessage . MsgReq  <$> CBOR.decode
      (SingBusy, 1) -> SomeMessage . MsgResp <$> CBOR.decode
      (SingIdle, 2) -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (SingIdle, _) -> fail "codecReqResp.StIdle: unexpected key"
      (SingBusy, _) -> fail "codecReqResp.StBusy: unexpected key"
      (SingDone, _) -> fail "codecReqResp.StBusy: unexpected key"

