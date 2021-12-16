{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.PingPong.Codec.CBOR where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)

import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read as CBOR

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.PingPong.Type

codecPingPong
  :: forall m.
     MonadST m
  => Codec PingPong CBOR.DeserialiseFailure m ByteString
codecPingPong = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg :: forall (pr :: PeerRole) st st'.
               PeerHasAgency pr st
            -> Message PingPong st st'
            -> CBOR.Encoding
  encodeMsg (ClientAgency TokIdle) MsgPing = CBOR.encodeWord 0
  encodeMsg (ServerAgency TokBusy) MsgPong = CBOR.encodeWord 1
  encodeMsg (ClientAgency TokIdle) MsgDone = CBOR.encodeWord 2

  decodeMsg :: forall (pr :: PeerRole) s (st :: PingPong).
               PeerHasAgency pr st
            -> CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) -> return $ SomeMessage MsgPing
      (ServerAgency TokBusy, 1) -> return $ SomeMessage MsgPong
      (ClientAgency TokIdle, 2) -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (ClientAgency TokIdle, _) -> fail "codecPingPong.StIdle: unexpected key"
      (ServerAgency TokBusy, _) -> fail "codecPingPong.StBusy: unexpected key"


