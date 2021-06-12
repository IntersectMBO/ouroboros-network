{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.PingPong.Codec.CBOR where

import           Control.Monad.Class.MonadST

import           Data.ByteString.Lazy (ByteString)
import           Data.Singletons

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
  encodeMsg :: forall st st'.
               Message PingPong st st'
            -> CBOR.Encoding
  encodeMsg MsgPing = CBOR.encodeWord 0
  encodeMsg MsgPong = CBOR.encodeWord 1
  encodeMsg MsgDone = CBOR.encodeWord 2

  decodeMsg :: forall s (st :: PingPong).
               SingI st
            => CBOR.Decoder s (SomeMessage st)
  decodeMsg = do
    key <- CBOR.decodeWord
    case (sing :: Sing st, key) of
      (SingIdle, 0) -> return $ SomeMessage MsgPing
      (SingBusy, 1) -> return $ SomeMessage MsgPong
      (SingIdle, 2) -> return $ SomeMessage MsgDone

      -- TODO proper exceptions
      (SingIdle, _) -> fail "codecPingPong.StIdle: unexpected key"
      (SingBusy, _) -> fail "codecPingPong.StBusy: unexpected key"
      (SingDone, _) -> fail "codecPingPong.StDone: unexpected key"


