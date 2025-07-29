{-# LANGUAGE RankNTypes       #-}

-- | The codec for the local message submission miniprotocol
--
module DMQ.Protocol.LocalMsgSubmission.Codec where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)

import DMQ.Protocol.LocalMsgSubmission.Type
import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec

codecLocalMsgSubmission
  :: forall sig reject m.
     MonadST m
  => (sig -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s sig)
  -> (reject -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s reject)
  -> Codec (LocalMsgSubmission sig reject) CBOR.DeserialiseFailure m ByteString
codecLocalMsgSubmission =
  codecLocalTxSubmission

encodeReject :: (reject -> CBOR.Encoding)
encodeReject = const CBOR.encodeNull

decodeReject :: CBOR.Decoder s ()
decodeReject = CBOR.decodeNull
