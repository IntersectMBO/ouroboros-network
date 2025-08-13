{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}

-- | The codec for the local message submission miniprotocol
--
module DMQ.Protocol.LocalMsgSubmission.Codec where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Control.Monad.Class.MonadST
import Data.ByteString.Lazy (ByteString)
import Data.Typeable
import Text.Printf

import Cardano.KESAgent.KES.Crypto (Crypto (..))

import DMQ.Protocol.LocalMsgSubmission.Type
import DMQ.Protocol.SigSubmission.Codec qualified as SigSubmission
import DMQ.Protocol.SigSubmission.Type (Sig (..))

import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec qualified as LTX

codecLocalMsgSubmission
  :: forall crypto m.
     ( MonadST m
     , Crypto crypto
     , Typeable crypto
     )
  => (SigMempoolFail -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s SigMempoolFail)
  -> AnnotatedCodec (LocalMsgSubmission (Sig crypto)) CBOR.DeserialiseFailure m ByteString
codecLocalMsgSubmission =
  LTX.anncodecLocalTxSubmission' SigWithBytes SigSubmission.encodeSig SigSubmission.decodeSig

encodeReject :: SigMempoolFail -> CBOR.Encoding
encodeReject = \case
  SigInvalid reason -> CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> CBOR.encodeString reason
  SigDuplicate      -> CBOR.encodeListLen 1 <> CBOR.encodeWord 1
  SigExpired        -> CBOR.encodeListLen 1 <> CBOR.encodeWord 2
  SigResultOther reason
                    -> CBOR.encodeListLen 2 <> CBOR.encodeWord 3 <> CBOR.encodeString reason

decodeReject :: CBOR.Decoder s SigMempoolFail
decodeReject = do
  len <- CBOR.decodeListLen
  tag <- CBOR.decodeWord
  case (tag, len) of
    (0, 2)     -> SigInvalid <$> CBOR.decodeString
    (1, 1)     -> pure SigDuplicate
    (2, 1)     -> pure SigExpired
    (3, 2)     -> SigResultOther <$> CBOR.decodeString
    _otherwise -> fail $ printf "unrecognized (tag,len) = (%d, %d)" tag len
