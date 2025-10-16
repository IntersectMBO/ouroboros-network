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
import Data.Text qualified as T
import Data.Tuple (swap)
import Text.Printf

import Cardano.Binary
import Cardano.KESAgent.KES.Crypto (Crypto (..))

import DMQ.Protocol.LocalMsgSubmission.Type
import DMQ.Protocol.SigSubmission.Codec qualified as SigSubmission
import DMQ.Protocol.SigSubmission.Type (Sig (..))
import DMQ.Protocol.SigSubmission.Validate

import Network.TypedProtocol.Codec.CBOR
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec qualified as LTX

codecLocalMsgSubmission
  :: forall crypto m.
     ( MonadST m
     , Crypto crypto
     )
  => (MempoolAddFail (Sig crypto) -> CBOR.Encoding)
  -> (forall s. CBOR.Decoder s (MempoolAddFail (Sig crypto)))
  -> AnnotatedCodec (LocalMsgSubmission (Sig crypto)) CBOR.DeserialiseFailure m ByteString
codecLocalMsgSubmission =
  LTX.anncodecLocalTxSubmission' SigWithBytes SigSubmission.encodeSig SigSubmission.decodeSig

encodeReject :: MempoolAddFail (Sig crypto) -> CBOR.Encoding
encodeReject = \case
  SigInvalid reason -> CBOR.encodeListLen 2 <> CBOR.encodeWord8 0 <> e
    where
      e = case reason of
        InvalidKESSignature ocertKESPeriod sigKESPeriod err ->  mconcat [
          CBOR.encodeListLen 4, CBOR.encodeWord8 0, toCBOR ocertKESPeriod, toCBOR sigKESPeriod, CBOR.encodeString (T.pack err)
          ]
        InvalidSignatureOCERT ocertN sigKESPeriod err -> mconcat [
          CBOR.encodeListLen 4, CBOR.encodeWord8 1, CBOR.encodeWord64 ocertN, toCBOR sigKESPeriod, CBOR.encodeString (T.pack err)
          ]
        KESBeforeStartOCERT startKESPeriod sigKESPeriod -> mconcat [
          CBOR.encodeListLen 3, CBOR.encodeWord8 2, toCBOR startKESPeriod, toCBOR sigKESPeriod
          ]
        KESAfterEndOCERT endKESPeriod sigKESPeriod ->  mconcat [
          CBOR.encodeListLen 3, CBOR.encodeWord8 3, toCBOR endKESPeriod, toCBOR sigKESPeriod
          ]
        UnrecognizedPool -> CBOR.encodeListLen 1 <> CBOR.encodeWord8 4
        NotInitialized   -> CBOR.encodeListLen 1 <> CBOR.encodeWord8 5
        ClockSkew        -> CBOR.encodeListLen 1 <> CBOR.encodeWord8 6
        InvalidOCertCounter seen received
          -> mconcat
              [CBOR.encodeListLen 3, CBOR.encodeWord8 7, CBOR.encodeWord64 seen, CBOR.encodeWord64 received]
  SigDuplicate      -> CBOR.encodeListLen 1 <> CBOR.encodeWord8 1
  SigExpired        -> CBOR.encodeListLen 1 <> CBOR.encodeWord8 2
  SigResultOther reason
                    -> CBOR.encodeListLen 2 <> CBOR.encodeWord8 3 <> CBOR.encodeString reason

decodeReject :: CBOR.Decoder s (MempoolAddFail (Sig crypto))
decodeReject = do
  len <- CBOR.decodeListLen
  tag <- CBOR.decodeWord8
  case (tag, len) of
    (0, 2) -> SigInvalid <$> decSigValidError
      where
        decSigValidError :: CBOR.Decoder s SigValidationError
        decSigValidError = do
          lenTag <- (,) <$> CBOR.decodeListLen <*> CBOR.decodeWord8
          case swap lenTag of
            (0, 4) -> InvalidKESSignature <$> fromCBOR <*> fromCBOR <*> (T.unpack <$> CBOR.decodeString)
            (1, 4) -> InvalidSignatureOCERT <$> CBOR.decodeWord64 <*> fromCBOR <*> (T.unpack <$> CBOR.decodeString)
            (2, 3) -> KESBeforeStartOCERT <$> fromCBOR <*> fromCBOR
            (3, 4) -> KESAfterEndOCERT <$> fromCBOR <*> fromCBOR
            (4, 1) -> pure UnrecognizedPool
            (5, 1) -> pure NotInitialized
            (6, 1) -> pure ClockSkew
            (7, 3) -> InvalidOCertCounter <$> fromCBOR <*> fromCBOR
            _otherwise -> fail $ printf "unrecognized (tag,len) = (%d, %d) when decoding SigInvalid tag" tag len

    (1, 1)     -> pure SigDuplicate
    (2, 1)     -> pure SigExpired
    (3, 2)     -> SigResultOther <$> CBOR.decodeString
    _otherwise -> fail $ printf "unrecognized (tag,len) = (%d, %d)" tag len
