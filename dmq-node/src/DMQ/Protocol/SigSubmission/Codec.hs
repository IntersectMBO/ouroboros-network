{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.SigSubmission.Codec
  ( codecSigSubmission
  , byteLimitsSigSubmission
  , timeLimitsSigSubmission
  , codecSigSubmissionId
    -- * Exported utility functions
  , encodeSig
  , decodeSig
  , encodeSigId
  , decodeSigId
  , encodeSigOpCertificate
  , decodeSigOpCertificate
  ) where

import Control.Monad (when)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Text.Printf

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Network.TypedProtocol.Codec.CBOR

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.DSIGN.Class (decodeSignedDSIGN, decodeVerKeyDSIGN,
           encodeSignedDSIGN)
import Cardano.Crypto.KES.Class (decodeSigKES, decodeVerKeyKES, encodeVerKeyKES)
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))

import DMQ.Protocol.SigSubmission.Type
import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils
import Ouroboros.Network.Protocol.Limits
import Ouroboros.Network.Protocol.TxSubmission2.Codec qualified as TX


-- | 'SigSubmission' time limits.
--
-- +-----------------------------+---------------+
-- | 'SigSubmission' state       | timeout (s)   |
-- +=============================+===============+
-- | `StInit`                    | `waitForever` |
-- +-----------------------------+---------------+
-- | `StIdle`                    | `waitForever` |
-- +-----------------------------+---------------+
-- | @'StTxIds' 'StBlocking'@    | `waitForever` |
-- +-----------------------------+---------------+
-- | @'StTxIds' 'StNonBlocking'@ | `shortWait`   |
-- +-----------------------------+---------------+
-- | `StTxs`                     | `shortWait`   |
-- +-----------------------------+---------------+
--
timeLimitsSigSubmission :: forall crypto. ProtocolTimeLimits (SigSubmission crypto)
timeLimitsSigSubmission = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: SigSubmission crypto).
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingInit                    = waitForever
    stateToLimit (SingTxIds SingBlocking)    = waitForever
    stateToLimit (SingTxIds SingNonBlocking) = shortWait
    stateToLimit SingTxs                     = shortWait
    stateToLimit SingIdle                    = waitForever
    stateToLimit a@SingDone                  = notActiveState a


-- TODO: these limits needs to be checked with the mithril team
byteLimitsSigSubmission :: forall crypto bytes.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (SigSubmission crypto) bytes
byteLimitsSigSubmission = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: SigSubmission crypto).
                    ActiveState st => StateToken st -> Word
    stateToLimit SingInit                    = smallByteLimit
    stateToLimit (SingTxIds SingBlocking)    = smallByteLimit
    stateToLimit (SingTxIds SingNonBlocking) = smallByteLimit
    stateToLimit SingTxs                     = smallByteLimit
    stateToLimit SingIdle                    = smallByteLimit
    stateToLimit a@SingDone                  = notActiveState a


encodeSigId :: SigId -> CBOR.Encoding
encodeSigId SigId { getSigId } = CBOR.encodeBytes (getSigHash getSigId)

decodeSigId :: forall s. CBOR.Decoder s SigId
decodeSigId = SigId . SigHash <$> CBOR.decodeBytes


-- | We follow the same encoding as in `cardano-ledger` for `OCert`.
--
encodeSigOpCertificate :: Crypto crypto
                       => SigOpCertificate crypto -> CBOR.Encoding
encodeSigOpCertificate (SigOpCertificate ocert) =
       CBOR.encodeListLen 4
    <> encodeVerKeyKES (ocertVkHot ocert)
    <> toCBOR (ocertN ocert)
    <> toCBOR (ocertKESPeriod ocert)
    <> encodeSignedDSIGN (ocertSigma ocert)


decodeSigOpCertificate :: forall s crypto. Crypto crypto
                       => CBOR.Decoder s (SigOpCertificate crypto)
decodeSigOpCertificate = do
    len <- CBOR.decodeListLen
    when (len /= 4) $ fail (printf "decodeSigOpCertificate: unexpected number of parameters %d" len)
    ocertVkHot <- decodeVerKeyKES
    ocertN <- fromCBOR
    ocertKESPeriod <- fromCBOR
    ocertSigma <- decodeSignedDSIGN
    return $ SigOpCertificate $ OCert {
        ocertVkHot,
        ocertN,
        ocertKESPeriod,
        ocertSigma
      }


-- | 'SigSubmission' protocol codec.
--
codecSigSubmission
  :: forall crypto m.
     ( Crypto crypto
     , MonadST m
     )
  => AnnotatedCodec (SigSubmission crypto) CBOR.DeserialiseFailure m ByteString
codecSigSubmission =
    TX.anncodecTxSubmission2'
      SigWithBytes
      encodeSigId decodeSigId
      encodeSig   decodeSig


encodeSig :: Sig crypto -> CBOR.Encoding
encodeSig = Utils.encodeBytes . sigRawBytes

decodeSig :: forall crypto s.
             ( Crypto crypto
             )
          => CBOR.Decoder s (ByteString -> SigRawWithSignedBytes crypto)
decodeSig = do
  -- start of signed data
  startOffset <- CBOR.peekByteOffset
  a <- CBOR.decodeListLen
  when (a /= 7) $ fail (printf "codecSigSubmission: unexpected number of parameters %d" a)
  sigRawId <- decodeSigId
  sigRawBody <- SigBody <$> CBOR.decodeBytes
  sigRawKESPeriod <- KESPeriod <$> CBOR.decodeWord
  sigRawExpiresAt <- realToFrac <$> CBOR.decodeWord32
  -- end of signed data
  endOffset <- CBOR.peekByteOffset

  sigRawKESSignature <- SigKESSignature <$> decodeSigKES
  sigRawOpCertificate <- decodeSigOpCertificate
  sigRawColdKey <- SigColdKey <$> decodeVerKeyDSIGN
  return $ \bytes -- ^ full bytes of the message, not just the sig part
         -> SigRawWithSignedBytes {
      sigRawSignedBytes = Utils.bytesBetweenOffsets startOffset endOffset bytes,
      sigRaw = SigRaw {
        sigRawId,
        sigRawBody,
        sigRawKESSignature,
        sigRawKESPeriod,
        sigRawOpCertificate,
        sigRawColdKey,
        sigRawExpiresAt
      }
    }


codecSigSubmissionId
  :: Monad m
  => Codec (SigSubmission crypto) CodecFailure m (AnyMessage (SigSubmission crypto))
codecSigSubmissionId = TX.codecTxSubmission2Id
