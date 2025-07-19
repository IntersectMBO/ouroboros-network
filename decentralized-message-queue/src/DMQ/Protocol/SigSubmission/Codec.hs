{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.SigSubmission.Codec
  ( codecSigSubmission
  , byteLimitsSigSubmission
  , timeLimitsSigSubmission
  , codecSigSubmissionId
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

import DMQ.Protocol.SigSubmission.Type
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
timeLimitsSigSubmission :: ProtocolTimeLimits SigSubmission
timeLimitsSigSubmission = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: SigSubmission).
                    ActiveState st => StateToken st -> Maybe DiffTime
    stateToLimit SingInit                    = waitForever
    stateToLimit (SingTxIds SingBlocking)    = waitForever
    stateToLimit (SingTxIds SingNonBlocking) = shortWait
    stateToLimit SingTxs                     = shortWait
    stateToLimit SingIdle                    = waitForever
    stateToLimit a@SingDone                  = notActiveState a


-- TODO: these limits needs to be checked with the mithril team
byteLimitsSigSubmission :: forall bytes.
                           (bytes -> Word)
                        -> ProtocolSizeLimits SigSubmission bytes
byteLimitsSigSubmission = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: SigSubmission).
                    ActiveState st => StateToken st -> Word
    stateToLimit SingInit                    = smallByteLimit
    stateToLimit (SingTxIds SingBlocking)    = smallByteLimit
    stateToLimit (SingTxIds SingNonBlocking) = smallByteLimit
    stateToLimit SingTxs                     = smallByteLimit
    stateToLimit SingIdle                    = smallByteLimit
    stateToLimit a@SingDone                  = notActiveState a


-- | 'SigSubmission' protocol codec.
--
codecSigSubmission
  :: forall m.
     MonadST m
  => Codec SigSubmission CBOR.DeserialiseFailure m ByteString
codecSigSubmission =
    TX.codecTxSubmission2 encodeSigId decodeSigId
                          encodeSig   decodeSig
  where
    encodeSigId :: SigId -> CBOR.Encoding
    encodeSigId SigId { getSigId } = CBOR.encodeBytes (getSigHash getSigId)

    decodeSigId :: forall s. CBOR.Decoder s SigId
    decodeSigId = SigId . SigHash <$> CBOR.decodeBytes

    encodeSig :: Sig -> CBOR.Encoding
    encodeSig Sig { sigId,
                    sigBody,
                    sigTTL,
                    sigKesSignature,
                    sigOpCertificate
                  }
       = CBOR.encodeListLen 5
      <> encodeSigId sigId
      <> CBOR.encodeBytes (getSigBody sigBody)
      <> CBOR.encodeWord32 (floor $ getSigTTL sigTTL)
      <> CBOR.encodeBytes (getSigKesSignature sigKesSignature)
      <> CBOR.encodeBytes (getSigOpCertificate sigOpCertificate)

    decodeSig :: forall s. CBOR.Decoder s Sig
    decodeSig = do
      a <- CBOR.decodeListLen
      when (a /= 5) $ fail (printf "codecSigSubmission: unexpected number of parameters %d" a)
      sigId <- decodeSigId
      sigBody <- SigBody <$> CBOR.decodeBytes
      sigTTL <- SigTTL . realToFrac <$> CBOR.decodeWord32
      sigKesSignature <- SigKesSignature <$> CBOR.decodeBytes
      sigOpCertificate <- SigOpCertificate <$> CBOR.decodeBytes
      return Sig {
          sigId,
          sigBody,
          sigTTL,
          sigKesSignature,
          sigOpCertificate
        }


codecSigSubmissionId
  :: Monad m
  => Codec SigSubmission CodecFailure m (AnyMessage SigSubmission)
codecSigSubmissionId = TX.codecTxSubmission2Id
