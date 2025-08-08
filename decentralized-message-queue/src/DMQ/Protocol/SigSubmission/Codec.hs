{-# LANGUAGE DataKinds           #-}
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
  , encodeSig
  , decodeSig
  , encodeSigId
  , decodeSigId
    -- * Utils for testing
  , encodeSigRaw
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


encodeSigId :: SigId -> CBOR.Encoding
encodeSigId SigId { getSigId } = CBOR.encodeBytes (getSigHash getSigId)

decodeSigId :: forall s. CBOR.Decoder s SigId
decodeSigId = SigId . SigHash <$> CBOR.decodeBytes


encodeSigRaw :: SigRaw
             -> CBOR.Encoding
encodeSigRaw SigRaw {
    sigRawId,
    sigRawBody,
    sigRawKESSignature,
    sigRawOpCertificate,
    sigRawExpiresAt
  }
  =  CBOR.encodeListLen 5
  <> encodeSigId sigRawId
  <> CBOR.encodeBytes (getSigBody sigRawBody)
  <> CBOR.encodeWord32 (floor sigRawExpiresAt)
  <> CBOR.encodeBytes (getSigOpCertificate sigRawOpCertificate)
  <> CBOR.encodeBytes (getSigKESSignature sigRawKESSignature)



-- | 'SigSubmission' protocol codec.
--
codecSigSubmission
  :: forall m.
     MonadST m
  => AnnotatedCodec SigSubmission CBOR.DeserialiseFailure m ByteString
codecSigSubmission =
    TX.anncodecTxSubmission2'
      SigWithBytes
      encodeSigId decodeSigId
      encodeSig   decodeSig


encodeSig :: Sig -> CBOR.Encoding
encodeSig = Utils.encodeBytes . sigRawBytes

decodeSig :: forall s. CBOR.Decoder s (ByteString -> SigRaw)
decodeSig = do
  a <- CBOR.decodeListLen
  when (a /= 5) $ fail (printf "codecSigSubmission: unexpected number of parameters %d" a)
  sigRawId <- decodeSigId
  sigRawBody <- SigBody <$> CBOR.decodeBytes
  sigRawExpiresAt <- realToFrac <$> CBOR.decodeWord32
  sigRawOpCertificate <- SigOpCertificate <$> CBOR.decodeBytes
  sigRawKESSignature <- SigKESSignature <$> CBOR.decodeBytes
  return $ \_ -> SigRaw {
      sigRawId,
      sigRawBody,
      sigRawKESSignature,
      sigRawOpCertificate,
      sigRawExpiresAt
    }


codecSigSubmissionId
  :: Monad m
  => Codec SigSubmission CodecFailure m (AnyMessage SigSubmission)
codecSigSubmissionId = TX.codecTxSubmission2Id
