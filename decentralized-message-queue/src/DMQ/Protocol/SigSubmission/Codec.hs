{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DMQ.Protocol.SigSubmission.Codec
  ( codecSigSubmission
  , byteLimitsSigSubmission
  , timeLimitsSigSubmission
  , codecSigSubmissionId
    -- * Utils for testing
  , encodeSigRaw
  ) where

import Control.Monad (when)
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadTime.SI
import Data.ByteString.Lazy (ByteString)
import Data.Typeable
import Text.Printf

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR

import Network.TypedProtocol.Codec.CBOR

import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.Binary (FromCBOR (..), ToCBOR (..))

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


encodeSigRaw :: ( Crypto crypto
                , Typeable crypto
                )
             => SigRaw crypto
             -> CBOR.Encoding
encodeSigRaw SigRaw {
    sigRawId,
    sigRawBody,
    sigRawExpiresAt,
    sigRawKesSignature,
    sigRawOpCertificate
  }
  =  CBOR.encodeListLen 5
  <> encodeSigId sigRawId
  <> CBOR.encodeBytes (getSigBody sigRawBody)
  <> CBOR.encodeWord32 (floor sigRawExpiresAt)
  <> CBOR.encodeBytes (getSigKesSignature sigRawKesSignature)
  <> toCBOR (getSigOpCertificate sigRawOpCertificate)

-- | 'SigSubmission' protocol codec.
--
codecSigSubmission
  :: forall crypto m.
     ( Crypto crypto
     , Typeable crypto
     , MonadST m
     )
  => AnnotatedCodec (SigSubmission crypto) CBOR.DeserialiseFailure m ByteString
codecSigSubmission =
    TX.anncodecTxSubmission2'
      SigWithBytes
      encodeSigId decodeSigId
      encodeSig   decodeSig
  where
    encodeSig :: Sig crypto -> CBOR.Encoding
    encodeSig = TX.encodeBytes . sigRawBytes

    decodeSig :: forall s. CBOR.Decoder s (ByteString -> SigRaw crypto)
    decodeSig = do
      a <- CBOR.decodeListLen
      when (a /= 5) $ fail (printf "codecSigSubmission: unexpected number of parameters %d" a)
      sigRawId <- decodeSigId
      sigRawBody <- SigBody <$> CBOR.decodeBytes
      sigRawExpiresAt <- realToFrac <$> CBOR.decodeWord32
      sigRawKesSignature <- SigKesSignature <$> CBOR.decodeBytes
      sigRawOpCertificate <- SigOpCertificate <$> fromCBOR
      return $ \_ -> SigRaw {
          sigRawId,
          sigRawBody,
          sigRawExpiresAt,
          sigRawOpCertificate,
          sigRawKesSignature
        }


codecSigSubmissionId
  :: Monad m
  => Codec (SigSubmission crypto) CodecFailure m (AnyMessage (SigSubmission crypto))
codecSigSubmissionId = TX.codecTxSubmission2Id
