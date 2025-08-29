{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE PatternSynonyms    #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKesSignature (..)
  , SigOpCertificate (..)
  , SigPayload (..)
  , Sig (SigRaw, Sig, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKesSignature)
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  ) where

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)

import Ouroboros.Network.Protocol.TxSubmission2.Type as SigSubmission hiding
           (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import Ouroboros.Network.Util.ShowProxy


newtype SigHash = SigHash { getSigHash :: ByteString }
  deriving stock (Show, Eq, Ord)

newtype SigId = SigId { getSigId :: SigHash }
  deriving stock (Show, Eq, Ord)

instance ShowProxy SigId where
  showProxy _ = "SigId"

newtype SigBody = SigBody { getSigBody :: ByteString }
  deriving stock (Show, Eq)


-- TODO:
-- This type should be something like: `SignedKES (KES crypto) SigPayload`
newtype SigKesSignature = SigKesSignature { getSigKesSignature :: ByteString }
  deriving stock (Show, Eq)

newtype SigOpCertificate = SigOpCertificate { getSigOpCertificate :: ByteString }
  deriving stock (Show, Eq)

-- | Sig type consists of payload and its KES signature.
--
data Sig = SigRaw {
    sigRawPayload      :: SigPayload,
    sigRawKesSignature :: SigKesSignature
  }
  deriving stock (Show, Eq)

data SigPayload = SigPayload {
    sigPayloadId            :: SigId,
    sigPayloadBody          :: SigBody,
    sigPayloadExpiresAt     :: POSIXTime,
    sigPayloadOpCertificate :: SigOpCertificate
  }
  deriving stock (Show, Eq)

-- | A convenient bidirectional pattern synonym for the `Sig` type.
--
pattern Sig
  :: SigId
  -> SigBody
  -> POSIXTime
  -> SigOpCertificate
  -> SigKesSignature
  -> Sig
pattern
    Sig { sigId,
          sigBody,
          sigExpiresAt,
          sigOpCertificate,
          sigKesSignature
        }
    <-
    SigRaw {
      sigRawPayload =
        SigPayload {
          sigPayloadId            = sigId,
          sigPayloadBody          = sigBody,
          sigPayloadExpiresAt     = sigExpiresAt,
          sigPayloadOpCertificate = sigOpCertificate
        },
      sigRawKesSignature = sigKesSignature
    }
  where
    Sig sigPayloadId
        sigPayloadBody
        sigPayloadExpiresAt
        sigPayloadOpCertificate
        sigRawKesSignature
      =
      SigRaw {
        sigRawPayload =
          SigPayload {
            sigPayloadId,
            sigPayloadBody,
            sigPayloadExpiresAt,
            sigPayloadOpCertificate
          },
        sigRawKesSignature
      }
{-# COMPLETE Sig #-}


instance ShowProxy Sig where
  showProxy _ = "Sig"


type SigSubmission = TxSubmission2.TxSubmission2 SigId Sig
