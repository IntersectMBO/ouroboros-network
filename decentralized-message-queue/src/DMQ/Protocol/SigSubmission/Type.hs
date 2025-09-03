{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE PatternSynonyms    #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKESSignature (..)
  , SigKESPeriod
  , SigOpCertificate (..)
  , SigColdKey (..)
  , Sig (SigRaw, Sig, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKESSignature, sigKESPeriod, sigColdKey)
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  ) where

import Data.ByteString (ByteString)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32)

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
newtype SigKESSignature = SigKESSignature { getSigKESSignature :: ByteString }
  deriving stock (Show, Eq)

newtype SigOpCertificate = SigOpCertificate { getSigOpCertificate :: ByteString }
  deriving stock (Show, Eq)

type SigKESPeriod = Word32

newtype SigColdKey = SigColdKey { getSigColdKey :: ByteString }
  deriving stock (Show, Eq)

-- | Sig type consists of payload and its KES signature.
--
data Sig = SigRaw {
    sigRawPayload      :: SigPayload,
    sigRawKESSignature :: SigKESSignature
  }
  deriving stock (Show, Eq)

data SigPayload = SigPayload {
    sigPayloadId            :: SigId,
    sigPayloadBody          :: SigBody,
    sigPayloadKESPeriod     :: SigKESPeriod,
    sigPayloadExpiresAt     :: POSIXTime,
    sigPayloadOpCertificate :: SigOpCertificate,
    sigPayloadColdKey       :: SigColdKey
  }
  deriving stock (Show, Eq)

-- | A convenient bidirectional pattern synonym for the `Sig` type.
--
pattern Sig
  :: SigId
  -> SigBody
  -> SigKESSignature
  -> SigKESPeriod
  -> SigOpCertificate
  -> SigColdKey
  -> POSIXTime
  -> Sig
pattern
    Sig { sigId,
          sigBody,
          sigKESSignature,
          sigKESPeriod,
          sigOpCertificate,
          sigColdKey,
          sigExpiresAt
        }
    <-
    SigRaw {
      sigRawPayload =
        SigPayload {
          sigPayloadId            = sigId,
          sigPayloadBody          = sigBody,
          sigPayloadKESPeriod     = sigKESPeriod,
          sigPayloadOpCertificate = sigOpCertificate,
          sigPayloadColdKey       = sigColdKey,
          sigPayloadExpiresAt     = sigExpiresAt
        },
      sigRawKESSignature = sigKESSignature
    }
  where
    Sig sigPayloadId
        sigPayloadBody
        sigRawKESSignature
        sigPayloadKESPeriod
        sigPayloadOpCertificate
        sigPayloadColdKey
        sigPayloadExpiresAt
      =
      SigRaw {
        sigRawPayload =
          SigPayload {
            sigPayloadId,
            sigPayloadBody,
            sigPayloadKESPeriod,
            sigPayloadOpCertificate,
            sigPayloadColdKey,
            sigPayloadExpiresAt
          },
        sigRawKESSignature
      }
{-# COMPLETE Sig #-}


instance ShowProxy Sig where
  showProxy _ = "Sig"


type SigSubmission = TxSubmission2.TxSubmission2 SigId Sig
