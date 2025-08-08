{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKESSignature (..)
  , SigOpCertificate (..)
  , SigRaw (..)
  , Sig (Sig, SigWithBytes, sigRaw, sigRawBytes, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKESSignature, sigBytes)
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
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
newtype SigKESSignature = SigKESSignature { getSigKESSignature :: ByteString }
  deriving stock (Show, Eq)

-- TODO:
-- This type should be more than just a `ByteString`.
newtype SigOpCertificate = SigOpCertificate { getSigOpCertificate :: ByteString }
  deriving stock (Show, Eq)

-- | Sig type consists of payload and its KES signature.
--
-- TODO: add signed bytes.
data SigRaw = SigRaw {
    sigRawId            :: SigId,
    sigRawBody          :: SigBody,
    sigRawKESSignature  :: SigKESSignature,
    sigRawOpCertificate :: SigOpCertificate,
    sigRawExpiresAt     :: POSIXTime
  }
  deriving stock (Show, Eq)

data Sig = SigWithBytes {
    sigRawBytes :: LBS.ByteString,
    sigRaw      :: SigRaw
  }
  deriving stock (Show, Eq)

-- | A convenient bidirectional pattern synonym for the `Sig` type.
--
pattern Sig
  :: SigId
  -> SigBody
  -> SigKESSignature
  -> SigOpCertificate
  -> POSIXTime
  -> LBS.ByteString
  -> Sig
pattern
    Sig { sigId,
          sigBody,
          sigKESSignature,
          sigOpCertificate,
          sigExpiresAt,
          sigBytes
        }
    <-
    SigWithBytes {
      sigRawBytes = sigBytes,
      sigRaw =
        SigRaw {
          sigRawId            = sigId,
          sigRawBody          = sigBody,
          sigRawKESSignature  = sigKESSignature,
          sigRawOpCertificate = sigOpCertificate,
          sigRawExpiresAt     = sigExpiresAt
        }
      }
  where
    Sig sigRawId
        sigRawBody
        sigRawKESSignature
        sigRawOpCertificate
        sigRawExpiresAt
        sigRawBytes
      =
      SigWithBytes {
        sigRawBytes = sigRawBytes,
        sigRaw      =
          SigRaw {
            sigRawId,
            sigRawBody,
            sigRawKESSignature,
            sigRawOpCertificate,
            sigRawExpiresAt
          }
      }
{-# COMPLETE Sig #-}


instance ShowProxy Sig where
  showProxy _ = "Sig"


type SigSubmission = TxSubmission2.TxSubmission2 SigId Sig
