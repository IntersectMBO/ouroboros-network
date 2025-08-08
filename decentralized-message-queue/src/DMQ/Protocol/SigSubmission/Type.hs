{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKesSignature (..)
  , SigOpCertificate (..)
  , SigRaw (..)
  , Sig (Sig, SigWithBytes, sigRaw, sigRawBytes, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKesSignature, sigBytes)
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
-- This type should be something like: `SignedKES (KES crypto) SigBody`.
newtype SigKesSignature = SigKesSignature { getSigKesSignature :: ByteString }
  deriving stock (Show, Eq)

-- TODO:
-- This type should be more than just a `ByteString`.
newtype SigOpCertificate = SigOpCertificate { getSigOpCertificate :: ByteString }
  deriving stock (Show, Eq)

-- | Sig type consists of payload and its KES signature.
--
data SigRaw = SigRaw {
    sigRawId            :: SigId,
    sigRawBody          :: SigBody,
    sigRawExpiresAt     :: POSIXTime,
    sigRawOpCertificate :: SigOpCertificate,
    sigRawKesSignature  :: SigKesSignature
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
  -> POSIXTime
  -> SigOpCertificate
  -> SigKesSignature
  -> LBS.ByteString
  -> Sig
pattern
    Sig { sigId,
          sigBody,
          sigExpiresAt,
          sigOpCertificate,
          sigKesSignature,
          sigBytes
        }
    <-
    SigWithBytes {
      sigRawBytes = sigBytes,
      sigRaw =
        SigRaw {
          sigRawId            = sigId,
          sigRawBody          = sigBody,
          sigRawExpiresAt     = sigExpiresAt,
          sigRawOpCertificate = sigOpCertificate,
          sigRawKesSignature  = sigKesSignature
        }
      }
  where
    Sig sigRawId
        sigRawBody
        sigRawExpiresAt
        sigRawOpCertificate
        sigRawKesSignature
        sigRawBytes
      =
      SigWithBytes {
        sigRawBytes = sigRawBytes,
        sigRaw      =
          SigRaw {
            sigRawId,
            sigRawBody,
            sigRawExpiresAt,
            sigRawOpCertificate,
            sigRawKesSignature
          }
      }
{-# COMPLETE Sig #-}


instance ShowProxy Sig where
  showProxy _ = "Sig"


type SigSubmission = TxSubmission2.TxSubmission2 SigId Sig
