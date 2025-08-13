{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKesSignature (..)
  , SigOpCertificate (..)
  , SigRaw (..)
  , Sig (Sig, SigWithBytes, sigRaw, sigRawBytes, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKesSignature, sigBytes)
  , validateSig
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  ) where

import Control.Monad.Class.MonadTime
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm)
import Cardano.Crypto.KES.Class (VerKeyKES)
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.Evolution qualified as KES
import Cardano.KESAgent.KES.OCert (OCert)

import Ouroboros.Network.Protocol.TxSubmission2.Type as SigSubmission hiding
           (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import Ouroboros.Network.Util.ShowProxy


newtype SigHash = SigHash { getSigHash :: ByteString }
  deriving stock (Show, Eq, Ord)

newtype SigId = SigId { getSigId :: SigHash }
  deriving stock (Show, Eq, Ord)

instance ShowProxy SigId where

newtype SigBody = SigBody { getSigBody :: ByteString }
  deriving stock (Show, Eq)


-- TODO:
-- This type should be something like: `SignedKES (KES crypto) SigBody`.
newtype SigKesSignature = SigKesSignature { getSigKesSignature :: ByteString }
  deriving stock (Show, Eq)

-- TODO:
-- This type should be more than just a `ByteString`.
newtype SigOpCertificate crypto = SigOpCertificate { getSigOpCertificate :: OCert crypto }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
                => Show (SigOpCertificate crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  ) => Eq   (SigOpCertificate crypto)


-- | Sig type consists of payload and its KES signature.
--
data SigRaw crypto = SigRaw {
    sigRawId            :: SigId,
    sigRawBody          :: SigBody,
    sigRawExpiresAt     :: POSIXTime,
    sigRawOpCertificate :: SigOpCertificate crypto,
    sigRawKesSignature  :: SigKesSignature
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
               => Show (SigRaw crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  )
               => Eq (SigRaw crypto)


data Sig crypto = SigWithBytes {
    sigRawBytes :: LBS.ByteString,
    sigRaw      :: SigRaw crypto
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
               => Show (Sig crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  )
               => Eq (Sig crypto)

-- | A convenient bidirectional pattern synonym for the `Sig` type.
--
pattern Sig
  :: SigId
  -> SigBody
  -> POSIXTime
  -> SigOpCertificate crypto
  -> SigKesSignature
  -> LBS.ByteString
  -> Sig crypto
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

instance Typeable crypto => ShowProxy (Sig crypto) where


data SigValidationError = InvalidKESSignature
  deriving Show

validateSig :: UTCTime
            -> KES.EvolutionConfig
            -> Sig crypto
            -> Either SigValidationError ()
validateSig now evolutionConfig _sig = Right ()
  where
    period = KES.getCurrentKESPeriodAt now evolutionConfig


type SigSubmission crypto = TxSubmission2.TxSubmission2 SigId (Sig crypto)
