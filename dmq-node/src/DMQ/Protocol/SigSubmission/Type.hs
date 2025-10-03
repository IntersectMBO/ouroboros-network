{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKESSignature (..)
  , SigKESPeriod
  , SigOpCertificate (..)
  , SigColdKey (..)
  , SigRaw (..)
  , SigRawWithSignedBytes (..)
  , Sig (Sig, SigWithBytes, sigRawWithSignedBytes, sigRawBytes, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKESPeriod, sigKESSignature, sigColdKey, sigSignedBytes, sigBytes)
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  , POSIXTime
    -- * Utilities
  , CBORBytes (..)
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16.Lazy as LBS.Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm)
import Cardano.Crypto.KES.Class (VerKeyKES)
-- import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.OCert (OCert (..))

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
-- This type should be something like: `SignedKES (KES crypto) SigPayload`
newtype SigKESSignature = SigKESSignature { getSigKESSignature :: ByteString }
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


type SigKESPeriod = Word

newtype SigColdKey = SigColdKey { getSigColdKey :: ByteString }
  deriving stock (Show, Eq)

-- | Sig type consists of payload and its KES signature.
--
-- TODO: add signed bytes.
data SigRaw crypto = SigRaw {
    sigRawId            :: SigId,
    sigRawBody          :: SigBody,
    sigRawKESPeriod     :: SigKESPeriod,
    -- ^ KES period when this signature was created.
    --
    -- NOTE: `kes-agent` library is using `Word` for KES period, CIP-137
    -- requires `Word64`, thus we're only supporting 64-bit architectures.
    sigRawExpiresAt     :: POSIXTime,
    sigRawKESSignature  :: SigKESSignature,
    sigRawOpCertificate :: SigOpCertificate crypto,
    sigRawColdKey       :: SigColdKey
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
               => Show (SigRaw crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  )
               => Eq (SigRaw crypto)

instance Crypto crypto
      => ToJSON (SigRaw crypto) where
  -- TODO: it is too verbose, we need verbosity levels for these JSON fields
  toJSON SigRaw { sigRawId
             {- , sigRawBody
                , sigRawKESPeriod
                , sigRawExpiresAt
                , sigRawKESSignature
                , sigRawOpCertificate
                , sigRawColdKey -}
                } =
    object [ "id"            .= show (getSigHash (getSigId sigRawId))
        {- , "body"          .= show (getSigBody sigRawBody)
           , "kesPeriod"     .= sigRawKESPeriod
           , "expiresAt"     .= show sigRawExpiresAt
           , "kesSignature"  .= show (getSigKESSignature sigRawKESSignature)

           , "opCertificate" .= show (getSignableRepresentation signable)
           , "coldKey"       .= show (getSigColdKey sigRawColdKey) -}
           ]
        {-
        where
          ocert    = getSigOpCertificate sigRawOpCertificate
          signable :: OCertSignable crypto
          signable = OCertSignable (ocertVkHot ocert) (ocertN ocert) (ocertKESPeriod ocert)
        -}

data SigRawWithSignedBytes crypto = SigRawWithSignedBytes {
    sigRawSignedBytes :: LBS.ByteString,
    -- ^ bytes signed by the KES key
    sigRaw            :: SigRaw crypto
    -- ^ the `SigRaw` data type
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
               => Show (SigRawWithSignedBytes crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  )
               => Eq (SigRawWithSignedBytes crypto)

instance Crypto crypto
      => ToJSON (SigRawWithSignedBytes crypto) where
  toJSON SigRawWithSignedBytes {sigRaw} = toJSON sigRaw

data Sig crypto = SigWithBytes {
    sigRawBytes           :: LBS.ByteString,
    -- ^ encoded `SigRaw` data type
    sigRawWithSignedBytes :: SigRawWithSignedBytes crypto
    -- ^ the `SigRaw` data type along with signed bytes
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
               => Show (Sig crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  )
               => Eq (Sig crypto)

instance Crypto crypto
      => ToJSON (Sig crypto) where
  toJSON SigWithBytes {sigRawWithSignedBytes} = toJSON sigRawWithSignedBytes

-- | A convenient bidirectional pattern synonym for the `Sig` type.
--
pattern Sig
  :: SigId
  -> SigBody
  -> SigKESSignature
  -> SigKESPeriod
  -> SigOpCertificate crypto
  -> SigColdKey
  -> POSIXTime
  -> LBS.ByteString
  -> LBS.ByteString
  -> Sig crypto
pattern
    Sig { sigId,
          sigBody,
          sigKESSignature,
          sigKESPeriod,
          sigOpCertificate,
          sigColdKey,
          sigExpiresAt,
          sigSignedBytes,
          sigBytes
        }
    <-
    SigWithBytes {
      sigRawBytes = sigBytes,
      sigRawWithSignedBytes =
        SigRawWithSignedBytes {
          sigRawSignedBytes = sigSignedBytes,
          sigRaw = SigRaw {
            sigRawId            = sigId,
            sigRawBody          = sigBody,
            sigRawKESSignature  = sigKESSignature,
            sigRawKESPeriod     = sigKESPeriod,
            sigRawOpCertificate = sigOpCertificate,
            sigRawColdKey       = sigColdKey,
            sigRawExpiresAt     = sigExpiresAt
          }
        }
      }
  where
    Sig sigRawId
        sigRawBody
        sigRawKESSignature
        sigRawKESPeriod
        sigRawOpCertificate
        sigRawColdKey
        sigRawExpiresAt
        sigRawSignedBytes
        sigRawBytes
      =
      SigWithBytes {
        sigRawBytes = sigRawBytes,
        sigRawWithSignedBytes = SigRawWithSignedBytes {
          sigRawSignedBytes,
          sigRaw = SigRaw {
            sigRawId,
            sigRawBody,
            sigRawKESPeriod,
            sigRawKESSignature,
            sigRawOpCertificate,
            sigRawColdKey,
            sigRawExpiresAt
          }
        }
      }
{-# COMPLETE Sig #-}

instance Typeable crypto => ShowProxy (Sig crypto) where

type SigSubmission crypto = TxSubmission2.TxSubmission2 SigId (Sig crypto)


--
-- Utilities
--

-- | A newtype wrapper to show CBOR bytes in hex format.
--
newtype CBORBytes = CBORBytes { getCBORBytes :: LBS.ByteString }
  deriving Eq

instance Show CBORBytes where
  show = LBS.Char8.unpack . LBS.Base16.encode . getCBORBytes
