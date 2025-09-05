{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module DMQ.Protocol.SigSubmission.Type
  ( -- * Data types
    SigHash (..)
  , SigId (..)
  , SigBody (..)
  , SigKESSignature (..)
  , SigOpCertificate (..)
  , SigColdKey (..)
  , SigRaw (..)
  , SigRawWithSignedBytes (..)
  , Sig (Sig, SigWithBytes, sigRawWithSignedBytes, sigRawBytes, sigId, sigBody, sigExpiresAt, sigOpCertificate, sigKESPeriod, sigKESSignature, sigColdKey, sigSignedBytes, sigBytes)
  , validateSig
    -- * `TxSubmission` mini-protocol
  , SigSubmission
  , module SigSubmission
  , POSIXTime
    -- * Utilities
  , CBORBytes (..)
    -- * Re-exports from `kes-agent`
  , KESPeriod (..)
  ) where

import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS.Base16
import Data.ByteString.Base16.Lazy as LBS.Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable
import Data.Word (Word64)

import Cardano.Crypto.DSIGN.Class (ContextDSIGN, DSIGNAlgorithm, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..), Signable)
import Cardano.KESAgent.KES.Crypto as KES
import Cardano.KESAgent.KES.Evolution qualified as KES
import Cardano.KESAgent.KES.OCert (KESPeriod (..), OCert (..), OCertSignable,
           validateOCert)

import Ouroboros.Network.Protocol.TxSubmission2.Type as SigSubmission hiding
           (TxSubmission2)
import Ouroboros.Network.Protocol.TxSubmission2.Type as TxSubmission2
import Ouroboros.Network.Util.ShowProxy


newtype SigHash = SigHash { getSigHash :: ByteString }
  deriving stock (Show, Eq, Ord)

newtype SigId = SigId { getSigId :: SigHash }
  deriving stock (Show, Eq, Ord)

instance ToJSON SigId where
  toJSON (SigId (SigHash bs)) =
    String (Text.decodeUtf8Lenient . BS.Base16.encode $ bs)

instance ShowProxy SigId where

newtype SigBody = SigBody { getSigBody :: ByteString }
  deriving stock (Show, Eq)


newtype SigKESSignature crypto = SigKESSignature { getSigKESSignature :: SigKES (KES crypto) }

deriving instance Show (SigKES (KES crypto))
               => Show (SigKESSignature crypto)
deriving instance Eq (SigKES (KES crypto))
               => Eq (SigKESSignature crypto)

newtype SigOpCertificate crypto = SigOpCertificate { getSigOpCertificate :: OCert crypto }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  )
                => Show (SigOpCertificate crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  ) => Eq (SigOpCertificate crypto)


newtype SigColdKey crypto = SigColdKey { getSigColdKey :: VerKeyDSIGN (KES.DSIGN crypto) }

deriving instance Show (VerKeyDSIGN (KES.DSIGN crypto))
               => Show (SigColdKey crypto)

deriving instance Eq (VerKeyDSIGN (KES.DSIGN crypto))
               => Eq (SigColdKey crypto)

-- | Sig type consists of payload and its KES signature.
--
-- TODO: add signed bytes.
data SigRaw crypto = SigRaw {
    sigRawId            :: SigId,
    sigRawBody          :: SigBody,
    sigRawKESPeriod     :: KESPeriod,
    -- ^ KES period when this signature was created.
    --
    -- NOTE: `kes-agent` library is using `Word` for KES period, CIP-137
    -- requires `Word64`, thus we're only supporting 64-bit architectures.
    sigRawOpCertificate :: SigOpCertificate crypto,
    sigRawColdKey       :: SigColdKey crypto,
    sigRawExpiresAt     :: POSIXTime,
    sigRawKESSignature  :: SigKESSignature crypto
    -- ^ KES signature of all previous fields.
    --
    -- NOTE: this field must be lazy, otetherwise tests will fail.
  }

deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Show (VerKeyKES (KES crypto))
                  , Show (SigKES (KES crypto))
                  )
               => Show (SigRaw crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  , Eq (SigKES (KES crypto))
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
                  , Show (SigKES (KES crypto))
                  )
               => Show (SigRawWithSignedBytes crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  , Eq (SigKES (KES crypto))
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
                  , Show (SigKES (KES crypto))
                  )
               => Show (Sig crypto)
deriving instance ( DSIGNAlgorithm (KES.DSIGN crypto)
                  , Eq (VerKeyKES (KES crypto))
                  , Eq (SigKES (KES crypto))
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
  -> SigKESSignature crypto
  -> KESPeriod
  -> SigOpCertificate crypto
  -> SigColdKey crypto
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


data SigValidationError =
    InvalidKESSignature KESPeriod KESPeriod String
  | InvalidSignatureOCERT
      !Word64    -- OCert counter
      !KESPeriod -- OCert KES period
      !String    -- DSIGN error message
  | KESBeforeStartOCERT KESPeriod KESPeriod
  | KESAfterEndOCERT KESPeriod KESPeriod
  deriving Show

validateSig :: forall crypto.
               ( Crypto crypto
               , ContextDSIGN (KES.DSIGN crypto) ~ ()
               , DSIGN.Signable (DSIGN crypto) (OCertSignable crypto)
               , ContextKES (KES crypto) ~ ()
               , Signable (KES crypto) ByteString
               )
            => KES.EvolutionConfig
            -> Sig crypto
            -> Either SigValidationError ()
validateSig _ec
            Sig { sigSignedBytes = signedBytes,
                  sigKESPeriod,
                  sigOpCertificate = SigOpCertificate ocert@OCert {
                      ocertKESPeriod,
                      ocertVkHot,
                      ocertN
                  },
                  sigColdKey = SigColdKey coldKey,
                  sigKESSignature = SigKESSignature kesSig
                }
            = do
            sigKESPeriod < endKESPeriod
              ?!  KESAfterEndOCERT endKESPeriod sigKESPeriod
            sigKESPeriod >= startKESPeriod
              ?!  KESBeforeStartOCERT startKESPeriod sigKESPeriod

            -- validate OCert, which includes verifying its signature
            validateOCert coldKey ocertVkHot ocert
              ?!: InvalidSignatureOCERT ocertN sigKESPeriod
            -- validate KES signature of the payload
            verifyKES () ocertVkHot
                         (unKESPeriod sigKESPeriod - unKESPeriod startKESPeriod)
                         (LBS.toStrict signedBytes)
                         kesSig
              ?!: InvalidKESSignature ocertKESPeriod sigKESPeriod
  where
    startKESPeriod, endKESPeriod :: KESPeriod

    startKESPeriod = ocertKESPeriod
    -- TODO: is `totalPeriodsKES` the same as `praosMaxKESEvo`
    -- or `sgMaxKESEvolution` in the genesis file?
    endKESPeriod   = KESPeriod $ unKESPeriod startKESPeriod
                               + totalPeriodsKES (Proxy :: Proxy (KES crypto))

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


--
-- Utility functions
--

(?!:) :: Either e1 a -> (e1 -> e2) -> Either e2 a
(?!:) = flip first

infix 1 ?!:

(?!) :: Bool -> e -> Either e ()
(?!) True  _ = Right ()
(?!) False e = Left e

infix 1 ?!
