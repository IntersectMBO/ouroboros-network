{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}

-- DUPLICATE -- adapted from: cardano-api/src/Cardano/Api/OperationalCertificate.hs

-- | Operational certificates
--
module Cardano.Api.OperationalCertificate (
    --  OperationalCertIssueError (..)
    OperationalCertificate (..)
  , OperationalCertificateIssueCounter (..)
  , Shelley.KESPeriod (..)
  , getHotKey
  , getKesPeriod
  , getOpCertCount
    -- , issueOperationalCertificate
    -- * Data family instances
  , AsType (..)
  ) where

import           Data.Word

import qualified Cardano.Ledger.Binary as CBOR (CBORGroup (..), shelleyProtVer,
                     toPlainDecoder, toPlainEncoding)
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Protocol.TPraos.OCert as Shelley

import           Cardano.Api.Any
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseTextEnvelope


-- ----------------------------------------------------------------------------
-- Operational certificates
--

data OperationalCertificate =
     OperationalCertificate
       !(Shelley.OCert StandardCrypto)
       !(VerificationKey StakePoolKey)
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR

data OperationalCertificateIssueCounter =
     OperationalCertificateIssueCounter
       { opCertIssueCount   :: !Word64
       , opCertIssueColdKey :: !(VerificationKey StakePoolKey) -- For consistency checking
       }
  deriving (Eq, Show)
  deriving anyclass SerialiseAsCBOR


instance ToCBOR OperationalCertificate where
    toCBOR = CBOR.toPlainEncoding CBOR.shelleyProtVer . encCBOR

instance FromCBOR OperationalCertificate where
    fromCBOR = CBOR.toPlainDecoder CBOR.shelleyProtVer decCBOR

instance ToCBOR OperationalCertificateIssueCounter where
    toCBOR = CBOR.toPlainEncoding CBOR.shelleyProtVer . encCBOR

instance FromCBOR OperationalCertificateIssueCounter where
    fromCBOR = CBOR.toPlainDecoder CBOR.shelleyProtVer decCBOR

instance EncCBOR OperationalCertificate where
    encCBOR (OperationalCertificate ocert vkey) =
      encCBOR (CBOR.CBORGroup ocert, vkey)

instance DecCBOR OperationalCertificate where
    decCBOR = do
      (CBOR.CBORGroup ocert, vkey) <- decCBOR
      return (OperationalCertificate ocert vkey)

instance EncCBOR OperationalCertificateIssueCounter where
    encCBOR (OperationalCertificateIssueCounter counter vkey) =
      encCBOR (counter, vkey)

instance DecCBOR OperationalCertificateIssueCounter where
    decCBOR = do
      (counter, vkey) <- decCBOR
      return (OperationalCertificateIssueCounter counter vkey)

instance HasTypeProxy OperationalCertificate where
    data AsType OperationalCertificate = AsOperationalCertificate
    proxyToAsType _ = AsOperationalCertificate

instance HasTypeProxy OperationalCertificateIssueCounter where
    data AsType OperationalCertificateIssueCounter = AsOperationalCertificateIssueCounter
    proxyToAsType _ = AsOperationalCertificateIssueCounter

instance HasTextEnvelope OperationalCertificate where
    textEnvelopeType _ = "NodeOperationalCertificate"

getHotKey :: OperationalCertificate -> VerificationKey KesKey
getHotKey (OperationalCertificate cert _) = KesVerificationKey $ Shelley.ocertVkHot cert

getKesPeriod :: OperationalCertificate -> Word
getKesPeriod (OperationalCertificate cert _) = Shelley.unKESPeriod $ Shelley.ocertKESPeriod cert

getOpCertCount :: OperationalCertificate -> Word64
getOpCertCount (OperationalCertificate cert _) = Shelley.ocertN cert
