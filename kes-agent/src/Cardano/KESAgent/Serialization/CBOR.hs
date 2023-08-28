{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Newtype wrappers used for serializing various things to/from on-disk
-- files.
module Cardano.KESAgent.Serialization.CBOR
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Serialization.TextEnvelope

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Binary (ToCBOR (..), FromCBOR (..))

import Data.Proxy
import Data.Typeable
import Data.Word (Word64)

-- * Stake pool keys (\'Cold\' keys)

newtype ColdVerKey d =
  ColdVerKey { coldVerKey :: VerKeyDSIGN d }

deriving newtype instance (Typeable d, ToCBOR (VerKeyDSIGN d)) => ToCBOR (ColdVerKey d)
deriving newtype instance (Typeable d, FromCBOR (VerKeyDSIGN d)) => FromCBOR (ColdVerKey d)
deriving newtype instance (Typeable d, Eq (VerKeyDSIGN d)) => Eq (ColdVerKey d)
deriving newtype instance (Typeable d, Show (VerKeyDSIGN d)) => Show (ColdVerKey d)

instance ( Typeable d
         , ToCBOR (VerKeyDSIGN d)
         , FromCBOR (VerKeyDSIGN d)
         , DSIGNAlgorithm d
         ) => HasTextEnvelope (ColdVerKey d) where
  getTEType _ = "StakePoolVerificationKey_" ++ algorithmNameDSIGN (Proxy @d)
  getTEDescription _ = "Cold verification key for stake pools"

newtype ColdSignKey d =
  ColdSignKey { coldSignKey :: SignKeyDSIGN d }

deriving newtype instance (Typeable d, ToCBOR (SignKeyDSIGN d)) => ToCBOR (ColdSignKey d)
deriving newtype instance (Typeable d, FromCBOR (SignKeyDSIGN d)) => FromCBOR (ColdSignKey d)
deriving newtype instance (Typeable d, Eq (SignKeyDSIGN d)) => Eq (ColdSignKey d)
deriving newtype instance (Typeable d, Show (SignKeyDSIGN d)) => Show (ColdSignKey d)

instance ( Typeable d
         , ToCBOR (SignKeyDSIGN d)
         , FromCBOR (SignKeyDSIGN d)
         , DSIGNAlgorithm d
         ) => HasTextEnvelope (ColdSignKey d) where
  getTEType _ = "StakePoolSigningKey_" ++ algorithmNameDSIGN (Proxy @d)
  getTEDescription _ = "Cold signing key for stake pools"

-- * KES keys
-- NB: No types are provided for serializing KES sign keys, because the whole
-- purpose of KES Agent is to never write KES sign keys to disk.

newtype KESVerKey k =
  KESVerKey { kesVerKey :: VerKeyKES k }

deriving newtype instance (Typeable k, ToCBOR (VerKeyKES k)) => ToCBOR (KESVerKey k)
deriving newtype instance (Typeable k, FromCBOR (VerKeyKES k)) => FromCBOR (KESVerKey k)
deriving newtype instance (Typeable k, Eq (VerKeyKES k)) => Eq (KESVerKey k)
deriving newtype instance (Typeable k, Show (VerKeyKES k)) => Show (KESVerKey k)

instance ( Typeable k
         , ToCBOR (VerKeyKES k)
         , FromCBOR (VerKeyKES k)
         , KESAlgorithm k
         ) => HasTextEnvelope (KESVerKey k) where
  getTEType _ = "KesVerificationKey_" ++ algorithmNameKES (Proxy @k)
  getTEDescription _ = "KES verification"

-- * Operational Certificates

-- | Wrapper newtype that gives us the correct 'ToCBOR' / 'FromCBOR' instances
-- for the signature contained in an OpCert. 'SignedDSIGN' itself does not
-- provide such instances, and if it did, they might not be the correct ones.
newtype Sigma c = Sigma (SignedDSIGN (DSIGN c) (OCertSignable c))

instance (Typeable c, Crypto c) => ToCBOR (Sigma c) where
  toCBOR (Sigma x) = encodeSignedDSIGN x

instance (Typeable c, Crypto c) => FromCBOR (Sigma c) where
  fromCBOR = Sigma <$> decodeSignedDSIGN

-- | The operational certificate, as serialized. The serialization format should
-- be compatible with what @cardano-node@ uses.
data OpCert c =
  OpCert
    { opCert :: !(OCert c)
    , opCertColdKey :: !(VerKeyDSIGN (DSIGN c))
    }
deriving instance (Typeable c, DSIGNAlgorithm (DSIGN c), Show (VerKeyKES (KES c)), Show (VerKeyDSIGN (DSIGN c))) => Show (OpCert c)
deriving instance (Typeable c, DSIGNAlgorithm (DSIGN c), Eq (VerKeyKES (KES c)), Eq (VerKeyDSIGN (DSIGN c))) => Eq (OpCert c)

instance ( Typeable c
         , Crypto c
         , ToCBOR (VerKeyKES (KES c))
         , ToCBOR (VerKeyDSIGN (DSIGN c))
         ) => ToCBOR (OpCert c) where
  toCBOR (OpCert (OCert vkHot ocertN kesPeriod sigma) vkey) =
    toCBOR ((vkHot, ocertN, kesPeriod, Sigma sigma), vkey)

instance ( Typeable c
         , Crypto c
         , FromCBOR (VerKeyKES (KES c))
         , FromCBOR (VerKeyDSIGN (DSIGN c))
         ) => FromCBOR (OpCert c) where
  fromCBOR = do
    ((vkHot, ocertN, kesPeriod, Sigma sigma), vkey) <- fromCBOR
    return (OpCert (OCert vkHot ocertN kesPeriod sigma) vkey)

instance ( Typeable c
         , Crypto c
         , FromCBOR (VerKeyKES (KES c))
         , FromCBOR (VerKeyDSIGN (DSIGN c))
         , ToCBOR (VerKeyKES (KES c))
         , ToCBOR (VerKeyDSIGN (DSIGN c))
         , FromCBOR (VerKeyDSIGN (DSIGN c))
         ) => HasTextEnvelope (OpCert c) where
  getTEType _ = "NodeOperationalCertificate"
  getTEDescription _ = "Operational Certificate"


-- * OpCert counter
data OpCertCounter c =
  OpCertCounter
    { opCertIssueCount :: !Word64
    , opCertIssueColdKey :: !(VerKeyDSIGN (DSIGN c))
    }

deriving instance (Typeable c, Eq (VerKeyDSIGN (DSIGN c))) => Eq (OpCertCounter c)
deriving instance (Typeable c, Show (VerKeyDSIGN (DSIGN c))) => Show (OpCertCounter c)

instance (Typeable c, ToCBOR (VerKeyDSIGN (DSIGN c))) => ToCBOR (OpCertCounter c) where
  toCBOR (OpCertCounter counter vkey) =
    toCBOR (counter, vkey)

instance (Typeable c, FromCBOR (VerKeyDSIGN (DSIGN c))) => FromCBOR (OpCertCounter c) where
  fromCBOR = do
    (counter, vkey) <- fromCBOR
    return (OpCertCounter counter vkey)

instance (Typeable c, ToCBOR (VerKeyDSIGN (DSIGN c)), FromCBOR (VerKeyDSIGN (DSIGN c))) => HasTextEnvelope (OpCertCounter c) where
  getTEType _ = "NodeOperationalCertificateIssueCounter"
  getTEDescription _ = "OpCert Counter"

