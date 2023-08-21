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
module Cardano.KESAgent.Serialization
where

import Cardano.KESAgent.OCert
import Cardano.KESAgent.TextEnvelope

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class
import Cardano.Binary (ToCBOR (..), FromCBOR (..))

import Data.Proxy
import Data.Typeable
import Data.Word (Word64)

-- * Stake pool keys (\'Cold\' keys)

newtype ColdVerKey c =
  ColdVerKey { coldVerKey :: VerKeyDSIGN (DSIGN c) }

deriving newtype instance (Typeable c, ToCBOR (VerKeyDSIGN (DSIGN c))) => ToCBOR (ColdVerKey c)
deriving newtype instance (Typeable c, FromCBOR (VerKeyDSIGN (DSIGN c))) => FromCBOR (ColdVerKey c)
deriving newtype instance (Typeable c, Eq (VerKeyDSIGN (DSIGN c))) => Eq (ColdVerKey c)
deriving newtype instance (Typeable c, Show (VerKeyDSIGN (DSIGN c))) => Show (ColdVerKey c)

instance ( Typeable c
         , ToCBOR (VerKeyDSIGN (DSIGN c))
         , FromCBOR (VerKeyDSIGN (DSIGN c))
         , DSIGNAlgorithm (DSIGN c)
         ) => HasTextEnvelope (ColdVerKey c) where
  getTEType _ = "StakePoolVerificationKey_" ++ algorithmNameDSIGN (Proxy @(DSIGN c))
  getTEDescription _ = "Cold verification key for stake pools"

newtype ColdSignKey c =
  ColdSignKey { coldSignKey :: SignKeyDSIGN (DSIGN c) }

deriving newtype instance (Typeable c, ToCBOR (SignKeyDSIGN (DSIGN c))) => ToCBOR (ColdSignKey c)
deriving newtype instance (Typeable c, FromCBOR (SignKeyDSIGN (DSIGN c))) => FromCBOR (ColdSignKey c)
deriving newtype instance (Typeable c, Eq (SignKeyDSIGN (DSIGN c))) => Eq (ColdSignKey c)
deriving newtype instance (Typeable c, Show (SignKeyDSIGN (DSIGN c))) => Show (ColdSignKey c)

instance ( Typeable c
         , ToCBOR (SignKeyDSIGN (DSIGN c))
         , FromCBOR (SignKeyDSIGN (DSIGN c))
         , DSIGNAlgorithm (DSIGN c)
         ) => HasTextEnvelope (ColdSignKey c) where
  getTEType _ = "StakePoolSigningKey_" ++ algorithmNameDSIGN (Proxy @(DSIGN c))
  getTEDescription _ = "Cold signing key for stake pools"

-- * KES keys
-- NB: No types are provided for serializing KES sign keys, because the whole
-- purpose of KES Agent is to never write KES sign keys to disk.

newtype KESVerKey c =
  KESVerKey { kesVerKey :: VerKeyKES (KES c) }

deriving newtype instance (Typeable c, ToCBOR (VerKeyKES (KES c))) => ToCBOR (KESVerKey c)
deriving newtype instance (Typeable c, FromCBOR (VerKeyKES (KES c))) => FromCBOR (KESVerKey c)
deriving newtype instance (Typeable c, Eq (VerKeyKES (KES c))) => Eq (KESVerKey c)
deriving newtype instance (Typeable c, Show (VerKeyKES (KES c))) => Show (KESVerKey c)

instance ( Typeable c
         , ToCBOR (VerKeyKES (KES c))
         , FromCBOR (VerKeyKES (KES c))
         , KESAlgorithm (KES c)
         ) => HasTextEnvelope (KESVerKey c) where
  getTEType _ = "KesVerificationKey_" ++ algorithmNameKES (Proxy @(KES c))
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

