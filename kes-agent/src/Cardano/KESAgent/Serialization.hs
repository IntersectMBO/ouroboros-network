{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
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
  getTEType _ = "KESVerificationKey_" ++ algorithmNameKES (Proxy @(KES c))
  getTEDescription _ = "KES verification"

-- * Operational Certificates
-- TODO

-- * OpCert counter
-- TODO
