{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ouroboros.Consensus.Crypto.VRF.NeverUsed (
    NeverVRF
  , VerKeyVRF(..)
  , SignKeyVRF(..)
  , CertVRF(..)
  ) where

import           Codec.Serialise

import           Ouroboros.Consensus.Crypto.VRF.Class

-- | VRF not available
--
-- The type of keys and certificates is isomorphic to unit, but when actually
-- trying to sign or verify something a runtime exception will be thrown.
data NeverVRF

instance VRFAlgorithm NeverVRF where
  data VerKeyVRF  NeverVRF = NeverUsedVerKeyVRF  deriving (Show, Eq, Ord)
  data SignKeyVRF NeverVRF = NeverUsedSignKeyVRF deriving (Show, Eq, Ord)
  data CertVRF    NeverVRF = NeverUsedCertVRF    deriving (Show, Eq, Ord)

  genKeyVRF         = return NeverUsedSignKeyVRF
  deriveVerKeyVRF _ = NeverUsedVerKeyVRF

  maxVRF    = error "VRF unavailable"
  evalVRF   = error "VRF unavailable"
  verifyVRF = error "VRF unavailable"

instance Serialise (CertVRF NeverVRF) where
  encode = \_ -> encode ()
  decode = (\() -> NeverUsedCertVRF) <$> decode
