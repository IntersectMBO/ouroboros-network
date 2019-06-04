{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract Verifiable Random Functions.
module Ouroboros.Consensus.Crypto.VRF.Class
  ( VRFAlgorithm (..)
  , CertifiedVRF(..)
  , evalCertified
  , verifyCertified
  ) where

import           Codec.Serialise (Serialise (..))
import           Codec.Serialise.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)
import           GHC.Stack
import           Numeric.Natural

class ( Show (VerKeyVRF v)
      , Ord (VerKeyVRF v)
      , Show (SignKeyVRF v)
      , Ord (SignKeyVRF v)
      , Show (CertVRF v)
      , Ord (CertVRF v)
      , Serialise (CertVRF v)
      )
      => VRFAlgorithm v where

    data VerKeyVRF v :: *
    data SignKeyVRF v :: *
    data CertVRF v :: *

    maxVRF :: proxy v -> Natural
    genKeyVRF :: MonadRandom m => m (SignKeyVRF v)
    deriveVerKeyVRF :: SignKeyVRF v -> VerKeyVRF v
    evalVRF :: (MonadRandom m, HasCallStack)
            => (a -> Encoding) -> a -> SignKeyVRF v -> m (Natural, CertVRF v)
    verifyVRF :: HasCallStack
              => (a -> Encoding) -> VerKeyVRF v -> a -> (Natural, CertVRF v) -> Bool

data CertifiedVRF v a = CertifiedVRF {
      certifiedNatural :: Natural
    , certifiedProof   :: CertVRF v
    }
  deriving Generic

deriving instance VRFAlgorithm v => Show (CertifiedVRF v a)
deriving instance VRFAlgorithm v => Eq   (CertifiedVRF v a)
deriving instance VRFAlgorithm v => Ord  (CertifiedVRF v a)

instance VRFAlgorithm v => Serialise (CertifiedVRF v a) where
  -- Use generic instance for now

evalCertified :: (VRFAlgorithm v, MonadRandom m)
              => (a -> Encoding) -> a -> SignKeyVRF v -> m (CertifiedVRF v a)
evalCertified toEnc a key = uncurry CertifiedVRF <$> evalVRF toEnc a key

verifyCertified :: (VRFAlgorithm v)
                => (a -> Encoding) -> VerKeyVRF v -> a -> CertifiedVRF v a -> Bool
verifyCertified toEnc vk a CertifiedVRF{..} = verifyVRF toEnc vk a (certifiedNatural, certifiedProof)
