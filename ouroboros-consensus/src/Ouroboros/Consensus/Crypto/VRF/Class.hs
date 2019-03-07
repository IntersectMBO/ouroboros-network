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

import           Codec.Serialise (Serialise)
import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)
import           Numeric.Natural

class ( Show (VerKeyVRF v)
      , Ord (VerKeyVRF v)
      , Serialise (VerKeyVRF v)
      , Show (SignKeyVRF v)
      , Ord (SignKeyVRF v)
      , Serialise (SignKeyVRF v)
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
    evalVRF :: (MonadRandom m, Serialise a) => a -> SignKeyVRF v -> m (Natural, CertVRF v)
    verifyVRF :: Serialise a => VerKeyVRF v -> a -> (Natural, CertVRF v) -> Bool

data CertifiedVRF v a = CertifiedVRF {
      certifiedNatural :: Natural
    , certifiedProof   :: CertVRF v
    }
  deriving Generic

deriving instance VRFAlgorithm v => Show (CertifiedVRF v a)
deriving instance VRFAlgorithm v => Eq   (CertifiedVRF v a)
deriving instance VRFAlgorithm v => Ord  (CertifiedVRF v a)

instance VRFAlgorithm v => Serialise (CertifiedVRF v a) where
  -- use generic instance for now

evalCertified :: (VRFAlgorithm v, MonadRandom m, Serialise a)
              => a -> SignKeyVRF v -> m (CertifiedVRF v a)
evalCertified a key = uncurry CertifiedVRF <$> evalVRF a key

verifyCertified :: (VRFAlgorithm v, Serialise a)
                => VerKeyVRF v -> a -> CertifiedVRF v a -> Bool
verifyCertified vk a CertifiedVRF{..} = verifyVRF vk a (certifiedNatural, certifiedProof)
