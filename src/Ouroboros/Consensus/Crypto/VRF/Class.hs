{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract Verifiable Random Functions.
module Ouroboros.Consensus.Crypto.VRF.Class
  ( VRFAlgorithm (..)
    -- TODO: Added to Lars' stuff, might need to modify
  , CertifiedVRF(..)
  , evalCertified
  , prop_vrf_max
  , prop_vrf_verify_pos
  , prop_vrf_verify_neg
  ) where

import           Crypto.Random (MonadRandom)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           Numeric.Natural
import           Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample,
                     (==>))

import           Ouroboros.Consensus.Util.Random (Seed, withSeed)
import           Ouroboros.Network.Serialise

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

instance VRFAlgorithm v => Arbitrary (SignKeyVRF v) where

    arbitrary = do
        seed <- arbitrary
        return $ withSeed seed genKeyVRF

    shrink = const []

instance VRFAlgorithm v => Arbitrary (VerKeyVRF v) where
    arbitrary = deriveVerKeyVRF <$> arbitrary
    shrink = const []

instance VRFAlgorithm v => Arbitrary (CertVRF v) where

    arbitrary = do
        a    <- arbitrary :: Gen Int
        sk   <- arbitrary
        seed <- arbitrary
        return $ withSeed seed $ fmap snd $ evalVRF a sk

    shrink = const []

prop_vrf_max :: forall a v. (Serialise a, VRFAlgorithm v)
             => Seed
             -> a
             -> SignKeyVRF v
             -> Property
prop_vrf_max seed a sk =
    let (y, _) = withSeed seed $ evalVRF a sk
        m      = maxVRF (Proxy :: Proxy v)
    in  counterexample ("expected " ++ show y ++ " <= " ++ show m) $ y <= m

prop_vrf_verify_pos :: forall a v. (Serialise a, VRFAlgorithm v)
                    => Seed
                    -> a
                    -> SignKeyVRF v
                    -> Bool
prop_vrf_verify_pos seed a sk =
    let (y, c) = withSeed seed $ evalVRF a sk
        vk     = deriveVerKeyVRF sk
    in  verifyVRF vk a (y, c)

prop_vrf_verify_neg :: forall a v. (Serialise a, VRFAlgorithm v)
                    => Seed
                    -> a
                    -> SignKeyVRF v
                    -> SignKeyVRF v
                    -> Property
prop_vrf_verify_neg seed a sk sk' = sk /= sk' ==>
    let (y, c) = withSeed seed $ evalVRF a sk'
        vk     = deriveVerKeyVRF sk
    in  not $ verifyVRF vk a (y, c)
