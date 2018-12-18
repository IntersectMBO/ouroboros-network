{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ouroboros.Consensus.Util.Orphans () where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Crypto.Random

import           Test.QuickCheck (Arbitrary (..), Gen)

import           Ouroboros.Network.Chain (Chain (..))
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Crypto.DSIGN.Class (DSIGNAlgorithm (..))
import           Ouroboros.Consensus.Crypto.Hash.Class (Hash,
                     HashAlgorithm (..), hash)
import           Ouroboros.Consensus.Crypto.VRF.Class (VRFAlgorithm (..))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random (withSeed)

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where

    arbitrary = do
        seed <- arbitrary
        return $ withSeed seed genKeyDSIGN

    shrink = const []

instance (Serialise a, Arbitrary a, HashAlgorithm h) => Arbitrary (Hash h a) where

    arbitrary = hash <$> arbitrary
    shrink = const []

instance DSIGNAlgorithm v => Arbitrary (VerKeyDSIGN v) where
    arbitrary = deriveVerKeyDSIGN <$> arbitrary
    shrink = const []

instance DSIGNAlgorithm v => Arbitrary (SigDSIGN v) where

    arbitrary = do
        a    <- arbitrary :: Gen Int
        sk   <- arbitrary
        seed <- arbitrary
        return $ withSeed seed $ signDSIGN a sk

    shrink = const []

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

instance MonadRandom m => MonadRandom (IdentityT m) where
     getRandomBytes = lift . getRandomBytes
