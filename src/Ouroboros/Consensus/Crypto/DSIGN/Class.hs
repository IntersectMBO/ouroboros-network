{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Abstract digital signatures.
module Ouroboros.Consensus.Crypto.DSIGN.Class
    ( DSIGNAlgorithm (..)
    , SignedDSIGN
    , signedDSIGN
    , verifySignedDSIGN
    , prop_dsign_verify_pos
    , prop_dsign_verify_neg_key
    , prop_dsign_verify_neg_msg
    ) where

import           Crypto.Random (MonadRandom)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util
import           Test.QuickCheck (Arbitrary (..), Gen, Property, (==>))

import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Network.Serialise

class ( Show (VerKeyDSIGN v)
      , Ord (VerKeyDSIGN v)
      , Serialise (VerKeyDSIGN v)
      , Show (SignKeyDSIGN v)
      , Ord (SignKeyDSIGN v)
      , Serialise (SignKeyDSIGN v)
      , Show (SigDSIGN v)
      , Condense (SigDSIGN v)
      , Ord (SigDSIGN v)
      , Serialise (SigDSIGN v)
      )
      => DSIGNAlgorithm v where

    data VerKeyDSIGN v :: *
    data SignKeyDSIGN v :: *
    data SigDSIGN v :: *

    genKeyDSIGN :: MonadRandom m => m (SignKeyDSIGN v)
    deriveVerKeyDSIGN :: SignKeyDSIGN v -> VerKeyDSIGN v
    signDSIGN :: (MonadRandom m, Serialise a) => a -> SignKeyDSIGN v -> m (SigDSIGN v)
    verifyDSIGN :: Serialise a => VerKeyDSIGN v -> a -> SigDSIGN v -> Bool

newtype SignedDSIGN v a = SignedDSIGN (SigDSIGN v)
  deriving (Generic)

deriving instance DSIGNAlgorithm v => Show (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Eq   (SignedDSIGN v a)
deriving instance DSIGNAlgorithm v => Ord  (SignedDSIGN v a)

instance Condense (SigDSIGN v) => Condense (SignedDSIGN v a) where
    condense (SignedDSIGN sig) = condense sig

instance DSIGNAlgorithm v => Serialise (SignedDSIGN v a) where
  -- use Generic instance
  --
signedDSIGN :: (DSIGNAlgorithm v, MonadRandom m, Serialise a)
            => a -> SignKeyDSIGN v -> m (SignedDSIGN v a)
signedDSIGN a key = SignedDSIGN <$> signDSIGN a key

verifySignedDSIGN :: (DSIGNAlgorithm v, Serialise a)
                  => VerKeyDSIGN v -> a -> SignedDSIGN v a -> Bool
verifySignedDSIGN key a (SignedDSIGN s) = verifyDSIGN key a s

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where

    arbitrary = do
        seed <- arbitrary
        return $ withSeed seed genKeyDSIGN

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

prop_dsign_verify_pos :: forall a v. (Serialise a, DSIGNAlgorithm v)
                      => Seed
                      -> a
                      -> SignKeyDSIGN v
                      -> Bool
prop_dsign_verify_pos seed a sk =
    let sig = withSeed seed $ signDSIGN a sk
        vk  = deriveVerKeyDSIGN sk
    in  verifyDSIGN vk a sig

prop_dsign_verify_neg_key :: forall a v. (Serialise a, DSIGNAlgorithm v)
                          => Seed
                          -> a
                          -> SignKeyDSIGN v
                          -> SignKeyDSIGN v
                          -> Property
prop_dsign_verify_neg_key seed a sk sk' = sk /= sk' ==>
    let sig = withSeed seed $ signDSIGN a sk'
        vk  = deriveVerKeyDSIGN sk
    in  not $ verifyDSIGN vk a sig

prop_dsign_verify_neg_msg :: forall a v. (Serialise a, Eq a, DSIGNAlgorithm v)
                          => Seed
                          -> a
                          -> a
                          -> SignKeyDSIGN v
                          -> Property
prop_dsign_verify_neg_msg seed a a' sk = a /= a' ==>
    let sig = withSeed seed $ signDSIGN a sk
        vk  = deriveVerKeyDSIGN sk
    in  not $ verifyDSIGN vk a' sig
