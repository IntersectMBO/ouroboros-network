{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.VRF
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Test.QuickCheck (Property, counterexample, (==>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Consensus.Util.Random (Seed, withSeed)
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.Serialise (Serialise, prop_serialise)

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup "Crypto.VRF"
  [ testVRFAlgorithm (Proxy :: Proxy MockVRF)   "MockVRF"
  , testVRFAlgorithm (Proxy :: Proxy SimpleVRF) "SimpleVRF"
  ]

testVRFAlgorithm :: forall proxy v. VRFAlgorithm v => proxy v -> String -> TestTree
testVRFAlgorithm _ n =
    testGroup n
        [ testProperty "serialise VerKey"  $ prop_serialise @(VerKeyVRF v)
        , testProperty "serialise SignKey" $ prop_serialise @(SignKeyVRF v)
        , testProperty "serialise Cert"    $ prop_serialise @(CertVRF v)
        , testProperty "max"               $ prop_vrf_max @Int @v
        , testProperty "verify positive"   $ prop_vrf_verify_pos @Int @v
        , testProperty "verify negative"   $ prop_vrf_verify_neg @Int @v
        ]

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
