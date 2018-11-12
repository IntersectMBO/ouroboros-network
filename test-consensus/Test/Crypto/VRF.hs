{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.VRF
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Crypto.VRF
import           Ouroboros.Network.Serialise (prop_serialise)

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
