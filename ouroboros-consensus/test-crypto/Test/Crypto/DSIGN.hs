{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.DSIGN
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Test.QuickCheck (Property, (==>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Network.Serialise (prop_serialise)
import           Ouroboros.Network.Serialise (Serialise)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random (Seed, withSeed)

import           Test.Util.Orphans.Arbitrary ()

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup "Crypto.DSIGN"
  [ testDSIGNAlgorithm (Proxy :: Proxy MockDSIGN)   "MockDSIGN"
  , testDSIGNAlgorithm (Proxy :: Proxy Ed448DSIGN)  "Ed448DSIGN"
  , testDSIGNAlgorithm (Proxy :: Proxy RSAPSSDSIGN) "RSAPSSDSIGN"
  ]

testDSIGNAlgorithm :: forall proxy v. DSIGNAlgorithm v => proxy v -> String -> TestTree
testDSIGNAlgorithm _ n =
    testGroup n
        [ testProperty "serialise VerKey"                 $ prop_serialise @(VerKeyDSIGN v)
        , testProperty "serialise SignKey"                $ prop_serialise @(SignKeyDSIGN v)
        , testProperty "serialise Sig"                    $ prop_serialise @(SigDSIGN v)
        , testProperty "verify positive"                  $ prop_dsign_verify_pos @Int @v
        , testProperty "verify newgative (wrong key)"     $ prop_dsign_verify_neg_key @Int @v
        , testProperty "verify newgative (wrong message)" $ prop_dsign_verify_neg_msg @Int @v
        ]

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
