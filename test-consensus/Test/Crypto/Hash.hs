{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.Hash
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Network.Serialise

--
-- The list of all tests
--
tests :: TestTree
tests =
  testGroup "Crypto.Hash"
  [ testHashAlgorithm (Proxy :: Proxy MD5)    "MD5"
  , testHashAlgorithm (Proxy :: Proxy SHA256) "SHA256"
  ]

testHashAlgorithm :: forall proxy h. HashAlgorithm h => proxy h -> String -> TestTree
testHashAlgorithm _ n =
    testGroup n
        [ testProperty "byte count"      $ prop_hash_correct_byteCount @h @String
        , testProperty "serialise"       $ prop_hash_serialise @h
        , testProperty "show/fromString" $ prop_hash_show_fromString @h @Double
        ]

prop_hash_serialise :: HashAlgorithm h => Hash h Int -> Property
prop_hash_serialise = prop_serialise
