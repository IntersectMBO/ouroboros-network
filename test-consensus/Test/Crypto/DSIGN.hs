{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Crypto.DSIGN
    ( tests
    ) where

import           Data.Proxy (Proxy (..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Network.Serialise (prop_serialise)

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
