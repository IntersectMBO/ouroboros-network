module Main (main) where

import           Cardano.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Shelley.Golden (tests)
import qualified Test.Consensus.Shelley.Serialisation (tests)
import qualified Test.ThreadNet.Shelley (tests)

import           Scratch.Shelley (_main)

main :: IO ()
main = sodiumInit >> asTypeOf _main (defaultMainWithIohkNightly tests)

tests :: TestTree
tests =
  testGroup "shelley"
  [ Test.Consensus.Shelley.Golden.tests
  , Test.Consensus.Shelley.Serialisation.tests
  , Test.ThreadNet.Shelley.tests
  ]
