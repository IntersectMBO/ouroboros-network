module Main (main) where

import           Test.Tasty

import qualified Test.Protocol.Codec (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "typed-transitions"
  [ Test.Protocol.Codec.tests
  ]
