module Main (main) where

import           Test.Tasty

import qualified Test.ThreadNet.Util.Tests (tests)
import qualified Test.Util.Split.Tests (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "test-infra"
  [ Test.ThreadNet.Util.Tests.tests
  , Test.Util.Split.Tests.tests
  ]
