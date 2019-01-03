module Main (main) where

import           Test.Tasty

import qualified Test.IOSim (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "IO Sim"
  [ Test.IOSim.tests
  ]
