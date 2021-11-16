module Main (main) where

import           Test.Tasty

import qualified Test.Ouroboros.Network.Testing.Data.AbsBearerInfo as AbsBearerInfo

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-testing"
  [ AbsBearerInfo.tests
  ]
