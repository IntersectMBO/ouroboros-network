module Main (main) where

import Test.Tasty

import Test.Ouroboros.Network.Data.AbsBearerInfo.Test qualified as AbsBearerInfo

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network-testing"
  [ AbsBearerInfo.tests
  ]
