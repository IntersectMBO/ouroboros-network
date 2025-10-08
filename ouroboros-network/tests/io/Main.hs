module Main (main) where

import Main.Utf8 (withUtf8)
import Test.Tasty

import Test.Ouroboros.Network.Pipe qualified (tests)
import Test.Ouroboros.Network.Socket qualified (tests)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network:io-tests"

  [ Test.Ouroboros.Network.Pipe.tests
  , Test.Ouroboros.Network.Socket.tests
  ]
