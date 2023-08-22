module Main (main) where

import           Main.Utf8 (withUtf8)
import           Test.Tasty

import qualified Test.Ouroboros.Network.Pipe (tests)
import qualified Test.Ouroboros.Network.Socket (tests)

main :: IO ()
main = withUtf8 $ defaultMain tests

tests :: TestTree
tests =
  testGroup "ouroboros-network:io-tests"

  [ Test.Ouroboros.Network.Pipe.tests
  , Test.Ouroboros.Network.Socket.tests
  ]
