module Main (main) where

import Main.Utf8 (withUtf8)

import Test.DMQ.NodeToNode qualified

import Test.Tasty


main :: IO ()
main = withUtf8 $ defaultMain tests


tests :: TestTree
tests =
  testGroup "decentralised-message-queue:tests"
  [ Test.DMQ.NodeToNode.tests
  ]
