module Main (main) where

import Main.Utf8 (withUtf8)

import Test.DMQ.NodeToClient qualified
import Test.DMQ.NodeToNode qualified
import Test.DMQ.Protocol.SigSubmission qualified

import Test.Tasty


main :: IO ()
main = withUtf8 $ defaultMain tests


tests :: TestTree
tests =
  testGroup "decentralised-message-queue:tests"
  [ Test.DMQ.NodeToClient.tests
  , Test.DMQ.NodeToNode.tests
  , Test.DMQ.Protocol.SigSubmission.tests
  ]
