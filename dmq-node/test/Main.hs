module Main (main) where

import Main.Utf8 (withUtf8)

import Cardano.Crypto.Libsodium

import Test.DMQ.NodeToClient qualified
import Test.DMQ.NodeToNode qualified
import Test.DMQ.Protocol.LocalMsgNotification qualified
import Test.DMQ.Protocol.LocalMsgSubmission qualified
import Test.DMQ.Protocol.SigSubmission qualified

import Test.Tasty


main :: IO ()
main = do
    sodiumInit
    withUtf8 $ defaultMain tests


tests :: TestTree
tests =
  testGroup "decentralised-message-queue:tests"
  [ Test.DMQ.NodeToClient.tests
  , Test.DMQ.NodeToNode.tests

    -- protocols
  , Test.DMQ.Protocol.SigSubmission.tests
  , Test.DMQ.Protocol.LocalMsgSubmission.tests
  , Test.DMQ.Protocol.LocalMsgNotification.tests
  ]
