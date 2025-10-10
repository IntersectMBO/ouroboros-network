module Main (main) where

import Main.Utf8 (withUtf8)

import Cardano.Crypto.Libsodium

import Test.DMQ.NodeToClient qualified
import Test.DMQ.NodeToNode qualified

import DMQ.Protocol.LocalMsgNotification.Test qualified
import DMQ.Protocol.LocalMsgSubmission.Test qualified
import DMQ.Protocol.SigSubmission.Test qualified

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
  , DMQ.Protocol.SigSubmission.Test.tests
  , DMQ.Protocol.LocalMsgSubmission.Test.tests
  , DMQ.Protocol.LocalMsgNotification.Test.tests
  ]
