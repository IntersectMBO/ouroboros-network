module Test.DMQ.Protocol.LocalMsgSubmission where

import Ouroboros.Network.Protocol.LocalTxSubmission.Test as LocalTxSubmission
import Test.Tasty


tests :: TestTree
tests = testGroup "DMQ.Protocol"
          [testGroup "LocalMsgSubmission" [LocalTxSubmission.tests]]
