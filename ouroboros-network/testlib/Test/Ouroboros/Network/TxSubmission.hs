module Test.Ouroboros.Network.TxSubmission (tests) where

import Test.Ouroboros.Network.TxSubmission.Common qualified as Common
import Test.Ouroboros.Network.TxSubmission.TxSubmissionV1 qualified as V1
import Test.Ouroboros.Network.TxSubmission.TxSubmissionV2 qualified as V2

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission"
  [ Common.tests
  , V1.tests
  , V2.tests
  ]
