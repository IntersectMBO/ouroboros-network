module Test.Ouroboros.Network.TxSubmission (tests) where

import Test.Ouroboros.Network.TxSubmission.AppV1 qualified as AppV1
import Test.Ouroboros.Network.TxSubmission.AppV2 qualified as AppV2
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TxLogic

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission"
  [ TxLogic.tests
  , AppV1.tests
  , AppV2.tests
  ]
