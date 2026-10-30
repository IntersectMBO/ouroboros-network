module Test.Ouroboros.Network.TxSubmission (tests) where

import Test.Ouroboros.Network.TxSubmission.AppV1 qualified as AppV1
import Test.Ouroboros.Network.TxSubmission.AppV2 qualified as AppV2
import Test.Ouroboros.Network.TxSubmission.MempoolWriter qualified as MempoolWriter
import Test.Ouroboros.Network.TxSubmission.Outbound qualified as Outbound
import Test.Ouroboros.Network.TxSubmission.SizedBytes qualified as SizedBytes
import Test.Ouroboros.Network.TxSubmission.TxLogic qualified as TxLogic

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Ouroboros.Network.TxSubmission"
  [ AppV1.tests
  , MempoolWriter.tests
  , Outbound.tests
  , SizedBytes.tests
  , TxLogic.tests
  , AppV2.tests
  ]
