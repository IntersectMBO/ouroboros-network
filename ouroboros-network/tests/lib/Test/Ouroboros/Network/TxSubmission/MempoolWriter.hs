module Test.Ouroboros.Network.TxSubmission.MempoolWriter (tests) where

import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO)
import Control.Monad.IOSim (runSimOrThrow)

import Ouroboros.Network.Protocol.TxSubmission2.Type (SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Inbound.V1
           (TxSubmissionMempoolWriter (..))

import Test.Ouroboros.Network.TxSubmission.Types

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps, (@?=))


tests :: TestTree
tests = testGroup "MempoolWriter"
  [ testCaseSteps "getMempoolWriter records only valid duplicates" unit_getMempoolWriter_recordsOnlyValidDuplicates
  ]


unit_getMempoolWriter_recordsOnlyValidDuplicates :: (String -> IO ()) -> IO ()
unit_getMempoolWriter_recordsOnlyValidDuplicates step = do
  step "Populate the inbound mempool with one valid tx and submit one invalid duplicate plus one valid duplicate"
  let (accepted, rejected, duplicateTxIds) =
        runSimOrThrow $ do
          duplicateVar <- newTVarIO []
          mempool <- newMempool [mkTx 17 True]
          let writer = getMempoolWriter duplicateVar mempool
          result <- mempoolAddTxs writer [mkTx 17 False, mkTx 17 True]
          duplicates <- readTVarIO duplicateVar
          pure (fst result, snd result, duplicates)

  step "Assert both submissions are rejected as duplicates but only the valid duplicate is recorded for result accounting"
  accepted @?= []
  rejected @?= [(17, DuplicateTx), (17, DuplicateTx)]
  duplicateTxIds @?= [17]
  where
    mkTx :: TxId -> Bool -> Tx TxId
    mkTx txid isValid =
      Tx {
          getTxId = txid,
          getTxSize = SizeInBytes 1,
          getTxAdvSize = SizeInBytes 1,
          getTxValid = isValid
        }
