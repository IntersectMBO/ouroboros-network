{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Tests fairness aspects of the mempool.
--
-- See 'testTxSizeFairness' for more details on the tests we run in this module.
module Test.Consensus.Mempool.Fairness (
    testTxSizeFairness
  , tests
  ) where

import qualified Cardano.Slotting.Time as Time
import           Control.Arrow ((***))
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Exception (assert)
import           Control.Monad (forever, void)
import qualified Control.Tracer as Tracer
import           Data.Foldable (asum)
import qualified Data.List as List
import           Data.Void (Void, vacuous)
import           Data.Word (Word32)
import           Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Util.IOLike (STM, atomically, retry)
import           System.Random (randomIO)
import           Test.Consensus.Mempool.Fairness.TestBlock
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?), (@?=))
import           Test.Util.TestBlock (testInitLedgerWithState)

tests :: TestTree
tests = testGroup "Mempool fairness"
                  [ testCase "There is no substantial bias in added transaction sizes" $
                              testTxSizeFairness TestParams { mempoolMaxCapacity =   100
                                                            , smallTxSize        =     1
                                                            , largeTxSize        =    10
                                                            , nrOftxsToCollect   = 1_000
                                                            , toleranceThreshold =     0.2 -- Somewhat arbitrarily chosen.
                                                            }
                  ]

type TestMempool = Mempool IO TestBlock Mempool.TicketNo

-- | Test if the mempool treats small and large transactions in the same way.
--
-- We run the following test:
--
-- - Given a mempool 'mp'.
-- - Concurrently:
--     - Run 'N' threads that add small transactions to 'mp'.
--     - Run 'N' threads that add large transactions to 'mp'.
--     - Remove transactions from 'mp' one by one, with a small delay between
--       removals. Collect the removed transactions.
--
-- We give the threads that add small transactions a head start to make sure
-- that the mempool fills up with small transactions. In this way the thread
-- that removes transactions one by one will remove small transactions first.
-- Then, if the mempool is fair, it will not always try to add a small
-- transaction as soon as it can, but it will eventually wait until enough
-- capacity has been freed (by the remover thread) so that a large transaction
-- can be added.
--
-- After collecting 'M' removed transactions, let 'diff' be the difference between
-- the number of small and large transactions that were added to 'mp', then we
-- check that:
--
-- > diff / M <= toleranceThreshold
--
-- See 'TestParams' for an explanation of the different parameters that
-- influence this test.
testTxSizeFairness :: TestParams -> IO ()
testTxSizeFairness TestParams { mempoolMaxCapacity, smallTxSize, largeTxSize, nrOftxsToCollect, toleranceThreshold } = do
    ----------------------------------------------------------------------------
    --  Obtain a mempool.
    ----------------------------------------------------------------------------
    let
      ledgerItf = Mempool.LedgerInterface {
              Mempool.getCurrentLedgerState = pure $ testInitLedgerWithState ()
          }

      sampleLedgerConfig =
          HardFork.defaultEraParams (Consensus.SecurityParam 10) (Time.slotLengthFromSec 2)
    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   sampleLedgerConfig
                   (Mempool.MempoolCapacityBytesOverride
                      (Mempool.MempoolCapacityBytes mempoolMaxCapacity))
                   Tracer.nullTracer
                   genTxSize

    ----------------------------------------------------------------------------
    --  Add and collect transactions
    ----------------------------------------------------------------------------
    let waitForSmallAddersToFillMempool = threadDelay 1_000
    txs <- runConcurrently [
                                           adders  mempool smallTxSize
      , waitForSmallAddersToFillMempool >> adders  mempool largeTxSize
      , waitForSmallAddersToFillMempool >> remover mempool             nrOftxsToCollect
      ]


    ----------------------------------------------------------------------------
    --  Count the small and large transactions
    ----------------------------------------------------------------------------
    let
      nrSmall :: Double
      nrLarge :: Double
      (nrSmall, nrLarge) = (fromIntegral . length *** fromIntegral . length)
                         $ List.partition  (<= smallTxSize)
                         $ fmap txSize txs
    length txs @?= nrOftxsToCollect
    theRatioOfTheDifferenceBetween nrSmall nrLarge `isBelow` toleranceThreshold
  where
    theRatioOfTheDifferenceBetween x y = (abs (x - y) / (x + y), x, y)

    -- At the end of the tests the proportion of small and large
    -- transactions that were added should be rouhgly the same. We tolerate
    -- the given theshold.
    isBelow (ratioDiff, nrSmall, nrLarge) threshold = ratioDiff <= threshold
      @? (    "The difference between the number of large and small transactions added "
           <> "exeeds the threshold (" <> show threshold <> ")\n"
           <> "Total number of small transactions that were added: " <> show nrSmall <> "\n"
           <> "Total number of large transactions that were added: " <> show nrLarge <> "\n"
           <> "Difference / Total: " <> show ratioDiff
         )

runConcurrently :: [IO a] -> IO a
runConcurrently = Async.runConcurrently . asum . fmap Async.Concurrently

-- | Test parameters.
--
-- When choosing the parameters bear in mind that:
--
-- - The smaller the difference between 'smallTxSize' and 'largeTxSize', the
--   harder it will be detect potential differences in way the mempool handles
--   small and large transactions.
--
-- - The larger the capacity, the higher the chance large transactions can be
--   added before the mempool is saturated.
--
data TestParams = TestParams {
    mempoolMaxCapacity :: Word32
  , smallTxSize        :: Word32
    -- ^ Size of what we consider to be a small transaction.
  , largeTxSize        :: Word32
    -- ^ Size of what we consider to be a large transaction.
  , nrOftxsToCollect   :: Int
    -- ^ How many added transactions we count.
  , toleranceThreshold :: Double
    -- ^ We tolerate a certain ratio between the difference of small and large
    -- transactions added, and the total transactions that were added. For
    -- instance, given a threshold of 0.2, if we measure the sizes of 100 added
    -- transactions, the difference between the number small and large
    -- transactions we counted should not be larger than 20.
  }

-- | Add transactions with the specified size to the mempool.
--
-- We launch a fixed number of adder threads.
--
-- This process does not finish. Hence the 'a' type parameter.
adders ::
     TestMempool
     -- ^ Mempool to which transactions will be added
  -> Word32
     -- ^ Transaction size
  -> IO a
adders mempool fixedTxSize = vacuous $ runConcurrently $ fmap adder [0..2]
  where
    adder :: Int -> IO Void
    adder _i = forever $ do
        -- We pick a random number for the transaction id.
        thisTxId <- randomIO
        void $ Mempool.addTxs mempool [mkGenTx thisTxId fixedTxSize]

-- | Remove the given number of transactions and return them.
--
remover ::
     TestMempool
     -- ^ Mempool to remove transactions from.
  -> Int
  -- ^ Number of transactions to remove.
  -> IO [Tx]
remover mempool total = do
    let result = loop [] total
    removedTxs <- result
    assert (length removedTxs == total) result
  where
    -- Remove transactions one by one till we reach 'n'.
    loop txs 0 = pure txs -- Note that the transactions will come out in reverse
                          -- order wrt the order in which they were added to the
                          -- mempool. That is ok since we only care about the
                          -- transaction sizes.
    loop txs n = do
        -- We wait 1ms to give the other threads the possibility to add
        -- transactions.
        threadDelay 1000
        gtx <- atomically $ getATxFromTheMempool
        Mempool.removeTxs mempool [Mempool.txId gtx]
        loop (unGenTx gtx:txs) (n-1)
      where
        getATxFromTheMempool =
          getTxsInSnapshot mempool >>= \case
              []  -> retry
              x:_ -> pure x

-- TODO: consider moving this to O.C.Mempool.API
getTxsInSnapshot :: TestMempool -> STM IO [Mempool.GenTx TestBlock]
getTxsInSnapshot mempool = fmap txsInSnapshot
                         $ Mempool.getSnapshot mempool
  where
    txsInSnapshot = fmap (Mempool.txForgetValidated . fst)
                  . Mempool.snapshotTxs
