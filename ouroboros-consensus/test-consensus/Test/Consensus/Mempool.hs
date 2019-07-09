{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Test.Consensus.Mempool (tests) where

import           Data.List (foldl')

import           Test.QuickCheck (Property, Testable, classify, counterexample,
                     tabulate, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM (MonadSTM, STM, atomically)
import           Control.Monad.Class.MonadThrow (MonadMask, MonadThrow)
import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Consensus.Ledger.Abstract (ledgerConfigView)
import           Ouroboros.Consensus.Mempool (Mempool (..),
                     MempoolSnapshot (..), openMempool)
import           Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util.ThreadRegistry (withThreadRegistry)

import           Ouroboros.Network.Chain (Chain (..))

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as Mock

import           Test.Util.TestBlock (GenTx (..), GenTxId (..), TestBlock,
                     computeGenTxId, singleNodeTestConfig, testInitExtLedger)
import           Test.Util.TestTx (TestTx (..))


tests :: TestTree
tests = testGroup "Mempool"
  [ testGroup "TxSeq"
      [ testProperty "lookupByTicketNo"                 prop_TxSeq_lookupByTicketNo
      ]
  , testProperty "getTxs == getTxsAfter zeroIdx"        prop_Mempool_getTxs_getTxsAfter
  , testProperty "addedTxs == getTxs"                   prop_Mempool_addTxs_getTxs
  , testProperty "Invalid transactions are never added" prop_Mempool_InvalidTxsNeverAdded
  ]

{-------------------------------------------------------------------------------
  Mempool Implementation Properties
-------------------------------------------------------------------------------}

-- | Test that @getTxs == getTxsAfter zeroIdx@.
prop_Mempool_getTxs_getTxsAfter :: Chain TestBlock
                                -> [TestTx]
                                -> Property
prop_Mempool_getTxs_getTxsAfter bc txs =
  testAddTxsWithMempoolAndSnapshot
    bc
    txs
    (\Mempool{zeroIdx} MempoolSnapshot{getTxs, getTxsAfter} ->
      getTxs === getTxsAfter zeroIdx)

-- | Test that all valid transactions added to a 'Mempool' can be retrieved
-- afterward.
--
-- n.b. In this test, we add a list of arbitrary transactions to an initially
-- empty 'Mempool', retrieve all of the transactions from the 'Mempool', and
-- assert that those transactions are /specifically/ equal to the /valid/
-- transactions that we attempted to add (the invalid ones should have been
-- dropped).
prop_Mempool_addTxs_getTxs :: Chain TestBlock
                           -> [TestTx]
                           -> Property
prop_Mempool_addTxs_getTxs bc txs =
    testAddTxsWithMempoolAndSnapshot
        bc
        txs
        (\_ MempoolSnapshot{getTxs} ->
          filter (genTxIsValid . snd) (testTxsToGenTxPairs txs)
              === dropThd getTxs)
  where
    dropThd :: [(a, b, c)] -> [(a, b)]
    dropThd = map (\(a, b, _) -> (a, b))
    --
    genTxIsValid :: GenTx TestBlock -> Bool
    genTxIsValid (TestGenTx (ValidTestTx _)) = True
    genTxIsValid _                           = False

-- | Test that invalid transactions can never be added to the 'Mempool'.
--
-- n.b. In this test, we add an arbitrary list of transactions to an initially
-- empty 'Mempool', retrieve all of the transactions from the 'Mempool', and
-- assert that none of those transactions are invalid.
prop_Mempool_InvalidTxsNeverAdded :: Chain TestBlock
                                  -> [TestTx]
                                  -> Property
prop_Mempool_InvalidTxsNeverAdded bc txs =
    testAddTxsWithMempoolAndSnapshot
        bc
        txs
        (\_ MempoolSnapshot{getTxs} ->
          filter (\(_, tx, _) -> genTxIsInvalid tx) getTxs === [])
  where
    genTxIsInvalid :: GenTx TestBlock -> Bool
    genTxIsInvalid (TestGenTx (InvalidTestTx _)) = True
    genTxIsInvalid _                             = False

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

testTxsToGenTxPairs :: [TestTx] -> [(GenTxId TestBlock, GenTx TestBlock)]
testTxsToGenTxPairs = map testTxToGenTxPair

testTxToGenTxPair :: TestTx -> (GenTxId TestBlock, GenTx TestBlock)
testTxToGenTxPair tx = (computeGenTxId genTx, genTx)
  where
    genTx = TestGenTx tx

testAddTxsWithMempoolAndSnapshot
  :: Testable prop
  => Chain TestBlock
  -> [TestTx]
  -> (   forall m.
         Mempool m TestBlock TicketNo
      -> MempoolSnapshot (GenTxId TestBlock) (GenTx TestBlock) TicketNo
      -> prop
     )
  -> Property
testAddTxsWithMempoolAndSnapshot bc txs prop =
  tabulate "Transactions" (map constrName txs) $
  runSimOrThrow $
  withOpenMempool bc $ \mempool -> do
    let genTxs = testTxsToGenTxPairs txs
        Mempool
          { addTxs
          , getSnapshot
          } = mempool
    invalidTxs <- addTxs genTxs
    snapshot@MempoolSnapshot{getTxs} <- atomically getSnapshot
    pure $
      counterexampleMempoolInfo genTxs invalidTxs getTxs $
      classifyMempoolSize getTxs $
      prop mempool snapshot

openDB :: forall m.
          ( MonadSTM m
          , MonadFork m
          , MonadMask m
          , MonadThrow m
          , MonadThrow (STM m)
          , MonadAsync m
          )
       => m (ChainDB m TestBlock)
openDB = Mock.openDB singleNodeTestConfig testInitExtLedger

withOpenMempool :: ( MonadSTM m
                   , MonadFork m
                   , MonadMask m
                   , MonadThrow m
                   , MonadThrow (STM m)
                   , MonadAsync m
                   )
                => Chain TestBlock
                -> (Mempool m TestBlock TicketNo -> m a)
                -> m a
withOpenMempool bc action = do
  chainDb <- ChainDB.fromChain openDB bc
  withThreadRegistry $ \registry -> do
    let cfg = ledgerConfigView singleNodeTestConfig
    action =<< openMempool registry chainDb cfg

-- | Classify whether we're testing against an empty or non-empty 'Mempool' in
-- each test case.
classifyMempoolSize :: Testable prop => [a] -> prop -> Property
classifyMempoolSize txs prop =
  classify (null txs)         "empty Mempool"     $
  classify (not . null $ txs) "non-empty Mempool" $
  prop

counterexampleMempoolInfo :: ( Show a
                             , Show b
                             , Show c
                             , Testable prop
                             )
                          => [a]
                          -> [b]
                          -> [c]
                          -> prop
                          -> Property
counterexampleMempoolInfo txsToAdd invalidTxs txsInMempool prop =
  counterexample
  ("Transactions to add: "         <> show txsToAdd   <> "\n" <>
   "Invalid transactions: "        <> show invalidTxs <> "\n" <>
   "Transactions in the Mempool: " <> show txsInMempool) $
  prop

-- TODO: Should be replaced by solution for issue #709.
constrName :: Show a => a -> String
constrName = head . words . show

{-------------------------------------------------------------------------------
  TxSeq
-------------------------------------------------------------------------------}

prop_TxSeq_lookupByTicketNo :: [Int] -> Bool
prop_TxSeq_lookupByTicketNo xs =
    and [ case TxSeq.lookupByTicketNo txseq tn of
            Just tx' -> tx == tx'
            Nothing  -> False
        | (tx, tn) <- TxSeq.fromTxSeq txseq ]
  where
    txseq :: TxSeq Int
    txseq = foldl' (TxSeq.:>) TxSeq.Empty
                   (zipWith TxTicket xs (map TicketNo [0..]))

