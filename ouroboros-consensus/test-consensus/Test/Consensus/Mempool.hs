{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Test.Consensus.Mempool (tests) where

import           Data.List (find, foldl', sort)

import           Test.QuickCheck (Property, Testable, classify, counterexample,
                     tabulate, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadSTM (MonadSTM, STM, atomically)
import           Control.Monad.Class.MonadThrow (MonadMask, MonadThrow)
import           Control.Monad.IOSim (SimM, runSimOrThrow, runSimTrace,
                     selectTraceEventsDynamic, traceM)

import           Control.Tracer (Tracer (..), nullTracer)

import           Data.Maybe (isJust)
import           Data.Typeable (Typeable)

import qualified Ouroboros.Network.Chain as Chain

import           Ouroboros.Consensus.Ledger.Abstract (ledgerConfigView)
import           Ouroboros.Consensus.Mempool (Mempool (..),
                     MempoolSnapshot (..), TraceEventMempool (..), openMempool)
import           Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util.ThreadRegistry (withThreadRegistry)

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Storage.ChainDB.Mock as Mock

import           Test.Util.TestBlock (BlockChain, GenTx (..), GenTxId (..),
                     TestBlock, chainToBlocks, computeGenTxId,
                     singleNodeTestConfig, testInitExtLedger)
import           Test.Util.TestTx (TestTx (..))


tests :: TestTree
tests = testGroup "Mempool"
  [ testGroup "TxSeq"
      [ testProperty "lookupByTicketNo"                 prop_TxSeq_lookupByTicketNo
      ]
  , testProperty "getTxs == getTxsAfter zeroIdx"        prop_Mempool_getTxs_getTxsAfter
  , testProperty "addedTxs == getTxs"                   prop_Mempool_addTxs_getTxs
  , testProperty "Invalid transactions are never added" prop_Mempool_InvalidTxsNeverAdded
  , testProperty "Added valid transactions are traced"  prop_Mempool_TraceValidTxs
  , testProperty "Rejected invalid txs are traced"      prop_Mempool_TraceRejectedTxs
  ]

{-------------------------------------------------------------------------------
  Mempool Implementation Properties
-------------------------------------------------------------------------------}

-- | Test that @getTxs == getTxsAfter zeroIdx@.
prop_Mempool_getTxs_getTxsAfter :: BlockChain
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
prop_Mempool_addTxs_getTxs :: BlockChain
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
prop_Mempool_InvalidTxsNeverAdded :: BlockChain
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

-- | Test that all valid transactions added to a 'Mempool' via 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceValidTxs :: BlockChain
                           -> [TestTx]
                           -> Property
prop_Mempool_TraceValidTxs bc txs =
    testAddTxsWithTrace bc txs traceProp
  where
    traceProp :: [GenTx TestBlock] -> [TraceEventMempool TestBlock] -> Property
    traceProp genTxs es =
        let addedTxs = maybe
                []
                (\(TraceMempoolAddTxs ts _) -> ts)
                (find isAddTxsEvent es)
        in sort (filter genTxIsValid genTxs) === sort addedTxs
    --
    genTxIsValid :: GenTx TestBlock -> Bool
    genTxIsValid (TestGenTx (ValidTestTx _)) = True
    genTxIsValid _                           = False
    --
    isAddTxsEvent :: TraceEventMempool blk -> Bool
    isAddTxsEvent (TraceMempoolAddTxs _ _) = True
    isAddTxsEvent _                        = False

-- | Test that all invalid rejected transactions returned from 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceRejectedTxs :: BlockChain
                              -> [TestTx]
                              -> Property
prop_Mempool_TraceRejectedTxs bc txs =
    testAddTxsWithTrace bc txs traceProp
  where
    traceProp :: [GenTx TestBlock] -> [TraceEventMempool TestBlock] -> Property
    traceProp genTxs es =
        let rejectedTxs = maybe
                []
                (\(TraceMempoolRejectedTxs ts _) -> ts)
                (find isRejectedTxsEvent es)
        in sort (filter genTxIsInvalid genTxs) === sort rejectedTxs
    --
    genTxIsInvalid :: GenTx TestBlock -> Bool
    genTxIsInvalid (TestGenTx (InvalidTestTx _)) = True
    genTxIsInvalid _                             = False
    --
    isRejectedTxsEvent :: TraceEventMempool blk -> Bool
    isRejectedTxsEvent (TraceMempoolRejectedTxs _ _) = True
    isRejectedTxsEvent _                             = False

-- TODO: Need to add a property which also tests 'TraceRemovedTxs' traces.
--       However, the 'TestTx' implementation must be extended before we can
--       do so as there is currently no way for 'ValidTestTx' transactions in
--       the 'Mempool' to become invalid and, therefore, removed from the
--       'Mempool'.

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
  => BlockChain
  -> [TestTx]
  -> (   forall m.
         Mempool m TestBlock TicketNo
      -> MempoolSnapshot TestBlock TicketNo
      -> prop
     )
  -> Property
testAddTxsWithMempoolAndSnapshot bc txs prop =
  tabulate "Transactions" (map constrName txs) $
  runSimOrThrow $
  withOpenMempool bc nullTracer $ \mempool -> do
    let genTxs = map TestGenTx txs
        Mempool
          { addTxs
          , getSnapshot
          } = mempool
    attemptedTxs <- addTxs genTxs
    let invalidTxs = map (isJust . snd) attemptedTxs
    snapshot@MempoolSnapshot{getTxs} <- atomically getSnapshot
    pure $
      counterexampleMempoolInfo genTxs invalidTxs getTxs $
      classifyMempoolSize getTxs $
      prop mempool snapshot

testAddTxsWithTrace
  :: Testable prop
  => BlockChain
  -> [TestTx]
  -> ([GenTx TestBlock] -> [TraceEventMempool TestBlock] -> prop)
  -> Property
testAddTxsWithTrace bc txs prop = do
  let genTxs = map TestGenTx txs
      trace  = selectTraceEventsDynamic $
               runSimTrace $
               withOpenMempool bc dynamicTracer $ \mempool -> do
                 let Mempool{addTxs} = mempool
                 addTxs genTxs
  counterexample ("\nTrace:\n" ++ unlines (map show trace)) $
    prop genTxs trace

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
                => BlockChain
                -> Tracer m (TraceEventMempool TestBlock)
                -> (Mempool m TestBlock TicketNo -> m a)
                -> m a
withOpenMempool bc tracer action = do
  chainDb <- ChainDB.fromChain openDB (Chain.fromOldestFirst (chainToBlocks bc))
  withThreadRegistry $ \registry -> do
    let cfg = ledgerConfigView singleNodeTestConfig
    action =<< openMempool registry chainDb cfg tracer

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

dynamicTracer :: Typeable a => Tracer (SimM s) a
dynamicTracer = Tracer traceM

{-------------------------------------------------------------------------------
  TxSeq Properties
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
