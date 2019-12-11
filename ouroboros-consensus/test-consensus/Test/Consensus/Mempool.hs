{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Mempool (tests) where

import           Control.Exception (assert)
import           Control.Monad (foldM, forM, forM_, void)
import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (State, evalState, get, modify)
import           Control.Monad.Class.MonadTime (Time (..))
import           Data.List (find, foldl', isSuffixOf, nub, sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Clock (secondsToDiffTime)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.IOSim (runSimOrThrow)

import           Control.Tracer (Tracer (..))

import           Ouroboros.Network.Block (pattern BlockPoint, SlotNo (..),
                     atSlot, withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     Thread, forkThread, withRegistry)

import           Test.Util.Orphans.IOLike ()

import           Test.Consensus.Mempool.TestBlock

tests :: TestTree
tests = testGroup "Mempool"
  [ testGroup "TxSeq"
      [ testProperty "lookupByTicketNo complete"           prop_TxSeq_lookupByTicketNo_complete
      , testProperty "lookupByTicketNo sound"              prop_TxSeq_lookupByTicketNo_sound
      , testProperty "splitAfterTxSize"                    prop_TxSeq_splitAfterTxSize
      ]
  , testProperty "snapshotTxs == snapshotTxsAfter zeroIdx" prop_Mempool_snapshotTxs_snapshotTxsAfter
  , testProperty "valid added txs == getTxs"               prop_Mempool_addTxs_getTxs
  , testProperty "addTxs txs == mapM (addTxs . pure) txs"  prop_Mempool_addTxs_one_vs_multiple
  , testProperty "result of addTxs"                        prop_Mempool_addTxs_result
  , testProperty "Invalid transactions are never added"    prop_Mempool_InvalidTxsNeverAdded
  , testProperty "Mempool capacity implementation"         prop_Mempool_Capacity
  , testProperty "Added valid transactions are traced"     prop_Mempool_TraceValidTxs
  , testProperty "Rejected invalid txs are traced"         prop_Mempool_TraceRejectedTxs
  , testProperty "Removed invalid txs are traced"          prop_Mempool_TraceRemovedTxs
  , testProperty "idx consistency"                         prop_Mempool_idx_consistency
  , testProperty "removeTxs"                               prop_Mempool_removeTxs
  ]

{-------------------------------------------------------------------------------
  Mempool Implementation Properties
-------------------------------------------------------------------------------}

-- | Test that @snapshotTxs == snapshotTxsAfter zeroIdx@.
prop_Mempool_snapshotTxs_snapshotTxsAfter :: TestSetup -> Property
prop_Mempool_snapshotTxs_snapshotTxsAfter setup =
    withTestMempool setup $ \TestMempool { mempool } -> do
      let Mempool { zeroIdx, getSnapshot } = mempool
      MempoolSnapshot { snapshotTxs, snapshotTxsAfter} <- atomically getSnapshot
      return $ snapshotTxs === snapshotTxsAfter zeroIdx

-- | Test that all valid transactions added to a 'Mempool' can be retrieved
-- afterward.
prop_Mempool_addTxs_getTxs :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_getTxs setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      let Mempool { addTxs, getSnapshot } = mempool
      _ <- addTxs (allTxs setup)
      MempoolSnapshot { snapshotTxs } <- atomically getSnapshot
      return $ counterexample (ppTxs (txs setup)) $
        validTxs setup `isSuffixOf` map fst snapshotTxs

-- | Same as 'prop_Mempool_addTxs_getTxs', but add the transactions one-by-one
-- instead of all at once.
prop_Mempool_addTxs_one_vs_multiple :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_one_vs_multiple setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      let Mempool { addTxs, getSnapshot } = mempool
      forM_ (allTxs setup) $ \tx -> addTxs [tx]
      MempoolSnapshot { snapshotTxs } <- atomically getSnapshot
      return $ counterexample (ppTxs (txs setup)) $
        validTxs setup `isSuffixOf` map fst snapshotTxs

-- | Test that the result of adding transaction to a 'Mempool' matches our
-- expectation: invalid transactions have errors associated with them and
-- valid transactions don't.
prop_Mempool_addTxs_result :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_result setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      let Mempool { addTxs } = mempool
      result <- addTxs (allTxs setup)
      return $ counterexample (ppTxs (txs setup)) $
        sort [(tx, isNothing mbErr)     | (tx, mbErr) <- result] ===
        sort [(TestGenTx testTx, valid) | (testTx, valid) <- txs setup]

-- | Test that invalid transactions are never added to the 'Mempool'.
prop_Mempool_InvalidTxsNeverAdded :: TestSetupWithTxs -> Property
prop_Mempool_InvalidTxsNeverAdded setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      let Mempool { addTxs, getSnapshot } = mempool
      txsInMempoolBefore <- map fst . snapshotTxs <$> atomically getSnapshot
      _ <- addTxs (allTxs setup)
      txsInMempoolAfter <- map fst . snapshotTxs <$> atomically getSnapshot
      return $ counterexample (ppTxs (txs setup)) $ conjoin
        -- Check for each transaction in the mempool (ignoring those already
        -- in the mempool beforehand) that it was a valid transaction.
        --
        -- Note that we can't check that no invalid transactions are in the
        -- mempool because the same transaction could be added twice: the
        -- first time as a valid one and the second time as an invalid one.
        [ find (== txInMempool) (validTxs setup) === Just txInMempool
        | txInMempool <- txsInMempoolAfter
        , txInMempool `notElem` txsInMempoolBefore
        ]

-- | After removing a transaction from the Mempool, it's actually gone.
prop_Mempool_removeTxs :: TestSetupWithTxInMempool -> Property
prop_Mempool_removeTxs (TestSetupWithTxInMempool testSetup tx) =
    withTestMempool testSetup $ \TestMempool { mempool } -> do
      let Mempool { removeTxs, getSnapshot } = mempool
      removeTxs [txId txToRemove]
      txsInMempoolAfter <- map fst . snapshotTxs <$> atomically getSnapshot
      return $ counterexample
        ("Transactions in the mempool after removing (" <>
         show txToRemove <> "): " <> show txsInMempoolAfter)
        (txToRemove `notElem` txsInMempoolAfter)
  where
    txToRemove = TestGenTx tx

-- | When the mempool is at capacity, test that 'addTxs' blocks when
-- attempting to add one more transaction and that it unblocks when there is
-- adequate space. Adequate space is made by adding all of the existing
-- transactions to the ledger and removing them from the mempool.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'.
prop_Mempool_Capacity :: MempoolCapTestSetup -> Property
prop_Mempool_Capacity mcts = withTestMempool mctsTestSetup $
    runCapacityTest (map TestGenTx mctsValidTxs)
  where
    MempoolCapTestSetup
      { mctsTestSetup
      , mctsValidTxs
      , mctsCapacity
      } = mcts

    runCapacityTest :: forall m. IOLike m
                    => [GenTx TestBlock]
                    -> TestMempool m
                    -> m Property
    runCapacityTest txs testMempool@TestMempool{getTraceEvents} =
      withRegistry $ \registry -> do
        env@MempoolCapTestEnv
          { mctEnvAddedTxs
          , mctEnvRemovedTxs
          } <- initMempoolCapTestEnv
        void $ forkAddValidTxs  env registry testMempool txs
        void $ forkUpdateLedger env registry testMempool txs

        -- Before we check the order of events, we must block until we've:
        -- * Added all of the transactions to the mempool
        -- * Removed all of the transactions from the mempool
        atomically $ do
          envAdded   <- readTVar mctEnvAddedTxs
          envRemoved <- readTVar mctEnvRemovedTxs
          check $  envAdded   == fromIntegral (length txs)
                && envRemoved == fromIntegral (length txs)

        -- Check the order of events
        events <- getTraceEvents
        pure $ checkTraceEvents events

    -- | Spawn a new thread which continuously attempts to fill the mempool to
    -- capacity until no more transactions remain. This should block whenever
    -- attempting to add more transactions while the mempool is full.
    forkAddValidTxs :: forall m. IOLike m
                    => MempoolCapTestEnv m
                    -> ResourceRegistry m
                    -> TestMempool m
                    -> [GenTx TestBlock]
                    -> m (Thread m ())
    forkAddValidTxs env registry testMempool txs =
      forkThread registry $ addValidTxs env testMempool txs

    -- | Recursively attempt to fill the mempool to capacity until no further
    -- transactions remain. This should block whenever attempting to add more
    -- transactions while the mempool is full.
    addValidTxs :: IOLike m
                => MempoolCapTestEnv m
                -> TestMempool m
                -> [GenTx TestBlock]
                -> m ()
    addValidTxs env testMempool txs = case txs of
      [] -> pure ()
      ts -> do
        let TestMempool{mempool} = testMempool
            Mempool{addTxs}      = mempool

        -- Attempt to fill the mempool to capacity
        let MempoolCapacity mpCap    = mctsCapacity
            (txsToAdd, txsRemaining) = splitAt (fromIntegral mpCap) ts
        _ <- addTxs txsToAdd
        atomically $
          modifyTVar (mctEnvAddedTxs env) (+ (fromIntegral $ length txsToAdd))

        addValidTxs env testMempool txsRemaining

    -- | Spawn a new thread which continuously removes all of the transactions
    -- from the mempool (once it has reached its capacity) and adds the valid
    -- ones to the ledger. This continues until the process has been repeated
    -- for all of the transactions involved in the test.
    forkUpdateLedger :: forall m. IOLike m
                     => MempoolCapTestEnv m
                     -> ResourceRegistry m
                     -> TestMempool m
                     -> [GenTx TestBlock]
                     -> m (Thread m ())
    forkUpdateLedger env registry testMempool txs =
      forkThread registry $ do
        -- First, wait until we've filled the mempool.
        -- After this point, the 'forkAddValidTxs' thread should be blocking on
        -- adding more transactions since the mempool is at capacity.
        atomically $ do
          let MempoolCapacity mpCap = mctsCapacity
          envAdded <- readTVar (mctEnvAddedTxs env)
          check (envAdded == mpCap)

        updateLedger env testMempool txs

    -- | Recursively remove transactions from the mempool and add them to the
    -- ledger. This continues until the process has been repeated for all of
    -- the transactions involved in the test.
    updateLedger :: IOLike m
                 => MempoolCapTestEnv m
                 -> TestMempool m
                 -> [GenTx TestBlock]
                 -> m ()
    updateLedger env testMempool txs = do
      let TestMempool{ mempool, addTxsToLedger } = testMempool
          MempoolCapTestEnv{ mctEnvRemovedTxs }  = env
          Mempool{ getSnapshot, withSyncState }  = mempool

      envRemoved <- atomically (readTVar mctEnvRemovedTxs)
      assert (envRemoved <= fromIntegral (length txs)) $
        if envRemoved == fromIntegral (length txs)
          then
            -- We've synced all of the transactions involved in this test, so
            -- we return.
            pure ()
          else do
            -- We add all of the transactions in the mempool to the ledger.
            -- We do this atomically so that the blocking/retrying 'addTxs'
            -- transaction won't begin syncing and start removing transactions
            -- from the mempool. By ensuring this doesn't happen, we'll get a
            -- simpler and more predictable event trace (which we'll check in
            -- 'checkTraceEvents').
            (snapshotTxs, _errs) <- atomically $ do
              MempoolSnapshot { snapshotTxs } <- getSnapshot
              errs <- addTxsToLedger (map (unTestGenTx . fst) snapshotTxs)
              pure (snapshotTxs, errs)

            -- Sync the mempool with the ledger.
            -- Now all of the transactions in the mempool should have been
            -- removed.
            withSyncState TxsForUnknownBlock (const (return ()))

            -- Indicate that we've removed the transactions from the mempool.
            atomically $ do
              let txsInMempool = map fst snapshotTxs
              envRemoved' <- readTVar mctEnvRemovedTxs
              writeTVar mctEnvRemovedTxs
                        (envRemoved' + fromIntegral (length txsInMempool))

            -- Continue syncing the mempool with the ledger state until we've
            -- removed all of the transactions involved in this test.
            updateLedger env testMempool txs

    checkTraceEvents :: [TraceEventMempool TestBlock]
                     -> Property
    checkTraceEvents events =
          map sortTxsInTrace (mempoolCapTestExpectedTrace mcts)
      === map sortTxsInTrace events

    sortTxsInTrace :: TraceEventMempool TestBlock
                   -> TraceEventMempool TestBlock
    sortTxsInTrace ev = case ev of
      TraceMempoolAddTxs      txs mpSz time -> TraceMempoolAddTxs      (sort txs) mpSz time
      TraceMempoolRemoveTxs   txs mpSz time -> TraceMempoolRemoveTxs   (sort txs) mpSz time
      TraceMempoolRejectedTxs txs mpSz time -> TraceMempoolRejectedTxs (sort txs) mpSz time
      TraceMempoolManuallyRemovedTxs txIds txs mpSz ->
        TraceMempoolManuallyRemovedTxs (sort txIds) (sort txs) mpSz

-- | Test that all valid transactions added to a 'Mempool' via 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceValidTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceValidTxs setup =
    withTestMempool (testSetup setup) $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents } = testMempool
          Mempool { addTxs } = mempool
      _ <- addTxs (allTxs setup)
      evs <- getTraceEvents
      return $ counterexample (ppTxs (txs setup)) $
        let addedTxs = maybe
              []
              (\(TraceMempoolAddTxs txs _ _) -> txs)
              (find isAddTxsEvent evs)
        in sort (validTxs setup)  === sort addedTxs
  where
    isAddTxsEvent :: TraceEventMempool blk -> Bool
    isAddTxsEvent (TraceMempoolAddTxs _ _ _) = True
    isAddTxsEvent _                          = False

-- | Test that all invalid rejected transactions returned from 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceRejectedTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceRejectedTxs setup =
    withTestMempool (testSetup setup) $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents } = testMempool
          Mempool { addTxs } = mempool
      _ <- addTxs (allTxs setup)
      evs <- getTraceEvents
      return $ counterexample (ppTxs (txs setup)) $
        let rejectedTxs = maybe
              []
              (\(TraceMempoolRejectedTxs txsAndErrs _ _) -> map fst txsAndErrs)
              (find isRejectedTxsEvent evs)
        in sort (invalidTxs setup) === sort rejectedTxs
  where
    isRejectedTxsEvent :: TraceEventMempool blk -> Bool
    isRejectedTxsEvent (TraceMempoolRejectedTxs _ _ _) = True
    isRejectedTxsEvent _                               = False

-- | Test that all transactions in the 'Mempool' that have become invalid
-- because of an update to the ledger are appropriately represented in the
-- trace of events.
prop_Mempool_TraceRemovedTxs :: TestSetup -> Property
prop_Mempool_TraceRemovedTxs setup =
    withTestMempool setup $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents, addTxsToLedger } = testMempool
          Mempool { getSnapshot, withSyncState } = mempool
      MempoolSnapshot { snapshotTxs } <- atomically getSnapshot
      -- We add all the transactions in the mempool to the ledger.
      let txsInMempool = map fst snapshotTxs
      errs <- atomically $ addTxsToLedger (map unTestGenTx txsInMempool)

      -- Sync the mempool with the ledger. Now all of the transactions in the
      -- mempool should have been removed.
      withSyncState TxsForUnknownBlock (const (return ()))

      evs  <- getTraceEvents
      -- Also check that 'addTxsToLedger' never resulted in an error.
      return $ map (const (Right ())) errs === errs .&&.
        let removedTxs = maybe
              []
              (\(TraceMempoolRemoveTxs txs _ _) -> txs)
              (find isRemoveTxsEvent evs)
        in sort txsInMempool === sort removedTxs
  where
    isRemoveTxsEvent :: TraceEventMempool blk -> Bool
    isRemoveTxsEvent (TraceMempoolRemoveTxs _ _ _) = True
    isRemoveTxsEvent _                             = False

{-------------------------------------------------------------------------------
  TestSetup: how to set up a TestMempool
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { testLedgerState :: LedgerState TestBlock
  , testInitialTxs  :: [TestTx]
    -- ^ These are all valid and will be the initial contents of the Mempool.
  , testMempoolCap  :: MempoolCapacity
  } deriving (Show)

ppTestSetup :: TestSetup -> String
ppTestSetup TestSetup { testLedgerState
                      , testInitialTxs
                      , testMempoolCap = MempoolCapacity mpCap
                      } = unlines $
    ["Ledger/chain contains TxIds:"]         <>
    (map condense (tlTxIds testLedgerState)) <>
    ["Initial contents of the Mempool:"]     <>
    (map condense testInitialTxs)            <>
    ["Mempool capacity:"]                    <>
    [condense mpCap]

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions.
--
-- n.b. the generated 'MempoolCapacity' will be of the value
-- @nbInitialTxs + extraCapacity@
genTestSetup :: Int -> Int -> Gen (TestSetup, LedgerState TestBlock)
genTestSetup maxInitialTxs extraCapacity = do
    ledgerSize   <- choose (0, maxInitialTxs)
    nbInitialTxs <- choose (0, maxInitialTxs)
    (_txs1,  ledger1) <- genValidTxs ledgerSize testInitLedger
    ( txs2,  ledger2) <- genValidTxs nbInitialTxs ledger1
    let mpCap     = MempoolCapacity $ fromIntegral (nbInitialTxs + extraCapacity)
        testSetup = TestSetup
          { testLedgerState = ledger1
          , testInitialTxs  = txs2
          , testMempoolCap  = mpCap
          }
    return (testSetup, ledger2)

instance Arbitrary TestSetup where
  arbitrary = sized $ \n -> do
    extraCapacity <- choose (0, n)
    fst <$> genTestSetup n extraCapacity
  shrink TestSetup { testLedgerState
                   , testInitialTxs
                   , testMempoolCap = MempoolCapacity mpCap
                   } =
    -- TODO we could shrink @testLedgerState@ too
    [ TestSetup { testLedgerState
                , testInitialTxs = testInitialTxs'
                , testMempoolCap = MempoolCapacity mpCap'
                }
    | testInitialTxs' <- shrinkList (const []) testInitialTxs
    , mpCap' <- shrinkIntegral mpCap
    , mpCap' > 0
    ]

-- | Generate a number of valid and invalid transactions and apply the valid
-- transactions to the given 'LedgerState'. The transactions along with a
-- 'Bool' indicating whether its valid ('True') or invalid ('False') and the
-- resulting 'LedgerState' are returned.
genTxs :: Int  -- ^ The number of transactions to generate
       -> LedgerState TestBlock
       -> Gen ([(TestTx, Bool)], LedgerState TestBlock)
genTxs = go [] Set.empty
  where
    go txs invalidTxIds n ledger
      | n <= 0 = return (reverse txs, ledger)
      | otherwise = do
          valid <- arbitrary
          if valid
            then do
              validTx <- genValidTx invalidTxIds ledger
              let ledger' = mustBeValid (applyTxToLedger ledger validTx)
              go ((validTx, True):txs) invalidTxIds (n - 1) ledger'
            else do
              invalidTx <- genInvalidTx invalidTxIds ledger
              let invalidTxIds' = Set.insert (testTxId invalidTx) invalidTxIds
              go ((invalidTx, False):txs) invalidTxIds' (n - 1) ledger

mustBeValid :: Except TestTxError (LedgerState TestBlock)
            -> LedgerState TestBlock
mustBeValid ex = case runExcept ex of
  Left _       -> error "impossible"
  Right ledger -> ledger

-- | Generate a number of valid transactions and apply these to the given
-- 'LedgerState'. The transactions and the resulting 'LedgerState' are
-- returned.
genValidTxs :: Int  -- ^ The number of valid transactions to generate
            -> LedgerState TestBlock
            -> Gen ([TestTx], LedgerState TestBlock)
genValidTxs = go []
  where
    go txs n ledger
      | n <= 0 = return (reverse txs, ledger)
      | otherwise = do
          tx <- genValidTx Set.empty ledger
          go (tx:txs) (n - 1) (mustBeValid (applyTxToLedger ledger tx))

genValidTx :: Set TestTxId
              -- ^ Already used for past invalid transactions, these cannot be
              -- reused. Note that the 'TestTxId's of the valid transactions
              -- are recorded in the 'LedgerState'.
           -> LedgerState TestBlock -> Gen TestTx
genValidTx invalidTxIds TestLedger { tlTxIds } =
   ValidTestTx <$> genNewTextTxId
  where
    genNewTextTxId = arbitrary `suchThat` \txid ->
      txid `notElem` tlTxIds && txid `notElem` invalidTxIds

genInvalidTx :: Set TestTxId
                -- ^ Already used for past invalid transactions, these cannot
                -- be reused. Note that the 'TestTxId's of the valid
                -- transactions are recorded in the 'LedgerState'.
             -> LedgerState TestBlock -> Gen TestTx
genInvalidTx invalidTxIds TestLedger { tlTxIds } = frequency
    [ (1, InvalidTestTx <$> genNewTextTxId)
    , (if null tlTxIds then 0 else 1, ValidTestTx <$> elements tlTxIds)
    ]
  where
    genNewTextTxId = arbitrary `suchThat` \txid ->
      txid `notElem` tlTxIds && txid `notElem` invalidTxIds

-- TODO property to check that is never possible for a valid transaction that
-- is in the chain to become invalid afterwards?

applyTxToLedger :: LedgerState TestBlock
                -> TestTx
                -> Except TestTxError (LedgerState TestBlock)
applyTxToLedger = \ledgerState tx ->
    -- We need to change the 'ledgerTipPoint' because that is used to check
    -- whether the ledger state has changed.
    -- TODO: We pretend that the ledger state is "ticked" here; this does not
    -- matter for our test ledger. We should at some point test with a test
    -- ledger in which we chain tick /does/ make a difference; it would be
    -- great if we had tests for instance with test blocks with a TTL.
    -- If we do change that here, however, then the 'updateLedgerTipPoint'
    -- function below is also not acceptable.
    updateLedgerTipPoint <$>
      applyTx LedgerConfig (TestGenTx tx) (TickedLedgerState ledgerState)
  where
    updateLedgerTipPoint (TickedLedgerState ledgerState) = ledgerState
        { tlLastApplied = BlockPoint { withHash = unSlotNo slot', atSlot = slot' } }
      where
        slot' = case ledgerTipSlot ledgerState of
          Origin  -> SlotNo 0
          At slot -> succ slot

{-------------------------------------------------------------------------------
  TestSetupWithTxs
-------------------------------------------------------------------------------}

data TestSetupWithTxs = TestSetupWithTxs
  { testSetup :: TestSetup
  , txs       :: [(TestTx, Bool)]
    -- ^ The 'Bool' indicates whether the transaction is valid
  } deriving (Show)

ppTxs :: [(TestTx, Bool)] -> String
ppTxs txs = unlines $
    ["Transactions:"]      <>
    [ condense tx <> ": " <> if valid then "VALID" else "INVALID"
    | (tx, valid) <- txs]

allTxs :: TestSetupWithTxs -> [GenTx TestBlock]
allTxs = map (TestGenTx . fst) . txs

validTxs :: TestSetupWithTxs -> [GenTx TestBlock]
validTxs = map (TestGenTx . fst) . filter snd . txs

invalidTxs :: TestSetupWithTxs -> [GenTx TestBlock]
invalidTxs = map (TestGenTx . fst) . filter (not . snd) . txs

instance Arbitrary TestSetupWithTxs where
  arbitrary = sized $ \n -> do
    nbTxs <- choose (0, n)
    (testSetup, ledger)  <- genTestSetup n nbTxs
    (txs,      _ledger') <- genTxs nbTxs ledger
    return TestSetupWithTxs { testSetup, txs }
  shrink TestSetupWithTxs { testSetup, txs } =
      [ TestSetupWithTxs { testSetup = testSetup', txs }
      | testSetup' <- shrink testSetup ] <>
      [ TestSetupWithTxs { testSetup, txs = txs' }
      | txs' <- map (revalidate testSetup) .
                shrinkList (const []) .
                map fst $ txs ]

revalidate :: TestSetup -> [TestTx] -> [(TestTx, Bool)]
revalidate TestSetup { testLedgerState, testInitialTxs } =
    go initLedgerState []
  where
    -- The LedgerState after adding the transactions initially in the mempool
    initLedgerState = foldl' (\l tx -> mustBeValid (applyTxToLedger l tx))
                         testLedgerState testInitialTxs

    go ledgerState revalidated txs = case txs of
      []      -> reverse revalidated
      tx:txs' -> case runExcept (applyTxToLedger ledgerState tx) of
        Left _             -> go ledgerState  ((tx, False):revalidated) txs'
        Right ledgerState' -> go ledgerState' ((tx, True):revalidated)  txs'

{-------------------------------------------------------------------------------
  TestSetupWithTxInMempol: a mempool and a transaction that is in the mempool
-------------------------------------------------------------------------------}

-- | A 'TestSetup' along with a transaction that is in the Mempool.
--
-- > 'txInMempool' `elem` 'testInitialTxs' 'testSetup'
data TestSetupWithTxInMempool = TestSetupWithTxInMempool TestSetup TestTx
  deriving (Show)

instance Arbitrary TestSetupWithTxInMempool where
  arbitrary = do
    TestSetupWithTxs { testSetup } <-
      arbitrary `suchThat` (not . null . testInitialTxs . testSetup)
    tx <- elements (testInitialTxs testSetup)
    return $ TestSetupWithTxInMempool testSetup tx
  shrink (TestSetupWithTxInMempool testSetup _tx) =
    [ TestSetupWithTxInMempool testSetup tx'
    | testSetup' <- shrink testSetup
    , not . null . testInitialTxs $ testSetup'
    , tx' <- testInitialTxs testSetup'
    ]

{-------------------------------------------------------------------------------
  TestMempool: a mempool with random contents
-------------------------------------------------------------------------------}

data TestMempool m = TestMempool
  { -- | A mempool with random contents.
    --
    -- Starts out synced with the ledger.
    mempool          :: Mempool m TestBlock TicketNo

    -- | When called, obtains all events traced after opening the mempool at
    -- the given state from oldest-to-newest.
    --
    -- Events traced while setting up the mempool to contain random contents
    -- are not included.
  , getTraceEvents   :: m [TraceEventMempool TestBlock]

    -- | Erase the events traced so far. The return of 'getTraceEvents' will
    -- again be an empty list until another event is traced.
  , eraseTraceEvents :: m ()

    -- | This function can be used to add transactions to the ledger/chain.
    --
    -- Remember to synchronise the mempool afterwards.
  , addTxsToLedger   :: [TestTx] -> STM m [Either TestTxError ()]
  }

withTestMempool
  :: forall prop. Testable prop
  => TestSetup
  -> (forall m. IOLike m => TestMempool m -> m prop)
  -> Property
withTestMempool setup@TestSetup { testLedgerState, testInitialTxs, testMempoolCap } prop =
    counterexample (ppTestSetup setup) $
    classify      (null testInitialTxs)  "empty Mempool"     $
    classify (not (null testInitialTxs)) "non-empty Mempool" $
    runSimOrThrow setUpAndRun
  where
    cfg = ledgerConfigView singleNodeTestConfig

    setUpAndRun :: forall m. IOLike m => m Property
    setUpAndRun = do

      -- Set up the LedgerInterface
      varCurrentLedgerState <- uncheckedNewTVarM testLedgerState
      let ledgerInterface = LedgerInterface
            { getCurrentLedgerState = readTVar varCurrentLedgerState
            }

      -- Set up the Tracer
      varEvents <- uncheckedNewTVarM []
      -- TODO use SimM's dynamicTracer
      let tracer = Tracer $ \ev -> atomically $ modifyTVar varEvents (ev:)

      -- Open the mempool and add the initial transactions
      mempool <- openMempoolWithoutSyncThread ledgerInterface
                                              cfg
                                              testMempoolCap
                                              tracer
      result  <- addTxs mempool (map TestGenTx testInitialTxs)
      whenJust (find (isJust . snd) result) $ \(invalidTx, _) -> error $
        "Invalid initial transaction: " <> condense invalidTx

      -- Clear the trace
      atomically $ writeTVar varEvents []

      -- Apply the property to the 'TestMempool' record
      property <$> prop TestMempool
        { mempool
        , getTraceEvents   = atomically $ reverse <$> readTVar varEvents
        , eraseTraceEvents = atomically $ writeTVar varEvents []
        , addTxsToLedger   = addTxsToLedger varCurrentLedgerState
        }

    addTxToLedger :: forall m. IOLike m
                  => StrictTVar m (LedgerState TestBlock)
                  -> TestTx
                  -> STM m (Either TestTxError ())
    addTxToLedger varCurrentLedgerState tx = do
      ledgerState <- readTVar varCurrentLedgerState
      case runExcept (applyTxToLedger ledgerState tx) of
        Left  e            -> return $ Left e
        Right ledgerState' -> do
          writeTVar varCurrentLedgerState ledgerState'
          return $ Right ()

    addTxsToLedger :: forall m. IOLike m
                   => StrictTVar m (LedgerState TestBlock)
                   -> [TestTx]
                   -> STM m [(Either TestTxError ())]
    addTxsToLedger varCurrentLedgerState txs =
      mapM (addTxToLedger varCurrentLedgerState) txs

{-------------------------------------------------------------------------------
  MempoolCapTestSetup
-------------------------------------------------------------------------------}

data MempoolCapTestSetup = MempoolCapTestSetup
  { mctsTestSetup :: TestSetup
  , mctsValidTxs  :: [TestTx]
  , mctsCapacity  :: MempoolCapacity
  } deriving (Show)

instance Arbitrary MempoolCapTestSetup where
  -- TODO: shrink
  arbitrary = do
    let nbInitialTxs = 0
    nbNewTxs <- choose (2, 1000)
    capacity <- choose (1, nbNewTxs - 1)

    -- In 'genTestSetup', the mempool capacity is calculated as such:
    -- @nbInitialTxs + extraCapacity@
    -- Because 'nbInitialTxs' is 0 in this case, passing that along with
    -- an 'extraCapacity' value of @nbNewTxs - 1@ to 'genTestSetup' guarantees
    -- that our mempool's capacity will be @nbNewTxs - 1@ (which is exactly
    -- what we want for 'prop_Mempool_Capacity').
    (testSetup, ledger) <- genTestSetup nbInitialTxs capacity

    (vtxs, _) <- genValidTxs nbNewTxs ledger
    pure MempoolCapTestSetup { mctsTestSetup = testSetup
                             , mctsValidTxs  = vtxs
                             , mctsCapacity  = MempoolCapacity (fromIntegral capacity)
                             }

-- | Given the 'MempoolCapTestSetup', compute the trace of events which we can
-- expect from a mempool capacity test.
mempoolCapTestExpectedTrace :: MempoolCapTestSetup
                            -> [TraceEventMempool TestBlock]
mempoolCapTestExpectedTrace mcts = go chunks
  where
    MempoolCapTestSetup
      { mctsValidTxs
      , mctsCapacity = MempoolCapacity mempoolCap
      } = mcts

    txs :: [GenTx TestBlock]
    txs = map TestGenTx mctsValidTxs

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs
      | n > 0     = take n xs : chunksOf n (drop n xs)
      | otherwise = error $  "mempoolCapTestExpectedTrace.chunksOf: "
                          <> "n is less than or equal to 0"

    chunks :: [[GenTx TestBlock]]
    chunks = chunksOf (fromIntegral mempoolCap) txs

    go :: [[GenTx TestBlock]] -> [TraceEventMempool TestBlock]
    go []         = []
    go (chunk:cs) = chunkExpectedTrace chunk ++ go cs

    chunkExpectedTrace chunk =
      [ TraceMempoolAddTxs    chunk (fromIntegral $ length chunk) nullTime
      , TraceMempoolRemoveTxs chunk 0 nullTime
      ]

    nullTime :: Time
    nullTime = Time $ secondsToDiffTime 0

{-------------------------------------------------------------------------------
  MempoolCapTestEnv: environment for tests related to mempool capacity
-------------------------------------------------------------------------------}

-- | A data type containing 'StrictTVar's by which the two threads spawned by
-- 'prop_Mempool_Capacity' can coordinate with each other.
data MempoolCapTestEnv m = MempoolCapTestEnv
  { mctEnvAddedTxs   :: StrictTVar m Word
    -- ^ The number of transactions which have been added to the mempool.
  , mctEnvRemovedTxs :: StrictTVar m Word
    -- ^ The number of transactions which have been removed from the mempool
    -- and added to the ledger.
  }

initMempoolCapTestEnv :: IOLike m => m (MempoolCapTestEnv m)
initMempoolCapTestEnv = do
  added   <- uncheckedNewTVarM 0
  removed <- uncheckedNewTVarM 0
  pure $ MempoolCapTestEnv { mctEnvAddedTxs   = added
                           , mctEnvRemovedTxs = removed
                           }

{-------------------------------------------------------------------------------
  TxSeq Properties
-------------------------------------------------------------------------------}

-- | Finds elements in the sequence
prop_TxSeq_lookupByTicketNo_complete :: [Int] -> Bool
prop_TxSeq_lookupByTicketNo_complete xs =
    and [ case TxSeq.lookupByTicketNo txseq tn of
            Just tx' -> tx == tx'
            Nothing  -> False
        | (tx, tn) <- TxSeq.fromTxSeq txseq ]
  where
    txseq :: TxSeq Int
    txseq = TxSeq.toTxSeq $ zip3 xs (map TicketNo [0..]) (repeat 0)

-- | Only finds elements in the sequence
prop_TxSeq_lookupByTicketNo_sound ::
    [Small Int] -> Small Int -> Property
prop_TxSeq_lookupByTicketNo_sound smalls small =
    case TxSeq.lookupByTicketNo txseq (mkTicketNo needle) of
      Just tx' ->
        label "successful hit" $
        counterexample ("needle: " ++ show needle) $
        counterexample ("haystack: " ++ show haystack) $
        tx' === needle
      Nothing  ->
        label "successful miss" $
        property $ needle `Set.notMember` haystack'
  where
    -- an ascending haystack of nonnegatives
    haystack = Set.toAscList haystack'
    haystack' = Set.fromList $ map (abs . getSmall) smalls

    -- a nonnegative needle
    needle = abs (getSmall small)

    -- the identity mapping over haystack
    txseq :: TxSeq Int
    txseq =
        foldl' (TxSeq.:>) TxSeq.Empty $ map mkTicket haystack

    mkTicket x = TxTicket x (mkTicketNo x) 0
    mkTicketNo = TicketNo . toEnum

-- | Test that the 'fst' of the result of 'splitAfterTxSize' only contains
-- 'TxTicket's whose summed up transaction sizes are less than or equal to
-- that of the 'TxSizeInBytes' which the 'TxSeq' was split on.
prop_TxSeq_splitAfterTxSize :: TxSizeSplitTestSetup -> Property
prop_TxSeq_splitAfterTxSize tss =
      property $ txSizeSum (txTickets before) <= tssTxSizeToSplitOn
  where
    TxSizeSplitTestSetup { tssTxSizeToSplitOn } = tss

    (before, _after) = splitAfterTxSize txseq tssTxSizeToSplitOn

    txseq :: TxSeq Int
    txseq = txSizeSplitTestSetupToTxSeq tss

    txSizeSum :: [TxTicket tx] -> TxSizeInBytes
    txSizeSum = sum . map txTicketTxSizeInBytes


{-------------------------------------------------------------------------------
  TxSizeSplitTestSetup
-------------------------------------------------------------------------------}

data TxSizeSplitTestSetup = TxSizeSplitTestSetup
  { tssTxSizes         :: ![TxSizeInBytes]
  , tssTxSizeToSplitOn :: !TxSizeInBytes
  } deriving Show

instance Arbitrary TxSizeSplitTestSetup where
  arbitrary = do
    let txSizeMaxBound = 10 * 1024 * 1024 -- 10MB transaction max bound
    txSizes <- listOf $ choose (1, txSizeMaxBound)
    let totalTxsSize = sum txSizes
    txSizeToSplitOn <- frequency
      [ (1, pure 0)
      , (7, choose (0, totalTxsSize))
      , (1, pure totalTxsSize)
      , (1, choose (totalTxsSize + 1, totalTxsSize + 1000))
      ]
    pure TxSizeSplitTestSetup
      { tssTxSizes = txSizes
      , tssTxSizeToSplitOn = txSizeToSplitOn
      }

  shrink TxSizeSplitTestSetup { tssTxSizes, tssTxSizeToSplitOn } =
    [ TxSizeSplitTestSetup
        { tssTxSizes         = tssTxSizes'
        , tssTxSizeToSplitOn = tssTxSizeToSplitOn'
        }
    | tssTxSizes' <- shrinkList (const []) tssTxSizes
    , tssTxSizeToSplitOn' <- shrinkIntegral tssTxSizeToSplitOn
    ]

-- | Convert a 'TxSizeSplitTestSetup' to a 'TxSeq'.
txSizeSplitTestSetupToTxSeq :: TxSizeSplitTestSetup -> TxSeq Int
txSizeSplitTestSetupToTxSeq TxSizeSplitTestSetup { tssTxSizes } =
  TxSeq.toTxSeq [(0, TicketNo 0, tssTxSize) | tssTxSize <- tssTxSizes]

{-------------------------------------------------------------------------------
  TicketNo Properties
-------------------------------------------------------------------------------}

-- Testing plan:
--
-- * Perform a number of actions: either add a new valid transaction to the
--   Mempool (invalid transactions have no effect on the @idx@s) or remove an
--   existing transaction from the Mempool.
--
-- * Adding a transaction is easy. Removing one is harder: we do this by
--   adding that transaction to the ledger and syncing the Mempool with the
--   ledger. As the transaction is now in the ledger, it is no longer valid
--   and must be removed from the Mempool.
--
-- * After executing each action, check whether the current ticket assignment
--   is still consistent with the expected ticket assignment. The ticket
--   assignment is a mapping from 'TicketNo' (@idx@) to transaction. The same
--   ticket may never be reused for another transaction, which is exactly what
--   we're testing here.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'. This makes it easier to generate 'Actions', because they
-- don't have to take the initial contents of the Mempool and 'LedgerState'
-- into account.
prop_Mempool_idx_consistency :: Actions -> Property
prop_Mempool_idx_consistency (Actions actions) =
    withTestMempool emptyTestSetup $ \testMempool@TestMempool { mempool } ->
      fmap conjoin $ forM actions $ \action -> do
        txsInMempool      <- map (unTestGenTx . fst) . snapshotTxs <$>
                             atomically (getSnapshot mempool)
        actionProp        <- executeAction testMempool action
        currentAssignment <- currentTicketAssignment mempool
        return $
          --  #692, fixed in #742: if the mempool becomes empty during
          -- operation. In this case, the 'TicketNo' counter would "reset" to
          -- 'zeroTicketNo'. Clients interacting with the mempool likely won't
          -- account for this.
          classify
            (Map.null currentAssignment)
            "Mempool became empty" $
          -- #692, fixed in #742: the transaction at the "back" of the mempool
          -- becomes invalid and is removed. In this case, the next
          -- transaction to be appended would take on the 'TicketNo' of the
          -- removed transaction (since this function only increments the
          -- 'TicketNo' associated with the transaction at the back of the
          -- mempool). Clients interacting with the mempool likely won't
          -- account for this.
          classify
            (lastOfMempoolRemoved txsInMempool action)
            "The last transaction in the mempool is removed" $
          actionProp .&&.
          currentAssignment `isConsistentWith` expectedAssignment
  where
    expectedAssignment = expectedTicketAssignment actions

    emptyTestSetup = TestSetup
      { testLedgerState = testInitLedger
      , testInitialTxs  = []
      , testMempoolCap  = MempoolCapacity 1000
      }

    lastOfMempoolRemoved txsInMempool = \case
      AddTx    _  -> False
      RemoveTx tx -> last txsInMempool == tx

    isConsistentWith curAsgn expAsgn
      | curAsgn `Map.isSubmapOf` expAsgn
      = property True
      | otherwise
      = counterexample
        ("Current tickets assignments: "  <> show curAsgn <>
         "\ninconsistent with expected: " <> show expAsgn)
        False

{-------------------------------------------------------------------------------
  TicketAssignment & Actions
-------------------------------------------------------------------------------}

data Action
  = AddTx    TestTx
  | RemoveTx TestTx
  deriving (Show)

newtype Actions = Actions [Action]
  deriving (Show)

-- | Track to which ticket number each transaction is assigned.
--
-- * We don't want multiple transaction to be assigned the same ticket number.
-- * We want each transaction to be always assigned the same ticket number.
type TicketAssignment = Map TicketNo TestTxId

-- | Compute the expected 'TicketAssignment' for the given actions.
expectedTicketAssignment :: [Action] -> TicketAssignment
expectedTicketAssignment actions =
    evalState (foldM addMapping mempty actions) (succ zeroTicketNo)
  where
    addMapping :: TicketAssignment -> Action -> State TicketNo TicketAssignment
    addMapping mapping (RemoveTx _tx) = return mapping
    addMapping mapping (AddTx     tx) = do
      nextTicketNo <- get
      modify succ
      return $ Map.insert nextTicketNo (testTxId tx) mapping

-- | Executes the action and verifies that it is actually executed using the
-- tracer, hence the 'Property' in the return type.
executeAction :: forall m. IOLike m => TestMempool m -> Action -> m Property
executeAction testMempool action = case action of
    AddTx tx -> do
      void $ addTxs [TestGenTx tx]
      expectTraceEvent $ \case
        TraceMempoolAddTxs [TestGenTx tx'] _ _
          | tx == tx'
          -> property True
        _ -> counterexample ("Transaction not added: " <> condense tx) False

    RemoveTx tx -> do
      void $ atomically $ addTxsToLedger [tx]
      -- Synchronise the Mempool with the updated chain
      withSyncState TxsForUnknownBlock $ \_snapshot -> return ()
      expectTraceEvent $ \case
        TraceMempoolRemoveTxs [TestGenTx tx'] _ _
          | tx == tx'
          -> property True
        _ -> counterexample ("Transaction not removed: " <> condense tx) False
  where
    TestMempool
      { mempool
      , eraseTraceEvents
      , getTraceEvents
      , addTxsToLedger
      } = testMempool
    Mempool { addTxs, withSyncState } = mempool

    expectTraceEvent :: (TraceEventMempool TestBlock -> Property) -> m Property
    expectTraceEvent checker = do
      evs <- getTraceEvents
      eraseTraceEvents
      return $ case evs of
        [ev] -> checker ev
        []   -> counterexample "No events traced"       False
        _    -> counterexample "Multiple events traced" False

currentTicketAssignment :: IOLike m
                        => Mempool m TestBlock TicketNo -> m TicketAssignment
currentTicketAssignment Mempool { withSyncState } =
    withSyncState TxsForUnknownBlock $ \MempoolSnapshot { snapshotTxs } ->
      return $ Map.fromList
        [ (ticketNo, testTxId (unTestGenTx tx))
        | (tx, ticketNo) <- snapshotTxs
        ]

instance Arbitrary Actions where
  arbitrary = sized $ \n -> do
      -- Note the use of 'nub' to avoid duplicates, because that would lead to
      -- collisions in the map.
      txsToAdd <- shuffle . nub . fst =<< genValidTxs n testInitLedger
      go n [] txsToAdd []
    where
      go :: Int       -- ^ Number of actions left to generate
         -> [Action]  -- ^ Already generated actions
         -> [TestTx]  -- ^ Transactions that can still be added
         -> [TestTx]  -- ^ Transactions that can still be removed
         -> Gen Actions
      go n actions toAdd toRem = case (toAdd, toRem) of
        _ | n <= 0                   -> return $ Actions (reverse actions)
        ([],           [])           -> return $ Actions (reverse actions)
        ([],           txRem:toRem') -> go (n - 1) (RemoveTx txRem:actions) [txRem] toRem'
        (txAdd:toAdd', [])           -> go (n - 1) (AddTx    txAdd:actions) toAdd'  [txAdd]
        (txAdd:toAdd', txRem:toRem') -> arbitrary >>= \case
          True  -> go (n - 1) (AddTx    txAdd:actions) toAdd' toRem
          False -> go (n - 1) (RemoveTx txRem:actions) toAdd  toRem'
