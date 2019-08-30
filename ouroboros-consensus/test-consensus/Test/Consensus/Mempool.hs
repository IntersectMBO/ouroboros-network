{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Mempool (tests) where

import           Control.Monad (foldM, forM, forM_, void)
import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.List (find, foldl', isSuffixOf, nub, sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.IOSim (runSimOrThrow)

import           Control.Tracer (Tracer (..))

import           Ouroboros.Network.Block (pattern BlockPoint, SlotNo (..),
                     atSlot, pointSlot, withHash)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Test.Consensus.Mempool.TestBlock

tests :: TestTree
tests = testGroup "Mempool"
  [ testGroup "TxSeq"
      [ testProperty "lookupByTicketNo complete"           prop_TxSeq_lookupByTicketNo_complete
      , testProperty "lookupByTicketNo sound"              prop_TxSeq_lookupByTicketNo_sound
      ]
  , testProperty "snapshotTxs == snapshotTxsAfter zeroIdx" prop_Mempool_snapshotTxs_snapshotTxsAfter
  , testProperty "valid added txs == getTxs"               prop_Mempool_addTxs_getTxs
  , testProperty "addTxs txs == mapM (addTxs . pure) txs"  prop_Mempool_addTxs_one_vs_multiple
  , testProperty "result of addTxs"                        prop_Mempool_addTxs_result
  , testProperty "Invalid transactions are never added"    prop_Mempool_InvalidTxsNeverAdded
  , testProperty "Added valid transactions are traced"     prop_Mempool_TraceValidTxs
  , testProperty "Rejected invalid txs are traced"         prop_Mempool_TraceRejectedTxs
  , testProperty "Removed invalid txs are traced"          prop_Mempool_TraceRemovedTxs
  , testProperty "idx consistency"                         prop_Mempool_idx_consistency
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
              (\(TraceMempoolAddTxs txs _) -> txs)
              (find isAddTxsEvent evs)
        in sort (validTxs setup)  === sort addedTxs
  where
    isAddTxsEvent :: TraceEventMempool blk -> Bool
    isAddTxsEvent (TraceMempoolAddTxs _ _) = True
    isAddTxsEvent _                        = False

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
              (\(TraceMempoolRejectedTxs txs _) -> txs)
              (find isRejectedTxsEvent evs)
        in sort (invalidTxs setup)  === sort rejectedTxs
  where
    isRejectedTxsEvent :: TraceEventMempool blk -> Bool
    isRejectedTxsEvent (TraceMempoolRejectedTxs _ _) = True
    isRejectedTxsEvent _                             = False

-- | Test that all transactions in the 'Mempool' that have become invalid
-- because of an update to the ledger are appropriately represented in the
-- trace of events.
prop_Mempool_TraceRemovedTxs :: TestSetup -> Property
prop_Mempool_TraceRemovedTxs setup =
    withTestMempool setup $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents, addTxToLedger } = testMempool
          Mempool { getSnapshot, withSyncState } = mempool
      MempoolSnapshot { snapshotTxs } <- atomically getSnapshot
      -- We add all the transactions in the mempool to the ledger.
      let txsInMempool = map fst snapshotTxs
      errs <- mapM (addTxToLedger . unTestGenTx) txsInMempool

      -- Sync the mempool with the ledger. Now all of the transactions in the
      -- mempool should have been removed.
      withSyncState (const (return ()))

      evs  <- getTraceEvents
      -- Also check that 'addTxToLedger' never resulted in an error.
      return $ map (const (Right ())) errs === errs .&&.
        let removedTxs = maybe
              []
              (\(TraceMempoolRemoveTxs txs _) -> txs)
              (find isRemoveTxsEvent evs)
        in sort txsInMempool === sort removedTxs
  where
    isRemoveTxsEvent :: TraceEventMempool blk -> Bool
    isRemoveTxsEvent (TraceMempoolRemoveTxs _ _) = True
    isRemoveTxsEvent _                           = False

{-------------------------------------------------------------------------------
  TestSetup: how to set up a TestMempool
-------------------------------------------------------------------------------}

data TestSetup = TestSetup
  { testLedgerState :: LedgerState TestBlock
  , testInitialTxs  :: [TestTx]
    -- ^ These are all valid and will be the initial contents of the Mempool.
  } deriving (Show)

ppTestSetup :: TestSetup -> String
ppTestSetup TestSetup { testLedgerState, testInitialTxs } = unlines $
    ["Ledger/chain contains TxIds:"]         <>
    (map condense (tlTxIds testLedgerState)) <>
    ["Initial contents of the Mempool:"]     <>
    (map condense testInitialTxs)

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions.
genTestSetup :: Int -> Gen (TestSetup, LedgerState TestBlock)
genTestSetup n = do
    ledgerSize   <- choose (0, n)
    nbInitialTxs <- choose (0, n)
    (_txs1,  ledger1) <- genValidTxs ledgerSize testInitLedger
    ( txs2,  ledger2) <- genValidTxs nbInitialTxs ledger1
    let testSetup = TestSetup
          { testLedgerState = ledger1
          , testInitialTxs  = txs2
          }
    return (testSetup, ledger2)

instance Arbitrary TestSetup where
  arbitrary = sized $ fmap fst . genTestSetup
  shrink TestSetup { testLedgerState, testInitialTxs } =
    -- TODO we could shrink @testLedgerState@ too
    [ TestSetup { testLedgerState, testInitialTxs = testInitialTxs' }
    | testInitialTxs' <- shrinkList (const []) testInitialTxs ]

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
    updateLedgerTipPoint <$> applyTx LedgerConfig (TestGenTx tx) ledgerState
  where
    updateLedgerTipPoint ledgerState = ledgerState
        { tlLastApplied = BlockPoint { withHash = unSlotNo slot', atSlot = slot' } }
      where
        slot' = case pointSlot (ledgerTipPoint ledgerState) of
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
    (testSetup, ledger)  <- genTestSetup n
    nbTxs <- choose (0, n)
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
  TestMempool: a mempool with random contents
-------------------------------------------------------------------------------}

data TestMempool m = TestMempool
  { -- | A mempool with random contents.
    --
    -- Starts out synched with the ledger.
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

    -- | This function can be used to add a transaction to the ledger/chain.
    --
    -- Remember to synchronise the mempool afterwards.
  , addTxToLedger    :: TestTx -> m (Either TestTxError ())
  }

withTestMempool
  :: forall prop. Testable prop
  => TestSetup
  -> (forall m. MonadSTM m => TestMempool m -> m prop)
  -> Property
withTestMempool setup@TestSetup { testLedgerState, testInitialTxs } prop =
    counterexample (ppTestSetup setup) $
    classify      (null testInitialTxs)  "empty Mempool"     $
    classify (not (null testInitialTxs)) "non-empty Mempool" $
    runSimOrThrow setUpAndRun
  where
    cfg = ledgerConfigView singleNodeTestConfig

    setUpAndRun :: forall m. MonadAsync m => m Property
    setUpAndRun = do

      -- Set up the LedgerInterface
      varCurrentLedgerState <- atomically $ newTVar testLedgerState
      let ledgerInterface = LedgerInterface
            { getCurrentLedgerState = readTVar varCurrentLedgerState
            }

      -- Set up the Tracer
      varEvents <- atomically $ newTVar []
      -- TODO use SimM's dynamicTracer
      let tracer = Tracer $ \ev -> atomically $ modifyTVar varEvents (ev:)

      -- Open the mempool and add the initial transactions
      mempool <- openMempoolWithoutSyncThread ledgerInterface cfg tracer
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
        , addTxToLedger    = atomically . addTxToLedger varCurrentLedgerState
        }

    addTxToLedger :: forall m. MonadSTM m
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
    txseq = foldl' (TxSeq.:>) TxSeq.Empty
                   (zipWith TxTicket xs (map TicketNo [0..]))

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

    mkTicket x = TxTicket x (mkTicketNo x)
    mkTicketNo = TicketNo . toEnum

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
executeAction :: forall m. MonadSTM m => TestMempool m -> Action -> m Property
executeAction testMempool action = case action of
    AddTx tx -> do
      void $ addTxs [TestGenTx tx]
      expectTraceEvent $ \case
        TraceMempoolAddTxs { _txs = [TestGenTx tx'] }
          | tx == tx'
          -> property True
        _ -> counterexample ("Transaction not added: " <> condense tx) False

    RemoveTx tx -> do
      void $ addTxToLedger tx
      -- Synchronise the Mempool with the updated chain
      withSyncState $ \_snapshot -> return ()
      expectTraceEvent $ \case
        TraceMempoolRemoveTxs { _txs = [TestGenTx tx'] }
          | tx == tx'
          -> property True
        _ -> counterexample ("Transaction not removed: " <> condense tx) False
  where
    TestMempool
      { mempool
      , eraseTraceEvents
      , getTraceEvents
      , addTxToLedger
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

currentTicketAssignment :: MonadSTM m
                        => Mempool m TestBlock TicketNo -> m TicketAssignment
currentTicketAssignment Mempool { withSyncState } =
    withSyncState $ \MempoolSnapshot { snapshotTxs } -> return $ Map.fromList
      [ (ticketNo, testTxId (unTestGenTx tx)) | (tx, ticketNo) <- snapshotTxs ]

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
