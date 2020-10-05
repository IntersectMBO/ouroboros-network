{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Mempool (tests) where

import           Control.Exception (assert)
import           Control.Monad (foldM, forM, forM_, void)
import           Control.Monad.Except (Except, runExcept)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.Bifunctor (first)
import           Data.Either (isRight)
import           Data.List (find, foldl', isSuffixOf, nub, partition, sort)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Stack (HasCallStack)

import           Test.QuickCheck hiding (elements)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Cardano.Binary (Encoding, toCBOR)
import           Cardano.Crypto.Hash

import           Control.Monad.IOSim (runSimOrThrow)

import           Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Mock.Ledger hiding (TxId)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (repeatedly, repeatedlyM,
                     safeMaximumOn, whenJust)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (elements)

tests :: TestTree
tests = testGroup "Mempool"
  [ testGroup "TxSeq"
      [ testProperty "lookupByTicketNo complete"           prop_TxSeq_lookupByTicketNo_complete
      , testProperty "lookupByTicketNo sound"              prop_TxSeq_lookupByTicketNo_sound
      , testProperty "splitAfterTxSize"                    prop_TxSeq_splitAfterTxSize
      , testProperty "splitAfterTxSizeSpec"                prop_TxSeq_splitAfterTxSizeSpec
      ]
  , testProperty "snapshotTxs == snapshotTxsAfter zeroIdx" prop_Mempool_snapshotTxs_snapshotTxsAfter
  , testProperty "valid added txs == getTxs"               prop_Mempool_addTxs_getTxs
  , testProperty "addTxs txs == mapM (addTxs . pure) txs"  prop_Mempool_addTxs_one_vs_multiple
  , testProperty "result of addTxs"                        prop_Mempool_addTxs_result
  , testProperty "Invalid transactions are never added"    prop_Mempool_InvalidTxsNeverAdded
  , testProperty "result of getCapacity"                   prop_Mempool_getCapacity
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
      _ <- addTxs mempool (allTxs setup)
      MempoolSnapshot { snapshotTxs } <- atomically $ getSnapshot mempool
      return $ counterexample (ppTxs (txs setup)) $
        validTxs setup `isSuffixOf` map fst snapshotTxs

-- | Same as 'prop_Mempool_addTxs_getTxs', but add the transactions one-by-one
-- instead of all at once.
prop_Mempool_addTxs_one_vs_multiple :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_one_vs_multiple setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      forM_ (allTxs setup) $ \tx -> addTxs mempool [tx]
      MempoolSnapshot { snapshotTxs } <- atomically $ getSnapshot mempool
      return $ counterexample (ppTxs (txs setup)) $
        validTxs setup `isSuffixOf` map fst snapshotTxs

-- | Test that the result of adding transaction to a 'Mempool' matches our
-- expectation: invalid transactions have errors associated with them and
-- valid transactions don't.
prop_Mempool_addTxs_result :: TestSetupWithTxs -> Property
prop_Mempool_addTxs_result setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      result <- addTxs mempool (allTxs setup)
      return $ counterexample (ppTxs (txs setup)) $
        [(tx, isMempoolTxAdded res) | (tx, res) <- result] ===
        [(testTx, valid)            | (testTx, valid) <- txs setup]

-- | Test that invalid transactions are never added to the 'Mempool'.
prop_Mempool_InvalidTxsNeverAdded :: TestSetupWithTxs -> Property
prop_Mempool_InvalidTxsNeverAdded setup =
    withTestMempool (testSetup setup) $ \TestMempool { mempool } -> do
      txsInMempoolBefore <- map fst . snapshotTxs <$>
        atomically (getSnapshot mempool)
      _ <- addTxs mempool (allTxs setup)
      txsInMempoolAfter <- map fst . snapshotTxs <$>
        atomically (getSnapshot mempool)
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
prop_Mempool_removeTxs (TestSetupWithTxInMempool testSetup txToRemove) =
    withTestMempool testSetup $ \TestMempool { mempool } -> do
      let Mempool { removeTxs, getSnapshot } = mempool
      removeTxs [txId txToRemove]
      txsInMempoolAfter <- map fst . snapshotTxs <$> atomically getSnapshot
      return $ counterexample
        ("Transactions in the mempool after removing (" <>
         show txToRemove <> "): " <> show txsInMempoolAfter)
        (txToRemove `notElem` txsInMempoolAfter)

-- | Test that 'getCapacity' returns the 'MempoolCapacityBytes' value that the
-- mempool was initialized with.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'.
prop_Mempool_getCapacity :: MempoolCapTestSetup -> Property
prop_Mempool_getCapacity mcts =
    withTestMempool testSetup $ \TestMempool{mempool} -> do
      actualCapacity <- atomically $ getCapacity mempool
      pure (actualCapacity === testCapacity)
  where
    MempoolCapacityBytesOverride testCapacity = testMempoolCapOverride testSetup
    MempoolCapTestSetup (TestSetupWithTxs testSetup _txsToAdd) = mcts

-- | Test the correctness of 'tryAddTxs' when the Mempool is (or will be) at
-- capacity.
--
-- Ignore the "100% empty Mempool" label in the test output, that is there
-- because we reuse 'withTestMempool' and always start with an empty Mempool
-- and 'LedgerState'.
prop_Mempool_Capacity :: MempoolCapTestSetup -> Property
prop_Mempool_Capacity (MempoolCapTestSetup testSetupWithTxs) =
  withTestMempool testSetup $ \TestMempool { mempool } -> do
    capacity <- atomically (getCapacity mempool)
    curSize <- msNumBytes . snapshotMempoolSize <$>
      atomically (getSnapshot mempool)
    res@(processed, unprocessed) <- tryAddTxs mempool (map fst txsToAdd)
    return $
      counterexample ("Initial size: " <> show curSize)    $
      classify (null processed)   "no transactions added"  $
      classify (null unprocessed) "all transactions added" $
      blindErrors res === expectedResult capacity curSize
  where
    TestSetupWithTxs testSetup txsToAdd = testSetupWithTxs

    -- | Convert 'MempoolAddTxResult' into a 'Bool':
    -- isMempoolTxAdded -> True, isMempoolTxRejected -> False.
    blindErrors
      :: ([(GenTx TestBlock, MempoolAddTxResult blk)], [GenTx TestBlock])
      -> ([(GenTx TestBlock, Bool)], [GenTx TestBlock])
    blindErrors (processed, toAdd) = (processed', toAdd)
      where
        processed' = [ (tx, isMempoolTxAdded txAddRes)
                     | (tx, txAddRes) <- processed ]

    expectedResult
      :: MempoolCapacityBytes
      -> Word32  -- ^ Current mempool size
      -> ([(GenTx TestBlock, Bool)], [GenTx TestBlock])
    expectedResult (MempoolCapacityBytes capacity) = \curSize ->
        go curSize [] txsToAdd
      where
        go
          :: Word32
          -> [(GenTx TestBlock, Bool)]
          -> [(GenTx TestBlock, Bool)]
          -> ([(GenTx TestBlock, Bool)], [GenTx TestBlock])
        go curSize processed = \case
          []
            -> (reverse processed, [])
          (tx, valid):txsToAdd'
            | let curSize' = curSize + txSize tx
            , curSize' <= capacity
            -> go (if valid then curSize' else curSize)
                  ((tx, valid):processed)
                  txsToAdd'
            | otherwise
            -> (reverse processed, tx:map fst txsToAdd')

-- | Test that all valid transactions added to a 'Mempool' via 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceValidTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceValidTxs setup =
    withTestMempool (testSetup setup) $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents } = testMempool
      _ <- addTxs mempool (allTxs setup)
      evs <- getTraceEvents
      return $ counterexample (ppTxs (txs setup)) $
        let addedTxs = mapMaybe isAddedTxsEvent evs
        in validTxs setup === addedTxs
  where
    isAddedTxsEvent :: TraceEventMempool blk -> Maybe (GenTx blk)
    isAddedTxsEvent (TraceMempoolAddedTx tx _ _) = Just tx
    isAddedTxsEvent _                            = Nothing

-- | Test that all invalid rejected transactions returned from 'addTxs' are
-- appropriately represented in the trace of events.
prop_Mempool_TraceRejectedTxs :: TestSetupWithTxs -> Property
prop_Mempool_TraceRejectedTxs setup =
    withTestMempool (testSetup setup) $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents } = testMempool
      _ <- addTxs mempool (allTxs setup)
      evs <- getTraceEvents
      return $ counterexample (ppTxs (txs setup)) $
        let rejectedTxs = mapMaybe isRejectedTxEvent evs
        in invalidTxs setup === rejectedTxs
  where
    isRejectedTxEvent :: TraceEventMempool blk -> Maybe (GenTx blk)
    isRejectedTxEvent (TraceMempoolRejectedTx tx _ _) = Just tx
    isRejectedTxEvent _                               = Nothing

-- | Test that all transactions in the 'Mempool' that have become invalid
-- because of an update to the ledger are appropriately represented in the
-- trace of events.
prop_Mempool_TraceRemovedTxs :: TestSetup -> Property
prop_Mempool_TraceRemovedTxs setup =
    withTestMempool setup $ \testMempool -> do
      let TestMempool { mempool, getTraceEvents, addTxsToLedger, getCurrentLedger } = testMempool
      MempoolSnapshot { snapshotTxs } <- atomically $ getSnapshot mempool
      -- We add all the transactions in the mempool to the ledger. Some of
      -- them will become invalid because all inputs have been spent.
      let txsInMempool = map fst snapshotTxs
      errs <- atomically $ addTxsToLedger txsInMempool

      -- Sync the mempool with the ledger. Now some of the transactions in the
      -- mempool should have been removed.
      void $ syncWithLedger mempool

      -- Predict which transactions should have been removed
      curLedger <- atomically getCurrentLedger
      let expected = expectedToBeRemoved curLedger txsInMempool

      -- Look at the trace to see which transactions actually got removed
      evs <- getTraceEvents
      let removedTxs = concat $ mapMaybe isRemoveTxsEvent evs

      -- Also check that 'addTxsToLedger' never resulted in an error.
      return $
        classify (not (null removedTxs)) "Removed some transactions" $
        map (const (Right ())) errs === errs .&&.
        sort expected === sort removedTxs
  where
    isRemoveTxsEvent :: TraceEventMempool blk -> Maybe [GenTx blk]
    isRemoveTxsEvent (TraceMempoolRemoveTxs txs _) = Just txs
    isRemoveTxsEvent _                             = Nothing

    expectedToBeRemoved :: LedgerState TestBlock -> [TestTx] -> [TestTx]
    expectedToBeRemoved ledgerState txsInMempool =
      [ tx
      | (tx, valid) <- fst $ validateTxs ledgerState txsInMempool
      , not valid
      ]

{-------------------------------------------------------------------------------
  TestSetup: how to set up a TestMempool
-------------------------------------------------------------------------------}

type TestBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

type TestTx = GenTx TestBlock

type TestTxId = TxId TestTx

type TestTxError = ApplyTxErr TestBlock

-- There are 5 (core)nodes and each gets 1000.
testInitLedger :: LedgerState TestBlock
testInitLedger = genesisSimpleLedgerState $ mkAddrDist (NumCoreNodes 5)

-- | Test config
--
-- (We don't really care about these values here)
testLedgerConfig :: LedgerConfig TestBlock
testLedgerConfig = SimpleLedgerConfig {
      simpleMockLedgerConfig = ()
    , simpleLedgerEraParams  =
        HardFork.defaultEraParams
          (SecurityParam 4)
          (slotLengthFromSec 20)
    }

data TestSetup = TestSetup
  { testLedgerState        :: LedgerState TestBlock
  , testInitialTxs         :: [TestTx]
    -- ^ These are all valid and will be the initial contents of the Mempool.
  , testMempoolCapOverride :: MempoolCapacityBytesOverride
  } deriving (Show)

ppTestSetup :: TestSetup -> String
ppTestSetup TestSetup { testInitialTxs
                      , testMempoolCapOverride
                      } = unlines $
    ["Initial contents of the Mempool:"]  <>
    (map ppTestTxWithHash testInitialTxs) <>
    ["Mempool capacity override:"]        <>
    [show testMempoolCapOverride]

ppTestTxWithHash :: TestTx -> String
ppTestTxWithHash x = condense
  (hashWithSerialiser toCBOR (simpleGenTx x) :: Hash MD5 Tx, x)

-- | Given some transactions, calculate the sum of their sizes in bytes.
txSizesInBytes :: [TestTx] -> TxSizeInBytes
txSizesInBytes = foldl' (\acc tx -> acc + txSize tx) 0

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions.
--
-- The generated 'testMempoolCap' will be:
-- > 'txSizesInBytes' 'testInitialTxs' + extraCapacity
genTestSetupWithExtraCapacity :: Int -> Word32 -> Gen (TestSetup, LedgerState TestBlock)
genTestSetupWithExtraCapacity maxInitialTxs extraCapacity = do
    ledgerSize   <- choose (0, maxInitialTxs)
    nbInitialTxs <- choose (0, maxInitialTxs)
    (_txs1,  ledger1) <- genValidTxs ledgerSize testInitLedger
    ( txs2,  ledger2) <- genValidTxs nbInitialTxs ledger1
    let initTxsSizeInBytes = txSizesInBytes txs2
        mpCap = MempoolCapacityBytes (initTxsSizeInBytes + extraCapacity)
        testSetup = TestSetup
          { testLedgerState        = ledger1
          , testInitialTxs         = txs2
          , testMempoolCapOverride = MempoolCapacityBytesOverride mpCap
          }
    return (testSetup, ledger2)

-- | Generate a 'TestSetup' and return the ledger obtained by applying all of
-- the initial transactions. Generates setups with a fixed
-- 'MempoolCapacityBytesOverride', no 'NoMempoolCapacityBytesOverride'.
genTestSetup :: Int -> Gen (TestSetup, LedgerState TestBlock)
genTestSetup maxInitialTxs = genTestSetupWithExtraCapacity maxInitialTxs 0

-- | Random 'MempoolCapacityBytesOverride'
instance Arbitrary TestSetup where
  arbitrary = sized $ \n -> do
    extraCapacity <- fromIntegral <$> choose (0, n)
    testSetup <- fst <$> genTestSetupWithExtraCapacity n extraCapacity
    noOverride <- arbitrary
    return $
      if noOverride
      then testSetup { testMempoolCapOverride = NoMempoolCapacityBytesOverride }
      else testSetup

  shrink TestSetup { testLedgerState
                   , testInitialTxs
                   , testMempoolCapOverride = MempoolCapacityBytesOverride
                       (MempoolCapacityBytes mpCap)
                   } =
    -- TODO we could shrink @testLedgerState@ too
    [ TestSetup { testLedgerState
                , testInitialTxs = testInitialTxs'
                , testMempoolCapOverride = MempoolCapacityBytesOverride
                    (MempoolCapacityBytes mpCap')
                }
    | let extraCap = mpCap - txSizesInBytes testInitialTxs
    , testInitialTxs' <- shrinkList (const []) testInitialTxs
    , isRight $ txsAreValid testLedgerState testInitialTxs'
    , let mpCap' = txSizesInBytes testInitialTxs' + extraCap
    ]

  -- TODO shrink to an override, that's an easier test case
  shrink TestSetup { testLedgerState
                   , testInitialTxs
                   , testMempoolCapOverride = NoMempoolCapacityBytesOverride
                   } =
    -- TODO we could shrink @testLedgerState@ too
    [ TestSetup { testLedgerState
                , testInitialTxs = testInitialTxs'
                , testMempoolCapOverride = NoMempoolCapacityBytesOverride
                }
    | testInitialTxs' <- shrinkList (const []) testInitialTxs
    , isRight $ txsAreValid testLedgerState testInitialTxs'
    ]

-- | Generate a number of valid and invalid transactions and apply the valid
-- transactions to the given 'LedgerState'. The transactions along with a
-- 'Bool' indicating whether its valid ('True') or invalid ('False') and the
-- resulting 'LedgerState' are returned.
genTxs :: Int  -- ^ The number of transactions to generate
       -> LedgerState TestBlock
       -> Gen ([(TestTx, Bool)], LedgerState TestBlock)
genTxs = go []
  where
    go txs n ledger
      | n <= 0 = return (reverse txs, ledger)
      | otherwise = do
          valid <- arbitrary
          if valid
            then do
              (validTx, ledger') <- genValidTx ledger
              go ((validTx, True):txs)    (n - 1) ledger'
            else do
              invalidTx <- genInvalidTx ledger
              go ((invalidTx, False):txs) (n - 1) ledger

mustBeValid :: HasCallStack
            => Except TestTxError (LedgerState TestBlock)
            -> LedgerState TestBlock
mustBeValid ex = case runExcept ex of
  Left _       -> error "impossible"
  Right ledger -> ledger

txIsValid :: LedgerState TestBlock -> TestTx -> Bool
txIsValid ledgerState tx =
    isRight $ runExcept $ applyTxToLedger ledgerState tx

txsAreValid
  :: LedgerState TestBlock
  -> [TestTx]
  -> Either TestTxError (LedgerState TestBlock)
txsAreValid ledgerState txs =
    runExcept $ repeatedlyM (flip applyTxToLedger) txs ledgerState

validateTxs
  :: LedgerState TestBlock
  -> [TestTx]
  -> ([(TestTx, Bool)], LedgerState TestBlock)
validateTxs = go []
  where
    go revalidated ledgerState = \case
      []      -> (reverse revalidated, ledgerState)
      tx:txs' -> case runExcept (applyTxToLedger ledgerState tx) of
        Left _             -> go ((tx, False):revalidated) ledgerState  txs'
        Right ledgerState' -> go ((tx, True):revalidated)  ledgerState' txs'

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
          (tx, ledger') <- genValidTx ledger
          go (tx:txs) (n - 1) ledger'

genValidTx :: LedgerState TestBlock -> Gen (TestTx, LedgerState TestBlock)
genValidTx ledgerState@(SimpleLedgerState MockState { mockUtxo = utxo }) = do
    -- Never let someone go broke, otherwise we risk concentrating all the
    -- wealth in one person. That would be problematic (for the society) but
    -- also because we wouldn't be able to generate any valid transactions
    -- anymore.

    let sender
          | Just (richest, _) <- safeMaximumOn snd $ Map.toList $
            sum . map snd <$> peopleWithFunds
          = richest
          | otherwise
          = error "no people with funds"

    recipient <- elements $ filter (/= sender) $ Map.keys peopleWithFunds
    let assets  = peopleWithFunds Map.! sender
        fortune = sum (map snd assets)
        ins     = Set.fromList $ map fst assets

    -- At most spent half of someone's fortune
    amount <- choose (1, fortune `div` 2)
    let outRecipient = (recipient, amount)
        outs
          | amount == fortune
          = [outRecipient]
          | otherwise
          = [outRecipient, (sender, fortune - amount)]
        tx = mkSimpleGenTx $ Tx DoNotExpire ins outs
    return (tx, mustBeValid (applyTxToLedger ledgerState tx))
  where
    peopleWithFunds :: Map Addr [(TxIn, Amount)]
    peopleWithFunds = Map.unionsWith (<>)
      [ Map.singleton addr [(txIn, amount)]
      | (txIn, (addr, amount)) <- Map.toList utxo
      ]

genInvalidTx :: LedgerState TestBlock -> Gen TestTx
genInvalidTx ledgerState@(SimpleLedgerState MockState { mockUtxo = utxo }) = do
    let peopleWithFunds = nub $ map fst $ Map.elems utxo
    sender    <- elements peopleWithFunds
    recipient <- elements $ filter (/= sender) peopleWithFunds
    let assets = filter (\(_, (addr, _)) -> addr == sender) $ Map.toList utxo
        ins    = Set.fromList $ map fst assets
    -- There is only 5 000 in 'testInitLedger', so any transaction spending
    -- more than 5 000 is invalid.
    amount <- choose (5_001, 10_000)
    let outs = [(recipient, amount)]
        tx   = mkSimpleGenTx $ Tx DoNotExpire ins outs
    return $ assert (not (txIsValid ledgerState tx)) tx

-- | Apply a transaction to the ledger
--
-- We don't have blocks in this test, but transactions only. In this function
-- we pretend the transaction /is/ a block, apply it to the UTxO, and then
-- update the tip of the ledger state, incrementing the slot number and faking
-- a hash.
applyTxToLedger :: LedgerState TestBlock
                -> TestTx
                -> Except TestTxError (LedgerState TestBlock)
applyTxToLedger (SimpleLedgerState mockState) tx =
    mkNewLedgerState <$> updateMockUTxO dummy tx mockState
  where
    -- All expiries in this test are 'DoNotExpire', so the current time is
    -- irrelevant.
    dummy :: SlotNo
    dummy = 0

    mkNewLedgerState mockState' =
      SimpleLedgerState mockState' { mockTip = BlockPoint slot' hash' }

    slot' = case pointSlot $ mockTip mockState of
      Origin      -> 0
      NotOrigin s -> succ s

    -- A little trick to instantiate the phantom parameter of 'Hash' (and
    -- 'HeaderHash') with 'TestBlock' while actually hashing the slot number:
    -- use a custom serialiser to instantiate the phantom type parameter with
    -- @Header TestBlock@, but actually encode the slot number instead.
    hash' :: HeaderHash TestBlock
    hash' = hashWithSerialiser fakeEncodeHeader (error "fake header")

    fakeEncodeHeader :: Header TestBlock -> Encoding
    fakeEncodeHeader _ = toCBOR slot'

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
allTxs = map fst . txs

validTxs :: TestSetupWithTxs -> [GenTx TestBlock]
validTxs = map fst . filter snd . txs

invalidTxs :: TestSetupWithTxs -> [GenTx TestBlock]
invalidTxs = map fst . filter (not . snd) . txs

instance Arbitrary TestSetupWithTxs where
  arbitrary = sized $ \n -> do
    nbTxs <- choose (0, n)
    (testSetup, ledger)  <- genTestSetup n
    (txs,      _ledger') <- genTxs nbTxs ledger
    testSetup' <- case testMempoolCapOverride testSetup of
      NoMempoolCapacityBytesOverride -> return testSetup
      MempoolCapacityBytesOverride (MempoolCapacityBytes mpCap) -> do
        noOverride <- arbitrary
        return testSetup {
              testMempoolCapOverride =
                if noOverride
                then NoMempoolCapacityBytesOverride
                else MempoolCapacityBytesOverride $ MempoolCapacityBytes $
                       mpCap + txSizesInBytes (map fst txs)
            }
    return TestSetupWithTxs { testSetup = testSetup', txs }

  shrink TestSetupWithTxs { testSetup, txs } =
      [ TestSetupWithTxs { testSetup = testSetup', txs }
      | testSetup' <- shrink testSetup ] <>
      [ TestSetupWithTxs { testSetup, txs = txs' }
      | txs' <- map (fst . revalidate testSetup) .
                shrinkList (const []) .
                map fst $ txs ]

revalidate :: TestSetup -> [TestTx] -> ([(TestTx, Bool)], LedgerState TestBlock)
revalidate TestSetup { testLedgerState, testInitialTxs } =
    validateTxs initLedgerState
  where
    -- The LedgerState after adding the transactions initially in the mempool
    initLedgerState = repeatedly
      (\tx l -> mustBeValid (applyTxToLedger l tx))
      testInitialTxs
      testLedgerState

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

    -- | Return the current ledger.
  , getCurrentLedger :: STM m (LedgerState TestBlock)
  }

-- NOTE: at the end of the test, this function also checks whether the Mempool
-- contents are valid w.r.t. the current ledger.
withTestMempool
  :: forall prop. Testable prop
  => TestSetup
  -> (forall m. IOLike m => TestMempool m -> m prop)
  -> Property
withTestMempool setup@TestSetup {..} prop =
      counterexample (ppTestSetup setup)
    $ classify
        (isOverride testMempoolCapOverride)
        "MempoolCapacityBytesOverride"
    $ classify
        (not (isOverride testMempoolCapOverride))
        "NoMempoolCapacityBytesOverride"
    $ classify (null testInitialTxs)       "empty Mempool"
    $ classify (not (null testInitialTxs)) "non-empty Mempool"
    $ runSimOrThrow setUpAndRun
  where
    isOverride (MempoolCapacityBytesOverride _) = True
    isOverride NoMempoolCapacityBytesOverride   = False

    setUpAndRun :: forall m. IOLike m => m Property
    setUpAndRun = do

      -- Set up the LedgerInterface
      varCurrentLedgerState <- uncheckedNewTVarM testLedgerState
      let ledgerInterface = LedgerInterface
            { getCurrentLedgerState = readTVar varCurrentLedgerState
            }

      -- Set up the Tracer
      varEvents <- uncheckedNewTVarM []
      -- TODO use IOSim's dynamicTracer
      let tracer = Tracer $ \ev -> atomically $ modifyTVar varEvents (ev:)

      -- Open the mempool and add the initial transactions
      mempool <-
        openMempoolWithoutSyncThread
          ledgerInterface
          testLedgerConfig
          testMempoolCapOverride
          tracer
          txSize
      result  <- addTxs mempool testInitialTxs
      -- the invalid transactions are reported in the same order they were
      -- added, so the first error is not the result of a cascade
      whenJust (find (isMempoolTxRejected . snd) result) $ \(invalidTx, _) ->
        error $ "Invalid initial transaction: " <> condense invalidTx

      -- Clear the trace
      atomically $ writeTVar varEvents []

      -- Apply the property to the 'TestMempool' record
      res <- property <$> prop TestMempool
        { mempool
        , getTraceEvents   = atomically $ reverse <$> readTVar varEvents
        , eraseTraceEvents = atomically $ writeTVar varEvents []
        , addTxsToLedger   = addTxsToLedger varCurrentLedgerState
        , getCurrentLedger = readTVar varCurrentLedgerState
        }
      validContents <- atomically $
            checkMempoolValidity
        <$> readTVar varCurrentLedgerState
        <*> getSnapshot mempool
      return $ res .&&. validContents

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

    -- | Check whether the transactions in the 'MempoolSnapshot' are valid
    -- w.r.t. the current ledger state.
    checkMempoolValidity :: LedgerState TestBlock
                         -> MempoolSnapshot TestBlock TicketNo
                         -> Property
    checkMempoolValidity ledgerState
                         MempoolSnapshot {
                             snapshotTxs
                           , snapshotSlotNo
                           } =
        case runExcept $ repeatedlyM
               (applyTx testLedgerConfig snapshotSlotNo)
               txs
               (TickedSimpleLedgerState ledgerState) of
          Right _ -> property True
          Left  e -> counterexample (mkErrMsg e) $ property False
      where
        txs = map fst snapshotTxs
        mkErrMsg e =
          "At the end of the test, the Mempool contents were invalid: " <>
          show e

{-------------------------------------------------------------------------------
  MempoolCapTestSetup
-------------------------------------------------------------------------------}

-- | Reuse 'TestSetupWithTxs' but just pick a specific capacity based on the
-- transactions to add.
newtype MempoolCapTestSetup = MempoolCapTestSetup TestSetupWithTxs
  deriving (Show)

instance Arbitrary MempoolCapTestSetup where
  -- TODO: shrink
  arbitrary = do
    testSetupWithTxs@TestSetupWithTxs { testSetup, txs } <- arbitrary
    -- The Mempool should at least be capable of containing the transactions
    -- it already contains.
    let currentSize      = sum (map txSize (testInitialTxs testSetup))
        capacityMinBound = currentSize
        validTxsToAdd    = [tx | (tx, True) <- txs]
        -- Use the current size + the sum of all the valid transactions to add
        -- as the upper bound.
        capacityMaxBound = currentSize + sum (map txSize validTxsToAdd)
    -- Note that we could pick @currentSize@, meaning that we can't add any
    -- more transactions to the Mempool
    capacity <- choose
      ( capacityMinBound
      , capacityMaxBound
      )
    let testSetup' = testSetup {
            testMempoolCapOverride = MempoolCapacityBytesOverride $
              MempoolCapacityBytes capacity
          }
    return $ MempoolCapTestSetup testSetupWithTxs { testSetup = testSetup' }

{-------------------------------------------------------------------------------
  TxSeq Properties
-------------------------------------------------------------------------------}

-- | Finds elements in the sequence
prop_TxSeq_lookupByTicketNo_complete :: [Int] -> Bool
prop_TxSeq_lookupByTicketNo_complete xs =
    and [ case TxSeq.lookupByTicketNo txseq tn of
            Just tx' -> tx == tx'
            Nothing  -> False
        | (tx, tn) <- TxSeq.toTuples txseq ]
  where
    txseq :: TxSeq Int
    txseq = TxSeq.fromList $ zipWith3 TxTicket xs (map TicketNo [0..]) (repeat 0)

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
      property $ txSizeSum (TxSeq.toList before) <= tssTxSizeToSplitOn
  where
    TxSizeSplitTestSetup { tssTxSizeToSplitOn } = tss

    (before, _after) = splitAfterTxSize txseq tssTxSizeToSplitOn

    txseq :: TxSeq Int
    txseq = txSizeSplitTestSetupToTxSeq tss

    txSizeSum :: [TxTicket tx] -> TxSizeInBytes
    txSizeSum = sum . map txTicketTxSizeInBytes


-- | Test that the results of 'splitAfterTxSizeSpec', a specification of
-- 'splitAfterTxSize', match those of the real 'splitAfterTxSize'
-- implementation.
prop_TxSeq_splitAfterTxSizeSpec :: TxSizeSplitTestSetup -> Property
prop_TxSeq_splitAfterTxSizeSpec tss =
         TxSeq.toList implBefore === TxSeq.toList specBefore
    .&&. TxSeq.toList implAfter  === TxSeq.toList specAfter
  where
    TxSizeSplitTestSetup { tssTxSizeToSplitOn } = tss

    (implBefore, implAfter) = splitAfterTxSize txseq tssTxSizeToSplitOn

    (specBefore, specAfter) = splitAfterTxSizeSpec txseq tssTxSizeToSplitOn

    txseq :: TxSeq Int
    txseq = txSizeSplitTestSetupToTxSeq tss

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
    TxSeq.fromList [TxTicket 0 (TicketNo 0) tssTxSize | tssTxSize <- tssTxSizes]

{-------------------------------------------------------------------------------
  TicketNo Properties
-------------------------------------------------------------------------------}

-- | Testing plan:
--
-- * Perform a number of actions: either add a new valid transaction to the
--   Mempool (invalid transactions have no effect on the @idx@s) or remove an
--   existing transaction from the Mempool.
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
        txsInMempool      <- map fst . snapshotTxs <$>
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
      { testLedgerState        = testInitLedger
      , testInitialTxs         = []
      , testMempoolCapOverride =
          MempoolCapacityBytesOverride $ MempoolCapacityBytes maxBound
      }

    lastOfMempoolRemoved txsInMempool = \case
      AddTxs    _   -> False
      RemoveTxs txs -> last txsInMempool `elem` txs

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
  = AddTxs    [TestTx]
    -- ^ When part of 'Actions', all these transactions are valid.
  | RemoveTxs [TestTx]
    -- ^ When part of 'Actions', removing these transactions will not
    -- invalidate any other transactions.
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
    addMapping mapping (RemoveTxs _txs) = return mapping
    addMapping mapping (AddTxs     txs) = do
      newMappings <- forM txs $ \tx -> do
        nextTicketNo <- get
        modify succ
        return (nextTicketNo, txId tx)
      return $ Map.union mapping (Map.fromList newMappings)

-- | Executes the action and verifies that it is actually executed using the
-- tracer, hence the 'Property' in the return type.
executeAction :: forall m. IOLike m => TestMempool m -> Action -> m Property
executeAction testMempool action = case action of
    AddTxs txs -> do
      void $ addTxs mempool txs
      tracedAddedTxs <- expectTraceEvent $ \case
        TraceMempoolAddedTx tx _ _ -> Just tx
        _                          -> Nothing
      return $ if tracedAddedTxs == txs
        then property True
        else counterexample
          ("Expected TraceMempoolAddedTx events for " <> condense txs <>
           " but got " <> condense tracedAddedTxs)
          False

    RemoveTxs txs -> do
      removeTxs mempool (map txId txs)
      tracedManuallyRemovedTxs <- expectTraceEvent $ \case
        TraceMempoolManuallyRemovedTxs txIds _ _ -> Just txIds
        _                                        -> Nothing
      return $ if concat tracedManuallyRemovedTxs == map txId txs
        then property True
        else counterexample
          ("Expected a TraceMempoolManuallyRemovedTxs event for " <>
           condense txs <> " but got " <>
           condense tracedManuallyRemovedTxs)
          False

  where
    TestMempool
      { mempool
      , eraseTraceEvents
      , getTraceEvents
      } = testMempool

    expectTraceEvent :: (TraceEventMempool TestBlock -> Maybe a) -> m [a]
    expectTraceEvent extractor = do
      evs <- getTraceEvents
      eraseTraceEvents
      return $ mapMaybe extractor evs

currentTicketAssignment :: IOLike m
                        => Mempool m TestBlock TicketNo -> m TicketAssignment
currentTicketAssignment Mempool { syncWithLedger } = do
    MempoolSnapshot { snapshotTxs } <- syncWithLedger
    return $ Map.fromList
      [ (ticketNo, txId tx)
      | (tx, ticketNo) <- snapshotTxs
      ]

instance Arbitrary Actions where
  arbitrary = sized $ genActions (choose (1, 3))

genActions
  :: Gen Int  -- ^ Generate the number of transactions to add
  -> Int      -- ^ How many actions
  -> Gen Actions
genActions genNbToAdd = go testInitLedger mempty mempty
  where
    go :: LedgerState TestBlock
          -- ^ Current ledger state with the contents of the Mempool applied
       -> [TestTx]  -- ^ Transactions currently in the Mempool
       -> [Action]  -- ^ Already generated actions
       -> Int       -- ^ Number of actions left to generate
       -> Gen Actions
    go ledger txs actions n
      | n <= 0    = return $ Actions (reverse actions)
      | otherwise = arbitrary >>= \case
        True
          | not (null txs)
            -- Remove a transaction (or multiple), but only if there are
            -- transactions to remove
          -> do
          tx <- elements txs
          let ((vTxs, iTxs), ledger') = first (partition snd) $
                validateTxs testInitLedger (filter (/= tx) txs)
              txs'       = map fst vTxs
              removedTxs = tx : map fst iTxs
          go ledger' txs' (RemoveTxs removedTxs:actions) (n - 1)
        _ -> do
          nbToAdd <- genNbToAdd
          (txs', ledger') <- genValidTxs nbToAdd ledger
          go ledger' (txs' <> txs) (AddTxs txs':actions) (n - 1)
