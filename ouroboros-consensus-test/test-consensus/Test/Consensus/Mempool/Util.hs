{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}

module Test.Consensus.Mempool.Util (
    TestBlock
  , TestTx
  , TestTxError
  , applyTxToLedger
  , genInvalidTx
  , genTxs
  , genValidTx
  , genValidTxs
  , mustBeValid
  , testInitLedger
  , testLedgerConfig
  , txIsValid
  ) where

import           Cardano.Binary (Encoding, toCBOR)
import           Cardano.Crypto.Hash
import           Cardano.Slotting.Slot
import           Control.Exception (assert)
import           Control.Monad.Except (Except)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Either (isRight)
import           Data.List (nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Config.SecurityParam
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Mock.Ledger hiding (TxId)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Util (safeMaximumOn)
import           Test.QuickCheck hiding (elements)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (elements)

type TestBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

type TestTx = GenTx TestBlock

type TestTxError = ApplyTxErr TestBlock

-- There are 5 (core)nodes and each gets 1000.
testInitLedger :: LedgerState TestBlock ValuesMK
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


-- | Generate a number of valid and invalid transactions and apply the valid
-- transactions to the given 'LedgerState'. The transactions along with a
-- 'Bool' indicating whether its valid ('True') or invalid ('False') and the
-- resulting 'LedgerState' are returned.
genTxs :: Int  -- ^ The number of transactions to generate
       -> LedgerState TestBlock ValuesMK
       -> Gen ([(TestTx, Bool)], LedgerState TestBlock ValuesMK)
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

-- | Generate a number of valid transactions and apply these to the given
-- 'LedgerState'. The transactions and the resulting 'LedgerState' are
-- returned.
genValidTxs :: Int  -- ^ The number of valid transactions to generate
            -> LedgerState TestBlock ValuesMK
            -> Gen ([TestTx], LedgerState TestBlock ValuesMK)
genValidTxs = go []
  where
    go txs n ledger
      | n <= 0 = return (reverse txs, ledger)
      | otherwise = do
          (tx, ledger') <- genValidTx ledger
          go (tx:txs) (n - 1) ledger'

mustBeValid :: HasCallStack
            => Except TestTxError (LedgerState TestBlock ValuesMK)
            -> LedgerState TestBlock ValuesMK
mustBeValid ex = case runExcept ex of
  Left _       -> error "impossible"
  Right ledger -> ledger

txIsValid :: LedgerState TestBlock ValuesMK -> TestTx -> Bool
txIsValid ledgerState tx =
    isRight $ runExcept $ applyTxToLedger ledgerState tx

genValidTx :: LedgerState TestBlock ValuesMK -> Gen (TestTx, LedgerState TestBlock ValuesMK)
genValidTx ledgerState@(SimpleLedgerState MockState {} (SimpleLedgerTables (ValuesMK utxo))) = do
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

genInvalidTx :: LedgerState TestBlock ValuesMK -> Gen TestTx
genInvalidTx ledgerState = do
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

  where
    SimpleLedgerState {
      simpleLedgerTables = SimpleLedgerTables (ValuesMK utxo)
    } = ledgerState


-- | Apply a transaction to the ledger
--
-- We don't have blocks in this test, but transactions only. In this function
-- we pretend the transaction /is/ a block, apply it to the UTxO, and then
-- update the tip of the ledger state, incrementing the slot number and faking
-- a hash.
applyTxToLedger :: LedgerState TestBlock ValuesMK
                -> TestTx
                -> Except TestTxError (LedgerState TestBlock ValuesMK)
applyTxToLedger st tx =
    let SimpleLedgerState mockState _ = stowLedgerTables st in
    unstowLedgerTables . mkNewLedgerState <$> updateMockUTxO dummy tx mockState
  where
    -- All expiries in this test are 'DoNotExpire', so the current time is
    -- irrelevant.
    dummy :: SlotNo
    dummy = 0

    mkNewLedgerState mockState' =
      SimpleLedgerState mockState' { mockTip = BlockPoint slot' hash' } emptyLedgerTables

    slot' = case pointSlot $ mockTip (simpleLedgerState st) of
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
