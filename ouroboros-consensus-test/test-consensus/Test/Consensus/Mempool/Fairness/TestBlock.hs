{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Consensus.Mempool.Fairness.TestBlock (
    TestBlock
  , TestBlock.PayloadDependentState (..)
  , Tx
  , genTxSize
  , mkGenTx
  , txSize
  , unGenTx
  ) where

import           Codec.Serialise
import           Control.DeepSeq (NFData)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Ledger.Abstract
                     (LedgerTablesAreTrivial (convertMapKind))
import qualified Ouroboros.Consensus.Ledger.Abstract as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Test.Util.TestBlock as TestBlock
import           Test.Util.TestBlock (TestBlockWith)

type TestBlock = TestBlockWith Tx
  -- We use 'Test.Util.TestBlock' because, even though it contains a lot of
  -- information we do not actually need for the mempool fairness tests, it
  -- already defines most of the many type classes that are needed to open a
  -- mempool.

-- | The fairness test for transaction sizes only cares about said aspect.
--
-- We do need to keep track of the transaction id.
--
-- All transactions will be accepted by the mempool.
data Tx = Tx { txNumber :: Int, txSize ::  Word32 }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

instance TestBlock.PayloadSemantics Tx where
  data instance PayloadDependentState Tx mk = NoPayLoadDependentState
    deriving (Show, Eq, Ord, Generic, NoThunks)
    deriving anyclass Serialise

  type PayloadDependentError Tx = ()

  applyPayload NoPayLoadDependentState _tx = Right NoPayLoadDependentState

  getPayloadKeySets = const NoTestLedgerTables


data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)


{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx { unGenTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

newtype instance Ledger.Validated (Ledger.GenTx TestBlock) =
    ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

genTxSize :: Ledger.GenTx TestBlock -> Word32
genTxSize = txSize . unGenTx

mkGenTx :: Int -> Word32 -> Ledger.GenTx TestBlock
mkGenTx anId aSize = TestBlockGenTx $ Tx { txNumber = anId, txSize = aSize }

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot gtx st = pure (
        TestBlock.TickedTestLedger
      $ convertMapKind
      $ TestBlock.getTickedTestLedger
        st
    , ValidatedGenTx gtx
    )

  reapplyTx _cfg _slot _gtx gst = pure
    $ TestBlock.TickedTestLedger
    $ convertMapKind
    $ TestBlock.getTickedTestLedger
      gst

  txsMaxBytes _ = error "The tests should override this value"
                  -- The tests should be in control of the mempool capacity,
                  -- since the judgement on whether the mempool is fair depends
                  -- on this parameter.

  txInBlockSize = txSize . unGenTx

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets _ = NoTestLedgerTables

{-------------------------------------------------------------------------------
  Ledger support (empty tables)
-------------------------------------------------------------------------------}

type instance Ledger.ApplyTxErr TestBlock = ()

instance Ledger.HasLedgerTables (Ledger.LedgerState TestBlock) where
  data LedgerTables (Ledger.LedgerState TestBlock) mk = NoTestLedgerTables
    deriving stock    (Generic, Eq, Show)
    deriving anyclass (NoThunks)

instance Ledger.LedgerTablesAreTrivial (Ledger.LedgerState TestBlock) where
  convertMapKind st = TestBlock.TestLedger lap NoPayLoadDependentState
    where
        TestBlock.TestLedger lap NoPayLoadDependentState = st
  trivialLedgerTables = NoTestLedgerTables

instance Ledger.CanStowLedgerTables (Ledger.LedgerState TestBlock) where

instance Ledger.HasTickedLedgerTables (Ledger.LedgerState TestBlock) where
  withLedgerTablesTicked (TestBlock.TickedTestLedger st) tables =
      TestBlock.TickedTestLedger $ Ledger.withLedgerTables st tables

instance Ledger.CanSerializeLedgerTables (Ledger.LedgerState TestBlock) where
