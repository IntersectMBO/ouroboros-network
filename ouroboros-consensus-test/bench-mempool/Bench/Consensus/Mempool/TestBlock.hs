{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Bench.Consensus.Mempool.TestBlock (
    -- * Test block
    TestBlock
    -- * Initial parameters
  , initialLedgerState
  , sampleLedgerConfig
  , sampleMempoolAndModelParams
    -- * Transactions
  , Token (Token)
  , Tx (Tx)
  , mkTx
  , txSize
  ) where

import           Bench.Consensus.MempoolWithMockedLedgerItf
import qualified Cardano.Slotting.Time as Time
import           Codec.Serialise (Serialise (..))
import           Control.DeepSeq (NFData)
import           Control.Monad.Trans.Except (except)
import           Data.Map.Diff.Strict (Diff)
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.TreeDiff (ToExpr)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam as Consensus
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerConfig,
                     LedgerState)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Ouroboros.Consensus.Ledger.Tables (CanSerializeLedgerTables,
                     CanStowLedgerTables, DiffMK (..), EmptyMK, HasLedgerTables,
                     HasTickedLedgerTables, IsMapKind (..), KeysMK (..),
                     LedgerTables, NameMK (..), ValuesMK (..))
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
                     (rawAttachAndApplyDiffs)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Test.Util.TestBlock (LedgerState (TestLedger),
                     PayloadSemantics (PayloadDependentError, PayloadDependentState, applyPayload, getPayloadKeySets),
                     TestBlockWith, Ticked1 (TickedTestLedger),
                     applyDirectlyToPayloadDependentState, lastAppliedPoint,
                     payloadDependentState)

{-------------------------------------------------------------------------------
  MempoolTestBlock
-------------------------------------------------------------------------------}

type TestBlock = TestBlockWith Tx

data Tx = Tx {
    consumed :: !(Set Token)
  , produced :: !(Set Token)
  }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (NoThunks, NFData)

newtype Token = Token { unToken :: Word8  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks, ToExpr, Serialise, NFData)

{-------------------------------------------------------------------------------
  Initial parameters
-------------------------------------------------------------------------------}

initialLedgerState :: IsMapKind mk => LedgerState (TestBlockWith Tx) mk
initialLedgerState = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = TestPLDS Ledger.emptyMK
    }

type instance LedgerCfg (LedgerState TestBlock) = HardFork.EraParams

sampleLedgerConfig :: LedgerConfig TestBlock
sampleLedgerConfig =
  HardFork.defaultEraParams (Consensus.SecurityParam 10) (Time.slotLengthFromSec 2)

sampleMempoolAndModelParams :: InitialMempoolAndModelParams TestBlock
sampleMempoolAndModelParams = MempoolAndModelParams {
      immpInitialState = initialLedgerState
    , immpLedgerConfig = sampleLedgerConfig
    }

{-------------------------------------------------------------------------------
  Payload semantics
-------------------------------------------------------------------------------}

newtype TxApplicationError =
    -- | The transaction could not be applied due to the given unavailable tokens.
    TxApplicationError { unavailable :: Set Token }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance PayloadSemantics Tx where
  newtype instance PayloadDependentState Tx mk = TestPLDS {
      getTestPLDS :: mk Token ()
    }
    deriving stock Generic

  type PayloadDependentError Tx = TxApplicationError

  applyPayload plds tx =
      let
        notFound = Set.filter (not . (`Map.member` tokMap)) consumed
      in if Set.null notFound
        then Right $ TestPLDS (Ledger.rawAttachAndApplyDiffs fullDiff toks)
        else Left  $ TxApplicationError notFound
    where
      TestPLDS toks@(ValuesMK tokMap) = plds
      Tx {consumed, produced}                = tx

      consumedDiff, producedDiff :: Diff Token ()
      consumedDiff = Diff.fromListDeletes [(t, ()) | t <- Set.toList consumed]
      producedDiff = Diff.fromListInserts [(t, ()) | t <- Set.toList produced]

      fullDiff :: DiffMK Token ()
      fullDiff = DiffMK $ consumedDiff <> producedDiff

  getPayloadKeySets tx = TestLedgerTables $ KeysMK $ consumed <> produced
    where
      Tx {consumed, produced} = tx

deriving stock instance (IsMapKind mk)
                        => Eq (PayloadDependentState Tx mk)
deriving stock instance (IsMapKind mk)
                        => Show (PayloadDependentState Tx mk)
deriving anyclass instance (IsMapKind mk)
                        => NoThunks (PayloadDependentState Tx mk)

instance Serialise (PayloadDependentState Tx EmptyMK) where
  encode = error "unused: encode"
  decode = error "unused: decode"

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance Block.CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance Block.StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Ledger tables
-------------------------------------------------------------------------------}

instance HasLedgerTables (LedgerState TestBlock) where
  newtype instance LedgerTables (LedgerState TestBlock) mk =
    TestLedgerTables { getTestLedgerTables :: mk Token () }
    deriving Generic

  projectLedgerTables st =
    TestLedgerTables $ getTestPLDS $ payloadDependentState st
  withLedgerTables st table = st {
        payloadDependentState = plds {
            getTestPLDS = getTestLedgerTables table
          }
      }
    where
      TestLedger { payloadDependentState = plds } = st

  pureLedgerTables = TestLedgerTables
  mapLedgerTables f (TestLedgerTables x) = TestLedgerTables (f x)
  traverseLedgerTables f (TestLedgerTables x) = TestLedgerTables <$> f x
  zipLedgerTables f (TestLedgerTables x) (TestLedgerTables y) =
    TestLedgerTables (f x y)
  zipLedgerTables3 f (TestLedgerTables x) (TestLedgerTables y) (TestLedgerTables z) =
    TestLedgerTables (f x y z)
  zipLedgerTablesA f (TestLedgerTables x) (TestLedgerTables y) =
    TestLedgerTables <$> f x y
  zipLedgerTables3A f (TestLedgerTables x) (TestLedgerTables y) (TestLedgerTables z) =
    TestLedgerTables <$> f x y z
  foldLedgerTables f (TestLedgerTables x) = f x
  foldLedgerTables2 f (TestLedgerTables x) (TestLedgerTables y) = f x y
  namesLedgerTables = TestLedgerTables $ NameMK "benchmempooltables"

instance CanSerializeLedgerTables (LedgerState TestBlock) where
  codecLedgerTables = error "unused: codecLedgerTables"

deriving stock instance IsMapKind mk
                     => Eq (LedgerTables (LedgerState TestBlock) mk)
deriving stock instance IsMapKind mk
                     => Show (LedgerTables (LedgerState TestBlock) mk)
deriving anyclass instance IsMapKind mk
                        => NoThunks (LedgerTables (LedgerState TestBlock) mk)

instance CanStowLedgerTables (LedgerState TestBlock) where
  stowLedgerTables     = error "unused: stowLedgerTables"
  unstowLedgerTables   = error "unused: unstowLedgerTables"

instance HasTickedLedgerTables (LedgerState TestBlock) where
  projectLedgerTablesTicked (TickedTestLedger st) =
    Ledger.projectLedgerTables st
  withLedgerTablesTicked (TickedTestLedger st) tables =
    TickedTestLedger $ Ledger.withLedgerTables st tables

{-------------------------------------------------------------------------------
  Mempool support
-------------------------------------------------------------------------------}

newtype instance Ledger.GenTx TestBlock = TestBlockGenTx { unGenTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks, Eq, Ord, NFData)

-- | For the mempool tests and benchmarks it is not imporant that we calculate
-- the actual size of the transaction in bytes.
txSize :: Ledger.GenTx TestBlock -> Mempool.TxSizeInBytes
txSize (TestBlockGenTx tx) = fromIntegral $ 1 + length (consumed tx) + length (produced tx)

mkTx ::
     [Token]
     -- ^ Consumed
  -> [Token]
     -- ^ Produced
  -> Ledger.GenTx TestBlock
mkTx cons prod = TestBlockGenTx $ Tx { consumed = Set.fromList cons
                                     , produced = Set.fromList prod
                                     }

instance Ledger.LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $ fmap (, ValidatedGenTx (TestBlockGenTx tx))
           $ applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt =
    fst <$> Ledger.applyTx cfg Ledger.DoNotIntervene slot genTx tickedSt
    -- FIXME: it is ok to use 'DoNotIntervene' here?

  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  txsMaxBytes _ = 20

  txInBlockSize = txSize

  txForgetValidated (ValidatedGenTx tx) = tx

  getTransactionKeySets (TestBlockGenTx tx) = TestLedgerTables $
    KeysMK $ consumed tx

newtype instance Ledger.TxId (Ledger.GenTx TestBlock) = TestBlockTxId Tx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance Ledger.HasTxId (Ledger.GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Ledger.Validated (Ledger.GenTx TestBlock) =
    ValidatedGenTx (Ledger.GenTx TestBlock)
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

type instance Ledger.ApplyTxErr TestBlock = TxApplicationError
