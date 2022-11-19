{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
-- |

module Test.Consensus.Mempool.StateMachine.TestBlock (tests) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar)
import           Control.Monad.Trans.Except (except)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Data.TreeDiff.Class (ToExpr)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Test.QuickCheck (Arbitrary, arbitrary, frequency, shrink)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Tracer

import           Cardano.Slotting.Slot (SlotNo (SlotNo))
import           Cardano.Slotting.Time (slotLengthFromSec)


import           Ouroboros.Consensus.Block.Abstract (CodecConfig, StorageConfig)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.HardFork.History (defaultEraParams)
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)
import           Ouroboros.Consensus.Util.IOLike (newTVarIO, readTVar)

import           Ouroboros.Network.Block (Point (Point), pattern BlockPoint,
                     pattern GenesisPoint)

import           Test.Util.Orphans.Arbitrary ()

import           Test.Consensus.Mempool.StateMachine
                     (openMempoolWithMockedLedgerItf, prop_sequential)

import           Test.Util.TestBlock (LedgerState (TestLedger),
                     PayloadSemantics (PayloadDependentError, PayloadDependentState, applyPayload),
                     TestBlockWith, applyDirectlyToPayloadDependentState,
                     lastAppliedPoint, payloadDependentState)

import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)

tests :: TestTree
tests = testGroup "Mempool State Machine" [
      testProperty "Sequential" (prop_sequential mOpenMempool)
    ]
  where
    mOpenMempool =
      let
        -- Fixme: see how and whether this is used.
        cfg :: LedgerConfig TestBlock
        cfg = defaultEraParams (SecurityParam 10) (slotLengthFromSec 2)

        capacityOverride :: MempoolCapacityBytesOverride
        capacityOverride = NoMempoolCapacityBytesOverride -- TODO we might want to generate this

        tracer :: Tracer IO (TraceEventMempool TestBlock)
        tracer = nullTracer
      in openMempoolWithMockedLedgerItf cfg capacityOverride tracer txSize

-- TODO: consider removing this level of indirection
type TestBlock = TestBlockWith Tx

newtype instance GenTx TestBlock = TestBlockGenTx Tx
  deriving stock (Generic)
  deriving newtype (Show, Arbitrary, Ord, Eq, NoThunks)

-- For the mempool tests it is not imporant that we calculate the actual size of the transaction in bytes
txSize :: GenTx TestBlock -> TxSizeInBytes
txSize (TestBlockGenTx tx) = fromIntegral $ 1 + length (produced tx)

data Tx = Tx
  { consumed :: Set Token
  , produced :: Set Token
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

-- Tokens from a small universe
newtype Token = Token { unToken :: Word8  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance Arbitrary Tx where
  arbitrary = Tx <$> arbitrary <*> arbitrary

  shrink Tx {produced, consumed} = uncurry Tx <$> shrink (consumed, produced)

--------------------------------------------------------------------------------
-- Payload semantics
--------------------------------------------------------------------------------

data TestLedgerState = TestLedgerState {
    availableTokens :: Set Token
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance Arbitrary TestLedgerState where
  arbitrary = TestLedgerState <$> arbitrary
  shrink    = fmap TestLedgerState . shrink . availableTokens

instance Arbitrary (LedgerState TestBlock) where
  arbitrary = TestLedger <$> arbitrary <*> arbitrary

instance Arbitrary (Point TestBlock) where
  arbitrary = frequency [ (1, pure GenesisPoint)
                        , (9, BlockPoint <$> arbitrary <*> arbitrary)
                        ]

  shrink GenesisPoint           = []
  shrink (BlockPoint slot hash) = fmap (uncurry BlockPoint) $ shrink (slot, hash)

-- FIXME: I'm sure somebody already defined this
instance Arbitrary SlotNo where
  arbitrary = SlotNo <$> arbitrary
  shrink (SlotNo s) = fmap SlotNo $ shrink s

initialLedgerState :: LedgerState (TestBlockWith Tx)
initialLedgerState = TestLedger {
      lastAppliedPoint      = GenesisPoint
    , payloadDependentState = TestLedgerState {
          availableTokens = Set.empty :: Set Token
        }
    }

data TxApplicationError =
    -- | The transaction could not be applied due to the given unavailable tokens.
    TxApplicationError { unavailable :: Set Token }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance PayloadSemantics Tx where
  type PayloadDependentState Tx = TestLedgerState

  type PayloadDependentError Tx = TxApplicationError

  applyPayload st@TestLedgerState { availableTokens } Tx { consumed, produced } =
    let
      notFound = Set.filter (not . (`Set.member` availableTokens)) consumed
    in if Set.null notFound
       then Right $ st{ availableTokens = availableTokens \\ consumed <> produced }
       else Left  $ TxApplicationError notFound

-- | TODO: for the time being 'TestBlock' does not have any codec config
data instance CodecConfig TestBlock = TestBlockCodecConfig
  deriving (Show, Generic, NoThunks)

-- | TODO: for the time being 'TestBlock' does not have any storage config
data instance StorageConfig TestBlock = TestBlockStorageConfig
  deriving (Show, Generic, NoThunks)

--------------------------------------------------------------------------------
-- TestBlock mempool support
--------------------------------------------------------------------------------

instance LedgerSupportsMempool TestBlock where
  applyTx _cfg _shouldIntervene _slot (TestBlockGenTx tx) tickedSt =
    except $ fmap (, ValidatedGenTx (TestBlockGenTx tx))
           $ applyDirectlyToPayloadDependentState tickedSt tx

  reapplyTx cfg slot (ValidatedGenTx genTx) tickedSt =
    fst <$> applyTx cfg DoNotIntervene slot genTx tickedSt
    -- FIXME: it is ok to use 'DoNotIntervene' here?

  -- FIXME: determine a suitable value for this. In particular, if the mempool
  -- capacity depends on this, we might have to tweak it to make sure we
  -- exercise the code-path that deals with a full mempool.
  txsMaxBytes _ = 200

  txInBlockSize = txSize

  txForgetValidated (ValidatedGenTx tx) = tx

newtype instance TxId (GenTx TestBlock) = TestBlockTxId Tx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance HasTxId (GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Validated (GenTx TestBlock) = ValidatedGenTx (GenTx TestBlock)
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

type instance ApplyTxErr TestBlock = TxApplicationError
