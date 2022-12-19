{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- TODO: explain how to use showMempoolTestScenarios in combination with
-- initialLedgerState in the REPL.
--
-- >>> import Test.Consensus.Mempool.StateMachine.TestBlock
-- >>> import Test.Consensus.Mempool.StateMachine
-- >>> showMempoolTestScenarios sampleMempoolAndModelParams
--
module Test.Consensus.Model.TestBlock (
    initialLedgerState
  , sampleMempoolAndModelParams
  , tests
    -- * Ledger state
  , TestLedgerState (TestLedgerState, availableTokens)
  , Token (Token, unToken)
    -- * Test transaction
  , GenTx (..)
  , Tx (Tx, produced, consumed)
  , Validated (..)
  , genValidTx
  , genValidTxs
  , txSize
    -- * Labelling
  , TestBlock
  , tagConsumedTx
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Trans.Except (except)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Data.TreeDiff.Class (ToExpr, defaultExprViaShow, toExpr)
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, frequency,
                     scale, shrink, sublistOf)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Tracer (Tracer, nullTracer)

import           Cardano.Slotting.Slot (EpochSize, SlotNo (SlotNo))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Data.Time.Clock (NominalDiffTime)

import           Ouroboros.Consensus.Block.Abstract (CodecConfig, StorageConfig)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.HardFork.History (defaultEraParams)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig)

import           Ouroboros.Network.Block (pattern BlockPoint,
                     pattern GenesisPoint)

import           Test.Util.Orphans.Arbitrary ()

import           Test.Consensus.Mempool.StateMachine
                     (InitialMempoolAndModelParams (MempoolAndModelParams, immpInitialState, immpLedgerConfig),
                     openMempoolWithMockedLedgerItf, prop_parallel,
                     prop_sequential)

import           Test.Util.TestBlock (LedgerState (TestLedger),
                     PayloadSemantics (PayloadDependentError, PayloadDependentState, applyPayload),
                     TestBlockWith, applyDirectlyToPayloadDependentState,
                     lastAppliedPoint, payloadDependentState)

import           Ouroboros.Consensus.Block (Point)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     HasTxId (..),
                     LedgerSupportsMempool (applyTx, reapplyTx, txForgetValidated, txInBlockSize, txsMaxBytes),
                     TxId, Validated, WhetherToIntervene (DoNotIntervene))
import           Ouroboros.Consensus.Mempool.API
                     (MempoolCapacityBytesOverride (NoMempoolCapacityBytesOverride),
                     TraceEventMempool, TxSizeInBytes)
import           Ouroboros.Consensus.Mempool.Impl ()
import           Test.QuickCheck (suchThat)

tests :: TestTree
tests = testGroup "Mempool State Machine" [
      testProperty "Sequential" (prop_sequential mOpenMempool)
    , testProperty "Parallel"   (prop_parallel mOpenMempool)
    ]
  where
    mOpenMempool =
      let
        capacityOverride :: MempoolCapacityBytesOverride
        capacityOverride = NoMempoolCapacityBytesOverride -- TODO we might want to generate this

        tracer :: Tracer IO (TraceEventMempool TestBlock)
        tracer = nullTracer
      in openMempoolWithMockedLedgerItf capacityOverride tracer txSize

instance Arbitrary HardFork.EraParams where
  arbitrary = pure $ defaultEraParams (SecurityParam 10) (slotLengthFromSec 2) -- TODO
  shrink _ = [] -- TODO

deriving instance ToExpr HardFork.EraParams
--  toExpr = App "EraParams" []
deriving anyclass instance ToExpr HardFork.SafeZone
deriving anyclass instance ToExpr EpochSize
deriving anyclass instance ToExpr SlotLength
instance ToExpr NominalDiffTime where
  toExpr = defaultExprViaShow -- TODO define this properly
deriving anyclass instance ToExpr (GenTx TestBlock)
deriving anyclass instance ToExpr Tx

-- TODO: consider removing this level of indirection
type TestBlock = TestBlockWith Tx

newtype instance GenTx TestBlock = TestBlockGenTx { blockTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, Arbitrary, NoThunks, Eq, Ord)

-- For the mempool tests it is not imporant that we calculate the actual size of the transaction in bytes
txSize :: GenTx TestBlock -> TxSizeInBytes
txSize (TestBlockGenTx tx) = fromIntegral $ 1 + length (consumed tx) + length (produced tx)

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

-- | Generate a valid transaction consuming a subset of available tokens
genValidTx :: Set Token -> Gen Tx
genValidTx available =
  Tx <$> (Set.fromList <$> sublistOf (Set.toList available)) <*> arbitrary

-- | Generate a list of valid transactions consuming _all_ tokens from the
-- the given set and producing _different tokens_
genValidTxs :: Set Token -> Gen [Tx]
genValidTxs toConsume = go toConsume mempty
  where
    go available generated
      | null available = pure []
      | otherwise = do
          n <- choose (1, min (length available) 10)
          let consumed = Set.fromList $ take n  (Set.toList available)
          produced <- arbitrary `suchThat` (\ tokens -> null (tokens  `Set.intersection` generated) && not (null tokens))
          (Tx consumed produced :) <$> go (available Set.\\ consumed) (generated `Set.union` produced)

--------------------------------------------------------------------------------
-- Payload semantics
--------------------------------------------------------------------------------

data TestLedgerState = TestLedgerState {
    availableTokens :: Set Token
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance Arbitrary TestLedgerState where
  arbitrary = TestLedgerState <$> scale (* 10) arbitrary
  shrink    = fmap TestLedgerState . shrink . availableTokens

instance Arbitrary (LedgerState TestBlock) where
  arbitrary = TestLedger <$> arbitrary <*> arbitrary
  shrink (TestLedger x y) = fmap (uncurry TestLedger) $ shrink (x, y)

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

sampleLedgerConfig :: LedgerConfig TestBlock
sampleLedgerConfig = defaultEraParams (SecurityParam 10) (slotLengthFromSec 2)

sampleMempoolAndModelParams :: InitialMempoolAndModelParams TestBlock
sampleMempoolAndModelParams = MempoolAndModelParams {
      immpInitialState = initialLedgerState
    , immpLedgerConfig = sampleLedgerConfig
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

  -- TODO: make it a parameter of `TestBlock` so that we can define the behaviour
  -- of the mempool from within the model
  -- currently unbounded so all valid transactions should end up being in the mempool
  txsMaxBytes _ = maxBound

  txInBlockSize = txSize

  txForgetValidated (ValidatedGenTx tx) = tx

newtype instance TxId (GenTx TestBlock) = TestBlockTxId Tx
  deriving stock (Generic)
  deriving newtype (Show, Ord, Eq)
  deriving anyclass (NoThunks)

instance HasTxId (GenTx TestBlock) where
  txId (TestBlockGenTx tx) = TestBlockTxId tx

newtype instance Validated (GenTx TestBlock) = ValidatedGenTx { fromValidated :: (GenTx TestBlock) }
  deriving stock (Generic)
  deriving newtype (Show, NoThunks)

type instance ApplyTxErr TestBlock = TxApplicationError

{------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

tagConsumedTx :: GenTx TestBlock-> Consumed
tagConsumedTx (TestBlockGenTx Tx{consumed}) =
  case Set.size consumed of
    0 -> ConsumedZero
    1 -> ConsumedOne
    _ -> ConsumedMultiple

data Consumed = ConsumedZero
              | ConsumedOne
              | ConsumedMultiple
  deriving Show
