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
-- |
-- TODO: explain how to use showMempoolTestScenarios in combination with
-- initialLedgerState in the REPL.
--
-- >>> import Test.Consensus.Mempool.StateMachine.TestBlock
-- >>> import Test.Consensus.Mempool.StateMachine
-- >>> showMempoolTestScenarios sampleMempoolAndModelParams
--
module Test.Consensus.Mempool.StateMachine.TestBlock (
    initialLedgerState
  , sampleMempoolAndModelParams
  , tests
    -- * Ledger state
  , TestLedgerState (TestLedgerState, availableTokens)
  , Token (Token, unToken)
    -- * Test transaction
  , GenTx (TestBlockGenTx)
  , Tx (Tx, produced, consumed)
    -- * Examples
  , addTwoTrivialTxsInOneCmd
    -- * Labelling
  , atLeastThreeNonTrivialValidatedTxs
  , atLeastTwoNonTrivialAddTxsMultiple
  , atLeastTwoNonTrivialValidatedTxs
  , labelTxInputs
  , tagConsumedTx
  ) where

import           Codec.Serialise (Serialise)
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar)
import           Control.Monad.Trans.Except (except)
import           Data.Set (Set, (\\))
import qualified Data.Set as Set
import           Data.TreeDiff.Class (ToExpr, defaultExprViaShow, genericToExpr,
                     toExpr)
import           Data.Word (Word8)
import           GHC.Exts (fromList)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Text.Show.Pretty (pPrint)

import           Test.QuickCheck (Arbitrary, Property, arbitrary, collect,
                     elements, frequency, listOf1, oneof, quickCheck, shrink,
                     tabulate, withMaxSuccess)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

-- TOOO: maybe we can avoid this dependency on state machine by having a custom way to represent events.
import           Test.StateMachine.Labelling (Event (Event), eventAfter,
                     eventBefore, eventCmd, eventResp)
import           Test.StateMachine.Types (Command (Command),
                     Commands (Commands), GenSym, Reason (Ok), Symbolic,
                     concrete, noCleanup, reference, unCommands)


import           Control.Tracer

import           Cardano.Slotting.Slot (EpochSize, SlotNo (SlotNo))
import           Cardano.Slotting.Time (SlotLength, slotLengthFromSec)
import           Data.Time.Clock (NominalDiffTime)

import           Ouroboros.Consensus.Block.Abstract (CodecConfig, StorageConfig)
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import           Ouroboros.Consensus.HardFork.History (defaultEraParams)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerConfig)
import           Ouroboros.Consensus.Util.IOLike (newTVarIO, readTVar)

import           Ouroboros.Network.Block (Point (Point), pattern BlockPoint,
                     pattern GenesisPoint)

import           Test.Util.Orphans.Arbitrary ()

import           Test.Consensus.Mempool.StateMachine (Cmd (..),
                     InitialMempoolAndModelParams (MempoolAndModelParams, immpInitialState, immpLedgerConfig),
                     Model (validatedTransactions), Response (..),
                     collectCommands, openMempoolWithMockedLedgerItf,
                     prop_parallel, prop_sequential, runMempoolCommands,
                     showMempoolTestScenariosWith)

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

newtype instance GenTx TestBlock = TestBlockGenTx { unGenTx :: Tx }
  deriving stock (Generic)
  deriving newtype (Show, Arbitrary, NoThunks, Eq, Ord)

-- For the mempool tests it is not imporant that we calculate the actual size of the transaction in bytes
txSize :: GenTx TestBlock -> TxSizeInBytes
txSize (TestBlockGenTx tx) = fromIntegral $ 1 + length (consumed tx) + length (produced tx)

data Tx = Tx
  { consumed :: Set Token
  , produced :: Set Token
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NoThunks)

instance Show Tx where
  show Tx {produced, consumed} = "[ " <> short produced  <> " -> " <> short consumed <> "]"
    where short xs = show $ fmap unToken $ Set.toList xs

-- Tokens from a small universe
newtype Token = Token { unToken :: Word8  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving anyclass (NoThunks, ToExpr, Serialise)

instance Arbitrary Tx where
  arbitrary = frequency [  (1, Tx     mempty      <$> smallTokens)
                         , (1, Tx <$> smallTokens <*> arbitrary )
                         , (1, Tx <$> arbitrary   <*> arbitrary)
                        ]
    where
      smallToken = Token <$> elements [0..10]
      smallTokens = Set.fromList <$> listOf1 smallToken

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

  -- We tweaked this in such a way that we test the case in which we exceed the
  -- maximum mempool capacity. The value used here depends on 'txInBlockSize'.
  txsMaxBytes _ = 20

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

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Add two trivial transactions in the same TryAddTxs command
addTwoTrivialTxsInOneCmd :: IO ()
addTwoTrivialTxsInOneCmd = do
  (hist, model, res) <- runMempoolCommands mOpenMempool sampleMempoolAndModelParams cmds
  pPrint hist
  pPrint model
  where
    cmds = Commands {
      unCommands =
        [ Command
            (TryAddTxs
               [ TestBlockGenTx (Tx { consumed = fromList [] , produced = fromList [] } )])
            RespOk
            []
        , Command
            (TryAddTxs
               [ TestBlockGenTx (Tx { consumed = fromList [] , produced = fromList [] }) ])
            RespOk
            []
        , Command
            (TryAddTxs
               [ TestBlockGenTx (Tx { consumed = fromList [] , produced = fromList [] }) ])
            RespOk
            []
        , Command GetSnapshot RespOk []
        ]
    }

{------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}
labelTxInputs :: IO ()
labelTxInputs = quickCheck $ withMaxSuccess 1000 $ noOpTx
  where
    noOpTx :: Tx -> Property
    noOpTx Tx { consumed } =
      collect (Set.size consumed) True

-- collectTxInputs :: IO ()
-- collectTxInputs = collectCommands @TestBlock @Symbolic maxConsumedSize
--   where maxConsumedSize (TryAddTxs xs) = maximum $ fmap (Set.size . consumed . unGenTx) xs
--         maxConsumedSize _              = 0

data Tag = -- | There were two commands at least that tried to add more than one
           -- transaction. This is important to make sure we ejercise the
           -- parallel addition of non-singleton transactions.
           AtLeastTwoTryAddTxsMultiple
         | AtLeastTwoNonTrivialValidatedTxs
           deriving (Show)

atLeastTwoNonTrivialAddTxsMultiple :: IO ()
atLeastTwoNonTrivialAddTxsMultiple =
    showMempoolTestScenariosWith sampleMempoolAndModelParams tagger
  where
    tagger :: [Event (Model TestBlock) (Cmd TestBlock) (Response TestBlock) Symbolic] -> [Tag]
    tagger events = [ AtLeastTwoTryAddTxsMultiple | 2 <= Set.size (Set.fromList (fmap (getTxs . eventCmd) (filter tryAddMoreThanOneTxs events))) ]
      where
        getTxs (TryAddTxs xs) = xs
        getTxs  _             = []
        tryAddMoreThanOneTxs event =
          case eventCmd event of
            TryAddTxs xs -> 1 <= Set.size (Set.fromList (filter ((1<=) . Set.size . produced . unGenTx) xs))
            _            -> False

atLeastThreeNonTrivialValidatedTxs :: IO ()
atLeastThreeNonTrivialValidatedTxs =
    showMempoolTestScenariosWith sampleMempoolAndModelParams tagger
  where
    tagger :: [Event (Model TestBlock) (Cmd TestBlock) (Response TestBlock) Symbolic] -> [Tag]
    tagger events = [ AtLeastTwoNonTrivialValidatedTxs | 1 <= length (filter moreThanOneValidatedTx events)]
      where
        moreThanOneValidatedTx event = 1 < length (validatedTransactions (eventAfter event))


-- | TODO: look for non-trivial validated txs
atLeastTwoNonTrivialValidatedTxs :: IO ()
atLeastTwoNonTrivialValidatedTxs =
    showMempoolTestScenariosWith sampleMempoolAndModelParams tagger
  where
    tagger :: [Event (Model TestBlock) (Cmd TestBlock) (Response TestBlock) Symbolic] -> [Tag]
    tagger events = [ AtLeastTwoNonTrivialValidatedTxs | 1 <= length (filter moreThanOneValidatedTx events)]
      where
        moreThanOneValidatedTx event = 1 < length (filter nonTrivial (validatedTransactions (eventAfter event)))
          where
            nonTrivial = not . Set.null . produced . unGenTx

-- Let's count the accepted validated transactions per-pair
newlyAcceptedTxs :: [Model TestBlock r] -> [Int]
newlyAcceptedTxs []                    = [0]
newlyAcceptedTxs models@(_:modelsTail) =
  fmap (max 0 . uncurry (-)) $ zip (numValidatedTxs modelsTail) (numValidatedTxs models)
  where
    numValidatedTxs = fmap (length . validatedTransactions)



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
