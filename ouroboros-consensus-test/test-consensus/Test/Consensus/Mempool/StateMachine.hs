{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Test.Consensus.Mempool.StateMachine (
    openMempoolWithMockedLedgerItf
  , prop_sequential
  , showMempoolTestScenarios
  , stateMachine
  ) where

import           Control.Monad.Class.MonadSTM.Strict (StrictTVar)
import           Control.Tracer (Tracer)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Kind (Type)
import           Data.List (partition)
import           Data.TreeDiff.Class (ToExpr)
import           GHC.Generics (Generic, Generic1)
import           NoThunks.Class (NoThunks)

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Monadic as QCM
import           Test.StateMachine (Reference, StateMachine (StateMachine),
                     showLabelledExamples)
import           Test.StateMachine.ConstructorName
                     (CommandNames (cmdName, cmdNames))
import           Test.StateMachine.Labelling (Event, eventResp)
import           Test.StateMachine.Logic (Logic (Top), (.==))
import           Test.StateMachine.Sequential (checkCommandNames,
                     forAllCommands, prettyCommands, runCommands)
import           Test.StateMachine.Types (GenSym, Reason (Ok), Symbolic,
                     concrete, genSym, noCleanup, reference)
import qualified Test.StateMachine.Types as SMT
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.StateMachine.Types.References (Concrete)

import           Ouroboros.Consensus.HeaderValidation (ValidateEnvelope)
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (DoNotIntervene))
import           Ouroboros.Consensus.Util.IOLike (newTVarIO, readTVar,
                     readTVarIO)

-- SUT
import           Data.Maybe (fromMaybe)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)
import           Text.Read (Lexeme (Char))

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- | A model of the mempool.
--
-- Mock functions operate on this model.
data Model blk (r :: Type -> Type) = Model {
      currentLedgerDBState :: LedgerState blk
    }
  deriving stock (Generic)

deriving stock instance (Eq   (LedgerState blk)) => Eq   (Model blk r)
deriving stock instance (Show (LedgerState blk)) => Show (Model blk r)

initModel :: LedgerState blk -> Model blk r

initModel initialState = Model initialState

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- We call the commands or actions of the state machine "events": this captures
-- the fact that the change of ledger state at the tip of the chain is an event
-- (external to the mempool).
--
-- The mempool is generalized over blocks, so there is not much we can do about
-- it.
data Cmd blk (r :: Type -> Type) =
      TryAddTxs [GenTx blk]
    | SetLedgerState (LedgerState blk)
    -- ^ Set the ledger state returned by the ledger interface mock to the given
    -- state.
  deriving stock (Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance (Show (GenTx blk), Show (LedgerState blk)) => Show (Cmd blk r)

-- | Successful command responses
data SuccessfulResponse blk (ref :: Type -> Type) =
      TryAddTxsSuccess (TryAddTxsResult blk)
    | RespOk
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk)) => Show (SuccessfulResponse blk ref)

data TryAddTxsResult blk = TryAddTxsResult {
      valid       :: [GenTx blk]
    , invalid     :: [GenTx blk]
    , unprocessed :: [GenTx blk]
    }

deriving stock instance Show (GenTx blk) => Show (TryAddTxsResult blk)

newtype Response blk ref = Response (SuccessfulResponse blk ref)
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk), Show (SuccessfulResponse blk ref)) => Show (Response blk ref)

-- | TODO: for now we use a mock reference as a placeholder for mock references
-- that might be used in the tests.
data MockRef = MockRef
  deriving (Eq, Show)

-- | Placeholder for actual references, ie, references returned by the mempool.
data ActualRef = ActualRef
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Command generation
-------------------------------------------------------------------------------}

generator ::
     (QC.Arbitrary (GenTx blk), QC.Arbitrary (LedgerState blk))
  => Model blk Symbolic -> Maybe (Gen (Cmd blk Symbolic))
generator (Model _      ) = Just
                          $ QC.frequency [ (100, fmap TryAddTxs QC.arbitrary)
                                         , (10,  genLedgerState)
                                           -- TODO: if the model is bootstrapped with an "empty"
                                           -- ledger state we will generate a lot of transactions
                                           -- that will simply be rejected. Maybe we can force
                                           -- the generator to always generate a 'SetLedgerState'
                                           -- action if this is uninitialized.
                                         ]

genLedgerState :: QC.Arbitrary (LedgerState blk) => Gen (Cmd blk Symbolic)
genLedgerState = fmap SetLedgerState QC.arbitrary

shrinker :: Model blk Symbolic -> Cmd blk Symbolic -> [Cmd blk Symbolic]
shrinker _model _event = [] -- TODO

{-------------------------------------------------------------------------------
  State machine
-------------------------------------------------------------------------------}

transition :: Model blk r -> Cmd blk r -> (Response blk) r -> Model blk r
transition model _event _response = model -- TODO

-- | TODO Here is where we call the actual mempool commands
semantics :: forall blk idx.
     LedgerSupportsMempool blk
  => MempoolWithMockedLedgerItf IO blk idx
  -> Cmd blk Concrete
  -> IO ((Response blk) Concrete)
semantics mempoolWithMockedLedgerItf (TryAddTxs txs) = do
    (processed, toProcess) <- tryAddTxs (getMempool mempoolWithMockedLedgerItf)
                                        DoNotIntervene
                                        txs -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
    let (added, rejected) = partition isMempoolTxAdded processed
    pure $! Response
         $! TryAddTxsSuccess
           TryAddTxsResult
              { valid       = fmap getTx added
              , invalid     = fmap getTx rejected
              , unprocessed = [] -- TODO
              }
  where
    getTx (MempoolTxAdded vtx) = txForgetValidated vtx

precondition :: Model blk Symbolic -> Cmd blk Symbolic -> Logic
precondition _model _event = Top

postcondition ::
     Model    blk Concrete
  -> Cmd      blk Concrete
  -> Response tx  Concrete
  -> Logic
postcondition _model _event _response = Top

mock  :: forall blk tx . Model blk Symbolic -> Cmd blk Symbolic -> GenSym (Response tx Symbolic)
mock _model _event = pure
                     $ Response
                     $ TryAddTxsSuccess
                       TryAddTxsResult
                       { valid       = [] -- TODO
                       , invalid     = [] -- TODO
                       , unprocessed = [] -- TODO
                       }

stateMachineIO ::
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , LedgerSupportsMempool blk
     )
  => MempoolWithMockedLedgerItf IO blk idx
  -> IO (StateMachine (Model blk) (Cmd blk) IO (Response blk))
stateMachineIO mempool = do
  initialState <- readTVarIO (getLedgerStateTVar mempool)
  pure $ stateMachine initialState mempool

-- | State machine for which we do not have a mempool, and therefore we cannot
-- use the SUT.
stateMachineWithoutSUT initialState = stateMachine initialState err
  where err = error $  "The SUT should not be used in this state machine:"
                    <> " there is no semantics defined for this state machine."

stateMachine initialState mempool = StateMachine
  { SMT.initModel     = initModel initialState
  , SMT.transition    = transition
  , SMT.precondition  = precondition
  , SMT.postcondition = postcondition
  , SMT.generator     = generator
  , SMT.shrinker      = shrinker
  , SMT.semantics     = semantics mempool
  , SMT.mock          = mock
  , SMT.invariant     = Nothing
  , SMT.cleanup       = noCleanup
  }

-- To run the property we require both a mempool, and a @TVar@ that can be used
-- to modifi
prop_sequential :: forall blk idx.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , ToExpr (LedgerState blk)
     , LedgerSupportsMempool blk
     )
  => (LedgerState blk ->  IO (MempoolWithMockedLedgerItf IO blk idx))
  -> LedgerState blk
  -> QC.Property
prop_sequential mempoolWithMockedLedgerItfAct initialState = do
  -- TODO: even though the mocked LedgerDB interface has an initial state, we
  -- generate the commands in such a way that the first action is _always_ set
  -- an intial state. This seems to make the code more brittle than it ought to
  -- be. However it is not possible to
  --
    forAllCommands (stateMachineWithoutSUT initialState) Nothing $ \cmds -> QCM.monadicIO $ do
        modelWithSUT <- QCM.run $ do
          mempool <- mempoolWithMockedLedgerItfAct initialState
          stateMachineIO mempool
        (hist, _model, res) <- runCommands modelWithSUT cmds
        prettyCommands modelWithSUT hist -- TODO: check if we need the mempool in 'prettyCommands'
          $ checkCommandNames cmds
          $ res QC.=== Ok
  where
    err = error "The actual mempool should not needed when generating commands."

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

-- Show the labelled examples. In particular:
--
-- Mempool with at least N accepted transactions
-- Mempool with at least N rejected transactions
--
-- https://hackage.haskell.org/package/quickcheck-state-machine-0.7.1/docs/Test-StateMachine-Labelling.html#t:Event
-- https://hackage.haskell.org/package/quickcheck-state-machine-0.7.1/docs/Test-StateMachine.html#v:showLabelledExamples-39-
--
-- showLabelledExamples
--   :: (Show tag, Show (model Symbolic))
--   => (Show (cmd Symbolic), Show (resp Symbolic))
--   => (Traversable cmd, Foldable resp)
--   => StateMachine model cmd m resp -> ([Event model cmd resp Symbolic] -> [tag]) -> IO ()
showMempoolTestScenarios :: forall blk.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Show (LedgerState blk)
     , LedgerSupportsMempool blk
     )
   => LedgerState blk -> IO ()
showMempoolTestScenarios initialState =
  showLabelledExamples (stateMachineWithoutSUT initialState) tagEventSeq
  where
    tagEventSeq :: [Event (Model blk) (Cmd blk) (Response blk) Symbolic] -> [Tag]
    tagEventSeq events = validTransactions events
      where
        -- Produce a AcceptedTransactions if one of the event contain a response with an accepted transaction
        validTransactions [] = [] --
        validTransactions (ev : evs) =
          case validTransactionsInResponse (eventResp ev) of
            [] -> validTransactions evs   -- We keep on looking
            _  -> [AcceptedTransactions] -- The list of events (trace) contained a valid transaction

validTransactionsInResponse :: Response blk ref -> [GenTx blk]
validTransactionsInResponse (Response (TryAddTxsSuccess res)) = valid res
validTransactionsInResponse _                                 = []

data Tag = AcceptedTransactions
  deriving (Show)
-- .... Here we don't use the actual mempool, BUT we do need a initial ledger state!

{------------------------------------------------------------------------------
  Mempool with a mocked ledger interface
------------------------------------------------------------------------------}

-- The idea of this data structure is that we make sure that the ledger
-- interface used by the mempool gets mocked in the right way.
--
data MempoolWithMockedLedgerItf m blk idx = MempoolWithMockedLedgerItf {
      getLedgerInterface :: LedgerInterface m blk
    , getLedgerStateTVar :: StrictTVar m (LedgerState blk) -- TODO: define setters and getters for this
    , getMempool         :: Mempool m blk idx
    }

openMempoolWithMockedLedgerItf::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerCfg (LedgerState blk)
  -> MempoolCapacityBytesOverride
  -> Tracer IO (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> LedgerState blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MempoolWithMockedLedgerItf IO blk TicketNo)
openMempoolWithMockedLedgerItf cfg capacityOverride tracer txSize initialState = do
    currentLedgerStateTVar <- newTVarIO initialState
    let ledgerItf = LedgerInterface {
            getCurrentLedgerState = readTVar currentLedgerStateTVar
        }
    mempool <- openMempoolWithoutSyncThread ledgerItf cfg capacityOverride tracer txSize
    pure MempoolWithMockedLedgerItf {
        getLedgerInterface = ledgerItf
      , getLedgerStateTVar = currentLedgerStateTVar
      , getMempool         = mempool
    }

{-------------------------------------------------------------------------------
  Instances required to run the state machine
-------------------------------------------------------------------------------}

deriving anyclass instance ToExpr (LedgerState blk) => ToExpr (Model blk Concrete)
