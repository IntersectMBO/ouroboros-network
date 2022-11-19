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
import           Test.StateMachine (Reference, StateMachine (StateMachine))
import           Test.StateMachine.ConstructorName
                     (CommandNames (cmdName, cmdNames))
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
import           Ouroboros.Consensus.Util.IOLike (newTVarIO, readTVar)

-- SUT
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
data Model (r :: Type -> Type) = Model
    deriving (Eq, Show, Generic)

initModel :: Model r
initModel = Model

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- We call the commands or actions of the state machine "events": this captures
-- the fact that the change of ledger state at the tip of the chain is an event
-- (external to the mempool).
--
-- The mempool is generalized over blocks, so there is not much we can do about
-- it.
data Event blk (r :: Type -> Type) = TryAddTxs [GenTx blk]
  deriving stock (Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance Show (GenTx blk) => Show (Event blk r)

-- | Successful command responses
data SuccessfulResponse blk (ref :: Type -> Type) =
    TryAddTxsSuccess (TryAddTxsResult blk)
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
     QC.Arbitrary (GenTx blk)
  => Model Symbolic -> Maybe (Gen (Event blk Symbolic))
generator _ = Just $ fmap TryAddTxs QC.arbitrary

shrinker :: Model Symbolic -> Event blk Symbolic -> [Event blk Symbolic]
shrinker _model _event = [] -- TODO

{-------------------------------------------------------------------------------
  State machine
-------------------------------------------------------------------------------}

transition :: Model r -> Event blk r -> (Response blk) r -> Model r
transition _model _event _response = Model

-- | TODO Here is where we call the actual mempool commands
semantics :: forall blk idx.
     LedgerSupportsMempool blk
  => MempoolWithMockedLedgerItf IO blk idx
  -> Event blk Concrete
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

precondition :: Model Symbolic -> Event blk Symbolic -> Logic
precondition _model _event = Top

postcondition ::
     Model        Concrete
  -> Event    blk Concrete
  -> Response tx  Concrete
  -> Logic
postcondition _model _event _response = Top

mock  :: forall blk tx . Model Symbolic -> Event blk Symbolic -> GenSym (Response tx Symbolic)
mock _model _event = pure
                     $ Response
                     $ TryAddTxsSuccess
                       TryAddTxsResult
                       { valid       = [] -- TODO
                       , invalid     = [] -- TODO
                       , unprocessed = [] -- TODO
                       }

stateMachine ::
     ( QC.Arbitrary (GenTx blk)
     , LedgerSupportsMempool blk
     )
  => MempoolWithMockedLedgerItf IO blk idx -> StateMachine Model (Event blk) IO (Response blk)
stateMachine mempool = StateMachine
  { SMT.initModel     = initModel
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
prop_sequential ::
     ( QC.Arbitrary (GenTx blk)
     , LedgerSupportsMempool blk
     )
  => (LedgerState blk -> IO (MempoolWithMockedLedgerItf IO blk idx))
  ->  LedgerState blk
  -> QC.Property
prop_sequential mempoolWithMockedLedgerItfAct initialState =
    forAllCommands (stateMachine err) Nothing $ \cmds ->
      QCM.monadicIO $ do
        mempool <- QCM.run $ mempoolWithMockedLedgerItfAct initialState
        (hist, _model, res) <- runCommands (stateMachine mempool) cmds
        prettyCommands (stateMachine mempool) hist -- TODO: check if we need the mempool in 'prettyCommands'
          $ checkCommandNames cmds
          $ res QC.=== Ok
  where
    err = error "The actual mempool should not needed when generating commands."

{------------------------------------------------------------------------------
  Mempool with a mocked ledger interface
------------------------------------------------------------------------------}

-- The idea of this data structure is that we make sure that the ledger
-- interface used by the mempool gets mocked in the right way.
data MempoolWithMockedLedgerItf m blk idx = MempoolWithMockedLedgerItf {
      getLedgerInterface :: LedgerInterface m blk
      -- | TVar that should be used to change the current ledger state that the
      -- mempool will see if it asks the ledger interface for the current ledger
      -- state.
    , getLedgerStateTVar :: StrictTVar m (LedgerState blk)
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
     -- ^ Initial ledger state
  -> IO (MempoolWithMockedLedgerItf IO blk TicketNo)
openMempoolWithMockedLedgerItf cfg capacityOverride tracer txSize initialLedgerState = do
    currentLedgerStateTVar <- newTVarIO initialLedgerState

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

instance ToExpr (Model Concrete)
