{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Mempool.StateMachine (
    openMempoolWithMockedLedgerItf
  , prop_parallel
  , prop_sequential
  , stateMachine
    -- * Model and SUT parameters
  , InitialMempoolAndModelParams (MempoolAndModelParams, immpInitialState, immpLedgerConfig)
    -- * Labelling
  , showMempoolTestScenarios
  ) where

import           Control.Monad (void)
import           Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically),
                     StrictTVar, writeTVar)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Tracer (Tracer)
import           Data.Kind (Type)
import           Data.Monoid (Sum (Sum, getSum))
import qualified Data.Set as Set
import           Data.TreeDiff.Class (ToExpr)
import           GHC.Generics (Generic, Generic1)

import qualified Debug.Trace as Debug

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Monadic as QCM
import           Test.StateMachine (StateMachine (StateMachine),
                     runParallelCommandsNTimes, showLabelledExamples)
import           Test.StateMachine.ConstructorName (CommandNames)
import           Test.StateMachine.Labelling (Event (Event), eventAfter,
                     eventBefore, eventCmd, eventResp)
import           Test.StateMachine.Logic (Logic (Top, (:&&)), (.==))
import           Test.StateMachine.Parallel (forAllParallelCommands,
                     prettyParallelCommands)
import           Test.StateMachine.Sequential (checkCommandNames,
                     forAllCommands, prettyCommands, runCommands)
import           Test.StateMachine.Types (GenSym, Reason (Ok), Symbolic,
                     concrete, noCleanup, reference)
import qualified Test.StateMachine.Types as SMT
import qualified Test.StateMachine.Types.Rank2 as Rank2
import           Test.StateMachine.Types.References (Concrete)

import           Cardano.Slotting.Slot (SlotNo (SlotNo),
                     WithOrigin (At, Origin))

import           Ouroboros.Network.Block (StandardHash, castPoint)

-- TODO: Proxy is well known, re-exporting this seems like a bad idea.
import           Ouroboros.Consensus.Block (Proxy (Proxy))
import           Ouroboros.Consensus.Block.Abstract (pattern NotOrigin)
import           Ouroboros.Consensus.HeaderValidation (BasicEnvelopeValidation,
                     ValidateEnvelope, minimumPossibleSlotNo)
import           Ouroboros.Consensus.Ledger.Abstract (GetTip (getTip), IsLedger,
                     UpdateLedger, ledgerTipSlot)
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerConfig,
                     LedgerState, TickedLedgerState, applyChainTick, getTipSlot)
import           Ouroboros.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (DoNotIntervene))
import           Ouroboros.Consensus.Util.IOLike (newTVarIO, readTVar,
                     readTVarIO)

-- SUT

import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo)

{-------------------------------------------------------------------------------
  Pure model
-------------------------------------------------------------------------------}

-- | A model of the mempool.
--
-- Mock functions operate on this model.
data Model blk (r :: Type -> Type) = Model {
      currentLedgerDBState    :: !(LedgerState blk)
    , forgingFor              :: !SlotNo
    , intermediateLedgerState :: !(TickedLedgerState blk)
      -- | We need to keep the model config around to be able to call the
      -- 'applyTx' from the 'LedgerSupportsMempool' class.
    , modelConfig             :: !(LedgerConfig blk) -- TODO we could make this a parameter of all functions
    , validatedTransactions   :: ![GenTx blk]
      -- | We use an integer for the maximum capacity because we want to detect
      -- if the SUT ever exceeds it.
      --
      -- See 'initModel' for the actual number used as initial mempool capacity.
    , remainingCapacity       :: !Int
    }
  deriving stock (Generic)

deriving stock instance ( Eq (LedgerState blk)
                        , Eq (TickedLedgerState blk)
                        , Eq (LedgerCfg (LedgerState blk))
                        , Eq (GenTx blk)
                        ) => Eq   (Model blk r)
deriving stock instance ( Show (LedgerState blk)
                        , Show (TickedLedgerState blk)
                        , Show (LedgerCfg (LedgerState blk))
                        , Show (GenTx blk)
                        ) => Show (Model blk r)

initModel :: forall blk r.
     ( BasicEnvelopeValidation blk
     , LedgerSupportsMempool blk -- We need this to calculate the mempool's remainig capacity
     )
  => InitialMempoolAndModelParams blk
  -> Model blk r
initModel params = Model {
       currentLedgerDBState    = initialState
     , forgingFor              = initialStateSlot
     , intermediateLedgerState = initialIntermediateLedgerState
     , modelConfig             = cfg
     , validatedTransactions   = []
     , remainingCapacity       = fromIntegral $ 2 * txsMaxBytes initialIntermediateLedgerState
     }
  where
    initialState                        = immpInitialState params
    cfg                                 = immpLedgerConfig params
    (  initialStateSlot
     , initialIntermediateLedgerState ) = tickLedgerState cfg initialState

tickLedgerState :: forall blk.
     ( UpdateLedger blk
     , BasicEnvelopeValidation blk
     )
  => LedgerConfig blk
  -> LedgerState blk
  -> (SlotNo, TickedLedgerState blk)
tickLedgerState cfg st = (tickedSlot, applyChainTick cfg tickedSlot st)
  where
    -- TODO: this is copying the implementation
    tickedSlot = case ledgerTipSlot st of
      Origin      -> minimumPossibleSlotNo (Proxy @blk)
      NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Commands and responses
-------------------------------------------------------------------------------}

data Cmd blk (r :: Type -> Type) =
      TryAddTxs [GenTx blk]
    | SetLedgerState (LedgerState blk)
    -- ^ Set the ledger state returned by the ledger interface mock to the given
    -- state.
    | GetSnapshot
    | SyncWithLedger
  deriving stock (Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance (Show (GenTx blk), Show (LedgerState blk)) => Show (Cmd blk r)

-- | Successful command responses
data Response blk (ref :: Type -> Type) =
      RespOk
    | Snap {
        -- | Transactions that have been sucessfully validated
        --
        -- TODO: do we need to include the ticket logic here?
        snapTxs         :: ![GenTx blk]
        -- | State of the mempool after applying all the transactions in 'snapTxs'
      , snapLedgerState :: !(TickedLedgerState blk)
    }
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk), Show (TickedLedgerState blk)) => Show (Response blk ref)

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

generator :: forall blk.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     )
  => Model blk Symbolic -> Maybe (Gen (Cmd blk Symbolic))
generator _ =
    Just $ QC.frequency [ (50, fmap TryAddTxs QC.arbitrary)
                          -- TODO: if the model is bootstrapped with an "empty"
                          -- ledger state we will generate a lot of transactions
                          -- that will simply be rejected. Maybe we can force
                          -- the generator to always generate a 'SetLedgerState'
                          -- action if this is uninitialized.
                         , (20, pure GetSnapshot)
                         , (15, pure SyncWithLedger)
                         , (15, fmap SetLedgerState QC.arbitrary)
                         ]

shrinker ::
     (QC.Arbitrary (GenTx blk), QC.Arbitrary (LedgerState blk))
  => Model blk Symbolic -> Cmd blk Symbolic -> [Cmd blk Symbolic]
shrinker _model (TryAddTxs txs)     = fmap TryAddTxs      $ QC.shrink txs
shrinker _model (SetLedgerState st) = fmap SetLedgerState $ QC.shrink st
shrinker _model GetSnapshot         = []
shrinker _model SyncWithLedger      = []

{-------------------------------------------------------------------------------
  State machine
-------------------------------------------------------------------------------}

semantics :: forall blk idx.
     LedgerSupportsMempool blk
  => MempoolWithMockedLedgerItf IO blk idx
  -> Cmd blk Concrete
  -> IO ((Response blk) Concrete)
semantics mempoolWithMockedLedgerItf (TryAddTxs txs) = do
    void $ tryAddTxs (getMempool mempoolWithMockedLedgerItf)
                      DoNotIntervene  -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
                      txs
    pure RespOk
semantics mempoolWithMockedLedgerItf (SetLedgerState newSt) = do
  setLedgerState mempoolWithMockedLedgerItf newSt
  pure RespOk
semantics mempoolWithMockedLedgerItf SyncWithLedger = do
  void $ syncWithLedger $ getMempool mempoolWithMockedLedgerItf
  pure RespOk
semantics mempoolWithMockedLedgerItf GetSnapshot = do
  -- TODO: Maybe mempoolWithMockedLedgerItf should implement the mempool API
  snap <- atomically $ getSnapshot $ getMempool mempoolWithMockedLedgerItf
  pure $ Snap {
      snapTxs         = fmap (txForgetValidated . fst) $ snapshotTxs snap
    , snapLedgerState = snapshotLedgerState snap
    }

precondition :: Model blk Symbolic -> Cmd blk Symbolic -> Logic
precondition _model _event = Top

postcondition :: forall blk.
     ( LedgerSupportsMempool blk
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (TickedLedgerState blk)
     )
  => Model    blk Concrete
  -> Cmd      blk Concrete
  -> Response blk Concrete
  -> Logic
postcondition _model TryAddTxs {} RespOk = Top
postcondition model  GetSnapshot  Snap { snapTxs, snapLedgerState } =
      (intermediateLedgerState model .== snapLedgerState)
  :&& (validatedTransactions   model .== snapTxs)
postcondition _model SyncWithLedger RespOk = Top
-- TODO: add an error clause for unexpected cmd/response commands
postcondition _model _cmd _response = Top

mock :: forall blk.
     Model blk Symbolic
  -> Cmd blk Symbolic
  -> GenSym (Response blk Symbolic)
mock _model (TryAddTxs _) = pure $ RespOk
  -- TODO: maybe we should take the mempool capacity into account. If we use
  -- this only to generate commands then this might not be necessary.
mock _model (SetLedgerState _) = pure RespOk
mock model  GetSnapshot        = pure Snap {
      snapTxs         = validatedTransactions model
    , snapLedgerState = intermediateLedgerState model
  }
mock _model SyncWithLedger = pure RespOk

applyTxs :: forall blk r.
     LedgerSupportsMempool blk
  => Model blk r
  -> [GenTx blk]
  -- TODO Comment this and consider replacing the use of a big tuple here.
  -> ([GenTx blk], TickedLedgerState  blk, Int)
applyTxs model txs = go txs [] intermediateLedgerState remainingCapacity
  where
    Model {intermediateLedgerState, remainingCapacity, modelConfig} = model
    go :: [GenTx blk]
       -> [GenTx blk]
       -> TickedLedgerState blk
       -> Int
       -> ([GenTx blk], TickedLedgerState  blk, Int)
    go []       !val !st !rcap = (reverse val, st, rcap)
    go (tx:tys) !val !st !rcap =
      let rcap' = rcap - txSize tx in
      if  rcap' < 0
      then (reverse val, st, rcap)
      else case mockApplyTx modelConfig st tx of
        Valid st'   -> go tys (tx:val)  st' rcap'
        Invalid st' -> go tys val       st' rcap

data TxApplyResult st = Valid st | Invalid st

mockApplyTx :: forall blk.
     (LedgerSupportsMempool  blk)
  => LedgerConfig blk
  -> TickedLedgerState blk
  -> GenTx blk
  -> TxApplyResult (TickedLedgerState blk)
mockApplyTx cfg st tx =
  case runExcept $ applyTx cfg DoNotIntervene slot tx st of
    -- TODO we also need to consider if we have space left
    Left _               -> Invalid st
    Right (tickedSt', _) -> Valid tickedSt'
  where
    -- FIXME: here we have to cast a WithOrigin SlotNo to SlotNo!!! Is the
    -- intermediateLedgerState guaranteed not to be at the origin given the way
    -- we initialize the mempool?
    At slot = getTipSlot st

transition ::
  ( Show (TickedLedgerState blk)
  , LedgerSupportsMempool blk
  , BasicEnvelopeValidation blk
  , StandardHash (LedgerState blk)
  ) => Model blk r -> Cmd blk r -> (Response blk) r -> Model blk r
transition model (TryAddTxs txs) RespOk =
  -- NOTE: this is consistent with the QSM example showed in https://github.com/stevana/quickcheck-state-machine#example
  model {
      intermediateLedgerState = st'
    , validatedTransactions   = validatedTransactions model <> valid
    , remainingCapacity       = rcap
  }
  where
    (valid, st', rcap) = applyTxs model txs
transition model (SetLedgerState newSt) RespOk =
  model { currentLedgerDBState = newSt }
transition model GetSnapshot _ = model
transition model SyncWithLedger _ =
    -- If the tip of the ledger DB does not change wrt the mempool's
    -- internal state we do  not revalidate.
    if getTip currentLedgerDBState == castPoint (getTip intermediateLedgerState)
    then model
    else
      -- TODO: the implementation of this function seems to be a combination of
      -- model initialization and trying to add all the validated transactions
      -- in the mempool, maybe we should abstract away this pattern.
      --
      model
        { forgingFor              = slot
        , intermediateLedgerState = tickedSt'
        , validatedTransactions   = valid
        , remainingCapacity       = rcap
        }
      where
        Model{ modelConfig
             , currentLedgerDBState
             , validatedTransactions
             , intermediateLedgerState } = model
        (slot,  tickedSt)                = tickLedgerState modelConfig currentLedgerDBState
        modelWithResetCapacity           = model {
                                               remainingCapacity = fromIntegral $ 2 * txsMaxBytes tickedSt'
                                             , intermediateLedgerState = tickedSt
                                             }
        (valid, tickedSt', rcap)         = applyTxs modelWithResetCapacity validatedTransactions
transition _ cmd resp = error $  "Unexpected command response combination."
                              <> " Command: " <> show cmd
                              <> " Response: " <> show resp -- TODO: use pretty printing

txsSize:: LedgerSupportsMempool blk => [GenTx blk] -> Int
txsSize = getSum . foldMap (Sum . txSize)

txSize:: LedgerSupportsMempool blk => GenTx blk -> Int
txSize = fromIntegral . txInBlockSize


-- | State machine for which we do not have a mempool, and therefore we cannot
-- use the SUT.
stateMachineWithoutSUT ::
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
  => InitialMempoolAndModelParams blk
  -> StateMachine (Model blk) (Cmd blk) IO (Response blk)
stateMachineWithoutSUT initialParams = stateMachine initialParams err
  where err = error $  "The SUT should not be used in this state machine:"
                    <> " there is no semantics defined for this state machine."

stateMachine ::
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Show (TickedLedgerState blk)
     , Eq (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
  => InitialMempoolAndModelParams blk
  -> MempoolWithMockedLedgerItf IO blk idx
  -> StateMachine (Model blk) (Cmd blk) IO (Response blk)
stateMachine initialParams mempool = StateMachine
  { SMT.initModel     = initModel initialParams
  , SMT.transition    = transition
  , SMT.precondition  = precondition
  , SMT.postcondition = postcondition
  , SMT.generator     = generator
  , SMT.shrinker      = shrinker
  , SMT.semantics     = semantics mempool
  , SMT.mock          = mock
  , SMT.invariant     = Nothing
  , SMT.cleanup       = \_m -> setLedgerState mempool (immpInitialState initialParams)
                      -- FIXME we should investigate if a cleanup action is
                      -- necessary, and if so write a proper cleanup action that
                      -- initializes the whole mempool.
  }

prop_sequential :: forall blk idx.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , ToExpr (LedgerState blk)
     , ToExpr (TickedLedgerState blk)
     , ToExpr (LedgerConfig blk)
     , ToExpr (GenTx blk)
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (TickedLedgerState blk)
     , Show (LedgerConfig blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
  => (InitialMempoolAndModelParams blk ->  IO (MempoolWithMockedLedgerItf IO blk idx))
  -> InitialMempoolAndModelParams blk
  -> QC.Property
prop_sequential mempoolWithMockedLedgerItfAct initialParams = QC.withMaxSuccess 10000 $ do   --  TODO set the number of tests the right way
    forAllCommands (stateMachineWithoutSUT initialParams) Nothing $ \cmds -> QCM.monadicIO $ do
        mempool <- QCM.run $ mempoolWithMockedLedgerItfAct initialParams
        let modelWithSUT = stateMachine initialParams mempool
        (hist, _model, res) <- runCommands modelWithSUT cmds
        prettyCommands modelWithSUT hist -- TODO: check if we need the mempool in 'prettyCommands'
          $ checkCommandNames cmds
          $ res QC.=== Ok

prop_parallel :: forall blk idx.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (TickedLedgerState blk)
     , Show (LedgerConfig blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
  => (InitialMempoolAndModelParams blk ->  IO (MempoolWithMockedLedgerItf IO blk idx))
  -> InitialMempoolAndModelParams blk
  -> QC.Property
prop_parallel mempoolWithMockedLedgerItfAct initialParams = QC.withMaxSuccess 10000 $ do
  forAllParallelCommands (stateMachineWithoutSUT initialParams) Nothing $ \cmds -> QCM.monadicIO $ do
        mempool <- QCM.run $ mempoolWithMockedLedgerItfAct initialParams
        let modelWithSUT = stateMachine initialParams mempool
        -- (hist, _model, res) <- runParallelCommands  modelWithSUT cmds
        res <- runParallelCommandsNTimes 1 modelWithSUT cmds
        prettyParallelCommands cmds res -- TODO: check if we need the mempool in 'prettyCommands'

-- Parameters common to the constructor functions for mempool and model
data InitialMempoolAndModelParams blk = MempoolAndModelParams {
      immpInitialState :: LedgerState blk
    , immpLedgerConfig :: LedgerConfig blk
    }

deriving instance ( Show (LedgerConfig blk)
                  , Show (LedgerState blk)
                  ) => Show (InitialMempoolAndModelParams blk)

instance ( QC.Arbitrary (LedgerState blk)
         , QC.Arbitrary (LedgerConfig blk)
         ) => QC.Arbitrary (InitialMempoolAndModelParams blk) where
  arbitrary = MempoolAndModelParams <$> QC.arbitrary <*> QC.arbitrary

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

-- Show the labelled examples. See 'Tag' for a description of the cases we tag.
showMempoolTestScenarios :: forall blk.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (LedgerConfig blk)
     , Show (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
   => InitialMempoolAndModelParams blk -> IO ()
showMempoolTestScenarios params =
  showLabelledExamples (stateMachineWithoutSUT params) tagEventSeq
  where
    tagEventSeq :: [Event (Model blk) (Cmd blk) (Response blk) Symbolic] -> [Tag]
    tagEventSeq = concatMap tagEvent
    -- TODO: stop as soon as we have collected all tags.

tagEvent ::
     Ord (GenTx blk)
  => Event (Model blk) (Cmd blk) (Response blk) Symbolic
  -> [Tag]
tagEvent Event {eventBefore, eventCmd, eventAfter} =
   tagAcceptedTxs eventBefore eventCmd eventAfter <> tagMaxCapacityReached eventAfter
  where
    tagAcceptedTxs model (TryAddTxs txs) model' =
        let
            addedTransactions     = validatedTransactions model' \\ validatedTransactions model
            remainingTransactions = txs \\ addedTransactions
        in
        case (txs, addedTransactions, remainingTransactions) of
          ([], _  , _  ) -> [AddedEmptyListOfTransactions]
          (_ , [] , _:_) -> [RemainingTransactions]
          (_ , _:_, [] ) -> [AcceptedTransactions]
          (_ , _:_, _:_) -> [AcceptedTransactions, RemainingTransactions]
          (_:_, [], [] ) -> error "This should not happen, there should be accepted or rejected transactions"
      where
          x \\ y = Set.toList (Set.fromList x Set.\\ Set.fromList y)
    tagAcceptedTxs _ _ _ = []
    tagMaxCapacityReached model' = [ MaxCapacityReached | remainingCapacity model' == 0 ]

-- | Tag to identify the different tests scenarios.
data Tag = AddedEmptyListOfTransactions
         | AcceptedTransactions
         | RemainingTransactions
         | MaxCapacityReached
  deriving (Show)

{------------------------------------------------------------------------------
  Mempool with a mocked ledger interface

  TODO: we might want to put this in a different module
------------------------------------------------------------------------------}

-- The idea of this data structure is that we make sure that the ledger
-- interface used by the mempool gets mocked in the right way.
--
data MempoolWithMockedLedgerItf m blk idx = MempoolWithMockedLedgerItf {
      getLedgerInterface :: LedgerInterface m blk
    , getLedgerStateTVar :: StrictTVar m (LedgerState blk) -- TODO: define setters and getters for this
    , getMempool         :: Mempool m blk idx
    }

openMempoolWithMockedLedgerItf ::
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolCapacityBytesOverride
  -> Tracer IO (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> InitialMempoolAndModelParams blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MempoolWithMockedLedgerItf IO blk TicketNo)
openMempoolWithMockedLedgerItf capacityOverride tracer txSizeImpl params = do
    currentLedgerStateTVar <- newTVarIO (immpInitialState params)
    let ledgerItf = LedgerInterface {
            getCurrentLedgerState = readTVar currentLedgerStateTVar
        }
    mempool <- openMempoolWithoutSyncThread
                   ledgerItf
                   (immpLedgerConfig params)
                   capacityOverride
                   tracer
                   txSizeImpl
    pure MempoolWithMockedLedgerItf {
        getLedgerInterface = ledgerItf
      , getLedgerStateTVar = currentLedgerStateTVar
      , getMempool         = mempool
    }
setLedgerState ::
     MempoolWithMockedLedgerItf IO blk idx
  -> LedgerState blk
  -> IO ()
setLedgerState MempoolWithMockedLedgerItf {getLedgerStateTVar} newSt =
  atomically $ writeTVar getLedgerStateTVar newSt

{-------------------------------------------------------------------------------
  Instances required to run the state machine
-------------------------------------------------------------------------------}

deriving anyclass instance ( ToExpr (LedgerState blk)
                           , ToExpr (TickedLedgerState blk)
                           , ToExpr (LedgerConfig blk)
                           , ToExpr (GenTx blk)
                           ) => ToExpr (Model blk Concrete)
deriving instance ToExpr SlotNo
