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
module Test.Consensus.Mempool.StateMachine (
    openMempoolWithMockedLedgerItf
  , prop_parallel
  , prop_sequential
  , stateMachine
    -- * Model and SUT parameters
  , InitialMempoolAndModelParams (MempoolAndModelParams, immpInitialState, immpLedgerConfig)
    -- * Labelling
  , showMempoolTestScenarios
  , tagTransactions
  ) where

import           Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically),
                     StrictTVar, writeTVar)
import           Control.Monad.Trans.Except (runExcept)
import           Control.Tracer (Tracer)
import           Data.Functor.Classes (Eq1, Show1)
import           Data.Kind (Type)
import           Data.List (partition)
import           Data.Monoid (Sum (Sum, getSum))
import           Data.TreeDiff.Class (ToExpr)
import           GHC.Generics (Generic, Generic1)
import           NoThunks.Class (NoThunks)

import qualified Debug.Trace as Debug

import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Monadic as QCM
import           Test.StateMachine (Reference, StateMachine (StateMachine),
                     runParallelCommands, runParallelCommandsNTimes,
                     showLabelledExamples)
import           Test.StateMachine.ConstructorName
                     (CommandNames (cmdName, cmdNames))
import           Test.StateMachine.Labelling (Event, eventResp)
import           Test.StateMachine.Logic
                     (Logic (Boolean, Not, Top, (:&&), (:=>)), (.==))
import           Test.StateMachine.Parallel (forAllParallelCommands,
                     prettyParallelCommands, runParallelCommands)
import           Test.StateMachine.Sequential (checkCommandNames,
                     forAllCommands, prettyCommands, runCommands)
import           Test.StateMachine.Types (GenSym, Reason (Ok), Symbolic,
                     concrete, genSym, noCleanup, reference)
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
import           Control.Monad (void)
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
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
      currentLedgerDBState    :: !(LedgerState blk)
    , forgingFor              :: !SlotNo
    , intermediateLedgerState :: !(TickedLedgerState blk)
    , modelConfig             :: !(LedgerConfig blk)
    , validatedTransactions   :: ![GenTx blk]
      -- | We use an integer for the maximum capacity because we want to detect
      -- if the SUT ever exceeds it.
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
     ( UpdateLedger blk
     , BasicEnvelopeValidation blk
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
    initialState = immpInitialState params
    cfg          = immpLedgerConfig params
    (initialStateSlot, initialIntermediateLedgerState) = tickLedgerState cfg initialState

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
    | GetSnapshot
    | SyncWithLedger
  deriving stock (Generic1)
  deriving anyclass (Rank2.Functor, Rank2.Foldable, Rank2.Traversable, CommandNames)

deriving stock instance (Show (GenTx blk), Show (LedgerState blk)) => Show (Cmd blk r)

-- | Successful command responses
data SuccessfulResponse blk (ref :: Type -> Type) =
      TryAddTxsResult {
        valid       :: ![GenTx blk]
      , invalid     :: ![GenTx blk]
      , unprocessed :: ![GenTx blk]
        -- | We need the new ticked state to avoid re-calculating transaction
        -- application in the model 'transition' function.
      , newTickedSt :: !(TickedLedgerState blk)
      }
    | RespOk
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

deriving stock instance (Show (GenTx blk), Show (TickedLedgerState blk)) => Show (SuccessfulResponse blk ref)

-- TODO: why not merge this with SuccessfulResponse?
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

generator :: forall blk.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , GetTip (LedgerState blk))
  => Model blk Symbolic -> Maybe (Gen (Cmd blk Symbolic))
generator (Model {currentLedgerDBState} ) = Just
                          $ QC.frequency [ (50, fmap TryAddTxs QC.arbitrary)
                                           -- TODO: if the model is bootstrapped with an "empty"
                                           -- ledger state we will generate a lot of transactions
                                           -- that will simply be rejected. Maybe we can force
                                           -- the generator to always generate a 'SetLedgerState'
                                           -- action if this is uninitialized.
                                         , (20, pure GetSnapshot)
                                         , (20, pure SyncWithLedger)
                                         , (10, fmap SetLedgerState QC.arbitrary)

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

-- | TODO Here is where we call the actual mempool commands
semantics :: forall blk idx.
     LedgerSupportsMempool blk
  => MempoolWithMockedLedgerItf IO blk idx
  -> Cmd blk Concrete
  -> IO ((Response blk) Concrete)
semantics mempoolWithMockedLedgerItf (TryAddTxs txs) = do
    (processed, toProcess) <- tryAddTxs (getMempool mempoolWithMockedLedgerItf)
                                        DoNotIntervene  -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
                                        txs
    let (added, rejected) = partition isMempoolTxAdded processed
    -- TODO hmmm, I'd need to take this from the internal mempool state. The
    -- alternative is to calculate this state by applying the transactions in
    -- the model, but we'll have to calculate this twice.
    newTickedSt <-   fmap snapshotLedgerState
                   $ atomically
                   $ getSnapshot (getMempool mempoolWithMockedLedgerItf)
    pure $! Response
         $! TryAddTxsResult
              { valid       = fmap getTx added
              , invalid     = fmap getTx rejected
              , unprocessed = toProcess
              , newTickedSt = newTickedSt
              }
  where
    getTx (MempoolTxAdded vtx)     = txForgetValidated vtx
    getTx (MempoolTxRejected tx _) = tx
semantics mempoolWithMockedLedgerItf (SetLedgerState newSt) = do
  setLedgerState mempoolWithMockedLedgerItf newSt
  pure $ Response RespOk
semantics mempoolWithMockedLedgerItf SyncWithLedger = do
  void $ syncWithLedger $ getMempool mempoolWithMockedLedgerItf
  pure $ Response RespOk
semantics mempoolWithMockedLedgerItf GetSnapshot = do
  -- TODO: Maybe mempoolWithMockedLedgerItf should implement the mempool API
  snap <- atomically $ getSnapshot $ getMempool mempoolWithMockedLedgerItf
  pure $ Response $ Snap {
      snapTxs         = fmap (txForgetValidated . fst) $ snapshotTxs snap
    , snapLedgerState = snapshotLedgerState snap
    }

precondition :: Model blk Symbolic -> Cmd blk Symbolic -> Logic
precondition _model _event = Top

postcondition :: forall blk.
     ( LedgerSupportsMempool blk
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (GenTx blk)
     , Show (LedgerState blk)
     , Show (TickedLedgerState blk)
     )
  => Model    blk Concrete
  -> Cmd      blk Concrete
  -> Response blk  Concrete
  -> Logic
postcondition Model {remainingCapacity}
              cmd@(TryAddTxs {})
              rsp@(Response TryAddTxsResult {valid}) =
      Boolean $ 0 <= remainingCapacity - txsSize valid
postcondition model GetSnapshot (Response Snap { snapTxs, snapLedgerState }) =
      (intermediateLedgerState model .== snapLedgerState)
  :&& (validatedTransactions   model .== snapTxs)
postcondition _model SyncWithLedger (Response RespOk) = Top
-- TODO: add an error clause for unexpected cmd/response commands
postcondition _model _cmd _response = Top

mock :: forall blk.
     (LedgerSupportsMempool blk)
  => Model blk Symbolic
  -> Cmd blk Symbolic
  -> GenSym (Response blk Symbolic)
-- Here we should use the LedgerSupports mempool instace for blk. Why? Because
-- we want to compare the model against the SUT using __the same impl__
mock Model {modelConfig, intermediateLedgerState} (TryAddTxs txs) =
  -- TODO: maybe we should take the mempool capacity into account. If we use
  -- this only to generate commands then this might not be necessary.
  pure $! Response
       $! TryAddTxsResult {
          valid       = val
        , invalid     = inv
        , unprocessed = unp
        , newTickedSt = st'
        }
  where
    (val, inv, unp, st') = applyTxs modelConfig intermediateLedgerState txs

mock _model (SetLedgerState _) = pure $! Response RespOk
mock model  GetSnapshot        = pure $! Response $! Snap {
      snapTxs         = validatedTransactions model
    , snapLedgerState = intermediateLedgerState model
  }
mock model SyncWithLedger = pure $! Response RespOk

applyTxs ::
     LedgerSupportsMempool blk
  => LedgerConfig blk
  -> TickedLedgerState blk
  -> [GenTx blk]
  -- TODO Comment this and consider replacing the use of a big tuple here. This might require factoring out the TryAddTxsResult
  -> ([GenTx blk], [GenTx blk], [GenTx blk], TickedLedgerState  blk)
applyTxs cfg initSt txs = go txs [] [] [] initSt
  where
    go []       !val !inv !unp !st = (reverse val, reverse inv, unp, st)
    go (tx:tys) !val !inv !unp !st  =
      case mockApplyTx cfg st tx of
        Valid st'   -> go tys (tx:val) inv      unp st'
        Invalid st' -> go tys val      (tx:inv) unp st'

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
  ( Show (GenTx blk)
  , Show (LedgerState blk)
  , Show (TickedLedgerState blk)
  , LedgerSupportsMempool blk
  , BasicEnvelopeValidation blk
  , StandardHash (LedgerState blk)
  ) => Model blk r -> Cmd blk r -> (Response blk) r -> Model blk r
transition model (TryAddTxs _) (Response TryAddTxsResult {valid, newTickedSt}) =
  model { intermediateLedgerState = newTickedSt
        , validatedTransactions   = validatedTransactions model <> valid
        , remainingCapacity       = remainingCapacity model - txsSize valid
        }
transition model (SetLedgerState newSt) (Response RespOk) =
  model { currentLedgerDBState = newSt }
transition model GetSnapshot _ = model
transition model SyncWithLedger _ =
    let Model {currentLedgerDBState, intermediateLedgerState} = model
    in
    -- If the tip of the ledger DB does not change wrt the mempool's
    -- internal state we do  not revalidate.
    if getTip currentLedgerDBState == castPoint (getTip intermediateLedgerState)
    then model
    else
      -- TODO: the implementation of this function seems to be a combination of
      -- model initialization and trying to add all the validated transactions
      -- in the mempool. Furthermore, we are performing transactions application
      -- both in the transitions and in the mock. I do not konw if this
      -- inconsistency is justified.
      model
        { forgingFor              = slot
        , intermediateLedgerState = tickedSt'
        , validatedTransactions   = valid
        , remainingCapacity       = (fromIntegral $ 2 * txsMaxBytes tickedSt') - txsSize valid
        }
      where
        Model{modelConfig, currentLedgerDBState, validatedTransactions} = model
        (slot,         tickedSt) = tickLedgerState modelConfig currentLedgerDBState
        (valid, _, _, tickedSt') = applyTxs modelConfig tickedSt validatedTransactions
transition _ cmd resp = error $  "Unexpected command response combination."
                              <> " Command: " <> show cmd
                              <> " Response: " <> show resp -- TODO: using pretty printing

txsSize:: LedgerSupportsMempool blk => [GenTx blk] -> Int
txsSize = fromIntegral . getSum . foldMap (Sum . txInBlockSize)

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
prop_sequential mempoolWithMockedLedgerItfAct initialParams = QC.withMaxSuccess  10000 $ do   --  TODO set the number of tests the right way
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
prop_parallel mempoolWithMockedLedgerItfAct initialParams = QC.withMaxSuccess 1000 $ do
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

-- Show the labelled examples. In particular:
--
-- Mempool with at least N accepted transactions
-- Mempool with at least N rejected transactions
--
showMempoolTestScenarios :: forall blk.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Eq (TickedLedgerState blk)
     , Show (LedgerState blk)
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
    tagEventSeq events =  validTransactions events
                       <> invalidTransactions events
      where
        -- Produce a AcceptedTransactions if one of the event contain a response with an accepted transaction
        validTransactions [] = [] --
        validTransactions (ev : evs) =
          case validTransactionsInResponse (eventResp ev) of
            [] -> validTransactions evs   -- We keep on looking
            _  -> [AcceptedTransactions] -- The list of events (trace) contained a valid transaction

        -- TODO: factor out duplication in logic
        invalidTransactions [] = [] --
        invalidTransactions (ev : evs) =
          case invalidTransactionsInResponse (eventResp ev) of
            [] -> validTransactions evs   -- We keep on looking
            _  -> [RejectedTransactions]

validTransactionsInResponse :: Response blk ref -> [GenTx blk]
validTransactionsInResponse (Response TryAddTxsResult {valid}) = valid
validTransactionsInResponse _                                  = []

invalidTransactionsInResponse :: Response blk ref -> [GenTx blk]
invalidTransactionsInResponse (Response TryAddTxsResult {invalid}) = invalid
invalidTransactionsInResponse _                                    = []

data Tag = AcceptedTransactions | RejectedTransactions
  deriving (Show)



tagTransactions :: forall blk tag.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , Ord (GenTx blk)
     , Show tag
     , Show (LedgerConfig blk)
     , Eq (TickedLedgerState blk)
     , Show (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     , StandardHash (LedgerState blk)
     )
   => InitialMempoolAndModelParams blk
   -> (GenTx blk -> tag)
   -> IO ()
tagTransactions params txTag =
  showLabelledExamples
      (stateMachineWithoutSUT params)
      (concatMap (mTxTag . eventResp))
  where mTxTag (Response TryAddTxsResult {valid, invalid}) =  fmap (ValidTag   . txTag) valid
                                                           <> fmap (InvalidTag   . txTag) invalid
        mTxTag _                                           = []

data ValidityTag tag = ValidTag tag | InvalidTag tag
  deriving Show

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

openMempoolWithMockedLedgerItf::
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
openMempoolWithMockedLedgerItf capacityOverride tracer txSize params = do
    currentLedgerStateTVar <- newTVarIO (immpInitialState params)
    let ledgerItf = LedgerInterface {
            getCurrentLedgerState = readTVar currentLedgerStateTVar
        }
    mempool <- openMempoolWithoutSyncThread
                   ledgerItf
                   (immpLedgerConfig params)
                   capacityOverride
                   tracer
                   txSize
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
