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
  , prop_sequential
  , stateMachine
    -- * Model and SUT parameters
  , InitialMempoolAndModelParams (MempoolAndModelParams, immpInitialState, immpLedgerConfig)
    -- * Labelling
  , showMempoolTestScenarios
  , tagTransactions
  ) where

import           Control.Monad.Class.MonadSTM.Strict (MonadSTM (atomically),
                     StrictTVar)
import           Control.Monad.Trans.Except (runExcept)
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

import           Cardano.Slotting.Slot (SlotNo (SlotNo),
                     WithOrigin (At, Origin))

-- TODO: Proxy is well known, re-exporting this seems like a bad idea.
import           Ouroboros.Consensus.Block (Proxy (Proxy))
import           Ouroboros.Consensus.Block.Abstract (pattern NotOrigin)
import           Ouroboros.Consensus.HeaderValidation (BasicEnvelopeValidation,
                     ValidateEnvelope, minimumPossibleSlotNo)
import           Ouroboros.Consensus.Ledger.Abstract (UpdateLedger,
                     ledgerTipSlot)
import           Ouroboros.Consensus.Ledger.Basics (LedgerCfg, LedgerConfig,
                     LedgerState, TickedLedgerState, applyChainTick, getTipSlot)
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
      currentLedgerDBState    :: !(LedgerState blk)
    , forgingFor              :: !SlotNo
    , intermediateLedgerState :: !(TickedLedgerState blk)
    , modelConfig             :: !(LedgerConfig blk)
    }
  deriving stock (Generic)

deriving stock instance ( Eq (LedgerState blk)
                        , Eq (TickedLedgerState blk)
                        , Eq (LedgerCfg (LedgerState blk))
                        ) => Eq   (Model blk r)
deriving stock instance ( Show (LedgerState blk)
                        , Show (TickedLedgerState blk)
                        , Show (LedgerCfg (LedgerState blk))
                        ) => Show (Model blk r)

initModel :: forall blk r.
     ( UpdateLedger blk
     , BasicEnvelopeValidation blk
     )
  => InitialMempoolAndModelParams blk
  -> Model blk r
initModel params = Model {
       currentLedgerDBState    = initialState
     , forgingFor              = initialStateSlot
     , intermediateLedgerState = applyChainTick cfg initialStateSlot initialState
     , modelConfig             = cfg
     }
  where
    initialState = immpInitialState params
    cfg          = immpLedgerConfig params
    -- TODO: this is copying the implementation
    initialStateSlot = case ledgerTipSlot initialState of
      Origin      -> minimumPossibleSlotNo (Proxy @blk)
      NotOrigin s -> succ s

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
--      TryAddTxsSuccess (TryAddTxsResult blk)
      TryAddTxsResult {
        valid       :: ![GenTx blk]
      , invalid     :: ![GenTx blk]
      , unprocessed :: ![GenTx blk]
      , newTickedSt :: !(TickedLedgerState blk)
      }
    | RespOk
  deriving stock Generic1
  deriving anyclass Rank2.Foldable

deriving stock instance (Show (GenTx blk), Show (TickedLedgerState blk)) => Show (SuccessfulResponse blk ref)

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
generator (Model {}      ) = Just
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

shrinker ::
     (QC.Arbitrary (GenTx blk), QC.Arbitrary (LedgerState blk))
  => Model blk Symbolic -> Cmd blk Symbolic -> [Cmd blk Symbolic]
shrinker _model (TryAddTxs txs)     = fmap TryAddTxs      $ QC.shrink txs
shrinker _model (SetLedgerState st) = fmap SetLedgerState $ QC.shrink st

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
    getTx (MempoolTxAdded vtx) = txForgetValidated vtx

precondition :: Model blk Symbolic -> Cmd blk Symbolic -> Logic
precondition _model _event = Top

postcondition ::
     Model    blk Concrete
  -> Cmd      blk Concrete
  -> Response blk  Concrete
  -> Logic
postcondition _model _event _response = Top

mock :: forall blk.
     (LedgerSupportsMempool blk)
  => Model blk Symbolic
  -> Cmd blk Symbolic
  -> GenSym (Response blk Symbolic)
-- Here we should use the LedgerSupports mempool instace for blk. Why? Because
-- we want to compare the model against the SUT using __the same impl__
mock Model {modelConfig, intermediateLedgerState} (TryAddTxs txs) =
  pure $! Response
       $! go intermediateLedgerState txs [] [] []
  where
    go :: TickedLedgerState blk
                      -> [GenTx blk]
                      -> [GenTx blk]
                      -> [GenTx blk]
                      -> [GenTx blk]
                      -> SuccessfulResponse blk ref
    go !st' [] !val !inv !unp = TryAddTxsResult {
          valid       = reverse val
        , invalid     = reverse inv
        , unprocessed = unp
        , newTickedSt = st'
        }

    go !st (tx:tys) !val !inv !unp =
      case mockApplyTx modelConfig st tx of
        Valid st'   -> go st' tys (tx:val) inv      unp
        Invalid st' -> go st' tys val      (tx:inv) unp
mock _ (SetLedgerState _) = pure $! Response RespOk

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

transition :: Model blk r -> Cmd blk r -> (Response blk) r -> Model blk r
transition model (TryAddTxs _) (Response TryAddTxsResult {newTickedSt}) =
  model { intermediateLedgerState = newTickedSt }
transition model (SetLedgerState newSt) (Response RespOk) =
  -- TODO
  model { currentLedgerDBState = newSt }

-- | State machine for which we do not have a mempool, and therefore we cannot
-- use the SUT.
stateMachineWithoutSUT ::
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk)
  => InitialMempoolAndModelParams blk
  -> StateMachine (Model blk) (Cmd blk) IO (Response blk)
stateMachineWithoutSUT initialParams = stateMachine initialParams err
  where err = error $  "The SUT should not be used in this state machine:"
                    <> " there is no semantics defined for this state machine."

stateMachine ::
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk)
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
  , SMT.cleanup       = noCleanup
  }

-- To run the property we require both a mempool, and a @TVar@ that can be used
-- to modifi
prop_sequential :: forall blk idx.
     ( QC.Arbitrary (GenTx blk)
     , QC.Arbitrary (LedgerState blk)
     , ToExpr (LedgerState blk)
     , ToExpr (TickedLedgerState blk)
     , ToExpr (LedgerConfig blk)
     , Show (TickedLedgerState blk)
     , Show (LedgerConfig blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
     )
  => (InitialMempoolAndModelParams blk ->  IO (MempoolWithMockedLedgerItf IO blk idx))
  -> InitialMempoolAndModelParams blk
  -> QC.Property
prop_sequential mempoolWithMockedLedgerItfAct initialParams = do
    forAllCommands (stateMachineWithoutSUT initialParams) Nothing $ \cmds -> QCM.monadicIO $ do
        mempool <- QCM.run $ mempoolWithMockedLedgerItfAct initialParams
        let modelWithSUT = stateMachine initialParams mempool
        (hist, _model, res) <- runCommands modelWithSUT cmds
        prettyCommands modelWithSUT hist -- TODO: check if we need the mempool in 'prettyCommands'
          $ checkCommandNames cmds
          $ res QC.=== Ok

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
--      , Show (LedgerState blk)
     , Show (LedgerConfig blk)
     , Show (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
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
     , Show tag
     , Show (LedgerConfig blk)
     , Show (TickedLedgerState blk)
     , LedgerSupportsMempool blk
     , BasicEnvelopeValidation blk
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

{-------------------------------------------------------------------------------
  Instances required to run the state machine
-------------------------------------------------------------------------------}

deriving anyclass instance ( ToExpr (LedgerState blk)
                           , ToExpr (TickedLedgerState blk)
                           , ToExpr (LedgerConfig blk)
                           ) => ToExpr (Model blk Concrete)
deriving instance ToExpr SlotNo
