{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Consensus.Model.Mempool where

import           Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, StrictTVar,
                     atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad (foldM)
import           Control.Monad.Class.MonadTimer (threadDelay)
import           Control.Monad.Reader (MonadTrans, lift)
import           Control.Monad.State (MonadState (..), StateT (StateT), gets,
                     modify)
import           Control.Tracer (Tracer)
import           Data.Either (isRight)
import qualified Data.Set as Set
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx,
                     WhetherToIntervene (..))
import           Ouroboros.Consensus.Mempool (LedgerInterface (..), Mempool,
                     TicketNo, TraceEventMempool, getSnapshot,
                     openMempoolWithoutSyncThread, tryAddTxs)
import           Ouroboros.Consensus.Mempool.API
                     (MempoolCapacityBytesOverride (NoMempoolCapacityBytesOverride),
                     TxSizeInBytes, snapshotTxs)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Test.Consensus.Mempool.StateMachine
                     (InitialMempoolAndModelParams (..))
import           Test.Consensus.Model.TestBlock (GenTx (..), TestBlock,
                     TestLedgerState (..), Tx, fromValidated, genValidTx,
                     txSize)
import           Test.QuickCheck (arbitrary, counterexample, shrink)
import           Test.QuickCheck.DynamicLogic (DynLogicModel)
import           Test.QuickCheck.StateModel (Any (..), Realized, RunModel (..),
                     StateModel (..))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (applyPayload, payloadDependentState)

data MempoolModel = Idle
  |  MempoolModel { transactions :: [GenTx TestBlock],
                    ledgerState  :: TestLedgerState }
    deriving (Show)

instance StateModel MempoolModel where
    data Action MempoolModel a where
        InitMempool :: InitialMempoolAndModelParams TestBlock -> Action MempoolModel ()
        -- | Adds some new transactions to mempoool
        -- Those transactions are guaranteed to not conflict with the curent state
        -- of the Mempool. Returns the list of txs effectively added to the mempool.
        AddTxs :: [GenTx TestBlock] -> Action MempoolModel [GenTx TestBlock]
        -- | An 'observation' that checks the given list of transactions is part of
        -- the mempool _after_ some time
        HasValidatedTxs :: [GenTx TestBlock] -> Int -> Action MempoolModel [GenTx TestBlock]


    arbitraryAction = \case
      Idle -> Some . InitMempool <$> arbitrary
      MempoolModel{ledgerState=TestLedgerState{availableTokens}} -> Some . AddTxs . (:[]) . TestBlockGenTx <$> genValidTx availableTokens

    precondition Idle InitMempool{}          = True
    precondition MempoolModel{ledgerState} (AddTxs gts)          =
      isRight $ foldM (applyPayload @Tx) ledgerState $ blockTx <$> gts
    precondition MempoolModel{transactions} (HasValidatedTxs gts _) =
      Set.fromList gts == Set.fromList transactions
    precondition _ _ = False

    shrinkAction _ (InitMempool imamp)   = Some . InitMempool <$> shrink imamp
    shrinkAction _ (AddTxs gts)          = Some . AddTxs <$> shrink gts
    shrinkAction _ (HasValidatedTxs gts wait) = Some . flip HasValidatedTxs wait <$> shrink gts

    initialState :: MempoolModel
    initialState = Idle

    nextState Idle (InitMempool imamp) _var =
      MempoolModel [] (payloadDependentState $ immpInitialState imamp)
    nextState MempoolModel{transactions, ledgerState} (AddTxs newTxs) _var =
      let newState = either (error . show) id $ foldM (applyPayload @Tx) ledgerState $ blockTx <$> newTxs
      in MempoolModel { transactions = transactions <> newTxs , ledgerState = newState }
    nextState Idle act@AddTxs{} _var = error $ "Invalid transition from Idle state with action: "<> show act
    nextState st _act _var = st

deriving instance Show (Action MempoolModel a)
deriving instance Eq (Action MempoolModel a)

instance DynLogicModel MempoolModel

data ConcreteMempool m = ConcreteMempool {
  theMempool :: Maybe (MempoolWithMockedLedgerItf m TestBlock TicketNo),
  tracer     :: Tracer m (TraceEventMempool TestBlock)
  }


newtype RunMonad m a = RunMonad {runMonad :: StateT (ConcreteMempool m) m a}
    deriving (Functor, Applicative, Monad, MonadFail, MonadState (ConcreteMempool m) )

instance MonadTrans RunMonad where
  lift m = RunMonad $ StateT $ \ s ->  (,s) <$> m

type instance Realized (RunMonad m) a = a

instance (IOLike m, MonadSTM m, MonadFail m) => RunModel MempoolModel (RunMonad m) where
  perform _ (InitMempool start) _ = do
    tr <- gets tracer
    let capacityOverride :: MempoolCapacityBytesOverride
        capacityOverride = NoMempoolCapacityBytesOverride -- TODO we might want to generate this
    mempool <- lift $ openMempoolWithMockedLedgerItf capacityOverride tr txSize start
    modify $ \ st -> st { theMempool = Just mempool}
  perform _ (AddTxs txs) _          = do
    Just mempoolWithMockedLedgerItf  <- gets theMempool
    lift $ snd <$> tryAddTxs (getMempool mempoolWithMockedLedgerItf)
      DoNotIntervene  -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
      txs
  perform _ (HasValidatedTxs gts wait) _ = do
    Just mempoolWithMockedLedgerItf <- gets theMempool
    waitForTxsToMatch mempoolWithMockedLedgerItf gts wait

  postcondition (_before, _after) (HasValidatedTxs gts _ ) _env result = pure $ Set.fromList gts == Set.fromList result
  postcondition _ _ _ _                               = pure True

  monitoring (_before, _after) HasValidatedTxs{} _env result =
    counterexample ("Validated txs: " <> show result)
  monitoring _ _ _ _ = id

-- | Keep retrieving content of the mempool at most `retryCount` times.
-- Loops and wait until either the content of the ledger matches `expected` set of transaction
-- or `retryCount` reaches 0.
--
-- Returns content of the mempool.
waitForTxsToMatch :: IOLike m => MempoolWithMockedLedgerItf m TestBlock TicketNo -> [GenTx TestBlock] -> Int -> RunMonad m [GenTx TestBlock]
waitForTxsToMatch mempoolWithMockedLedgerItf expected = \case
  0 -> getAllValidatedTxs mempoolWithMockedLedgerItf
  retryCount -> do
    txs <- getAllValidatedTxs mempoolWithMockedLedgerItf
    if Set.fromList txs /= Set.fromList expected
      then lift (threadDelay 10) >> waitForTxsToMatch mempoolWithMockedLedgerItf expected (retryCount -1 )
      else pure txs


getAllValidatedTxs :: (MonadSTM m) => MempoolWithMockedLedgerItf m TestBlock idx -> RunMonad m [GenTx TestBlock]
getAllValidatedTxs mempool = do
  snap <- lift $ atomically $ getSnapshot $ getMempool mempool
  pure $ fmap (fromValidated . fst) . snapshotTxs $ snap

-- The idea of this data structure is that we make sure that the ledger
-- interface used by the mempool gets mocked in the right way.
--
data MempoolWithMockedLedgerItf m blk idx = MempoolWithMockedLedgerItf {
      getLedgerInterface :: LedgerInterface m blk
    , getLedgerStateTVar :: StrictTVar m (LedgerState blk) -- TODO: define setters and getters for this
    , getMempool         :: Mempool m blk idx
    }

openMempoolWithMockedLedgerItf ::
     ( MonadSTM m, IOLike m)
  => MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool TestBlock)
  -> (GenTx TestBlock -> TxSizeInBytes)
  -> InitialMempoolAndModelParams TestBlock
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> m (MempoolWithMockedLedgerItf m TestBlock TicketNo)
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
     MonadSTM m =>
     MempoolWithMockedLedgerItf m TestBlock TicketNo
  -> LedgerState TestBlock
  -> m ()
setLedgerState MempoolWithMockedLedgerItf {getLedgerStateTVar} newSt =
  atomically $ writeTVar getLedgerStateTVar newSt
