{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Consensus.Model.Mempool where

import           Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, StrictTVar,
                     atomically, newTVarIO, readTVar, writeTVar)
import           Control.Monad (void)
import           Control.Monad.Reader (MonadReader (ask), MonadTrans,
                     ReaderT (..), lift)
import           Control.Tracer (Tracer)
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState)
import           Ouroboros.Consensus.HeaderValidation (ValidateEnvelope)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx,
                     WhetherToIntervene (..))
import           Ouroboros.Consensus.Mempool (LedgerInterface (..), Mempool,
                     TicketNo, TraceEventMempool, getSnapshot,
                     openMempoolWithoutSyncThread, tryAddTxs)
import           Ouroboros.Consensus.Mempool.API (MempoolCapacityBytesOverride,
                     TxSizeInBytes)
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import           Test.Consensus.Mempool.StateMachine
                     (InitialMempoolAndModelParams (..))
import           Test.Consensus.Model.TestBlock (TestBlock)
import           Test.QuickCheck.DynamicLogic (DynLogicModel)
import           Test.QuickCheck.StateModel (Realized, RunModel (..),
                     StateModel (..))

data MempoolModel = MempoolModel {transactions :: [GenTx TestBlock]}
    deriving (Show)

instance StateModel MempoolModel where
    data Action MempoolModel a where
        AddTxs :: [GenTx TestBlock] -> Action MempoolModel ()
        -- | An 'observation' that checks the given list of transactions is part of
        -- the mempool
        HasValidatedTxs :: [GenTx TestBlock] -> Action MempoolModel ()
        -- | An action to explicitly wait some amount of time
        Wait :: Int -> Action MempoolModel ()

    arbitraryAction = error "not implemented"

    initialState = MempoolModel []

deriving instance Show (Action MempoolModel a)
deriving instance Eq (Action MempoolModel a)

instance DynLogicModel MempoolModel

type ConcreteMempool m = MempoolWithMockedLedgerItf m TestBlock TicketNo

newtype RunMonad m a = RunMonad {runMonad :: ReaderT (ConcreteMempool m) m a}
    deriving (Functor, Applicative, Monad, MonadReader (ConcreteMempool m) )

instance MonadTrans RunMonad where
  lift m = RunMonad $ ReaderT $ \ _ -> m

type instance Realized (RunMonad m) a = a

instance MonadSTM m => RunModel MempoolModel (RunMonad m) where
  perform _ (AddTxs txs) _          = do
    mempoolWithMockedLedgerItf  <- ask
    lift $ void $ tryAddTxs (getMempool mempoolWithMockedLedgerItf)
      DoNotIntervene  -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
      txs
    pure ()
  perform _ (HasValidatedTxs txs) _ = do
    mempoolWithMockedLedgerItf <- ask
    -- TODO: Maybe mempoolWithMockedLedgerItf should implement the mempool API
    _snap <- lift $ atomically $ getSnapshot $ getMempool mempoolWithMockedLedgerItf
    pure ()
  perform _st _act _env             = error "not implemented"

-- The idea of this data structure is that we make sure that the ledger
-- interface used by the mempool gets mocked in the right way.
--
data MempoolWithMockedLedgerItf m blk idx = MempoolWithMockedLedgerItf {
      getLedgerInterface :: LedgerInterface m blk
    , getLedgerStateTVar :: StrictTVar m (LedgerState blk) -- TODO: define setters and getters for this
    , getMempool         :: Mempool m blk idx
    }

openMempoolWithMockedLedgerItf ::
     ( MonadSTM m, IOLike m, ValidateEnvelope TestBlock     )
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
