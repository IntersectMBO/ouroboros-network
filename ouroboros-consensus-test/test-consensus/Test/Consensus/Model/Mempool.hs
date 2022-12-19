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

import           Control.Concurrent.Class.MonadSTM.Strict (MonadSTM,
                     StrictTQueue, StrictTVar, atomically, isEmptyTQueue,
                     labelTQueueIO, newTQueueIO, newTVarIO, readTQueue,
                     readTVar, writeTQueue, writeTVar)
import           Control.Monad (foldM, forM, forever, zipWithM)
import           Control.Monad.Reader (MonadTrans, lift)
import           Control.Monad.State (MonadState (..), StateT (StateT), gets,
                     modify)
import           Control.Tracer (Tracer)
import           Data.Either (isRight)
import           Debug.Trace (trace)
import           Ouroboros.Consensus.HardFork.Combinator.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx,
                     WhetherToIntervene (..))
import           Ouroboros.Consensus.Mempool (LedgerInterface (..), Mempool,
                     TicketNo, TraceEventMempool, getSnapshot,
                     openMempoolWithoutSyncThread, tryAddTxs)
import           Ouroboros.Consensus.Mempool.API
                     (MempoolCapacityBytesOverride (NoMempoolCapacityBytesOverride),
                     TxSizeInBytes, snapshotTxs)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadAsync (Async),
                     MonadLabelledSTM, async, labelThisThread)
import           Test.Consensus.Mempool.StateMachine
                     (InitialMempoolAndModelParams (..))
import           Test.Consensus.Model.TestBlock (GenTx (..), TestBlock,
                     TestLedgerState (..), Tx (..), fromValidated, genValidTxs,
                     txSize)
import           Test.QuickCheck (arbitrary, choose, counterexample, frequency,
                     scale, shrink, tabulate)
import           Test.QuickCheck.DynamicLogic (DynLogicModel)
import           Test.QuickCheck.StateModel (Any (..), Realized, RunModel (..),
                     StateModel (..))
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (applyPayload, payloadDependentState)

data MempoolModel = Idle
  |  Open { startState   :: TestLedgerState,
            transactions :: [GenTx TestBlock],
            currentState :: TestLedgerState}
    deriving (Show)

instance StateModel MempoolModel where
    data Action MempoolModel a where
        -- | Initial state of the mempool and number of clients to run
        InitMempool :: InitialMempoolAndModelParams TestBlock -> Int -> Action MempoolModel ()
        -- | Adds some new transactions to mempoool
        -- Returns the number of transactions submitted.
        AddTxs :: [GenTx TestBlock] -> Action MempoolModel Int
        -- | An 'action' that checks the current state of the mempool is consistent, eg.
        -- all transactions can apply to the current ledger's state.
        -- Returns the number of txs still in flight from clients and the total number of txs
        -- in the mempool
        HasConsistentTxs :: Action MempoolModel (Int, [GenTx TestBlock])

    arbitraryAction = \case
      Idle ->
        Some <$> (InitMempool <$> arbitrary <*> choose (2, 10))
      Open{currentState=TestLedgerState{availableTokens}} ->
        frequency [ (10, Some . AddTxs . (fmap TestBlockGenTx) <$> genValidTxs availableTokens)
                  , (1 , pure $ Some HasConsistentTxs)
                  ]

    precondition Idle InitMempool{}          = True
    precondition Open{currentState} (AddTxs gts)          =
      isRight $ foldM (applyPayload @Tx) currentState $ blockTx <$> gts
    precondition Open{} HasConsistentTxs = True
    precondition _ _ = False

    shrinkAction _ (InitMempool imamp n)   = Some <$> (InitMempool <$> shrink imamp <*> shrink n)
    shrinkAction _ (AddTxs gts)          = Some . AddTxs <$> shrink gts
    shrinkAction _ HasConsistentTxs = []

    initialState :: MempoolModel
    initialState = Idle

    nextState Idle (InitMempool imamp _) _var =
      let ledgerState = (payloadDependentState $ immpInitialState imamp)
      in Open{ startState = ledgerState, transactions =  [] , currentState = ledgerState }
    nextState open@Open{startState,transactions, currentState} (AddTxs newTxs) _var =
      let newState = either (error . show) id $ foldM (applyPayload @Tx) currentState $ blockTx <$> newTxs
      in Open{ startState, transactions = transactions <> newTxs , currentState = newState }
    nextState Idle act@AddTxs{} _var = error $ "Invalid transition from Idle state with action: "<> show act
    nextState st _act _var = st

deriving instance Show (Action MempoolModel a)
deriving instance Eq (Action MempoolModel a)

instance DynLogicModel MempoolModel

-- | A single client trying to add transactions to the mempool
-- concurrenty with other clients
data MempoolClient m = MempoolClient {
  txsQueue :: StrictTQueue m (GenTx TestBlock),
  thread   :: Async m ()
  }

data ConcreteMempool m = ConcreteMempool {
  theMempool :: Maybe (MempoolWithMockedLedgerItf m TestBlock TicketNo),
  tracer     :: Tracer m (TraceEventMempool TestBlock),
  clients    :: [MempoolClient m]
  }

newtype RunMonad m a = RunMonad {runMonad :: StateT (ConcreteMempool m) m a}
    deriving (Functor, Applicative, Monad, MonadFail, MonadState (ConcreteMempool m) )

instance MonadTrans RunMonad where
  lift m = RunMonad $ StateT $ \ s ->  (,s) <$> m

type instance Realized (RunMonad m) a = a

instance (IOLike m, MonadSTM m, MonadFail m, MonadLabelledSTM m) => RunModel MempoolModel (RunMonad m) where
  perform _ (InitMempool start n) _ = do
    tr <- gets tracer
    let capacityOverride :: MempoolCapacityBytesOverride
        capacityOverride = NoMempoolCapacityBytesOverride -- TODO we might want to generate this
    mempool <- lift $ openMempoolWithMockedLedgerItf capacityOverride tr txSize start
    clients <- lift $ forM [ 1.. n] (startMempoolClient mempool)
    modify $ \ st -> st { theMempool = Just mempool, clients}
  perform _ (AddTxs txs) _          =
    gets clients >>= (length <$>) . lift . dispatch txs
  perform _ HasConsistentTxs _ = do
    clients <- gets clients
    Just mempool <- gets theMempool
    allTxs <- getAllValidatedTxs mempool
    busy <- lift $ atomically $ sequence $ (fmap not . isEmptyTQueue . txsQueue) <$> clients
    pure (length $ filter id  busy, allTxs)


  postcondition (_before, Open{startState}) HasConsistentTxs _env result = do
    Just mempool <- gets theMempool
    allTxs <- getAllValidatedTxs mempool
    let finalLedger = foldM (applyPayload @Tx) startState $ blockTx <$> allTxs
    pure $ isRight finalLedger

  postcondition _ _ _ _                               = pure True

  monitoring (_before, _after) HasConsistentTxs _env result =
    counterexample ("Conflicting txs: " <> show result) . tabulate "Txs Queue length" [show $ fst result]  . tabulate "Txs in Mempool" [groupByLength $ snd result]
  monitoring _ (AddTxs txs) _ _ = tabulate "Submitted Txs" [groupByLength txs]
  monitoring _ _ _ _ = id

groupByLength :: [a] -> String
groupByLength xs =
  let tens = length xs `div` 10
      hundreds = length xs `div` 100
  in if tens < 10
     then "<= " <> show (tens * 10)
     else "<= " <> show (hundreds * 100)

dispatch :: IOLike m => [GenTx TestBlock] -> [MempoolClient m] -> m [GenTx TestBlock]
dispatch txs clients =
  zipWithM submitTx txs (cycle clients)
  where
   submitTx tx MempoolClient{txsQueue} = atomically (writeTQueue txsQueue tx) >> pure tx

addTxs :: Functor m => MempoolWithMockedLedgerItf m blk idx -> [GenTx blk] -> m [GenTx blk]
addTxs mempool txs =
  snd <$> tryAddTxs (getMempool mempool)
      DoNotIntervene  -- TODO: we need to think if we want to model the 'WhetherToIntervene' behaviour.
      txs

startMempoolClient :: (IOLike m, MonadLabelledSTM m) => MempoolWithMockedLedgerItf m TestBlock TicketNo -> Int -> m (MempoolClient m)
startMempoolClient mempool idx = do
  txsQueue <- newTQueueIO
  labelTQueueIO txsQueue $ "client-queue-" <> show idx
  thread <- async $ runClient txsQueue
  pure MempoolClient{txsQueue, thread}
  where
    runClient q = do
      labelThisThread $ "client-" <> show idx
      forever $ do
        tx <- atomically $ readTQueue q
        addTxs mempool [tx]

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
