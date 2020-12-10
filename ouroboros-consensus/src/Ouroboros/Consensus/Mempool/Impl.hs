{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  , MempoolCapacityBytesOverride (..)
  , LedgerInterface (..)
  , chainDBLedgerInterface
  , TicketNo
    -- * For testing purposes
  , openMempoolWithoutSyncThread
  , MempoolEnv(..)
  , MempoolArgs(..)
  ) where

import           Control.Monad.Except
import           Data.Typeable

import           Control.Tracer

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.ImplPure
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, zeroTicketNo)
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> MempoolArgs blk
  -> Tracer m (TraceEventMempool blk)
  -> m (Mempool m blk TicketNo)
openMempool registry ledger args tracer = do
    env <- initMempoolEnv ledger args tracer
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerInterface m blk
  -> MempoolArgs blk
  -> Tracer m (TraceEventMempool blk)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger args tracer  =
    mkMempool <$> initMempoolEnv ledger args tracer

mkMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { tryAddTxs      = implTryAddTxs      env
    , removeTxs      = implRemoveTxs      env
    , syncWithLedger = implSyncWithLedger env
    , getSnapshot    = implGetSnapshot    env
    , getSnapshotFor = implGetSnapshotFor env
    , getCapacity    = implGetCapacity    env
    , getTxSize      = mpArgsTxSize       (mpEnvArgs env)
    , zeroIdx        = zeroTicketNo
    }

-- | Abstract interface needed to run a Mempool.
newtype LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

initMempoolEnv :: ( IOLike m
                  , NoThunks (GenTxId blk)
                  , LedgerSupportsMempool blk
                  , ValidateEnvelope blk
                  )
               => LedgerInterface m blk
               -> MempoolArgs blk
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface args tracer = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState (mpArgsLedgerCfg args)  (ForgeInUnknownSlot st)
    isVar <- newTVarIO $ initInternalState (mpArgsCapacityOverride args) zeroTicketNo slot st'
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvArgs             = args
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (
                                   IOLike m
                                 , LedgerSupportsMempool blk
                                 , HasTxId (GenTx blk)
                                 , ValidateEnvelope blk
                                 )
                              => ResourceRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv =
    void $ onEachChange
      registry
      "Mempool.syncStateOnTipPointChange"
      id
      Nothing
      getCurrentTip
      action
  where
    action :: Point blk -> m ()
    action _tipPoint = void $ implSyncWithLedger menv

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip =
          ledgerTipPoint (Proxy @blk)
      <$> getCurrentLedgerState (mpEnvLedger menv)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

data MempoolEnv m blk = MempoolEnv {
      mpEnvArgs     :: MempoolArgs blk
    , mpEnvLedger   :: LedgerInterface m blk
    , mpEnvStateVar :: StrictTVar m (InternalState blk)
    , mpEnvTracer   :: Tracer m (TraceEventMempool blk)
    }

-- | Read the internal state and the ledger state, calls the function
-- with it and writes the state with the first element of the result
-- of the function and returns the second part of the result of the
-- function as result
atomicallyDoWithState ::
     IOLike m
  => MempoolEnv m blk
  -> (MempoolArgs blk -> InternalState blk -> LedgerState blk -> (InternalState blk, res))
  -> m res
atomicallyDoWithState MempoolEnv {..} func = atomically $ do
    state <- readTVar mpEnvStateVar
    ledgerState <- getCurrentLedgerState mpEnvLedger
    let (state', res) = func mpEnvArgs state ledgerState
    writeTVar mpEnvStateVar state'
    pure res

-- | Add a bunch of transactions (oldest to newest)
--
-- This function returns two lists: the transactions that were added or
-- rejected, and the transactions that could not yet be added, because the
-- Mempool capacity was reached. See 'addTxs' for a function that blocks in
-- case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- POSTCONDITON:
-- > (processed, toProcess) <- implTryAddTxs mpEnv txs
-- > map fst processed ++ toProcess == txs
implTryAddTxs ::
     forall m blk. (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> [GenTx blk]
  -> m ( [(GenTx blk, MempoolAddTxResult blk)]
         -- Transactions that were added or rejected. A prefix of the input
         -- list.
       , [GenTx blk]
         -- Transactions that have not yet been added because the capacity
         -- of the Mempool has been reached. A suffix of the input list.
       )
implTryAddTxs mpEnv txs = go [] (pureTryAddTxs mpArgs txs)
  where
    MempoolEnv { mpEnvStateVar, mpEnvArgs = mpArgs, mpEnvTracer } = mpEnv

    -- Note: we execute the continuation returned by 'atomically'
    go :: [(GenTx blk, MempoolAddTxResult blk)]
          -- ^ Accumulator, stored in reverse order
       -> (InternalState blk -> TryAddTxs blk)
       -> m ( [(GenTx blk, MempoolAddTxResult blk)]
            , [GenTx blk]
            )
    go acc k = join $ atomically $ do
      -- Before executing each step of the 'TryAddTxs' flow, we grab the latest
      -- 'InternalState' from the @TVar@
      is <- readTVar mpEnvStateVar
      case k is of
        Done -> do
          return $ return (reverse acc, [])
        NoSpaceLeft remaining -> do
          return $ return (reverse acc, remaining)
        WriteValidTx is' tx k' -> do
          -- Each time we have found a valid transaction, we update the Mempool.
          -- This keeps our STM transactions short, avoiding repeated work.
          writeTVar mpEnvStateVar is'
          -- Return a continuation that traces and interprets the next step of
          -- the 'TryAddTxs' flow.
          return $ do
            traceWith mpEnvTracer $
              TraceMempoolAddedTx
                tx
                (isMempoolSize is)
                (isMempoolSize is')
            go ((tx, MempoolTxAdded):acc) k'
        RejectInvalidTx tx err k' ->
          -- Invalid transactions do not affect the state, so no need to update.
          return $ do
            traceWith mpEnvTracer $
              TraceMempoolRejectedTx
                tx
                err
                (isMempoolSize is)
            go ((tx, MempoolTxRejected err):acc) k'

implRemoveTxs
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs mpEnv txIds = do
  tm <- atomicallyDoWithState mpEnv (pureRemoveTxs txIds)
  unless (null txIds) $ traceWith (mpEnvTracer mpEnv) tm

implSyncWithLedger :: ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv = do
    (snapshot, tm) <- atomicallyDoWithState mpEnv pureSyncWithLedger
    traceWith (mpEnvTracer mpEnv) tm
    return snapshot

implGetSnapshot ::
  (IOLike m, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} =
    implSnapshotFromIS <$> readTVar mpEnvStateVar

implGetSnapshotFor :: forall m blk.
                      ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
                   => MempoolEnv m blk
                   -> ForgeLedgerState blk
                   -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshotFor mpEnv blockLedgerState =
    updatedSnapshot <$> readTVar mpEnvStateVar
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvArgs
      } = mpEnv

    updatedSnapshot :: InternalState blk -> MempoolSnapshot blk TicketNo
    updatedSnapshot =
          implSnapshotFromIS
        . internalStateFromVR
        . validateStateFor
            (mpArgsCapacityOverride mpEnvArgs)
            (mpArgsLedgerCfg mpEnvArgs)
            blockLedgerState

-- | \( O(1) \). Return the cached value of the current capacity of the
-- mempool in bytes.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity MempoolEnv{mpEnvStateVar} =
    getCapacityIS <$> readTVar mpEnvStateVar
