{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mempool.Impl (
    implTryAddTxs
  , implAddTxsBlock
  , implRemoveTxs
  , implSyncWithLedger
  , implGetSnapshot
  , implGetSnapshotFor
  , implGetCapacity
  , mpEnvTxSize
  , zeroTicketNo
  , initMempoolEnv
  , chainDBLedgerInterface
  , forkSyncStateOnTipPointChange
  ) where

import           Control.Monad.Except
import           Control.Tracer
import qualified Data.Set as Set
import           Data.Typeable

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Data
import           Ouroboros.Consensus.Mempool.Pure
import           Ouroboros.Consensus.Mempool.TxSeq (MempoolSize, TicketNo,
                     TxTicket (..), msNumBytes, zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

initMempoolEnv :: ( IOLike m
                  , NoThunks (GenTxId blk)
                  , LedgerSupportsMempool blk
                  , ValidateEnvelope blk
                  )
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacityBytesOverride
               -> Tracer m (TraceEventMempool blk)
               -> (GenTx blk -> TxSizeInBytes)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride tracer txSize = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState cfg (ForgeInUnknownSlot st)
    isVar <- newTVarIO $ initInternalState capacityOverride zeroTicketNo slot st'
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      , mpEnvTxSize           = txSize
      , mpEnvCapacityOverride = capacityOverride
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
implTryAddTxs
  :: forall m blk. (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> [GenTx blk]
  -> m ( [(GenTx blk, MempoolAddTxResult blk)]
         -- Transactions that were added or rejected. A prefix of the input
         -- list.
       , [GenTx blk]
         -- Transactions that have not yet been added because the capacity
         -- of the Mempool has been reached. A suffix of the input list.
       )
implTryAddTxs mpEnv@MempoolEnv{mpEnvStateVar} toAdd = atomically $ do
  inState <- readTVar mpEnvStateVar
  --let (inState', res) = pureTryAddTx mpEnv (inState,[]) toAdd
  --writeTVar mpEnvStateVar inState'
  --return res
  undefined

-- | Wrapper around 'implTryAddTxs' that blocks until all transaction have
-- either been added to the Mempool or rejected.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- POSTCONDITON:
-- > processed <- addTxs mpEnv txs
-- > map fst processed == txs
implAddTxsBlock
  :: forall m blk. (MonadSTM m, IOLike m, HasTxId (GenTx blk), LedgerSupportsMempool blk)
  => MempoolEnv m blk
  -> [GenTx blk]
  -> m [(GenTx blk, MempoolAddTxResult blk)]
implAddTxsBlock memEnv = \txs -> do
    (processed, toAdd) <- implTryAddTxs memEnv txs
    case toAdd of
      [] -> return processed
      _  -> go [processed] toAdd
  where
    go
      :: [[(GenTx blk, MempoolAddTxResult blk)]]
         -- ^ The outer list is in reverse order, but all the inner lists will
         -- be in the right order.
      -> [GenTx blk]
      -> m [(GenTx blk, MempoolAddTxResult blk)]
    go acc []         = return (concat (reverse acc))
    go acc txs@(tx:_) = do
      let firstTxSize = mpEnvTxSize memEnv tx
      -- Wait until there's at least room for the first transaction we're
      -- trying to add, otherwise there's no point in trying to add it.
      atomically $ do
        curSize <- msNumBytes . snapshotMempoolSize <$> implGetSnapshot memEnv
        MempoolCapacityBytes capacity <- implGetCapacity memEnv
        check (curSize + firstTxSize <= capacity)
      -- It is possible that between the check above and the call below, other
      -- transactions are added, stealing our spot, but that's fine, we'll
      -- just recurse again without progress.
      (added, toAdd) <- implTryAddTxs memEnv txs
      go (added:acc) toAdd


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
    (removed, mempoolSize) <- atomically $ do
      IS { isTxs, isLastTicketNo } <- readTVar mpEnvStateVar
      st <- getCurrentLedgerState mpEnvLedger
      -- Filtering is O(n), but this function will rarely be used, as it is an
      -- escape hatch when there's an inconsistency between the ledger and the
      -- mempool.
      let txTickets' = filter
              ((`notElem` toRemove) . txId . txTicketTx)
              (TxSeq.toList isTxs)
          (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot st)
          vr = revalidateTxsFor
            capacityOverride
            cfg
            slot
            ticked
            isLastTicketNo
            txTickets'
          is' = internalStateFromVR vr
      writeTVar mpEnvStateVar is'
      return (map fst (vrInvalid vr), isMempoolSize is')

    unless (null txIds) $
      traceWith mpEnvTracer $
        TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
  where
    MempoolEnv
      { mpEnvLedgerCfg = cfg
      , mpEnvLedger
      , mpEnvTracer
      , mpEnvStateVar
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

    toRemove = Set.fromList txIds

implSyncWithLedger :: ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
                   => MempoolEnv m blk -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} = do
    (removed, mempoolSize, snapshot) <- atomically $ do
      vr <- validateIS mpEnv
      writeTVar mpEnvStateVar (internalStateFromVR vr)
      -- The size of the mempool /after/ removing invalid transactions.
      mempoolSize <- getMempoolSize mpEnv
      snapshot    <- implGetSnapshot mpEnv
      return (map fst (vrInvalid vr), mempoolSize, snapshot)
    unless (null removed) $
      traceWith mpEnvTracer $ TraceMempoolRemoveTxs removed mempoolSize
    return snapshot

implGetSnapshot :: (IOLike m, HasTxId (GenTx blk))
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
      , mpEnvLedgerCfg
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

    updatedSnapshot :: InternalState blk -> MempoolSnapshot blk TicketNo
    updatedSnapshot =
          implSnapshotFromIS
        . internalStateFromVR
        . validateStateFor capacityOverride mpEnvLedgerCfg blockLedgerState

-- | \( O(1) \). Return the cached value of the current capacity of the
-- mempool in bytes.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity MempoolEnv{mpEnvStateVar} =
    isCapacity <$> readTVar mpEnvStateVar

-- | \( O(1) \). Return the number of transactions in the Mempool paired with
-- their total size in bytes.
getMempoolSize :: IOLike m
               => MempoolEnv m blk
               -> STM m MempoolSize
getMempoolSize MempoolEnv{mpEnvStateVar} =
    isMempoolSize <$> readTVar mpEnvStateVar


-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS :: forall m blk.
              ( IOLike m
              , LedgerSupportsMempool blk
              , HasTxId (GenTx blk)
              , ValidateEnvelope blk
              )
           => MempoolEnv m blk
           -> STM m (ValidationResult blk)
validateIS mpEnv =
    validateStateFor capacityOverride mpEnvLedgerCfg
      <$> (ForgeInUnknownSlot <$> getCurrentLedgerState mpEnvLedger)
      <*> readTVar mpEnvStateVar
  where
    MempoolEnv {
        mpEnvLedger
      , mpEnvLedgerCfg
      , mpEnvStateVar
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv
