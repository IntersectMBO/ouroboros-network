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
--  , zeroTicketNo
  , initMempoolEnv
  , forkSyncStateOnTipPointChange
  , chainDBLedgerInterface
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Tracer
import           Data.Maybe (isJust, isNothing, listToMaybe)
import qualified Data.Set as Set
import           Data.Typeable

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Data
import           Ouroboros.Consensus.Mempool.Pure
import           Ouroboros.Consensus.Mempool.TxSeq (MempoolSize, TicketNo,
                     msNumBytes, zeroTicketNo)
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB, getCurrentLedger)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> getCurrentLedger chainDB
    }

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
-- Read the internal state, class the function with it and writes the state
-- with the first element of the result of the function and returns the second
-- part of the result of the function as result
atomicallyDoWithState :: IOLike m
  => MempoolEnv m blk
  -> (MempoolEnv m blk -> InternalState blk -> LedgerState blk -> (InternalState blk, res))
  -> m (InternalState blk, res)
atomicallyDoWithState mempool func =
  atomically $ do
    state <- readTVar (mpEnvStateVar mempool)
    ledgerState <- getCurrentLedgerState (mpEnvLedger mempool)
    let (state', res) = func mempool state ledgerState
    writeTVar (mpEnvStateVar mempool) state'
    pure (state', res)

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
implTryAddTxs mpEnv = go []
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvLedgerCfg = cfg
      , mpEnvTracer
      , mpEnvTxSize
      } = mpEnv

    done acc toAdd = return (reverse acc, toAdd)

    go acc []                     = done acc []
    go acc toAdd@(firstTx:toAdd') =
        -- Note: we execute the continuation returned by 'atomically'
        join $ atomically $ readTVar mpEnvStateVar >>= tryAdd
      where
        tryAdd is
          -- No space in the Mempool.
          | let firstTxSize = mpEnvTxSize firstTx
                curSize = msNumBytes $ isMempoolSize is
          , curSize + firstTxSize > getMempoolCapacityBytes (isCapacity is)
          = return $ done acc toAdd

          | otherwise
          = do
              let vr  = extendVRNew cfg firstTx mpEnvTxSize $
                          validationResultFromIS is
                  is' = internalStateFromVR vr
              unless (null (vrNewValid vr)) $
                -- Each time we have found a valid transaction, we update the
                -- Mempool. This keeps our STM transactions short, avoiding
                -- repeated work.
                --
                -- Note that even if the transaction were invalid, we could
                -- still write the state, because in that case we would have
                -- that @is == is'@, but there's no reason to do that
                -- additional write.
                writeTVar mpEnvStateVar is'

              -- We only extended the ValidationResult with a single
              -- transaction ('firstTx'). So if it's not in 'vrInvalid', it
              -- must be in 'vrNewValid'.
              return $ case listToMaybe (vrInvalid vr) of
                -- The transaction was valid
                Nothing ->
                  assert (isJust (vrNewValid vr)) $ do
                    traceWith mpEnvTracer $ TraceMempoolAddedTx
                      firstTx
                      (isMempoolSize is)
                      (isMempoolSize is')
                    go ((firstTx, MempoolTxAdded):acc) toAdd'
                Just (_, err) ->
                  assert (isNothing (vrNewValid vr))  $
                  assert (length (vrInvalid vr) == 1) $ do
                    traceWith mpEnvTracer $ TraceMempoolRejectedTx
                      firstTx
                      err
                      (isMempoolSize is)
                    go
                      ((firstTx, MempoolTxRejected err):acc)
                      toAdd'

-- implTryAddTxs mpEnv@MempoolEnv{mpEnvStateVar} toAdd = atomically $ do
--   inState <- readTVar mpEnvStateVar
--   --let (inState', res) = pureTryAddTx mpEnv (inState,[]) toAdd
--   --writeTVar mpEnvStateVar inState'
--   --return res
--   undefined

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
  (_, tm) <- atomicallyDoWithState mpEnv (pureRemoveTxs txIds)
  unless (null txIds) $ traceWith (mpEnvTracer mpEnv) tm

implSyncWithLedger :: ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv = do
  (_, (snapshot, tm)) <- atomicallyDoWithState mpEnv pureSyncWithLedger
  traceWith (mpEnvTracer mpEnv) tm
  return snapshot

implGetSnapshot :: (IOLike m, HasTxId (GenTx blk))
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} =
    pureSnapshotFromIS <$> readTVar mpEnvStateVar

implGetSnapshotFor :: forall m blk.
                      ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
                   => MempoolEnv m blk
                   -> ForgeLedgerState blk
                   -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshotFor mpEnv@MempoolEnv{mpEnvStateVar} blockLedgerState = do
  state <- readTVar mpEnvStateVar
  pure $ updatedSnapshot mpEnv blockLedgerState state


-- | \( O(1) \). Return the cached value of the current capacity of the
-- mempool in bytes.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity MempoolEnv{mpEnvStateVar} =
    isCapacity <$> readTVar mpEnvStateVar
