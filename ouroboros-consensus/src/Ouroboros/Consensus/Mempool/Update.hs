{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

-- | Operations that update the mempool. They are internally divided in the pure
-- and impure sides of the operation.
module Ouroboros.Consensus.Mempool.Update (
    implRemoveTxs
  , implSyncWithLedger
  , implTryAddTxs
    -- * Exported for deprecated modules
  , pureRemoveTxs
  , pureSyncWithLedger
  ) where

import           Control.Exception (assert)
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set

import           Control.Monad (join)
import           Control.Tracer
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Mempool.API hiding (MempoolCapacityBytes,
                     MempoolCapacityBytesOverride, MempoolSize,
                     TraceEventMempool, computeMempoolCapacity)
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common
import           Ouroboros.Consensus.Mempool.TxSeq (TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq

{-------------------------------------------------------------------------------
  Add transactions
-------------------------------------------------------------------------------}

-- | Result of trying to add a transaction to the mempool.
data TryAddTxs blk =
    -- | No space is left in the mempool and no more transactions could be
    -- added.
    NoSpaceLeft
    -- | A transaction was processed.
  | TryAddTxs
      (Maybe (InternalState blk))
      -- ^ If the transaction was accepted, the new state that can be written to
      -- the TVar.
      (MempoolAddTxResult blk)
      -- ^ The result of trying to add the transaction to the mempool.
      (TraceEventMempool blk)
      -- ^ The event emitted by the operation.

-- | Add a list of transactions (oldest to newest) by interpreting a 'TryAddTxs'
-- from 'pureTryAddTxs'.
--
-- This function returns two lists: the transactions that were added or
-- rejected, and the transactions that could not yet be added, because the
-- Mempool capacity was reached. See 'addTxs' for a function that blocks in
-- case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- See the necessary invariants on the Haddock for 'API.tryAddTxs'.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- INVARIANT: The code needs that read and writes on the state are coupled
-- together or inconsistencies will arise. To ensure that STM transactions are
-- short, each iteration of the helper function is a separate STM transaction.
implTryAddTxs ::
     ( MonadSTM m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => StrictTVar m (InternalState blk)
     -- ^ The InternalState TVar.
  -> LedgerConfig blk
     -- ^ The configuration of the ledger.
  -> (GenTx blk -> TxSizeInBytes)
     -- ^ The function to calculate the size of a
     -- transaction.
  -> Tracer m (TraceEventMempool blk)
     -- ^ The tracer.
  -> WhetherToIntervene
  -> [GenTx blk]
     -- ^ The list of transactions to add to the mempool.
  -> m ([MempoolAddTxResult blk], [GenTx blk])
implTryAddTxs istate cfg txSize trcr wti =
    go []
  where
    go acc = \case
      []     -> pure (reverse acc, [])
      tx:txs -> join $ atomically $ do
        is <- readTVar istate
        case pureTryAddTxs cfg txSize wti tx is of
          NoSpaceLeft             -> pure $ pure (reverse acc, tx:txs)
          TryAddTxs is' result ev -> do
            whenJust is' (writeTVar istate)
            pure $ do
              traceWith trcr ev
              go (result:acc) txs

-- | Craft a 'TryAddTxs' value containing the resulting state if applicable, the
-- tracing event and the result of adding this transaction. See the
-- documentation of 'implTryAddTxs' for some more context.
pureTryAddTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerCfg (LedgerState blk)
     -- ^ The ledger configuration.
  -> (GenTx blk -> TxSizeInBytes)
     -- ^ The function to claculate the size of a transaction.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> TryAddTxs blk
pureTryAddTxs cfg txSize wti tx is
  | let size    = txSize tx
        curSize = msNumBytes  $ isMempoolSize is
  , curSize + size > getMempoolCapacityBytes (isCapacity is)
  = NoSpaceLeft
  | otherwise
  = case eVtx of
      -- We only extended the ValidationResult with a single transaction
      -- ('tx'). So if it's not in 'vrInvalid', it must be in 'vrNewValid'.
      Right vtx ->
        assert (isJust (vrNewValid vr)) $
          TryAddTxs
            (Just is')
            (MempoolTxAdded vtx)
            (TraceMempoolAddedTx
              vtx
              (isMempoolSize is)
              (isMempoolSize is')
            )
      Left err ->
        assert (isNothing (vrNewValid vr))  $
          assert (length (vrInvalid vr) == 1) $
            TryAddTxs
              Nothing
              (MempoolTxRejected tx err)
              (TraceMempoolRejectedTx
               tx
               err
               (isMempoolSize is)
              )
    where
      (eVtx, vr) = extendVRNew cfg txSize wti tx $ validationResultFromIS is
      is'        = internalStateFromVR vr

{-------------------------------------------------------------------------------
  Remove transactions
-------------------------------------------------------------------------------}

-- | A datatype containing the state resulting after removing the requested
-- transactions from the mempool and maybe a message to be traced while removing
-- them.
data RemoveTxs blk =
    WriteRemoveTxs (InternalState blk) (Maybe (TraceEventMempool blk))

-- | See 'Ouroboros.Consensus.Mempool.API.removeTxs'.
implRemoveTxs ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs menv txs = do
    tr <- atomically $ do
        is <- readTVar istate
        ls <- getCurrentLedgerState ldgrInterface
        let WriteRemoveTxs is' t = pureRemoveTxs cfg co txs is ls
        writeTVar istate is'
        pure t
    whenJust tr (traceWith trcr)
  where
    MempoolEnv { mpEnvStateVar = istate
               , mpEnvLedger = ldgrInterface
               , mpEnvTracer = trcr
               , mpEnvLedgerCfg = cfg
               , mpEnvCapacityOverride = co
               } = menv

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> [GenTxId blk]
  -> InternalState blk
  -> LedgerState blk
  -> RemoveTxs blk
pureRemoveTxs cfg capacityOverride txIds is lstate =
    -- Filtering is O(n), but this function will rarely be used, as it is an
    -- escape hatch when there's an inconsistency between the ledger and the
    -- mempool.
    let toRemove       = Set.fromList txIds
        txTickets'     = filter
                           (   (`notElem` toRemove)
                             . txId
                             . txForgetValidated
                             . txTicketTx
                           )
                           (TxSeq.toList (isTxs is))
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot lstate)
        vr             = revalidateTxsFor
                           capacityOverride
                           cfg
                           slot
                           ticked
                           (isLastTicketNo is)
                           txTickets'
        is'            = internalStateFromVR vr
        needsTrace     = if null txIds
                         then
                           Nothing
                         else
                           Just $ TraceMempoolManuallyRemovedTxs
                             txIds
                             (map fst (vrInvalid vr))
                             (isMempoolSize is')
    in WriteRemoveTxs is' needsTrace

{-------------------------------------------------------------------------------
  Sync with ledger
-------------------------------------------------------------------------------}

-- | A datatype containing the new state produced by syncing with the Ledger, a
-- snapshot of that mempool state and, if needed, a tracing message.
data SyncWithLedger blk =
    NewSyncedState (InternalState blk)
                   (MempoolSnapshot blk)
                   (Maybe (TraceEventMempool blk))

-- | See 'Ouroboros.Consensus.Mempool.API.syncWithLedger'.
implSyncWithLedger ::
     (
       IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk)
implSyncWithLedger menv = do
    (mTrace, mp) <- atomically $ do
      is <- readTVar istate
      ls <- getCurrentLedgerState ldgrInterface
      let NewSyncedState is' msp mTrace = pureSyncWithLedger is ls cfg co
      writeTVar istate is'
      return (mTrace, msp)
    whenJust mTrace (traceWith trcr)
    return mp
  where
    MempoolEnv { mpEnvStateVar = istate
               , mpEnvLedger = ldgrInterface
               , mpEnvTracer = trcr
               , mpEnvLedgerCfg = cfg
               , mpEnvCapacityOverride = co
               } = menv

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => InternalState blk
  -> LedgerState blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> SyncWithLedger blk
pureSyncWithLedger istate lstate lcfg capacityOverride =
    let vr          = validateStateFor
                        capacityOverride
                        lcfg
                        (ForgeInUnknownSlot lstate)
                        istate
        removed     = map fst (vrInvalid vr)
        istate'     = internalStateFromVR vr
        mTrace      = if null removed
                      then
                        Nothing
                      else
                        Just $ TraceMempoolRemoveTxs removed (isMempoolSize istate')
        snapshot    = snapshotFromIS istate'
    in
      NewSyncedState istate' snapshot mTrace
