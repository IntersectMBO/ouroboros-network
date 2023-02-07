{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set

import           Cardano.Slotting.Slot

import           Control.Tracer
import           Ouroboros.Consensus.Block.Abstract (castHash, castPoint,
                     pointHash)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Storage.LedgerDB.HD.ReadsKeySets
                     (PointNotFound (..))
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

-- | Result of processing a transaction to be added to the mempool.
data TransactionResult blk =
    NoSpaceLeft
  | TransactionProcessed
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
implTryAddTxs
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> WhetherToIntervene
  -> [GenTx blk]
  -> m ([MempoolAddTxResult blk], [GenTx blk])
implTryAddTxs mpEnv wti =
    go []
  where
    MempoolEnv {
        mpEnvLedgerCfg = cfg
      , mpEnvLedger    = ldgrInterface
      , mpEnvStateVar  = istate
      , mpEnvTracer    = trcr
      , mpEnvTxSize    = txSize
      } = mpEnv

    -- MAYBE batch transaction keys queries
    go :: [MempoolAddTxResult blk]
       -> [GenTx blk]
       -> m ([MempoolAddTxResult blk], [GenTx blk])
    go acc = \case
      []            -> pure (reverse acc, [])
      txs@(tx:next) -> do
        is <- atomically $ takeTMVar istate
        mTbs <- getLedgerTablesAtFor ldgrInterface (isTip is) [tx]
        case mTbs of
          Right tbs -> case pureTryAddTx cfg txSize wti tx is tbs of
            NoSpaceLeft -> do
              atomically $ putTMVar istate is
              pure (reverse acc, txs)
            TransactionProcessed is' result ev -> do
              atomically $ putTMVar istate $ fromMaybe is is'
              traceWith trcr ev
              go (result:acc) next
          Left PointNotFound{} -> do
            -- We couldn't retrieve the values because the state is no longer on
            -- the db. We need to resync.
            atomically $ putTMVar istate is
            (_, mTrace) <- implSyncWithLedger mpEnv
            whenJust mTrace (traceWith trcr)
            go acc txs


-- | Craft a 'TryAddTxs' value containing the resulting state if applicable, the
-- tracing event and the result of adding this transaction. See the
-- documentation of 'implTryAddTxs' for some more context.
pureTryAddTx
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerCfg (LedgerState blk)
     -- ^ The ledger configuration.
  -> (GenTx blk -> TxSizeInBytes)
     -- ^ The function to calculate the size of a transaction.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> LedgerTables (LedgerState blk) ValuesMK
     -- ^ Values for this transaction
  -> TransactionResult blk
pureTryAddTx cfg txSize wti tx is values
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
          TransactionProcessed
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
            TransactionProcessed
              Nothing
              (MempoolTxRejected tx err)
              (TraceMempoolRejectedTx
               tx
               err
               (isMempoolSize is)
              )
  where
    (eVtx, vr) = extendVRNew cfg txSize wti tx $ validationResultFromIS values is
    is'        = internalStateFromVR vr

{-------------------------------------------------------------------------------
  Remove transactions
-------------------------------------------------------------------------------}

-- | See 'Ouroboros.Consensus.Mempool.API.removeTxs'.
implRemoveTxs ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
   => MempoolEnv m blk
   -> NE.NonEmpty (GenTxId blk)
   -> m ()
implRemoveTxs mpEnv toRemove = do
    (is, ls) <- atomically $ do
      is <- takeTMVar istate
      ls <- getCurrentLedgerState ldgrInterface
      pure (is, ls)
    let toKeep = filter
                 (   (`notElem` Set.fromList (NE.toList toRemove))
                     . txId
                     . txForgetValidated
                     . txTicketTx
                 )
                 (TxSeq.toList $ isTxs is)
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot ls)
        toKeep' = [ txForgetValidated . TxSeq.txTicketTx $ tx | tx <- toKeep ]
    mTbs <- getLedgerTablesAtFor ldgrInterface (castPoint (getTip ls)) toKeep'
    case mTbs of
      Left PointNotFound{} -> do
        atomically $ putTMVar istate is
        implRemoveTxs mpEnv toRemove
      Right tbs -> do
        let (is', t) = pureRemoveTxs
                         capacityOverride
                         cfg
                         slot
                         ticked
                         tbs
                         (isLastTicketNo is)
                         toKeep
                         toRemove
        atomically $ putTMVar istate is'
        traceWith trcr t
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> TicketNo
  -> [TxTicket (Validated (GenTx blk))] -- ^ Txs to keep
  -> NE.NonEmpty (GenTxId blk) -- ^ IDs to remove
  -> (InternalState blk, TraceEventMempool blk)
pureRemoveTxs capacityOverride lcfg slot lstate values tkt txs txIds =
    let (is', removed) = revalidateTxsFor
                           capacityOverride
                           lcfg
                           slot
                           lstate
                           values
                           tkt
                           txs
        trace = TraceMempoolManuallyRemovedTxs
                  txIds
                  removed
                  (isMempoolSize is')
    in (is', trace)

{-------------------------------------------------------------------------------
  Sync with ledger
-------------------------------------------------------------------------------}

-- | See 'Ouroboros.Consensus.Mempool.API.syncWithLedger'.
implSyncWithLedger ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , ValidateEnvelope blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk, Maybe (TraceEventMempool blk))
implSyncWithLedger mpEnv = do
  (is, ls) <- atomically $ do
    is <- takeTMVar istate
    ls <- getCurrentLedgerState ldgrInterface
    pure (is, ls)

  let (slot, ls') = tickLedgerState cfg $ ForgeInUnknownSlot ls

  if pointHash (isTip is) == castHash (getTipHash ls) &&
     isSlotNo is == slot
    then do
    -- The tip didn't change, put the same state.
    atomically $ putTMVar istate is
    pure (snapshotFromIS is, Nothing)
    else do
    -- We need to revalidate
    let pt = castPoint (getTip ls)
        txs = [ txForgetValidated . TxSeq.txTicketTx $ tx
              | tx <- TxSeq.toList $ isTxs is
              ]
    mTbs <- getLedgerTablesAtFor ldgrInterface pt txs
    case mTbs of
      Right tbs -> do
        let (is', mTrace) = pureSyncWithLedger
                              capacityOverride
                              cfg
                              slot
                              ls'
                              tbs
                              is
        atomically $ putTMVar istate is'
        whenJust mTrace (traceWith trcr)
        return (snapshotFromIS is', mTrace)
      Left PointNotFound{} -> do
        -- If the point is gone, resync
        atomically $ putTMVar istate is
        implSyncWithLedger mpEnv
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ( InternalState blk
     , Maybe (TraceEventMempool blk)
     )
pureSyncWithLedger capacityOverride lcfg slot lstate values istate =
  let (is', removed) = revalidateTxsFor
                         capacityOverride
                         lcfg
                         slot
                         lstate
                         values
                         (isLastTicketNo istate)
                         (TxSeq.toList $ isTxs istate)
      mTrace = if null removed
               then
                 Nothing
               else
                 Just $ TraceMempoolRemoveTxs removed (isMempoolSize is')
  in (is', mTrace)
