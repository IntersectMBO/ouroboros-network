{-# LANGUAGE FlexibleContexts #-}

-- | Pure side of the Mempool implementation.
module Ouroboros.Consensus.Mempool.Impl.Pure (
    -- * Mempool
    TransactionResult (..)
  , pureGetSnapshotFor
  , pureRemoveTxs
  , pureSyncWithLedger
  , pureTryAddTx
  ) where

import           Control.Exception (assert)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, isNothing)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Mempool.API (TxSizeInBytes)
import           Ouroboros.Consensus.Mempool.Impl.Types
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)

{-------------------------------------------------------------------------------
  Mempool Implementation
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

-- | Craft a 'TransactionResult' value containing the resulting state if
-- applicable, the tracing event and the result of adding this transaction. See
-- the documentation of 'implTryAddTxs' for some more context.
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

-- | Revalidate the other transactions in the mempool and provide an updated
-- InternalState
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

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
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

-- | Get a snapshot of the mempool state that is valid with respect to
-- the given ledger state, together with the ticked ledger state.
pureGetSnapshotFor
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ForgeLedgerState blk
  -> MempoolSnapshot blk
pureGetSnapshotFor _ _ _ _ ForgeInUnknownSlot{} =
  error "Tried to get a snapshot for unknown slot"
pureGetSnapshotFor capacityOverride cfg values is (ForgeInKnownSlot slot st) =
  implSnapshotFromIS $
    if (pointHash (isTip is) == castHash (getTipHash st) && isSlotNo is == slot)
    then is
    else fst $ revalidateTxsFor
                 capacityOverride
                 cfg
                 slot
                 st
                 values
                 (isLastTicketNo is)
                 (TxSeq.toList $ isTxs is)

{-------------------------------------------------------------------------------
  Revalidate transactions
-------------------------------------------------------------------------------}

-- | Revalidate the given transactions against the given ticked ledger state.
--
-- Note that this function will perform revalidation so it is expected that the
-- transactions given to it were previously applied, for example if we are
-- revalidating the whole set of transactions onto a new state, or if we remove
-- some transactions and revalidate the remaining ones.
revalidateTxsFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
     -- ^ The ticked ledger state againt which txs will be revalidated
  -> LedgerTables (LedgerState blk) ValuesMK
     -- ^ The tables with all the inputs for the transactions
  -> TicketNo -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (Validated (GenTx blk))]
  -> ( InternalState blk
     , [Validated (GenTx blk)]
     )
revalidateTxsFor capacityOverride cfg slot st values lastTicketNo txTickets =
    let vr = repeatedly
             (extendVRPrevApplied cfg)
             txTickets
             $ validationResultFromIS values
             $ initInternalState capacityOverride lastTicketNo slot st
    in (internalStateFromVR vr, map fst (vrInvalid vr))
