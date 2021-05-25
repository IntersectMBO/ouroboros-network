{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure side of the Mempool implementation.
--
-- Operations are performed in a pure style returning data types that model
-- the control flow through the operation and can then be interpreted to perform
-- the actual STM/IO operations.

module Ouroboros.Consensus.Mempool.Impl.Pure (
    -- * Mempool
    implTryAddTxs
  , pureGetSnapshotFor
  , pureRemoveTxs
  , pureSyncWithLedger
  , runRemoveTxs
  , runSyncWithLedger
    -- * MempoolSnapshot
  , implSnapshotFromIS
  ) where

import           Control.Exception (assert)
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set
import           Data.Word (Word32)

import           Control.Monad (join)
import           Control.Tracer
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl.Types
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxTicket (..),
                     zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Mempool Implementation
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
implTryAddTxs
  :: forall m blk.
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
  -> [GenTx blk]
     -- ^ The list of transactions to add to the mempool.
  -> m ([MempoolAddTxResult blk], [GenTx blk])
implTryAddTxs istate cfg txSize trcr =
    go []
  where
    go acc = \case
      []     -> pure (reverse acc, [])
      tx:txs -> join $ atomically $ do
        is <- readTVar istate
        case pureTryAddTxs cfg txSize tx is of
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
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> TryAddTxs blk
pureTryAddTxs cfg txSize tx is
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
      (eVtx, vr) = extendVRNew cfg tx txSize $ validationResultFromIS is
      is'        = internalStateFromVR vr

-- | A datatype containing the state resulting after removing the requested
-- transactions from the mempool and maybe a message to be traced while removing
-- them.
data RemoveTxs blk =
    WriteRemoveTxs (InternalState blk) (Maybe (TraceEventMempool blk))

-- | Intepret a 'RemoveTxs' with the resulting values produced by manually
-- removing the transactions given to 'pureRemoveTxs' from the mempool.
runRemoveTxs
  :: forall m blk. IOLike m
  => StrictTVar m (InternalState blk)
  -> RemoveTxs blk
  -> STM m (Maybe (TraceEventMempool blk))
runRemoveTxs stateVar (WriteRemoveTxs is t) = do
    writeTVar stateVar is
    return t

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
pureRemoveTxs cfg capacityOverride txIds IS { isTxs, isLastTicketNo } lstate =
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
                           (TxSeq.toList isTxs)
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot lstate)
        vr             = revalidateTxsFor
                           capacityOverride
                           cfg
                           slot
                           ticked
                           isLastTicketNo
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

-- | A datatype containing the new state produced by syncing with the Ledger, a
-- snapshot of that mempool state and, if needed, a tracing message.
data SyncWithLedger blk =
    NewSyncedState (InternalState blk)
                   (MempoolSnapshot blk TicketNo)
                   (Maybe (TraceEventMempool blk))

-- | Intepret a 'SyncWithLedger' value produced by syncing the transactions in
--  the mempool with the current ledger state of the 'ChainDB'.
--
-- The transactions that exist within the mempool will be revalidated
-- against the current ledger state. Transactions which are found to be
-- invalid with respect to the current ledger state, will be dropped
-- from the mempool, whereas valid transactions will remain.
--
-- n.b. in our current implementation, when one opens a mempool, we
-- spawn a thread which performs this action whenever the 'ChainDB' tip
-- point changes.
runSyncWithLedger
  :: forall m blk. IOLike m
  => StrictTVar m (InternalState blk)
  -> SyncWithLedger blk
  -> STM m (Maybe (TraceEventMempool blk), MempoolSnapshot blk TicketNo)
runSyncWithLedger stateVar (NewSyncedState is msp mTrace) = do
    writeTVar stateVar is
    return (mTrace, msp)

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
    let vr          = validateIS istate lstate lcfg capacityOverride
        removed     = map fst (vrInvalid vr)
        istate'     = internalStateFromVR vr
        mTrace      = if null removed
                      then
                        Nothing
                      else
                        Just $ TraceMempoolRemoveTxs removed (isMempoolSize istate')
        snapshot    = implSnapshotFromIS istate'
    in
      NewSyncedState istate' snapshot mTrace

-- | Get a snapshot of the mempool state that is valid with respect to
-- the given ledger state
pureGetSnapshotFor
  :: forall blk.
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> ForgeLedgerState blk
  -> MempoolCapacityBytesOverride
  -> InternalState blk
  -> MempoolSnapshot blk TicketNo
pureGetSnapshotFor cfg blockLedgerState capacityOverride =
      implSnapshotFromIS
    . internalStateFromVR
    . validateStateFor capacityOverride cfg blockLedgerState

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
implSnapshotFromIS
  :: HasTxId (GenTx blk)
  => InternalState blk
  -> MempoolSnapshot blk TicketNo
implSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotTxsForSize  = implSnapshotGetTxsForSize  is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotLedgerState = isLedgerState              is
    }
 where
  implSnapshotGetTxs :: InternalState blk
                     -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxs = flip implSnapshotGetTxsAfter zeroTicketNo

  implSnapshotGetTxsAfter :: InternalState blk
                          -> TicketNo
                          -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxsAfter IS{isTxs} =
    TxSeq.toTuples . snd . TxSeq.splitAfterTicketNo isTxs

  implSnapshotGetTxsForSize :: InternalState blk
                            -> Word32
                            -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxsForSize IS{isTxs} =
    TxSeq.toTuples . fst . TxSeq.splitAfterTxSize isTxs

  implSnapshotGetTx :: InternalState blk
                    -> TicketNo
                    -> Maybe (Validated (GenTx blk))
  implSnapshotGetTx IS{isTxs} = (isTxs `TxSeq.lookupByTicketNo`)

  implSnapshotHasTx :: Ord (GenTxId blk)
                    => InternalState blk
                    -> GenTxId blk
                    -> Bool
  implSnapshotHasTx IS{isTxIds} = flip Set.member isTxIds

  implSnapshotGetMempoolSize :: InternalState blk
                             -> MempoolSize
  implSnapshotGetMempoolSize = TxSeq.toMempoolSize . isTxs
