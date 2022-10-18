{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure side of the Mempool implementation.
--
-- Operations are performed in a pure style returning data types that model
-- the control flow through the operation and can then be interpreted to perform
-- the actual STM/IO operations.

module Ouroboros.Consensus.Mempool.Impl.Pure (
    -- * Mempool
    RemoveTxs (..)
  , SyncWithLedger (..)
  , TryAddTxs (..)
  , pureGetSnapshotFor
  , pureRemoveTxs
  , pureSyncWithLedger
  , pureTryAddTxs
    -- * MempoolSnapshot
  , implSnapshotFromIS
  ) where

import           Control.Exception (assert)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set as Set

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Impl.Types
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxTicket (..),
                     zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq

import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSizeInBytes)

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
     -- ^ The function to calculate the size of a transaction.
  -> WhetherToIntervene
  -> GenTx blk
     -- ^ The transaction to add to the mempool.
  -> InternalState blk
     -- ^ The current internal state of the mempool.
  -> LedgerTables (LedgerState blk) ValuesMK
     -- ^ Values for this transaction
  -> TryAddTxs blk
pureTryAddTxs cfg txSize wti tx is values
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
      (eVtx, vr) = extendVRNew cfg txSize wti tx $ validationResultFromIS values is
      is'        = internalStateFromVR vr

-- | A datatype containing the state resulting after removing the requested
-- transactions from the mempool and maybe a message to be traced while removing
-- them.
data RemoveTxs blk =
    WriteRemoveTxs (InternalState blk) (TraceEventMempool blk)

-- | Craft a 'RemoveTxs' that manually removes the given transactions from the
-- mempool, returning inside it an updated InternalState.
pureRemoveTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> [TxTicket (Validated (GenTx blk))] -- ^ Txs to keep
  -> NE.NonEmpty (GenTxId blk)
  -> InternalState blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> RemoveTxs blk
pureRemoveTxs lcfg capacityOverride txs txIds IS { isLastTicketNo, isDbChangelog } slot lstate values =
    let vr          = revalidateTxsFor
                        capacityOverride
                        lcfg
                        slot
                        lstate
                        values
                        isDbChangelog
                        isLastTicketNo
                        txs
        is'         = internalStateFromVR vr
        needsTrace  = TraceMempoolManuallyRemovedTxs
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

-- | Create a 'SyncWithLedger' value representing the values that will need to
-- be stored for committing this synchronization with the Ledger.
--
-- See the documentation of 'runSyncWithLedger' for more context.
pureSyncWithLedger
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => InternalState blk
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> LedgerTables (LedgerState blk) ValuesMK
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> SyncWithLedger blk
pureSyncWithLedger istate slot lstate values lcfg capacityOverride =
    let vr          = revalidateTxsFor
                       capacityOverride
                       lcfg
                       slot
                       lstate
                       values
                       (isDbChangelog istate)
                       (isLastTicketNo istate)
                       (TxSeq.toList $ isTxs istate)
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
-- the given ledger state, together with the ticked ledger state.
pureGetSnapshotFor
  :: forall blk.
     ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> InternalState blk
  -> ForgeLedgerState blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> MempoolSnapshot blk TicketNo
pureGetSnapshotFor _ _ _ ForgeInUnknownSlot{} _ = error "Tried to get a snapshot for unknown slot"
pureGetSnapshotFor cfg capacityOverride is (ForgeInKnownSlot slot st) values = implSnapshotFromIS $
  if (isTip is == castHash (getTipHash st) && isSlotNo is == slot)
  then is
  else internalStateFromVR
     $ revalidateTxsFor
         capacityOverride
         cfg
         slot
         st
         values
         (isDbChangelog is)
         (isLastTicketNo is)
         (TxSeq.toList $ isTxs is)

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

-- | Create a 'MempoolSnapshot' from a given 'InternalState' of the mempool.
implSnapshotFromIS
  :: HasTxId (GenTx blk)
  => InternalState blk
  -> MempoolSnapshot blk TicketNo
implSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotTipHash     = isTip                      is
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
