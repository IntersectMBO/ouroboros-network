{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}


module Ouroboros.Consensus.Mempool.Data (
    MempoolEnv(..)
  , ValidationResult(..)
  , InternalState(..)
  , LedgerInterface(..)
  , MempoolAddTxResult(..)
  , MempoolSnapshot(..)
  , MempoolCapacityBytes(..)
  , MempoolCapacityBytesOverride(..)
  , ForgeLedgerState(..)
  , TraceEventMempool(..)
  ) where

import           Control.Tracer (Tracer)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Word (Word32)
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Ticked (Ticked)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.TxSeq
import           Ouroboros.Consensus.Util.IOLike


import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)


data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger           :: LedgerInterface m blk
    , mpEnvLedgerCfg        :: LedgerConfig blk
    , mpEnvStateVar         :: StrictTVar m (InternalState blk)
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvTxSize           :: GenTx blk -> TxSizeInBytes
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
    }


{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBeforeTip      :: ChainHash blk

    -- | The slot number of the (imaginary) block the txs will be placed in
  , vrSlotNo         :: SlotNo

    -- | Capacity of the Mempool. Corresponds to 'vrBeforeTip' and
    -- 'vrBeforeSlotNo', /not/ 'vrAfter'.
  , vrBeforeCapacity :: MempoolCapacityBytes

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid          :: TxSeq (GenTx blk)

    -- | The cached IDs of transactions that were found to be valid (oldest to
    -- newest)
  , vrValidTxIds     :: Set (GenTxId blk)

    -- | A new transaction (not previously known) which was found to be valid.
    --
    -- n.b. This will only contain a valid transaction that was /newly/ added
    -- to the mempool (not a previously known valid transaction).
  , vrNewValid       :: Maybe (GenTx blk)

    -- | The state of the ledger after applying 'vrValid' against the ledger
    -- state identifeid by 'vrBeforeTip'.
  , vrAfter          :: TickedLedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- From oldest to newest.
  , vrInvalid        :: [(GenTx blk, ApplyTxErr blk)]

    -- | The mempool 'TicketNo' counter.
    --
    -- When validating new transactions, this should be incremented, starting
    -- from 'isLastTicketNo' of the 'InternalState'.
    -- When validating previously applied transactions, this field should not
    -- be affected.
  , vrLastTicketNo   :: TicketNo
  }


{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      --
      -- NOTE: the total size of the transactions in 'isTxs' may exceed the
      -- current capacity ('isCapacity'). When the capacity computed from the
      -- ledger has shrunk, we don't remove transactions from the Mempool to
      -- satisfy the new lower limit. We let the transactions get removed in
      -- the normal way: by becoming invalid w.r.t. the updated ledger state.
      -- We treat a Mempool /over/ capacity in the same way as a Mempool /at/
      -- capacity.
      isTxs          :: !(TxSeq (GenTx blk))

      -- | The cached IDs of transactions currently in the mempool.
      --
      -- This allows one to more quickly lookup transactions by ID from a
      -- 'MempoolSnapshot' (see 'snapshotHasTx').
      --
      -- This should always be in-sync with the transactions in 'isTxs'.
    , isTxIds        :: !(Set (GenTxId blk))

      -- | The cached ledger state after applying the transactions in the
      -- Mempool against the chain's ledger state. New transactions will be
      -- validated against this ledger.
      --
      -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
      -- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
    , isLedgerState  :: !(TickedLedgerState blk)

      -- | The tip of the chain that 'isTxs' was validated against
      --
      -- This comes from the underlying ledger state ('tickedLedgerState')
    , isTip          :: !(ChainHash blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
      --
      -- This comes from 'applyChainTick' ('tickedSlotNo').
    , isSlotNo       :: !SlotNo

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo :: !TicketNo

      -- | Current maximum capacity of the Mempool. Result of
      -- 'computeMempoolCapacity' using the current chain's
      -- 'TickedLedgerState'.
      --
      -- NOTE: this does not correspond to 'isLedgerState', which is the
      -- 'TickedLedgerState' /after/ applying the transactions in the Mempool.
      -- There might be a transaction in the Mempool triggering a change in
      -- the maximum transaction capacity of a block, which would change the
      -- Mempool's capacity (unless overridden). We don't want the Mempool's
      -- capacity to depend on its contents. The mempool is assuming /all/ its
      -- transactions will be in the next block. So any changes caused by that
      -- block will take effect after applying it and will only affect the
      -- next block.
    , isCapacity     :: !MempoolCapacityBytes
    }
  deriving (Generic)

deriving instance ( NoThunks (GenTx blk)
                  , NoThunks (GenTxId blk)
                  , NoThunks (Ticked (LedgerState blk))
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity (see 'MaxTxCapacityOverride')
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.
  deriving (Eq, Show)

-- | Abstract interface needed to run a Mempool.
newtype LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | The result of attempting to add a transaction to the mempool.
data MempoolAddTxResult blk
  = MempoolTxAdded
    -- ^ The transaction was added to the mempool.
  | MempoolTxRejected !(ApplyTxErr blk)
    -- ^ The transaction was rejected and could not be added to the mempool
    -- for the specified reason.

deriving instance Eq (ApplyTxErr blk) => Eq (MempoolAddTxResult blk)
deriving instance Show (ApplyTxErr blk) => Show (MempoolAddTxResult blk)

-- | The ledger state wrt to which we should produce a block
--
-- The transactions in the mempool will be part of the body of a block, but a
-- block consists of a header and a body, and the full validation of a block
-- consists of first processing its header and only then processing the body.
-- This is important, because processing the header may change the state of the
-- ledger: the update system might be updated, scheduled delegations might be
-- applied, etc., and such changes should take effect before we validate any
-- transactions.
data ForgeLedgerState blk =
    -- | The slot number of the block is known
    --
    -- This will only be the case when we realized that we are the slot leader
    -- and we are actually producing a block. It is the caller's responsibility
    -- to call 'applyChainTick' and produce the ticked ledger state.
    ForgeInKnownSlot SlotNo (TickedLedgerState blk)

    -- | The slot number of the block is not yet known
    --
    -- When we are validating transactions before we know in which block they
    -- will end up, we have to make an assumption about which slot number to use
    -- for 'applyChainTick' to prepare the ledger state; we will assume that
    -- they will end up in the slot after the slot at the tip of the ledger.
  | ForgeInUnknownSlot (LedgerState blk)

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacityBytes = MempoolCapacityBytes {
      getMempoolCapacityBytes :: Word32
    }
  deriving (Eq, Show, Generic)

deriving instance NoThunks MempoolCapacityBytes


-- | A pure snapshot of the contents of the mempool. It allows fetching
-- information about transactions in the mempool, and fetching individual
-- transactions.
--
-- This uses a transaction sequence number type for identifying transactions
-- within the mempool sequence. The sequence number is local to this mempool,
-- unlike the transaction hash. This allows us to ask for all transactions
-- after a known sequence number, to get new transactions. It is also used to
-- look up individual transactions.
--
-- Note that it is expected that 'getTx' will often return 'Nothing'
-- even for tx sequence numbers returned in previous snapshots. This happens
-- when the transaction has been removed from the mempool between snapshots.
--
data MempoolSnapshot blk idx = MempoolSnapshot {
    -- | Get all transactions (oldest to newest) in the mempool snapshot along
    -- with their ticket number.
    snapshotTxs         :: [(GenTx blk, idx)]

    -- | Get all transactions (oldest to newest) in the mempool snapshot,
    -- along with their ticket number, which are associated with a ticket
    -- number greater than the one provided.
  , snapshotTxsAfter    :: idx -> [(GenTx blk, idx)]

    -- | Get as many transactions (oldest to newest) from the mempool
    -- snapshot, along with their ticket number, such that their combined size
    -- is <= the given limit (in bytes).
  , snapshotTxsForSize  :: Word32 -> [(GenTx blk, idx)]

    -- | Get a specific transaction from the mempool snapshot by its ticket
    -- number, if it exists.
  , snapshotLookupTx    :: idx -> Maybe (GenTx blk)

    -- | Determine whether a specific transaction exists within the mempool
    -- snapshot.
  , snapshotHasTx       :: GenTxId blk -> Bool

    -- | Get the size of the mempool snapshot.
  , snapshotMempoolSize :: MempoolSize

    -- | The block number of the "virtual block" under construction
  , snapshotSlotNo      :: SlotNo

    -- | The ledger state after all transactions in the snapshot
  , snapshotLedgerState :: TickedLedgerState blk
  }


-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      !(GenTx blk)
      -- ^ New, valid transaction that was added to the Mempool.
      !MempoolSize
      -- ^ The size of the Mempool before adding the transaction.
      !MempoolSize
      -- ^ The size of the Mempool after adding the transaction.
  | TraceMempoolRejectedTx
      !(GenTx blk)
      -- ^ New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      !(ApplyTxErr blk)
      -- ^ The reason for rejecting the transaction.
      !MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolRemoveTxs
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      !MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolManuallyRemovedTxs
      ![GenTxId blk]
      -- ^ Transactions that have been manually removed from the Mempool.
      ![GenTx blk]
      -- ^ Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      !MempoolSize
      -- ^ The current size of the Mempool.

deriving instance ( Eq (GenTx blk)
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  ) => Show (TraceEventMempool blk)
