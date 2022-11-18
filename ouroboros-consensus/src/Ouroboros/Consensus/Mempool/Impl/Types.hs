{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Types required for implementing the Mempool.

module Ouroboros.Consensus.Mempool.Impl.Types (
    -- * Internal State
    InternalState (..)
  , initInternalState
  , isMempoolSize
    -- * Snapshot
  , MempoolSnapshot (..)
  , implSnapshotFromIS
    -- * Validation result
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
    -- * Tick ledger state
  , ForgeLedgerState (..)
  , tickLedgerState
    -- * Conversions
  , internalStateFromVR
  , validationResultFromIS
    -- * Mempool size
  , MempoolCapacityBytes (..)
  , MempoolCapacityBytesOverride (..)
  , TxSeq.MempoolSize (..)
  , computeMempoolCapacity
    -- * Tx addition
  , MempoolAddTxResult (..)
  , mempoolTxAddedToMaybe
    -- * Tracing
  , TraceEventMempool (..)
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.TxSeq (MempoolSize, TicketNo,
                     TxSeq (..), TxTicket (..), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq (applyDiff)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSizeInBytes)

{-------------------------------------------------------------------------------
  Ledger state considered for forging
-------------------------------------------------------------------------------}

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
    -- and we are actually producing a block.
    ForgeInKnownSlot SlotNo (TickedLedgerState blk DiffMK)

    -- | The slot number of the block is not yet known
    --
    -- When we are validating transactions before we know in which block they
    -- will end up, we have to make an assumption about which slot number to use
    -- for 'applyChainTick' to prepare the ledger state; we will assume that
    -- they will end up in the slot after the slot at the tip of the ledger. -- TODO @js See comment in 'tickLedgerState'
  | ForgeInUnknownSlot (LedgerState blk EmptyMK)

-- | Tick the 'LedgerState' using the given 'BlockSlot' or the next slot after
-- the ledger state on top of which we are going to apply the transactions.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk DiffMK)
tickLedgerState _   (ForgeInKnownSlot slot st) = (slot, st)
tickLedgerState cfg (ForgeInUnknownSlot st) =
  let
      slot =
          -- Optimistically assume that the transactions will be included in a
          -- block in the next available slot
          --
          -- TODO: We should use time here instead
          -- <https://github.com/input-output-hk/ouroboros-network/issues/1298>
          -- Once we do, the ValidateEnvelope constraint can go.
          withOrigin (minimumPossibleSlotNo (Proxy @blk)) succ $ ledgerTipSlot st
  in
    (slot, applyChainTick cfg slot st)

{-------------------------------------------------------------------------------
  Internal State
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
      isTxs          :: !(TxSeq (Validated (GenTx blk)))

      -- | The cached IDs of transactions currently in the mempool.
      --
      -- This allows one to more quickly lookup transactions by ID from a
      -- 'MempoolSnapshot' (see 'snapshotHasTx').
      --
      -- This should always be in-sync with the transactions in 'isTxs'.
    , isTxIds        :: !(Set (GenTxId blk))

      -- | The cached ledger state after ticking the ledger state identified by
      -- 'isTip' to 'isSlotNo' and applying the transactions in the Mempool
      -- against this ledger state. New transactions will be validated by
      -- applying them on top of 'isLedgerState'.
      --
      -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
      -- transactions in 'isTxs' against the ledger identified 'isTip' as tip
      -- after ticking it to 'isSlotNo'.
    , isLedgerState  :: !(TickedLedgerState blk DiffMK)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip          :: !(Point blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
      --
      -- Note in particular that if the mempool is revalidated against a state S
      -- at slot s, then the state will be ticked (for now to the successor
      -- slot, see 'tickLedgerState') and 'isSlotNo' will be set to @succ s@,
      -- which is different from the slot of the original ledger state, which
      -- will remain in 'isTip'.
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

deriving instance ( NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (TickedLedgerState blk DiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) SeqDiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) KeysMK)
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> TxSeq.MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

initInternalState
  :: LedgerSupportsMempool blk
  => MempoolCapacityBytesOverride
  -> TicketNo  -- ^ Used for 'isLastTicketNo'
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> InternalState blk
initInternalState capacityOverride lastTicketNo slot st = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isLedgerState  = st
    , isTip          = castPoint (getTip st)
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity st capacityOverride
    }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | A ValidationResult is created from an InternalState and is used while
-- revalidating the ledger state or validating transactions, but should never be
-- returned to the clients of the mempool API. It merely tracks the result since
-- the last validation and should be used to produce an internal state in the
-- end.
data ValidationResult invalidTx blk = ValidationResult {
      -- | The tip of the chain before applying these transactions
      vrBeforeTip      :: Point blk

      -- | The slot number of the (imaginary) block the txs will be placed in
    , vrSlotNo         :: SlotNo

      -- | Capacity of the Mempool. Corresponds to 'vrBeforeTip' and
      -- 'vrBeforeSlotNo', /not/ 'vrAfter'.
    , vrBeforeCapacity :: MempoolCapacityBytes

      -- | The transactions that were found to be valid (oldest to newest)
    , vrValid          :: TxSeq (Validated (GenTx blk))

      -- | The cached IDs of transactions that were found to be valid (oldest to
      -- newest)
    , vrValidTxIds     :: Set (GenTxId blk)

      -- | A new transaction (not previously known) which was found to be valid.
      --
      -- n.b. This will only contain a valid transaction that was /newly/ added
      -- to the mempool (not a previously known valid transaction).
    , vrNewValid       :: Maybe (Validated (GenTx blk))

      -- | The state of the ledger after applying 'vrValid' against the ledger
      -- state identified by 'vrBeforeTip'.
      --
      -- INVARIANT: Must contain the diffs for all the applied transactions
      -- during the validation process in which this ValidationResult is being
      -- created, as well as the values for the new transactions remaining to be
      -- validated.
    , vrAfter          :: TickedLedgerState blk TrackingMK

      -- | The transactions that were invalid, along with their errors
      --
      -- From oldest to newest.
    , vrInvalid        :: [(invalidTx, ApplyTxErr blk)]

      -- | The mempool 'TicketNo' counter.
      --
      -- When validating new transactions, this should be incremented, starting
      -- from 'isLastTicketNo' of the 'InternalState'.
      -- When validating previously applied transactions, this field should not
      -- be affected.
    , vrLastTicketNo   :: TicketNo
  }

-- | Extend 'ValidationResult' with a previously validated transaction that
-- may or may not be valid in this ledger state
--
-- NOTE: Even previously validated transactions may not be valid in a different
-- ledger state;  it is /still/ useful to indicate whether we have previously
-- validated this transaction because, if we have, we can utilize 'reapplyTx'
-- rather than 'applyTx' and, therefore, skip things like cryptographic
-- signatures.
extendVRPrevApplied :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
                    => LedgerConfig blk
                    -> TxTicket (Validated (GenTx blk))
                    -> ValidationResult (Validated (GenTx blk)) blk
                    -> ValidationResult (Validated (GenTx blk)) blk
extendVRPrevApplied cfg txTicket vr =
    case runExcept (reapplyTx cfg vrSlotNo tx (forgetLedgerTablesDiffsTicked vrAfter)) of
      Left err  -> vr { vrInvalid    = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid      = vrValid :> txTicket
                      , vrValidTxIds = Set.insert (txId (txForgetValidated tx)) vrValidTxIds
                      , vrAfter      = prependLedgerTablesTrackingDiffs st' vrAfter
                      }
  where
    TxTicket { txTicketTx = tx } = txTicket
    ValidationResult { vrValid, vrSlotNo, vrValidTxIds, vrAfter, vrInvalid } = vr

-- | Extend 'ValidationResult' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
--
-- PRECONDITION: 'vrNewValid' is 'Nothing'. In other words: new transactions
-- should be validated one-by-one, not by calling 'extendVRNew' on its result
-- again.
extendVRNew :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
            => LedgerConfig blk
            -> (GenTx blk -> TxSizeInBytes)
            -> WhetherToIntervene
            -> GenTx blk
            -> ValidationResult (GenTx blk) blk
            -> ( Either (ApplyTxErr blk) (Validated (GenTx blk))
               , ValidationResult (GenTx blk) blk
               )
extendVRNew cfg txSize wti tx vr = assert (isNothing vrNewValid) $
    case runExcept (applyTx cfg wti vrSlotNo tx $ forgetLedgerTablesDiffsTicked vrAfter) of
      Left err         ->
        ( Left err
        , vr { vrInvalid      = (tx, err) : vrInvalid
             }
        )
      Right (st', vtx) ->
        ( Right vtx
        , vr { vrValid        = vrValid :> TxTicket vtx nextTicketNo (txSize tx)
             , vrValidTxIds   = Set.insert (txId tx) vrValidTxIds
             , vrNewValid     = Just vtx
             , vrAfter        = prependLedgerTablesTrackingDiffs st' vrAfter
             , vrLastTicketNo = nextTicketNo
             }
        )
  where
    ValidationResult {
        vrValid
      , vrValidTxIds
      , vrAfter
      , vrInvalid
      , vrLastTicketNo
      , vrNewValid
      , vrSlotNo
      } = vr

    nextTicketNo = succ vrLastTicketNo

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: TickedTableStuff (LedgerState blk)
                    => ValidationResult invalidTx blk
                    -> InternalState blk
internalStateFromVR vr = IS {
      isTxs          = vrValid
    , isTxIds        = vrValidTxIds
    , isLedgerState  = forgetLedgerTablesValuesTicked vrAfter
    , isTip          = vrBeforeTip
    , isSlotNo       = vrSlotNo
    , isLastTicketNo = vrLastTicketNo
    , isCapacity     = vrBeforeCapacity
    }
  where
    ValidationResult {
        vrBeforeTip
      , vrSlotNo
      , vrBeforeCapacity
      , vrValid
      , vrValidTxIds
      , vrAfter
      , vrLastTicketNo
      } = vr

-- | Construct a 'ValidationResult' from internal state.
validationResultFromIS :: TickedTableStuff (LedgerState blk)
                       => LedgerTables (LedgerState blk) ValuesMK
                       -> InternalState blk
                       -> ValidationResult invalidTx blk
validationResultFromIS values is = ValidationResult {
      vrBeforeTip      = isTip
    , vrSlotNo         = isSlotNo
    , vrBeforeCapacity = isCapacity
    , vrValid          = isTxs
    , vrValidTxIds     = isTxIds
    , vrNewValid       = Nothing
    , vrAfter          = zipOverLedgerTablesTicked f isLedgerState values
    , vrInvalid        = []
    , vrLastTicketNo   = isLastTicketNo
    }
  where

    f :: Ord k => DiffMK k v -> ValuesMK k v -> TrackingMK k v
    f (ApplyDiffMK d) (ApplyValuesMK v) = ApplyTrackingMK (applyDiff v d) d

    IS {
        isTxs
      , isTxIds
      , isLedgerState
      , isTip
      , isSlotNo
      , isLastTicketNo
      , isCapacity
      } = is

{-------------------------------------------------------------------------------
  Mempool capacity in bytes
-------------------------------------------------------------------------------}

-- | Represents the maximum number of bytes worth of transactions that a
-- 'Mempool' can contain.
newtype MempoolCapacityBytes = MempoolCapacityBytes {
    getMempoolCapacityBytes :: Word32
  }
  deriving newtype (Eq, Show, NoThunks)

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.
  deriving (Eq, Show)

-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
computeMempoolCapacity
  :: LedgerSupportsMempool blk
  => TickedLedgerState blk mk
  -> MempoolCapacityBytesOverride
  -> MempoolCapacityBytes
computeMempoolCapacity st = \case
    NoMempoolCapacityBytesOverride        -> noOverride
    MempoolCapacityBytesOverride override -> override
  where
    noOverride = MempoolCapacityBytes (txsMaxBytes st * 2)

{-------------------------------------------------------------------------------
  Result of adding a transaction to the mempool
-------------------------------------------------------------------------------}

-- | The result of attempting to add a transaction to the mempool.
data MempoolAddTxResult blk
  = MempoolTxAdded !(Validated (GenTx blk))
    -- ^ The transaction was added to the mempool.
  | MempoolTxRejected !(GenTx blk) !(ApplyTxErr blk)
    -- ^ The transaction was rejected and could not be added to the mempool
    -- for the specified reason.

deriving instance (Eq (GenTx blk), Eq (Validated (GenTx blk)), Eq (ApplyTxErr blk)) => Eq (MempoolAddTxResult blk)
deriving instance (Show (GenTx blk), Show (Validated (GenTx blk)), Show (ApplyTxErr blk)) => Show (MempoolAddTxResult blk)

mempoolTxAddedToMaybe :: MempoolAddTxResult blk -> Maybe (Validated (GenTx blk))
mempoolTxAddedToMaybe (MempoolTxAdded vtx) = Just vtx
mempoolTxAddedToMaybe _                    = Nothing

{-------------------------------------------------------------------------------
  Tracing support for the mempool operations
-------------------------------------------------------------------------------}

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      (Validated (GenTx blk))
      -- ^ New, valid transaction that was added to the Mempool.
      TxSeq.MempoolSize
      -- ^ The size of the Mempool before adding the transaction.
      TxSeq.MempoolSize
      -- ^ The size of the Mempool after adding the transaction.
  | TraceMempoolRejectedTx
      (GenTx blk)
      -- ^ New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      (ApplyTxErr blk)
      -- ^ The reason for rejecting the transaction.
      TxSeq.MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolRemoveTxs
      [Validated (GenTx blk)]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      TxSeq.MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolManuallyRemovedTxs
      (NE.NonEmpty (GenTxId blk))
      -- ^ Transactions that have been manually removed from the Mempool.
      [Validated (GenTx blk)]
      -- ^ Previously valid transactions that are no longer valid because they
      -- dependend on transactions that were manually removed from the
      -- Mempool. These transactions have also been removed from the Mempool.
      --
      -- This list shares not transactions with the list of manually removed
      -- transactions.
      TxSeq.MempoolSize
      -- ^ The current size of the Mempool.

deriving instance ( Eq (GenTx blk)
                  , Eq (Validated (GenTx blk))
                  , Eq (GenTxId blk)
                  , Eq (ApplyTxErr blk)
                  ) => Eq (TraceEventMempool blk)

deriving instance ( Show (GenTx blk)
                  , Show (Validated (GenTx blk))
                  , Show (GenTxId blk)
                  , Show (ApplyTxErr blk)
                  ) => Show (TraceEventMempool blk)

{-------------------------------------------------------------------------------
  Snapshot of the mempool
-------------------------------------------------------------------------------}

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
data MempoolSnapshot blk = MempoolSnapshot {
    -- | Get all transactions (oldest to newest) in the mempool snapshot along
    -- with their ticket number.
    snapshotTxs         :: [(Validated (GenTx blk), TicketNo)]

    -- | Get all transactions (oldest to newest) in the mempool snapshot,
    -- along with their ticket number, which are associated with a ticket
    -- number greater than the one provided.
  , snapshotTxsAfter    :: TicketNo -> [(Validated (GenTx blk), TicketNo)]

    -- | Get a specific transaction from the mempool snapshot by its ticket
    -- number, if it exists.
  , snapshotLookupTx    :: TicketNo -> Maybe (Validated (GenTx blk))

    -- | Determine whether a specific transaction exists within the mempool
    -- snapshot.
  , snapshotHasTx       :: GenTxId blk -> Bool

    -- | Get the size of the mempool snapshot.
  , snapshotMempoolSize :: TxSeq.MempoolSize

    -- | The block number of the "virtual block" under construction
  , snapshotSlotNo      :: SlotNo

  , snapshotTipHash     :: ChainHash blk
  }

-- | Create a 'MempoolSnapshot' from a given 'InternalState' of the mempool.
implSnapshotFromIS ::
     HasTxId (GenTx blk)
  => InternalState blk
  -> MempoolSnapshot blk
implSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotTipHash     = pointHash (isTip                      is)
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
