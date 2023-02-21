{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definition of common types used in "Ouroboros.Consensus.Mempool.Init",
-- "Ouroboros.Consensus.Mempool.Update" and "Ouroboros.Consensus.Mempool.Query".
module Ouroboros.Consensus.Mempool.Impl.Common (
    -- * Internal state
    InternalState (..)
  , isMempoolSize
    -- * Mempool environment
  , MempoolEnv (..)
  , initMempoolEnv
    -- * Ledger interface
  , LedgerInterface (..)
  , chainDBLedgerInterface
    -- * Validation
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
  , revalidateTxsFor
    -- * Tracing
  , TraceEventMempool (..)
    -- * Conversions
  , internalStateFromVR
  , snapshotFromIS
  , validationResultFromIS
    -- * Ticking a ledger state
  , tickLedgerState
    -- * Exported for deprecated modules
  , initInternalState
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import           Control.Exception (assert)
import           Control.Monad.Trans.Except (runExcept)
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.Map.Diff.Strict.Internal
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Generics (Generic)

import           Control.Tracer

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.LedgerDB.Query
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
                     (PointNotFound)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (LedgerTables (..),
                     ledgerState)
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Mempool.API hiding (MempoolCapacityBytes,
                     MempoolCapacityBytesOverride, MempoolSize,
                     TraceEventMempool, computeMempoolCapacity)
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.TxSeq (TxSeq (..), TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq

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

      -- | The cached ledger state after applying the transactions in the
      -- Mempool against the chain's ledger state. New transactions will be
      -- validated against this ledger.
      --
      -- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
      -- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
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
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
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
    , isTip          = castPoint $ getTip st
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity st capacityOverride
    }

{-------------------------------------------------------------------------------
  Ledger Interface
-------------------------------------------------------------------------------}

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
    { -- | Get the current tip of the LedgerDB.
      getCurrentLedgerState :: STM m (LedgerState blk EmptyMK)
      -- | Get values at the given point on the chain. Returns Nothing if the
      -- anchor moved or if the state is not found on the ledger db.
    , getLedgerTablesAtFor
        :: Point blk
        -> [GenTx blk]
        -> m (Either
              (PointNotFound blk)
              (LedgerTables (LedgerState blk) ValuesMK))
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     ( IOLike m
     , LedgerSupportsMempool blk
     )
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState =
        ledgerState . current <$> ChainDB.getLedgerDB chainDB
    , getLedgerTablesAtFor = \pt txs -> do
        let keys = ExtLedgerStateTables
                 $ foldl' (zipLedgerTables (<>)) emptyLedgerTables
                 $ map getTransactionKeySets txs
        fmap unExtLedgerStateTables <$> ChainDB.getLedgerTablesAtFor chainDB pt keys
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger           :: LedgerInterface m blk
    , mpEnvLedgerCfg        :: LedgerConfig blk
    , mpEnvStateVar         :: StrictTMVar m (InternalState blk)
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvTxSize           :: GenTx blk -> TxSizeInBytes
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
    }

initMempoolEnv :: ( IOLike m
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
    isVar <- newTMVarIO $ initInternalState capacityOverride TxSeq.zeroTicketNo slot st'
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      , mpEnvTxSize           = txSize
      , mpEnvCapacityOverride = capacityOverride
      }

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk DiffMK)
tickLedgerState _cfg (ForgeInKnownSlot slot st) = (slot, st)
tickLedgerState  cfg (ForgeInUnknownSlot st) =
    (slot, applyChainTick cfg slot st)
  where
    -- Optimistically assume that the transactions will be included in a block
    -- in the next available slot
    --
    -- TODO: We should use time here instead
    -- <https://github.com/input-output-hk/ouroboros-network/issues/1298>
    -- Once we do, the ValidateEnvelope constraint can go.
    slot :: SlotNo
    slot = case ledgerTipSlot st of
             Origin      -> minimumPossibleSlotNo (Proxy @blk)
             NotOrigin s -> succ s

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

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
      -- state identifeid by 'vrBeforeTip'.
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
-- n.b. Even previously validated transactions may not be valid in a different
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
internalStateFromVR ::
     HasTickedLedgerTables (LedgerState blk)
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
validationResultFromIS ::
     HasTickedLedgerTables (LedgerState blk)
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
    f (DiffMK d) (ValuesMK v) = TrackingMK (unsafeApplyDiff v d) d

    IS {
        isTxs
      , isTxIds
      , isLedgerState
      , isTip
      , isSlotNo
      , isLastTicketNo
      , isCapacity
      } = is

-- | Create a Mempool Snapshot from a given Internal State of the mempool.
snapshotFromIS
  :: (HasTxId (GenTx blk), GetTip (Ticked1 (LedgerState blk)))
  => InternalState blk
  -> MempoolSnapshot blk
snapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo                   is
    , snapshotTipHash     = pointHash $ castPoint $ getTip $ isLedgerState is
    , snapshotState       = isLedgerState is
    }
 where
  implSnapshotGetTxs :: InternalState blk
                     -> [(Validated (GenTx blk), TicketNo)]
  implSnapshotGetTxs = flip implSnapshotGetTxsAfter TxSeq.zeroTicketNo

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

{-------------------------------------------------------------------------------
  Validating txs
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


{-------------------------------------------------------------------------------
  Tracing support for the mempool operations
-------------------------------------------------------------------------------}

-- | Events traced by the Mempool.
data TraceEventMempool blk
  = TraceMempoolAddedTx
      (Validated (GenTx blk))
      -- ^ New, valid transaction that was added to the Mempool.
      MempoolSize
      -- ^ The size of the Mempool before adding the transaction.
      MempoolSize
      -- ^ The size of the Mempool after adding the transaction.
  | TraceMempoolRejectedTx
      (GenTx blk)
      -- ^ New, invalid transaction thas was rejected and thus not added to
      -- the Mempool.
      (ApplyTxErr blk)
      -- ^ The reason for rejecting the transaction.
      MempoolSize
      -- ^ The current size of the Mempool.
  | TraceMempoolRemoveTxs
      [Validated (GenTx blk)]
      -- ^ Previously valid transactions that are no longer valid because of
      -- changes in the ledger state. These transactions have been removed
      -- from the Mempool.
      MempoolSize
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
      MempoolSize
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
