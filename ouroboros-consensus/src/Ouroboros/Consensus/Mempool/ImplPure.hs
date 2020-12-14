{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mempool.ImplPure (
    MempoolArgs(..)
  , InternalState -- opaque
  , initInternalState
  , getCapacityIS
  , getTxIdsIS
  , MempoolCapacityBytesOverride(..)
  , tickLedgerState
  , ValidationResult(..)
  , revalidateTxsFor
  , validateStateFor
  , validationResultFromIS
  , validateIS
  , extendVRNew
  , internalStateFromVR
  , implSnapshotGetMempoolSize
  , implSnapshotHasTx
  , implSnapshotGetTxs
  , implSnapshotGetTx
  , implSnapshotGetTxsForSize
  , implSnapshotFromIS
  , TryAddTxs(..)
  , pureTryAddTxs
  , pureSyncWithLedger
  , pureRemoveTxs
  , isMempoolSize
    -- * For testing purposes
  , runTryAddTxs
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Monad.State.Strict (State, get, put, runState)
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | An override for the default 'MempoolCapacityBytes' which is 2x the
-- maximum transaction capacity (see 'MaxTxCapacityOverride')
data MempoolCapacityBytesOverride
  = NoMempoolCapacityBytesOverride
    -- ^ Use 2x the maximum transaction capacity of a block. This will change
    -- dynamically with the protocol parameters adopted in the current ledger.
  | MempoolCapacityBytesOverride !MempoolCapacityBytes
    -- ^ Use the following 'MempoolCapacityBytes'.
  deriving (Eq, Show)

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

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

data MempoolArgs blk = MempoolArgs {
      mpArgsLedgerCfg        :: LedgerConfig blk
    , mpArgsTxSize           :: GenTx blk -> TxSizeInBytes
    , mpArgsCapacityOverride :: MempoolCapacityBytesOverride
    }

initInternalState
  :: LedgerSupportsMempool blk
  => MempoolCapacityBytesOverride
  -> TicketNo  -- ^ Used for 'isLastTicketNo'
  -> SlotNo
  -> TickedLedgerState blk
  -> InternalState blk
initInternalState capacityOverride lastTicketNo slot ledgerState = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isLedgerState  = ledgerState
    , isTip          = castHash (getTipHash ledgerState)
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity ledgerState capacityOverride
    }

getCapacityIS :: InternalState blk -> MempoolCapacityBytes
getCapacityIS = isCapacity

getTxIdsIS :: InternalState blk -> Set (GenTxId blk)
getTxIdsIS = isTxIds

-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
computeMempoolCapacity
  :: LedgerSupportsMempool blk
  => TickedLedgerState blk
  -> MempoolCapacityBytesOverride
  -> MempoolCapacityBytes
computeMempoolCapacity ledgerState = \case
    NoMempoolCapacityBytesOverride        -> noOverride
    MempoolCapacityBytesOverride override -> override
  where
    noOverride = MempoolCapacityBytes (maxTxCapacity ledgerState * 2)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

-- | Data type representating the control flow of adding a transaction.
--
-- Context:
--
-- The Mempool's 'InternalState' is stored in a @TVar@, which means it has to be
-- updated atomically. However, when adding a bunch of transactions, validating
-- all of them might not be cheap, so in case of a 'retry' we might have to
-- revalidate /many/ of them. This duplicated work is bad for the throughput and
-- the latency.
--
-- To mitigate this, we try to keep our STM transactions as short as possible.
-- Each time we have validated a transaction and found it to be valid, we update
-- the state in the @TVar@ accordingly. This means we will need an STM
-- transaction per (blockchain) transaction.
--
-- By representing this control flow by a data type we can decouple the actual
-- logic from how it is executed, allowing for a concurrent, STM-based
-- interpreter as well as a single-threaded, pure interpreter for testing
-- purposes.
data TryAddTxs blk =
    -- | The transaction was valid.
    WriteValidTx
      (InternalState blk)
      -- ^ The resulting state that can be written to the @TVar@.
      (GenTx blk)
      -- ^ The valid transaction
      (InternalState blk -> TryAddTxs blk)
      -- ^ How to continue with the remaining transactions

    -- | The transaction was invalid
  | RejectInvalidTx
      (GenTx blk)
      -- ^ The invalid transaction
      (ApplyTxErr blk)
      -- ^ Why the transaction was invalid
      (InternalState blk -> TryAddTxs blk)
      -- ^ How to continue with the remaining transactions

    -- | No space left in the Mempool for these transactions
  | NoSpaceLeft [GenTx blk]

    -- | No more transactions to add
  | Done

-- | Return the control flow to add a list of transactions against the given
-- 'InternalState'.
--
-- See 'runTryAddTxs' for an example of a pure interpreter of this control flow.
pureTryAddTxs ::
     (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolArgs blk
  -> [GenTx blk]
  -> InternalState blk
  -> TryAddTxs blk
pureTryAddTxs _      [] _  = Done
pureTryAddTxs mpArgs toAdd@(firstTx:toAdd') state
    | let firstTxSize = mpArgsTxSize mpArgs firstTx
          curSize = msNumBytes $ isMempoolSize state
    , curSize + firstTxSize > getMempoolCapacityBytes (isCapacity state)
    = NoSpaceLeft toAdd

    | otherwise
    , let vr = extendVRNew cfg firstTx txSize $ validationResultFromIS state
    = case vrInvalid vr of
        [(_, err)] ->
          RejectInvalidTx
            firstTx
            err
            (pureTryAddTxs mpArgs toAdd')
        -- We only extended the ValidationResult with a single transaction
        -- ('firstTx'). So if it's not in 'vrInvalid', it will be in
        -- 'vrNewValid'.
        _otherwise ->
          WriteValidTx
            (internalStateFromVR vr)
            firstTx
            (pureTryAddTxs mpArgs toAdd')
  where
    MempoolArgs
      { mpArgsLedgerCfg = cfg
      , mpArgsTxSize    = txSize
      } = mpArgs

-- | Run the 'TryAddTxs' flow in a pure way, returning the result as well as the
-- final 'InternalState'.
runTryAddTxs ::
     forall blk. (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolArgs blk
  -> InternalState blk
  -> [GenTx blk]
  -> ( ([(GenTx blk, MempoolAddTxResult blk)], [GenTx blk])
     , InternalState blk
     )
runTryAddTxs mpArgs = \state txs ->
    runState (go (pureTryAddTxs mpArgs txs state)) state
  where
    go :: TryAddTxs blk
       -> State (InternalState blk)
                ([(GenTx blk, MempoolAddTxResult blk)], [GenTx blk])
    go = \case
        WriteValidTx state' tx k -> do
          put state'
          (res, remaining) <- go (k state')
          return ((tx, MempoolTxAdded):res, remaining)
        RejectInvalidTx tx err k -> do
          state <- get
          (res, remaining) <- go (k state)
          return ((tx, MempoolTxRejected err):res, remaining)
        NoSpaceLeft remaining ->
          return ([], remaining)
        Done ->
          return ([], [])

pureRemoveTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => [GenTxId blk]
  -> MempoolArgs blk
  -> InternalState blk
  -> LedgerState blk
  -> (InternalState blk, TraceEventMempool blk)
pureRemoveTxs txIds mpArgs IS{isTxs, isLastTicketNo} ledgerState =
  (state', tracer)
  where
      -- Filtering is O(n), but this function will rarely be used, as it is an
      -- escape hatch when there's an inconsistency between the ledger and the
      -- mempool.
    txTickets' = filter
            ((`notElem` toRemove) . txId . txTicketTx)
            (TxSeq.toList isTxs)
    (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot ledgerState)
    vr = revalidateTxsFor
            capacityOverride
            cfg
            slot
            ticked
            isLastTicketNo
            txTickets'
    state' = internalStateFromVR vr
    removed = map fst (vrInvalid vr)
    mempoolSize = isMempoolSize state'
    tracer = TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
    MempoolArgs
      { mpArgsLedgerCfg = cfg
      , mpArgsCapacityOverride = capacityOverride
      } = mpArgs
    toRemove = Set.fromList txIds

pureSyncWithLedger
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolArgs blk
  -> InternalState blk
  -> LedgerState blk
  -> (InternalState blk, (MempoolSnapshot blk TicketNo, TraceEventMempool blk))
pureSyncWithLedger mpArgs state ledgerState = (newState, (snapshot, trace))
  where
    vr          = validateIS mpArgs state ledgerState
    newState    = internalStateFromVR vr
    snapshot    = implSnapshotFromIS newState
    removed     = map fst (vrInvalid vr)
    -- The size of the mempool /after/ removing invalid transactions.
    mempoolSize = isMempoolSize newState
    trace       = TraceMempoolRemoveTxs removed mempoolSize

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

implSnapshotFromIS :: HasTxId (GenTx blk)
                   => InternalState blk
                   -> MempoolSnapshot blk TicketNo
implSnapshotFromIS state = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         state
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    state
    , snapshotTxsForSize  = implSnapshotGetTxsForSize  state
    , snapshotLookupTx    = implSnapshotGetTx          state
    , snapshotHasTx       = implSnapshotHasTx          state
    , snapshotMempoolSize = implSnapshotGetMempoolSize state
    , snapshotSlotNo      = isSlotNo                   state
    , snapshotLedgerState = isLedgerState              state
    }

implSnapshotGetTxs :: InternalState blk
                   -> [(GenTx blk, TicketNo)]
implSnapshotGetTxs = flip implSnapshotGetTxsAfter zeroTicketNo

implSnapshotGetTxsAfter :: InternalState blk
                        -> TicketNo
                        -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn =
    TxSeq.toTuples $ snd $ TxSeq.splitAfterTicketNo isTxs tn

implSnapshotGetTxsForSize :: InternalState blk
                          -> Word32
                          -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsForSize IS{isTxs} maxSize =
    TxSeq.toTuples $ fst $ TxSeq.splitAfterTxSize isTxs maxSize

implSnapshotGetTx :: InternalState blk
                  -> TicketNo
                  -> Maybe (GenTx blk)
implSnapshotGetTx IS{isTxs} tn = isTxs `TxSeq.lookupByTicketNo` tn

implSnapshotHasTx :: Ord (GenTxId blk)
                  => InternalState blk
                  -> GenTxId blk
                  -> Bool
implSnapshotHasTx IS{isTxIds} txid = Set.member txid isTxIds

implSnapshotGetMempoolSize :: InternalState blk
                           -> MempoolSize
implSnapshotGetMempoolSize = TxSeq.toMempoolSize . isTxs

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

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: ValidationResult blk -> InternalState blk
internalStateFromVR vr = IS {
      isTxs          = vrValid
    , isTxIds        = vrValidTxIds
    , isLedgerState  = vrAfter
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
validationResultFromIS :: InternalState blk -> ValidationResult blk
validationResultFromIS state = ValidationResult {
      vrBeforeTip      = isTip
    , vrSlotNo         = isSlotNo
    , vrBeforeCapacity = isCapacity
    , vrValid          = isTxs
    , vrValidTxIds     = isTxIds
    , vrNewValid       = Nothing
    , vrAfter          = isLedgerState
    , vrInvalid        = []
    , vrLastTicketNo   = isLastTicketNo
    }
  where
    IS {
        isTxs
      , isTxIds
      , isLedgerState
      , isTip
      , isSlotNo
      , isLastTicketNo
      , isCapacity
      } = state

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
                    -> TxTicket (GenTx blk)
                    -> ValidationResult blk
                    -> ValidationResult blk
extendVRPrevApplied cfg txTicket vr =
    case runExcept (reapplyTx cfg vrSlotNo tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid      = vrValid :> txTicket
                      , vrValidTxIds = Set.insert (txId tx) vrValidTxIds
                      , vrAfter      = st'
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
            -> GenTx blk
            -> (GenTx blk -> TxSizeInBytes)
            -> ValidationResult blk
            -> ValidationResult blk
extendVRNew cfg tx txSize vr = assert (isNothing vrNewValid) $
    case runExcept (applyTx cfg vrSlotNo tx vrAfter) of
      Left err  -> vr { vrInvalid      = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid        = vrValid :> TxTicket tx nextTicketNo (txSize tx)
                      , vrValidTxIds   = Set.insert (txId tx) vrValidTxIds
                      , vrNewValid     = Just tx
                      , vrAfter        = st'
                      , vrLastTicketNo = nextTicketNo
                      }
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

-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS :: forall blk.
              ( LedgerSupportsMempool blk
              , HasTxId (GenTx blk)
              , ValidateEnvelope blk
              )
           => MempoolArgs blk
           -> InternalState blk
           -> LedgerState blk
           -> ValidationResult blk
validateIS mpArgs state ledgerState =
    validateStateFor (mpArgsCapacityOverride mpArgs) (mpArgsLedgerCfg mpArgs)
      (ForgeInUnknownSlot ledgerState)
      state


-- | Given a (valid) internal state, validate it against the given ledger
-- state and 'BlockSlot'.
--
-- When these match the internal state's 'isTip' and 'isSlotNo', this is very
-- cheap, as the given internal state will already be valid against the given
-- inputs.
--
-- When these don't match, the transaction in the internal state will be
-- revalidated ('revalidateTxsFor').
validateStateFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => MempoolCapacityBytesOverride
  -> LedgerConfig     blk
  -> ForgeLedgerState blk
  -> InternalState    blk
  -> ValidationResult blk
validateStateFor capacityOverride cfg blockLedgerState state
    | isTip    == castHash (getTipHash st')
    , isSlotNo == slot
    = validationResultFromIS state
    | otherwise
    = revalidateTxsFor
        capacityOverride
        cfg
        slot
        st'
        isLastTicketNo
        (TxSeq.toList isTxs)
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = state
    (slot, st') = tickLedgerState cfg blockLedgerState

-- | Revalidate the given transactions (@['TxTicket' ('GenTx' blk)]@), which
-- are /all/ the transactions in the Mempool against the given ticked ledger
-- state, which corresponds to the chain's ledger state.
revalidateTxsFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> TickedLedgerState blk
  -> TicketNo
     -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (GenTx blk)]
  -> ValidationResult blk
revalidateTxsFor capacityOverride cfg slot ledgerState lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      (validationResultFromIS state)
  where
    state = initInternalState capacityOverride lastTicketNo slot ledgerState

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk)
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
