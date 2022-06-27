{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types required for implementing the Mempool.

module Ouroboros.Consensus.Mempool.Impl.Types (
    -- * Internal State
    InternalState (..)
  , initInternalState
  , isMempoolSize
    -- * Validation result
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
  , revalidateTxsFor
  , validateStateFor
    -- * Tick ledger state
  , tickLedgerState
    -- * Conversions
  , internalStateFromVR
  , validationResultFromIS
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Data.Maybe (isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Generics (Generic)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..))
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike

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
    , isLedgerState  :: !(TickedLedgerState blk TrackingMK)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip          :: !(ChainHash blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
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
                  , NoThunks (TickedLedgerState blk TrackingMK)
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
  -> TickedLedgerState blk TrackingMK
  -> InternalState blk
initInternalState capacityOverride lastTicketNo slot st = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isLedgerState  = st
    , isTip          = castHash (getTipHash st)
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity st capacityOverride
    }

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | A ValidationResult is created from an InternalState and is used while
-- revalidating the ledger state or validating transactions, but should never be
-- returned. It merely tracks the result since the last validation and should be
-- used to produce an internal state in the end.
data ValidationResult invalidTx blk = ValidationResult {
      -- | The tip of the chain before applying these transactions
      vrBeforeTip      :: ChainHash blk

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
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
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

-- | Given a (previously valid) internal state, validate it against the given
-- ledger state and slot.
--
-- When these match the internal state's 'isTip' and 'isSlotNo', this is very
-- cheap, as the given internal state will already be valid against the given
-- inputs. However, it is important to note that the new state passed as
-- parameter ('blockLedgerState') potentially includes some values which were
-- not required by the set of transactions that were used on the last mempool
-- revalidation/sync, therefore not being in 'isLedgerState is'. Then the way to
-- get a state with those new values included is to apply the same accumulated
-- differences from 'isLedgerState is' to 'blockLedgerState'.
--
-- When these don't match, the transactions in the internal state will be
-- revalidated ('revalidateTxsFor') on top of the given ledger state.
--
-- The returned ticked ledger state will be used by the block forging logic to
-- determine how many transactions can be put on the new block, hence, the 'mk'
-- is irrelevant.
--
-- PRECONDITION: The differences in the internal state (which consist of
-- transaction differences and possibly ticking differences) must be applicable
-- to the ledger state provided in 'blockLedgerState', this means, that the keys
-- that were used to retrieve the values in 'blockLedgerState' must be a
-- superset of the keys that were used to retrieve the values in 'isLedgerState
-- is' (before ticking and applying the transactions).
validateStateFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => InternalState    blk
  -> LedgerConfig     blk
  -> MempoolCapacityBytesOverride
  -> ForgeLedgerState blk
  -> (TickedLedgerState blk TrackingMK, ValidationResult (Validated (GenTx blk)) blk)
validateStateFor is cfg capacityOverride blockLedgerState
    | isTip    == castHash (getTipHash st')
    , isSlotNo == slot
    = ( -- 'isLedgerState is' is equivalent to st' except in the UTxO set.
        -- Therefore it is safe to return this one, as we don't use the 'mk'.
        isLedgerState is
      , validationResultFromIS
        is { isLedgerState = reapplyTrackingTicked (isLedgerState is) (forgeLedgerState blockLedgerState) })
    | otherwise
    = ( st'
      , revalidateTxsFor
        capacityOverride
        cfg
        slot
        st'
        isLastTicketNo
        (TxSeq.toList isTxs))
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = is
    (slot, st') = tickLedgerState cfg blockLedgerState

-- | Revalidate the given transactions (@['TxTicket' ('GenTx' blk)]@), against
-- the given ticked ledger state.
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
  -> TickedLedgerState blk TrackingMK
  -> TicketNo -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (Validated (GenTx blk))]
  -> ValidationResult (Validated (GenTx blk)) blk
revalidateTxsFor capacityOverride cfg slot st lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      vr
  where
    vr = validationResultFromIS
       $ initInternalState capacityOverride lastTicketNo slot st

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot' or the next slot after
-- the ledger state on top of which we are going to apply the transactions.
tickLedgerState
  :: forall blk. (UpdateLedger blk, ValidateEnvelope blk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk
  -> (SlotNo, TickedLedgerState blk TrackingMK)
tickLedgerState cfg fiks =
  let st   = forgeLedgerState fiks
      slot = case fiks of
        ForgeInKnownSlot s _ -> s
        ForgeInUnknownSlot{} ->
          -- Optimistically assume that the transactions will be included in a block
          -- in the next available slot
          --
          -- TODO: We should use time here instead
          -- <https://github.com/input-output-hk/ouroboros-network/issues/1298>
          -- Once we do, the ValidateEnvelope constraint can go.
          case ledgerTipSlot st of
             Origin      -> minimumPossibleSlotNo (Proxy @blk)
             NotOrigin s -> succ s
  in
    (slot, attachAndApplyDiffsTicked (applyChainTick cfg slot (forgetLedgerTables st)) st)

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: ValidationResult invalidTx blk -> InternalState blk
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
validationResultFromIS :: InternalState blk -> ValidationResult invalidTx blk
validationResultFromIS is = ValidationResult {
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
      } = is
 
