{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Types required for implementing the Mempool.

module Ouroboros.Consensus.Mempool.Impl.Types (
    -- * Internal State
    InternalState (..)
  , initInternalState
  , isLedgerState
  , isMempoolSize
    -- will be removed
  , isMempoolSize
    -- ** MempoolChangelog
  , MempoolChangelog (..)
  , appendLedgerTablesOnMempoolChangelog
  , forwardTableKeySetsOnMempool
  , getTransactionKeySets
  , rewindTableKeySetsOnMempool
    -- * Validation result
  , ValidationResult (..)
  , extendVRNew
  , extendVRPrevApplied
  , revalidateTxsFor
  , validateIS
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
import           Ouroboros.Consensus.Storage.LedgerDB.HD (extendSeqUtxoDiff)
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (DbChangelog,
                     RewoundTableKeySets, UnforwardedReadSets,
                     forwardTableKeySets, rewindTableKeySets)
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Singletons (SingI)

{-------------------------------------------------------------------------------
  Internal State
-------------------------------------------------------------------------------}

data MempoolChangelog blk = MempoolChangelog {
    mcChangelog   :: !(DbChangelog (LedgerState blk))
  , mcLedgerState :: !(TickedLedgerState blk ValuesMK)
  , mcDiffs       :: !(LedgerTables (LedgerState blk) DiffMK)
  -- TODO store slot number here the moment MempoolChangelog is initialized
  } deriving (Generic)

deriving instance ( NoThunks (LedgerTables (LedgerState blk) DiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) SeqDiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) EmptyMK)
                  , NoThunks (LedgerState blk EmptyMK)
                  , NoThunks (TickedLedgerState blk ValuesMK)
                  ) => NoThunks (MempoolChangelog blk)

withTransactionDiffs ::
     ( TableStuff (LedgerState blk)
     , GetTip (TickedLedgerState blk ValuesMK)
     )
  => MempoolChangelog blk
  -> DbChangelog (LedgerState blk)
withTransactionDiffs MempoolChangelog {mcChangelog, mcLedgerState, mcDiffs} =
  let
    diffs = changelogDiffs mcChangelog
    slotNo = case getTipSlot mcLedgerState of
      Origin       -> error "this should not happen because ledger is ticked"
      NotOrigin sn -> sn
    appendDiffs ::
         Ord k
      => ApplyMapKind 'SeqDiffMK k v
      -> ApplyMapKind 'DiffMK k v
      -> ApplyMapKind 'SeqDiffMK k v
    appendDiffs (ApplySeqDiffMK diffSeq) (ApplyDiffMK diff) =
      ApplySeqDiffMK $ extendSeqUtxoDiff diffSeq slotNo diff
   in mcChangelog {
      changelogDiffs = zipLedgerTables appendDiffs diffs mcDiffs
    }

rewindTableKeySetsOnMempool ::
     ( TableStuff (LedgerState blk)
     , GetTip (TickedLedgerState blk ValuesMK)
     )
  => MempoolChangelog blk
  -> TableKeySets (LedgerState blk)
  -> RewoundTableKeySets (LedgerState blk)
rewindTableKeySetsOnMempool mempoolChangelog tks =
  withTransactionDiffs mempoolChangelog `rewindTableKeySets` tks

forwardTableKeySetsOnMempool ::
     ( TableStuff (LedgerState blk)
     , GetTip (TickedLedgerState blk ValuesMK)
     )
  => MempoolChangelog blk
  -> UnforwardedReadSets (LedgerState blk)
  -> Either (WithOrigin SlotNo, WithOrigin SlotNo)
            (TableReadSets (LedgerState blk))
forwardTableKeySetsOnMempool mempoolChangelog urs =
  withTransactionDiffs mempoolChangelog `forwardTableKeySets` urs

appendLedgerTablesOnMempoolChangelog ::
     MempoolChangelog blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> MempoolChangelog blk
appendLedgerTablesOnMempoolChangelog mempoolChangelog lt =
  mempoolChangelog {
    mcLedgerState = mcLedgerState mempoolChangelog `appendLedgerTablesTicked` lt
  }

-- | The cached ledger state after applying the transactions in the
-- Mempool against the chain's ledger state. New transactions will be
-- validated against this ledger.
--
-- INVARIANT: 'isLedgerState' is the ledger resulting from applying the
-- transactions in 'isTxs' against the ledger identified 'isTip' as tip.
isLedgerState  :: InternalState blk -> TickedLedgerState blk ValuesMK
isLedgerState = mcLedgerState . isMempoolChangelog

--------------------------------------------------------------------------------

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
      isTxs              :: !(TxSeq (Validated (GenTx blk)))

      -- | The cached IDs of transactions currently in the mempool.
      --
      -- This allows one to more quickly lookup transactions by ID from a
      -- 'MempoolSnapshot' (see 'snapshotHasTx').
      --
      -- This should always be in-sync with the transactions in 'isTxs'.
    , isTxIds            :: !(Set (GenTxId blk))

    , isMempoolChangelog :: !(MempoolChangelog blk)

      -- | The tip of the chain that 'isTxs' was validated against
      --
      -- This comes from the underlying ledger state ('tickedLedgerState')
    , isTip              :: !(ChainHash blk)

      -- | The most recent 'SlotNo' that 'isTxs' was validated against
      --
      -- This comes from 'applyChainTick' ('tickedSlotNo').
    , isSlotNo           :: !SlotNo

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo     :: !TicketNo

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
    , isCapacity         :: !MempoolCapacityBytes
    }
  deriving (Generic)

deriving instance ( NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (TickedLedgerState blk ValuesMK)
                  , NoThunks (LedgerTables (LedgerState blk) DiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) SeqDiffMK)
                  , NoThunks (LedgerTables (LedgerState blk) EmptyMK)
                  , NoThunks (LedgerState blk EmptyMK)
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
  -> MempoolChangelog blk
  -> InternalState blk
initInternalState capacityOverride lastTicketNo slot memChangelog = IS {
      isTxs          = TxSeq.Empty
    , isTxIds        = Set.empty
    , isMempoolChangelog = memChangelog
    , isTip          = castHash (getTipHash st)
    , isSlotNo       = slot
    , isLastTicketNo = lastTicketNo
    , isCapacity     = computeMempoolCapacity st capacityOverride
    }
  where
    st = mcLedgerState memChangelog -- MTODO

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult invalidTx blk = ValidationResult {
      -- | The tip of the chain before applying these transactions
      vrBeforeTip        :: ChainHash blk

      -- | The slot number of the (imaginary) block the txs will be placed in
    , vrSlotNo           :: SlotNo

      -- | Capacity of the Mempool. Corresponds to 'vrBeforeTip' and
      -- 'vrBeforeSlotNo', /not/ 'vrAfter'.
    , vrBeforeCapacity   :: MempoolCapacityBytes

      -- | The transactions that were found to be valid (oldest to newest)
    , vrValid            :: TxSeq (Validated (GenTx blk))

      -- | The cached IDs of transactions that were found to be valid (oldest to
      -- newest)
    , vrValidTxIds       :: Set (GenTxId blk)

      -- | A new transaction (not previously known) which was found to be valid.
      --
      -- n.b. This will only contain a valid transaction that was /newly/ added
      -- to the mempool (not a previously known valid transaction).
    , vrNewValid         :: Maybe (Validated (GenTx blk))

      -- | TODO: add comment
    , vrMempoolChangelog :: !(MempoolChangelog blk)

      -- | The transactions that were invalid, along with their errors
      --
      -- From oldest to newest.
    , vrInvalid          :: [(invalidTx, ApplyTxErr blk)]

      -- | The mempool 'TicketNo' counter.
      --
      -- When validating new transactions, this should be incremented, starting
      -- from 'isLastTicketNo' of the 'InternalState'.
      -- When validating previously applied transactions, this field should not
      -- be affected.
    , vrLastTicketNo     :: TicketNo
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
    case runExcept (reapplyTx cfg vrSlotNo tx vrLedgerState) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid      = vrValid :> txTicket
                      , vrValidTxIds = Set.insert (txId (txForgetValidated tx)) vrValidTxIds
                      , vrMempoolChangelog = extendMempoolChangelog st'
                      }
  where
    TxTicket { txTicketTx = tx } = txTicket
    ValidationResult { vrValid, vrSlotNo, vrValidTxIds, vrMempoolChangelog, vrInvalid } = vr
    vrLedgerState = mcLedgerState vrMempoolChangelog
    extendMempoolChangelog = undefined -- MTODO

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
    case runExcept (applyTx cfg wti vrSlotNo tx vrAfter) of
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
             , vrMempoolChangelog = extendMempoolChangelog st'
             , vrLastTicketNo = nextTicketNo
             }
        )
  where
    ValidationResult {
        vrValid
      , vrValidTxIds
      , vrMempoolChangelog
      , vrInvalid
      , vrLastTicketNo
      , vrNewValid
      , vrSlotNo
      } = vr

    nextTicketNo = succ vrLastTicketNo
    extendMempoolChangelog = undefined -- MTODO implement this
    vrAfter = mcLedgerState vrMempoolChangelog

-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk), ValidateEnvelope blk)
  => InternalState blk
  -> LedgerState blk ValuesMK
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> ValidationResult (Validated (GenTx blk)) blk
validateIS istate lstate lconfig capacityOverride =
    validateStateFor capacityOverride lconfig (ForgeInUnknownSlot lstate) istate

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
  -> ForgeLedgerState blk ValuesMK
  -> InternalState    blk
  -> ValidationResult (Validated (GenTx blk)) blk
validateStateFor capacityOverride cfg blockLedgerState is
    | isTip    == castHash (getTipHash st')
    , isSlotNo == slot
    = validationResultFromIS is
    | otherwise
    = revalidateTxsFor
        capacityOverride
        cfg
        slot
        (newMemChangelog st')
        isLastTicketNo
        (TxSeq.toList isTxs)
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = is
    (slot, st') = tickLedgerState cfg blockLedgerState
    newMemChangelog = undefined -- MTODO

-- | Revalidate the given transactions (@['TxTicket' ('GenTx' blk)]@), which
-- are /all/ the transactions in the Mempool against the given ticked ledger
-- state, which corresponds to the chain's ledger state.
revalidateTxsFor
  :: (LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> SlotNo
  -> MempoolChangelog blk
  -> TicketNo
     -- ^ 'isLastTicketNo' & 'vrLastTicketNo'
  -> [TxTicket (Validated (GenTx blk))]
  -> ValidationResult (Validated (GenTx blk)) blk
revalidateTxsFor capacityOverride cfg slot memChangelog lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      (validationResultFromIS is)
  where
    is = initInternalState capacityOverride lastTicketNo slot memChangelog

{-------------------------------------------------------------------------------
  Ticking the ledger state
-------------------------------------------------------------------------------}

-- | Tick the 'LedgerState' using the given 'BlockSlot'.
tickLedgerState
  :: forall blk mk. (UpdateLedger blk, ValidateEnvelope blk, SingI mk)
  => LedgerConfig     blk
  -> ForgeLedgerState blk mk
  -> (SlotNo, TickedLedgerState blk mk)
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
  Conversions
-------------------------------------------------------------------------------}

-- | Construct internal state from 'ValidationResult'
--
-- Discards information about invalid and newly valid transactions
internalStateFromVR :: ValidationResult invalidTx blk -> InternalState blk
internalStateFromVR vr = IS {
      isTxs          = vrValid
    , isTxIds        = vrValidTxIds
    , isMempoolChangelog = vrMempoolChangelog
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
      , vrMempoolChangelog
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
    , vrMempoolChangelog = isMempoolChangelog
    , vrInvalid        = []
    , vrLastTicketNo   = isLastTicketNo
    }
  where
    IS {
        isTxs
      , isTxIds
      , isMempoolChangelog
      , isTip
      , isSlotNo
      , isLastTicketNo
      , isCapacity
      } = is
