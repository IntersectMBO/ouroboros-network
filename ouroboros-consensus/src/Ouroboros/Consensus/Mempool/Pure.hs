{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Mempool.Pure where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Control.Tracer
import           Data.Maybe (isJust, isNothing, listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Data
import           Ouroboros.Consensus.Mempool.TxSeq (MempoolSize, TicketNo,
                     TxSeq (..), TxTicket (..), msNumBytes, zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)


-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

initInternalState
  :: LedgerSupportsMempool blk
  => MempoolCapacityBytesOverride
  -> TicketNo  -- ^ Used for 'isLastTicketNo'
  -> SlotNo
  -> TickedLedgerState blk
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

-- | If no override is provided, calculate the default mempool capacity as 2x
-- the current ledger's maximum transaction capacity of a block.
computeMempoolCapacity
  :: LedgerSupportsMempool blk
  => TickedLedgerState blk
  -> MempoolCapacityBytesOverride
  -> MempoolCapacityBytes
computeMempoolCapacity st = \case
    NoMempoolCapacityBytesOverride        -> noOverride
    MempoolCapacityBytesOverride override -> override
  where
    noOverride = MempoolCapacityBytes (maxTxCapacity st * 2)

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

-- If snd part of result is Just, it is a transaction, that was added or rejected.
-- If it is Nothing their was no space for the transaction
pureTryAddTx
  :: forall m blk. (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> InternalState blk
  -> GenTx blk
  -> ( InternalState blk
     , Maybe (GenTx blk, MempoolAddTxResult blk)
     )

pureTryAddTx MempoolEnv{mpEnvTxSize, mpEnvLedgerCfg} inState transaction
          -- No space in the Mempool.
          |  let transactionSize = mpEnvTxSize transaction
                 curSize = msNumBytes $ isMempoolSize inState
          , curSize + transactionSize > getMempoolCapacityBytes (isCapacity inState)
          = (inState, Nothing)
          | otherwise
          = let vr  = extendVRNew mpEnvLedgerCfg transaction mpEnvTxSize $
                          validationResultFromIS inState
                inState' = internalStateFromVR vr
            in case vrNewValid vr of
              Just _ ->
                -- Each time we have found a valid transaction, we update the
                -- Mempool. This keeps our STM transactions short, avoiding
                -- repeated work.
                --
                -- Note that even if the transaction were invalid, we could
                -- still write the state, because in that case we would have
                -- that @inState == is'@, but there's no reason to do that
                -- additional write.

              -- We only extended the ValidationResult with a single
              -- transaction ('transaction'). So if it's not in 'vrInvalid', it
              -- must be in 'vrNewValid'.
                case listToMaybe (vrInvalid vr) of
                  -- The transaction was valid
                  Nothing       -> undefined
                    -- traceWith mpEnvTracer
                    -- $ TraceMempoolAddedTx
                    --     transaction
                    --     (isMempoolSize is)
                    --     (isMempoolSize is')
                    --     $ ((transaction, MempoolTxAdded):acc) toAdd'
                  Just (a, err) -> undefined
                    --    assert (isNothing (vrNewValid vr))
                    -- $  assert (length (vrInvalid vr) == 1)
                    -- $  traceWith mpEnvTracer
                    -- $  TraceMempoolRejectedTx
                    --       transaction
                    --       err
                    --       (isMempoolSize inState)
                    -- $  ((transaction, MempoolTxRejected err):acc) toAdd'

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

implSnapshotFromIS :: HasTxId (GenTx blk)
                   => InternalState blk
                   -> MempoolSnapshot blk TicketNo
implSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = implSnapshotGetTxs         is
    , snapshotTxsAfter    = implSnapshotGetTxsAfter    is
    , snapshotTxsForSize  = implSnapshotGetTxsForSize  is
    , snapshotLookupTx    = implSnapshotGetTx          is
    , snapshotHasTx       = implSnapshotHasTx          is
    , snapshotMempoolSize = implSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo is
    , snapshotLedgerState = isLedgerState is
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
validateStateFor capacityOverride cfg blockLedgerState is
    | isTip    == castHash (getTipHash st')
    , isSlotNo == slot
    = validationResultFromIS is
    | otherwise
    = revalidateTxsFor
        capacityOverride
        cfg
        slot
        st'
        isLastTicketNo
        (TxSeq.toList isTxs)
  where
    IS { isTxs, isTip, isSlotNo, isLastTicketNo } = is
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
revalidateTxsFor capacityOverride cfg slot st lastTicketNo txTickets =
    repeatedly
      (extendVRPrevApplied cfg)
      txTickets
      (validationResultFromIS is)
  where
    is = initInternalState capacityOverride lastTicketNo slot st

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
