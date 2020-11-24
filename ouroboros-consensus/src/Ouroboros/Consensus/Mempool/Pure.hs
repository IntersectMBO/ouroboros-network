{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.Mempool.Pure (
    pureRemoveTxs
  , pureSyncWithLedger
  , updatedSnapshot
  , validateIS
  , isMempoolSize
  , initInternalState
  , computeMempoolCapacity
  , extendVRNew
  , internalStateFromVR
  , validationResultFromIS
  , pureTryAddTxsAtomically
  , pureSnapshotFromIS
  , pureSnapshotGetTxs
  , pureSnapshotGetTxsAfter
  , pureSnapshotGetTxsForSize
  , pureSnapshotGetTx
  , pureSnapshotHasTx
  , pureSnapshotGetMempoolSize
  , extendVRPrevApplied
  , validateStateFor
  , revalidateTxsFor
  , tickLedgerState
  , isMempoolTxAdded
  , isMempoolTxRejected
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)

import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSizeInBytes)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.Data
import           Ouroboros.Consensus.Mempool.TxSeq (MempoolSize, TicketNo,
                     TxSeq ((:>)), TxTicket (..), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

pureTryAddTxsAtomically
  :: (LedgerSupportsMempool blk
  , HasTxId (GenTx blk))
  => [GenTx blk]
  -> MempoolEnv m blk
  -> InternalState blk
  -> LedgerState blk
  -> (InternalState blk,
       ( [(GenTx blk, MempoolAddTxResult blk)]
           -- Transactions that were added or rejected. A prefix of the input
           -- list.
       , [GenTx blk]
           -- Transactions that have not yet been added because the capacity
           -- of the Mempool has been reached. A suffix of the input list.
       , [TraceEventMempool blk]
     ))

pureTryAddTxsAtomically txs mpEnv iState _ =
  let (txv, txu, traces, iStateC) = foldl go ([],[],[],iState) txs
  in  (iStateC, (reverse txv, reverse txu, reverse traces))
  where
    MempoolEnv
      { mpEnvLedgerCfg
      , mpEnvTxSize
      } = mpEnv
    accumulatorOutOfCapacity (_, txu, _, _) =
      not (null txu)
    accumulatorAddOutOfCapacity tx (txv, txu, traces, iStateC) =
      (txv, tx:txu, traces, iStateC)
    go acc@(txv, txu, traces, iStateC) tx
      | accumulatorOutOfCapacity acc
      = accumulatorAddOutOfCapacity tx acc
      | -- No space in the Mempool.
        let txSize = mpEnvTxSize tx
            curSize = TxSeq.msNumBytes $ isMempoolSize iStateC
      , curSize + txSize > getMempoolCapacityBytes (isCapacity iStateC)
      = accumulatorAddOutOfCapacity tx acc
      | otherwise
      = let vr  = extendVRNew mpEnvLedgerCfg tx mpEnvTxSize
                      $ validationResultFromIS iStateC
            iStateC' = internalStateFromVR vr
        in case listToMaybe (vrInvalid vr) of
            Nothing ->
              -- We only extended the ValidationResult with a single
              -- transaction ('tx'). So if it's not in 'vrInvalid', it
              -- must be in 'vrNewValid'.
                let trace = TraceMempoolAddedTx
                              tx
                              (isMempoolSize iStateC)
                              (isMempoolSize iStateC')
                in  ((tx, MempoolTxAdded) : txv, txu, trace : traces, iStateC')
            Just (_, err) ->
                assert (isNothing (vrNewValid vr))  $
                assert (length (vrInvalid vr) == 1) $
                let trace = TraceMempoolRejectedTx
                              tx
                              err
                              (isMempoolSize iStateC)
                in ((tx, MempoolTxRejected err) : txv, txu, trace : traces, iStateC')


pureRemoveTxs
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => [GenTxId blk]
  -> MempoolEnv m blk
  -> InternalState blk
  -> LedgerState blk
  -> (InternalState blk, TraceEventMempool blk)
pureRemoveTxs txIds mpEnv IS{isTxs, isLastTicketNo} ledgerState  =
      -- Filtering is O(n), but this function will rarely be used, as it is an
      -- escape hatch when there's an inconsistency between the ledger and the
      -- mempool.
      let txTickets' = filter
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
          is' = internalStateFromVR vr
          removed = map fst (vrInvalid vr)
          mempoolSize = isMempoolSize is'
          tracer = TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
      in (is', tracer)
  where
    MempoolEnv
      { mpEnvLedgerCfg = cfg
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

    toRemove = Set.fromList txIds

pureSyncWithLedger
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> InternalState blk
  -> LedgerState blk
  -> (InternalState blk, (MempoolSnapshot blk TicketNo, TraceEventMempool blk))
pureSyncWithLedger mpEnv state ledgerState  =
  let vr = validateIS mpEnv state ledgerState
      newState    = internalStateFromVR vr
      snapshot    = pureSnapshotFromIS newState
      removed     = map fst (vrInvalid vr)
      -- The size of the mempool /after/ removing invalid transactions.
      mempoolSize = isMempoolSize newState
      trace       = TraceMempoolRemoveTxs removed mempoolSize
  in (newState, (snapshot, trace))

updatedSnapshot ::
     forall m blk.
     (LedgerSupportsMempool blk, ValidateEnvelope blk, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> ForgeLedgerState blk
  -> InternalState blk
  -> MempoolSnapshot blk TicketNo
updatedSnapshot mpEnv blockLedgerState =
      pureSnapshotFromIS
    . internalStateFromVR
    . validateStateFor capacityOverride cfg blockLedgerState
  where
    MempoolEnv
      { mpEnvLedgerCfg = cfg
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS :: forall m blk.
              ( LedgerSupportsMempool blk
              , HasTxId (GenTx blk)
              , ValidateEnvelope blk
              )
           => MempoolEnv m blk
           -> InternalState blk
           -> LedgerState blk
           -> ValidationResult blk
validateIS mpEnv state ledgerState =
    validateStateFor (mpEnvCapacityOverride mpEnv) (mpEnvLedgerCfg mpEnv)
      (ForgeInUnknownSlot ledgerState)
      state

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

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

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

pureSnapshotFromIS :: HasTxId (GenTx blk)
                   => InternalState blk
                   -> MempoolSnapshot blk TicketNo
pureSnapshotFromIS is = MempoolSnapshot {
      snapshotTxs         = pureSnapshotGetTxs         is
    , snapshotTxsAfter    = pureSnapshotGetTxsAfter    is
    , snapshotTxsForSize  = pureSnapshotGetTxsForSize  is
    , snapshotLookupTx    = pureSnapshotGetTx          is
    , snapshotHasTx       = pureSnapshotHasTx          is
    , snapshotMempoolSize = pureSnapshotGetMempoolSize is
    , snapshotSlotNo      = isSlotNo is
    , snapshotLedgerState = isLedgerState is
    }

pureSnapshotGetTxs :: InternalState blk
                   -> [(GenTx blk, TicketNo)]
pureSnapshotGetTxs = flip pureSnapshotGetTxsAfter zeroTicketNo

pureSnapshotGetTxsAfter :: InternalState blk
                        -> TicketNo
                        -> [(GenTx blk, TicketNo)]
pureSnapshotGetTxsAfter IS{isTxs} tn =
    TxSeq.toTuples $ snd $ TxSeq.splitAfterTicketNo isTxs tn

pureSnapshotGetTxsForSize :: InternalState blk
                          -> Word32
                          -> [(GenTx blk, TicketNo)]
pureSnapshotGetTxsForSize IS{isTxs} maxSize =
    TxSeq.toTuples $ fst $ TxSeq.splitAfterTxSize isTxs maxSize

pureSnapshotGetTx :: InternalState blk
                  -> TicketNo
                  -> Maybe (GenTx blk)
pureSnapshotGetTx IS{isTxs} tn = isTxs `TxSeq.lookupByTicketNo` tn

pureSnapshotHasTx :: Ord (GenTxId blk)
                  => InternalState blk
                  -> GenTxId blk
                  -> Bool
pureSnapshotHasTx IS{isTxIds} txid = Set.member txid isTxIds

pureSnapshotGetMempoolSize :: InternalState blk
                           -> MempoolSize
pureSnapshotGetMempoolSize = TxSeq.toMempoolSize . isTxs

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

isMempoolTxAdded :: MempoolAddTxResult blk -> Bool
isMempoolTxAdded MempoolTxAdded = True
isMempoolTxAdded _              = False

isMempoolTxRejected :: MempoolAddTxResult blk -> Bool
isMempoolTxRejected (MempoolTxRejected _) = True
isMempoolTxRejected _                     = False
