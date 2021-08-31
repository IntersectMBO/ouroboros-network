{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Mempool.Impl (
    LedgerInterface (..)
  , MempoolCapacityBytesOverride (..)
  , TicketNo
  , chainDBLedgerInterface
  , openMempool
    -- * For testing purposes
  , openMempoolWithoutSyncThread
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Data.Maybe (isJust, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word (Word32)
import           GHC.Generics (Generic)

import           Control.Tracer

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)

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

openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg capacityOverride tracer txSize = do
    env <- initMempoolEnv ledger cfg capacityOverride tracer txSize
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize

mkMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { tryAddTxs      = implTryAddTxs      env
    , removeTxs      = implRemoveTxs      env
    , syncWithLedger = implSyncWithLedger env
    , getSnapshot    = implGetSnapshot    env
    , getSnapshotFor = implGetSnapshotFor env
    , getCapacity    = implGetCapacity    env
    , getTxSize      = mpEnvTxSize        env
    , zeroIdx        = zeroTicketNo
    }

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     (IOLike m, IsLedger (LedgerState blk))
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
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

deriving instance ( NoThunks (Validated (GenTx blk))
                  , NoThunks (GenTxId blk)
                  , NoThunks (Ticked (LedgerState blk))
                  , StandardHash blk
                  , Typeable blk
                  ) => NoThunks (InternalState blk)

-- | \( O(1) \). Return the number of transactions in the internal state of
-- the Mempool paired with their total size in bytes.
isMempoolSize :: InternalState blk -> MempoolSize
isMempoolSize = TxSeq.toMempoolSize . isTxs

data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger           :: LedgerInterface m blk
    , mpEnvLedgerCfg        :: LedgerConfig blk
    , mpEnvStateVar         :: StrictTVar m (InternalState blk)
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvTxSize           :: GenTx blk -> TxSizeInBytes
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
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

initMempoolEnv :: ( IOLike m
                  , NoThunks (GenTxId blk)
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
    isVar <- newTVarIO $ initInternalState capacityOverride zeroTicketNo slot st'
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      , mpEnvTxSize           = txSize
      , mpEnvCapacityOverride = capacityOverride
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

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (
                                   IOLike m
                                 , LedgerSupportsMempool blk
                                 , HasTxId (GenTx blk)
                                 , ValidateEnvelope blk
                                 )
                              => ResourceRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv =
    void $ forkLinkedWatcher
      registry
      "Mempool.syncStateOnTipPointChange"
      Watcher {
          wFingerprint = id
        , wInitial     = Nothing
        , wNotify      = action
        , wReader      = getCurrentTip
        }
  where
    action :: Point blk -> m ()
    action _tipPoint = void $ implSyncWithLedger menv

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip =
          ledgerTipPoint (Proxy @blk)
      <$> getCurrentLedgerState (mpEnvLedger menv)

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

-- | Add a bunch of transactions (oldest to newest)
--
-- This function returns two lists: the transactions that were added or
-- rejected, and the transactions that could not yet be added, because the
-- Mempool capacity was reached. See 'addTxs' for a function that blocks in
-- case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
--
-- POSTCONDITON:
-- > (processed, toProcess) <- implTryAddTxs mpEnv txs
-- > map fst processed ++ toProcess == txs
implTryAddTxs
  :: forall m blk. (IOLike m, LedgerSupportsMempool blk, HasTxId (GenTx blk))
  => MempoolEnv m blk
  -> [GenTx blk]
  -> m ( [MempoolAddTxResult blk]
         -- Transactions that were added or rejected. The carried transactions
         -- constitute a prefix of the input list.
       , [GenTx blk]
         -- Transactions that have not yet been added because the capacity
         -- of the Mempool has been reached. A suffix of the input list.
       )
implTryAddTxs mpEnv = go []
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvLedgerCfg = cfg
      , mpEnvTracer
      , mpEnvTxSize
      } = mpEnv

    done acc toAdd = return (reverse acc, toAdd)

    go acc []                     = done acc []
    go acc toAdd@(firstTx:toAdd') =
        -- Note: we execute the continuation returned by 'atomically'
        join $ atomically $ readTVar mpEnvStateVar >>= tryAdd
      where
        tryAdd is
          -- No space in the Mempool.
          | let firstTxSize = mpEnvTxSize firstTx
                curSize = msNumBytes $ isMempoolSize is
          , curSize + firstTxSize > getMempoolCapacityBytes (isCapacity is)
          = return $ done acc toAdd

          | otherwise
          = do
              let (eVtx, vr) = extendVRNew cfg firstTx mpEnvTxSize $
                                validationResultFromIS is
                  is' = internalStateFromVR vr
              unless (null (vrNewValid vr)) $
                -- Each time we have found a valid transaction, we update the
                -- Mempool. This keeps our STM transactions short, avoiding
                -- repeated work.
                --
                -- Note that even if the transaction were invalid, we could
                -- still write the state, because in that case we would have
                -- that @is == is'@, but there's no reason to do that
                -- additional write.
                writeTVar mpEnvStateVar is'

              return $ case eVtx of
                -- The transaction was valid
                Right vtx ->
                  assert (isJust (vrNewValid vr)) $ do
                    traceWith mpEnvTracer $ TraceMempoolAddedTx
                      vtx
                      (isMempoolSize is)
                      (isMempoolSize is')
                    go ((MempoolTxAdded vtx):acc) toAdd'
                -- The transaction was invalid
                Left err ->
                  assert (isNothing (vrNewValid vr))  $
                  assert (length (vrInvalid vr) == 1) $ do
                    traceWith mpEnvTracer $ TraceMempoolRejectedTx
                      firstTx
                      err
                      (isMempoolSize is)
                    go
                      ((MempoolTxRejected firstTx err):acc)
                      toAdd'

implRemoveTxs
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk
  -> [GenTxId blk]
  -> m ()
implRemoveTxs mpEnv txIds = do
    (removed, mempoolSize) <- atomically $ do
      IS { isTxs, isLastTicketNo } <- readTVar mpEnvStateVar
      st <- getCurrentLedgerState mpEnvLedger
      -- Filtering is O(n), but this function will rarely be used, as it is an
      -- escape hatch when there's an inconsistency between the ledger and the
      -- mempool.
      let txTickets' = filter
              ((`notElem` toRemove) . txId . txForgetValidated . txTicketTx)
              (TxSeq.toList isTxs)
          (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot st)
          vr = revalidateTxsFor
            capacityOverride
            cfg
            slot
            ticked
            isLastTicketNo
            txTickets'
          is' = internalStateFromVR vr
      writeTVar mpEnvStateVar is'
      return (map fst (vrInvalid vr), isMempoolSize is')

    unless (null txIds) $
      traceWith mpEnvTracer $
        TraceMempoolManuallyRemovedTxs txIds removed mempoolSize
  where
    MempoolEnv
      { mpEnvLedgerCfg = cfg
      , mpEnvLedger
      , mpEnvTracer
      , mpEnvStateVar
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

    toRemove = Set.fromList txIds

implSyncWithLedger :: ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
                   => MempoolEnv m blk -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} = do
    (removed, mempoolSize, snapshot) <- atomically $ do
      vr <- validateIS mpEnv
      writeTVar mpEnvStateVar (internalStateFromVR vr)
      -- The size of the mempool /after/ removing invalid transactions.
      mempoolSize <- getMempoolSize mpEnv
      snapshot    <- implGetSnapshot mpEnv
      return (map fst (vrInvalid vr), mempoolSize, snapshot)
    unless (null removed) $
      traceWith mpEnvTracer $ TraceMempoolRemoveTxs removed mempoolSize
    return snapshot

implGetSnapshot :: (IOLike m, HasTxId (GenTx blk))
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} =
    implSnapshotFromIS <$> readTVar mpEnvStateVar

implGetSnapshotFor :: forall m blk.
                      ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
                   => MempoolEnv m blk
                   -> ForgeLedgerState blk
                   -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshotFor mpEnv blockLedgerState =
    updatedSnapshot <$> readTVar mpEnvStateVar
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvLedgerCfg
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

    updatedSnapshot :: InternalState blk -> MempoolSnapshot blk TicketNo
    updatedSnapshot =
          implSnapshotFromIS
        . internalStateFromVR
        . validateStateFor capacityOverride mpEnvLedgerCfg blockLedgerState

-- | \( O(1) \). Return the cached value of the current capacity of the
-- mempool in bytes.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity MempoolEnv{mpEnvStateVar} =
    isCapacity <$> readTVar mpEnvStateVar

-- | \( O(1) \). Return the number of transactions in the Mempool paired with
-- their total size in bytes.
getMempoolSize :: IOLike m
               => MempoolEnv m blk
               -> STM m MempoolSize
getMempoolSize MempoolEnv{mpEnvStateVar} =
    isMempoolSize <$> readTVar mpEnvStateVar

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
                   -> [(Validated (GenTx blk), TicketNo)]
implSnapshotGetTxs = (flip implSnapshotGetTxsAfter) zeroTicketNo

implSnapshotGetTxsAfter :: InternalState blk
                        -> TicketNo
                        -> [(Validated (GenTx blk), TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn =
    TxSeq.toTuples $ snd $ TxSeq.splitAfterTicketNo isTxs tn

implSnapshotGetTxsForSize :: InternalState blk
                          -> Word32
                          -> [(Validated (GenTx blk), TicketNo)]
implSnapshotGetTxsForSize IS{isTxs} maxSize =
    TxSeq.toTuples $ fst $ TxSeq.splitAfterTxSize isTxs maxSize

implSnapshotGetTx :: InternalState blk
                  -> TicketNo
                  -> Maybe (Validated (GenTx blk))
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
    -- state identifeid by 'vrBeforeTip'.
  , vrAfter          :: TickedLedgerState blk

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
    case runExcept (reapplyTx cfg vrSlotNo tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid      = vrValid :> txTicket
                      , vrValidTxIds = Set.insert (txId (txForgetValidated tx)) vrValidTxIds
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
            -> ValidationResult (GenTx blk) blk
            -> ( Either (ApplyTxErr blk) (Validated (GenTx blk))
               , ValidationResult (GenTx blk) blk
               )
extendVRNew cfg tx txSize vr = assert (isNothing vrNewValid) $
    case runExcept (applyTx cfg vrSlotNo tx vrAfter) of
      Left err         ->
          ( Left err
          , vr { vrInvalid = (tx, err) : vrInvalid
               }
          )
      Right (st', vtx) ->
          ( Right vtx
          , vr { vrValid        = vrValid :> TxTicket vtx nextTicketNo (txSize tx)
               , vrValidTxIds   = Set.insert (txId tx) vrValidTxIds
               , vrNewValid     = Just vtx
               , vrAfter        = st'
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

-- | Validate the internal state against the current ledger state and the
-- given 'BlockSlot', revalidating if necessary.
validateIS :: forall m blk.
              ( IOLike m
              , LedgerSupportsMempool blk
              , HasTxId (GenTx blk)
              , ValidateEnvelope blk
              )
           => MempoolEnv m blk
           -> STM m (ValidationResult (Validated (GenTx blk)) blk)
validateIS mpEnv =
    validateStateFor capacityOverride mpEnvLedgerCfg
      <$> (ForgeInUnknownSlot <$> getCurrentLedgerState mpEnvLedger)
      <*> readTVar mpEnvStateVar
  where
    MempoolEnv {
        mpEnvLedger
      , mpEnvLedgerCfg
      , mpEnvStateVar
      , mpEnvCapacityOverride = capacityOverride
      } = mpEnv

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
  -> [TxTicket (Validated (GenTx blk))]
  -> ValidationResult (Validated (GenTx blk)) blk
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
