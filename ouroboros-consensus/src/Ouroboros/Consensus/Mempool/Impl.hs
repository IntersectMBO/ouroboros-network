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
    openMempool
  , MempoolCapacityBytesOverride (..)
  , LedgerInterface (..)
  , chainDBLedgerInterface
  , TicketNo
    -- * For testing purposes
  , openMempoolWithoutSyncThread
  ) where

import           Control.Exception (assert)
import           Control.Monad.Except
import           Data.Maybe (isJust, isNothing, listToMaybe)
import           Data.Typeable

import           Control.Tracer

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.ImplPure
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, zeroTicketNo)
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}
-- | Abstract interface needed to run a Mempool.
newtype LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: IOLike m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

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
    , getTxSize      = mpArgsTxSize       (mpEnvArgs env)
    , zeroIdx        = zeroTicketNo
    }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

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
    let args = MempoolArgs cfg txSize capacityOverride
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvArgs             = args
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      }

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
    void $ onEachChange
      registry
      "Mempool.syncStateOnTipPointChange"
      id
      Nothing
      getCurrentTip
      action
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

data MempoolEnv m blk = MempoolEnv {
      mpEnvArgs     :: MempoolArgs blk
    , mpEnvLedger   :: LedgerInterface m blk
    , mpEnvStateVar :: StrictTVar m (InternalState blk)
    , mpEnvTracer   :: Tracer m (TraceEventMempool blk)
    }

-- Read the internal state and the ledger state, calls the function with it and writes the state
-- with the first element of the result of the function and returns the second
-- part of the result of the function as result
atomicallyDoWithState :: IOLike m
  => MempoolEnv m blk
  -> (MempoolArgs blk -> InternalState blk -> LedgerState blk -> (InternalState blk, res))
  -> m res
atomicallyDoWithState mpEnv func =
  atomically $ do
    state <- readTVar (mpEnvStateVar mpEnv)
    ledgerState <- getCurrentLedgerState (mpEnvLedger mpEnv)
    let (state', res) = func (mpEnvArgs mpEnv) state ledgerState
    writeTVar (mpEnvStateVar mpEnv) state'
    pure res

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
  -> m ( [(GenTx blk, MempoolAddTxResult blk)]
         -- Transactions that were added or rejected. A prefix of the input
         -- list.
       , [GenTx blk]
         -- Transactions that have not yet been added because the capacity
         -- of the Mempool has been reached. A suffix of the input list.
       )
implTryAddTxs mpEnv = go []
  where
    MempoolEnv
      { mpEnvStateVar
      , mpEnvArgs = mpArgs
      , mpEnvTracer
      } = mpEnv

    done acc toAdd = return (reverse acc, toAdd)

    go acc []                     = done acc []
    go acc toAdd@(firstTx:toAdd') =
        -- Note: we execute the continuation returned by 'atomically'
        join $ atomically $ readTVar mpEnvStateVar >>= tryAdd
      where
        tryAdd is
          -- No space in the Mempool.
          | let firstTxSize = mpArgsTxSize mpArgs firstTx
                curSize = msNumBytes $ isMempoolSize is
          , curSize + firstTxSize > getMempoolCapacityBytes (isCapacity is)
          = return $ done acc toAdd

          | otherwise
          = do
              let vr  = extendVRNew (mpArgsLedgerCfg mpArgs) firstTx (mpArgsTxSize mpArgs) $
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

              -- We only extended the ValidationResult with a single
              -- transaction ('firstTx'). So if it's not in 'vrInvalid', it
              -- must be in 'vrNewValid'.
              return $ case listToMaybe (vrInvalid vr) of
                -- The transaction was valid
                Nothing ->
                  assert (isJust (vrNewValid vr)) $ do
                    traceWith mpEnvTracer $ TraceMempoolAddedTx
                      firstTx
                      (isMempoolSize is)
                      (isMempoolSize is')
                    go ((firstTx, MempoolTxAdded):acc) toAdd'
                Just (_, err) ->
                  assert (isNothing (vrNewValid vr))  $
                  assert (length (vrInvalid vr) == 1) $ do
                    traceWith mpEnvTracer $ TraceMempoolRejectedTx
                      firstTx
                      err
                      (isMempoolSize is)
                    go
                      ((firstTx, MempoolTxRejected err):acc)
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
  tm <- atomicallyDoWithState mpEnv (pureRemoveTxs txIds)
  unless (null txIds) $ traceWith (mpEnvTracer mpEnv) tm

implSyncWithLedger :: ( IOLike m
                      , LedgerSupportsMempool blk
                      , HasTxId (GenTx blk)
                      , ValidateEnvelope blk
                      )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv = do
  (snapshot, tm) <- atomicallyDoWithState mpEnv pureSyncWithLedger
  traceWith (mpEnvTracer mpEnv) tm
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
      , mpEnvArgs
      } = mpEnv

    updatedSnapshot :: InternalState blk -> MempoolSnapshot blk TicketNo
    updatedSnapshot =
          implSnapshotFromIS
        . internalStateFromVR
        . validateStateFor
            (mpArgsCapacityOverride mpEnvArgs)
            (mpArgsLedgerCfg mpEnvArgs)
            blockLedgerState

-- | \( O(1) \). Return the cached value of the current capacity of the
-- mempool in bytes.
implGetCapacity :: IOLike m => MempoolEnv m blk -> STM m MempoolCapacityBytes
implGetCapacity MempoolEnv{mpEnvStateVar} =
    isCapacity <$> readTVar mpEnvStateVar

-- | \( O(1) \). Return the number of transactions in the Mempool paired with
-- their total size in bytes.
_getMempoolSize :: IOLike m
               => MempoolEnv m blk
               -> STM m MempoolSize
_getMempoolSize MempoolEnv{mpEnvStateVar} =
    isMempoolSize <$> readTVar mpEnvStateVar
