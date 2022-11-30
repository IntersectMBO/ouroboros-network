{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Monadic side of the Mempool implementation.
--
-- Using the functions defined in Ouroboros.Consensus.Mempool.Impl.Pure,
-- a dedicated constructor 'openMempool' is provided to encapsulate the mempool
-- functionality.
--
-- The implementation is based on a MempoolEnv that captures the relevant
-- variables to manage the mempool and is then used to craft functions that
-- conform to the Mempool datatype API.
--
-- The operations performed on the Mempool are written in a pure fashion in
-- Ouroboros.Consensus.Mempool.Impl.Pure.
module Ouroboros.Consensus.Mempool.Impl (
    openMempool
    -- * For testing purposes
  , LedgerInterface (..)
  , chainDbLedgerInterface
  , openMempoolWithoutSyncThread
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict.TMVar (newTMVarIO)
import           Control.Monad.Except
import           Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import           Control.Tracer

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.Impl.Pure
import           Ouroboros.Consensus.Mempool.Impl.Types
import           Ouroboros.Consensus.Mempool.TxSeq (TxTicket (txTicketTx),
                     zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk@ in @m@ to manipulate the mempool. It will also
-- fork a thread that syncs the mempool and the ledger when the ledger changes.
openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk)
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
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize

mkMempool ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk -> Mempool m blk
mkMempool mpEnv = Mempool
    { tryAddTxs      = implTryAddTxs mpEnv
    , removeTxs      = implRemoveTxs mpEnv
    , syncWithLedger = fst <$> implSyncWithLedger mpEnv
    , getSnapshot    = implSnapshotFromIS <$> readTMVar istate
    , getSnapshotFor = implGetSnapshotFor mpEnv
    , getCapacity    = isCapacity <$> readTMVar istate
    , getTxSize      = txSize
    }
  where
    MempoolEnv {
        mpEnvStateVar         = istate
      , mpEnvTxSize           = txSize
      } = mpEnv


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
              (ChainDB.PointNotFound blk)
              (LedgerTables (LedgerState blk) ValuesMK))
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDbLedgerInterface ::
     ( IOLike m
     , LedgerSupportsMempool blk
     )
  => ChainDB m blk -> LedgerInterface m blk
chainDbLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState =
        ledgerState . ledgerDbCurrent <$> ChainDB.getLedgerDB chainDB
    , getLedgerTablesAtFor = \pt txs -> do
        let keys = ExtLedgerStateTables
                 $ foldl' (zipLedgerTables (<>)) polyEmptyLedgerTables
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
    let (slot, st') = tickLedgerState cfg $ ForgeInUnknownSlot st
    isVar <- newTMVarIO
             $ initInternalState capacityOverride zeroTicketNo slot st'
    return MempoolEnv
        { mpEnvLedger           = ledgerInterface
        , mpEnvLedgerCfg        = cfg
        , mpEnvStateVar         = isVar
        , mpEnvTracer           = tracer
        , mpEnvTxSize           = txSize
        , mpEnvCapacityOverride = capacityOverride
        }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (
                                   IOLike m
                                 , LedgerSupportsMempool blk
                                 , LedgerSupportsProtocol blk
                                 , HasTxId (GenTx blk)
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
          ledgerTipPoint
      <$> getCurrentLedgerState (mpEnvLedger menv)

{-------------------------------------------------------------------------------
  Implementing the mempool API
-------------------------------------------------------------------------------}

-- | Add a list of transactions (oldest to newest).
--
-- This function returns two lists: the transactions that were added or
-- rejected, and the transactions that could not yet be added, because the
-- Mempool capacity was reached. See 'addTxs' for a function that blocks in
-- case the Mempool capacity is reached.
--
-- Transactions are added one by one, updating the Mempool each time one was
-- added successfully.
--
-- See the necessary invariants on the Haddock for 'API.tryAddTxs'.
--
-- This function syncs the Mempool contents with the ledger state in case the
-- ledger state in the Mempool is no longer accessible for the LedgerDB and
-- therefore we wouldn't be able to apply transactions to it. This function must
-- apply the transaction on /some/ ledger state and if the one in the mempool
-- goes missing, necessarily the background thread that syncs the mempool will
-- fire soon. By performing the sync here we just make sure the ledger state is
-- updated before we retry to add the transaction, and the subsequent call to
-- 'implSyncWithLedger' by the background thread will be cheap.
--
-- INVARIANT: The code needs that read and writes on the state are coupled
-- together or inconsistencies will arise. To ensure that STM transactions are
-- short, each iteration of the helper function is a separate STM transaction.
implTryAddTxs
  :: forall m blk.
     ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> WhetherToIntervene
  -> [GenTx blk]
  -> m ([MempoolAddTxResult blk], [GenTx blk])
implTryAddTxs mpEnv wti =
    go []
  where
    MempoolEnv {
        mpEnvLedgerCfg = cfg
      , mpEnvLedger    = ldgrInterface
      , mpEnvStateVar  = istate
      , mpEnvTracer    = trcr
      , mpEnvTxSize    = txSize
      } = mpEnv

    -- MAYBE batch transaction keys queries
    go :: [MempoolAddTxResult blk]
       -> [GenTx blk]
       -> m ([MempoolAddTxResult blk], [GenTx blk])
    go acc = \case
      []            -> pure (reverse acc, [])
      txs@(tx:next) -> do
        is <- atomically $ takeTMVar istate
        mTbs <- getLedgerTablesAtFor ldgrInterface (isTip is) [tx]
        case mTbs of
          Right tbs -> case pureTryAddTx cfg txSize wti tx is tbs of
            NoSpaceLeft -> do
              atomically $ putTMVar istate is
              pure (reverse acc, txs)
            TransactionProcessed is' result ev -> do
              atomically $ putTMVar istate $ fromMaybe is is'
              traceWith trcr ev
              go (result:acc) next
          Left ChainDB.PointNotFound{} -> do
            -- We couldn't retrieve the values because the state is no longer on
            -- the db. We need to resync.
            atomically $ putTMVar istate is
            (_, mTrace) <- implSyncWithLedger mpEnv
            whenJust mTrace (traceWith trcr)
            go acc txs

implSyncWithLedger ::
     forall m blk. (
       IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk, Maybe (TraceEventMempool blk))
implSyncWithLedger mpEnv = do
  (is, ls) <- atomically $ do
    is <- takeTMVar istate
    ls <- getCurrentLedgerState ldgrInterface
    pure (is, ls)

  let (slot, ls') = tickLedgerState cfg $ ForgeInUnknownSlot ls

  if pointHash (isTip is) == castHash (getTipHash ls) &&
     isSlotNo is == slot
    then do
    -- The tip didn't change, put the same state.
    atomically $ putTMVar istate is
    pure $ (implSnapshotFromIS is, Nothing)
    else do
    -- We need to revalidate
    let pt = castPoint (getTip ls)
        txs = [ txForgetValidated . TxSeq.txTicketTx $ tx
              | tx <- TxSeq.toList $ isTxs is
              ]
    mTbs <- getLedgerTablesAtFor ldgrInterface pt txs
    case mTbs of
      Right tbs -> do
        let (is', mTrace) = pureSyncWithLedger
                              capacityOverride
                              cfg
                              slot
                              ls'
                              tbs
                              is
        atomically $ putTMVar istate is'
        whenJust mTrace (traceWith trcr)
        return $ (implSnapshotFromIS is', mTrace)
      Left ChainDB.PointNotFound{} -> do
        -- If the point is gone, resync
        atomically $ putTMVar istate is
        implSyncWithLedger mpEnv
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

implRemoveTxs ::
   forall m blk. (
       IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
   => MempoolEnv m blk
   -> NE.NonEmpty (GenTxId blk)
   -> m ()
implRemoveTxs mpEnv toRemove = do
    (is, ls) <- atomically $ do
      is <- takeTMVar istate
      ls <- getCurrentLedgerState ldgrInterface
      pure (is, ls)
    let toKeep = filter
                 (   (`notElem` Set.fromList (NE.toList toRemove))
                     . txId
                     . txForgetValidated
                     . txTicketTx
                 )
                 (TxSeq.toList $ isTxs is)
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot ls)
        toKeep' = [ txForgetValidated . TxSeq.txTicketTx $ tx | tx <- toKeep ]
    mTbs <- getLedgerTablesAtFor ldgrInterface (castPoint (getTip ls)) toKeep'
    case mTbs of
      Left ChainDB.PointNotFound{} -> do
        atomically $ putTMVar istate is
        implRemoveTxs mpEnv toRemove
      Right tbs -> do
        let (is', t) = pureRemoveTxs
                         capacityOverride
                         cfg
                         slot
                         ticked
                         tbs
                         (isLastTicketNo is)
                         toKeep
                         toRemove
        atomically $ putTMVar istate is'
        traceWith trcr t
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedger           = ldgrInterface
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

implGetSnapshotFor ::
  forall m blk. (
       IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> Point blk -- ^ The point on the ledger database where we want to acquire a
               -- snapshot on
  -> SlotNo -- ^ Get snapshot for this slot number (usually the current slot)
  -> TickedLedgerState blk DiffMK -- ^ The ledger state at 'pt' ticked to 'slot'
  -> m (Maybe (MempoolSnapshot blk))
implGetSnapshotFor mpEnv pt slot ticked = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked) &&
     isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure . Just . implSnapshotFromIS $ is
    else do
      -- We need to revalidate the transactions.
      let txs = [ txForgetValidated . TxSeq.txTicketTx $ tx
                | tx <- TxSeq.toList $ isTxs is
                ]
          go = do
            mTbs <- getLedgerTablesAtFor ldgrInterface pt txs
            case mTbs of
              Right tbs                    -> pure $ Just $ getSnap is tbs
              Left ChainDB.PointNotFound{} -> pure Nothing
      go
  where
    getSnap is tbs = pureGetSnapshotFor
                       capacityOverride
                       cfg
                       tbs
                       is
                       (ForgeInKnownSlot slot ticked)
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedgerCfg        = cfg
               , mpEnvLedger           = ldgrInterface
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv
