{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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

import           Control.Monad.Class.MonadSTM.Strict (newTMVarIO)
import           Control.Monad.Except
import           Data.Foldable (foldl')
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Typeable

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
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo,
                     TxTicket (txTicketTx), zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.LedgerDB.InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk
                     (LedgerBackingStore, readKeySets)
import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk TicketNo@ in @m@ to manipulate the mempool. It
-- will also fork a thread that syncs the mempool and the ledger when the ledger
-- changes.
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
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize

mkMempool ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool mpEnv = Mempool
    { tryAddTxs      = implTryAddTxs mpEnv
    , removeTxs      = implRemoveTxs mpEnv
    , syncWithLedger = implSyncWithLedger mpEnv
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
    { -- | Get the current tip of the LedgerDB and the current changelog that
      -- allows us to forward values to that state.
      getCurrentLedgerAndChangelog :: STM m (LedgerState blk EmptyMK, MempoolChangelog blk)
    , -- | Retrieve the reference to the backing store from the ChainDB. This is
      -- monadic only because the @LgrDB@ is on the @ChainDBEnv@ and we need to
      -- get it using monad @m@.
      getBackingStore  ::     m (LedgerBackingStore m (ExtLedgerState blk))
    , -- | Wrapper for the DbChangelog flushing lock.
      --
      -- See 'Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB.withReadLock'
      withReadLock     :: forall a. m a -> m a
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDbLedgerInterface ::
     ( IOLike m
     , LedgerSupportsMempool blk
     )
  => ChainDB m blk -> LedgerInterface m blk
chainDbLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerAndChangelog = do
        ldb <- ChainDB.getLedgerDB chainDB
        pure ( ledgerState $ ledgerDbCurrent   ldb
             , (\dbch -> MempoolChangelog
                 (changelogDiffAnchor dbch)
                 (unExtLedgerStateTables $ changelogDiffs dbch))
               $ ledgerDbChangelog ldb
             )
    , getBackingStore = ChainDB.getBackingStore chainDB
    , withReadLock = ChainDB.withLgrReadLock chainDB
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
    (st, dbch) <- atomically $ getCurrentLedgerAndChangelog ledgerInterface
    let (slot, st') = tickLedgerState cfg $ ForgeInUnknownSlot st
    isVar <- newTMVarIO $ initInternalState capacityOverride zeroTicketNo slot st' dbch
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
          ledgerTipPoint (Proxy @blk)
        . fst
      <$> getCurrentLedgerAndChangelog (mpEnvLedger menv)

-- | Add a list of transactions (oldest to newest) by interpreting a 'TryAddTxs'
-- from 'pureTryAddTxs'.
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
-- This function does not sync the Mempool contents with the ledger state in
-- case the latter changes, it relies on the background thread to do that.
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
      , mpEnvStateVar  = istate
      , mpEnvTracer    = trcr
      , mpEnvTxSize    = txSize
      } = mpEnv

    -- MAYBE batch transaction keys queries
    go :: [MempoolAddTxResult blk]
       -> [GenTx blk]
       -> m ([MempoolAddTxResult blk], [GenTx blk])
    go acc = \case
      []         -> pure (reverse acc, [])
      txs@(tx:next) -> do
        is <- atomically $ takeTMVar istate

        let err :: m ([MempoolAddTxResult blk], [GenTx blk])
            err = do
              -- forwarding failed because the changelog anchor changed compared
              -- to the one in the internal state.
              atomically $ putTMVar istate is
              void $ implSyncWithLedger mpEnv
              go acc txs
            ok :: LedgerTables (LedgerState blk) ValuesMK -> m ([MempoolAddTxResult blk], [GenTx blk])
            ok = \values ->
              case pureTryAddTxs cfg txSize wti tx is values of
                NoSpaceLeft             -> do
                  atomically $ putTMVar istate is
                  pure (reverse acc, txs)
                TryAddTxs is' result ev -> do
                  atomically $ putTMVar istate $ fromMaybe is is'
                  traceWith trcr ev
                  go (result:acc) next
        fullForward mpEnv (isDbChangelog is) [tx] err ok

implSyncWithLedger ::
     forall m blk. (
       IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv = withReadLock ldgrInterface $ do
  res <- atomically $ do
    is <- takeTMVar istate
    (ls, dbch) <- getCurrentLedgerAndChangelog ldgrInterface

    let (slot, ls') = tickLedgerState cfg $ ForgeInUnknownSlot ls

    case ( isTip    is == castHash (getTipHash ls)
         , isSlotNo is == slot
         , mcAnchor (isDbChangelog is) == mcAnchor dbch) of
      (True, True, True) -> do
        -- The tip and the changelog didn't change, put the same state.
        putTMVar istate is
        return . Left . implSnapshotFromIS $ is
      (True, True, False) -> do
        -- The tip didn't change but the changelog did, just update the
        -- changelog in the state.
        let is' = is { isDbChangelog = dbch }
        putTMVar istate is'
        return . Left . implSnapshotFromIS $ is'
      _ -> do
        -- The tip changed so we have to revalidate the transactions and we will
        -- also use the updated changelog
        return $ Right (is { isDbChangelog = dbch }, slot, ls')
  case res of
    -- Left means that we are already synced
    Left snapshot -> pure snapshot
    -- Right means we have to revalidate
    Right (is, slot, ls) -> do
      let ok = \values -> do
            let NewSyncedState is' snapshot mTrace = pureSyncWithLedger is slot ls values cfg capacityOverride
            atomically $ putTMVar istate is'
            whenJust mTrace (traceWith trcr)
            return snapshot
      forward_ mpEnv (isDbChangelog is) (TxSeq.toList $ isTxs is) ok

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
implRemoveTxs mpEnv txs = withReadLock ldgrInterface $ do
    (is, ls) <- atomically $ do
      is         <- takeTMVar istate
      (ls, dbch) <- getCurrentLedgerAndChangelog ldgrInterface
      pure (is { isDbChangelog = dbch }, ls)
    let toRemove       = Set.fromList $ NE.toList txs
        txs'           = filter
                         (   (`notElem` toRemove)
                           . txId
                           . txForgetValidated
                           . txTicketTx
                         )
                         (TxSeq.toList $ isTxs is)
        (slot, ticked) = tickLedgerState cfg (ForgeInUnknownSlot ls)

    forward_ mpEnv (isDbChangelog is) txs'
      (\values -> do
          let WriteRemoveTxs is' t = pureRemoveTxs cfg capacityOverride txs' txs is slot ticked values
          atomically $ putTMVar istate is'
          traceWith trcr t)
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
  -> SlotNo
  -> TickedLedgerState blk DiffMK
  -> MempoolChangelog blk
  -> m (MempoolSnapshot blk TicketNo)
implGetSnapshotFor mpEnv slot ticked mempoolCh = do
  res <- atomically $ do
    is <- readTMVar istate
    if   isTip    is == castHash (getTipHash ticked)
      && isSlotNo is == slot
      then
        -- We are looking for a snapshot exactly for the ledger state we already
        -- have cached, then just return it.
        pure . Left . implSnapshotFromIS $ is
      else
        -- We need to revalidate the transactions.
        pure $ Right is
  case res of
    Left snap -> pure snap
    Right is ->
      forward_ mpEnv mempoolCh (TxSeq.toList $ isTxs is)
               ( pure
               . pureGetSnapshotFor cfg capacityOverride is (ForgeInKnownSlot slot ticked)
               )
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Run the continuation with the ledger tables after forwarding, calling error
-- if forwarding fails.
forward_ :: ( IOLike m
            , LedgerSupportsMempool blk
            )
         => MempoolEnv m blk
         -> MempoolChangelog blk
         -> [TxTicket (Validated (GenTx blk))] -- ^ Txs to retrieve values for
         -> (LedgerTables (LedgerState blk) ValuesMK -> m a)
         -> m a
forward_ env ch txs = fullForward
                        env
                        ch
                        (map (txForgetValidated. txTicketTx) txs)
                        (error "This must not happen: the read lock should be held!")

-- | Run one of the continuations with the forwarded ledger tables.
fullForward :: ( IOLike m
               , LedgerSupportsMempool blk
               )
            => MempoolEnv m blk
            -> MempoolChangelog blk
            -> [GenTx blk]
            -> m a
            -> (LedgerTables (LedgerState blk) ValuesMK -> m a)
            -> m a
fullForward mpEnv dbch txs err ok = do
  bkst <- getBackingStore (mpEnvLedger mpEnv)
  let rew = RewoundTableKeySets (mcAnchor dbch) (ExtLedgerStateTables $ getKeysForTxList txs)
  UnforwardedReadSets s vals keys <- defaultReadKeySets (readKeySets bkst) (readDb rew)
  let ufs = UnforwardedReadSets s (unExtLedgerStateTables vals) (unExtLedgerStateTables keys)
  case forwardTableKeySets' (mcAnchor dbch) (mcDifferences dbch) ufs of
    Left _         -> err
    Right fwValues -> ok fwValues

getKeysForTxList :: LedgerSupportsMempool blk
                 => [GenTx blk]
                 -> LedgerTables (LedgerState blk) KeysMK
getKeysForTxList =
    foldl' (zipLedgerTables (<>)) polyEmptyLedgerTables
  . map getTransactionKeySets
