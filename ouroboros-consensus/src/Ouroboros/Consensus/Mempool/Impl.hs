{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
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
  , chainDBLedgerInterface
  , openMempoolWithoutSyncThread
  ) where

import qualified Control.Exception as Exn
import           Control.Monad.Class.MonadSTM.Strict (newTMVarIO)
import           Control.Monad.Except
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
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (StaticEither (..), whenJust)
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
    , removeTxs      = \txids -> getStatePair mpEnv (StaticLeft ()) txids [] >>= \case
        StaticLeft (_is, Nothing) -> pure ()
        StaticLeft (is,  Just ls) -> do
          mTrace <- atomically $ do
            let p = pureRemoveTxs cfg capacityOverride txids is ls
            runRemoveTxs istate p
          whenJust mTrace (traceWith trcr)
    , syncWithLedger = implSyncWithLedger mpEnv
    , getSnapshot    = implSnapshotFromIS <$> readTMVar istate
    , getLedgerAndSnapshotFor = \p slot -> do
        o <- getStatePair mpEnv (StaticRight p) [] []
        let StaticRight mbPair = o
        case mbPair of
          Nothing        -> pure Nothing
          Just (is, ls') -> do
            let (ticked, snapshot) =
                  pureGetSnapshotAndTickedFor
                    cfg
                    (ForgeInKnownSlot slot $ ledgerState ls')
                    capacityOverride
                    is
            atomically $ putTMVar istate is
            pure $ Just (forgetLedgerTables ls', ticked, snapshot)
    , getCapacity    = isCapacity <$> readTMVar istate
    , getTxSize      = txSize
    , zeroIdx        = zeroTicketNo
    }
  where
    MempoolEnv {
        mpEnvCapacityOverride = capacityOverride
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = istate
      , mpEnvTracer           = trcr
      , mpEnvTxSize           = txSize
      } = mpEnv

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
    { getCurrentLedgerState :: STM m (ExtLedgerState blk EmptyMK)
    , getLedgerStateForTxs  :: forall b a.
           StaticEither b () (Point blk)
        -> (ExtLedgerState blk EmptyMK -> m (a, [GenTx blk]))
        -> m (StaticEither
                b
                       (a, LedgerTables (LedgerState blk) ValuesMK)
                (Maybe (a, LedgerTables (LedgerState blk) ValuesMK))
              )
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     ( IOLike m
     , LedgerSupportsMempool blk
     )
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ChainDB.getCurrentLedger chainDB
    , getLedgerStateForTxs  = \seP m -> do
        let m' ls = do
              (a, txs) <- m ls
              pure
                $ (,) a
                $ ExtLedgerStateTables
                $ foldl (zipLedgerTables (<>)) polyEmptyLedgerTables
                $ map getTransactionKeySets txs
        case seP of
          StaticLeft () -> do
            o <- ChainDB.getLedgerStateForKeys chainDB (StaticLeft ()) m'
            let StaticLeft (a, ExtLedgerStateTables tables) = o
            pure $ StaticLeft (a, tables)
          StaticRight p -> do
            o <- ChainDB.getLedgerStateForKeys chainDB (StaticRight p) m'
            let StaticRight mbPair = o
            pure $ StaticRight $ case mbPair of
              Nothing                               -> Nothing
              Just (a, ExtLedgerStateTables tables) -> Just (a, tables)
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
--                  , NoThunks (GenTxId blk)   -- TODO how to use this with the TMVar?
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
    st <- atomically $ ledgerState <$> getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState cfg $ ForgeInUnknownSlot $ unstowLedgerTables st
    isVar <- newTMVarIO $ initInternalState capacityOverride zeroTicketNo slot st'
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
          (ledgerTipPoint (Proxy @blk) . ledgerState)
      <$> getCurrentLedgerState (mpEnvLedger menv)



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
        mpEnvCapacityOverride = capacityOverride
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = istate
      , mpEnvTracer           = trcr
      , mpEnvTxSize           = txSize
      } = mpEnv

    -- TODO batch the txs
    go acc = \case
      []     -> pure (reverse acc, [])
      tx:txs -> do
        getStatePair mpEnv (StaticLeft ()) [] [tx] >>= \case
          StaticLeft (_is0, Nothing) -> error "impossible! implTryAddTxs"
          StaticLeft (is0,  Just ls) -> do
            let p@(NewSyncedState is1 _snapshot mTrace) =
                  -- this is approximately a noop if the state is already in
                  -- sync
                  pureSyncWithLedger is0 ls cfg capacityOverride
            whenJust mTrace (traceWith trcr)
            case pureTryAddTxs cfg txSize wti tx is1 of
              NoSpaceLeft               -> do
                void $ atomically $ runSyncWithLedger istate p
                pure (reverse acc, tx:txs)
              TryAddTxs mbIs2 result ev -> do
                atomically $ putTMVar istate $ fromMaybe is1 mbIs2
                traceWith trcr ev
                go (result:acc) txs

implSyncWithLedger ::
     forall m blk. (
       IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> m (MempoolSnapshot blk TicketNo)
implSyncWithLedger mpEnv = getStatePair mpEnv (StaticLeft ()) [] [] >>= \case
    StaticLeft (is, Nothing) -> pure (implSnapshotFromIS is)
    StaticLeft (is, Just ls) -> do
      (_is', mTrace, snapshot) <- atomically $ do
        let p = pureSyncWithLedger is ls cfg capacityOverride
        runSyncWithLedger istate p
      whenJust mTrace (traceWith trcr)
      return snapshot
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Get the current state pair and the current ledger state loaded with all the
-- necessary 'ValuesMK'
--
-- No ledger state is only returned.
--
-- If there is a desired ledger state is given, then no ledger state is only
-- returned if that state is not on the current chain.
--
-- NOTE: The ledger state is not necessarily the anchor of the 'InternalState'!
getStatePair :: forall m blk b.
     ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     , Typeable b
     )
  => MempoolEnv m blk
  -> StaticEither b () (Point blk)
     -- ^ desired ledger state, otherwise uses the tip
  -> [GenTxId blk]
     -- ^ txids in internal state to not fetch the inputs of
  -> [GenTx blk]
     -- ^ txs not in internal state to fetch the inputs of
  -> m (StaticEither
          b
          (InternalState blk, Maybe (LedgerState blk ValuesMK))
          (Maybe (InternalState blk, ExtLedgerState blk ValuesMK))
       )
getStatePair mpEnv seP removals txs =
    wrap $ \ls -> atomically $ do
      let tip = getTip ls
      is0 <- takeTMVar istate
      let nothingToDo =
               isTip is0 == castHash (pointHash tip)
            && null removals
            && null txs
      when nothingToDo $ case seP of
        StaticLeft () -> throwSTM $ ShortCircuitGetStatePairExn (StaticLeft (is0, Nothing))
        StaticRight{} -> pure ()
      -- Beyond this point, every exception is fatal. There's no need for
      -- @flip finally (putTMVar istate is)@ or similar, since that 'TMVar' is
      -- inconsequential for a clean shutdown.
      --
      -- TODO confirm that logic ^^^
      let keptTxs :: [GenTx blk]
          keptTxs =
              filter ((`notElem` Set.fromList removals) . txId)
            $ map (txForgetValidated . TxSeq.txTicketTx) . TxSeq.toList
            $ isTxs is0
      pure ((ls, is0), keptTxs <> txs)
  where
    MempoolEnv {
        mpEnvStateVar = istate
      , mpEnvLedger   = ldgrInterface
      } = mpEnv

    wrap stm =
         handle (\(ShortCircuitGetStatePairExn x) -> pure x)
       $ fmap finish
       $ getLedgerStateForTxs ldgrInterface seP
       $ stm

    finish ::
         StaticEither
           b
                  ((ExtLedgerState blk EmptyMK, InternalState blk), LedgerTables (LedgerState blk) ValuesMK)
           (Maybe ((ExtLedgerState blk EmptyMK, InternalState blk), LedgerTables (LedgerState blk) ValuesMK))
      -> StaticEither
            b
            (InternalState blk, Maybe (LedgerState blk ValuesMK))
            (Maybe (InternalState blk, ExtLedgerState blk ValuesMK))
    finish = \case
      StaticLeft ((ls, is), tables)         -> StaticLeft (is, Just $ withLedgerTables (ledgerState ls) tables)
      StaticRight Nothing                   -> StaticRight Nothing
      StaticRight (Just ((ls, is), tables)) -> StaticRight $ Just (is, ls `withLedgerTables` ExtLedgerStateTables tables)

data ShortCircuitGetStatePairExn b blk =
    ShortCircuitGetStatePairExn
      (StaticEither
         b
         (InternalState blk, Maybe (LedgerState blk ValuesMK))
         (Maybe (InternalState blk, ExtLedgerState blk ValuesMK))
      )
  deriving (Exn.Exception)

instance Show (ShortCircuitGetStatePairExn b blk) where
  showsPrec p (ShortCircuitGetStatePairExn _x) =
      showParen (p > 10) $ showString "ShortCircuitGetStatePairExn _x"
