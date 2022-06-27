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
  , chainDBLedgerInterface
  , openMempoolWithoutSyncThread
  ) where

import qualified Control.Exception as Exn
import           Control.Monad.Class.MonadSTM.Strict (newTMVarIO)
import           Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Typeable

import           Control.Tracer

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB

import           Data.Bifunctor (Bifunctor (second), bimap)
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
    , removeTxs      = \txids -> getStatePair mpEnv (StaticLeft ()) (NE.toList txids) [] >>= \case
        StaticLeft (_is, Nothing) -> pure ()
          -- FIXME: This case should be impossible! 'getStatePair' will never
          -- return 'Nothing' since the removals list is nonempty. Shouldn't we
          -- use 'error' here?
        StaticLeft (is,  Just ls) -> do
          mTrace <- atomically $ runRemoveTxs istate
                               $ pureRemoveTxs cfg capacityOverride txids is (ledgerState ls)
          traceWith trcr mTrace
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
                    is
                    cfg
                    capacityOverride
                    (ForgeInKnownSlot slot $ ledgerState ls')
            atomically $ putTMVar istate is
            pure $ Just (forgetLedgerTables ls', ticked, snapshot)
    , getCapacity    = isCapacity <$> readTMVar istate
    , getTxSize      = txSize
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
      -- | See 'getLedgerStateForKeys' in module @Ouroboros.Consensus.Storage.ChainDB.API@.
      --
      -- The only difference between 'getLedgerStateForKeys' and
      -- 'getLedgerStateForTxs' is that in the former we operate with
      -- 'ExtLedgerState' and in the latter we use 'LedgerState'.
    , getLedgerStateForTxs  :: forall b a.
           StaticEither b () (Point blk)
        -> (ExtLedgerState blk EmptyMK -> m (a, LedgerTables (ExtLedgerState blk) KeysMK))
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
    , getLedgerStateForTxs  = \seP m ->
        fmap
          (bimap (      second unExtLedgerStateTables)
                 (fmap (second unExtLedgerStateTables)))
        $ ChainDB.getLedgerStateForKeys chainDB seP m
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
          ledgerTipPoint (Proxy @blk) . ledgerState
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
                  pureSyncWithLedger is0 (ledgerState ls) cfg capacityOverride
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
    StaticLeft (is, Nothing) ->
      -- In this case, the point of the ledger state at the tip of the chain,
      -- and the point of the internal ledger state are the same. Therefore
      -- there is nothing to do.
      pure (implSnapshotFromIS is)
    StaticLeft (is, Just ls) -> do
      (_is', mTrace, snapshot) <- atomically
                                $ runSyncWithLedger istate
                                $ pureSyncWithLedger is (ledgerState ls) cfg capacityOverride
      whenJust mTrace (traceWith trcr)
      return snapshot
  where
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvTracer           = trcr
               , mpEnvLedgerCfg        = cfg
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

{-------------------------------------------------------------------------------
  GetStatePair
-------------------------------------------------------------------------------}

-- | Get the current mempool internal state and the ledger state at the tip of
-- the chain, loaded with all the 'ValuesMK' that are needed to validate the
-- both the transactions in the mempool as well as the transactions provided as
-- parmeters.
--
-- This function allows to specify the keys in the internal state we do not want
-- to fetch the inputs from (see @removals@ below).
--
-- In the 'StaticLeft' case, this function returns 'Nothing' when there are no
-- removals and additions (the last two parameters are empty) __and__ the point
-- of the internal state coincides with that of the ledger state at the tip of
-- the chain. If the aforementioned conditions are met, then we already have in
-- the ledger state all the values that we need to validate the transactions in
-- the internal state ('isTxs').
--
-- In the 'StaticRight' case, if a block point is given and that point is not on
-- the current chain, then no ledger state is returned.
--
-- NOTE: The ledger state is not necessarily the anchor of the 'InternalState'!
-- FIXME: What is the anchor of the internal state?
getStatePair :: forall m blk b.
     ( IOLike m
     , LedgerSupportsMempool blk
     , LedgerSupportsProtocol blk
     , HasTxId (GenTx blk)
     , Typeable b
     )
  => MempoolEnv m blk
  -> StaticEither b () (Point blk)
     -- ^ desired ledger state, otherwise uses the ledger state at the tip of
     -- the chain
  -> [GenTxId blk]
     -- ^ removals: txids in internal state to not fetch the inputs of
  -> [GenTx blk]
     -- ^ additons: txs not in internal state to fetch the inputs of
  -> m (StaticEither
          b
                 (InternalState blk, Maybe (ExtLedgerState blk ValuesMK))
          (Maybe (InternalState blk,        ExtLedgerState blk ValuesMK))
       )
getStatePair MempoolEnv { mpEnvStateVar, mpEnvLedger } seP removals txs =
      handle (\(ShortCircuitGetStatePairExn x) -> pure x)
    $ fmap finish
    $ getLedgerStateForTxs mpEnvLedger seP
    $ \ls -> atomically $ do
        let tip = getTip ls
        is0 <- takeTMVar mpEnvStateVar
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
        let keptTxs :: [GenTx blk]
            keptTxs =
                filter ((`notElem` Set.fromList removals) . txId)
              $ map (txForgetValidated . TxSeq.txTicketTx) . TxSeq.toList
              $ isTxs is0
            keys = ExtLedgerStateTables
                 . foldl (zipLedgerTables (<>)) polyEmptyLedgerTables
                 . map getTransactionKeySets
                 $ keptTxs <> txs
        pure ((ls, is0), keys)
  where
    finish ::
         StaticEither
           b
                  ((ExtLedgerState blk EmptyMK, InternalState blk), LedgerTables (LedgerState blk) ValuesMK)
           (Maybe ((ExtLedgerState blk EmptyMK, InternalState blk), LedgerTables (LedgerState blk) ValuesMK))
      -> StaticEither
            b
                   (InternalState blk, Maybe (ExtLedgerState blk ValuesMK))
            (Maybe (InternalState blk,        ExtLedgerState blk ValuesMK))
    finish = \case
      StaticLeft ((ls, is), tables)         -> StaticLeft (is, Just $ ls `withLedgerTables` ExtLedgerStateTables tables)
      StaticRight Nothing                   -> StaticRight Nothing
      StaticRight (Just ((ls, is), tables)) -> StaticRight $ Just (is, ls `withLedgerTables` ExtLedgerStateTables tables)

-- | A type to perform a short circuit when there is nothing to do and we
-- requested a ledger state on top of the 'ChainDB' (i.e. we are using
-- StaticLeft)
newtype ShortCircuitGetStatePairExn b blk =
    ShortCircuitGetStatePairExn
      (StaticEither
         b
                (InternalState blk, Maybe (ExtLedgerState blk ValuesMK))
         (Maybe (InternalState blk,        ExtLedgerState blk ValuesMK))
      )
  deriving (Exn.Exception)

instance Show (ShortCircuitGetStatePairExn b blk) where
  showsPrec p (ShortCircuitGetStatePairExn _x) =
      showParen (p > 10) $ showString "ShortCircuitGetStatePairExn _x"
