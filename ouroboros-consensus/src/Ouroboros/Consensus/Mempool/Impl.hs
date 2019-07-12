{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  ) where

import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import           Data.Word (Word64)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Control.Tracer

import           Ouroboros.Network.Block (ChainHash)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Storage.ChainDB.API

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     appendTx, lookupByTicketNo, splitAfterTicketNo,
                     zeroTicketNo)
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Util (repeatedly)
import           Ouroboros.Consensus.Util.STM (onEachChange)
import           Ouroboros.Consensus.Util.ThreadRegistry (ThreadRegistry)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool :: ( MonadAsync m
               , MonadFork m
               , MonadMask m
               , MonadSTM m
               , ApplyTx blk
               )
            => ThreadRegistry m
            -> ChainDB m blk
            -> LedgerConfig blk
            -> Tracer m (TraceEventMempool blk)
            -> m (Mempool m blk TicketNo)
openMempool registry chainDB cfg tracer = do
    env <- initMempoolEnv chainDB cfg tracer
    forkSyncStateOnTipPointChange registry env
    return Mempool {
        addTxs      = implAddTxs env
      , syncState   = implSyncState env
      , getSnapshot = implGetSnapshot env
      , zeroIdx     = zeroTicketNo
      }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      isTxs :: TxSeq (GenTx blk)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip :: ChainHash blk
    }

data MempoolEnv m blk = MempoolEnv {
      mpEnvChainDB   :: ChainDB m blk
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvStateVar  :: TVar m (InternalState blk)
    , mpEnvTracer    :: Tracer m (TraceEventMempool blk)
    }

initInternalState :: InternalState blk
initInternalState = IS TxSeq.Empty Block.GenesisHash

initMempoolEnv :: MonadSTM m
               => ChainDB m blk
               -> LedgerConfig blk
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv chainDB cfg tracer = do
    isVar <- atomically $ newTVar initInternalState
    return $ MempoolEnv chainDB cfg isVar tracer

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'ChainDB'
-- tip point changes.
forkSyncStateOnTipPointChange :: ( MonadAsync m
                                 , MonadFork m
                                 , MonadMask m
                                 , ApplyTx blk
                                 )
                              => ThreadRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv = do
    initialTipPoint <- atomically $ getTipPoint mpEnvChainDB
    onEachChange registry id initialTipPoint (getTipPoint mpEnvChainDB) action
  where
    action _tipPoint = implSyncState menv
    MempoolEnv { mpEnvChainDB } = menv

{-------------------------------------------------------------------------------
  Mempool Implementation
-------------------------------------------------------------------------------}

implAddTxs :: forall m blk. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk
           -> [GenTx blk]
           -> m [(GenTx blk, Maybe (ApplyTxErr blk))]
implAddTxs mpEnv@MempoolEnv{mpEnvStateVar, mpEnvLedgerCfg, mpEnvTracer} txs = do
    (removed, accepted, rejected, mempoolSize) <- atomically $ do
      -- First sync the state, which might remove some transactions
      syncRes@ValidationResult { vrInvalid = removed } <- validateIS mpEnv
      -- Then validate the new transactions
      let ValidationResult
            { vrBefore, vrValid
            , vrNewValid = accepted
            , vrInvalid  = rejected
            } = validateNew syncRes
      writeTVar mpEnvStateVar IS { isTxs = vrValid, isTip = vrBefore }
      mempoolSize <- getMempoolSize mpEnv
      return (removed, accepted, rejected, mempoolSize)

    traceAll (TraceMempoolRemoveTx   mempoolSize) (map fst removed)
    traceAll (TraceMempoolAddTx      mempoolSize) accepted
    traceAll (TraceMempoolRejectedTx mempoolSize) (map fst rejected)

    return $ [(tx, Just err) | (tx, err) <- rejected] ++
             zip accepted (repeat Nothing)
  where
    traceAll mkEv = mapM_ (traceWith mpEnvTracer . mkEv)

    -- | We first reset 'vrInvalid' to an empty list such that afterwards it
    -- will only contain the /new/ invalid transactions.
    validateNew :: ValidationResult blk -> ValidationResult blk
    validateNew res = extendsVR
        mpEnvLedgerCfg
        False
        txs
        res { vrInvalid = [] }

implSyncState :: (MonadSTM m, ApplyTx blk)
              => MempoolEnv m blk
              -> m ()
implSyncState mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} = do
    (removed, mempoolSize) <- atomically $ do
      ValidationResult { vrBefore, vrValid, vrInvalid } <- validateIS mpEnv
      writeTVar mpEnvStateVar IS { isTxs = vrValid, isTip = vrBefore }
      -- The number of transactions in the mempool /after/ removing invalid
      -- transactions.
      mempoolSize <- getMempoolSize mpEnv
      return (map fst vrInvalid, mempoolSize)
    mapM_ (traceWith mpEnvTracer . TraceMempoolRemoveTx mempoolSize) removed

implGetSnapshot :: ( MonadSTM m
                   , ApplyTx blk
                   )
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot (GenTxId blk) (GenTx blk) TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} = do
  is <- readTVar mpEnvStateVar
  pure MempoolSnapshot
    { getTxs      = implSnapshotGetTxs      is
    , getTxsAfter = implSnapshotGetTxsAfter is
    , getTx       = implSnapshotGetTx       is
    }

-- | Return the number of transactions in the Mempool.
getMempoolSize :: MonadSTM m => MempoolEnv m blk -> STM m Word64
getMempoolSize MempoolEnv{mpEnvStateVar} =
    fromIntegral . Foldable.length . isTxs <$> readTVar mpEnvStateVar

{-------------------------------------------------------------------------------
  MempoolSnapshot Implementation
-------------------------------------------------------------------------------}

implSnapshotGetTxs :: ApplyTx blk
                   => InternalState blk
                   -> [(GenTxId blk, GenTx blk, TicketNo)]
implSnapshotGetTxs = (flip implSnapshotGetTxsAfter) zeroTicketNo

implSnapshotGetTxsAfter :: ApplyTx blk
                        => InternalState blk
                        -> TicketNo
                        -> [(GenTxId blk, GenTx blk, TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn = map
  (\(tx, txTn) -> (computeGenTxId tx, tx, txTn))
  (TxSeq.fromTxSeq $ snd $ splitAfterTicketNo isTxs tn)

implSnapshotGetTx :: ApplyTx blk
                  => InternalState blk
                  -> TicketNo
                  -> Maybe (GenTxId blk, GenTx blk)
implSnapshotGetTx IS{isTxs} tn =
  case isTxs `lookupByTicketNo` tn of
    Nothing -> Nothing
    Just tx -> Just (computeGenTxId tx, tx)

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBefore       :: ChainHash blk

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid        :: TxSeq (GenTx blk)

    -- | New transactions (not previously known) which were found to be valid.
    --
    -- n.b. This will only contain valid transactions which were /newly/ added
    -- to the mempool (not previously known valid transactions).
    --
    -- Order not guaranteed.
  , vrNewValid     :: [GenTx blk]

    -- | The state of the ledger after 'vrValid'
    --
    -- NOTE: This is intentionally not a strict field, so that we don't
    -- evaluate the final ledger state if we don't have to.
  , vrAfter        :: LedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- Order not guaranteed
  , vrInvalid      :: [(GenTx blk, ApplyTxErr blk)]
  }

-- | Initialize 'ValidationResult' from a ledger state and a list of
-- transactions /known/ to be valid in that ledger state
initVR :: forall blk. ApplyTx blk
       => LedgerConfig blk
       -> TxSeq (GenTx blk)
       -> (ChainHash blk, LedgerState blk)
       -> ValidationResult blk
initVR cfg = \knownValid (tip, st) -> ValidationResult {
      vrBefore   = tip
    , vrValid    = knownValid
    , vrNewValid = []
    , vrAfter    = afterKnownValid
                     (Foldable.toList knownValid)
                     st
    , vrInvalid  = []
    }
  where
    afterKnownValid :: [GenTx blk] -> LedgerState blk -> LedgerState blk
    afterKnownValid []       = id
    afterKnownValid (tx:txs) = afterKnownValid txs . reapplyTxSameState cfg tx

-- | Extend 'ValidationResult' with a transaction that may or may not be
-- valid in this ledger state
--
-- Even previously validated transactions may not be valid in a  different
-- ledger state;  it is /still/ useful to indicate whether we /have/ previously
-- validated this transaction, because if we have, we can skip things like
-- cryptographic signatures.
extendVR :: ApplyTx blk
         => LedgerConfig blk
         -> Bool -- ^ Were these transactions previously validated?
         -> GenTx blk
         -> ValidationResult blk
         -> ValidationResult blk
extendVR cfg prevApplied tx
         vr@ValidationResult{vrValid, vrNewValid, vrAfter, vrInvalid} =
    let apply | prevApplied = reapplyTx
              | otherwise   = applyTx in
    case runExcept (apply cfg tx vrAfter) of
      Left err  -> vr { vrInvalid  = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid    = vrValid `appendTx` tx
                      , vrAfter    = st'
                      , vrNewValid = if not prevApplied
                                     then tx : vrNewValid
                                     else vrNewValid
                      }

-- | Apply 'extendVR' to a list of transactions, in order
extendsVR :: ApplyTx blk
          => LedgerConfig blk
          -> Bool -- ^ Were these transactions previously applied?
          -> [GenTx blk]
          -> ValidationResult blk
          -> ValidationResult blk
extendsVR cfg prevApplied = repeatedly (extendVR cfg prevApplied)

-- | Validate internal state
validateIS :: forall m blk. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk -> STM m (ValidationResult blk)
validateIS MempoolEnv{mpEnvChainDB, mpEnvLedgerCfg, mpEnvStateVar} =
    go <$> (Block.pointHash <$> getTipPoint      mpEnvChainDB)
       <*> (ledgerState     <$> getCurrentLedger mpEnvChainDB)
       <*> readTVar mpEnvStateVar
  where
    go :: ChainHash        blk
       -> LedgerState      blk
       -> InternalState    blk
       -> ValidationResult blk
    go tip st IS{isTxs, isTip}
      | tip == isTip = initVR mpEnvLedgerCfg isTxs (tip, st)
      | otherwise    = extendsVR mpEnvLedgerCfg True (Foldable.toList isTxs) $
                         initVR mpEnvLedgerCfg TxSeq.Empty (tip, st)
