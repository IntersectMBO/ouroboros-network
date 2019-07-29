{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  , LedgerInterface (..)
  , chainDBLedgerInterface
    -- * For testing purposes
  , openMempoolWithoutSyncThread
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

import           Ouroboros.Storage.ChainDB (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Mempool.TxSeq (TicketNo, TxSeq (..),
                     TxTicket (..), fromTxSeq, lookupByTicketNo,
                     splitAfterTicketNo, zeroTicketNo)
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
            -> LedgerInterface m blk
            -> LedgerConfig blk
            -> Tracer m (TraceEventMempool blk)
            -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg tracer = do
    env <- initMempoolEnv ledger cfg tracer
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadSTM m
     , ApplyTx blk
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> Tracer m (TraceEventMempool blk)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg tracer =
    mkMempool <$> initMempoolEnv ledger cfg tracer

mkMempool :: (MonadSTM m, ApplyTx blk)
          => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool env = Mempool
    { addTxs        = implAddTxs env
    , withSyncState = implWithSyncState env
    , getSnapshot   = implGetSnapshot env
    , zeroIdx       = zeroTicketNo
    }

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
  { getCurrentLedgerState :: STM m (LedgerState blk)
  }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface :: MonadSTM m => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      isTxs          :: TxSeq (GenTx blk)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip          :: ChainHash blk

      -- | The mempool 'TicketNo' counter.
      --
      -- See 'vrLastTicketNo' for more information.
    , isLastTicketNo :: TicketNo
    }

data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger    :: LedgerInterface m blk
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvStateVar  :: TVar m (InternalState blk)
    , mpEnvTracer    :: Tracer m (TraceEventMempool blk)
    }

initInternalState :: InternalState blk
initInternalState = IS TxSeq.Empty Block.GenesisHash zeroTicketNo

initMempoolEnv :: MonadSTM m
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> Tracer m (TraceEventMempool blk)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg tracer = do
    isVar <- atomically $ newTVar initInternalState
    return MempoolEnv
      { mpEnvLedger    = ledgerInterface
      , mpEnvLedgerCfg = cfg
      , mpEnvStateVar  = isVar
      , mpEnvTracer    = tracer
      }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: ( MonadAsync m
                                 , MonadFork m
                                 , MonadMask m
                                 , ApplyTx blk
                                 )
                              => ThreadRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv = do
    initialTipPoint <- atomically getCurrentTip
    onEachChange registry id initialTipPoint getCurrentTip action
  where
    action _tipPoint = implWithSyncState menv (const (return ()))
    MempoolEnv { mpEnvLedger } = menv
    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip = ledgerTipPoint <$> getCurrentLedgerState mpEnvLedger

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
            { vrBefore, vrValid, vrLastTicketNo
            , vrNewValid = accepted
            , vrInvalid  = rejected
            } = validateNew syncRes
      writeTVar mpEnvStateVar IS { isTxs          = vrValid
                                 , isTip          = vrBefore
                                 , isLastTicketNo = vrLastTicketNo
                                 }
      mempoolSize <- getMempoolSize mpEnv
      return (removed, accepted, rejected, mempoolSize)

    traceBatch TraceMempoolRemoveTxs   mempoolSize (map fst removed)
    traceBatch TraceMempoolAddTxs      mempoolSize accepted
    traceBatch TraceMempoolRejectedTxs mempoolSize (map fst rejected)

    return $ [(tx, Just err) | (tx, err) <- rejected] ++
             zip accepted (repeat Nothing)
  where
    traceBatch mkEv size batch
      | null batch = return ()
      | otherwise  = traceWith mpEnvTracer (mkEv batch size)

    -- | We first reset 'vrInvalid' to an empty list such that afterwards it
    -- will only contain the /new/ invalid transactions.
    validateNew :: ValidationResult blk -> ValidationResult blk
    validateNew res = repeatedly
        (extendVRNew mpEnvLedgerCfg)
        txs
        res { vrInvalid = [] }

implWithSyncState
  :: (MonadSTM m, ApplyTx blk)
  => MempoolEnv m blk
  -> (MempoolSnapshot blk TicketNo -> STM m a)
  -> m a
implWithSyncState mpEnv@MempoolEnv{mpEnvTracer, mpEnvStateVar} f = do
    (removed, mempoolSize, res) <- atomically $ do
      ValidationResult
        { vrBefore
        , vrValid
        , vrInvalid
        , vrLastTicketNo
        } <- validateIS mpEnv
      writeTVar mpEnvStateVar IS
        { isTxs          = vrValid
        , isTip          = vrBefore
        , isLastTicketNo = vrLastTicketNo
        }
      -- The number of transactions in the mempool /after/ removing invalid
      -- transactions.
      mempoolSize <- getMempoolSize mpEnv
      snapshot    <- implGetSnapshot mpEnv
      res         <- f snapshot
      return (map fst vrInvalid, mempoolSize, res)
    unless (null removed) $
      traceWith mpEnvTracer $ TraceMempoolRemoveTxs removed mempoolSize
    return res

implGetSnapshot :: ( MonadSTM m
                   , ApplyTx blk
                   )
                => MempoolEnv m blk
                -> STM m (MempoolSnapshot blk TicketNo)
implGetSnapshot MempoolEnv{mpEnvStateVar} = do
  is <- readTVar mpEnvStateVar
  pure MempoolSnapshot
    { snapshotTxs      = implSnapshotGetTxs      is
    , snapshotTxsAfter = implSnapshotGetTxsAfter is
    , snapshotLookupTx = implSnapshotGetTx       is
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
                   -> [(GenTx blk, TicketNo)]
implSnapshotGetTxs = (flip implSnapshotGetTxsAfter) zeroTicketNo

implSnapshotGetTxsAfter :: ApplyTx blk
                        => InternalState blk
                        -> TicketNo
                        -> [(GenTx blk, TicketNo)]
implSnapshotGetTxsAfter IS{isTxs} tn =
    fromTxSeq $ snd $ splitAfterTicketNo isTxs tn

implSnapshotGetTx :: ApplyTx blk
                  => InternalState blk
                  -> TicketNo
                  -> Maybe (GenTx blk)
implSnapshotGetTx IS{isTxs} tn = isTxs `lookupByTicketNo` tn

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

    -- | The mempool 'TicketNo' counter.
    --
    -- When validating new transactions, this should be incremented, starting
    -- from 'isLastTicketNo' of the 'InternalState'.
    -- When validating previously applied transactions, this field should not
    -- be affected.
  , vrLastTicketNo :: TicketNo
  }

-- | Initialize 'ValidationResult' from a ledger state and a list of
-- transactions /known/ to be valid in that ledger state
initVR :: forall blk. ApplyTx blk
       => LedgerConfig blk
       -> TxSeq (GenTx blk)
       -> (ChainHash blk, LedgerState blk)
       -> TicketNo
       -> ValidationResult blk
initVR cfg = \knownValid (tip, st) lastTicketNo -> ValidationResult {
      vrBefore       = tip
    , vrValid        = knownValid
    , vrNewValid     = []
    , vrAfter        = afterKnownValid
                         (Foldable.toList knownValid)
                         st
    , vrInvalid      = []
    , vrLastTicketNo = lastTicketNo
    }
  where
    afterKnownValid :: [GenTx blk] -> LedgerState blk -> LedgerState blk
    afterKnownValid []       = id
    afterKnownValid (tx:txs) = afterKnownValid txs . reapplyTxSameState cfg tx

-- | Extend 'ValidationResult' with a previously validated transaction that
-- may or may not be valid in this ledger state
--
-- n.b. Even previously validated transactions may not be valid in a different
-- ledger state;  it is /still/ useful to indicate whether we have previously
-- validated this transaction because, if we have, we can utilize 'reapplyTx'
-- rather than 'applyTx' and, therefore, skip things like cryptographic
-- signatures.
extendVRPrevApplied :: ApplyTx blk
                    => LedgerConfig blk
                    -> (GenTx blk, TicketNo)
                    -> ValidationResult blk
                    -> ValidationResult blk
extendVRPrevApplied cfg (tx, tn)
         vr@ValidationResult{vrValid, vrAfter, vrInvalid} =
    case runExcept (reapplyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid   = vrValid :> TxTicket tx tn
                      , vrAfter   = st'
                      }

-- | Extend 'ValidationResult' with a new transaction (one which we have not
-- previously validated) that may or may not be valid in this ledger state.
extendVRNew :: ApplyTx blk
            => LedgerConfig blk
            -> GenTx blk
            -> ValidationResult blk
            -> ValidationResult blk
extendVRNew cfg tx
         vr@ValidationResult { vrValid
                             , vrAfter
                             , vrInvalid
                             , vrLastTicketNo
                             , vrNewValid
                             } =
    let nextTicketNo = succ vrLastTicketNo
    in  case runExcept (applyTx cfg tx vrAfter) of
      Left err  -> vr { vrInvalid      = (tx, err) : vrInvalid
                      }
      Right st' -> vr { vrValid        = vrValid :> TxTicket tx nextTicketNo
                      , vrNewValid     = tx : vrNewValid
                      , vrAfter        = st'
                      , vrLastTicketNo = nextTicketNo
                      }

-- | Validate internal state
validateIS :: forall m blk. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk -> STM m (ValidationResult blk)
validateIS MempoolEnv{mpEnvLedger, mpEnvLedgerCfg, mpEnvStateVar} =
    go <$> getCurrentLedgerState mpEnvLedger <*> readTVar mpEnvStateVar
  where
    go :: LedgerState      blk
       -> InternalState    blk
       -> ValidationResult blk
    go st IS{isTxs, isTip, isLastTicketNo}
        | tip == isTip
        = initVR mpEnvLedgerCfg isTxs (tip, st) isLastTicketNo
        | otherwise
        = repeatedly (extendVRPrevApplied mpEnvLedgerCfg) (fromTxSeq isTxs)
        $ initVR mpEnvLedgerCfg TxSeq.Empty (tip, st) isLastTicketNo
      where
        tip = Block.pointHash $ ledgerTipPoint st
