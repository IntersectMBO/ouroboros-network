{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  ) where

import           Control.Monad.Except
import qualified Data.Foldable as Foldable

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

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
               , Ord (GenTxId blk)
               , ApplyTx blk
               )
            => ThreadRegistry m
            -> ChainDB m blk hdr
            -> LedgerConfig blk
            -> m (Mempool m blk TicketNo)
openMempool registry chainDB cfg = do
    env <- initMempoolEnv chainDB cfg
    forkSyncStateOnTipPointChange registry env
    return Mempool {
        addTxs      = implAddTxs env
      , syncState   = implSyncState env
      , getTxs      = implGetTxs env
      , getTxsAfter = implGetTxsAfter env
      , getTx       = implGetTx env
      , zeroIdx     = zeroTicketNo
      }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
      -- | Transactions currently in the mempool
      isTxs :: TxSeq (GenTxId blk, GenTx blk)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip :: ChainHash blk
    }

data MempoolEnv m blk hdr = MempoolEnv {
      mpEnvChainDB   :: ChainDB m blk hdr
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvStateVar  :: TVar m (InternalState blk)
    }

initInternalState :: InternalState blk
initInternalState = IS TxSeq.Empty Block.GenesisHash

initMempoolEnv :: MonadSTM m
               => ChainDB m blk hdr
               -> LedgerConfig blk
               -> m (MempoolEnv m blk hdr)
initMempoolEnv chainDB cfg = do
    isVar <- atomically $ newTVar initInternalState
    return $ MempoolEnv chainDB cfg isVar

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'ChainDB'
-- tip point changes.
forkSyncStateOnTipPointChange :: ( MonadAsync m
                                 , MonadFork m
                                 , MonadMask m
                                 , Ord (GenTxId blk)
                                 , ApplyTx blk
                                 , Block.StandardHash blk
                                 )
                              => ThreadRegistry m
                              -> MempoolEnv m blk hdr
                              -> m ()
forkSyncStateOnTipPointChange registry menv = do
  initialTipPoint <- atomically $ getTipPoint mpEnvChainDB
  onEachChange registry id initialTipPoint (getTipPoint mpEnvChainDB) action
 where
  action _ = atomically $ void $ implSyncState menv
  MempoolEnv { mpEnvChainDB } = menv

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

-- TODO: This may return some transactions as invalid that aren't new. Remove?
implAddTxs :: forall m blk hdr. (MonadSTM m, Ord (GenTxId blk), ApplyTx blk)
           => MempoolEnv m blk hdr
           -> [(GenTxId blk, GenTx blk)]
           -> m [(GenTx blk, ApplyTxErr blk)]
implAddTxs mpEnv@MempoolEnv{mpEnvStateVar, mpEnvLedgerCfg} txs =
    atomically $ do
      ValidationResult {
        vrBefore,
        vrValid,
        vrInvalid
      } <- validateNew <$> validateIS mpEnv
      writeTVar mpEnvStateVar IS { isTxs   = vrValid
                                 , isTip   = vrBefore
                                 }
      return vrInvalid
  where
    validateNew :: ValidationResult blk ->  ValidationResult blk
    validateNew = extendsVR mpEnvLedgerCfg False txs

implSyncState :: (MonadSTM m, Ord (GenTxId blk), ApplyTx blk)
              => MempoolEnv m blk hdr
              -> STM m [(GenTx blk, ApplyTxErr blk)]
implSyncState mpEnv@MempoolEnv{mpEnvStateVar} = do
  ValidationResult {
    vrBefore
  , vrValid
  , vrInvalid
  } <- validateIS mpEnv
  writeTVar mpEnvStateVar IS { isTxs   = vrValid
                             , isTip   = vrBefore
                             }
  pure vrInvalid

implGetTxs :: (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk hdr
           -> STM m [(GenTxId blk, GenTx blk, TicketNo)]
implGetTxs = (flip implGetTxsAfter) zeroTicketNo

implGetTxsAfter :: (MonadSTM m, ApplyTx blk)
                => MempoolEnv m blk hdr
                -> TicketNo
                -> STM m [(GenTxId blk, GenTx blk, TicketNo)]
implGetTxsAfter MempoolEnv{mpEnvStateVar} tn = do
  IS { isTxs } <- readTVar mpEnvStateVar
  pure $ fromTxSeq $ snd $ splitAfterTicketNo isTxs tn
 where
  fromTxSeq txSeq = fmap
    (\(txid, tx) -> (txid, tx, tn))
    (Foldable.toList txSeq)

implGetTx :: (MonadSTM m, ApplyTx blk)
          => MempoolEnv m blk hdr
          -> TicketNo
          -> STM m (Maybe (GenTxId blk, GenTx blk))
implGetTx MempoolEnv{mpEnvStateVar} tn =
  (`lookupByTicketNo` tn) . isTxs <$> readTVar mpEnvStateVar

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBefore   :: ChainHash blk

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid    :: TxSeq (GenTxId blk, GenTx blk)

    -- | The state of the ledger after 'vrValid'
    --
    -- NOTE: This is intentionally not a strict field, so that we don't
    -- evaluate the final ledger state if we don't have to.
  , vrAfter    :: LedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- Order not guaranteed
  , vrInvalid  :: [(GenTx blk, ApplyTxErr blk)]
  }

-- | Initialize 'ValidationResult' from a ledger state and a list of
-- transactions /known/ to be valid in that ledger state
initVR :: forall blk. ApplyTx blk
       => LedgerConfig blk
       -> TxSeq (GenTxId blk, GenTx blk)
       -> (ChainHash blk, LedgerState blk)
       -> ValidationResult blk
initVR cfg = \knownValid (tip, st) -> ValidationResult {
      vrBefore  = tip
    , vrValid   = knownValid
    , vrAfter   = afterKnownValid
                    (map snd (Foldable.toList knownValid))
                    st
    , vrInvalid = []
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
         -> (GenTxId blk, GenTx blk)
         -> ValidationResult blk
         -> ValidationResult blk
extendVR cfg prevApplied (txid, tx)
         vr@ValidationResult{vrValid, vrAfter, vrInvalid} =
    let apply | prevApplied = reapplyTx
              | otherwise   = applyTx in
    case runExcept (apply cfg tx vrAfter) of
      Left err  -> vr { vrInvalid = (tx, err) : vrInvalid }
      Right st' -> vr { vrValid   = vrValid `appendTx` (txid, tx)
                      , vrAfter   = st' }

-- | Apply 'extendVR' to a list of transactions, in order
extendsVR :: ApplyTx blk
          => LedgerConfig blk
          -> Bool -- ^ Were these transactions previously applied?
          -> [(GenTxId blk, GenTx blk)]
          -> ValidationResult blk
          -> ValidationResult blk
extendsVR cfg prevApplied = repeatedly (extendVR cfg prevApplied)

-- | Validate internal state
validateIS :: forall m blk hdr. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk hdr -> STM m (ValidationResult blk)
validateIS MempoolEnv{mpEnvChainDB, mpEnvLedgerCfg, mpEnvStateVar} =
    go <$> (getChainHash <$> getTipPoint      mpEnvChainDB)
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

    getChainHash :: Block.Point blk -> ChainHash blk
    getChainHash = Block.fromTPoint Block.GenesisHash (Block.BlockHash . Block.pointHash)
