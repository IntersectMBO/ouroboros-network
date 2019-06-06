{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.Mempool.Impl (
    openMempool
  ) where

import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Block (ChainHash)
import qualified Ouroboros.Network.Block as Block

import           Ouroboros.Storage.ChainDB.API

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util (repeatedly)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

openMempool :: (MonadSTM m, ApplyTx blk)
            => ChainDB m blk hdr
            -> LedgerConfig blk
            -> m (Mempool m blk)
openMempool chainDB cfg = do
    env <- initMempoolEnv chainDB cfg
    return Mempool {
        addTxs = implAddTxs env
      , getTxs = implGetTxs env
      }

{-------------------------------------------------------------------------------
  Internal state
-------------------------------------------------------------------------------}

-- | Internal state in the mempool
data InternalState blk = IS {
     -- | Transactions currently in the mempool
      isTxs :: Seq (GenTx blk)

      -- | The tip of the chain that 'isTxs' was validated against
    , isTip :: ChainHash blk
    }

data MempoolEnv m blk hdr = MempoolEnv {
      mpEnvChainDB   :: ChainDB m blk hdr
    , mpEnvLedgerCfg :: LedgerConfig blk
    , mpEnvStateVar  :: TVar m (InternalState blk)
    }

initInternalState :: InternalState blk
initInternalState = IS Seq.empty Block.GenesisHash

initMempoolEnv :: MonadSTM m
               => ChainDB m blk hdr
               -> LedgerConfig blk
               -> m (MempoolEnv m blk hdr)
initMempoolEnv chainDB cfg = do
    isVar <- atomically $ newTVar initInternalState
    return $ MempoolEnv chainDB cfg isVar

{-------------------------------------------------------------------------------
  Implementation
-------------------------------------------------------------------------------}

-- TODO: This may return some transactions as invalid that aren't new. Remove?
implAddTxs :: forall m blk hdr. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk hdr
           -> [GenTx blk]
           -> m [(GenTx blk, ApplyTxErr blk)]
implAddTxs mpEnv@MempoolEnv{..} txs = atomically $ do
    ValidationResult{..} <- validateNew <$> validateIS mpEnv
    writeTVar mpEnvStateVar IS { isTxs = vrValid
                               , isTip = vrBefore
                               }
    return vrInvalid
  where
    validateNew :: ValidationResult blk ->  ValidationResult blk
    validateNew = extendsVR mpEnvLedgerCfg False txs

implGetTxs :: (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk hdr
           -> STM m (Seq (GenTx blk ))
implGetTxs mpEnv@MempoolEnv{..} = do
    ValidationResult{..} <- validateIS mpEnv
    -- TODO (maybe): log the transactions that we silently discard
    writeTVar mpEnvStateVar IS { isTxs = vrValid
                               , isTip = vrBefore
                               }
    return vrValid

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidationResult blk = ValidationResult {
    -- | The tip of the chain before applying these transactions
    vrBefore  :: ChainHash blk

    -- | The transactions that were found to be valid (oldest to newest)
  , vrValid   :: Seq (GenTx blk)

    -- | The state of the ledger after 'vrValid'
    --
    -- NOTE: This is intentionally not a strict field, so that we don't
    -- evaluate the final ledger state if we don't have to.
  , vrAfter   :: LedgerState blk

    -- | The transactions that were invalid, along with their errors
    --
    -- Order not guaranteed
  , vrInvalid :: [(GenTx blk, ApplyTxErr blk)]
  }

-- | Initialize 'ValidationResult' from a ledger state and a list of
-- transactions /known/ to be valid in that ledger state
initVR :: forall blk. ApplyTx blk
       => LedgerConfig blk
       -> Seq (GenTx blk)
       -> (ChainHash blk, LedgerState blk)
       -> ValidationResult blk
initVR cfg = \knownValid (tip, st) -> ValidationResult {
      vrBefore  = tip
    , vrValid   = knownValid
    , vrAfter   = afterKnownValid (Foldable.toList knownValid) st
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
         -> Bool -- ^ Was these transactions previously validated?
         -> GenTx blk
         -> ValidationResult blk
         -> ValidationResult blk
extendVR cfg prevApplied tx ValidationResult{..} =
    case runExcept $ (if prevApplied then reapplyTx else applyTx) cfg tx vrAfter of
      Left err  -> ValidationResult {
                       vrBefore  = vrBefore
                     , vrValid   = vrValid
                     , vrAfter   = vrAfter
                     , vrInvalid = (tx, err) : vrInvalid
                     }
      Right st' -> ValidationResult {
                       vrBefore  = vrBefore
                     , vrValid   = vrValid :|> tx
                     , vrAfter   = st'
                     , vrInvalid = vrInvalid
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
validateIS :: forall m blk hdr. (MonadSTM m, ApplyTx blk)
           => MempoolEnv m blk hdr -> STM m (ValidationResult blk)
validateIS MempoolEnv{..} =
    go <$> (Block.pointHash <$> getTipPoint      mpEnvChainDB)
       <*> (ledgerState     <$> getCurrentLedger mpEnvChainDB)
       <*> readTVar mpEnvStateVar
  where
    go :: ChainHash        blk
       -> LedgerState      blk
       -> InternalState    blk
       -> ValidationResult blk
    go tip st IS{..}
      | tip == isTip = initVR mpEnvLedgerCfg isTxs (tip, st)
      | otherwise    = extendsVR mpEnvLedgerCfg True (Foldable.toList isTxs) $
                         initVR mpEnvLedgerCfg Seq.empty (tip, st)
