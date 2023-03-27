{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Mempool with a mocked ledger interface
module Bench.Consensus.MempoolWithMockedLedgerItf (
    InitialMempoolAndModelParams (..)
    -- * Mempool with a mocked LedgerDB interface
  , MempoolWithMockedLedgerItf (getMempool)
  , getTxs
  , openMempoolWithMockedLedgerItf
  , setLedgerState
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict (StrictTVar,
                     atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import           Control.DeepSeq (NFData (rnf))
import           Control.Tracer (Tracer)
import           Data.Foldable (foldMap')
import           Ouroboros.Consensus.HeaderValidation as Header
import           Ouroboros.Consensus.Ledger.Basics (LedgerState, ValuesMK)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool

data MempoolWithMockedLedgerItf m blk = MempoolWithMockedLedgerItf {
      getLedgerInterface :: !(Mempool.LedgerInterface m blk)
    , getLedgerStateTVar :: !(StrictTVar m (LedgerState blk ValuesMK))
    , getMempool         :: !(Mempool m blk)
    }

instance NFData (MempoolWithMockedLedgerItf m blk) where
  -- TODO: check we're OK with skipping the evaluation of the
  -- MempoolWithMockedLedgerItf. The only data we could force here is the
  -- 'LedgerState' inside 'getLedgerStateTVar', but that would require adding a
  -- 'NFData' constraint and perform unsafe IO. Since we only require this
  -- instance to be able to use
  -- [env](<https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env),
  -- and we only care about initializing the mempool before running the
  -- benchmarks, maybe this definition is enough.
  rnf MempoolWithMockedLedgerItf {} = ()

data InitialMempoolAndModelParams blk = MempoolAndModelParams {
      immpInitialState :: !(Ledger.LedgerState blk ValuesMK)
    , immpLedgerConfig :: !(Ledger.LedgerConfig blk)
    }

openMempoolWithMockedLedgerItf ::
     ( Ledger.LedgerSupportsMempool blk
     , Ledger.HasTxId (Ledger.GenTx blk)
     , Header.ValidateEnvelope blk
     )
  => Mempool.MempoolCapacityBytesOverride
  -> Tracer IO (Mempool.TraceEventMempool blk)
  -> (Ledger.GenTx blk -> Mempool.TxSizeInBytes)
  -> InitialMempoolAndModelParams blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MempoolWithMockedLedgerItf IO blk)
openMempoolWithMockedLedgerItf capacityOverride tracer txSizeImpl params = do
    currentLedgerStateTVar <- newTVarIO (immpInitialState params)
    let ledgerItf = Mempool.LedgerInterface {
            Mempool.getCurrentLedgerState = Ledger.forgetLedgerTables <$>
              readTVar currentLedgerStateTVar
          , Mempool.getLedgerTablesAtFor = \_pt txs -> do
              let keys = foldMap' Ledger.getTransactionKeySets txs
              st <- readTVarIO currentLedgerStateTVar
              let values = Ledger.restrictValues st keys
              pure (Right values)
          }
    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   (immpLedgerConfig params)
                   capacityOverride
                   tracer
                   txSizeImpl
    pure MempoolWithMockedLedgerItf {
        getLedgerInterface = ledgerItf
      , getLedgerStateTVar = currentLedgerStateTVar
      , getMempool         = mempool
    }

setLedgerState ::
     MempoolWithMockedLedgerItf IO blk
  -> LedgerState blk ValuesMK
  -> IO ()
setLedgerState MempoolWithMockedLedgerItf {getLedgerStateTVar} newSt =
  atomically $ writeTVar getLedgerStateTVar newSt

getTxs ::
     (Ledger.LedgerSupportsMempool blk)
  => MempoolWithMockedLedgerItf IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
    snapshotTxs <- fmap Mempool.snapshotTxs $ atomically
                                            $ Mempool.getSnapshot
                                            $ getMempool mockedMempool
    pure $ fmap (Ledger.txForgetValidated . fst) snapshotTxs
