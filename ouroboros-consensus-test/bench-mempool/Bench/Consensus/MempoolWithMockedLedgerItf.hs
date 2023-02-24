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
                     atomically, newTVarIO, readTVar, writeTVar)
import           Control.Tracer (Tracer)

import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import qualified Ouroboros.Consensus.Ledger.Basics as Ledger
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger

import           Ouroboros.Consensus.HeaderValidation as Header

import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool


data MempoolWithMockedLedgerItf m blk = MempoolWithMockedLedgerItf {
      getLedgerInterface :: !(Mempool.LedgerInterface m blk)
    , getLedgerStateTVar :: !(StrictTVar m (LedgerState blk))
    , getMempool         :: !(Mempool m blk)
    }

data InitialMempoolAndModelParams blk = MempoolAndModelParams {
      immpInitialState :: !(Ledger.LedgerState blk)
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
            Mempool.getCurrentLedgerState = readTVar currentLedgerStateTVar
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
  -> LedgerState blk
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
