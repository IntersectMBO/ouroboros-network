{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.TraceSize (
    -- * Generic
    traceSize
    -- * Ledger DB specific
  , LedgerDbSize (..)
  , traceLedgerDbSize
  ) where

import           Cardano.Prelude (CountFailure, computeHeapSize)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Tracer
import           Data.Word

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics

import           Ouroboros.Consensus.Storage.LedgerDB.InMemory (LedgerDB)
import qualified Ouroboros.Consensus.Storage.LedgerDB.InMemory as LedgerDB

{-------------------------------------------------------------------------------
  Generic
-------------------------------------------------------------------------------}

-- | Generic helper to trace a value and its size
traceSize :: MonadIO m
          => Tracer m (a, Either CountFailure Word64)
          -> Tracer m a
traceSize (Tracer f) = Tracer $ \a -> do
    sz <- liftIO $ computeHeapSize a
    f (a, sz)

{-------------------------------------------------------------------------------
  Ledger DB specific
-------------------------------------------------------------------------------}

data LedgerDbSize l = LedgerDbSize {
      -- | The tip of the ledger DB
      ledgerDbTip       :: Point l

      -- | Size of the ledger at the tip of the DB
    , ledgerDbSizeTip   :: Either CountFailure Word64

      -- | Size of the entire (in-memory) ledger DB
    , ledgerDbSizeTotal :: Either CountFailure Word64
    }
  deriving (Show)

-- | Trace the size of the ledger
--
-- Only traces slots for which the predicate results true (genesis will be
-- considered to be slot 0).
traceLedgerDbSize :: forall m l. (MonadIO m, GetTip l)
                  => (Word64 -> Bool)
                  -> Tracer m (LedgerDbSize l)
                  -> Tracer m (LedgerDB l)
traceLedgerDbSize p (Tracer f) = Tracer $ \(!db) -> do
    let !ledger = LedgerDB.ledgerDbCurrent db
        !tip    = getTip ledger

    when (shouldTrace tip) $ do
      sizeTip   <- liftIO $ computeHeapSize ledger
      sizeTotal <- liftIO $ computeHeapSize db
      f $ LedgerDbSize {
              ledgerDbTip       = tip
            , ledgerDbSizeTip   = sizeTip
            , ledgerDbSizeTotal = sizeTotal
            }
  where
    shouldTrace :: Point l -> Bool
    shouldTrace GenesisPoint     = p 0
    shouldTrace (BlockPoint s _) = p (unSlotNo s)
