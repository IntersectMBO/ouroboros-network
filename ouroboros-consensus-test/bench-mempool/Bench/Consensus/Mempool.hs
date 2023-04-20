{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Bench.Consensus.Mempool (
    -- * Commands
    MempoolCmd (..)
  , mkSimpleTryAdd
    -- ** Queries on commands
  , txsAdded
  , txsAddedInCmds
    -- * Commands execution
  , run
  ) where

import           Bench.Consensus.Mempool.TestBlock ()
import           Bench.Consensus.MempoolWithMockedLedgerItf
                     (MempoolWithMockedLedgerItf, getMempool)
import           Control.DeepSeq (NFData)
import           Control.Monad (void)
import           Data.Foldable (traverse_)
import           GHC.Generics (Generic)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import           Ouroboros.Consensus.Mempool (Mempool (..))

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

newtype MempoolCmd blk = TryAdd [Ledger.GenTx blk]
  deriving (Generic)

deriving stock instance Show (Ledger.GenTx blk) => Show (MempoolCmd blk)
deriving anyclass instance (NFData (Ledger.GenTx blk)) => NFData (MempoolCmd blk)

mkSimpleTryAdd :: Ledger.GenTx blk -> MempoolCmd blk
mkSimpleTryAdd gtx = TryAdd [gtx]

txsAdded :: MempoolCmd blk -> [Ledger.GenTx blk]
txsAdded (TryAdd xs) = xs

txsAddedInCmds :: [MempoolCmd blk] -> [Ledger.GenTx blk]
txsAddedInCmds = concatMap txsAdded

{-------------------------------------------------------------------------------
  Commands execution
-------------------------------------------------------------------------------}

-- TODO: the interpretation of running the command should be defined elsewhere,
-- and tested by state-mathine tests.
run ::
     Applicative m
  => MempoolWithMockedLedgerItf m blk -> [MempoolCmd blk] -> m ()
run mempool = traverse_ (runCmd mempool)

runCmd ::
     Applicative m
  => MempoolWithMockedLedgerItf m blk -> MempoolCmd blk -> m ()
runCmd mempoolWithMockedItf = \case
    TryAdd txs -> void $ tryAddTxs mempool Ledger.DoNotIntervene txs -- TODO: we might want to benchmark the 'Intervene' case
  where
    mempool = getMempool mempoolWithMockedItf
