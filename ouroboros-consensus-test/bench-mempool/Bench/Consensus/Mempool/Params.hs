{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- | Functions related to initial parameters for the mempool. See
-- 'InitialMempoolAndModelParams'.
module Bench.Consensus.Mempool.Params (
    -- * Types
    InitialMempoolAndModelParams (..)
  , mkParams
    -- * Defaults
  , defaultInMemoryBSS
  , defaultLMDB_BSS
  , defaultLedgerDbCfg
  , defaultParams
    -- * Construction of configurations
  , sampleLedgerConfig
  , sampleLedgerDbCfg
    -- * Construction of initial state
  , ledgerStateFromTokens
  , testBlocksFromTxs
  ) where

import           Bench.Consensus.Mempool.TestBlock (PayloadDependentState (..),
                     TestBlock, Token (..), Tx)
import qualified Cardano.Slotting.Time as Time
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Ouroboros.Consensus.Block as Block
import           Ouroboros.Consensus.Config.SecurityParam
                     (SecurityParam (SecurityParam))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Basics (LedgerConfig, ValuesMK (..))
import           Ouroboros.Consensus.Storage.LedgerDB (BackingStoreSelector,
                     LedgerDbCfg)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB
                     (LMDBLimits (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Init
                     (BackingStoreSelector (..))
import           Ouroboros.Consensus.Storage.LedgerDB.LedgerDB
                     (LedgerDbCfg (..))
import qualified Test.Util.TestBlock as TestBlock
import           Test.Util.TestBlock (LedgerState (..))

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Initial parameters for the mempool.
--
-- === Parameters for the ledger interface
--
-- One goal of the mempool parameters is to provide enough information to set up
-- an interface to the ledger database. Setting up a ledger interface requires
-- two main parts of the ledger database: a backing store, and a changelog.
--
-- Which backing store implementation we use is determined by
-- 'immpBackingStoreSelector'. The backing store will be initialised using
-- values from 'immpBackingState'. The changelog keeps track of differences on
-- values that are induced by applying blocks. Each diff in the changelog
-- corresponds to a block. As such, the changelog will be populated by applying
-- blocks from 'immpChangelogBlocks' in sequence to 'immpBackingState'.
--
-- INVARIANT: applying the blocks in 'immpChangelogBlocks' in sequence to
-- 'immpBackingState' should not fail.
--
-- ==== Effect on performance
--
-- How we populate the ledger database with values and differences could affect
-- the performance of mempool operations. To be precise, each time we need a
-- partial ledger state to apply transactions to, we /rewind-read-forward/.
--
-- * Rewind: Rewind keys by determining which slot the tip of the backing store
--   points to.
-- * Read: Read values from the backing store for the rewound keys.
-- * Forward: Forward the read values through the changelog.
--
-- How expensive these steps are depends on how we populate the backing store
-- and changelog. We are not sure if we can estimate the cost of mempool
-- operations on these parameters only, but in general, we suspect that:
--
-- * Reading values succesfully from the backing store incurs extra costs (e.g.,
--   deserialisation and I/O costs), compared to when a value is not found in
--   the backing store.
-- * Forwarding becomes more expensive as the following increase: (i) the number
--   of blocks, and (ii) the size of the diffs induced by blocks.
--
data InitialMempoolAndModelParams m blk = MempoolAndModelParams {
      -- | The values that will be used to initialise a backing store.
      immpBackingState         :: !(LedgerState blk ValuesMK)
      -- | Blocks that will be used to populate a changelog.
    , immpChangelogBlocks      :: ![blk]
    , immpLedgerConfig         :: !(LedgerDbCfg (LedgerState blk))
    , immpBackingStoreSelector :: !(BackingStoreSelector m)
    }

mkParams ::
     LedgerState blk ValuesMK
  -> [blk]
  -> LedgerDbCfg (LedgerState blk)
  -> BackingStoreSelector m
  -> InitialMempoolAndModelParams m blk
mkParams = MempoolAndModelParams

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

defaultLedgerDbCfg :: LedgerDbCfg (LedgerState TestBlock)
defaultLedgerDbCfg = sampleLedgerDbCfg (SecurityParam 10)

defaultInMemoryBSS :: BackingStoreSelector m
defaultInMemoryBSS = InMemoryBackingStore

defaultLMDB_BSS :: MonadIO m => BackingStoreSelector m
defaultLMDB_BSS = LMDBBackingStore LMDBLimits {
      lmdbMapSize = 100 * 1024 * 1024
    , lmdbMaxDatabases = 3
    , lmdbMaxReaders = 16
    }

-- | Default parameters: empty in-memory backing store, empty changelog.
defaultParams :: InitialMempoolAndModelParams m TestBlock
defaultParams =
    mkParams
      (ledgerStateFromTokens [])
      []
      defaultLedgerDbCfg
      defaultInMemoryBSS

{-------------------------------------------------------------------------------
  Construction of configurations
-------------------------------------------------------------------------------}

sampleLedgerConfig :: SecurityParam -> LedgerConfig TestBlock
sampleLedgerConfig secparam =
  HardFork.defaultEraParams secparam (Time.slotLengthFromSec 2)

sampleLedgerDbCfg :: SecurityParam -> LedgerDbCfg (LedgerState TestBlock)
sampleLedgerDbCfg secparam = LedgerDbCfg {
    ledgerDbCfgSecParam = secparam
  , ledgerDbCfg        = sampleLedgerConfig secparam
  }

{-------------------------------------------------------------------------------
  Construction of initial state
-------------------------------------------------------------------------------}

ledgerStateFromTokens :: [Token] -> LedgerState TestBlock ValuesMK
ledgerStateFromTokens tks = TestLedger {
      lastAppliedPoint      = Block.GenesisPoint
    , payloadDependentState = TestPLDS $ ValuesMK $
                                Map.fromList $ map (,()) tks
    }

-- | @'testBlocksFromTxs' txs@ creates a list of successive 'TestBlock's, where
-- each block corresponds to one of the 'Tx's in @txs@.
--
-- POSTCONDITION: The @i@-th result block contains only transaction @txs !! i@.
-- The length of the resulting list is equal to the length of @txs@.
testBlocksFromTxs :: [Tx] -> [TestBlock]
testBlocksFromTxs []         = []
testBlocksFromTxs (tx0:txs0) = go [firstBlk] txs0
  where
    firstBlk :: TestBlock
    firstBlk = TestBlock.firstBlockWithPayload
                0
                tx0

    -- Create a new block using the latest block in the accumulator as a
    -- predecessor.
    go :: [TestBlock] -> [Tx] -> [TestBlock]
    go [] _                     = error "impossible: there should be a \
                                        \previous block!"
    go acc []                   = reverse acc
    go acc@(prevBlk:_) (tx:txs) = go (nextBlk:acc) txs
      where
        nextBlk = mkSuccessorBlock prevBlk tx

    mkSuccessorBlock :: TestBlock -> Tx -> TestBlock
    mkSuccessorBlock prevBlk =
        TestBlock.successorBlockWithPayload
          (Block.blockHash prevBlk)
          (Block.blockSlot prevBlk)
