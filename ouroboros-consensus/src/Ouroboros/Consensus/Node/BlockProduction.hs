{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , blockProductionIO
    -- * Get leader proof
  , defaultGetLeaderProof
  ) where

import           Control.Tracer (Tracer)
import           Crypto.Random (MonadRandom)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
      -- | Check if we should produce a block
      getLeaderProof :: Tracer m (ForgeState blk)
                     -> Ticked (LedgerView (BlockProtocol blk))
                     -> ConsensusState     (BlockProtocol blk)
                     -> m (LeaderCheck     (BlockProtocol blk))

      -- | Produce a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- Note that this function is not run in @m@, but in some monad @n@
      -- which only has the ability to produce random number and access to the
      -- 'ForgeState'.
    , produceBlock :: BlockNo               -- Current block number
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> m blk
    }

blockProductionIO :: forall blk. (BlockSupportsProtocol blk, CanForge blk)
                  => TopLevelConfig blk
                  -> CanBeLeader (BlockProtocol blk)
                  -> MaintainForgeState IO blk
                  -> IO (BlockProduction IO blk)
blockProductionIO cfg canBeLeader mfs = do
    varForgeState <- newMVar (initForgeState mfs)
    let upd :: Update IO (ForgeState blk)
        upd = updateFromMVar varForgeState
    return $ BlockProduction {
        getLeaderProof = \tracer ->
          defaultGetLeaderProof
            cfg
            canBeLeader
            mfs
            (traceUpdate tracer upd)
      , produceBlock = \bno ledgerState txs proof -> do
          forgeState <- readMVar varForgeState
          forgeBlock cfg forgeState bno ledgerState txs proof
      }

{-------------------------------------------------------------------------------
  Get leader proof
-------------------------------------------------------------------------------}

defaultGetLeaderProof :: ( MonadRandom m
                         , ConsensusProtocol (BlockProtocol blk)
                         )
                      => TopLevelConfig blk
                      -> CanBeLeader (BlockProtocol blk)
                      -> MaintainForgeState m blk
                      -> Update m (ForgeState blk)
                      -> Ticked (LedgerView (BlockProtocol blk))
                      -> ConsensusState     (BlockProtocol blk)
                      -> m (LeaderCheck     (BlockProtocol blk))
defaultGetLeaderProof cfg proof mfs upd lgrSt consensusSt = do
    updateForgeState mfs upd (tickedSlotNo lgrSt)
    checkIsLeader
      (configConsensus cfg)
      proof
      lgrSt
      consensusSt
