{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , blockProductionIO
  , blockProductionIOLike
  ) where

import           Control.Monad.Trans (lift)
import           Control.Tracer (Tracer, natTracer)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Random

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
      -- | Check if we should produce a block
      getLeaderProof :: Tracer m (ForgeState blk)
                     -> Ticked (LedgerView (BlockProtocol blk))
                     -> ConsensusState     (BlockProtocol blk)
                     -> m (Maybe (IsLeader (BlockProtocol blk)))

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
    varForgeState <- newTVarM (initForgeState mfs)
    let upd :: Update IO (ForgeState blk)
        upd = updateFromTVar varForgeState
    return $ BlockProduction {
        getLeaderProof = \tracer ->
          defaultGetLeaderProof
            cfg
            canBeLeader
            mfs
            (traceUpdate tracer upd)
      , produceBlock = \bno ledgerState txs proof -> do
          forgeState <- atomically $ readTVar varForgeState
          forgeBlock cfg forgeState bno ledgerState txs proof
      }

-- | Block production in 'IOLike'
--
-- Unlike 'IO', 'IOLike' does not give us 'MonadRandom', and so we need to
-- simulate it.
blockProductionIOLike :: forall m blk.
                         (IOLike m, BlockSupportsProtocol blk, CanForge blk)
                      => TopLevelConfig blk
                      -> CanBeLeader (BlockProtocol blk)
                      -> MaintainForgeState (ChaChaT m) blk
                      -> StrictTVar m ChaChaDRG
                      -> (   ForgeState blk
                          -> BlockNo
                          -> TickedLedgerState blk
                          -> [GenTx blk]
                          -> IsLeader (BlockProtocol blk)
                          -> ChaChaT m blk)
                      -> m (BlockProduction m blk)
blockProductionIOLike cfg canBeLeader mfs varRNG forge = do
    varForgeState <- newTVarM (initForgeState mfs)
    let upd :: Update (ChaChaT m) (ForgeState blk)
        upd = hoistUpdate lift $ updateFromTVar varForgeState
    return $ BlockProduction {
        getLeaderProof = \tracer ledgerState consensusState ->
          simMonadRandom varRNG $
            defaultGetLeaderProof
              cfg
              canBeLeader
              mfs
              (traceUpdate (natTracer lift tracer) upd)
              ledgerState
              consensusState
      , produceBlock = \bno ledgerState txs proof -> do
          forgeState <- atomically $ readTVar varForgeState
          simMonadRandom varRNG $
            forge
              forgeState
              bno
              ledgerState
              txs
              proof
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
                      -> m (Maybe (IsLeader (BlockProtocol blk)))
defaultGetLeaderProof cfg proof mfs upd lgrSt consensusSt = do
    updateForgeState mfs upd (tickedSlotNo lgrSt)
    checkIsLeader
      (configConsensus cfg)
      proof
      lgrSt
      consensusSt
