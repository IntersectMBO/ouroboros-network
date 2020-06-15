{-# LANGUAGE ScopedTypeVariables #-}
module Test.ThreadNet.Util.BlockProduction (
    blockProductionIOLike
  ) where

import           Control.Monad.Trans (lift)
import           Control.Tracer (natTracer)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Node.BlockProduction
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

import           Test.Util.Random

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
    varForgeState <- newMVar (initForgeState mfs)
    let upd :: Update (ChaChaT m) (ForgeState blk)
        upd = updateFromMVar (castStrictMVar varForgeState)
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
          forgeState <- readMVar varForgeState
          simMonadRandom varRNG $
            forge
              forgeState
              bno
              ledgerState
              txs
              proof
      }
