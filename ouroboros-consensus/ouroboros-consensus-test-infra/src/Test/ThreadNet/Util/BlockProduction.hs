{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.ThreadNet.Util.BlockProduction (
    blockProductionIOLike
  ) where

import           Control.Monad.Trans (lift)
import           Control.Tracer (natTracer)

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
blockProductionIOLike ::
     forall m blk.
     ( IOLike m
     , BlockSupportsProtocol blk
     , NoUnexpectedThunks (ExtraForgeState blk)
     )
  => TopLevelConfig blk
  -> CanBeLeader (BlockProtocol blk)
  -> MaintainForgeState m blk
  -> StrictTVar m ChaChaDRG
  -> (   ForgeState blk
      -> BlockNo
      -> TickedLedgerState blk
      -> [GenTx blk]
      -> IsLeader (BlockProtocol blk)
      -> m blk)
      -- ^ We don't use 'forgeBlock' directly but a custom function, used to
      -- create EBBs JIT.
  -> m (BlockProduction m blk)
blockProductionIOLike cfg canBeLeader mfs varRNG forge = do
    varForgeState :: StrictMVar m (ForgeState blk) <- newMVar (initForgeState mfs)
    return $ BlockProduction {
        getLeaderProof = \tracer ledgerState chainDepState ->
          simMonadRandom varRNG $
            defaultGetLeaderProof
              cfg
              canBeLeader
              (hoistMaintainForgeState lift mfs)
              (castStrictMVar varForgeState)
              (natTracer lift tracer)
              ledgerState
              chainDepState
      , produceBlock = \bno ledgerState txs proof -> do
          forgeState <- readMVar varForgeState
          forge
            forgeState
            bno
            ledgerState
            txs
            proof
      }
