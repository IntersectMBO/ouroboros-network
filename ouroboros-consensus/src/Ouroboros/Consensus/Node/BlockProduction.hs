{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , getLeaderProof
  , defaultBlockProduction
  , customForgeBlockProduction
    -- * Get leader proof
  , defaultGetLeaderProof
  ) where

import           Control.Tracer (Tracer, traceWith)
import           GHC.Stack

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util ((.....:))
import           Ouroboros.Consensus.Util.IOLike

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
      -- | Check if we should produce a block
      getLeaderProof_ :: HasCallStack
                      => Tracer m (ForgeState blk)
                      -> SlotNo
                      -> Ticked (LedgerView    (BlockProtocol blk))
                      -> Ticked (ChainDepState (BlockProtocol blk))
                      -> m (LeaderCheck        (BlockProtocol blk))

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
                   -> SlotNo                -- Current slot number
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> m blk
    }

getLeaderProof :: HasCallStack
               => BlockProduction m blk
               -> Tracer m (ForgeState blk)
               -> SlotNo
               -> Ticked (LedgerView    (BlockProtocol blk))
               -> Ticked (ChainDepState (BlockProtocol blk))
               -> m (LeaderCheck        (BlockProtocol blk))
getLeaderProof = getLeaderProof_

defaultBlockProduction ::
     forall m blk.
     ( BlockSupportsProtocol blk
     , CanForge blk
     , IOLike m
     )
  => TopLevelConfig blk
  -> CanBeLeader (BlockProtocol blk)
  -> MaintainForgeState m blk
  -> m (BlockProduction m blk)
defaultBlockProduction cfg canBeLeader mfs =
    customForgeBlockProduction cfg canBeLeader mfs forge
  where
    forge = return .....: forgeBlock cfg

-- | Variant of 'defaultBlockProduction' that allows overriding the function
-- to forge a block.
--
-- This is used in the ThreadNet tests to create EBBs JIT.
customForgeBlockProduction ::
     forall m blk.
     ( BlockSupportsProtocol blk
     , NoUnexpectedThunks (ExtraForgeState blk)
     , IOLike m
     )
  => TopLevelConfig blk
  -> CanBeLeader (BlockProtocol blk)
  -> MaintainForgeState m blk
  -> (   ForgeState blk
      -> BlockNo
      -> SlotNo
      -> TickedLedgerState blk
      -> [GenTx blk]
      -> IsLeader (BlockProtocol blk)
      -> m blk)
  -> m (BlockProduction m blk)
customForgeBlockProduction cfg canBeLeader mfs forge = do
    varForgeState <- newMVar (initForgeState mfs)
    return $ BlockProduction {
        getLeaderProof_ =
          defaultGetLeaderProof
            cfg
            canBeLeader
            mfs
            varForgeState
      , produceBlock = \bno sno ledgerState txs proof -> do
          forgeState <- readMVar varForgeState
          forge
            forgeState
            bno
            sno
            ledgerState
            txs
            proof
      }

{-------------------------------------------------------------------------------
  Get leader proof
-------------------------------------------------------------------------------}

defaultGetLeaderProof ::
     ( MonadSTM m
     , MonadCatch m
     , ConsensusProtocol (BlockProtocol blk)
     , HasCallStack
     )
  => TopLevelConfig blk
  -> CanBeLeader (BlockProtocol blk)
  -> MaintainForgeState m blk
  -> StrictMVar m (ForgeState blk)
  -> Tracer m (ForgeState blk)
  -> SlotNo
  -> Ticked (LedgerView (BlockProtocol blk))
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (LeaderCheck (BlockProtocol blk))
defaultGetLeaderProof cfg proof mfs varForgeState tracer slot lgrSt chainDepSt = do
    forgeState' <- modifyMVar varForgeState $ \forgeState -> do
      forgeState' <-
        updateForgeState
          mfs
          (configIndep cfg)
          slot
          forgeState
      return (forgeState', forgeState')
    traceWith tracer forgeState'
    return $ checkIsLeader
               (configConsensus cfg)
               proof
               (chainIndepState forgeState')
               slot
               lgrSt
               chainDepSt
