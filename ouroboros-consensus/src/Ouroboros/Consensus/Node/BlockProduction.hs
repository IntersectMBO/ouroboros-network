{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , getLeaderProof
  , blockProductionIO
    -- * Get leader proof
  , defaultGetLeaderProof
  ) where

import           Control.Tracer (Tracer, traceWith)
import           Crypto.Random (MonadRandom)
import           GHC.Stack

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.Util.IOLike

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
      -- | Check if we should produce a block
      getLeaderProof_ :: HasCallStack
                      => Tracer m (ForgeState blk)
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
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> m blk
    }

getLeaderProof :: HasCallStack
               => BlockProduction m blk
               -> Tracer m (ForgeState blk)
               -> Ticked (LedgerView    (BlockProtocol blk))
               -> Ticked (ChainDepState (BlockProtocol blk))
               -> m (LeaderCheck        (BlockProtocol blk))
getLeaderProof = getLeaderProof_

blockProductionIO :: forall blk. (BlockSupportsProtocol blk, CanForge blk)
                  => TopLevelConfig blk
                  -> CanBeLeader (BlockProtocol blk)
                  -> MaintainForgeState IO blk
                  -> IO (BlockProduction IO blk)
blockProductionIO cfg canBeLeader mfs = do
    varForgeState <- newMVar (initForgeState mfs)
    return $ BlockProduction {
        getLeaderProof_ =
          defaultGetLeaderProof
            cfg
            canBeLeader
            mfs
            varForgeState
      , produceBlock = \bno ledgerState txs proof -> do
          forgeState <- readMVar varForgeState
          return $ forgeBlock cfg forgeState bno ledgerState txs proof
      }

{-------------------------------------------------------------------------------
  Get leader proof
-------------------------------------------------------------------------------}

defaultGetLeaderProof ::
     ( MonadSTM m
     , MonadCatch m
     , MonadRandom m
     , ConsensusProtocol (BlockProtocol blk)
     , HasCallStack
     )
  => TopLevelConfig blk
  -> CanBeLeader (BlockProtocol blk)
  -> MaintainForgeState m blk
  -> StrictMVar m (ForgeState blk)
  -> Tracer m (ForgeState blk)
  -> Ticked (LedgerView (BlockProtocol blk))
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (LeaderCheck (BlockProtocol blk))
defaultGetLeaderProof cfg proof mfs varForgeState tracer lgrSt chainDepSt = do
    forgeState' <- modifyMVar varForgeState $ \forgeState -> do
      forgeState' <-
        updateForgeState
          mfs
          (configIndep cfg)
          (tickedSlotNo lgrSt)
          forgeState
      return (forgeState', forgeState')
    traceWith tracer forgeState'
    checkIsLeader
      (configConsensus cfg)
      proof
      (chainIndepState forgeState')
      lgrSt
      chainDepSt
