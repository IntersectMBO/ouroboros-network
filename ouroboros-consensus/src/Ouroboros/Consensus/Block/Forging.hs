{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Block.Forging (
    CannotForge
  , ForgeStateInfo
  , BlockForging(..)
  , getLeaderProof
  ) where

import           Control.Tracer (Tracer, traceWith)
import           GHC.Stack

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Ticked


-- | Information about why we /cannot/ forge a block, although we are a leader
--
-- This should happen only rarely. An example might be that our hot key
-- does not (yet/anymore) match the delegation state.
type family CannotForge blk :: *

-- | Info traced after every call to 'updateForgeState', i.e., at the start of
-- each slot.
type family ForgeStateInfo blk :: *

-- | Stateful wrapper around block production
--
-- NOTE: do not refer to the consensus or ledger config in the closure of this
-- record because they might contain an @EpochInfo Identity@, which will be
-- incorrect when used as part of the hard fork combinator.
data BlockForging m blk = BlockForging {
      -- | Proof that the node can be a leader
      --
      -- NOTE: the other fields of this record may refer to this value (or a
      -- value derived from it) in their closure, which means one should not
      -- override this field independently from the others.
      canBeLeader :: CanBeLeader (BlockProtocol blk)

      -- | Update the forge state.
      --
      -- When the node can be a leader, this will be called at the start of
      -- each slot, right before calling 'checkCanForge'.
      --
      -- The returned info is traced.
    , updateForgeState :: SlotNo -> m (ForgeStateInfo blk)

      -- | After checking that the node indeed is a leader ('checkIsLeader'
      -- returned 'Just'), do another check to see whether we can actually
      -- forge a block. It might not be possible, e.g., because the KES key is
      -- out of date.
      --
      -- When this is the case, we trace the reason and don't call
      -- 'forgeBlock'.
    , checkCanForge ::
           forall p. BlockProtocol blk ~ p
        => TopLevelConfig blk
        -> SlotNo
        -> Ticked (ChainDepState p)
        -> IsLeader p
        -> m (Maybe (CannotForge blk))

      -- | Forge a block
      --
      -- The function is passed the contents of the mempool; this is a set of
      -- transactions that is guaranteed to be consistent with the ledger state
      -- (also provided as an argument) and with each other (when applied in
      -- order). In principle /all/ of them could be included in the block (up
      -- to maximum block size).
      --
      -- NOTE: do not refer to the consensus or ledger config in the closure,
      -- because they might contain an @EpochInfo Identity@, which will be
      -- incorrect when used as part of the hard fork combinator. Use the
      -- given 'TopLevelConfig' instead, as it is guaranteed to be correct
      -- even when used as part of the hard fork combinator.
    , forgeBlock ::
           TopLevelConfig blk
        -> BlockNo                      -- Current block number
        -> SlotNo                       -- Current slot number
        -> TickedLedgerState blk        -- Current ledger state
        -> [GenTx blk]                  -- Contents of the mempool
        -> IsLeader (BlockProtocol blk) -- Proof we are leader
        -> m blk
    }

getLeaderProof ::
     forall m blk.
     ( Monad m
     , ConsensusProtocol (BlockProtocol blk)
     , HasCallStack
     )
  => BlockForging m blk
  -> Tracer m (ForgeStateInfo blk)
  -> Tracer m (CannotForge blk)
  -> TopLevelConfig blk
  -> SlotNo
  -> Ticked (ChainDepState (BlockProtocol blk))
  -> m (Maybe (IsLeader (BlockProtocol blk)))
getLeaderProof BlockForging{..}
               forgeStateInfoTracer
               cannotForgeTracer
               cfg
               slot
               tickedChainDepState = do
    forgeStateInfo <- updateForgeState slot
    traceWith forgeStateInfoTracer forgeStateInfo

    case checkIsLeader (configConsensus cfg) canBeLeader slot tickedChainDepState of
      Nothing       -> return Nothing
      Just isLeader -> do
        mCannotForge <- checkCanForge cfg slot tickedChainDepState isLeader
        case mCannotForge of
          Nothing          -> return $ Just isLeader
          Just cannotForge -> do
            traceWith cannotForgeTracer cannotForge
            return Nothing
