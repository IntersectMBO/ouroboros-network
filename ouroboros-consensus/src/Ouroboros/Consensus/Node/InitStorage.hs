module Ouroboros.Consensus.Node.InitStorage (NodeInitStorage (..)) where

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB.Init (InitChainDB)
import           Ouroboros.Consensus.Storage.ImmutableDB (ChunkInfo)

-- | Functionality needed to initialise the storage layer of the node.
class NodeInitStorage blk where
  -- | The 'ChunkInfo' to use for the ImmutableDB, i.e., how many slots to put
  -- together in a single chunk file.
  --
  -- For example, for Byron, one would use the epoch size.
  nodeImmutableDbChunkInfo :: StorageConfig blk -> ChunkInfo

  -- | Check the integrity of a block, i.e., that it has not been corrupted by
  -- a bitflip.
  --
  -- Check this by, e.g., verifying whether the block has a valid signature
  -- and that the hash of the body matches the body hash stores in the header.
  --
  -- This does not check the validity of the contents of the block, e.g.,
  -- whether the transactions are valid w.r.t. the ledger, or whether it's
  -- sent by a malicious node.
  nodeCheckIntegrity :: StorageConfig blk -> blk -> Bool

  -- | This function is called when starting up the node, right after the
  -- ChainDB was opened, and before we connect to other nodes and start block
  -- production.
  --
  -- This function can be used to, for example, create the genesis EBB in case
  -- the chain(DB) is empty.
  --
  -- We only provide a limited interface to the chain DB. This is primarily
  -- useful for the definition of combinators (which may need to turn a
  -- 'InitChainDB' for one type of block into an 'InitChainDB' for a closely
  -- related type of block).
  nodeInitChainDB :: IOLike m => StorageConfig blk -> InitChainDB m blk -> m ()
  nodeInitChainDB _ _ = return ()
