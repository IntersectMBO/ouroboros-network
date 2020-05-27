{-# LANGUAGE RankNTypes #-}

module Ouroboros.Consensus.Node.BlockProduction (
    BlockProduction(..)
  , defaultBlockProduction
  ) where

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

-- | Stateful wrapper around block production
data BlockProduction m blk = BlockProduction {
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
      produceBlock :: forall n. MonadRandom n
                   => (forall a. m a -> n a)
                   -- Lift actions into @n@
                   --
                   -- This allows block production to execute arbitrary side
                   -- effects; this is primarily useful for tests.

                   -> Update n (ForgeState blk)
                   -> BlockNo               -- Current block number
                   -> TickedLedgerState blk -- Current ledger state
                   -> [GenTx blk]           -- Contents of the mempool
                   -> IsLeader (BlockProtocol blk) -- Proof we are leader
                   -> n blk

      -- | How to run a computation requiring 'MonadRandom'.
      --
      -- When @m = IO@, this can be 'runMonadRandomIO', because the
      -- 'MonadRandom' instance for 'IO' can be used.
      --
      -- In the tests, we can simulate a 'MonadRandom' by keeping track of a
      -- DRG in a 'TVar'.
    , runMonadRandomDict :: RunMonadRandom m
    }

defaultBlockProduction :: CanForge blk
                       => TopLevelConfig blk -> BlockProduction IO blk
defaultBlockProduction cfg = BlockProduction {
      produceBlock       = \_lift' -> forgeBlock cfg
    , runMonadRandomDict = runMonadRandomIO
    }
