{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
module Ouroboros.Consensus.Block.Forge (
    CanForge (..)
    -- * Infrastructure for dealing with state updates
  , Update(..)
  , updateFromTVar
  , liftUpdate
  ) where

import           Crypto.Random (MonadRandom)
import           Data.Bifunctor (first)


import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Block

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  CanForge
-------------------------------------------------------------------------------}

class NoUnexpectedThunks (ForgeState blk) => CanForge blk where

  -- | (Chain-independent) state required to forge blocks
  type ForgeState blk :: *

  -- Default to ()
  type ForgeState blk = ()

  -- | Forge a new block
  forgeBlock
    :: MonadRandom m
    => TopLevelConfig blk
    -> Update m (ForgeState blk)
    -> BlockNo                -- ^ Current block number
    -> TickedLedgerState blk  -- ^ Current ledger
    -> [GenTx blk]            -- ^ Txs to add in the block
    -> IsLeader (BlockProtocol blk)
    -> m blk

{-------------------------------------------------------------------------------
  Updating the state
-------------------------------------------------------------------------------}

-- | Update a stateful value
newtype Update m a = Update {
      -- | Update the value, and produce a result
      --
      -- If 'Nothing', the action will be retried.
      runUpdate :: forall b. (a -> Maybe (a, b)) -> m b
    }

updateFromTVar :: MonadSTM m => StrictTVar m a -> Update m a
updateFromTVar var = Update $ \f -> atomically $ do
    a <- readTVar var
    case f a of
      Nothing      -> retry
      Just (a', b) -> writeTVar var a' >> return b

liftUpdate :: (large -> small)
           -> (small -> large -> large)
           -> Update m large
           -> Update m small
liftUpdate get set (Update update) = Update $ \f ->
    update $ \large ->
      first (flip set large) <$> (f (get large))
