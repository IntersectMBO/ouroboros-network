{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.Node.State (
    NodeState
    -- * Infrastructure for dealing with state updates
  , Update(..)
  , updateFromTVar
  , liftUpdate
  ) where

import           Data.Bifunctor (first)

import           Ouroboros.Consensus.Util.IOLike

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

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | (Chain-independent) node state required to run the protocol
type family NodeState blk :: *
