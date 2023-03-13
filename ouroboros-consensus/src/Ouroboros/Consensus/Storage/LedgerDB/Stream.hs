{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Storage.LedgerDB.Stream (
    NextBlock
  , NextItem (..)
  , StreamAPI (..)
  , streamAll
  ) where

import           Control.Monad.Except
import           GHC.Stack

import           Ouroboros.Consensus.Block

{-------------------------------------------------------------------------------
  Abstraction over the streaming API provided by the Chain DB
-------------------------------------------------------------------------------}

{-# DEPRECATED NextBlock ["Use Ouroboros.Consensus.Storage.LedgerDB (NextItem(NextItem))", "or Ouroboros.Consensus.Storage.LedgerDB (NextItem)"] #-}
-- | Next block returned during streaming
data NextItem blk = NoMoreItems | NextItem blk | NextBlock blk
type NextBlock blk = NextItem blk


-- | Stream blocks from the immutable DB
--
-- When we initialize the ledger DB, we try to find a snapshot close to the
-- tip of the immutable DB, and then stream blocks from the immutable DB to its
-- tip to bring the ledger up to date with the tip of the immutable DB.
--
-- In CPS form to enable the use of 'withXYZ' style iterator init functions.
newtype StreamAPI m blk a = StreamAPI {
      -- | Start streaming after the specified block
      streamAfter :: forall b. HasCallStack
        => Point blk
        -- Reference to the block corresponding to the snapshot we found
        -- (or 'GenesisPoint' if we didn't find any)

        -> (Either (RealPoint blk) (m (NextItem a)) -> m b)
        -- Get the next block (by value)
        --
        -- Should be @Left pt@ if the snapshot we found is more recent than the
        -- tip of the immutable DB. Since we only store snapshots to disk for
        -- blocks in the immutable DB, this can only happen if the immutable DB
        -- got truncated due to disk corruption. The returned @pt@ is a
        -- 'RealPoint', not a 'Point', since it must always be possible to
        -- stream after genesis.
        -> m b
    }

-- | Stream all blocks
streamAll ::
     forall m blk e b a. (Monad m, HasCallStack)
  => StreamAPI m blk b
  -> Point blk             -- ^ Starting point for streaming
  -> (RealPoint blk -> e)  -- ^ Error when tip not found
  -> a                     -- ^ Starting point when tip /is/ found
  -> (b -> a -> m a)       -- ^ Update function for each block
  -> ExceptT e m a
streamAll StreamAPI{..} tip notFound e f = ExceptT $
    streamAfter tip $ \case
      Left tip' -> return $ Left (notFound tip')

      Right getNext -> do
        let go :: a -> m a
            go a = do mNext <- getNext
                      case mNext of
                        NoMoreItems -> return a
                        NextItem b  -> go =<< f b a
                        -- This is here only to silence the non-exhaustiveness
                        -- check but it will never be matched
                        NextBlock b -> go =<< f b a
        Right <$> go e
