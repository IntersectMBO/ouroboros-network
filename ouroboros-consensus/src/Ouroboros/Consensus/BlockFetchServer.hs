{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Ouroboros.Consensus.BlockFetchServer
  ( blockFetchServer
    -- * Trace events
  , TraceBlockFetchServerEvent
    -- * Exceptions
  , BlockFetchServerException
  ) where

import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer, traceWith)

import           Ouroboros.Network.Block (HeaderHash, Point, StandardHash)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (..), BlockFetchSendBlocks (..),
                     BlockFetchServer (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Storage.ChainDB (ChainDB, IteratorResult (..))
import qualified Ouroboros.Storage.ChainDB as ChainDB

data BlockFetchServerException blk =
      -- | A block that was supposed to be included in a batch was garbage
      -- collected since we started the batch and can no longer be sent.
      --
      -- This will very rarely happen, only in the following scenario: when
      -- the batch started, the requested blocks were on the current chain,
      -- but then the current chain changed such that the requested blocks are
      -- now on a fork. If while requesting the blocks from the batch, there
      -- were a pause of /hours/ such that the fork gets older than @k@, then
      -- the next request after this long pause could result in this
      -- exception, as the block to stream from the old fork could have been
      -- garbage collected. However, the network protocol will have timed out
      -- long before this happens.
      BlockGCed (HeaderHash blk)

deriving instance StandardHash blk => Show (BlockFetchServerException blk)

instance (Typeable blk, StandardHash blk)
      => Exception (BlockFetchServerException blk)

-- | Block fetch server based on
-- 'Ouroboros.Network.BlockFetch.Examples.mockBlockFetchServer1', but using
-- the 'ChainDB'.
blockFetchServer
    :: forall m blk.
       ( IOLike m
       , StandardHash blk
       , Typeable     blk
       )
    => Tracer m (TraceBlockFetchServerEvent blk)
    -> ChainDB m blk
    -> ResourceRegistry m
    -> BlockFetchServer blk m ()
blockFetchServer tracer chainDB registry = senderSide
  where
    senderSide :: BlockFetchServer blk m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange blk
               -> m (BlockFetchBlockSender blk m ())
    receiveReq (ChainRange start end) = do
      errIt <- ChainDB.streamBlocks
        chainDB
        registry
        (ChainDB.StreamFromInclusive start)
        (ChainDB.StreamToInclusive   end)
      case errIt of
        -- The range is not in the ChainDB or it forks off more than @k@
        -- blocks back.
        Left  _  -> do
          traceWith tracer (TraceNoBlocks start end)
          pure $ SendMsgNoBlocks $ return senderSide
        -- When we got an iterator, it will stream at least one block since
        -- its bounds are inclusive, so we don't have to check whether the
        -- iterator is empty.
        Right it -> pure $ SendMsgStartBatch $ sendBlocks it


    sendBlocks :: ChainDB.Iterator m blk
               -> m (BlockFetchSendBlocks blk m ())
    sendBlocks it = do
      next <- ChainDB.iteratorNext it
      case next of
        IteratorResult blk     -> return $ SendMsgBlock blk (sendBlocks it)
        IteratorExhausted      -> do
          ChainDB.iteratorClose it
          return $ SendMsgBatchDone $ return senderSide
        IteratorBlockGCed hash -> do
          ChainDB.iteratorClose it
          throwM $ BlockGCed @blk hash


{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Block Fetch Server.
data TraceBlockFetchServerEvent blk
  = TraceNoBlocks (Point blk) (Point blk)
    -- ^ the server is about to send 'MsgNoBlocks' because it could not obtain
    -- an iterator for these two points
    --
    -- start (inclusive), stop (inclusive)
  deriving (Eq, Show)
