{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
module Ouroboros.Consensus.BlockFetchServer
  ( blockFetchServer
    -- * Trace events
  , TraceBlockFetchServerEvent
    -- * Exceptions
  , BlockFetchServerException
  ) where

import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Ouroboros.Network.Block (HeaderHash, Serialised (..),
                     StandardHash, castPoint)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (..), BlockFetchSendBlocks (..),
                     BlockFetchServer (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Storage.ChainDB (ChainDB, Deserialisable (..),
                     IteratorResult (..))
import qualified Ouroboros.Storage.ChainDB as ChainDB

data BlockFetchServerException =
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
      forall blk. (Typeable blk, StandardHash blk) =>
        BlockGCed (HeaderHash blk)

deriving instance Show BlockFetchServerException

instance Exception BlockFetchServerException

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
    -> BlockFetchServer (Serialised blk) m ()
blockFetchServer _tracer chainDB registry = senderSide
  where
    senderSide :: BlockFetchServer (Serialised blk) m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange (Serialised blk)
               -> m (BlockFetchBlockSender (Serialised blk) m ())
    receiveReq (ChainRange start end) = do
      errIt <- ChainDB.streamBlocks
        chainDB
        registry
        (ChainDB.StreamFromInclusive (castPoint start))
        (ChainDB.StreamToInclusive   (castPoint end))
      return $ case errIt of
        -- The range is not in the ChainDB or it forks off more than @k@
        -- blocks back.
        Left  _  -> SendMsgNoBlocks $ return senderSide
        -- When we got an iterator, it will stream at least one block since
        -- its bounds are inclusive, so we don't have to check whether the
        -- iterator is empty.
        Right it -> SendMsgStartBatch $ sendBlocks it

    sendBlocks :: ChainDB.Iterator m (Deserialisable m blk blk)
               -> m (BlockFetchSendBlocks (Serialised blk) m ())
    sendBlocks it = do
      next <- ChainDB.iteratorNext it
      case next of
        IteratorResult blk     ->
          return $ SendMsgBlock (serialised blk) (sendBlocks it)
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
   -- TODO no events yet. Tracing the messages send/received over the network
   -- might be all we need?
  deriving (Eq, Show)
