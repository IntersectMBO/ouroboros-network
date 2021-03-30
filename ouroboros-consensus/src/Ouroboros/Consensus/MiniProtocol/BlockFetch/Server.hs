{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE EmptyDataDeriving         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( blockFetchServer
    -- * Trace events
  , TraceBlockFetchServerEvent(..)
    -- * Exceptions
  , BlockFetchServerException
  ) where

import           Control.Tracer (Tracer, traceWith)
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (..), BlockFetchSendBlocks (..),
                     BlockFetchServer (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

import           Ouroboros.Consensus.Storage.ChainDB (ChainDB,
                     IteratorResult (..), WithPoint (..),
                     getSerialisedBlockWithPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

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
        BlockGCed (RealPoint blk)

      -- | Thrown when requesting the genesis block from the database
      --
      -- Although the genesis block has a hash and a point associated with it,
      -- it does not actually exist other than as a concept; we cannot read and
      -- return it.
    | NoGenesisBlock

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
    -> NodeToNodeVersion
    -> ResourceRegistry m
    -> BlockFetchServer (Serialised blk) (Point blk) m ()
blockFetchServer tracer chainDB _version registry = senderSide
  where
    senderSide :: BlockFetchServer (Serialised blk) (Point blk) m ()
    senderSide = BlockFetchServer receiveReq' ()

    receiveReq' :: ChainRange (Point blk)
                -> m (BlockFetchBlockSender (Serialised blk) (Point blk) m ())
    receiveReq' (ChainRange start end) =
      case (start, end) of
        (BlockPoint s h, BlockPoint s' h') ->
          receiveReq (RealPoint s h) (RealPoint s' h')
        _otherwise ->
          throwIO NoGenesisBlock

    receiveReq :: RealPoint blk
               -> RealPoint blk
               -> m (BlockFetchBlockSender (Serialised blk) (Point blk) m ())
    receiveReq start end = do
      errIt <- ChainDB.stream
        chainDB
        registry
        getSerialisedBlockWithPoint
        (ChainDB.StreamFromInclusive start)
        (ChainDB.StreamToInclusive   end)
      return $ case errIt of
        -- The range is not in the ChainDB or it forks off more than @k@
        -- blocks back.
        Left  _  -> SendMsgNoBlocks $ return senderSide
        -- When we got an iterator, it will stream at least one block since
        -- its bounds are inclusive, so we don't have to check whether the
        -- iterator is empty.
        Right it -> SendMsgStartBatch $ sendBlocks it

    sendBlocks :: ChainDB.Iterator m blk (WithPoint blk (Serialised blk))
               -> m (BlockFetchSendBlocks (Serialised blk) (Point blk) m ())
    sendBlocks it = do
      next <- ChainDB.iteratorNext it
      case next of
        IteratorResult blk     -> do
          traceWith tracer $ TraceBlockFetchServerSendBlock $ point blk
          return $ SendMsgBlock (withoutPoint blk) (sendBlocks it)
        IteratorExhausted      -> do
          ChainDB.iteratorClose it
          return $ SendMsgBatchDone $ return senderSide
        IteratorBlockGCed pt -> do
          ChainDB.iteratorClose it
          throwIO $ BlockGCed @blk pt


{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Block Fetch Server.
data TraceBlockFetchServerEvent blk =
    -- | The server sent a block to the peer.
    TraceBlockFetchServerSendBlock !(Point blk)
  deriving (Eq, Show)
