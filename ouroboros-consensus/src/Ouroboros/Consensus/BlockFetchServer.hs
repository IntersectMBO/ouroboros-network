{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Ouroboros.Consensus.BlockFetchServer
  ( blockFetchServer
  ) where

import           Data.Typeable (Typeable)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Ouroboros.Network.Block (HeaderHash)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (..), BlockFetchSendBlocks (..),
                     BlockFetchServer (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Ouroboros.Storage.ChainDB (ChainDB, IteratorResult (..),
                     UnknownRange)
import qualified Ouroboros.Storage.ChainDB as ChainDB

data BlockFetchServerException blk =
      -- | A block that was supposed to be included in a batch was garbage
      -- collected since we started the batch and can no longer be sent.
      --
      -- This will very rarely happen: only when streaming some old fork. This
      -- will not happen when streaming blocks from the current chain (old or
      -- new) or a recent fork.
      BlockGCed (HeaderHash blk)

deriving instance (Show (HeaderHash blk))
    => Show (BlockFetchServerException blk)

instance (Typeable blk, Show (HeaderHash blk))
    => Exception (BlockFetchServerException blk)

-- | Block fetch server based on
-- 'Ouroboros.Network.BlockFetch.Examples.mockBlockFetchServer1', but using
-- the 'ChainDB'.
blockFetchServer
    :: forall m hdr blk.
       ( MonadSTM   m
       , MonadThrow m
       , HeaderHash hdr ~ HeaderHash blk
       , Typeable blk
       , Show (HeaderHash blk)
       )
    => Tracer m String
    -> ChainDB m blk hdr
    -> BlockFetchServer blk m ()
blockFetchServer _tracer chainDB = senderSide
  where
    senderSide :: BlockFetchServer blk m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange blk
               -> m (BlockFetchBlockSender blk m ())
    receiveReq chainRange = withIter chainRange $ \errIt ->
      return $ case errIt of
        -- The range is not in the ChainDB or it forks off more than @k@
        -- blocks back.
        Left  _  -> SendMsgNoBlocks $ return senderSide
        -- When we got an iterator, it will stream at least one block since
        -- its bounds are inclusive, so we don't have to check whether the
        -- iterator is empty.
        Right it -> SendMsgStartBatch $ sendBlocks it


    sendBlocks :: ChainDB.Iterator m blk
               -> m (BlockFetchSendBlocks blk m ())
    sendBlocks it = do
      next <- ChainDB.iteratorNext it
      case next of
        IteratorExhausted      -> return $ SendMsgBatchDone $ return senderSide
        IteratorResult blk     -> return $ SendMsgBlock blk (sendBlocks it)
        IteratorBlockGCed hash -> throwM $ BlockGCed @blk hash

    withIter :: ChainRange blk
             -> (Either (UnknownRange blk) (ChainDB.Iterator m blk) -> m a)
             -> m a
    withIter (ChainRange start end) = bracket
      -- TODO when streamBlocks throws an exception, we should return
      -- SendMsgNoBlocks. Which exception will the ChainDB throw?
        (ChainDB.streamBlocks
          chainDB
          (ChainDB.StreamFromInclusive start)
          (ChainDB.StreamToInclusive   end))
        (mapM_ ChainDB.iteratorClose)
