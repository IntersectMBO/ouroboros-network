{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Ouroboros.Consensus.BlockFetchServer
  ( blockFetchServer
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Tracer (Tracer)

import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (..), BlockFetchSendBlocks (..),
                     BlockFetchServer (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))

import           Ouroboros.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB


-- | Block fetch server based on
-- 'Ouroboros.Network.BlockFetch.Examples.mockBlockFetchServer1', but using
-- the 'ChainDB'.
blockFetchServer
    :: forall m blk. (MonadSTM m, MonadThrow m)
    => Tracer m String
    -> ChainDB m blk
    -> BlockFetchServer blk m ()
blockFetchServer _tracer chainDB = senderSide
  where
    senderSide :: BlockFetchServer blk m ()
    senderSide = BlockFetchServer receiveReq ()

    receiveReq :: ChainRange blk
               -> m (BlockFetchBlockSender blk m ())
    receiveReq chainRange = withIter chainRange $ \it ->
      -- When we got an iterator, it will stream at least one block since its
      -- bounds are inclusive (when the bounds are invalid, we won't get an
      -- iterator and we reply with 'SendMsgNoBlocks'), so we don't have to
      -- check whether the iterator is empty (and reply with
      -- 'SendMsgNoBlocks').
      return $ SendMsgStartBatch $ sendBlocks it


    sendBlocks :: ChainDB.Iterator m blk
               -> m (BlockFetchSendBlocks blk m ())
    sendBlocks it = do
      next <- ChainDB.iteratorNext it
      return $ case next of
        ChainDB.IteratorExhausted  -> SendMsgBatchDone (return senderSide)
        ChainDB.IteratorResult blk -> SendMsgBlock blk (sendBlocks it)

    withIter :: ChainRange blk -> (ChainDB.Iterator m blk -> m a) -> m a
    withIter (ChainRange start end) = bracket
      -- TODO when streamBlocks throws an exception, we should return
      -- SendMsgNoBlocks. Which exception will the ChainDB throw?
        (ChainDB.streamBlocks
          chainDB
          (ChainDB.StreamFromInclusive start)
          (ChainDB.StreamToInclusive   end))
        ChainDB.iteratorClose
