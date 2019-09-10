{-# LANGUAGE EmptyDataDeriving   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncServer
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
    -- * Trace events
  , TraceChainSyncServerEvent
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Ouroboros.Network.Block (ChainUpdate (..), HeaderHash, Point (..), castPoint)
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)


-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
--
chainSyncHeadersServer
    :: forall m blk. MonadSTM m
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> ResourceRegistry m
    -> ChainSyncServer (Header blk) (Point (Header blk)) m ()
chainSyncHeadersServer tracer chainDB registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newHeaderReader chainDB registry
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr
      server

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
chainSyncBlocksServer
    :: forall m blk. MonadSTM m
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> ResourceRegistry m
    -> ChainSyncServer blk (Point blk) m ()
chainSyncBlocksServer tracer chainDB registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newBlockReader chainDB registry
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr
      server


-- | A chain sync server.
--
-- This is a version of
-- 'Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample' that
-- uses a 'chainDB' and a 'Reader' instead of
-- 'Ourboros.Network.ChainProducerState.ChainProducerState'.
--
-- All the hard work is done by the 'Reader's provided by the 'ChainDB'.
--
chainSyncServerForReader
    :: forall m blk b.
       ( MonadSTM m
       , HeaderHash blk ~ HeaderHash b
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader  m blk b
    -> ChainSyncServer b (Point b) m ()
chainSyncServerForReader _tracer chainDB rdr =
    idle'
  where
    idle :: ServerStIdle b (Point b) m ()
    idle =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = ChainDB.readerClose rdr
      }

    idle' :: ChainSyncServer b (Point b) m ()
    idle' = ChainSyncServer (return idle)

    handleRequestNext :: m (Either (ServerStNext b (Point b) m ())
                                (m (ServerStNext b (Point b) m ())))
    handleRequestNext = ChainDB.readerInstruction rdr >>= \case
      Just update -> do
        tip <- castPoint <$> atomically (ChainDB.getTipPoint chainDB)
        return $ Left $ sendNext tip update
      Nothing     -> return $ Right $ do
        -- Reader is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.readerInstructionBlocking rdr
        tip    <- castPoint <$> atomically (ChainDB.getTipPoint chainDB)
        return $ sendNext tip update

    sendNext :: Point b
             -> ChainUpdate blk b
             -> ServerStNext b (Point b) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip idle'
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip idle'

    handleFindIntersect :: [Point b]
                        -> m (ServerStIntersect b (Point b) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr (map castPoint points)
      tip     <- castPoint <$> atomically (ChainDB.getTipPoint chainDB)
      return $ case changed :: Maybe (Point blk) of
        Just pt -> SendMsgIntersectFound    (castPoint pt) tip idle'
        Nothing -> SendMsgIntersectNotFound tip idle'

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Server.
data TraceChainSyncServerEvent blk
   -- TODO no events yet. Tracing the messages send/received over the network
   -- might be all we need?
  deriving (Eq, Show)
