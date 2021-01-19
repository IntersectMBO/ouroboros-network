{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
  , chainSyncHeaderServerReader
  , chainSyncBlockServerReader
  , Tip
    -- * Trace events
  , TraceChainSyncServerEvent (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Block (ChainUpdate (..), Serialised,
                     Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, Reader,
                     WithPoint (..), getSerialisedBlockWithPoint,
                     getSerialisedHeaderWithPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)


chainSyncHeaderServerReader
    :: ChainDB m blk
    -> ResourceRegistry m
    -> m (Reader m blk (WithPoint blk (SerialisedHeader blk)))
chainSyncHeaderServerReader chainDB registry = ChainDB.newReader chainDB registry getSerialisedHeaderWithPoint

chainSyncBlockServerReader
    :: ChainDB m blk
    -> ResourceRegistry m
    -> m (Reader m blk (WithPoint blk (Serialised blk)))
chainSyncBlockServerReader chainDB registry = ChainDB.newReader chainDB registry getSerialisedBlockWithPoint

-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
--
chainSyncHeadersServer
    :: forall m blk.
       ( IOLike m
       , HasHeader (Header blk)
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader m blk (WithPoint blk (SerialisedHeader blk))
    -> NodeToNodeVersion
    -> ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB rdr _version =
    ChainSyncServer $
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr in
      server

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
chainSyncBlocksServer
    :: forall m blk. (IOLike m, HasHeader (Header blk))
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader m blk (WithPoint blk (Serialised blk))
    -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB rdr =
    ChainSyncServer $
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr in
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
       ( IOLike m
       , HasHeader (Header blk)
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader  m blk (WithPoint blk b)
    -> ChainSyncServer b (Point blk) (Tip blk) m ()
chainSyncServerForReader tracer chainDB rdr =
    idle'
  where
    idle :: ServerStIdle b (Point blk) (Tip blk) m ()
    idle = ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = pure ()
      }

    idle' :: ChainSyncServer b (Point blk) (Tip blk) m ()
    idle' = ChainSyncServer $ return idle

    handleRequestNext :: m (Either (ServerStNext b (Point blk) (Tip blk) m ())
                                (m (ServerStNext b (Point blk) (Tip blk) m ())))
    handleRequestNext = ChainDB.readerInstruction rdr >>= \case
      Just update -> do
        tip <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerRead tip (point <$> update)
        Left <$> sendNext tip (withoutPoint <$> update)
      Nothing     -> return $ Right $ do
        -- Reader is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.readerInstructionBlocking rdr
        tip    <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerReadBlocked tip (point <$> update)
        sendNext tip (withoutPoint <$> update)

    sendNext :: Tip blk
             -> ChainUpdate blk b
             -> m (ServerStNext b (Point blk) (Tip blk) m ())
    sendNext tip update = case update of
      AddBlock hdr -> do
        traceWith tracer TraceChainSyncRollForward
        return $ SendMsgRollForward hdr tip idle'
      RollBack pt  -> do
        traceWith tracer TraceChainSyncRollBackward
        return $ SendMsgRollBackward pt tip idle'

    handleFindIntersect :: [Point blk]
                        -> m (ServerStIntersect b (Point blk) (Tip blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr points
      tip     <- atomically $ ChainDB.getCurrentTip chainDB
      case changed of
        Just pt -> do
          traceWith tracer TraceChainSyncIntersectFound
          return $ SendMsgIntersectFound pt tip idle'
        Nothing -> do
          traceWith tracer TraceChainSyncIntersectNotFound
          return $ SendMsgIntersectNotFound tip idle'

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Server.
--
-- The whole headers/blocks in the traced 'ChainUpdate' are substituted with
-- their corresponding 'Point'.
data TraceChainSyncServerEvent blk
  = TraceChainSyncServerRead        (Tip blk) (ChainUpdate blk (Point blk))
  | TraceChainSyncServerReadBlocked (Tip blk) (ChainUpdate blk (Point blk))
  | TraceChainSyncRollForward
  | TraceChainSyncRollBackward
  | TraceChainSyncIntersectFound
  | TraceChainSyncIntersectNotFound
  deriving (Eq, Show)
