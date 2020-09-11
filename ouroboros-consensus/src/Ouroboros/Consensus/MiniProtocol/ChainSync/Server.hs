{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
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
    -> NodeToNodeVersion
    -> ResourceRegistry m
    -> ChainSyncServer (SerialisedHeader blk) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB _version registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newReader chainDB registry getSerialisedHeaderWithPoint
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr
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
    -> ResourceRegistry m
    -> ChainSyncServer (Serialised blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newReader chainDB registry getSerialisedBlockWithPoint
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
       ( IOLike m
       , HeaderHash blk ~ HeaderHash b
       , HasHeader (Header blk)
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader  m blk (WithPoint blk b)
    -> ChainSyncServer b (Tip blk) m ()
chainSyncServerForReader tracer chainDB rdr =
    idle'
  where
    idle :: ServerStIdle b (Tip blk) m ()
    idle = ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = ChainDB.readerClose rdr
      }

    idle' :: ChainSyncServer b (Tip blk) m ()
    idle' = ChainSyncServer $ return idle

    handleRequestNext :: m (Either (ServerStNext b (Tip blk) m ())
                                (m (ServerStNext b (Tip blk) m ())))
    handleRequestNext = ChainDB.readerInstruction rdr >>= \case
      Just update -> do
        tip <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerRead tip (point <$> update)
        return $ Left $ sendNext tip (withoutPoint <$> update)
      Nothing     -> return $ Right $ do
        -- Reader is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.readerInstructionBlocking rdr
        tip    <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerReadBlocked tip (point <$> update)
        return $ sendNext tip (withoutPoint <$> update)

    sendNext :: Tip blk
             -> ChainUpdate blk b
             -> ServerStNext b (Tip blk) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip idle'
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip idle'

    handleFindIntersect :: [Point b]
                        -> m (ServerStIntersect b (Tip blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr (map castPoint points)
      tip     <- atomically $ ChainDB.getCurrentTip chainDB
      return $ case changed :: Maybe (Point blk) of
        Just pt -> SendMsgIntersectFound    (castPoint pt) tip idle'
        Nothing -> SendMsgIntersectNotFound tip idle'

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
  deriving (Eq, Show)
