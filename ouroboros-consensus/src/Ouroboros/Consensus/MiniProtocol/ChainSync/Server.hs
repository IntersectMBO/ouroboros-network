{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncBlockServerFollower
  , Tip
    -- * Trace events
  , TraceChainSyncServerEvent (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Block (ChainUpdate (..), Serialised,
                     Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Consensus.Storage.ChainDB.API (ChainDB, Follower,
                     WithPoint (..), getSerialisedBlockWithPoint,
                     getSerialisedHeaderWithPoint)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)


chainSyncHeaderServerFollower
    :: ChainDB m blk
    -> ResourceRegistry m
    -> m (Follower m blk (WithPoint blk (SerialisedHeader blk)))
chainSyncHeaderServerFollower chainDB registry = ChainDB.newFollower chainDB registry getSerialisedHeaderWithPoint

chainSyncBlockServerFollower
    :: ChainDB m blk
    -> ResourceRegistry m
    -> m (Follower m blk (WithPoint blk (Serialised blk)))
chainSyncBlockServerFollower chainDB registry = ChainDB.newFollower chainDB registry getSerialisedBlockWithPoint

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
    -> Follower m blk (WithPoint blk (SerialisedHeader blk))
    -> NodeToNodeVersion
    -> ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB flr _version =
    ChainSyncServer $
      let ChainSyncServer server = chainSyncServerForFollower tracer chainDB flr in
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
    -> Follower m blk (WithPoint blk (Serialised blk))
    -> ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB flr =
    ChainSyncServer $
      let ChainSyncServer server = chainSyncServerForFollower tracer chainDB flr in
      server

-- | A chain sync server.
--
-- This is a version of
-- 'Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample' that
-- uses a 'chainDB' and a 'Follower' instead of
-- 'Ourboros.Network.ChainProducerState.ChainProducerState'.
--
-- All the hard work is done by the 'Follower's provided by the 'ChainDB'.
--
chainSyncServerForFollower ::
     forall m blk b.
     ( IOLike m
     , HasHeader (Header blk)
     )
  => Tracer m (TraceChainSyncServerEvent blk)
  -> ChainDB m blk
  -> Follower  m blk (WithPoint blk b)
  -> ChainSyncServer b (Point blk) (Tip blk) m ()
chainSyncServerForFollower tracer chainDB flr =
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
    handleRequestNext = ChainDB.followerInstruction flr >>= \case
      Just update -> do
        tip <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerRead tip (point <$> update)
        return $ Left $ sendNext tip (withoutPoint <$> update)
      Nothing     -> return $ Right $ do
        -- Follower is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.followerInstructionBlocking flr
        tip    <- atomically $ ChainDB.getCurrentTip chainDB
        traceWith tracer $
          TraceChainSyncServerReadBlocked tip (point <$> update)
        return $ sendNext tip (withoutPoint <$> update)

    sendNext :: Tip blk
             -> ChainUpdate blk b
             -> ServerStNext b (Point blk) (Tip blk) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip idle'
      RollBack pt  -> SendMsgRollBackward pt tip idle'

    handleFindIntersect :: [Point blk]
                        -> m (ServerStIntersect b (Point blk) (Tip blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.followerForward flr points
      tip     <- atomically $ ChainDB.getCurrentTip chainDB
      return $ case changed of
        Just pt -> SendMsgIntersectFound    pt tip idle'
        Nothing -> SendMsgIntersectNotFound    tip idle'

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
