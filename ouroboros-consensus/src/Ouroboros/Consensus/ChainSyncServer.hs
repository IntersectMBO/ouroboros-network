{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncServer
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
  , Tip
    -- * Trace events
  , TraceChainSyncServerEvent (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Block (ChainUpdate (..), HeaderHash,
                     Point (..), Serialised, Tip (..), castPoint)
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (ChainDB, Reader,
                     SerialisedWithPoint (..), getSerialisedBlockWithPoint,
                     getSerialisedHeaderWithPoint)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)

-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
--
chainSyncHeadersServer
    :: forall m blk. IOLike m
    => Tracer m (TraceChainSyncServerEvent blk (Header blk))
    -> ChainDB m blk
    -> ResourceRegistry m
    -> ChainSyncServer (Serialised (Header blk)) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB registry =
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
    :: forall m blk. IOLike m
    => Tracer m (TraceChainSyncServerEvent blk blk)
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
       )
    => Tracer m (TraceChainSyncServerEvent blk b)
    -> ChainDB m blk
    -> Reader  m blk (SerialisedWithPoint blk b)
    -> ChainSyncServer (Serialised b) (Tip blk) m ()
chainSyncServerForReader tracer chainDB rdr =
    idle'
  where
    idle :: ServerStIdle (Serialised b) (Tip blk) m ()
    idle = ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = ChainDB.readerClose rdr
      }

    idle' :: ChainSyncServer (Serialised b) (Tip blk) m ()
    idle' = ChainSyncServer $ return idle

    handleRequestNext :: m (Either (ServerStNext (Serialised b) (Tip blk) m ())
                                (m (ServerStNext (Serialised b) (Tip blk) m ())))
    handleRequestNext = ChainDB.readerInstruction rdr >>= \case
      Just update -> do
        tip <- getTip
        traceWith tracer $
          TraceChainSyncServerRead tip (point <$> update)
        return $ Left $ sendNext tip (serialised <$> update)
      Nothing     -> return $ Right $ do
        -- Reader is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.readerInstructionBlocking rdr
        tip    <- getTip
        traceWith tracer $
          TraceChainSyncServerReadBlocked tip (point <$> update)
        return $ sendNext tip (serialised <$> update)

    sendNext :: Tip blk
             -> ChainUpdate blk (Serialised b)
             -> ServerStNext (Serialised b) (Tip blk) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip idle'
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip idle'

    handleFindIntersect :: [Point (Serialised b)]
                        -> m (ServerStIntersect (Serialised b) (Tip blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr (map castPoint points)
      tip     <- getTip
      return $ case changed :: Maybe (Point blk) of
        Just pt -> SendMsgIntersectFound    (castPoint pt) tip idle'
        Nothing -> SendMsgIntersectNotFound tip idle'

    getTip :: m (Tip blk)
    getTip = atomically $ do
      tipPoint   <- castPoint <$> ChainDB.getTipPoint   chainDB
      tipBlockNo <-               ChainDB.getTipBlockNo chainDB
      return Tip { tipPoint, tipBlockNo }

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Server.
--
-- The whole headers/blocks in the traced 'ChainUpdate' are substituted with
-- their corresponding 'Point'. The @b@ parameter is left as a phantom type
-- parameter to avoid confusing header with block.
data TraceChainSyncServerEvent blk b
  = TraceChainSyncServerRead        (Tip blk) (ChainUpdate blk (Point blk))
  | TraceChainSyncServerReadBlocked (Tip blk) (ChainUpdate blk (Point blk))
  deriving (Eq, Show)
