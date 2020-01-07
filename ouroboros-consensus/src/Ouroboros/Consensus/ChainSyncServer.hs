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
                     Point (..), Serialised, Tip (..), WithBlockSize (..),
                     castPoint)
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (BlockComponent (..), ChainDB,
                     Reader, SerialisedWithPoint (..),
                     getSerialisedBlockWithPoint, getSerialisedHeaderWithPoint)
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
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> ResourceRegistry m
    -> ChainSyncServer (WithBlockSize (Serialised (Header blk))) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newReader chainDB registry
        (WithBlockSize <$> GetBlockSize <*> getSerialisedHeaderWithPoint)
      let ChainSyncServer server = chainSyncServerForReader
            tracer chainDB rdr convert toPoint
      server
  where
    convert :: WithBlockSize (SerialisedWithPoint blk (Header blk))
            -> WithBlockSize (Serialised (Header blk))
    convert = fmap serialised

    toPoint :: WithBlockSize (SerialisedWithPoint blk (Header blk))
            -> Point blk
    toPoint = point . withoutBlockSize

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
chainSyncBlocksServer
    :: forall m blk. IOLike m
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> ResourceRegistry m
    -> ChainSyncServer (Serialised blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB registry =
    ChainSyncServer $ do
      rdr <- ChainDB.newReader chainDB registry getSerialisedBlockWithPoint
      let ChainSyncServer server = chainSyncServerForReader
            tracer chainDB rdr convert toPoint
      server
  where
    convert :: SerialisedWithPoint blk blk -> Serialised blk
    convert = serialised

    toPoint :: SerialisedWithPoint blk blk -> Point blk
    toPoint = point

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
    :: forall m blk bReader bServer.
       ( IOLike m
       , HeaderHash bServer ~ HeaderHash blk
       )
    => Tracer m (TraceChainSyncServerEvent blk)
    -> ChainDB m blk
    -> Reader  m blk bReader
    -> (bReader -> bServer)
       -- ^ Convert a @bReader@ from the 'Reader' to a @bServer@ the
       -- 'ChainSyncServer' will return.
    -> (bReader -> Point blk)
    -> ChainSyncServer bServer (Tip blk) m ()
chainSyncServerForReader tracer chainDB rdr convert toPoint =
    idle'
  where
    idle :: ServerStIdle bServer (Tip blk) m ()
    idle = ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = ChainDB.readerClose rdr
      }

    idle' :: ChainSyncServer bServer (Tip blk) m ()
    idle' = ChainSyncServer $ return idle

    handleRequestNext
      :: m (Either (ServerStNext bServer (Tip blk) m ())
                   (m (ServerStNext bServer (Tip blk) m ())))
    handleRequestNext = ChainDB.readerInstruction rdr >>= \case
      Just update -> do
        tip <- getTip
        traceWith tracer $
          TraceChainSyncServerRead tip (toPoint <$> update)
        return $ Left $ sendNext tip (convert <$> update)
      Nothing     -> return $ Right $ do
        -- Reader is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.readerInstructionBlocking rdr
        tip    <- getTip
        traceWith tracer $
          TraceChainSyncServerReadBlocked tip (toPoint <$> update)
        return $ sendNext tip (convert <$> update)

    sendNext :: Tip blk
             -> ChainUpdate blk bServer
             -> ServerStNext bServer (Tip blk) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip idle'
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip idle'

    handleFindIntersect :: [Point bServer]
                        -> m (ServerStIntersect bServer (Tip blk) m ())
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
-- their corresponding 'Point'.
data TraceChainSyncServerEvent blk
  = TraceChainSyncServerRead        (Tip blk) (ChainUpdate blk (Point blk))
  | TraceChainSyncServerReadBlocked (Tip blk) (ChainUpdate blk (Point blk))
  deriving (Eq, Show)
