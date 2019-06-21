{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncServer
  ( chainSyncHeadersServer
  , chainSyncBlocksServer
  ) where


import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Ouroboros.Network.Block (Point (..), castPoint, HeaderHash)
import           Ouroboros.Network.Chain (ChainUpdate (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block


-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
--
chainSyncHeadersServer
    :: forall m blk. MonadSTM m
    => Tracer m String
    -> ChainDB m blk (Header blk)
    -> ChainSyncServer (Header blk) (Point blk) m ()
chainSyncHeadersServer tracer chainDB =
    ChainSyncServer $ do
      rdr <- ChainDB.newHeaderReader chainDB
      let ChainSyncServer server = chainSyncServerForReader tracer chainDB rdr
      server

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
chainSyncBlocksServer
    :: forall m blk. MonadSTM m
    => Tracer m String
    -> ChainDB m blk (Header blk)
    -> ChainSyncServer blk (Point blk) m ()
chainSyncBlocksServer tracer chainDB =
    ChainSyncServer $ do
      rdr <- ChainDB.newBlockReader chainDB
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
    :: forall m blk hdr b.
       MonadSTM m
    => HeaderHash blk ~ HeaderHash b -- b is used at either type blk or hdr
    => Tracer m String
    -> ChainDB m blk hdr
    -> Reader  m b
    -> ChainSyncServer b (Point blk) m ()
chainSyncServerForReader _tracer chainDB rdr =
    idle'
  where
    idle :: ServerStIdle b (Point blk) m ()
    idle =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext,
        recvMsgFindIntersect = handleFindIntersect,
        recvMsgDoneClient    = return ()
      }

    idle' :: ChainSyncServer b (Point blk) m ()
    idle' = ChainSyncServer (return idle)

    handleRequestNext :: m (Either (ServerStNext b (Point blk) m ())
                                (m (ServerStNext b (Point blk) m ())))
    handleRequestNext = do
      mupdate <- ChainDB.readerInstruction rdr
      tip     <- atomically $ castPoint <$> ChainDB.getTipPoint chainDB
      return $ case mupdate of
        Just update -> Left  $ sendNext tip update
        Nothing     -> Right $ sendNext tip <$>
                                 ChainDB.readerInstructionBlocking rdr
                       -- Reader is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: Point blk
             -> ChainUpdate b
             -> ServerStNext b (Point blk) m ()
    sendNext tip update = case update of
      AddBlock hdr -> SendMsgRollForward   hdr           tip idle'
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip idle'

    handleFindIntersect :: [Point blk]
                        -> m (ServerStIntersect b (Point blk) m ())
    handleFindIntersect points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr (map castPoint points)
      tip     <- atomically $ castPoint <$> ChainDB.getTipPoint chainDB
      return $ case changed of
        Just pt -> SendMsgIntersectImproved (castPoint pt) tip idle'
        Nothing -> SendMsgIntersectUnchanged               tip idle'
