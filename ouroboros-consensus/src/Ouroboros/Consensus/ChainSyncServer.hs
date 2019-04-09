{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncServer
  ( chainSyncServer
  ) where


import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Ouroboros.Network.Block (HeaderHash, Point (..), castPoint)
import           Ouroboros.Network.Chain (ChainUpdate (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB


-- | Chain Sync Server
--
-- This is a version of
-- 'Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample' that
-- uses the 'ChainDB' instead of
-- 'Ourboros.Network.ChainProducerState.ChainProducerState'.
--
-- All the hard work is done by the 'Reader's provided by the
-- 'ChainDB'.
chainSyncServer
    :: forall m blk hdr.
       (MonadSTM m, HeaderHash hdr ~ HeaderHash blk)
    => Tracer m String
    -> ChainDB m blk hdr
    -> ChainSyncServer hdr (Point hdr) m ()
chainSyncServer _tracer chainDB = ChainSyncServer $
    idle <$> ChainDB.newReader chainDB
  where
    idle :: Reader m hdr -> ServerStIdle hdr (Point hdr) m ()
    idle rdr =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext rdr,
        recvMsgFindIntersect = handleFindIntersect rdr,
        recvMsgDoneClient    = return ()
      }

    idle' :: Reader m hdr -> ChainSyncServer hdr (Point hdr) m ()
    idle' = ChainSyncServer . return . idle

    handleRequestNext :: Reader m hdr
                      -> m (Either (ServerStNext hdr (Point hdr) m ())
                                (m (ServerStNext hdr (Point hdr) m ())))
    handleRequestNext rdr = do
      mupdate <- ChainDB.readerInstruction rdr
      tip     <- atomically $ castPoint <$> ChainDB.getTipPoint chainDB
      return $ case mupdate of
        Just update -> Left  $ sendNext rdr tip update
        Nothing     -> Right $ sendNext rdr tip <$> ChainDB.readerInstructionBlocking rdr
                       -- Reader is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: Reader m hdr
             -> Point hdr
             -> ChainUpdate hdr
             -> ServerStNext hdr (Point hdr) m ()
    sendNext rdr tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr tip (idle' rdr)
      RollBack pt  -> SendMsgRollBackward pt  tip (idle' rdr)

    handleFindIntersect :: Reader m hdr
                        -> [Point hdr]
                        -> m (ServerStIntersect hdr (Point hdr) m ())
    handleFindIntersect rdr points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr points
      tip     <- atomically $ castPoint <$> ChainDB.getTipPoint chainDB
      return $ case changed of
        Just pt -> SendMsgIntersectImproved  pt tip (idle' rdr)
        Nothing -> SendMsgIntersectUnchanged    tip (idle' rdr)
