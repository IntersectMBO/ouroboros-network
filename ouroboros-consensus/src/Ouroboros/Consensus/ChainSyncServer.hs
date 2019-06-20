{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}
module Ouroboros.Consensus.ChainSyncServer
  ( chainSyncServer
  ) where


import           Control.Monad.Class.MonadSTM
import           Control.Tracer

import           Ouroboros.Network.Block (Point (..), castPoint)
import           Ouroboros.Network.Chain (ChainUpdate (..))
import           Ouroboros.Network.Protocol.ChainSync.Server

import           Ouroboros.Storage.ChainDB.API (ChainDB, Reader)
import qualified Ouroboros.Storage.ChainDB.API as ChainDB

import           Ouroboros.Consensus.Block


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
    :: forall m blk. MonadSTM m
    => Tracer m String
    -> ChainDB m blk (Header blk)
    -> ChainSyncServer (Header blk) (Point blk) m ()
chainSyncServer _tracer chainDB = ChainSyncServer $
    idle <$> ChainDB.newHeaderReader chainDB
  where
    idle :: Reader m (Header blk)
         -> ServerStIdle (Header blk) (Point blk) m ()
    idle rdr =
      ServerStIdle {
        recvMsgRequestNext   = handleRequestNext rdr,
        recvMsgFindIntersect = handleFindIntersect rdr,
        recvMsgDoneClient    = return ()
      }

    idle' :: Reader m (Header blk)
          -> ChainSyncServer (Header blk) (Point blk) m ()
    idle' = ChainSyncServer . return . idle

    handleRequestNext :: Reader m (Header blk)
                      -> m (Either (ServerStNext (Header blk) (Point blk) m ())
                                (m (ServerStNext (Header blk) (Point blk) m ())))
    handleRequestNext rdr = do
      mupdate <- ChainDB.readerInstruction rdr
      tip     <- atomically $ ChainDB.getTipPoint chainDB
      return $ case mupdate of
        Just update -> Left  $ sendNext rdr tip update
        Nothing     -> Right $ sendNext rdr tip <$> ChainDB.readerInstructionBlocking rdr
                       -- Reader is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext :: Reader m (Header blk)
             -> Point blk
             -> ChainUpdate (Header blk)
             -> ServerStNext (Header blk) (Point blk) m ()
    sendNext rdr tip update = case update of
      AddBlock hdr -> SendMsgRollForward  hdr            tip (idle' rdr)
      RollBack pt  -> SendMsgRollBackward (castPoint pt) tip (idle' rdr)

    handleFindIntersect :: Reader m (Header blk)
                        -> [Point blk]
                        -> m (ServerStIntersect (Header blk) (Point blk) m ())
    handleFindIntersect rdr points = do
      -- TODO guard number of points
      changed <- ChainDB.readerForward rdr (map castPoint points)
      tip     <- atomically $ ChainDB.getTipPoint chainDB
      return $ case changed of
        Just pt -> SendMsgIntersectImproved (castPoint pt) tip (idle' rdr)
        Nothing -> SendMsgIntersectUnchanged               tip (idle' rdr)
