{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.LocalTxSubmissionServer
  ( localTxSubmissionServer
    -- * Trace events
  , TraceLocalTxSubmissionServerEvent (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.RecentTxIds (RecentTxIds)
import qualified Ouroboros.Network.RecentTxIds as RecentTxIds

import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.IOLike


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: (IOLike m, ApplyTx blk, HasTxId (GenTx blk))
  => Tracer m (TraceLocalTxSubmissionServerEvent blk)
  -> Mempool m blk idx
  -> StrictTVar m (RecentTxIds (GenTxId blk))
  -> LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()
localTxSubmissionServer tracer mempool varRecentTxIds =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer $ TraceReceivedTx tx
        res <- addTxs mempool [tx]

        currentTime <- getMonotonicTime
        atomically $ modifyTVar varRecentTxIds $
          RecentTxIds.insertTxId (txId tx) currentTime

        case res of
          [(_tx, mbErr)] -> return (mbErr, server)
          -- The output list of addTxs has the same length as the input list.
          _              -> error "addTxs: unexpected result"

    , recvMsgDone = ()
    }


{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceLocalTxSubmissionServerEvent blk
  = TraceReceivedTx (GenTx blk)
    -- ^ A transaction was received.

deriving instance Eq   (GenTx blk)
               => Eq   (TraceLocalTxSubmissionServerEvent blk)
deriving instance Show (GenTx blk)
               => Show (TraceLocalTxSubmissionServerEvent blk)
