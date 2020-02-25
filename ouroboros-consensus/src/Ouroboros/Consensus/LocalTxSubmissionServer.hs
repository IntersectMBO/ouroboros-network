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

import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.IOLike


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: (MonadSTM m, ApplyTx blk)
  => Tracer m (TraceLocalTxSubmissionServerEvent blk)
  -> Mempool m blk idx
  -> LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()
localTxSubmissionServer tracer mempool =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer $ TraceReceivedTx tx
        res <- addTxs mempool [tx]
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
