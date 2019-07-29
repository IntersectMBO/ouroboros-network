{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.TxSubmission
  ( localTxSubmissionServer
    -- * Trace events
  , TraceLocalTxSubmissionServerEvent (..)
  ) where

import           Control.Tracer

import           Ouroboros.Network.Protocol.LocalTxSubmission.Server

import           Ouroboros.Consensus.Mempool.API


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: ( Monad m
     , Show (ApplyTxErr blk)
     , ApplyTx blk
     )
  => Tracer m (TraceLocalTxSubmissionServerEvent blk)
  -> Mempool m blk idx
  -> LocalTxSubmissionServer (GenTx blk) String m ()
localTxSubmissionServer tracer Mempool{addTxs} =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer $ TraceReceivedTx tx
        res <- addTxs [tx]
        -- The 'addTxs' action returns the failing ones, whereas the protocol
        -- returns a Maybe failure for a single tx, so we must convert here.
        case res of
          [(_tx,rej)] -> return (Just (show rej), server)
          _           -> return (Nothing,         server),

      recvMsgDone = ()
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
