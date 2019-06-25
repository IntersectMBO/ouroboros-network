{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}

module Ouroboros.Consensus.TxSubmission
  ( localTxSubmissionServer
  ) where

import           Control.Tracer

import           Ouroboros.Network.Protocol.LocalTxSubmission.Server

import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util.Condense


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: ( Monad m
     , Show (ApplyTxErr blk) -- TODO: consider using condense
     , Condense (GenTx blk)
     )
  => Tracer m String
  -> Mempool m blk idx
  -> LocalTxSubmissionServer (GenTx blk) String m ()
localTxSubmissionServer tracer Mempool{addTxs} =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer (condense tx)
        res <- addTxs [tx]
        -- The 'addTxs' action returns the failing ones, whereas the protocol
        -- returns a Maybe failure for a single tx, so we must convert here.
        case res of
          [(_tx,rej)] -> return (Just (show rej), server)
          _           -> return (Nothing,         server),

      recvMsgDone = ()
    }

