{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}

module Ouroboros.Consensus.TxSubmission
  ( localTxSubmissionServer
  ) where

import           Control.Tracer

import           Ouroboros.Network.Protocol.LocalTxSubmission.Server

import           Ouroboros.Consensus.Mempool.API


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: (Monad m, Show (ApplyTxErr blk)) -- TODO: consider using condense
  => Tracer m unused --TODO: trace adding txs here or in the mempool directly
  -> Mempool m blk
  -> LocalTxSubmissionServer (GenTx blk) String m ()
localTxSubmissionServer _tracer Mempool{addTxs} =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        res <- addTxs [tx]
        -- The 'addTxs' action returns the failing ones, whereas the protocol
        -- returns a Maybe failure for a single tx, so we must convert here.
        case res of
          [(_tx,rej)] -> return (Just (show rej), server)
          _           -> return (Nothing,         server),

      recvMsgDone = ()
    }

