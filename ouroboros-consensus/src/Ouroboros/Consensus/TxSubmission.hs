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
     , ApplyTx blk
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
        case res of
          [(_tx, mbErr)] -> return (show <$> mbErr, server)
          -- The output list of addTxs has the same length as the input list.
          _              -> error "addTxs: unexpected result"

    , recvMsgDone = ()
    }

