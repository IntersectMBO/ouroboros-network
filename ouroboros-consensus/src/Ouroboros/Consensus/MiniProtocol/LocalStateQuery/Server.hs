{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
  ( localStateQueryServer
  ) where

import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.IOLike

localStateQueryServer ::
     forall m blk. (IOLike m, QueryLedger blk)
  => LedgerConfig blk
  -> (Point blk -> STM m (Maybe (ExtLedgerState blk)))
     -- ^ Get a past ledger
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> LocalStateQueryServer blk (Query blk) m ()
localStateQueryServer cfg getPastLedger getImmutablePoint =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Query blk) m ()
    idle = ServerStIdle {
          recvMsgAcquire = handleAcquire
        , recvMsgDone    = return ()
        }

    handleAcquire :: Point blk -> m (ServerStAcquiring blk (Query blk) m ())
    handleAcquire pt = do
        (mPastLedger, immutablePoint) <-
          atomically $ (,) <$> getPastLedger pt <*> getImmutablePoint

        return $ case mPastLedger of
          Just pastLedger
            -> SendMsgAcquired $ acquired (ledgerState pastLedger)
          Nothing
            | pointSlot pt < pointSlot immutablePoint
            -> SendMsgFailure AcquireFailurePointTooOld idle
            | otherwise
            -> SendMsgFailure AcquireFailurePointNotOnChain idle

    acquired :: LedgerState blk -> ServerStAcquired blk (Query blk) m ()
    acquired ledgerState = ServerStAcquired {
          recvMsgQuery     = handleQuery ledgerState
        , recvMsgReAcquire = handleAcquire
        , recvMsgRelease   = return idle
        }

    handleQuery ::
         LedgerState blk
      -> Query blk result
      -> m (ServerStQuerying blk (Query blk) m () result)
    handleQuery ledgerState query = return $
        SendMsgResult
          (answerQuery cfg query ledgerState)
          (acquired ledgerState)
