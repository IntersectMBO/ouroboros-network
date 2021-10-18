{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HasAnnTip (..))
import           Ouroboros.Consensus.Ledger.Basics (DiskLedgerView)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query
import           Ouroboros.Consensus.Util.IOLike

localStateQueryServer ::
     forall m blk. (IOLike m, QueryLedger blk, ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> DiskLedgerView blk m
  -> STM m (Point blk)
     -- ^ Get tip point
  -> (Point blk -> STM m (Maybe (ExtLedgerState SmallL blk)))
     -- ^ Get a past ledger
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg dlv getTipPoint getPastLedger getImmutablePoint =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Point blk) (Query blk) m ()
    idle = ServerStIdle {
          recvMsgAcquire = handleAcquire
        , recvMsgDone    = return ()
        }

    handleAcquire :: Maybe (Point blk)
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt = do
        (pt, mPastLedger, immutablePoint) <- atomically $ do
          pt <- maybe getTipPoint pure mpt
          (pt,,) <$> getPastLedger pt <*> getImmutablePoint

        return $ case mPastLedger of
          Just pastLedger
            -> SendMsgAcquired $ acquired pastLedger
          Nothing
            | pointSlot pt < pointSlot immutablePoint
            -> SendMsgFailure AcquireFailurePointTooOld idle
            | otherwise
            -> SendMsgFailure AcquireFailurePointNotOnChain idle

    acquired :: ExtLedgerState SmallL blk
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired ledgerState = ServerStAcquired {
          recvMsgQuery     = handleQuery ledgerState
        , recvMsgReAcquire = handleAcquire
        , recvMsgRelease   = return idle
        }

    handleQuery ::
         ExtLedgerState SmallL blk
      -> Query          blk fp result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery ledgerState query = do
      result <- answerQuery cfg dlv query (error "handleQuery FootPrint cast" ledgerState)
      pure $ SendMsgResult result (acquired ledgerState)
