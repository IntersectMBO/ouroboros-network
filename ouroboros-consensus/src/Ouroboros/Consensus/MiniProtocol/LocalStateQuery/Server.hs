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
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (Query, QueryLedger)
import qualified Ouroboros.Consensus.Ledger.Query as Query
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk (LedgerDB')
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

localStateQueryServer ::
     forall m blk. (IOLike m, QueryLedger blk, Query.ConfigSupportsNode blk, HasAnnTip blk)
  => ExtLedgerCfg blk
  -> ResourceRegistry m
  -> STM m (Point blk)
     -- ^ Get tip point
  -> (Point blk -> STM m (Maybe (LedgerDB' blk)))
     -- ^ Get a past ledger
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg _rreg getTipPoint getPastLedger getImmutablePoint =
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
        -- TODO this must also acquire the lock that prevents the ChainSel logic
        -- from flushing DbChangelog prefixes to disk

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

    acquired :: LedgerDB' blk
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired ledgerDB = ServerStAcquired {
          recvMsgQuery     = handleQuery ledgerDB
        , recvMsgReAcquire =
            -- TODO this must also release the lock that prevents the ChainSel
            -- logic from flushing a DbChangelog prefix to disk
            handleAcquire
        , recvMsgRelease   =
            -- TODO this must also release the lock that prevents the ChainSel
            -- logic from flushing a DbChangelog prefix to disk
            return idle
        }

    handleQuery ::
         LedgerDB' blk
      -> Query blk fp result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery ledgerDB query = do
      result <- Query.handleQuery cfg (error "dlv") query ledgerDB
      pure $ SendMsgResult result (acquired ledgerDB)
