{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query (Query, QueryLedger)
import qualified Ouroboros.Consensus.Ledger.Query as Query
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Util (StaticEither (..), fromStaticLeft,
                     fromStaticRight)
import           Ouroboros.Consensus.Util.IOLike

localStateQueryServer ::
     forall m blk.
     ( IOLike m
     , QueryLedger blk
     , Query.ConfigSupportsNode blk
     , LedgerSupportsProtocol blk
     )
  => ExtLedgerCfg blk
  -> (forall b.
         StaticEither b () (Point blk)
      -> m (StaticEither
             b
                                 (DiskLedgerView m (ExtLedgerState blk))
             (Either (Point blk) (DiskLedgerView m (ExtLedgerState blk)))
           )
     )
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg getDLV =
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
        case mpt of
          Nothing -> do
            dlv <- fromStaticLeft <$> getDLV (StaticLeft ())
            return $ SendMsgAcquired $ acquired dlv
          Just pt -> do
            ei <- fromStaticRight <$> getDLV (StaticRight pt)
            case ei of
              Left immP
                | pointSlot pt < pointSlot immP
                -> return $ SendMsgFailure AcquireFailurePointTooOld idle
                | otherwise
                -> return $ SendMsgFailure AcquireFailurePointNotOnChain idle
              Right dlv -> return $ SendMsgAcquired $ acquired dlv

    acquired :: DiskLedgerView m (ExtLedgerState blk)
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired dlv = ServerStAcquired {
          recvMsgQuery     = handleQuery dlv
        , recvMsgReAcquire = \mp -> do close; handleAcquire mp
        , recvMsgRelease   =        do close; return idle
        }
      where
        DiskLedgerView _st _dbRead _dbReadRange close = dlv

    handleQuery ::
         DiskLedgerView m (ExtLedgerState blk)
      -> Query blk fp result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery dlv query = do
      result <- Query.handleQuery cfg dlv query
      return $ SendMsgResult result (acquired dlv)
