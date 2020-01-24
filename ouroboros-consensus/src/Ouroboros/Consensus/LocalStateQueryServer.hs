{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.LocalStateQueryServer
  ( localStateQueryServer
  ) where

import           Data.Functor ((<&>))

import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Network.Block (Point)

import           Ouroboros.Consensus.Ledger.Abstract (LedgerState (..),
                     QueryLedger (..))
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.ChainDB (LedgerCursor (..),
                     LedgerCursorFailure (..))

localStateQueryServer
  :: forall m blk. (IOLike m, QueryLedger blk)
  => m (LedgerCursor m blk)
  -> LocalStateQueryServer blk (Query blk) (Result blk) m ()
localStateQueryServer newLedgerCursor =
    LocalStateQueryServer $ idle <$> newLedgerCursor
  where
    idle
      :: LedgerCursor m blk
      -> ServerStIdle blk (Query blk) (Result blk) m ()
    idle ledgerCursor = ServerStIdle
      { recvMsgAcquire = handleAcquire ledgerCursor
      , recvMsgDone    = return ()
      }

    handleAcquire
      :: LedgerCursor m blk
      -> Point blk
      -> m (ServerStAcquiring blk (Query blk) (Result blk) m ())
    handleAcquire ledgerCursor pt =
      ledgerCursorMove ledgerCursor pt <&> \case
        Left failure ->
          SendMsgFailure (translateFailure failure) (idle ledgerCursor)
        Right ExtLedgerState { ledgerState } ->
          SendMsgAcquired (acquired ledgerState ledgerCursor)

    acquired
      :: LedgerState blk
      -> LedgerCursor m blk
      -> ServerStAcquired blk (Query blk) (Result blk) m ()
    acquired ledgerState ledgerCursor = ServerStAcquired
      { recvMsgQuery     = handleQuery ledgerState ledgerCursor
      , recvMsgReAcquire = handleAcquire ledgerCursor
      , recvMsgRelease   = return $ idle ledgerCursor
      }

    handleQuery
      :: LedgerState blk
      -> LedgerCursor m blk
      -> Query blk
      -> m (ServerStQuerying blk (Query blk) (Result blk) m ())
    handleQuery ledgerState ledgerCursor query = return $
      SendMsgResult
        (answerQuery query ledgerState)
        (acquired ledgerState ledgerCursor)

    translateFailure
      :: LedgerCursorFailure
      -> AcquireFailure
    translateFailure = \case
      PointTooOld     -> AcquireFailurePointTooOld
      PointNotOnChain -> AcquireFailurePointNotOnChain
