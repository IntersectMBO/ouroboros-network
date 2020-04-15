{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
  ( localStateQueryServer
  ) where

import           Data.Functor ((<&>))

import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Ouroboros.Network.Block (Point)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.ChainDB (LedgerCursor (..),
                     LedgerCursorFailure (..))

localStateQueryServer
  :: forall m blk. (IOLike m, QueryLedger blk)
  => LedgerConfig blk
  -> m (LedgerCursor m blk)
  -> LocalStateQueryServer blk (Query blk) m ()
localStateQueryServer cfg newLedgerCursor =
    LocalStateQueryServer $ idle <$> newLedgerCursor
  where
    idle
      :: LedgerCursor m blk
      -> ServerStIdle blk (Query blk) m ()
    idle ledgerCursor = ServerStIdle
      { recvMsgAcquire = handleAcquire ledgerCursor
      , recvMsgDone    = return ()
      }

    handleAcquire
      :: LedgerCursor m blk
      -> Point blk
      -> m (ServerStAcquiring blk (Query blk) m ())
    handleAcquire ledgerCursor pt =
      ledgerCursorMove ledgerCursor pt <&> \case
        Left failure ->
          SendMsgFailure (translateFailure failure) (idle ledgerCursor)
        Right ExtLedgerState { ledgerState } ->
          SendMsgAcquired (acquired ledgerState ledgerCursor)

    acquired
      :: LedgerState blk
      -> LedgerCursor m blk
      -> ServerStAcquired blk (Query blk) m ()
    acquired ledgerState ledgerCursor = ServerStAcquired
      { recvMsgQuery     = handleQuery ledgerState ledgerCursor
      , recvMsgReAcquire = handleAcquire ledgerCursor
      , recvMsgRelease   = return $ idle ledgerCursor
      }

    handleQuery
      :: LedgerState blk
      -> LedgerCursor m blk
      -> Query blk result
      -> m (ServerStQuerying blk (Query blk) m () result)
    handleQuery ledgerState ledgerCursor query = return $
      SendMsgResult
        (answerQuery cfg query ledgerState)
        (acquired ledgerState ledgerCursor)

    translateFailure
      :: LedgerCursorFailure
      -> AcquireFailure
    translateFailure = \case
      PointTooOld     -> AcquireFailurePointTooOld
      PointNotOnChain -> AcquireFailurePointNotOnChain
