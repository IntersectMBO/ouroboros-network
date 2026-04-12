{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Bench.TxSubmissionV2Server
  ( DirectServerFixture
  , DirectServerResult
  , mkDirectServerFixture
  , runDirectServerBenchmark
  ) where

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Tracer (nullTracer)

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type
           (NumTxIdsToReq (getNumTxIdsToReq), SizeInBytes (..))
import Ouroboros.Network.TxSubmission.Inbound.V2
           (TxSubmissionInitDelay (NoTxSubmissionInitDelay),
           defaultTxDecisionPolicy, txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry (newSharedTxStateVar,
           newTxSubmissionCountersVar, withPeer)
import Ouroboros.Network.TxSubmission.Inbound.V2.Types (TxSubmissionCounters,
           emptySharedTxState)

import Test.Ouroboros.Network.TxSubmission.Types
           (Tx (Tx, getTxAdvSize, getTxId, getTxSize, getTxValid), TxId,
           emptyMempool, getMempoolReader, getMempoolWriter, readMempool)


data DirectServerFixture = DirectServerFixture
  { dsPeerAddr         :: !Int
  , dsTxIdReplyBatches :: !Int
  , dsTxSize           :: !SizeInBytes
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass NFData


data DirectServerResult = DirectServerResult
  { dsAcceptedTxs :: !Int
  , dsCounters    :: !TxSubmissionCounters
  }
  deriving stock    (Eq, Show, Generic)
  deriving anyclass NFData


data PendingReply
  = PendingTxIds !NumTxIdsToReq ![(TxId, SizeInBytes)]
  | PendingTxs !(Map TxId SizeInBytes)


mkDirectServerFixture
  :: Int -> DirectServerFixture
mkDirectServerFixture batches =
  DirectServerFixture
    { dsPeerAddr = 1
    , dsTxIdReplyBatches = batches
    , dsTxSize = SizeInBytes 1024
    }


runDirectServerBenchmark
  :: DirectServerFixture -> IO DirectServerResult
runDirectServerBenchmark
  DirectServerFixture {
      dsPeerAddr,
      dsTxIdReplyBatches,
      dsTxSize
    } = do
  inboundMempool <- emptyMempool
  duplicateTxIdsVar <- Lazy.newTVarIO []
  sharedStateVar <- newSharedTxStateVar emptySharedTxState
  countersVar <- newTxSubmissionCountersVar mempty

  withPeer
    defaultTxDecisionPolicy
    (getMempoolReader inboundMempool)
    sharedStateVar
    countersVar
    dsPeerAddr
    $ \api -> do
    let server =
          txSubmissionInboundV2
            nullTracer
            NoTxSubmissionInitDelay
            (getMempoolReader inboundMempool)
            (getMempoolWriter duplicateTxIdsVar inboundMempool)
            getTxSize
            api

    case server of
      TxSubmissionServerPipelined initServer -> do
        st0 <- initServer
        driveServer
          dsTxSize
          dsTxIdReplyBatches
          1
          []
          st0

    (DirectServerResult
      . length <$> readMempool inboundMempool)
      <*> readTVarIO countersVar


driveServer
  :: SizeInBytes
  -> Int
  -> TxId
  -> [PendingReply]
  -> ServerStIdle n TxId (Tx TxId) IO ()
  -> IO ()
driveServer !txSize !remainingBatches !nextTxId !pending =
    \case
      SendMsgRequestTxIdsBlocking _ req kDone k
        | remainingBatches <= 0 -> kDone
        | otherwise -> do
            let (txids, nextTxId') = mkTxIdReply txSize nextTxId req
            st' <- k (NonEmpty.fromList txids)
            driveServer txSize (remainingBatches - 1) nextTxId' pending st'

      SendMsgRequestTxIdsPipelined _ req k -> do
        let (txids, nextTxId', remainingBatches') =
              if remainingBatches <= 0
                 then ([], nextTxId, remainingBatches)
                 else let (txids', nextTxId'') = mkTxIdReply txSize nextTxId req
                      in (txids', nextTxId'', remainingBatches - 1)
            pending' = pending ++ [PendingTxIds req txids]
        st' <- k
        driveServer txSize remainingBatches' nextTxId' pending' st'

      SendMsgRequestTxsPipelined requested k -> do
        st' <- k
        driveServer
          txSize
          remainingBatches
          nextTxId
          (pending ++ [PendingTxs requested])
          st'

      CollectPipelined mNone collect ->
        case pending of
          reply : pending' -> do
            st' <- collect (renderPendingReply reply)
            driveServer txSize remainingBatches nextTxId pending' st'
          [] ->
            case mNone of
              Just k -> k >>= driveServer txSize remainingBatches nextTxId []
              Nothing ->
                error $
                  "TxSubmissionV2 direct benchmark: unexpected "
                    ++ "CollectPipelined with no pending replies"


mkTxIdReply
  :: SizeInBytes
  -> TxId
  -> NumTxIdsToReq
  -> ([(TxId, SizeInBytes)], TxId)
mkTxIdReply txSize nextTxId req =
    ( [ (txid, txSize)
      | txid <- [nextTxId .. nextTxId + replyCount - 1]
      ]
    , nextTxId + replyCount
    )
  where
    replyCount = fromIntegral (getNumTxIdsToReq req)


renderPendingReply
  :: PendingReply -> Collect TxId (Tx TxId)
renderPendingReply = \case
  PendingTxIds req txids ->
    CollectTxIds req txids

  PendingTxs requested ->
    CollectTxs requested
      [ Tx {
           getTxId = txid,
           getTxSize = txSize,
           getTxAdvSize = txSize,
           getTxValid = True
         }
      | (txid, txSize) <- Map.toAscList requested
      ]
