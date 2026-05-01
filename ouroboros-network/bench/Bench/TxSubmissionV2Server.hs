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
  , mkMultiPeerFixture
  , runDirectServerBenchmark
  ) where

import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadAsync (mapConcurrently_)
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
           (Tx (..), TxId,
           emptyMempool, getMempoolReader, getMempoolWriter, readMempool)


data DirectServerFixture = DirectServerFixture
  { dsPeerCount        :: !Int
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
    { dsPeerCount        = 1
    , dsTxIdReplyBatches = batches
    , dsTxSize           = SizeInBytes 1024
    }


-- | Multi-peer fixture: @peers@ peers each playing back the same txid stream.
--
-- Models the mainnet steady state where many upstream peers advertise the
-- same fresh txids. The first peer downloads each tx body and submits it to
-- the mempool, which puts the key into 'sharedRetainedTxs'. Subsequent
-- peers find the txid in the retained set and ack-skip without ever
-- requesting the body.
mkMultiPeerFixture
  :: Int -- ^ peer count
  -> Int -- ^ batches per peer
  -> DirectServerFixture
mkMultiPeerFixture peers batches =
  DirectServerFixture
    { dsPeerCount        = peers
    , dsTxIdReplyBatches = batches
    , dsTxSize           = SizeInBytes 1024
    }


runDirectServerBenchmark
  :: DirectServerFixture -> IO DirectServerResult
runDirectServerBenchmark
  DirectServerFixture {
      dsPeerCount,
      dsTxIdReplyBatches,
      dsTxSize
    } = do
  inboundMempool <- emptyMempool
  duplicateTxIdsVar <- Lazy.newTVarIO []
  sharedStateVar <- newSharedTxStateVar emptySharedTxState
  countersVar <- newTxSubmissionCountersVar mempty

  let runPeer addr =
        withPeer
          defaultTxDecisionPolicy
          (getMempoolReader inboundMempool)
          sharedStateVar
          countersVar
          addr
          $ \api -> do
            let server =
                  txSubmissionInboundV2
                    nullTracer
                    NoTxSubmissionInitDelay
                    defaultTxDecisionPolicy
                    (getMempoolWriter duplicateTxIdsVar inboundMempool)
                    getTxSize
                    api
            case server of
              TxSubmissionServerPipelined initServer -> do
                st0 <- initServer
                driveServer dsTxSize dsTxIdReplyBatches 1 [] st0

  -- Spawn one async per peer. Peers race for tx leases, contend on
  -- the shared STM state, and exercise the scheduler the same way
  -- they do on mainnet.
  mapConcurrently_ runPeer [1 .. dsPeerCount]

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
           getTxValid = True,
           getTxParent = Nothing
         }
      | (txid, txSize) <- Map.toAscList requested
      ]
