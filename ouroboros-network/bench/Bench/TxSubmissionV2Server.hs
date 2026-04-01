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
           (TxDecisionPolicy (..), TxSubmissionInitDelay (NoTxSubmissionInitDelay),
           defaultTxDecisionPolicy, txSubmissionInboundV2)
import Ouroboros.Network.TxSubmission.Inbound.V2.Registry
           (newPeerTxInFlightRegistry, newSharedTxStateVar,
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
  , dsPolicy           :: !TxDecisionPolicy
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
    , dsPolicy           = defaultTxDecisionPolicy
    }


-- | Multi-peer fixture: @peers@ peers each playing back the same txid stream.
--
-- Models the mainnet steady state where many upstream peers advertise the
-- same fresh txids. The first peer downloads each tx body and submits it to
-- the mempool, which puts the key into 'sharedRetainedTxs'. Subsequent
-- peers find the txid in the retained set and ack-skip without ever
-- requesting the body.
--
-- The fixture disables pipelined txid requests so the benchmark measures
-- STM contention on the shared state without picker cycles introduced by
-- the pipelined-txid loosening.  Body requests still use the protocol's
-- pipelined wire form.
mkMultiPeerFixture
  :: Int -- ^ peer count
  -> Int -- ^ batches per peer
  -> DirectServerFixture
mkMultiPeerFixture peers batches =
  DirectServerFixture
    { dsPeerCount        = peers
    , dsTxIdReplyBatches = batches
    , dsTxSize           = SizeInBytes 1024
    , dsPolicy           = defaultTxDecisionPolicy {
                             disablePipelinedTxIdRequests = True
                           }
    }


runDirectServerBenchmark
  :: DirectServerFixture -> IO DirectServerResult
runDirectServerBenchmark DirectServerFixture {
      dsPeerCount,
      dsTxIdReplyBatches,
      dsTxSize,
      dsPolicy
    } = do
  inboundMempool <- emptyMempool
  duplicateTxIdsVar <- Lazy.newTVarIO []
  sharedStateVar <- newSharedTxStateVar emptySharedTxState
  inFlightRegistry <- newPeerTxInFlightRegistry
  countersVar <- newTxSubmissionCountersVar mempty

  let writer = getMempoolWriter duplicateTxIdsVar inboundMempool

      runPeer addr =
        withPeer
          dsPolicy
          (getMempoolReader inboundMempool)
          sharedStateVar
          inFlightRegistry
          countersVar
          addr
          $ \api -> do
            let server =
                  txSubmissionInboundV2
                    nullTracer
                    NoTxSubmissionInitDelay
                    dsPolicy
                    writer
                    getTxSize
                    api
                stream = [1 .. dsTxIdReplyBatches * 6]
            case server of
              TxSubmissionServerPipelined initServer -> do
                st0 <- initServer
                driveServer dsTxSize stream [] st0

  mapConcurrently_ runPeer [1 .. dsPeerCount]

  (DirectServerResult
    . length <$> readMempool inboundMempool)
    <*> readTVarIO countersVar


driveServer
  :: SizeInBytes
  -> [TxId]
  -> [PendingReply]
  -> ServerStIdle n TxId (Tx TxId) IO ()
  -> IO ()
driveServer !txSize !stream !pending =
    \case
      SendMsgRequestTxIdsBlocking _ req kDone k
        | null stream -> kDone
        | otherwise -> do
            let (txids, stream') = takeReply txSize req stream
            st' <- k (NonEmpty.fromList txids)
            driveServer txSize stream' pending st'

      SendMsgRequestTxIdsPipelined _ req k -> do
        let (txids, stream') = takeReply txSize req stream
            pending' = pending ++ [PendingTxIds req txids]
        st' <- k
        driveServer txSize stream' pending' st'

      SendMsgRequestTxsPipelined requested k -> do
        st' <- k
        driveServer
          txSize
          stream
          (pending ++ [PendingTxs requested])
          st'

      CollectPipelined mNone collect ->
        case pending of
          reply : pending' -> do
            st' <- collect (renderPendingReply reply)
            driveServer txSize stream pending' st'
          [] ->
            case mNone of
              Just k -> k >>= driveServer txSize stream []
              Nothing ->
                error $
                  "TxSubmissionV2 direct benchmark: unexpected "
                    ++ "CollectPipelined with no pending replies"


-- | Take the next @req@-sized chunk from the precomputed txid stream. If
-- the stream is exhausted, return an empty reply and the empty stream.
takeReply
  :: SizeInBytes
  -> NumTxIdsToReq
  -> [TxId]
  -> ([(TxId, SizeInBytes)], [TxId])
takeReply txSize req stream =
    let (taken, rest) = splitAt replyCount stream
    in ([ (t, txSize) | t <- taken ], rest)
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
