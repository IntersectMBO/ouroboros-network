{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.Registry
  ( SharedTxStateVar
  , newSharedTxStateVar
  , TxChannelsVar
  , newTxChannelsVar
  , PeerTxAPI (..)
  , decisionLogicThread
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)

import Data.Foldable (foldl', traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Ouroboros.Network.DeltaQ (PeerGSV (..))
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Decision
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Mempool.Reader

-- | Communication channels between `TxSubmission` client mini-protocol and
-- decision logic.
--
newtype TxChannels m peeraddr txid tx = TxChannels {
      txChannelMap :: Map peeraddr (StrictMVar m (TxDecision txid tx))
    }

type TxChannelsVar m peeraddr txid tx = StrictMVar m (TxChannels m peeraddr txid tx)

newTxChannelsVar :: MonadMVar m => m (TxChannelsVar m peeraddr txid tx)
newTxChannelsVar = newMVar (TxChannels Map.empty)

-- | API to access `PeerTxState` inside `PeerTxStateVar`.
--
data PeerTxAPI m txid tx = PeerTxAPI {
    readTxDecision      :: m (TxDecision txid tx),
    -- ^ a blocking action which reads `TxDecision`

    handleReceivedTxIds :: NumTxIdsToReq
                        -> StrictSeq txid
                        -- ^ received txids
                        -> Map txid SizeInBytes
                        -- ^ received sizes of advertised tx's
                        -> m (),
    -- ^ handle received txids

    handleReceivedTxs   :: Set txid
                        -- ^ requested txids
                        -> Map txid tx
                        -- ^ received txs
                        -> m ()
    -- ^ handle received txs
  }


-- | A bracket function which registers / de-registers a new peer in
-- `SharedTxStateVar` and `PeerTxStateVar`s,  which exposes `PeerTxStateAPI`.
-- `PeerTxStateAPI` is only safe inside the  `withPeer` scope.
--
withPeer
    :: forall peeraddr txid tx idx m a.
       ( MonadMask m
       , MonadMVar m
       , MonadSTM  m
       , Ord txid
       , Ord peeraddr
       , Show peeraddr
       )
    => Tracer m (DebugSharedTxState peeraddr txid tx)
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> TxSubmissionMempoolReader txid tx idx m
    -> peeraddr
    --  ^ new peer
    -> (PeerTxAPI m txid tx -> m a)
    -- ^ callback which gives access to `PeerTxStateAPI`
    -> m a
withPeer tracer
         channelsVar
         sharedStateVar
         TxSubmissionMempoolReader { mempoolGetSnapshot }
         peeraddr io =
    bracket
      (do -- create a communication channel
          !peerTxAPI <-
            modifyMVar channelsVar
              \ TxChannels { txChannelMap } -> do
                chann <- newEmptyMVar
                let (chann', txChannelMap') =
                      Map.alterF (\mbChann ->
                                   let !chann'' = fromMaybe chann mbChann
                                   in (chann'', Just chann''))
                                 peeraddr
                                 txChannelMap
                return
                  ( TxChannels { txChannelMap = txChannelMap' }
                  , PeerTxAPI { readTxDecision = takeMVar chann',
                                handleReceivedTxIds,
                                handleReceivedTxs }
                  )

          atomically $ modifyTVar sharedStateVar registerPeer
          return peerTxAPI
      )
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      (\_ -> uninterruptibleMask_ do
        atomically $ modifyTVar sharedStateVar unregisterPeer
        modifyMVar_ channelsVar
          \ TxChannels { txChannelMap } ->
            return TxChannels { txChannelMap = Map.delete peeraddr txChannelMap }
      )
      io
  where
    registerPeer :: SharedTxState peeraddr txid tx
                 -> SharedTxState peeraddr txid tx
    registerPeer st@SharedTxState { peerTxStates } =
      st { peerTxStates =
             Map.insert
               peeraddr
               PeerTxState {
                 availableTxIds           = Map.empty,
                 requestedTxIdsInflight   = 0,
                 requestedTxsInflightSize = 0,
                 requestedTxsInflight     = Set.empty,
                 unacknowledgedTxIds      = StrictSeq.empty,
                 unknownTxs               = Set.empty }
               peerTxStates
         }

    -- TODO: this function needs to be tested!
    unregisterPeer :: SharedTxState peeraddr txid tx
                   -> SharedTxState peeraddr txid tx
    unregisterPeer st@SharedTxState { peerTxStates,
                                      bufferedTxs,
                                      referenceCounts } =
        st { peerTxStates    = peerTxStates',
             bufferedTxs     = bufferedTxs',
             referenceCounts = referenceCounts' }
      where
        (PeerTxState { unacknowledgedTxIds }, peerTxStates') =
          Map.alterF
            (\case
              Nothing -> error ("TxSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
              Just a  -> (a, Nothing))
            peeraddr
            peerTxStates

        referenceCounts' =
          foldl' (flip $ Map.update
                             \cnt -> if cnt > 1
                                     then Just $! pred cnt
                                     else Nothing)
          referenceCounts
          unacknowledgedTxIds

        liveSet = Map.keysSet referenceCounts'

        bufferedTxs' = bufferedTxs
                       `Map.restrictKeys`
                       liveSet

    --
    -- PeerTxAPI
    --

    handleReceivedTxIds :: NumTxIdsToReq
                        -> StrictSeq txid
                        -> Map txid SizeInBytes
                        -> m ()
    handleReceivedTxIds numTxIdsToReq txidsSeq txidsMap = do
      -- TODO: hide this inside `receivedTxIds` so it's run in the same STM
      -- transaction.
      mempoolSnapshot <- atomically mempoolGetSnapshot
      receivedTxIds tracer
                    sharedStateVar
                    mempoolSnapshot
                    peeraddr
                    numTxIdsToReq
                    txidsSeq
                    txidsMap


    handleReceivedTxs :: Set txid
                      -- ^ requested txids
                      -> Map txid tx
                      -- ^ received txs
                      -> m ()
    handleReceivedTxs txids txs =
      collectTxs tracer sharedStateVar peeraddr txids txs


decisionLogicThread
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadMVar  m
       , MonadSTM   m
       , Ord peeraddr
       , Ord txid
       )
    => Tracer m (DebugSharedTxState peeraddr txid tx)
    -> TxDecisionPolicy
    -> StrictTVar m (Map peeraddr PeerGSV)
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
decisionLogicThread tracer policy gsvVar txChannelsVar sharedStateVar = go
  where
    go :: m Void
    go = do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      threadDelay 0.005 -- 5ms

      (decisions, st) <- atomically do
        sharedCtx <-
              SharedDecisionContext
          <$> readTVar gsvVar
          <*> readTVar sharedStateVar
        let activePeers = filterActivePeers policy (sdcSharedTxState sharedCtx)

        -- block until at least one peer is active
        check (not (Map.null activePeers))

        let (sharedState, decisions) = makeDecisions policy sharedCtx activePeers
        writeTVar sharedStateVar sharedState
        return (decisions, sharedState)
      traceWith tracer (DebugSharedTxState st)
      TxChannels { txChannelMap } <- readMVar txChannelsVar
      traverse_
        (\(mvar, d) -> modifyMVar_ mvar (\d' -> pure (d' <> d)))
        (Map.intersectionWith (,)
          txChannelMap
          decisions)
      go
