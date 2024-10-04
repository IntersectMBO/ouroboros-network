{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.Registry
  ( TxChannels (..)
  , TxChannelsVar
  , TxMempoolSem
  , SharedTxStateVar
  , newSharedTxStateVar
  , newTxChannelsVar
  , newTxMempoolSem
  , PeerTxAPI (..)
  , decisionLogicThread
  , drainRejectionThread
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Class.MonadTime.SI

import Data.Foldable (traverse_
#if !MIN_VERSION_base(4,20,0)
                     , foldl'
#endif
                     )
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Control.Tracer (Tracer, traceWith)
import Ouroboros.Network.DeltaQ (PeerGSV (..))
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.Decision
import Ouroboros.Network.TxSubmission.Inbound.Policy
import Ouroboros.Network.TxSubmission.Inbound.State
import Ouroboros.Network.TxSubmission.Inbound.Types
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

newtype TxMempoolSem m = TxMempoolSem (TSem m)

newTxMempoolSem :: MonadSTM m => m (TxMempoolSem m)
newTxMempoolSem = TxMempoolSem <$> atomically (newTSem 1)

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
                        -> m (Maybe TxSubmissionProtocolError),
    -- ^ handle received txs

    countRejectedTxs    :: Time
                        -> Double
                        -> m Double,

    withMempoolSem      :: m (Either (txid, tx) (txid, tx))
                        -> m (Either (txid, tx) (txid, tx))
  }


-- | A bracket function which registers / de-registers a new peer in
-- `SharedTxStateVar` and `PeerTxStateVar`s,  which exposes `PeerTxStateAPI`.
-- `PeerTxStateAPI` is only safe inside the  `withPeer` scope.
--
withPeer
    :: forall tx peeraddr txid idx m a.
       ( MonadMask m
       , MonadMVar m
       , MonadSTM  m
       , MonadMonotonicTime m
       , Ord txid
       , Show txid
       , Ord peeraddr
       , Show peeraddr
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> TxChannelsVar m peeraddr txid tx
    -> TxMempoolSem m
    -> TxDecisionPolicy
    -> SharedTxStateVar m peeraddr txid tx
    -> TxSubmissionMempoolReader txid tx idx m
    -> (tx -> SizeInBytes)
    -> peeraddr
    --  ^ new peer
    -> (PeerTxAPI m txid tx -> m a)
    -- ^ callback which gives access to `PeerTxStateAPI`
    -> m a
withPeer tracer
         channelsVar
         (TxMempoolSem mempoolSem)
         policy@TxDecisionPolicy { bufferedTxsMinLifetime }
         sharedStateVar
         TxSubmissionMempoolReader { mempoolGetSnapshot }
         txSize
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
                                handleReceivedTxs,
                                countRejectedTxs,
                                withMempoolSem
                                }
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
                 unknownTxs               = Set.empty,
                 score                    = 0,
                 scoreTs                  = Time 0,
                 downloadedTxs            = Map.empty,
                 toMempoolTxs             = Map.empty }
               peerTxStates
         }

    -- TODO: this function needs to be tested!
    unregisterPeer :: SharedTxState peeraddr txid tx
                   -> SharedTxState peeraddr txid tx
    unregisterPeer st@SharedTxState { peerTxStates,
                                      bufferedTxs,
                                      referenceCounts,
                                      inflightTxs,
                                      inflightTxsSize,
                                      limboTxs } =
        st { peerTxStates    = peerTxStates',
             bufferedTxs     = bufferedTxs',
             referenceCounts = referenceCounts',
             inflightTxs     = inflightTxs',
             inflightTxsSize = inflightTxsSize',
             limboTxs        = limboTxs' }
      where
        (PeerTxState { unacknowledgedTxIds, requestedTxsInflight,
                       requestedTxsInflightSize, toMempoolTxs }
                     , peerTxStates') =
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

        inflightTxs' = foldl purgeInflightTxs inflightTxs requestedTxsInflight
        inflightTxsSize' = inflightTxsSize - requestedTxsInflightSize

        limboTxs' =
          foldl' (flip $ Map.update
                             \cnt -> if cnt > 1
                                     then Just $! pred cnt
                                     else Nothing)
          limboTxs
          (Map.keysSet toMempoolTxs)

        purgeInflightTxs m txid = Map.alter fn txid m
          where
            fn (Just n) | n > 1 = Just $! pred n
            fn _                = Nothing
    --
    -- PeerTxAPI
    --

    withMempoolSem :: m (Either (txid,tx) (txid, tx)) -> m (Either (txid,tx) (txid,tx))
    withMempoolSem a =
        bracket_ (atomically $ waitTSem mempoolSem)
                 (atomically $ signalTSem mempoolSem)
                 (do
                     r <- a
                     now <- getMonotonicTime
                     atomically $ modifyTVar sharedStateVar (updateBufferedTx now r)
                     return r
                 )
      where
        updateBufferedTx :: Time
                         -> Either (txid, tx) (txid, tx)
                         -> SharedTxState peeraddr txid tx
                         -> SharedTxState peeraddr txid tx
        updateBufferedTx _ (Left (txid,_tx)) st@SharedTxState { peerTxStates
                                                             , limboTxs } =
            st { peerTxStates = peerTxStates'
               , limboTxs = limboTxs' }
          where
            limboTxs' = Map.update (\case
                                          1 -> Nothing
                                          n -> Just $! pred n) txid limboTxs

            peerTxStates' = Map.update fn peeraddr peerTxStates
              where
                fn ps = Just $! ps { toMempoolTxs = Map.delete txid (toMempoolTxs ps)}

        updateBufferedTx now (Right (txid, tx))
                         st@SharedTxState { peerTxStates
                                          , bufferedTxs
                                          , referenceCounts
                                          , timedTxs
                                          , limboTxs } =
            st { peerTxStates = peerTxStates'
               , bufferedTxs = bufferedTxs'
               , timedTxs = timedTxs'
               , referenceCounts = referenceCounts'
               , limboTxs = limboTxs'
               }
          where
            limboTxs' = Map.update (\case
                                          1 -> Nothing
                                          n -> Just $! pred n) txid limboTxs

            timedTxs' = Map.alter atf (addTime bufferedTxsMinLifetime now) timedTxs
              where
                atf :: Maybe [txid]
                    -> Maybe [txid]
                atf Nothing      = Just [txid]
                atf (Just txids) = Just $! (txid:txids)

            referenceCounts' = Map.alter afn txid referenceCounts
              where
                afn :: Maybe Int
                    -> Maybe Int
                afn Nothing  = Just 1
                afn (Just n) = Just $! succ n

            bufferedTxs' =  Map.insert txid (Just tx) bufferedTxs

            peerTxStates' = Map.update fn peeraddr peerTxStates
              where
                fn ps = Just $! ps { toMempoolTxs = Map.delete txid (toMempoolTxs ps)}

    handleReceivedTxIds :: NumTxIdsToReq
                        -> StrictSeq txid
                        -> Map txid SizeInBytes
                        -> m ()
    handleReceivedTxIds numTxIdsToReq txidsSeq txidsMap =
      receivedTxIds tracer
                    sharedStateVar
                    mempoolGetSnapshot
                    peeraddr
                    numTxIdsToReq
                    txidsSeq
                    txidsMap


    handleReceivedTxs :: Set txid
                      -- ^ requested txids
                      -> Map txid tx
                      -- ^ received txs
                      -> m (Maybe TxSubmissionProtocolError)
    handleReceivedTxs txids txs =
      collectTxs tracer txSize sharedStateVar peeraddr txids txs

    countRejectedTxs :: Time
                     -> Double
                     -> m Double
    countRejectedTxs now n = atomically $ do
      modifyTVar sharedStateVar cntRejects
      st <- readTVar sharedStateVar
      case Map.lookup peeraddr (peerTxStates st)  of
           Nothing -> error ("TxSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
           Just ps -> return $ score ps
       where
        cntRejects :: SharedTxState peeraddr txid tx
                   -> SharedTxState peeraddr txid tx
        cntRejects st@SharedTxState { peerTxStates } =
          let peerTxStates' = Map.update (\ps -> Just $! updateRejects policy now n ps) peeraddr peerTxStates in
          st {peerTxStates    = peerTxStates'}

updateRejects :: TxDecisionPolicy
              -> Time
              -> Double
              -> PeerTxState txid tx
              -> PeerTxState txid tx
updateRejects _ now 0 pts | score pts == 0 = pts {scoreTs = now}
updateRejects TxDecisionPolicy { scoreRate, scoreMax } now n
              pts@PeerTxState { score, scoreTs } =
    let duration = diffTime now scoreTs
        !drain = realToFrac duration * scoreRate
        !drained = max 0 $ score - drain in
    pts { score = max 0 $ min scoreMax $ drained + n
        , scoreTs = now }

drainRejectionThread
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadSTM m
       , MonadThread m
       , Ord txid
       )
    => TxDecisionPolicy
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
drainRejectionThread policy sharedStateVar = do
    labelThisThread "tx-rejection-drain"
    now <- getMonotonicTime
    go $ addTime drainInterval now
  where
    drainInterval :: DiffTime
    drainInterval = 7

    go :: Time -> m Void
    go !nextDrain = do
      threadDelay 1

      !now <- getMonotonicTime
      atomically $ do
        st <- readTVar sharedStateVar
        let ptss = if now > nextDrain then Map.map (updateRejects policy now 0) (peerTxStates st)
                                      else peerTxStates st
            st' = tickTimedTxs now st
        writeTVar sharedStateVar (st' { peerTxStates = ptss })

      if now > nextDrain
         then go $ addTime drainInterval now
         else go nextDrain

decisionLogicThread
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadMVar  m
       , MonadSTM   m
       , MonadMask  m
       , MonadFork  m
       , Ord peeraddr
       , Ord txid
       , Hashable peeraddr
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> TxDecisionPolicy
    -> STM m (Map peeraddr PeerGSV)
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
decisionLogicThread tracer policy readGSVVar txChannelsVar sharedStateVar = do
    labelThisThread "tx-decision"
    go
  where
    go :: m Void
    go = do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      threadDelay 0.005 -- 5ms

      (decisions, st) <- atomically do
        sharedCtx <-
              SharedDecisionContext
          <$> readGSVVar
          <*> readTVar sharedStateVar
        let activePeers = filterActivePeers policy (sdcSharedTxState sharedCtx)

        -- block until at least one peer is active
        check (not (Map.null activePeers))

        let (sharedState, decisions) = makeDecisions policy sharedCtx activePeers
        writeTVar sharedStateVar sharedState
        return (decisions, sharedState)
      traceWith tracer (TraceSharedTxState "decisionLogicThread" st)
      traceWith tracer (TraceTxDecisions decisions)
      TxChannels { txChannelMap } <- readMVar txChannelsVar
      traverse_
        (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
        (Map.intersectionWith (,)
          txChannelMap
          decisions)
      go

    -- Variant of modifyMVar_ that puts a default value if the MVar is empty.
    modifyMVarWithDefault_ :: StrictMVar m a -> a -> (a -> m a) -> m ()
    modifyMVarWithDefault_ m d io =
      mask $ \restore -> do
        mbA  <- tryTakeMVar m
        case mbA of
          Just a -> do
            a' <- restore (io a) `onException` putMVar m a
            putMVar m a'
          Nothing -> putMVar m d
