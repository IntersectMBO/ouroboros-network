{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.TxSubmission.Inbound.V2.Registry
  ( TxChannels (..)
  , TxChannelsVar
  , TxMempoolSem
  , SharedTxStateVar
  , newSharedTxStateVar
  , newTxChannelsVar
  , newTxMempoolSem
  , PeerTxAPI (..)
  , decisionLogicThreads
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM qualified as Lazy
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Data.Foldable as Foldable (foldl', traverse_)
import Data.Hashable
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Word (Word64)
import Data.Void (Void)

import Control.Tracer (Tracer, traceWith)
import Ouroboros.Network.Protocol.TxSubmission2.Type
import Ouroboros.Network.TxSubmission.Inbound.V2.Decision
import Ouroboros.Network.TxSubmission.Inbound.V2.Policy
import Ouroboros.Network.TxSubmission.Inbound.V2.State
import Ouroboros.Network.TxSubmission.Inbound.V2.Types
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

bumpGeneration :: SharedTxState peeraddr txid tx -> SharedTxState peeraddr txid tx
bumpGeneration st@SharedTxState { generation } = st { generation = generation + 1 }

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

    handleReceivedTxs   :: Map txid SizeInBytes
                        -- ^ requested txids
                        -> Map txid tx
                        -- ^ received txs
                        -> m (Maybe TxSubmissionProtocolError),
    -- ^ handle received txs

    submitTxsToMempool     :: Tracer m (TraceTxSubmissionInbound txid tx)
                          -> [(txid, tx)]
                          -> m ()
    -- ^ submit the given txs to the mempool.
  }


data TxMempoolResult = TxAccepted | TxRejected

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
       , Typeable txid
       , Ord peeraddr
       , Show peeraddr
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> TxChannelsVar m peeraddr txid tx
    -> TxMempoolSem m
    -> TxDecisionPolicy
    -> SharedTxStateVar m peeraddr txid tx
    -> TxSubmissionMempoolReader txid tx idx m
    -> TxSubmissionMempoolWriter txid tx idx m
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
         TxSubmissionMempoolWriter { mempoolAddTxs }
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
                                submitTxsToMempool }
                  )

          atomically $ modifyTVar sharedStateVar (bumpGeneration . registerPeer)
          return peerTxAPI
      )
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      (\_ -> uninterruptibleMask_ do
        atomically $ modifyTVar sharedStateVar (bumpGeneration . unregisterPeer)
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
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeer :: SharedTxState peeraddr txid tx
                   -> SharedTxState peeraddr txid tx
    unregisterPeer st@SharedTxState { peerTxStates,
                                      bufferedTxs,
                                      referenceCounts,
                                      inflightTxs,
                                      inflightTxsSize,
                                      inSubmissionToMempoolTxs } =
        st { peerTxStates             = peerTxStates',
             bufferedTxs              = bufferedTxs',
             referenceCounts          = referenceCounts',
             inflightTxs              = inflightTxs',
             inflightTxsSize          = inflightTxsSize',
             inSubmissionToMempoolTxs = inSubmissionToMempoolTxs' }
      where
        (PeerTxState { unacknowledgedTxIds,
                       requestedTxsInflight,
                       requestedTxsInflightSize,
                       toMempoolTxs }
          , peerTxStates')
          =
          Map.alterF
            (\case
              Nothing -> error ("TxSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
              Just a  -> (a, Nothing))
            peeraddr
            peerTxStates

        referenceCounts' =
          Foldable.foldl'
            (flip $ Map.update \cnt ->
              if cnt > 1
              then Just $! pred cnt
              else Nothing
            )
          referenceCounts
          unacknowledgedTxIds

        liveSet = Map.keysSet referenceCounts'

        bufferedTxs' = bufferedTxs
                       `Map.restrictKeys`
                       liveSet

        inflightTxs' = Foldable.foldl' purgeInflightTxs inflightTxs requestedTxsInflight
        inflightTxsSize' = inflightTxsSize - requestedTxsInflightSize

        -- When we unregister a peer, we need to subtract all txs in the
        -- `toMempoolTxs`, as they will not be submitted to the mempool.
        inSubmissionToMempoolTxs' =
          Foldable.foldl' (flip $ Map.update \cnt ->
               if cnt > 1
               then Just $! pred cnt
               else Nothing
            )
          inSubmissionToMempoolTxs
          (Map.keysSet toMempoolTxs)

        purgeInflightTxs m txid = Map.alter fn txid m
          where
            fn (Just a ) | inFlightCount a > 1 = Just $! a { inFlightCount = inFlightCount a - 1 }
            fn _                               = Nothing

    --
    -- PeerTxAPI
    --

    submitTxsToMempool :: Tracer m (TraceTxSubmissionInbound txid tx)
                       -> [(txid, tx)]
                       -> m ()
    submitTxsToMempool _ [] = return ()
    submitTxsToMempool txTracer txs =
        bracket_ (atomically $ waitTSem mempoolSem)
                 (atomically $ signalTSem mempoolSem)
          $ do
            start <- getMonotonicTime
            mpSnapshot <- atomically mempoolGetSnapshot

            -- Note that checking if the mempool contains a TX before
            -- spending several ms attempting to add it to the pool has
            -- been judged immoral.
            let toSubmit =
                  [ (txid, tx)
                  | (txid, tx) <- txs
                  , not (mempoolHasTx mpSnapshot txid)
                  ]
                toSubmitTxs = map snd toSubmit

            acceptedTxIds <-
              if null toSubmitTxs
                 then return []
                 else mempoolAddTxs toSubmitTxs

            end <- getMonotonicTime
            let duration = end `diffTime` start
                acceptedSet = Set.fromList acceptedTxIds
                isAccepted txid = txid `Set.member` acceptedSet
                rejectedTxIds = [ txid | (txid, _) <- txs, not (isAccepted txid) ]
                acceptedCount = length acceptedTxIds
                rejectedCount = length rejectedTxIds

            !s <- countRejectedTxs end (fromIntegral rejectedCount)
            traceWith txTracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                ptxcAccepted = acceptedCount
              , ptxcRejected = rejectedCount
              , ptxcScore    = s
              }

            atomically $ modifyTVar sharedStateVar $ \st ->
              bumpGeneration $ Foldable.foldl' (updateBufferedTx end acceptedSet) st txs

            when (not $ List.null acceptedTxIds) $
              traceWith txTracer (TraceTxInboundAddedToMempool acceptedTxIds duration)

            when (not $ List.null rejectedTxIds) $
              traceWith txTracer (TraceTxInboundRejectedFromMempool rejectedTxIds duration)

      where
        updateBufferedTx :: Time
                         -> Set txid
                         -> SharedTxState peeraddr txid tx
                         -> (txid, tx)
                         -> SharedTxState peeraddr txid tx
        updateBufferedTx now acceptedSet st (txid, tx) =
            let res = if txid `Set.member` acceptedSet
                         then TxAccepted
                         else TxRejected
            in updateBufferedTxResult now res txid tx st

        updateBufferedTxResult :: Time
                               -> TxMempoolResult
                               -> txid
                               -> tx
                               -> SharedTxState peeraddr txid tx
                               -> SharedTxState peeraddr txid tx
        updateBufferedTxResult _ TxRejected txid _ st@SharedTxState { peerTxStates
                                                       , inSubmissionToMempoolTxs } =
            st { peerTxStates = peerTxStates'
               , inSubmissionToMempoolTxs = inSubmissionToMempoolTxs' }
          where
            inSubmissionToMempoolTxs' =
              Map.update (\case 1 -> Nothing; n -> Just $! pred n)
                         txid inSubmissionToMempoolTxs

            peerTxStates' = Map.update fn peeraddr peerTxStates
              where
                fn ps = Just $! ps { toMempoolTxs = Map.delete txid (toMempoolTxs ps)}

        updateBufferedTxResult now TxAccepted txid tx
                         st@SharedTxState { peerTxStates
                                          , bufferedTxs
                                          , referenceCounts
                                          , timedTxs
                                          , inSubmissionToMempoolTxs } =
            st { peerTxStates = peerTxStates'
               , bufferedTxs = bufferedTxs'
               , timedTxs = timedTxs'
               , referenceCounts = referenceCounts'
               , inSubmissionToMempoolTxs = inSubmissionToMempoolTxs'
               }
          where
            inSubmissionToMempoolTxs' =
              Map.update (\case 1 -> Nothing; n -> Just $! pred n)
                         txid inSubmissionToMempoolTxs

            timedTxs' = Map.alter fn (addTime bufferedTxsMinLifetime now) timedTxs
              where
                fn :: Maybe [txid] -> Maybe [txid]
                fn Nothing      = Just [txid]
                fn (Just txids) = Just $! (txid:txids)

            referenceCounts' = Map.alter fn txid referenceCounts
              where
                fn :: Maybe Int -> Maybe Int
                fn Nothing  = Just 1
                fn (Just n) = Just $! succ n

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


    handleReceivedTxs :: Map txid SizeInBytes
                      -- ^ requested txids with their announced size
                      -> Map txid tx
                      -- ^ received txs
                      -> m (Maybe TxSubmissionProtocolError)
    handleReceivedTxs txids txs =
      collectTxs tracer txSize sharedStateVar peeraddr txids txs

    -- Update `score` & `scoreTs` fields of `PeerTxState`, return the new
    -- updated `score`.
    --
    -- PRECONDITION: the `Double` argument is non-negative.
    countRejectedTxs :: Time
                     -> Double
                     -> m Double
    countRejectedTxs _ n | n < 0 =
      error ("TxSubmission.countRejectedTxs: invariant violation for peer " ++ show peeraddr)
    countRejectedTxs now n = atomically $ stateTVar sharedStateVar $ \st ->
        let (result, peerTxStates') = Map.alterF fn peeraddr (peerTxStates st)
        in (result, bumpGeneration st { peerTxStates = peerTxStates' })
      where
        fn :: Maybe (PeerTxState txid tx) -> (Double, Maybe (PeerTxState txid tx))
        fn Nothing   = error ("TxSubmission.withPeer: invariant violation for peer " ++ show peeraddr)
        fn (Just ps) = (score ps', Just $! ps')
          where
            ps' = updateRejects policy now n ps


updateRejects :: TxDecisionPolicy
              -> Time
              -> Double
              -> PeerTxState txid tx
              -> PeerTxState txid tx
updateRejects _ now 0 pts | score pts == 0 = pts {scoreTs = now}
updateRejects TxDecisionPolicy { scoreRate, scoreMax } now n
              pts@PeerTxState { score, scoreTs } =
    let duration = diffTime now scoreTs
        !drain   = realToFrac duration * scoreRate
        !drained = max 0 $ score - drain in
    pts { score   = min scoreMax $ drained + n
        , scoreTs = now
        }


drainRejectionThread
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadSTM m
       , MonadThread m
       , Ord txid
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> TxDecisionPolicy
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
drainRejectionThread tracer policy sharedStateVar = do
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
      st''' <- atomically $ do
        st <- readTVar sharedStateVar
        let ptss = if now > nextDrain then Map.map (updateRejects policy now 0) (peerTxStates st)
                                      else peerTxStates st
            st' = tickTimedTxs now st
                    { peerTxStates = ptss }
            st'' = st' { inflightTxs = Map.filter (filterStaleReq now) (inflightTxs st')}
        writeTVar sharedStateVar (bumpGeneration st'')
        return st''
      traceWith tracer (TraceSharedTxState "drainRejectionThread" st''')

      if now > nextDrain
         then go $ addTime drainInterval now
         else go nextDrain

    filterStaleReq :: Time -> InFlightState -> Bool
    filterStaleReq now e = inFlightCount e > 0 || inFlightNextReq e > now

decisionLogicThread
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadMVar  m
       , MonadMask  m
       , MonadFork  m
       , MonadTimer  m
       , Ord peeraddr
       , Ord txid
       , Hashable peeraddr
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> Tracer m TxSubmissionCounters
    -> TxDecisionPolicy
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
decisionLogicThread tracer counterTracer policy txChannelsVar sharedStateVar = do
    labelThisThread "tx-decision"
    initialGeneration <- atomically $ generation <$> readTVar sharedStateVar
    go initialGeneration
  where
    go :: Word64 -> m Void
    go lastSeen = do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      threadDelay _DECISION_LOOP_DELAY

      now <- getMonotonicTime
      nextDelay <- atomically $ do
        sharedTxState <- readTVar sharedStateVar
        return $ nextDecisionDelay now sharedTxState
      delayVar <- registerDelay nextDelay
      -- Wait until there is potentially some work to do or the timer expires.
      atomically do
        sharedTxState <- readTVar sharedStateVar
        let newGeneration = generation sharedTxState > lastSeen
        timerExpired <- Lazy.readTVar delayVar

        -- block until state changes or the timer expires
        if newGeneration
           then return ()
        else if timerExpired
           then return ()
           else retry

      -- Use a fresh timestamp for decisions
      now' <- getMonotonicTime
      res_m <- atomically do
        sharedTxState <- readTVar sharedStateVar
        let activePeers = filterActivePeers now' policy sharedTxState

        if Map.null activePeers
           then return (Left (generation sharedTxState))
           else do
             let (sharedState, decisions) = makeDecisions now' policy sharedTxState activePeers
             writeTVar sharedStateVar sharedState
             return $ Right (decisions, sharedState)

      case res_m of
           Left lastSeen' -> go lastSeen'
           Right (decisions, st) -> do
             traceWith tracer (TraceSharedTxState "decisionLogicThread" st)
             traceWith tracer (TraceTxDecisions decisions)
             TxChannels { txChannelMap } <- readMVar txChannelsVar
             traverse_
               (\(mvar, d) -> modifyMVarWithDefault_ mvar d (\d' -> pure (d' <> d)))
               (Map.intersectionWith (,)
               txChannelMap
               decisions)
             traceWith counterTracer (mkTxSubmissionCounters st)
             go (generation st)

    nextDecisionDelay
      :: Time
      -> SharedTxState peeraddr txid tx
      -> DiffTime
    nextDecisionDelay now SharedTxState { inflightTxs } =
      fromMaybe maxDelay (diffTimeNow <$> nextWake)
      where
        -- If there are no outstanding TXs we wait for a long time
        -- or until an STM value changes.
        maxDelay :: DiffTime
        maxDelay = 120

        nextWake :: Maybe Time
        nextWake =
          Foldable.foldl' step Nothing inflightTxs

        step :: Maybe Time -> InFlightState -> Maybe Time
        step acc InFlightState { inFlightNextReq } =
          if inFlightNextReq <= now
             then acc
             else Just $ maybe inFlightNextReq (min inFlightNextReq) acc

        diffTimeNow :: Time -> DiffTime
        diffTimeNow t = t `diffTime` now

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


-- | Run `decisionLogicThread` and `drainRejectionThread`.
--
decisionLogicThreads
    :: forall m peeraddr txid tx.
       ( MonadDelay m
       , MonadMVar  m
       , MonadMask  m
       , MonadAsync m
       , MonadFork  m
       , MonadTimer  m
       , Ord peeraddr
       , Ord txid
       , Hashable peeraddr
       )
    => Tracer m (TraceTxLogic peeraddr txid tx)
    -> Tracer m TxSubmissionCounters
    -> TxDecisionPolicy
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
decisionLogicThreads tracer counterTracer policy txChannelsVar sharedStateVar =
  uncurry (<>) <$>
    drainRejectionThread tracer policy sharedStateVar
    `concurrently`
    decisionLogicThread tracer counterTracer policy txChannelsVar sharedStateVar


-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
