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
  , decisionLogicThread
  , drainRejectionThread
  , withPeer
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Data.Foldable as Foldable (foldl', traverse_)
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
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

    submitTxToMempool     :: Tracer m (TraceTxSubmissionInbound txid tx)
                          -> txid -> tx -> m ()
    -- ^ submit the given (txid, tx) to the mempool.
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
                                submitTxToMempool }
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
            fn (Just n) | n > 1 = Just $! pred n
            fn _                = Nothing

    --
    -- PeerTxAPI
    --

    submitTxToMempool :: Tracer m (TraceTxSubmissionInbound txid tx) -> txid -> tx -> m ()
    submitTxToMempool txTracer txid tx =
        bracket_ (atomically $ waitTSem mempoolSem)
                 (atomically $ signalTSem mempoolSem)
          $ do
            res <- addTx
            start <- getMonotonicTime
            atomically $ modifyTVar sharedStateVar (updateBufferedTx start res)
            end <- getMonotonicTime
            let duration = end `diffTime` start
            case res of
              TxAccepted -> traceWith txTracer (TraceTxInboundAddedToMempool [txid] duration)
              TxRejected -> traceWith txTracer (TraceTxInboundRejectedFromMempool [txid] duration)

      where
        -- add the tx to the mempool
        addTx :: m TxMempoolResult
        addTx = do
          mpSnapshot <- atomically mempoolGetSnapshot

          -- Note that checking if the mempool contains a TX before
          -- spending several ms attempting to add it to the pool has
          -- been judged immoral.
          if mempoolHasTx mpSnapshot txid
             then do
               !now <- getMonotonicTime
               !s <- countRejectedTxs now 1
               traceWith txTracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                    ptxcAccepted = 0
                  , ptxcRejected = 1
                  , ptxcScore    = s
                  }
               return TxRejected
             else do
               acceptedTxs <- mempoolAddTxs [tx]
               end <- getMonotonicTime
               if null acceptedTxs
                  then do
                      !s <- countRejectedTxs end 1
                      traceWith txTracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                          ptxcAccepted = 0
                        , ptxcRejected = 1
                        , ptxcScore    = s
                        }
                      return TxRejected
                  else do
                      !s <- countRejectedTxs end 0
                      traceWith txTracer $ TraceTxSubmissionProcessed ProcessedTxCount {
                          ptxcAccepted = 1
                        , ptxcRejected = 0
                        , ptxcScore    = s
                        }
                      return TxAccepted

        updateBufferedTx :: Time
                         -> TxMempoolResult
                         -> SharedTxState peeraddr txid tx
                         -> SharedTxState peeraddr txid tx
        updateBufferedTx _ TxRejected st@SharedTxState { peerTxStates
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

        updateBufferedTx now TxAccepted
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


    handleReceivedTxs :: Set txid
                      -- ^ requested txids
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
        in (result, st { peerTxStates = peerTxStates' })
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
    -> TxChannelsVar m peeraddr txid tx
    -> SharedTxStateVar m peeraddr txid tx
    -> m Void
decisionLogicThread tracer policy txChannelsVar sharedStateVar = do
    labelThisThread "tx-decision"
    go
  where
    go :: m Void
    go = do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      threadDelay 0.005 -- 5ms

      (decisions, st) <- atomically do
        sharedTxState <- readTVar sharedStateVar
        let activePeers = filterActivePeers policy sharedTxState

        -- block until at least one peer is active
        check (not (Map.null activePeers))

        let (sharedState, decisions) = makeDecisions policy sharedTxState activePeers
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
