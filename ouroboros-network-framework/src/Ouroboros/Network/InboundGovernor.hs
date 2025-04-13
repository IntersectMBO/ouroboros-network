{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | Server implementation based on 'ConnectionManager'
--
-- This module should be imported qualified.
--
module Ouroboros.Network.InboundGovernor
  ( -- * Run Inbound Protocol Governor
    PublicState (..)
  , Arguments (..)
  , with
    -- * Trace
  , Trace (..)
  , Debug (..)
  , Event (..)
  , NewConnectionInfo (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  , AcceptConnectionsPolicyTrace (..)
    -- * Re-exports
  , Transition' (..)
  , TransitionTrace' (..)
  , ResponderCounters (..)
    -- * API's exported for testing purposes
  , maturedPeers
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException (..))
import Control.Monad (foldM, forM_, forever, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer (..), traceWith)

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Cache
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict
import Data.Monoid.Synchronisation
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Network.Mux qualified as Mux
import Network.Mux.Types qualified as Mux

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.InboundGovernor.InformationChannel (InformationChannel)
import Ouroboros.Network.InboundGovernor.InformationChannel qualified as InfoChannel
import Ouroboros.Network.InboundGovernor.State
import Ouroboros.Network.Mux
import Ouroboros.Network.Server.RateLimiting

-- | Period of time after which a peer transitions from a fresh to a mature one,
-- see `matureDuplexPeers` and `freshDuplexPeers`.
--
inboundMaturePeerDelay :: DiffTime
inboundMaturePeerDelay = 15 * 60


-- | Every ~30s we wake up the inbound governor.  This is to give a chance to
-- mark some of the inbound connections as mature.
--
inactionTimeout :: DiffTime
inactionTimeout = 31.415927


data Arguments muxMode handlerTrace socket peerAddr initiatorCtx responderCtx
               handle handleError versionNumber versionData bytes m a b x =
  Arguments {
      transitionTracer   :: Tracer m (RemoteTransitionTrace peerAddr),
      -- ^ transition tracer
      tracer             :: Tracer m (Trace peerAddr),
      -- ^ main inbound governor tracer
      debugTracer        :: Tracer m (Debug peerAddr versionData),
      -- ^ debug inbound governor tracer
      connectionDataFlow :: versionData -> DataFlow,
      -- ^ connection data flow
      infoChannel        :: InboundGovernorInfoChannel muxMode initiatorCtx peerAddr versionData ByteString m a b,
      -- ^ 'InformationChannel' which passes 'NewConnectionInfo' for outbound
      -- connections from connection manager to the inbound governor.
      idleTimeout        :: Maybe DiffTime,
      -- ^ protocol idle timeout.  The remote site must restart a mini-protocol
      -- within given timeframe (Nothing indicates no timeout).
      withConnectionManager
        :: ConnectionHandler muxMode handlerTrace socket peerAddr handle handleError versionNumber versionData m
        -> (ConnectionManager muxMode socket peerAddr handle handleError m -> m x)
        -> m x,
      -- ^ connection manager continuation
      mkConnectionHandler
        :: (   StrictTVar m (StrictMaybe ResponderCounters)
            -> Tracer m (Mux.WithBearer (ConnectionId peerAddr) Mux.Trace))
        -> ConnectionHandler muxMode handlerTrace socket peerAddr handle handleError versionNumber versionData m
      -- ^ Connection handler builder, which injects a special tracer
      -- created here and routed into the muxer via the connection manager.
      -- The purpose is to inform the IG loop
      -- of miniprotocol responder activity such that proper and efficient
      -- peer cold/warm/hot transitions can be tracked.
    }


-- | Run the server, which consists of the following components:
--
-- * /inbound governor/, it corresponds to p2p-governor on outbound side
-- * /accept loop(s)/, one per given ip address.  We support up to one ipv4
--   address and up to one ipv6 address, i.e. an ipv6 enabled node will run two
--   accept loops on listening on different addresses with shared /inbound governor/.
--
-- The server can be run in either of two 'Network.Mux.Mode'-es:
--
-- * 'InitiatorResponderMode'
-- * 'ResponderMode'
--
-- The first one is used in data diffusion for /Node-To-Node protocol/, while the
-- other is useful for running a server for the /Node-To-Client protocol/.
--
with :: forall (muxMode :: Mux.Mode) socket peerAddr initiatorCtx responderCtx
               handle handlerTrace handleError versionData versionNumber bytes m a b x.
        ( Alternative (STM m)
        , MonadAsync       m
        , MonadCatch       m
        , MonadEvaluate    m
        , MonadLabelledSTM m
        , MonadThrow       m
        , MonadThrow  (STM m)
        , MonadTime        m
        , MonadTimer       m
        , MonadMask        m
        , Ord peerAddr
        , HasResponder muxMode ~ True
        , MonadTraceSTM m
        , MonadFork m
        , MonadDelay m
        , Show peerAddr
        )
     => Arguments muxMode handlerTrace socket peerAddr initiatorCtx responderCtx
                  handle handleError versionNumber versionData bytes m a b x
     -> (   Async m Void
         -> m (PublicState peerAddr versionData)
         -> ConnectionManager muxMode socket peerAddr handle handleError m
         -> m x)
     -> m x
with
    Arguments {
      transitionTracer = trTracer,
      tracer,
      debugTracer,
      connectionDataFlow,
      idleTimeout,
      infoChannel,
      withConnectionManager,
      mkConnectionHandler
    }
    k
    = do
    stateVar <- newTVarIO emptyState
    active   <- newTVarIO True -- ^ inbound governor status: True = Active
    let connectionHandler =
          mkConnectionHandler $ inboundGovernorMuxTracer infoChannel
                                                         connectionDataFlow
                                                         stateVar
                                                         active
    withConnectionManager connectionHandler \connectionManager ->
      withAsync
        (  labelThisThread "inbound-governor-loop" >>
           forever (inboundGovernorStep connectionManager stateVar >> yield)
         `catch` \e -> do
           -- following the next statement, the ig tracer will no longer
           -- write to the info channel queue.
           atomically $ writeTVar active False
           -- To avoid the risk of a full information channel queue
           -- and blocking on mux traces which will prevent connection cleanup,
           -- we drain it here just in case one last time.
           _ <- atomically $ InfoChannel.readMessages infoChannel
           handleError stateVar e)
      \thread ->
        k thread (mkPublicState <$> readTVarIO stateVar) connectionManager
  where
    emptyState :: State muxMode initiatorCtx peerAddr versionData m a b
    emptyState = State {
        connections       = Map.empty,
        matureDuplexPeers = Map.empty,
        freshDuplexPeers  = OrdPSQ.empty,
        countersCache     = mempty
      }

    -- Trace final transition mostly for testing purposes.
    --
    -- NOTE: `inboundGovernorLoop` doesn't throw synchronous exceptions, this is
    -- just need to handle asynchronous exceptions.
    handleError
      :: StrictTVar m (State muxMode initiatorCtx peerAddr versionData m a b)
      -> SomeException
      -> m Void
    handleError var e = do
      PublicState { remoteStateMap } <- mkPublicState <$> readTVarIO var
      _ <- Map.traverseWithKey
             (\connId remoteSt ->
               traceWith trTracer $
                 TransitionTrace (remoteAddress connId)
                 Transition { fromState = Just remoteSt,
                              toState   = Nothing }
             )
             remoteStateMap
      throwIO e

    -- The inbound protocol governor single step, which may
    -- process multipe events from the information channel
    inboundGovernorStep
      :: ConnectionManager muxMode socket peerAddr handle handleError m
      -> StrictTVar m (State muxMode initiatorCtx peerAddr versionData m a b)
      -> m ()
    inboundGovernorStep connectionManager stateVar = do
      time <- getMonotonicTime
      inactivityVar <- registerDelay inactionTimeout
      events <- atomically do
        state <- readTVar stateVar
        runFirstToFinish $
            -- we deliberately read the info channel queue after
            -- the relevant item in each firsttofinish to limit
            -- contention
            FirstToFinish do
              -- mark connections as mature
              case maturedPeers time (freshDuplexPeers state) of
                (as, _)     | Map.null as
                            -> retry
                (as, fresh) ->
                  (MaturedDuplexPeers as fresh :) <$> InfoChannel.readMessages infoChannel
         <> FirstToFinish do
              firstCommit <- runFirstToFinish $
                Map.foldMapWithKey firstPeerCommitRemote (connections state)
              -- it is important we read the channel here, and join it after
              -- the firstCommit. Registering responder starts are atomic wrt
              -- handling an expired peer in the tracer. If the CM drops the
              -- expired connection (CommitRemote below), the tracer must not
              -- register a promotion activity.
              (firstCommit :) <$> InfoChannel.readMessages infoChannel
         <> FirstToFinish do
              muxEvents <- InfoChannel.readMessages infoChannel
              check (not . null $ muxEvents) >> pure muxEvents
         <> FirstToFinish do
              -- spin the inbound governor loop; it will re-run with new
              -- time, which allows to make some peers mature.
              LazySTM.readTVar inactivityVar >>= check >> pure [InactivityTimeout]

      forM_ events \event -> do
        state <- readTVarIO stateVar
        decision <- case event of
          NewConnection
            -- new connection has been announced by either accept loop or
            -- by connection manager (in which case the connection is in
            -- 'DuplexState').
            (NewConnectionInfo
              provenance
              connId
              dataFlow
              Handle {
                hMux         = csMux,
                hMuxBundle   = muxBundle,
                hVersionData = csVersionData
              }) -> do

                traceWith tracer (TrNewConnection provenance connId)
                let responderContext = ResponderContext { rcConnectionId = connId }

                connections <- Map.alterF
                  (\case
                    -- connection
                    Nothing -> do
                      let csMPMHot =
                            [ ( miniProtocolNum mpH
                              , MiniProtocolData mpH responderContext Hot
                              )
                            | mpH <- projectBundle SingHot muxBundle
                            ]
                          csMPMWarm =
                            [ ( miniProtocolNum mpW
                              , MiniProtocolData mpW responderContext Warm
                              )
                            | mpW <- projectBundle SingWarm muxBundle
                            ]
                          csMPMEstablished =
                            [ ( miniProtocolNum mpE
                              , MiniProtocolData mpE responderContext Established
                              )
                            | mpE <- projectBundle SingEstablished muxBundle
                            ]
                          csMiniProtocolMap =
                              Map.fromList
                              (csMPMHot ++ csMPMWarm ++ csMPMEstablished)

                      mCompletionMap
                        <-
                        foldM
                          (\acc mpd@MiniProtocolData { mpdMiniProtocol } ->
                            runResponder csMux mpd >>= \case
                              -- synchronous exceptions when starting
                              -- a mini-protocol are non-recoverable; we
                              -- close the connection and allow the server
                              -- to continue.
                              Left err -> do
                                traceWith tracer (TrResponderStartFailure connId (miniProtocolNum mpdMiniProtocol) err)
                                Mux.stop csMux
                                return Nothing

                              Right completion ->  do
                                let acc' = Map.insert (miniProtocolNum mpdMiniProtocol)
                                                      completion
                                       <$> acc
                                -- force under lazy 'Maybe'
                                case acc' of
                                  Just !_ -> return acc'
                                  Nothing -> return acc'
                          )
                          (Just Map.empty)
                          csMiniProtocolMap

                      case mCompletionMap of
                        -- there was an error when starting one of the
                        -- responders, we let the server continue without this
                        -- connection.
                        Nothing -> return Nothing

                        Just csCompletionMap -> do
                          mv <- traverse registerDelay idleTimeout
                          let -- initial state is 'RemoteIdle', if the remote end will not
                              -- start any responders this will unregister the inbound side.
                              csRemoteState :: RemoteState m
                              csRemoteState = RemoteIdle (case mv of
                                                            Nothing -> pure False
                                                            Just v  -> LazySTM.readTVar v)

                              connState = ConnectionState {
                                  csMux,
                                  csVersionData,
                                  csMiniProtocolMap,
                                  csCompletionMap,
                                  csRemoteState
                                }

                          return (Just connState)

                    -- inbound governor might be notified about a connection
                    -- which is already tracked.  In such case we preserve its
                    -- state.
                    --
                    -- In particular we preserve an ongoing timeout on
                    -- 'RemoteIdle' state.
                    Just connState -> return (Just connState)

                  )
                  connId
                  (connections state)

                -- update state and continue the recursive loop
                let state' = state {
                        connections,
                        freshDuplexPeers =
                          case dataFlow of
                            Unidirectional -> freshDuplexPeers state
                            Duplex         -> OrdPSQ.insert (remoteAddress connId) time csVersionData
                                                            (freshDuplexPeers state)
                      }
                return . Just $ StateWithPeerTransition state' connId

          MuxFinished connId result -> do

            merr <- atomically result
            case merr of
              Nothing  -> traceWith tracer (TrMuxCleanExit connId)
              Just err -> traceWith tracer (TrMuxErrored connId err)

            -- the connection manager does should realise this on itself.
            -- we bypass the assertion check since MuxFinished could have been
            -- placed on the queue by a racing thread before we managed
            -- to remove the connection from our state at the end of this loop.
            let state' = unregisterConnection True connId state
            return . Just $ StateWithPeerTransition state' connId -- ^ even though it might not be true, but it's benign

          MiniProtocolTerminated
            Terminated {
                tConnId,
                tMux,
                tMiniProtocolData = mpd@MiniProtocolData { mpdMiniProtocol = miniProtocol },
                tResult
              } -> do
            tResult' <- atomically tResult
            let num = miniProtocolNum miniProtocol
            case tResult' of
              Left e -> do
                -- a mini-protocol errored.  In this case mux will shutdown, and
                -- the connection manager will tear down the socket. Before bailing out,
                -- the IG tracer will emit BearState Dead which will unregister the connection
                -- in some following iteration via MuxFinished, but for this peer it should
                -- be the very next message.
                traceWith tracer $
                  TrResponderErrored tConnId num e
                return Nothing

              Right _ ->
                runResponder tMux mpd >>= \case
                  Right completionAction -> do
                    traceWith tracer (TrResponderRestarted tConnId num)
                    let state' = updateMiniProtocol tConnId num completionAction state
                    return . Just $ OnlyStateChange state'

                  Left err -> do
                    -- there is no way to recover from synchronous exceptions; we
                    -- stop mux which allows to close resources held by
                    -- connection manager.
                    traceWith tracer (TrResponderStartFailure tConnId num err)
                    Mux.stop tMux
                    return Nothing

          WaitIdleRemote connId -> do
            -- @
            --    DemotedToCold^{dataFlow}_{Remote} : InboundState Duplex
            --                                      → InboundIdleState Duplex
            -- @
            -- NOTE: `demotedToColdRemote` doesn't throw, hence exception handling
            -- is not needed.
            res <- demotedToColdRemote connectionManager connId
            traceWith tracer (TrWaitIdleRemote connId res)
            case res of
              OperationSuccess {}  -> do
                mv <- traverse registerDelay idleTimeout
                let timeoutSTM :: STM m Bool
                    !timeoutSTM = case mv of
                      Nothing -> pure False
                      Just v  -> LazySTM.readTVar v

                    state' = updateRemoteState connId (RemoteIdle timeoutSTM) state

                return . Just $ StateWithPeerTransition state' connId
              -- if the connection handler failed by this time, it will have
              -- written BearerState Dead to the IG tracer and we will handle this
              -- in MuxFinished case on the next iteration, where it will unregister
              -- the connection
              _otherwise -> return Nothing

          -- @
          --    PromotedToWarm^{Duplex}_{Remote}
          -- @
          -- or
          -- @
          --    Awake^{dataFlow}_{Remote}
          -- @
          --
          -- Note: the 'AwakeRemote' is detected as soon as mux detects any
          -- traffic.  This means that we'll observe this transition also if the
          -- first message that arrives is terminating a mini-protocol.
          AwakeRemote connId -> do
            -- notify the connection manager about the transition
            --
            -- NOTE: `promotedToWarmRemote` doesn't throw, hence exception handling
            -- is not needed.
            res <- promotedToWarmRemote connectionManager connId
            traceWith tracer (TrPromotedToWarmRemote connId res)

            let state' = updateRemoteState
                           connId
                           RemoteWarm
                           state
            return . Just $ StateWithPeerTransition state' connId

          RemotePromotedToHot connId -> do
            traceWith tracer (TrPromotedToHotRemote connId)
            let state' = updateRemoteState connId RemoteHot state
            return . Just $ StateWithPeerTransition state' connId

          RemoteDemotedToWarm connId -> do
            traceWith tracer (TrDemotedToWarmRemote connId)
            let state' = updateRemoteState connId RemoteWarm state
            return . Just $ StateWithPeerTransition state' connId

          CommitRemote connId -> do
            -- NOTE: `releaseInboundConnection` doesn't throw, hence exception
            -- handling is not needed.
            res <- releaseInboundConnection connectionManager connId
            traceWith tracer $ TrDemotedToColdRemote connId res
            case res of
              OperationSuccess transition ->
                case transition of
                  -- the following two cases are when the connection was not used
                  -- by p2p-governor, the connection will be closed.
                  CommitTr -> do
                    -- @
                    --    Commit^{dataFlow}_{Remote} : InboundIdleState dataFlow
                    --                               → TerminatingState
                    -- @
                    let state' = unregisterConnection False connId state
                    return . Just $ StateWithPeerTransition state' connId

                  -- the connection is still used by p2p-governor, carry on but put
                  -- it in 'RemoteCold' state.  This will ensure we keep ready to
                  -- serve the peer.
                  -- @
                  --    DemotedToCold^{Duplex}_{Remote} : DuplexState
                  --                                    → OutboundState Duplex
                  -- @
                  -- or
                  -- @
                  --    Awake^{Duplex}^{Local} : InboundIdleState Duplex
                  --                           → OutboundState Duplex
                  -- @
                  --
                  -- note: the latter transition is level triggered rather than
                  -- edge triggered. The server state is updated once protocol
                  -- idleness expires rather than as soon as the connection
                  -- manager was requested outbound connection.
                  KeepTr -> do
                    let state' = updateRemoteState connId RemoteCold state
                    return . Just $ StateWithPeerTransition state' connId

              _otherwise -> return Nothing

          MaturedDuplexPeers newMatureDuplexPeers freshDuplexPeers -> do
            traceWith tracer $ TrMaturedConnections (Map.keysSet newMatureDuplexPeers)
                                                    (Set.fromList $ OrdPSQ.keys freshDuplexPeers)
            return . Just $ OnlyStateChange state { matureDuplexPeers = newMatureDuplexPeers
                                                               <> matureDuplexPeers state,
                                                    freshDuplexPeers }

          InactivityTimeout -> do
            traceWith tracer $ TrInactive ((\(a,b,_) -> (a,b)) <$> OrdPSQ.toList (freshDuplexPeers state))
            return Nothing

        mask_ $ do
          case decision of
            Just (OnlyStateChange state') -> do
              atomically $ writeTVar stateVar state'
              traceWith debugTracer (Debug state')
            Just (StateWithPeerTransition state' p) -> do
              atomically $ writeTVar stateVar state'
              traceWith debugTracer (Debug state')
              traceWith trTracer (mkRemoteTransitionTrace p state state')
            _otherwise -> pure ()

        case decision of
          _ | Just state' <- withState -> do
                mapTraceWithCache TrInboundGovernorCounters
                                  tracer
                                  (countersCache state')
                                  (counters state')
                traceWith tracer $ TrRemoteState $
                      mkRemoteSt . csRemoteState
                  <$> connections state'

                -- Update Inbound Governor Counters cache values
                let newCounters       = counters state'
                    Cache oldCounters = countersCache state'
                    state'' | newCounters /= oldCounters = state' { countersCache = Cache newCounters }
                            | otherwise                 = state'

                atomically $ writeTVar stateVar state''
            where
              withState = case decision of
                Just (OnlyStateChange s)            -> Just s
                Just (StateWithPeerTransition s _p) -> Just s
                _otherwise                          -> Nothing

          _otherwise -> return ()

-- | The tracer embedded with the mux tracer by the connection handler
-- for inbound or outbound duplex connections for efficient tracking
-- of inbound governor transitions for a given peer
--
inboundGovernorMuxTracer
  :: (MonadSTM m, Ord peerAddr)
  => InboundGovernorInfoChannel muxMode initiatorCtx peerAddr versionData ByteString m a b
  -> (versionData -> DataFlow)
  -> StrictTVar m (State muxMode initiatorCtx peerAddr versionData m a b)
  -> StrictTVar m Bool
  -> StrictTVar m (StrictMaybe ResponderCounters)
  -> Tracer m (Mux.WithBearer (ConnectionId peerAddr) Mux.Trace)
inboundGovernorMuxTracer infoChannel connectionDataFlow stateVar activeVar countersVar =
  Tracer \(Mux.WithBearer peer trace) -> do
    -- hello from muxer main thread
    -- code here is running in the context of the connection handler/muxer
    -- so care must be taken not to deadlock ourselves
    active <- readTVarIO activeVar
    case (trace, active) of
      (_, True) | Just miniProtocolNum <- miniProtocolStarted trace -> atomically do
        connections <- connections <$> readTVar stateVar
        mCounters   <- readTVar countersVar
        case (Map.lookup peer connections, mCounters) of
          (Just (ConnectionState { csRemoteState, csMiniProtocolMap }),
           SJust rc@ResponderCounters { numTraceHotResponders,
                                        numTraceNonHotResponders }) -> do
            let miniProtocolTemp = getProtocolTemp miniProtocolNum csMiniProtocolMap
                commit = do
                  case (numTraceNonHotResponders, numTraceHotResponders) of
                    (0, 0) -> InfoChannel.writeMessage infoChannel
                                                       (AwakeRemote peer)
                    _      -> pure ()
                  case (miniProtocolTemp, numTraceHotResponders) of
                    (Hot, 0) -> InfoChannel.writeMessage infoChannel
                                                         (RemotePromotedToHot peer)
                    _        -> pure ()
                  case miniProtocolTemp of
                    Hot    -> writeTVar countersVar $
                                SJust rc { numTraceHotResponders =
                                             succ numTraceHotResponders }
                    _orNot -> writeTVar countersVar $
                                SJust rc { numTraceNonHotResponders =
                                             succ numTraceNonHotResponders }

            case csRemoteState of
              -- we retry on expired because we let the IG
              -- loop handle this peer. If the connection is released,
              -- and CM reports CommitTr, this peer will disappear
              -- from the connections so on retry we will hit the
              -- _otherwise clause instead and promotion will fail,
              -- as it should. Otherwise, if KeepTr is returned,
              -- we can handle 'AwakeRemote' from this peer.
              RemoteIdle timeoutSTM -> do
                expired <- timeoutSTM
                if expired then retry else commit
              _ -> commit

          _otherwise -> writeTVar countersVar SNothing

      (_, True) | Just miniProtocolNum <- miniProtocolTerminated trace -> atomically do
        connections <- connections <$> readTVar stateVar
        mCounters   <- readTVar countersVar
        case (Map.lookup peer connections, mCounters) of
          (Just (ConnectionState { csMux,
                                   csVersionData,
                                   csMiniProtocolMap,
                                   csCompletionMap }),
           SJust rc@ResponderCounters { numTraceHotResponders,
                                        numTraceNonHotResponders }) -> do
            InfoChannel.writeMessage infoChannel $
              MiniProtocolTerminated $ Terminated {
                tConnId = peer,
                tMux = csMux,
                tMiniProtocolData = csMiniProtocolMap Map.! miniProtocolNum,
                tDataFlow = connectionDataFlow csVersionData,
                tResult = csCompletionMap Map.! miniProtocolNum }
            case trace of
              Mux.TraceCleanExit {} -> do
                let miniProtocolTemp = getProtocolTemp miniProtocolNum csMiniProtocolMap
                case (miniProtocolTemp, numTraceHotResponders) of
                  (Hot, 1) -> InfoChannel.writeMessage infoChannel
                                                       (RemoteDemotedToWarm peer)
                  _        -> pure ()
                when (numTraceHotResponders + numTraceNonHotResponders == 1) $
                  InfoChannel.writeMessage infoChannel
                                           (WaitIdleRemote peer)
                case miniProtocolTemp of
                    Hot    -> writeTVar countersVar $
                                SJust rc { numTraceHotResponders =
                                             pred numTraceHotResponders }
                    _orNot -> writeTVar countersVar $
                                SJust rc { numTraceNonHotResponders =
                                             pred numTraceNonHotResponders }

              _otherwise -> writeTVar countersVar SNothing

          _otherwise -> writeTVar countersVar SNothing


      (_, True) | True <- muxStopped trace -> atomically do
        State { connections } <- readTVar stateVar
        case Map.lookup peer connections of
          Just ConnectionState {csMux} ->
            InfoChannel.writeMessage infoChannel $
              MuxFinished peer (Mux.stopped csMux)
          _otherwise -> pure ()
        writeTVar countersVar SNothing

      _otherwise -> return ()
    where
      muxStopped = \case
        Mux.TraceStopped -> True
        Mux.TraceState Mux.Dead -> True
        _otherwise -> False

      getProtocolTemp miniProtocolNum csMiniProtocolMap  =
        let miniData = csMiniProtocolMap Map.! miniProtocolNum
         in mpdMiniProtocolTemp miniData

      miniProtocolTerminated = \case
        Mux.TraceCleanExit miniProtocolNum Mux.ResponderDir -> Just miniProtocolNum
        Mux.TraceExceptionExit miniProtocolNum Mux.ResponderDir _e -> Just miniProtocolNum
        _otherwise -> Nothing

      miniProtocolStarted = \case
        -- is any responder started eagerly???
        Mux.TraceStartEagerly miniProtocolNum Mux.ResponderDir -> Just miniProtocolNum
        Mux.TraceStartedOnDemand miniProtocolNum Mux.ResponderDir -> Just miniProtocolNum
        _otherwise -> Nothing


-- | Run a responder mini-protocol.
--
-- @'HasResponder' mode ~ True@ is used to rule out
-- 'InitiatorProtocolOnly' case.
--
runResponder :: forall (mode :: Mux.Mode) initiatorCtx peerAddr m a b.
                 ( Alternative (STM m)
                 , HasResponder mode ~ True
                 , MonadAsync       m
                 , MonadLabelledSTM m
                 , MonadCatch       m
                 , MonadMask        m
                 , MonadThrow  (STM m)
                 )
              => Mux.Mux mode m
              -> MiniProtocolData mode initiatorCtx peerAddr m a b
              -> m (Either SomeException (STM m (Either SomeException b)))
runResponder mux
             MiniProtocolData {
               mpdMiniProtocol     = miniProtocol,
               mpdResponderContext = responderContext
             } =
    -- do not catch asynchronous exceptions, which are non recoverable
    tryJust (\e -> case fromException e of
              Just (SomeAsyncException _) -> Nothing
              Nothing                     -> Just e) $
      case miniProtocolRun miniProtocol of
        ResponderProtocolOnly responder ->
          Mux.runMiniProtocol
            mux (miniProtocolNum miniProtocol)
            Mux.ResponderDirectionOnly
            (miniProtocolStart miniProtocol)
            (runMiniProtocolCb responder responderContext)

        InitiatorAndResponderProtocol _ responder ->
          Mux.runMiniProtocol
            mux (miniProtocolNum miniProtocol)
            Mux.ResponderDirection
            (miniProtocolStart miniProtocol)
            (runMiniProtocolCb responder responderContext)


maturedPeers :: Ord peerAddr
             => Time
             -> OrdPSQ peerAddr Time versionData
             -> (Map peerAddr versionData, OrdPSQ peerAddr Time versionData)
maturedPeers time freshPeers =
      first (Map.fromList . map (\(addr, _p, v) -> (addr, v)))
    $ OrdPSQ.atMostView ((-inboundMaturePeerDelay) `addTime` time)
                           freshPeers

--
-- Trace
--


-- | 'Nothing' represents uninitialised state.
--
type RemoteTransition = Transition' (Maybe RemoteSt)

type RemoteTransitionTrace peerAddr = TransitionTrace' peerAddr (Maybe RemoteSt)

mkRemoteTransitionTrace :: Ord peerAddr
                        => ConnectionId peerAddr
                        -> State muxMode initiatorCtx peerAddr versionData m a b
                        -> State muxMode initiatorCtx peerAddr versionData m a b
                        -> RemoteTransitionTrace peerAddr
mkRemoteTransitionTrace connId fromState toState =
    TransitionTrace
      (remoteAddress connId)
      Transition { fromState = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (connections fromState)
                 , toState   = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (connections toState)
                 }


-- | A channel which instantiates to 'NewConnectionInfo' and
-- 'Handle'.
--
-- * /Producer:/ connection manger for duplex outbound connections.
-- * /Consumer:/ inbound governor.
--
type InboundGovernorInfoChannel (muxMode :: Mux.Mode) initiatorCtx peerAddr versionData bytes m a b =
    InformationChannel (Event (muxMode :: Mux.Mode) (Handle muxMode initiatorCtx (ResponderContext peerAddr) versionData bytes m a b) initiatorCtx peerAddr versionData m a b) m


-- | Announcement message for a new connection.
--
data NewConnectionInfo peerAddr handle

    -- | Announce a new connection.  /Inbound protocol governor/ will start
    -- responder protocols using 'StartOnDemand' strategy and monitor remote
    -- transitions: @PromotedToWarm^{Duplex}_{Remote}@ and
    -- @DemotedToCold^{dataFlow}_{Remote}@.
    = NewConnectionInfo
      !Provenance
      !(ConnectionId peerAddr)
      !DataFlow
      !handle

instance Show peerAddr
      => Show (NewConnectionInfo peerAddr handle) where
      show (NewConnectionInfo provenance connId dataFlow _) =
        concat [ "NewConnectionInfo "
               , show provenance
               , " "
               , show connId
               , " "
               , show dataFlow
               ]


-- | Edge triggered events to which the /inbound protocol governor/ reacts.
--
data Event (muxMode :: Mux.Mode) handle initiatorCtx peerAddr versionData m a b
    -- | A request to start mini-protocol bundle, either from the server or from
    -- connection manager after a duplex connection was negotiated.
    --
    = NewConnection !(NewConnectionInfo peerAddr handle)

    -- | A multiplexer exited.
    --
    | MuxFinished            !(ConnectionId peerAddr) (STM m (Maybe SomeException))

    -- | A mini-protocol terminated either cleanly or abruptly.
    --
    | MiniProtocolTerminated !(Terminated muxMode initiatorCtx peerAddr m a b)

    -- | Transition from 'RemoteEstablished' to 'RemoteIdle'.
    --
    | WaitIdleRemote         !(ConnectionId peerAddr)

    -- | A remote @warm → hot@ transition.  It is scheduled as soon as all hot
    -- mini-protocols are running.
    --
    | RemotePromotedToHot    !(ConnectionId peerAddr)

    -- | A @hot → warm@ transition.  It is scheduled as soon as any hot
    -- mini-protocol terminates.
    --
    | RemoteDemotedToWarm    !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' to 'RemoteCold'.
    --
    | CommitRemote           !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' or 'RemoteCold' to 'RemoteEstablished'.
    --
    | AwakeRemote            !(ConnectionId peerAddr)

    -- | Update `igsMatureDuplexPeers` and `igsFreshDuplexPeers`.
    --
    | MaturedDuplexPeers   !(Map peerAddr versionData)         -- ^ newly matured duplex peers
                           !(OrdPSQ peerAddr Time versionData) -- ^ queue of fresh duplex peers

    | InactivityTimeout


-- STM transactions which detect 'Event's (signals)
--


-- | A signal which returns an 'Event'.  Signals are combined together and
-- passed used to fold the current state map.
--
type EventSignal (muxMode :: Mux.Mode) handle initiatorCtx peerAddr versionData m a b =
        ConnectionId peerAddr
     -> ConnectionState muxMode initiatorCtx peerAddr versionData m a b
     -> FirstToFinish (STM m) (Event muxMode handle initiatorCtx peerAddr versionData m a b)


-- | When a mini-protocol terminates we take 'Terminated' out of 'ConnectionState
-- and pass it to the main loop.  This is just enough to decide if we need to
-- restart a mini-protocol and to do the restart.
--
data Terminated muxMode initiatorCtx peerAddr m a b = Terminated {
    tConnId           :: !(ConnectionId peerAddr),
    tMux              :: !(Mux.Mux muxMode m),
    tMiniProtocolData :: !(MiniProtocolData muxMode initiatorCtx peerAddr m a b),
    tDataFlow         :: !DataFlow,
    tResult           :: STM m (Either SomeException b) -- !(Either SomeException b)
  }


-- | First peer for which the 'RemoteIdle' timeout expires.
--
firstPeerCommitRemote :: (Alternative (STM m), MonadSTM m)
                      => EventSignal muxMode handle initiatorCtx peerAddr versionData m a b
firstPeerCommitRemote
    connId ConnectionState { csRemoteState }
    = case csRemoteState of
        -- the connection is already in 'RemoteCold' state
        RemoteCold            -> mempty
        RemoteEstablished     -> mempty
        RemoteIdle timeoutSTM -> FirstToFinish do
          expired <- timeoutSTM
          if expired then pure $ CommitRemote connId else retry


data IGAssertionLocation peerAddr
  = InboundGovernorLoop !(Maybe (ConnectionId peerAddr)) !AbstractState
  deriving Show

data Trace peerAddr
    = TrNewConnection                !Provenance !(ConnectionId peerAddr)
    | TrResponderRestarted           !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderStartFailure        !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderErrored             !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderStarted             !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderTerminated          !(ConnectionId peerAddr) !MiniProtocolNum
    | TrPromotedToWarmRemote         !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrPromotedToHotRemote          !(ConnectionId peerAddr)
    | TrDemotedToWarmRemote          !(ConnectionId peerAddr)
    | TrDemotedToColdRemote          !(ConnectionId peerAddr) !(OperationResult DemotedToColdRemoteTr)
    -- ^ All mini-protocols terminated.  The boolean is true if this connection
    -- was not used by p2p-governor, and thus the connection will be terminated.
    | TrWaitIdleRemote               !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrMuxCleanExit                 !(ConnectionId peerAddr)
    | TrMuxErrored                   !(ConnectionId peerAddr) SomeException
    | TrInboundGovernorCounters      !Counters
    | TrRemoteState                  !(Map (ConnectionId peerAddr) RemoteSt)
    | TrUnexpectedlyFalseAssertion   !(IGAssertionLocation peerAddr)
    -- ^ This case is unexpected at call site.
    | TrInboundGovernorError         !SomeException
    | TrMaturedConnections           !(Set peerAddr) !(Set peerAddr)
    | TrInactive                     ![(peerAddr, Time)]
  deriving Show


data Debug peerAddr versionData = forall muxMode initiatorCtx m a b.
    Debug (State muxMode initiatorCtx peerAddr versionData m a b)

data LoopDecision state peer = OnlyStateChange         !state
                             | StateWithPeerTransition !state !peer
