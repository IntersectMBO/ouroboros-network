{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
  , newPublicStateVar
  , emptyPublicState
  , Arguments (..)
  , with
    -- * Trace
  , Trace (..)
  , Debug (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  , AcceptConnectionsPolicyTrace (..)
    -- * Re-exports
  , Transition' (..)
  , TransitionTrace' (..)
    -- * API's exported for testing purposes
  , maturedPeers
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException (..))
import Control.Monad (foldM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)

import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Cache
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid.Synchronisation
import Data.OrdPSQ (OrdPSQ)
import Data.OrdPSQ qualified as OrdPSQ
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)

import Network.Mux qualified as Mux

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.InformationChannel
           (InboundGovernorInfoChannel)
import Ouroboros.Network.ConnectionManager.InformationChannel qualified as InfoChannel
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.InboundGovernor.Event
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


data Arguments muxMode socket initiatorCtx networkState peerAddr versionNumber versionData m a b = Arguments {
      transitionTracer   :: Tracer m (RemoteTransitionTrace peerAddr),
      -- ^ transition tracer
      tracer             :: Tracer m (Trace peerAddr),
      -- ^ main inbound governor tracer
      debugTracer        :: Tracer m (Debug peerAddr versionData),
      -- ^ debug inbound governor tracer
      connectionDataFlow :: versionData -> DataFlow,
      -- ^ connection data flow
      infoChannel        :: InboundGovernorInfoChannel muxMode initiatorCtx networkState peerAddr versionData ByteString m a b,
      -- ^ 'InformationChannel' which passes 'NewConnectionInfo' for outbound
      -- connections from connection manager to the inbound governor.
      idleTimeout        :: Maybe DiffTime,
      -- ^ protocol idle timeout.  The remote site must restart a mini-protocol
      -- within given timeframe (Nothing indicates no timeout).
      connectionManager  :: MuxConnectionManager muxMode socket initiatorCtx
                                                    (ResponderContext peerAddr)
                                                    networkState
                                                    peerAddr
                                                    versionData versionNumber
                                                    ByteString m a b,
      -- ^ connection manager
      readNetworkState   :: m networkState
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
with :: forall (muxMode :: Mux.Mode) socket initiatorCtx networkState peerAddr versionData versionNumber m a b x.
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
        )
     => Arguments muxMode socket initiatorCtx networkState peerAddr versionNumber versionData m a b
     -> (   Async m Void
         -> STM m (PublicState peerAddr versionData)
         -> m x)
     -> m x
with
    Arguments {
      transitionTracer = trTracer,
      tracer,
      debugTracer,
      connectionDataFlow,
      infoChannel,
      idleTimeout,
      connectionManager,
      readNetworkState
    }
    k
    = do
    labelThisThread "inbound-governor"
    -- TODO: avoid a `TVar`.
    var <- newTVarIO (mkPublicState emptyState)
    withAsync ((do
               labelThisThread "inbound-governor-loop"
               inboundGovernorLoop var emptyState)
                `catch`
               handleError var) $
      \thread -> k thread (readTVar var)
  where
    -- Trace final transition mostly for testing purposes.
    --
    -- NOTE: `inboundGovernorLoop` doesn't throw synchronous exceptions, this is
    -- just need to handle asynchronous exceptions.
    handleError
      :: StrictTVar m (PublicState peerAddr versionData)
      -> SomeException
      -> m Void
    handleError var e = do
      PublicState { remoteStateMap } <- readTVarIO var
      _ <- Map.traverseWithKey
             (\connId remoteSt ->
               traceWith trTracer $
                 TransitionTrace (remoteAddress connId)
                 Transition { fromState = Just remoteSt,
                              toState   = Nothing }
             )
             remoteStateMap
      throwIO e

    -- The inbound protocol governor recursive loop.  The 'connections' is
    -- updated as we recurse.
    --
    inboundGovernorLoop
      :: StrictTVar m (PublicState peerAddr versionData)
      -> State muxMode initiatorCtx networkState peerAddr versionData m a b
      -> m Void
    inboundGovernorLoop var !state = do
      time <- getMonotonicTime
      inactivityVar <- registerDelay inactionTimeout

      event
        <- atomically $ runFirstToFinish $
               FirstToFinish  (
                 -- mark connections as mature
                 case maturedPeers time (freshDuplexPeers state) of
                   (as, _)     | Map.null as
                               -> retry
                   (as, fresh) -> pure $ MaturedDuplexPeers as fresh
               )
            <> Map.foldMapWithKey
                 (    firstMuxToFinish
                   <> firstPeerDemotedToCold
                   <> firstPeerCommitRemote
                   <> firstMiniProtocolToFinish connectionDataFlow
                   <> firstPeerPromotedToWarm
                   <> firstPeerPromotedToHot
                   <> firstPeerDemotedToWarm

                   :: EventSignal muxMode initiatorCtx networkState peerAddr versionData m a b
                 )
                 (connections state)
            <> FirstToFinish (
                 NewConnection <$> InfoChannel.readMessage infoChannel
               )
            <> FirstToFinish (
                  -- spin the inbound governor loop; it will re-run with new
                  -- time, which allows to make some peers mature.
                  LazySTM.readTVar inactivityVar >>= check >> pure InactivityTimeout
               )
      (mbConnId, state') <- case event of
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
                          runResponder csMux readNetworkState mpd >>= \case
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
                                                          Nothing -> retry
                                                          Just v  -> LazySTM.readTVar v >>= check)

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

              time' <- getMonotonicTime
              -- update state and continue the recursive loop
              let state' = state {
                      connections,
                      freshDuplexPeers =
                        case dataFlow of
                          Unidirectional -> freshDuplexPeers state
                          Duplex         -> OrdPSQ.insert (remoteAddress connId) time' csVersionData
                                                          (freshDuplexPeers state)
                    }
              return (Just connId, state')

        MuxFinished connId merr -> do

          case merr of
            Nothing  -> traceWith tracer (TrMuxCleanExit connId)
            Just err -> traceWith tracer (TrMuxErrored connId err)

          -- the connection manager does should realise this on itself.
          let state' = unregisterConnection connId state
          return (Just connId, state')

        MiniProtocolTerminated
          Terminated {
              tConnId,
              tMux,
              tMiniProtocolData = mpd@MiniProtocolData { mpdMiniProtocol = miniProtocol },
              tResult
            } ->
          let num = miniProtocolNum miniProtocol in
          case tResult of
            Left e -> do
              -- a mini-protocol errored.  In this case mux will shutdown, and
              -- the connection manager will tear down the socket.  We can just
              -- forget the connection from 'State'.
              traceWith tracer $
                TrResponderErrored tConnId num e

              let state' = unregisterConnection tConnId state
              return (Just tConnId, state')

            Right _ ->
              runResponder tMux readNetworkState mpd >>= \case
                Right completionAction -> do
                  traceWith tracer (TrResponderRestarted tConnId num)
                  let state' = updateMiniProtocol tConnId num completionAction state
                  return (Nothing, state')

                Left err -> do
                  -- there is no way to recover from synchronous exceptions; we
                  -- stop mux which allows to close resources held by
                  -- connection manager.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stop tMux

                  let state' = unregisterConnection tConnId state

                  return (Just tConnId, state')


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
            TerminatedConnection {} -> do
              let state' = unregisterConnection connId state
              return (Just connId, state')
            OperationSuccess {}  -> do
              mv <- traverse registerDelay idleTimeout
              let timeoutSTM :: STM m ()
                  !timeoutSTM = case mv of
                    Nothing -> retry
                    Just v  -> LazySTM.readTVar v >>= check

              let state' = updateRemoteState connId (RemoteIdle timeoutSTM) state

              return (Just connId, state')
            -- It could happen that the connection got deleted by connection
            -- manager due to some async exception so we need to unregister it
            -- from the inbound governor state.
            UnsupportedState UnknownConnectionSt -> do
              let state' = unregisterConnection connId state
              return (Just connId, state')
            UnsupportedState {} -> do
              return (Just connId, state)

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

          case resultInState res of
            UnknownConnectionSt -> do
              let state' = unregisterConnection connId state
              return (Just connId, state')
            _ -> do
              let state' = updateRemoteState
                             connId
                             RemoteWarm
                             state
              return (Just connId, state')

        RemotePromotedToHot connId -> do
          traceWith tracer (TrPromotedToHotRemote connId)
          let state' = updateRemoteState connId RemoteHot state

          return (Just connId, state')

        RemoteDemotedToWarm connId -> do
          traceWith tracer (TrDemotedToWarmRemote connId)
          let state' = updateRemoteState connId RemoteWarm state

          return (Just connId, state')

        CommitRemote connId -> do
          -- NOTE: `releaseInboundConnection` doesn't throw, hence exception
          -- handling is not needed.
          res <- releaseInboundConnection connectionManager connId
          traceWith tracer $ TrDemotedToColdRemote connId res
          case res of
            UnsupportedState {} -> do
              -- 'inState' can be either:
              -- @'UnknownConnection'@,
              -- @'InReservedOutboundState'@,
              -- @'InUnnegotiatedState',
              -- @'InOutboundState' 'Unidirectional'@,
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              let state' = unregisterConnection connId state
              return (Just connId, state')

            TerminatedConnection {} -> do
              -- 'inState' can be either:
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              let state' = unregisterConnection connId state
              return (Just connId, state')

            OperationSuccess transition ->
              case transition of
                -- the following two cases are when the connection was not used
                -- by p2p-governor, the connection will be closed.
                CommitTr -> do
                  -- @
                  --    Commit^{dataFlow}_{Remote} : InboundIdleState dataFlow
                  --                               → TerminatingState
                  -- @
                  let state' = unregisterConnection connId state
                  return (Just connId, state')

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

                  return (Just connId, state')

        MaturedDuplexPeers newMatureDuplexPeers freshDuplexPeers -> do
          traceWith tracer $ TrMaturedConnections (Map.keysSet newMatureDuplexPeers)
                                                  (Set.fromList $ OrdPSQ.keys freshDuplexPeers)
          pure (Nothing, state { matureDuplexPeers = newMatureDuplexPeers
                                                  <> matureDuplexPeers state,
                                 freshDuplexPeers })

        InactivityTimeout -> do
          traceWith tracer $ TrInactive ((\(a,b,_) -> (a,b)) <$> OrdPSQ.toList (freshDuplexPeers state))
          pure (Nothing, state)

      mask_ $ do
        atomically $ writeTVar var (mkPublicState state')
        traceWith debugTracer (Debug state')
        case mbConnId of
          Just cid -> traceWith trTracer (mkRemoteTransitionTrace cid state state')
          Nothing  -> pure ()

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

      inboundGovernorLoop var state''


-- | Run a responder mini-protocol.
--
-- @'HasResponder' mode ~ True@ is used to rule out
-- 'InitiatorProtocolOnly' case.
--
runResponder :: forall (mode :: Mux.Mode) initiatorCtx networkState peerAddr m a b.
                 ( Alternative (STM m)
                 , HasResponder mode ~ True
                 , MonadAsync       m
                 , MonadLabelledSTM m
                 , MonadCatch       m
                 , MonadMask        m
                 , MonadThrow  (STM m)
                 )
              => Mux.Mux mode m
              -> m networkState
              -> MiniProtocolData mode initiatorCtx networkState peerAddr m a b
              -> m (Either SomeException (STM m (Either SomeException b)))
runResponder mux readNetworkState
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

        ResponderProtocolOnlyWithState responder ->
          Mux.runMiniProtocol
            mux (miniProtocolNum miniProtocol)
            Mux.ResponderDirectionOnly
            (miniProtocolStart miniProtocol)
            (runMiniProtocolCb responder (readNetworkState, responderContext))

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
                        -> State muxMode initiatorCtx networkState peerAddr versionData m a b
                        -> State muxMode initiatorCtx networkState peerAddr versionData m a b
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


data Debug peerAddr versionData = forall muxMode initiatorCtx networkState m a b.
    Debug (State muxMode initiatorCtx networkState peerAddr versionData m a b)
