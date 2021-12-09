{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Server implementation based on 'ConnectionManager'
--
module Ouroboros.Network.InboundGovernor
  ( InboundGovernorObservableState (..)
  , newObservableStateVar
  , newObservableStateVarIO
  , newObservableStateVarFromSeed
  -- * Run Inbound Protocol Governor
  , inboundGovernor
  -- * Trace
  , InboundGovernorTrace (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Exception (SomeAsyncException (..), assert)
import           Control.Applicative (Alternative (..), (<|>))
import           Control.Monad (foldM, when)
import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)

import           Data.Cache
import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Network.Mux as Mux

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types hiding (TrUnexpectedlyFalseAssertion)
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.Mux hiding (ControlMessage)
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.InboundGovernor.Event
import           Ouroboros.Network.InboundGovernor.State
import           Ouroboros.Network.InboundGovernor.ControlChannel (ServerControlChannel)
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as ControlChannel



-- | Run the server, which consists of the following components:
--
-- * /inbound governor/, it corresponds to p2p-governor on outbound side
-- * /accept loop(s)/, one per given ip address.  We support up to one ipv4
--   address and up to one ipv6 address, i.e. an ipv6 enabled node will run two
--   accept loops on listening on different addresses with shared /inbound governor/.
--
-- The server can be run in either of two 'MuxMode'-es:
--
-- * 'InitiatorResponderMode'
-- * 'ResponderMode'
--
-- The first one is used in data diffusion for /Node-To-Node protocol/, while the
-- other is useful for running a server for the /Node-To-Client protocol/.
--
inboundGovernor :: forall (muxMode :: MuxMode) socket peerAddr versionNumber m a b.
                   ( MonadAsync    m
                   , MonadCatch    m
                   , MonadEvaluate m
                   , MonadThrow    m
                   , MonadThrow    (STM m)
                   , MonadTime     m
                   , MonadTimer    m
                   , MonadMask     m
                   , Ord peerAddr
                   , HasResponder muxMode ~ True
                   )
                => Tracer m (RemoteTransitionTrace peerAddr)
                -> Tracer m (InboundGovernorTrace peerAddr)
                -> ServerControlChannel muxMode peerAddr ByteString m a b
                -> DiffTime -- protocol idle timeout
                -> MuxConnectionManager muxMode socket peerAddr
                                        versionNumber ByteString m a b
                -> StrictTVar m InboundGovernorObservableState
                -> m Void
inboundGovernor trTracer tracer serverControlChannel inboundIdleTimeout
                connectionManager observableStateVar = do
    -- State needs to be a TVar, otherwise, when catching the exception inside
    -- the loop we do not have access to the most recentversion of the state
    -- and might be truncating transitions.
    st <- atomically $ newTVar emptyState
    inboundGovernorLoop st
     `catch`
       (\(e :: SomeAsyncException) -> do
         state <- atomically $ readTVar st
         _ <- Map.traverseWithKey
               (\connId _ ->
                 traceWith trTracer
                           (mkRemoteTransitionTrace connId state emptyState)
               )
               (igsConnections state)

         throwIO e
       )
  where
    emptyState :: InboundGovernorState muxMode peerAddr m a b
    emptyState = InboundGovernorState {
            igsConnections   = Map.empty,
            igsObservableVar = observableStateVar,
            igsCountersCache = mempty
          }

    -- The inbound protocol governor recursive loop.  The 'igsConnections' is
    -- updated as we recurs.
    --
    inboundGovernorLoop
      :: StrictTVar m (InboundGovernorState muxMode peerAddr m a b)
      -> m Void
    inboundGovernorLoop !st = do
      state <- atomically $ readTVar st
      mapTraceWithCache TrInboundGovernorCounters
                        tracer
                        (igsCountersCache state)
                        (inboundGovernorCounters state)
      traceWith tracer $ TrRemoteState $
            mkRemoteSt . csRemoteState
        <$> igsConnections state

      event
        <- atomically $
                (uncurry MuxFinished    <$> firstMuxToFinish state)
            <|> (MiniProtocolTerminated <$> firstMiniProtocolToFinish state)
            <|> (AwakeRemote            <$> firstPeerPromotedToWarm state)
            <|> (RemotePromotedToHot    <$> firstPeerPromotedToHot state)
            <|> (RemoteDemotedToWarm    <$> firstPeerDemotedToWarm state)
            <|>                             firstPeerDemotedToCold state
            <|> (NewConnection          <$> ControlChannel.readMessage
                                              serverControlChannel)
      (mbConnId, state') <- case event of
        NewConnection
          -- new connection has been announced by either accept loop or
          -- by connection manager (in which case the connection is in
          -- 'DuplexState').
          (ControlChannel.NewConnection
            provenance
            connId
            csDataFlow
            (Handle csMux muxBundle _)) -> do

              traceWith tracer (TrNewConnection provenance connId)

              igsConnections <- Map.alterF
                      (\case
                        -- connection
                        Nothing -> do
                          let csMPMHot =
                                [ ( miniProtocolNum mpH
                                  , MiniProtocolData mpH Hot
                                  )
                                | mpH <- projectBundle TokHot muxBundle
                                ]
                              csMPMWarm =
                                [ ( miniProtocolNum mpW
                                  , MiniProtocolData mpW Warm
                                  )
                                | mpW <- projectBundle TokWarm muxBundle
                                ]
                              csMPMEstablished =
                                [ ( miniProtocolNum mpE
                                  , MiniProtocolData mpE Established
                                  )
                                | mpE <- projectBundle TokEstablished muxBundle
                                ]
                              csMiniProtocolMap =
                                  Map.fromList
                                  (csMPMHot ++ csMPMWarm ++ csMPMEstablished)

                          mCompletionMap
                            <-
                            foldM
                              (\acc MiniProtocolData { mpdMiniProtocol } -> do
                                 result <- runResponder
                                             csMux mpdMiniProtocol
                                             Mux.StartOnDemand
                                 case result of
                                   -- synchronous exceptions when starting
                                   -- a mini-protocol are non-recoverable; we
                                   -- close the connection and allow the server
                                   -- to continue.
                                   Left err -> do
                                     traceWith tracer (TrResponderStartFailure connId (miniProtocolNum mpdMiniProtocol) err)
                                     Mux.stopMux csMux
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
                              v <- registerDelay inboundIdleTimeout
                              let -- initial state is 'RemoteIdle', if the remote end will not
                                  -- start any responders this will unregister the inbound side.
                                  csRemoteState :: RemoteState m
                                  csRemoteState = RemoteIdle (LazySTM.readTVar v >>= check)

                                  connState = ConnectionState {
                                      csMux,
                                      csDataFlow,
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
                      (igsConnections state)


              -- update state and continue the recursive loop
              let state' = state { igsConnections }
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
              tMiniProtocolData = MiniProtocolData { mpdMiniProtocol },
              tResult
            } ->
          let num = miniProtocolNum mpdMiniProtocol in
          case tResult of
            Left e -> do
              -- a mini-protocol errored.  In this case mux will shutdown, and
              -- the connection manager will tear down the socket.  We can just
              -- forget the connection from 'InboundGovernorState'.
              traceWith tracer $
                TrResponderErrored tConnId num e

              let state' = unregisterConnection tConnId state
              return (Just tConnId, state')

            Right _ -> do
              result
                <- runResponder tMux mpdMiniProtocol Mux.StartOnDemand
              case result of
                Right completionAction -> do
                  traceWith tracer (TrResponderRestarted tConnId num)
                  let state' = updateMiniProtocol tConnId num completionAction
                             $ state
                  return (Nothing, state')

                Left err -> do
                  -- there is no way to recover from synchronous exceptions; we
                  -- stop mux which allows to close resources held by
                  -- connection manager.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stopMux tMux

                  let state' = unregisterConnection tConnId state

                  return (Just tConnId, state')


        WaitIdleRemote connId -> do
          -- @
          --    DemotedToCold^{dataFlow}_{Remote} : InboundState Duplex
          --                                      → InboundIdleState Duplex
          -- @
          res <- demotedToColdRemote connectionManager
                                     (remoteAddress connId)
          traceWith tracer (TrWaitIdleRemote connId res)
          case res of
            TerminatedConnection {} -> do
              let state' = unregisterConnection connId state
              return (Just connId, state')
            OperationSuccess {}  -> do
              v <- registerDelay inboundIdleTimeout
              let timeoutSTM :: STM m ()
                  !timeoutSTM = LazySTM.readTVar v >>= check

              let state' = updateRemoteState connId (RemoteIdle timeoutSTM) state

              return (Just connId, state')
            UnsupportedState {} ->
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
          -- notify the connection manager about the transiton
          res <- promotedToWarmRemote connectionManager
                                      (remoteAddress connId)
          traceWith tracer (TrPromotedToWarmRemote connId res)

          when (resultInState res == UnknownConnectionSt) $ do
            traceWith tracer (TrUnexpectedlyFalseAssertion
                                (InboundGovernorLoop
                                  (Just connId)
                                  UnknownConnectionSt)
                             )
            _ <- evaluate (assert False)
            pure ()

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
          res <- unregisterInboundConnection connectionManager
                                             (remoteAddress connId)
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

      mask_ $ do
        atomically $ writeTVar st state'
        case mbConnId of
          Just cid -> traceWith trTracer (mkRemoteTransitionTrace cid state state')
          Nothing  -> pure ()

      inboundGovernorLoop st


-- | Run a responder mini-protocol.
--
-- @'HasResponder' mode ~ True@ is used to rule out
-- 'InitiatorProtocolOnly' case.
--
runResponder :: forall (mode :: MuxMode) m a b.
                 ( HasResponder mode ~ True
                 , MonadAsync m
                 , MonadCatch m
                 , MonadThrow (STM m)
                 )
              => Mux.Mux mode m
              -> MiniProtocol mode ByteString m a b
              -> Mux.StartOnDemandOrEagerly
              -> m (Either SomeException (STM m (Either SomeException b)))
runResponder mux
             MiniProtocol { miniProtocolNum, miniProtocolRun }
             startStrategy =
    -- do not catch asynchronous exceptions, which are non recoverable
    tryJust (\e -> case fromException e of
              Just (SomeAsyncException _) -> Nothing
              Nothing                     -> Just e) $
      case miniProtocolRun of
        ResponderProtocolOnly responder ->
          Mux.runMiniProtocol
            mux miniProtocolNum
            Mux.ResponderDirectionOnly
            startStrategy
            -- TODO: eliminate 'fromChannel'
            (runMuxPeer responder . fromChannel)

        InitiatorAndResponderProtocol _ responder ->
          Mux.runMiniProtocol
            mux miniProtocolNum
            Mux.ResponderDirection
            startStrategy
            (runMuxPeer responder . fromChannel)


--
-- Trace
--

-- | Remote connection state tracked by inbound protocol governor.
--
data RemoteSt = RemoteWarmSt
              | RemoteHotSt
              | RemoteIdleSt
              | RemoteColdSt
  deriving (Eq, Show)


mkRemoteSt :: RemoteState m -> RemoteSt
mkRemoteSt  RemoteWarm    = RemoteWarmSt
mkRemoteSt  RemoteHot     = RemoteHotSt
mkRemoteSt (RemoteIdle _) = RemoteIdleSt
mkRemoteSt  RemoteCold    = RemoteColdSt


-- | 'Nothing' represents unitialised state.
--
type RemoteTransition = Transition' (Maybe RemoteSt)

type RemoteTransitionTrace peerAddr = TransitionTrace' peerAddr (Maybe RemoteSt)

mkRemoteTransitionTrace :: Ord peerAddr
                        => ConnectionId peerAddr
                        -> InboundGovernorState muxMode peerAddr m a b
                        -> InboundGovernorState muxMode peerAddr m a b
                        -> RemoteTransitionTrace peerAddr
mkRemoteTransitionTrace connId fromState toState =
    TransitionTrace
      (remoteAddress connId)
      Transition { fromState = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (igsConnections fromState)
                 , toState   = mkRemoteSt
                             . csRemoteState
                           <$> Map.lookup connId (igsConnections toState)
                 }


data IGAssertionLocation peerAddr
  = InboundGovernorLoop !(Maybe (ConnectionId peerAddr)) !AbstractState
  deriving Show

data InboundGovernorTrace peerAddr
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
    | TrInboundGovernorCounters      !InboundGovernorCounters
    | TrRemoteState                  !(Map (ConnectionId peerAddr) RemoteSt)
    | TrUnexpectedlyFalseAssertion   !(IGAssertionLocation peerAddr)
    -- ^ This case is unexpected at call site.
  deriving Show
