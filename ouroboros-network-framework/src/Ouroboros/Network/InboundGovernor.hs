{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
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
  -- * PrunePolicy
  , randomPrunePolicy
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
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)

import           Data.Cache
import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random as Rnd

import qualified Network.Mux as Mux

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
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
    let state = InboundGovernorState {
            igsConnections   = Map.empty,
            igsObservableVar = observableStateVar,
            igsCountersCache = mempty
          }
    inboundGovernorLoop state
  where
    -- The inbound protocol governor recursive loop.  The 'igsConnections' is
    -- updated as we recurs.
    --
    inboundGovernorLoop
      :: InboundGovernorState muxMode peerAddr m a b
      -> m Void
    inboundGovernorLoop !state = do
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
            <|>                             firstPeerDemotedToCold state
            <|> (NewConnection          <$> ControlChannel.readMessage
                                              serverControlChannel)
      case event of
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
              traceWith trTracer (mkRemoteTransitionTrace connId state state')
              inboundGovernorLoop state'

        MuxFinished connId merr -> do

          case merr of
            Nothing  -> traceWith tracer (TrMuxCleanExit connId)
            Just err -> traceWith tracer (TrMuxErrored connId err)

          -- the connection manager does should realise this on itself.
          let state' = unregisterConnection connId state
          traceWith trTracer (mkRemoteTransitionTrace connId state state')
          inboundGovernorLoop state'

        MiniProtocolTerminated
          Terminated {
              tConnId,
              tMux,
              tMiniProtocolData = MiniProtocolData { mpdMiniProtocol,
                                                     mpdMiniProtocolTemp
                                                   },
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
              traceWith trTracer (mkRemoteTransitionTrace tConnId state state')
              inboundGovernorLoop state'

            Right _ -> do
              result
                <- runResponder tMux mpdMiniProtocol Mux.StartOnDemand
              case result of
                Right completionAction -> do
                  traceWith tracer (TrResponderRestarted tConnId num)

                  let isHot = mpdMiniProtocolTemp == Hot
                      state' = ( if isHot
                                 then updateRemoteState tConnId RemoteWarm
                                 else id
                               )
                             . updateMiniProtocol tConnId num completionAction
                             $ state

                  -- remote state is only updated when 'isHot' is 'True'
                  when isHot
                     $ traceWith trTracer (mkRemoteTransitionTrace tConnId state state')
                  inboundGovernorLoop state'

                Left err -> do
                  -- there is no way to recover from synchronous exceptions; we
                  -- stop mux which allows to close resources held by
                  -- connection manager.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stopMux tMux

                  let state' = unregisterConnection tConnId state
                  traceWith trTracer (mkRemoteTransitionTrace tConnId state state')
                  inboundGovernorLoop state'


        WaitIdleRemote connId -> do
          -- @
          --    DemotedToCold^{dataFlow}_{Remote} : InboundState Duplex
          --                                      → InboundIdleState Duplex
          -- @
          res <- demotedToColdRemote connectionManager
                                     (remoteAddress connId)
          traceWith tracer (TrWaitIdleRemote connId res)
          v <- registerDelay inboundIdleTimeout
          let timeoutSTM :: STM m ()
              !timeoutSTM = LazySTM.readTVar v >>= check

          let state' = updateRemoteState connId (RemoteIdle timeoutSTM) state
          traceWith trTracer (mkRemoteTransitionTrace connId state state')
          inboundGovernorLoop state'

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
          _ <- evaluate $ assert (resultInState res /= UnknownConnectionSt)
          let state' = updateRemoteState
                         connId
                         RemoteWarm
                         state
          traceWith trTracer (mkRemoteTransitionTrace connId state state')
          inboundGovernorLoop state'

        RemotePromotedToHot connId -> do
          traceWith tracer (TrPromotedToHotRemote connId)
          let state' = updateRemoteState connId RemoteHot state
          traceWith trTracer (mkRemoteTransitionTrace connId state state')
          inboundGovernorLoop state'

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
              traceWith trTracer (mkRemoteTransitionTrace connId state state')
              inboundGovernorLoop state'

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
                  traceWith trTracer (mkRemoteTransitionTrace connId state state')
                  inboundGovernorLoop state'

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
                  traceWith trTracer (mkRemoteTransitionTrace connId state state')
                  inboundGovernorLoop state'


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
-- PrunePolicy
--

-- | Sort by upstreamness and a random score.
--
-- Note: this 'PrunePolicy' does not depend on 'igsConnections'.  We put
-- 'igsPrng' in 'InboundGovernorState' only to show that we can have
-- a 'PrunePolicy' which depends on the 'InboundGovernorState' as a more
-- refined policy would do.
--
-- /complexity:/ \(\mathcal{O}(n\log\;n)\)
--
-- TODO: complexity could be improved.
--
randomPrunePolicy :: ( MonadSTM m
                     , Ord peerAddr
                     )
                  => StrictTVar m InboundGovernorObservableState
                  -> PrunePolicy peerAddr (STM m)
randomPrunePolicy stateVar mp n = do
    state <- readTVar stateVar
    let (prng', prng'') = Rnd.split (igosPrng state)
    writeTVar stateVar (state { igosPrng = prng'' })

    return
      $ Set.fromList
      . take n
      . map (fst . fst)
      -- 'True' values (upstream / outbound connections) will sort last.
      . sortOn (\((_, connType), score) -> (isUpstream connType, score))
      . zip (Map.assocs mp)
      $ (Rnd.randoms prng' :: [Int])
  where
    isUpstream :: ConnectionType -> Bool
    isUpstream = \connType ->
      case connType of
        UnnegotiatedConn Outbound -> True
        UnnegotiatedConn Inbound  -> False
        OutboundIdleConn _        -> True
        InboundIdleConn         _ -> False
        NegotiatedConn Outbound _ -> True
        NegotiatedConn Inbound  _ -> False
        DuplexConn                -> True

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




data InboundGovernorTrace peerAddr
    = TrNewConnection               !Provenance !(ConnectionId peerAddr)
    | TrResponderRestarted          !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderStartFailure       !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderErrored            !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderStarted            !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderTerminated         !(ConnectionId peerAddr) !MiniProtocolNum
    | TrPromotedToWarmRemote        !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrPromotedToHotRemote         !(ConnectionId peerAddr)
    | TrDemotedToColdRemote         !(ConnectionId peerAddr) !(OperationResult DemotedToColdRemoteTr)
    -- ^ All mini-protocols terminated.  The boolean is true if this connection
    -- was not used by p2p-governor, and thus the connection will be terminated.
    | TrWaitIdleRemote              !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrMuxCleanExit                !(ConnectionId peerAddr)
    | TrMuxErrored                  !(ConnectionId peerAddr) SomeException
    | TrInboundGovernorCounters     !InboundGovernorCounters
    | TrRemoteState                 !(Map (ConnectionId peerAddr) RemoteSt)
  deriving Show
