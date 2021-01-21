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
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Exception (SomeAsyncException (..), assert)
import           Control.Applicative (Alternative (..), (<|>))
import           Control.Monad (foldM)
import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.List (sortOn)
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
                   , MonadThrow    m
                   , MonadThrow    (STM m)
                   , MonadTime     m
                   , MonadTimer    m
                   , Ord peerAddr
                   , HasResponder muxMode ~ True
                   )
                => Tracer m (InboundGovernorTrace peerAddr)
                -> ServerControlChannel muxMode peerAddr ByteString m a b
                -> DiffTime -- protocol idle timeout
                -> MuxConnectionManager muxMode socket peerAddr
                                        versionNumber ByteString m a b
                -> StrictTVar m InboundGovernorObservableState
                -> m Void
inboundGovernor tracer serverControlChannel inboundIdleTimeout
                connectionManager observableStateVar = do
    let state = InboundGovernorState {
            igsConnections   = Map.empty,
            igsObservableVar = observableStateVar
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
      event
        <- atomically $
                (uncurry MuxFinished    <$> firstMuxToFinish state)
            <|> (MiniProtocolTerminated <$> firstMiniProtocolToFinish state)
            <|> (AwakeRemote            <$> firstPeerPromotedToWarm state)
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

              -- update state and continue the recursive loop
              (\igsConnections -> inboundGovernorLoop state { igsConnections })
                =<< Map.alterF
                      (\case
                        -- connection
                        Nothing -> do
                          let csMiniProtocolMap =
                                foldr
                                  (\miniProtocol ->
                                    Map.insert (miniProtocolNum miniProtocol)
                                                miniProtocol)
                                  Map.empty
                                  (concat muxBundle)

                          mCompletionMap
                            <-
                            foldM
                              (\acc miniProtocol -> do
                                 result <- runResponder
                                             csMux miniProtocol
                                             Mux.StartOnDemand
                                 case result of
                                   -- synchronous exceptions when starting
                                   -- a mini-protocol are non-recoverable; we
                                   -- close the connection and allow the server
                                   -- to continue.
                                   Left err -> do
                                     traceWith tracer (TrResponderStartFailure connId (miniProtocolNum miniProtocol) err)
                                     Mux.stopMux csMux
                                     return Nothing

                                   Right completion ->  do
                                     let acc' = Map.insert (miniProtocolNum miniProtocol)
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

        MuxFinished connId merr -> do
          case merr of
            Nothing  -> traceWith tracer (TrMuxCleanExit connId)
            Just err -> traceWith tracer (TrMuxErrored connId err)
          -- the connection manager does should realise this on itself.
          inboundGovernorLoop (unregisterConnection connId state)

        MiniProtocolTerminated
          Terminated {
              tConnId,
              tMux,
              tMiniProtocol,
              tResult
            } ->
          let num = miniProtocolNum tMiniProtocol in
          case tResult of
            Left e -> do
              -- a mini-protocol errored.  In this case mux will shutdown, and
              -- the connection manager will tear down the socket.  We can just
              -- forget the connection from 'InboundGovernorState'.
              traceWith tracer $
                TrResponderErrored tConnId num e
              inboundGovernorLoop
                (unregisterConnection
                  tConnId
                  state)

            Right _ -> do
              result
                <- runResponder tMux tMiniProtocol Mux.StartOnDemand
              case result of
                Right completionAction -> do
                  traceWith tracer (TrResponderRestarted tConnId num)
                  inboundGovernorLoop
                    (updateMiniProtocol tConnId num completionAction state)
                Left err -> do
                  -- there is no way to recover from synchronous exceptions; we
                  -- stop mux which allows to close resources held by
                  -- connection manager.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stopMux tMux
                  inboundGovernorLoop
                    (unregisterConnection tConnId state)

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
          inboundGovernorLoop
            (updateRemoteState connId
                               (RemoteIdle timeoutSTM)
                               state)

        -- @
        --    PromotedToWarm^{Duplex}_{Remote}
        -- @
        -- or
        -- @
        --    Awake^{dataFlow}_{Remote}
        -- @
        AwakeRemote connId -> do
          -- notify the connection manager about the transiton
          res <- promotedToWarmRemote connectionManager
                                      (remoteAddress connId)
          traceWith tracer (TrPromotedToWarmRemote connId res)
          assert (resultInState res /= UnknownConnectionSt) $
            inboundGovernorLoop
              (updateRemoteState connId
                                 RemoteEstablished
                                 state)

        CommitRemote connId -> do
          res <- unregisterInboundConnection connectionManager
                                             (remoteAddress connId)
          traceWith tracer $ TrDemotedToColdRemote connId res
          case res of
            UnsupportedState {} ->
              -- 'inState' can be either:
              -- @'UnknownConnection'@,
              -- @'InReservedOutboundState'@,
              -- @'InUnnegotiatedState',
              -- @'InOutboundState' 'Unidirectional'@,
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              inboundGovernorLoop
                (unregisterConnection connId state)

            OperationSuccess transition ->
              case transition of
                -- the following two cases are when the connection was not used
                -- by p2p-governor, the connection will be closed.
                CommitTr ->
                  -- @
                  --    Commit^{dataFlow}_{Remote} : InboundIdleState dataFlow
                  --                               → TerminatingState
                  -- @
                  inboundGovernorLoop
                    (unregisterConnection connId state)

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
                KeepTr ->
                  inboundGovernorLoop
                    (updateRemoteState connId RemoteCold state)


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
        InboundIdleConn         _ -> False
        NegotiatedConn Outbound _ -> True
        NegotiatedConn Inbound  _ -> False
        DuplexConn                -> True

--
-- Trace
--

data InboundGovernorTrace peerAddr
    = TrNewConnection               !Provenance !(ConnectionId peerAddr)
    | TrResponderRestarted          !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderStartFailure       !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderErrored            !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | TrResponderTerminated         !(ConnectionId peerAddr) !MiniProtocolNum
    | TrPromotedToWarmRemote        !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrDemotedToColdRemote         !(ConnectionId peerAddr) !(OperationResult DemotedToColdRemoteTr)
    -- ^ All mini-protocols terminated.  The boolean is true if this connection
    -- was not used by p2p-governor, and thus the connection will be terminated.
    | TrWaitIdleRemote              !(ConnectionId peerAddr) !(OperationResult AbstractState)
    | TrMuxCleanExit                !(ConnectionId peerAddr)
    | TrMuxErrored                  !(ConnectionId peerAddr) SomeException
  deriving Show
