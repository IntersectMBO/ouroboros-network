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
module Ouroboros.Network.Server2
  ( ServerArguments (..)
  , InboundGovernorObservableState (..)
  , newObservableStateVar
  , newObservableStateVarIO
  , newObservableStateVarFromSeed
  -- * Run server
  , run
  -- * PrunePolicy
  , randomPrunePolicy
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Exception (assert)
import           Control.Applicative (Alternative (..), (<|>))
import           Control.Monad (foldM)
import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Void (Void)
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           System.Random (StdGen)
import qualified System.Random as Rnd

import qualified Network.Mux as Mux
import           Network.Mux.Types ( MiniProtocolStatus (..),
                                     MiniProtocolDir (..))

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.Mux hiding (ControlMessage)
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Server2.ControlChannel (ServerControlChannel)
import qualified Ouroboros.Network.Server2.ControlChannel as ControlChannel
import           Ouroboros.Network.Snocket


-- | Currently only 'StdGen', but in the future this will be extended to
-- a record which contains some useful statistics about peers to support more
-- advances prune strategies (see. 'PruneStrategy').
--
newtype InboundGovernorObservableState = InboundGovernorObservableState {
      igosPrng :: StdGen
    }

-- | Create new observable state 'StrictTVar'.
--
newObservableStateVar
    :: MonadSTM m
    => StdGen
    -> m (StrictTVar m InboundGovernorObservableState)
newObservableStateVar prng =
    newTVarIO (InboundGovernorObservableState prng)


-- | Using the global 'StdGen'.
--
newObservableStateVarIO
    :: IO (StrictTVar IO InboundGovernorObservableState)
newObservableStateVarIO = do
    g <- Rnd.getStdGen
    let (g', igsPrng) = Rnd.split g
    Rnd.setStdGen g'
    newObservableStateVar igsPrng


-- | Useful for testing, is is using 'Rnd.mkStdGen'.
--
newObservableStateVarFromSeed
    :: MonadSTM m
    => Int
    -> m (StrictTVar m InboundGovernorObservableState)
newObservableStateVarFromSeed = newObservableStateVar . Rnd.mkStdGen


-- | 'InboundGovernorState', which consist of pure part, and a mutable part.
-- The mutable part can be observable from outside.  Future version could
-- contain additional statistics on the peers.
--
data InboundGovernorState muxMode peerAddr m a b =
    InboundGovernorState {
        -- | Map of connections state.  Modifying 'igsConnections' outside of
        -- 'inboundGovernorLoop' is not safe.
        --
        igsConnections   :: !(Map (ConnectionId peerAddr)
                                  (ConnectionState muxMode peerAddr m a b)),

        -- | PRNG available to 'PrunePolicy'.
        --
        igsObservableVar :: !(StrictTVar m InboundGovernorObservableState)
      }


-- | Per connection state tracked by /inbound protocol governor/.
--
data ConnectionState muxMode peerAddr m a b = ConnectionState {
      -- | Mux interface.
      --
      csMux             :: !(Mux.Mux muxMode m),

      -- | Connection data flow.
      --
      csDataFlow        :: !DataFlow,

      -- | All supported mini-protocols.
      --
      csMiniProtocolMap :: !(Map MiniProtocolNum
                                (MiniProtocol muxMode ByteString m a b)),

      -- | Map of all running mini-protocol completion STM actions.
      --
      csCompletionMap   :: !(Map MiniProtocolNum
                                 (STM m (Either SomeException b))),

      -- | State of the connection.
      --
      csRemoteState     :: !(RemoteState m)

    }


-- | Each inbound connection is either in 'RemoteIdle', 'RemoteCold' or
-- 'RemoteEstablished' state.  We only need to support
-- @PromotedToWarm^{Duplex}_{Remote}@,
-- @DemotedToCold^{Duplex}_{Remote}@ and
-- @DemotedToCold^{Unidirectional}_{Remote}@ transitions.
--
data RemoteState m
    -- | After @PromotedToWarm^{dataFlow}_{Remote}@ a connection is in
    -- 'RemoteEstablished' state.
    --
    = RemoteEstablished

    -- | After @DemotedToCold^{dataFlow}_{Remote}@ is detected.  This state
    -- corresponds to 'InboundIdleState'. In this state we are checking
    -- if the responder protocols are idle during protocol idle timeout
    -- (represented by an 'STM' action)
    --
    -- 'RemoteIdle' is the initial state of an accepted a connection.
    --
    | RemoteIdle !(STM m ())

    -- | The 'RemoteCold' state for 'Duplex' connections allows us to have
    -- responders started using the on-demand strategy.  This assures that once
    -- the remote peer start using the connection the local side will be ready
    -- to serve it.
    --
    -- For a 'Duplex' connection: a 'RemoteIdle' connection transitions to
    -- 'RemoteCold' state after all responders being idle for
    -- 'serverProtocolIdleTimeout'. This triggers
    -- 'unregisterInboundConnection'.
    --
    -- For a 'Unidreictional' connection: after all responders terminated.
    --
    | RemoteCold

--
-- State management functions
--


-- | Remove connection from 'InboundGovernorState'.
--
unregisterConnection :: Ord peerAddr
                     => ConnectionId peerAddr
                     -> InboundGovernorState muxMode peerAddr m a b
                     -> InboundGovernorState muxMode peerAddr m a b
unregisterConnection connId state =
    state { igsConnections =
              assert (connId `Map.member` igsConnections state) $
              Map.delete connId (igsConnections state)
          }


-- | Update a mini-protocol in 'ConnectionState'.  Once a mini-protocol was
-- restarted we put the new completion action into 'csCompletionMap'.
--
updateMiniProtocol :: Ord peerAddr
                   => ConnectionId peerAddr
                   -> MiniProtocolNum
                   -> STM m (Either SomeException b)
                   -> InboundGovernorState muxMode peerAddr m a b
                   -> InboundGovernorState muxMode peerAddr m a b
updateMiniProtocol connId miniProtocolNum completionAction state =
    state { igsConnections =
              Map.adjust (\connState@ConnectionState { csCompletionMap } ->
                           connState {
                             csCompletionMap =
                               assert (miniProtocolNum `Map.member` csCompletionMap) $
                               Map.insert miniProtocolNum
                                          completionAction
                                          csCompletionMap
                            }
                         )
                         connId
                         (igsConnections state)
          }


-- | Set 'csRemoteState' for a given connection.
--
updateRemoteState :: Ord peerAddr
                  => ConnectionId peerAddr
                  -> RemoteState m
                  -> InboundGovernorState muxMode peerAddr m a b
                  -> InboundGovernorState muxMode peerAddr m a b
updateRemoteState connId csRemoteState state =
    state {
      igsConnections =
        Map.adjust
          (\connState -> connState { csRemoteState })
          connId
          (igsConnections state)
    }


--
-- Edge triggered events
--


-- | Events to which the /inbound protocol governor/ reacts.
--
data Event (muxMode :: MuxMode) peerAddr m a b
    -- | A request to start mini-protocol bundle, either from the server or from
    -- connection manager after a duplex connection was negotiated.
    --
    = NewConnection !(ControlChannel.NewConnection peerAddr
                        (Handle muxMode peerAddr ByteString m a b))

    -- | A multiplexer exited.
    --
    | MuxFinished            !(ConnectionId peerAddr) !(Maybe SomeException)

    -- | A mini-protocol terminated either cleanly or abruptly.
    --
    | MiniProtocolTerminated !(Terminated muxMode peerAddr m a b)

    -- | Transition from 'RemoteEstablished' to 'RemoteIdle'.
    --
    | WaitIdleRemote         !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' to 'RemoteCold'.
    --
    | CommitRemote           !(ConnectionId peerAddr)

    -- | Transition from 'RemoteIdle' or 'RemoteCold' to 'RemoteEstablished'.
    --
    | AwakeRemote            !(ConnectionId peerAddr)


--
-- STM transactions which detect 'Event's
--


-- | A mux stopped.  If mux exited cleanly no error is attached.
--
firstMuxToFinish
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr, Maybe SomeException)
firstMuxToFinish InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId ConnectionState { csMux } outer ->
            (connId,) <$> Mux.muxStopped csMux
        <|> outer)
      empty
      igsConnections


-- | When a mini-protocol terminates we take 'Terminated' out of 'ConnectionState
-- and pass it to the main loop.  This is just enough to decide if we need to
-- restart a mini-protocol and to do the restart.
--
data Terminated muxMode peerAddr m a b = Terminated {
    tConnId       :: !(ConnectionId peerAddr),
    tMux          :: !(Mux.Mux muxMode m),
    tMiniProtocol :: !(MiniProtocol muxMode ByteString m a b),
    tDataFlow     :: !DataFlow,
    tResult       :: !(Either SomeException b)
  }
        

-- | Detect when one of the mini-protocols terminated.
--
-- /triggers:/ 'MiniProtocolTerminated'.
--
firstMiniProtocolToFinish
    :: Alternative (STM m)
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (Terminated muxMode peerAddr m a b)
firstMiniProtocolToFinish InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId ConnectionState { csMux,
                                 csMiniProtocolMap,
                                 csDataFlow,
                                 csCompletionMap
                               }
        outer ->
        Map.foldrWithKey
          (\miniProtocolNum completionAction inner ->
                (\tResult -> Terminated {
                    tConnId       = connId,
                    tMux          = csMux,
                    tMiniProtocol = csMiniProtocolMap Map.! miniProtocolNum,
                    tDataFlow     = csDataFlow,
                    tResult
                  })
            <$> completionAction
            <|> inner
          )
          outer
          csCompletionMap
      )
      empty
      igsConnections


-- | Detect when one of the peers was promoted to warm, e.g.
-- @PromotedToWarm^{Duplex}_{Remote}@ or
-- @PromotedToWarm^{Unidirectional}_{Remote}@.
--
-- /triggers:/ 'PromotedToWarm'
--
-- Note: The specification only describes @PromotedToWarm^{Duplex}_{Remote}@
-- transition, but here we don't make a distinction on @Duplex@ and
-- @Unidirectional@ connections.
--
firstPeerPromotedToWarm
    :: forall muxMode peerAddr m a b. MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr)
firstPeerPromotedToWarm InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId ConnectionState { csMux, csRemoteState } outer ->
        case csRemoteState of
          -- the connection is already in 'RemoteEstablished' state.
          RemoteEstablished -> outer

          -- If the connection is in 'RemoteCold' state we do first to finish
          -- synchronisation to detect incoming traffic on any of the responder
          -- mini-protocols.
          --
          -- This works for both duplex and unidirectional connections (e.g. p2p
          -- \/ non-p2p nodes), for which protocols are started eagerly, unlike
          -- for p2p nodes for which we start all mini-protocols on demand.
          -- Using 'miniProtocolStatusVar' is ok for unidirectional connection,
          -- as we never restart the protocols for them.  They transition to
          -- 'RemoteEstablished' as soon the connection is accepted.  This
          -- is because for eagerly started mini-protocols mux puts them in
          -- 'StatusRunning' as soon as mini-protocols are set in place by
          -- 'runMiniProtocol'.
          RemoteCold ->
            Map.foldrWithKey
              (fn connId)
              outer
              (Mux.miniProtocolStateMap csMux)

          -- We skip it here; this case is done in 'firstPeerDemotedToCold'.
          RemoteIdle {} -> outer
      )
      retry
      igsConnections
  where
    fn :: ConnectionId peerAddr
       -> (MiniProtocolNum, MiniProtocolDir)
       -> STM m MiniProtocolStatus
       -> STM m (ConnectionId peerAddr)
       -> STM m (ConnectionId peerAddr)
    fn connId =
      \(_miniProtcolNum, miniProtocolDir)
      miniProtocolStatus
      inner ->
        case miniProtocolDir of
          InitiatorDir -> inner

          ResponderDir ->
                (do status <- miniProtocolStatus
                    case status of
                      StatusIdle          -> retry
                      StatusStartOnDemand -> retry
                      StatusRunning       -> return connId
                )
            <|> inner



-- | Await for first peer demoted to cold, i.e. detect the
-- @DemotedToCold^{Duplex}_{Remote}@.
--
-- /triggers:/ 'DemotedToColdRemote'
--
firstPeerDemotedToCold
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (Event muxMode peerAddr m a b)
firstPeerDemotedToCold InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId
        ConnectionState {
          csMux,
          csRemoteState
        }
        outer ->
        case csRemoteState of
          -- the connection is already in 'RemoteCold' state
          RemoteCold -> outer

          -- Responders are started using 'StartOnDemand' strategy. We detect
          -- when all of the responders are in 'StatusIdle' or
          -- 'StatusStartOnDemand' and subsequently put the connection in
          -- 'RemoteIdle' state.
          --
          -- In compat mode, when established mini-protocols terminate they will
          -- not be restarted.
          RemoteEstablished ->
                outer
            <|> (Map.foldlWithKey'
                  (\inner (_, miniProtocolDir) miniProtocolStatus ->
                    case miniProtocolDir of
                      InitiatorDir -> inner

                      ResponderDir ->
                           inner
                        >> miniProtocolStatus >>= \case
                             StatusIdle          -> return ()
                             StatusStartOnDemand -> return ()
                             StatusRunning       -> retry
                  )
                  (return ())
                  (Mux.miniProtocolStateMap csMux)
                ) $> WaitIdleRemote connId

          -- Possible for both 'Unidirectional' and 'Duplex' connections; In
          -- non-compat mode wait for first of:
          --
          -- 1. timeout, in which case we will transition to 'RemoteCold',
          -- 2. one of mini-protocols to wake up, in which case we transition
          --    back to 'RemoteEstablished';
          -- 3. mux stopped;
          --
          -- This is done to solve a situation where one of the mini-protocols
          -- terminates quickly while other mini-protocols are inactive
          -- (e.g. their initial messages are still in flight).
          RemoteIdle timeoutSTM ->
              -- In non-compat mode, at this stage we know that all
              -- mini-protocols terminated, and we wait for a period of
              -- idleness.  Note that the meaning of 'timeoutSTM' is
              -- different from the compat mode case.
                  outer
              <|> Map.foldlWithKey'
                    (\inner (_, miniProtocolDir) miniProtocolStatus ->
                        case miniProtocolDir of
                          InitiatorDir -> inner

                          ResponderDir ->
                               (miniProtocolStatus >>= \case
                                 StatusIdle          -> retry
                                 StatusStartOnDemand -> retry
                                 StatusRunning       -> return (AwakeRemote connId))
                            <|> inner
                    )
                    (
                          timeoutSTM $> CommitRemote connId
                    )
                    (Mux.miniProtocolStateMap csMux)
      )
      retry
      igsConnections


--
-- Server API
--


-- | Server static configuration.
--
data ServerArguments (muxMode  :: MuxMode) socket peerAddr versionNumber bytes m a b =
    ServerArguments {
      serverSockets             :: NonEmpty socket,
      serverSnocket             :: Snocket m socket peerAddr,
      serverTracer              :: Tracer m (ServerTrace peerAddr),
      serverConnectionLimits    :: AcceptedConnectionsLimit,
      serverConnectionManager   :: MuxConnectionManager muxMode socket peerAddr
                                                        versionNumber bytes m a b,

      -- | Time for which all protocols need to be idle to trigger
      -- 'DemotedToCold' transition.
      --
      serverProtocolIdleTimeout :: DiffTime,

      -- | Server control var is passed as an argument; this allows to use the
      -- server to run and manage responders which needs to be started on
      -- inbound connections.
      --
      serverControlChannel      :: ServerControlChannel muxMode peerAddr bytes m a b,

      -- | Observable mutable s tate.
      --
      serverObservableStateVar  :: StrictTVar m InboundGovernorObservableState
    }


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
run :: forall muxMode socket peerAddr versionNumber m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadTime  m
       , MonadTimer m
       , HasResponder muxMode ~ True
       , Ord      peerAddr
       )
    => ServerArguments muxMode socket peerAddr versionNumber ByteString m a b
    -> m Void
run ServerArguments {
      serverSockets,
      serverSnocket,
      serverTracer = tracer,
      serverConnectionLimits,
      serverProtocolIdleTimeout,
      serverConnectionManager,
      serverControlChannel,
      serverObservableStateVar
    } = do
      let sockets = NonEmpty.toList serverSockets
          serverState = InboundGovernorState {
              igsConnections   = Map.empty,
              igsObservableVar = serverObservableStateVar
            }
      localAddresses <- traverse (getLocalAddr serverSnocket) sockets
      traceWith tracer (TrServerStarted localAddresses)
      (runConcurrently
          $ foldr1 (<>)
          $ Concurrently (inboundGovernorLoop serverState)
          : (Concurrently . acceptLoop . accept serverSnocket <$> sockets))
        `finally`
          traceWith tracer TrServerStopped
        `catch`
          \(e :: SomeException) -> do
            case fromException e of
              Just (_ :: AsyncCancelled) -> pure ()
              Nothing -> traceWith tracer (TrServerError e)
            throwIO e
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
                                   
                                   Left err -> do
                                     -- errors when starting a mini-protocol are
                                     -- non-recoverable; we close the connection
                                     -- and allow the server to continue.
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
                              v <- registerDelay serverProtocolIdleTimeout
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
                  -- there is no way to recover from such errors; we stop
                  -- mux which allows to recover resources held by
                  -- connection manager as well.
                  traceWith tracer (TrResponderStartFailure tConnId num err)
                  Mux.stopMux tMux
                  inboundGovernorLoop
                    (unregisterConnection tConnId state)

        WaitIdleRemote connId -> do
          -- @
          --    DemotedToCold^{dataFlow}_{Remote} : InboundState Duplex
          --                                      → InboundIdleState Duplex
          -- @
          res <- demotedToColdRemote serverConnectionManager
                                     (remoteAddress connId)
          traceWith tracer (TrWaitIdleRemote connId res)
          v <- registerDelay serverProtocolIdleTimeout
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
          res <- promotedToWarmRemote serverConnectionManager
                                      (remoteAddress connId)
          traceWith tracer (TrPromotedToWarmRemote connId res)
          assert (resultInState res /= UnknownConnectionSt) $
            inboundGovernorLoop
              (updateRemoteState connId
                                 RemoteEstablished
                                 state)

        CommitRemote connId -> do
          res <- unregisterInboundConnection serverConnectionManager
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

                DemotedToColdRemoteTr ->
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


    acceptLoop :: Accept m socket peerAddr
               -> m Void
    acceptLoop acceptOne = do
      runConnectionRateLimits
        (TrAcceptPolicyTrace `contramap` tracer)
        (numberOfConnections serverConnectionManager)
        serverConnectionLimits
      result <- runAccept acceptOne
      case result of
        (AcceptFailure err, acceptNext) -> do
          traceWith tracer (TrAcceptError err)
          acceptLoop acceptNext
        (Accepted socket peerAddr, acceptNext) -> do
          traceWith tracer (TrAcceptConnection peerAddr)
          -- using withAsync ensures that the thread that includes inbound
          -- connection (which is a blocking operation), is killed when the
          -- server terminates.
          withAsync
            (do
              a <-
                includeInboundConnection
                  serverConnectionManager
                  socket peerAddr
              case a of
                Connected connId dataFlow handle ->
                  atomically $
                    ControlChannel.writeMessage
                      serverControlChannel
                      (ControlChannel.NewConnection Inbound connId dataFlow handle)
                Disconnected {} ->
                  pure ()
            )
            $ \_ -> acceptLoop acceptNext


-- | Run a responder mini-protocol.
--
-- @'HasResponder' mode ~ True@ is used to rule out
-- 'InitiatorProtocolOnly' case.
--
runResponder :: forall (mode :: MuxMode) m a b.
                 ( HasResponder mode ~ True
                 , MonadAsync m
                 , MonadCatch m
                 )
              => Mux.Mux mode m
              -> MiniProtocol mode ByteString m a b
              -> Mux.StartOnDemandOrEagerly
              -> m (Either SomeException (STM m (Either SomeException b)))
runResponder mux
             MiniProtocol { miniProtocolNum, miniProtocolRun }
             startStrategy =
    try $
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

data ServerTrace peerAddr
    = TrAcceptConnection            !peerAddr
    | TrNewConnection               !Provenance !(ConnectionId peerAddr)
    | TrAcceptError                 !SomeException
    | TrAcceptPolicyTrace           !AcceptConnectionsPolicyTrace
    | TrServerStarted               ![peerAddr]
    | TrServerStopped
    | TrServerError                 !SomeException
    -- ^ similar to 'TrAcceptConnection' but it is logged once the connection is
    -- handed to inbound connection manager, e.g. after handshake negotiation.
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
