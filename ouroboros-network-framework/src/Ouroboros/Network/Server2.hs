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
import           Data.Set (Set)
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
import           Ouroboros.Network.MuxMode
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

      -- | Set of established mini-protocols; only need for compatiblity mode.
      --
      csEstablishedSet  :: !(Set MiniProtocolNum),

      -- | Compatibility mode for 'NodeToNodeV_5' and lower.
      --
      csCompatMode      :: !Bool,

      -- | State of the connection.
      --
      csRemoteState     :: !(RemoteState m)

    }
                          
              


-- | Each inbound connection is either in 'RemoteWaitIdle', 'RemoteCold' or
-- 'RemoteEstablished' state.  We only need to support
-- @PromotedToWarm^{Duplex}_{Remote}@,
-- @DemotedToCold^{Duplex}_{Remote}@ and
-- @DemotedToCold^{Unidirectional}_{Remote}@ transitions.
--
data RemoteState m
    -- | After @PromotedToWarm^{Duplex}_{Remote}@ a connection is in
    -- 'RemoteEstablished' state.
    --
    = RemoteEstablished

    -- | After @PromotedToCold^{Duplex}_{Remote}@ is detected.  This state is
    -- similar to 'WaitRemoteIdle', in that in both states we are checking if
    -- the responder protocols are idle, they are however triggered by different
    -- events: @PromotedToCold^{Duplex}_{Remote}@ vs
    -- @DemotedToCold^{Cold}_{Local}@.
    --
    | RemoteWaitIdle !(STM m ())

    -- | For a 'Duplex' connection: after all responders being idle for
    -- 'serverRespondersIdleTimeout', a 'RemoteWaitIdle' transitions to
    -- 'RemoteCold', which triggers 'unregisterInboundConnection'.
    --
    -- For a 'Unidreictional' connection: after all responders terminated.
    --
    | RemoteCold

--
-- State management functions
--


-- | Register a new connection in 'InboundGovernorState'.
--
registerConnection :: Ord peerAddr
                   => ConnectionId peerAddr
                   -> ConnectionState      muxMode peerAddr m a b
                   -> InboundGovernorState muxMode peerAddr m a b
                   -> InboundGovernorState muxMode peerAddr m a b
registerConnection connId connState state =
    state { igsConnections =
              Map.insert connId connState (igsConnections state)
          }

-- | Remove connection from 'InboundGovernorState'.
--
unregisterConnection :: Ord peerAddr
                     => ConnectionId peerAddr
                     -> InboundGovernorState muxMode peerAddr m a b
                     -> InboundGovernorState muxMode peerAddr m a b
unregisterConnection connId state =
    state { igsConnections =
              Map.delete connId (igsConnections state)
          }

-- |  Delete a mini-protocol from 'ConnectionState'.  This is only useful for
-- 'Unidirectional' connections for which we don't need to restart
-- mini-protocols which terminated.
--
unregisterMiniProtocol :: Ord peerAddr
                       => ConnectionId peerAddr
                       -> MiniProtocolNum
                       -> InboundGovernorState muxMode peerAddr m a b
                       -> InboundGovernorState muxMode peerAddr m a b
unregisterMiniProtocol connId miniProtocolNum state =
    state { igsConnections =
              Map.adjust
                (\connState ->
                  connState {
                      csCompletionMap =
                        Map.delete miniProtocolNum
                          (csCompletionMap connState)
                    })
                connId
                (igsConnections state)
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


-- | Events to which the /inbound protocol governor/ reacts.  They split into
-- two groups: ones that are generated by the /inbound protocol governor/
-- itself, usually as a result of mux state change (i.e. mini-protocol
-- termination) and ones that can also come from outside as a result of
-- connection state change in the /connection manager/.
--
data Event (muxMode :: MuxMode) peerAddr m a b
    -- | A request to start mini-protocol bundle, either from the server or from
    -- connection manager.
    --
    -- This even is triggered either by accepting a connection or by connection
    -- manager, after creating duplex outbound connection.
    --
    = NewConnection !(ControlChannel.NewConnection peerAddr
                         (Handle muxMode peerAddr ByteString m a b))

    -- | A mini-protocol terminated either cleanly or abruptly.
    --
    | MiniProtocolTerminated !(Terminated muxMode peerAddr m a b)


    -- | Connection promoted to warm state by the remote peer.
    --
    | PromotedToWarmRemote   !(ConnectionId peerAddr)

    -- | Connection demoted to cold state by the remote peer.
    --
    | DemotedToColdRemote    !(DemotedToColdAction peerAddr)


--
-- STM transactions which detect 'Event's
--


-- | When a mini-protocol terminates we take 'Terminated' out of 'ConnectionState
-- and pass it to the main loop.  This is just enough to decide if we need to
-- restart a mini-protocol and to do the restart.
--
data Terminated muxMode peerAddr m a b = Terminated {
    tConnId                  :: !(ConnectionId peerAddr),
    tMux                     :: !(Mux.Mux muxMode m),
    tMiniProtocol            :: !(MiniProtocol muxMode ByteString m a b),
    tDataFlow                :: !DataFlow,
    tResult                  :: !(Either SomeException b),
    tEstablishedInCompatMode :: !Bool
    -- ^ an established protocol terminated in compat mode
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
                                 csCompletionMap,
                                 csCompatMode,
                                 csEstablishedSet
                                 }
        outer ->
        Map.foldrWithKey
          (\miniProtocolNum completionAction inner ->
                (\tResult -> Terminated {
                    tConnId       = connId,
                    tMux          = csMux,
                    tMiniProtocol = csMiniProtocolMap Map.! miniProtocolNum,
                    tDataFlow     = csDataFlow,
                    tResult,
                    tEstablishedInCompatMode =
                         csCompatMode
                      && miniProtocolNum `elem` csEstablishedSet
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
firstPromotedToWarm
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (ConnectionId peerAddr)
firstPromotedToWarm InboundGovernorState { igsConnections } =
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
              (\(_miniProtcolNum, miniProtocolDir)
                miniProtocolStatusVar
                inner ->
                  case miniProtocolDir of
                    InitiatorDir -> inner

                    ResponderDir ->
                          (do status <- readTVar miniProtocolStatusVar
                              case status of
                                StatusIdle          -> retry
                                StatusStartOnDemand -> retry
                                StatusRunning       -> return connId
                          )
                      <|> inner
              )
              outer
              (Mux.miniProtocolStateMap csMux)

          RemoteWaitIdle {} -> outer
      )
      retry
      igsConnections


-- | Transition to one of 'RemoteState''s.
--
data DemotedToColdAction peerAddr =
    -- | Transition from 'RemoteEstablished' to 'RemoteWaitIdle'.
    --
    ToRemoteWaitIdle    !(ConnectionId peerAddr)

    -- | Transition from 'RemoteWaitIdle' to 'RemoteCold'.
    --
  | ToRemoteCold        !(ConnectionId peerAddr)

    -- | Transition from 'RemoteWaitIdle' to 'RemoteEstablished'.
    --
  | ToRemoteEstablished !(ConnectionId peerAddr)


-- | Await for first peer demoted to cold, i.e. detect the
-- @DemotedToCold^{Duplex}_{Remote}@.
--
-- /triggers:/ 'DemotedToColdRemote'
--
firstDemotedToCold
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr m a b
    -> STM m (DemotedToColdAction peerAddr)
firstDemotedToCold InboundGovernorState { igsConnections } =
    Map.foldrWithKey
      (\connId
        ConnectionState {
          csMux,
          csRemoteState,
          csCompatMode,
          csEstablishedSet
        }
        outer ->
        case csRemoteState of
          -- the connection is already in 'RemoteCold' state
          RemoteCold -> outer

          -- Responders are started using 'StartOnDemand' strategy. We detect
          -- when all of the responders are in 'StatusIdle' or
          -- 'StatusStartOnDemand' and subsequently put the connection in
          -- 'RemoteWaitIdle' state.
          --
          -- In compat mode, when established mini-protocols terminate they will
          -- not be restarted.
          RemoteEstablished ->
            let protocolMap =
                  if csCompatMode
                    then
                      Mux.miniProtocolStateMap csMux
                        `Map.restrictKeys`
                          (Set.map (,ResponderDir) csEstablishedSet)
                    else
                      Mux.miniProtocolStateMap csMux
            in 
                  (Map.foldrWithKey
                    (\(_, miniProtocolDir) miniProtocolStatusVar inner ->
                      case miniProtocolDir of
                        InitiatorDir -> inner

                        ResponderDir ->
                             inner
                          >> readTVar miniProtocolStatusVar >>= \case
                               StatusIdle          -> return ()
                               StatusStartOnDemand -> return ()
                               StatusRunning       -> retry
                      
                    )
                    (return ())
                    protocolMap
                  ) $> ToRemoteWaitIdle connId
              <|> outer

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
          RemoteWaitIdle timeoutSTM ->
            if csCompatMode
              then
                -- In compat mode, we do not restart established mini-protocols.
                -- We just give time for all other mini-protocols to
                -- terminate.  We cannot wait here on other mini-protocols to
                -- become idle, since hot and warm mini-protocols needs to be
                -- restarted whenever they finish (to suport remote `hot → warm`
                -- and `warm → hot` transitions).  We are also not guaranteed on
                -- the order in which mini-protocols terminate, thus simply not
                -- restarting hot and warm protocols in 'RemoteWaitIdle` state
                -- will not work.   We choose the simplest choice, just wait on
                -- `timeoutSTM` or mux termination before sealing
                -- 'ToRemoteCold'.  Though a non timely termination should be
                -- considered a protocol error, we will not treat it as such.
                    timeoutSTM           $> ToRemoteCold connId
                <|> Mux.muxStopped csMux $> ToRemoteCold connId
              else
                -- In non-compat mode, at this stage we know that all
                -- mini-protocols terminated, and we wait for a period of
                -- idleness.  Note that the meaning of 'timeoutSTM' is
                -- different frrom the compat mode case.
                Map.foldrWithKey
                  (\(_, miniProtocolDir) miniProtocolStatusVar inner ->
                      case miniProtocolDir of
                        InitiatorDir -> inner

                        ResponderDir ->
                             (readTVar miniProtocolStatusVar >>= \case
                               StatusIdle          -> retry
                               StatusStartOnDemand -> retry
                               StatusRunning       -> return (ToRemoteEstablished connId))
                          <|> inner
                  )
                  ( 
                        timeoutSTM           $> ToRemoteCold connId
                    <|> Mux.muxStopped csMux $> ToRemoteCold connId
                    <|> outer
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
      serverHasInitiator        :: SingHasInitiator muxMode,
      serverSockets             :: NonEmpty socket,
      serverSnocket             :: Snocket m socket peerAddr,
      serverTracer              :: Tracer m (ServerTrace peerAddr),
      serverConnectionLimits    :: AcceptedConnectionsLimit,
      serverConnectionManager   :: MuxConnectionManager muxMode socket peerAddr
                                                        versionNumber bytes m a b,

      -- | Time for which all protocols need to be idle to trigger
      -- 'DemotedToCold' transition.
      --
      serverRespondersIdleTimeout :: DiffTime,

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
      serverHasInitiator,
      serverSockets,
      serverSnocket,
      serverTracer = tracer,
      serverConnectionLimits,
      serverRespondersIdleTimeout,
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
          : (Concurrently . acceptLoop . accept serverSnocket) `map` sockets)
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
                (NewConnection          <$> ControlChannel.readMessage
                                              serverControlChannel)
            <|> (MiniProtocolTerminated <$> firstMiniProtocolToFinish state)
            <|> (PromotedToWarmRemote   <$> firstPromotedToWarm state)
            <|> (DemotedToColdRemote    <$> firstDemotedToCold state)

      case event of
        NewConnection
          -- new connection has been announced by either accept loop or
          -- by connection manager (in which case the connection is in
          -- 'DuplexState').
          (ControlChannel.NewConnection
            provenance
            connId
            csDataFlow
            (Handle csMux
                    muxBundle
                    _
                    csCompatMode)) -> do
              traceWith tracer (TrStartResponders provenance connId)
              let csMiniProtocolMap =
                    foldr
                      (\miniProtocol ->
                        Map.insert (miniProtocolNum miniProtocol)
                                    miniProtocol)
                      Map.empty
                      (concat muxBundle)

              csCompletionMap
                <-
                foldM
                  (\mp miniProtocol -> do
                     completion <- runResponder
                                     csMux miniProtocol
                                     (if csCompatMode
                                        then Mux.StartEagerly
                                        else Mux.StartOnDemand)
                     return (Map.insert (miniProtocolNum miniProtocol)
                                        completion mp)
                     )
                  Map.empty
                  csMiniProtocolMap
            
              let connState = ConnectionState {
                      csMux,
                      csDataFlow,
                      csMiniProtocolMap,
                      csCompletionMap,
                      csEstablishedSet =
                        case muxBundle of
                          Bundle { withEstablished = WithEstablished ptls } ->
                            Set.fromList [ miniProtocolNum ptl | ptl <- ptls ],
                      csRemoteState = RemoteCold,
                      csCompatMode
                    }

              inboundGovernorLoop
                (registerConnection connId connState state)

        MiniProtocolTerminated
          Terminated {
              tConnId,
              tMux,
              tMiniProtocol,
              tResult,
              tEstablishedInCompatMode
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
              -- restart the mini-protocol; 'Duplex' mode is only possible when
              -- not compatibility mode if off.
              if tEstablishedInCompatMode
                then do
                  traceWith tracer
                    (TrEstablishedMiniProtocolTerminated tConnId)
                  inboundGovernorLoop
                    (unregisterMiniProtocol tConnId num state)
                else do
                  completionAction
                    <- runResponder tMux tMiniProtocol Mux.StartOnDemand
                  traceWith tracer $
                    TrResponderRestarted tConnId num
                  inboundGovernorLoop
                    (updateMiniProtocol tConnId num completionAction state)

        PromotedToWarmRemote connId -> do
          case serverHasInitiator of
            SingHasInitiator -> do
              res <- promotedToWarmRemote serverConnectionManager (remoteAddress connId)
              case res of
                UnsupportedState inState ->
                  traceWith tracer
                    (TrPromoteToWarmRemoteInUnsupportedState connId inState)
                OperationSuccess _ ->
                  traceWith tracer (TrPromotedToWarmRemote connId)
              inboundGovernorLoop
                (updateRemoteState connId RemoteEstablished state)

            SingNoInitiator ->
              inboundGovernorLoop
                (updateRemoteState connId RemoteEstablished state)

        DemotedToColdRemote (ToRemoteWaitIdle connId) -> do
          traceWith tracer (TrDemotedToRemoteWaitIdle connId)
          v <- registerDelay serverRespondersIdleTimeout
          let timeoutSTM :: STM m ()
              !timeoutSTM = LazySTM.readTVar v >>= check
          inboundGovernorLoop
            (updateRemoteState connId
                               (RemoteWaitIdle timeoutSTM)
                               state)

        DemotedToColdRemote (ToRemoteEstablished connId) -> do
          traceWith tracer (TrPromotedToRemoteEstablished connId)
          inboundGovernorLoop
            (updateRemoteState connId
                               RemoteEstablished
                               state)

        DemotedToColdRemote (ToRemoteCold connId) -> do
          result <- unregisterInboundConnection
                      serverConnectionManager (remoteAddress connId)
          case result of
            UnsupportedState (InOutboundState Duplex) ->
              assert False $
              -- Unexpected state; if we already are in @'OutboundState'
              -- 'Duplex'@, it means we already 'DemotedToColdRemote' before.
              inboundGovernorLoop state

            UnsupportedState inState -> do
              traceWith tracer
                (TrDemoteToColdRemoteInUnsupportedState connId inState)
              -- 'inState' can be either:
              -- @'UnknownConnection'@,
              -- @'InReservedOutboundState'@,
              -- @'InUnnegotiatedState',
              -- @'InOutboundState' 'Unidirectional'@, 
              -- @'InTerminatingState'@,
              -- @'InTermiantedState'@.
              inboundGovernorLoop
                (unregisterConnection connId state)

            OperationSuccess tr -> do
              traceWith tracer $
                TrDemotedToColdRemote connId tr
              case tr of
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
                -- it in 'RemoteCold' state.
                -- @
                --    DemotedToCold^{Duplex}_{Remote} : DuplexState
                --                                    → OutboundState Duplex
                -- @
                DemotedToColdRemoteDuplexTr ->
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
          -- server is killed, possibly by an async exception.
          withAsync
            (do
              a <-
                includeInboundConnection
                  serverConnectionManager
                  socket peerAddr
              case a of
                Connected connId dataFlow handle ->
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
              -> m (STM m (Either SomeException b))
runResponder mux
             MiniProtocol { miniProtocolNum, miniProtocolRun }
             startStrategy =
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
    | TrStartResponders             !Provenance  !(ConnectionId peerAddr)
    | TrAcceptError                 !SomeException
    | TrAcceptPolicyTrace           !AcceptConnectionsPolicyTrace
    | TrServerStarted               ![peerAddr]
    | TrServerStopped
    | TrServerError                 !SomeException
    | TrResponderRestarted          !(ConnectionId peerAddr) !MiniProtocolNum
    | TrResponderErrored            !(ConnectionId peerAddr) !MiniProtocolNum
                                    !SomeException
    | TrResponderTerminated         !(ConnectionId peerAddr) !MiniProtocolNum
    | TrPromotedToWarmRemote        !(ConnectionId peerAddr)
    | TrPromoteToWarmRemoteInUnsupportedState
                                    !(ConnectionId peerAddr) !InState
    | TrDemotedToColdRemote         !(ConnectionId peerAddr) !DemotedToColdRemoteTr
    -- ^ All mini-protocols terminated.  The boolean is true if this connection
    -- was not used by p2p-governor, and thus the connection will be terminated.
    | TrDemoteToColdRemoteInUnsupportedState
                                    !(ConnectionId peerAddr) !InState
    | TrDemotedToRemoteWaitIdle     !(ConnectionId peerAddr)
    | TrPromotedToRemoteEstablished !(ConnectionId peerAddr)
    | TrEstablishedMiniProtocolTerminated
                                    !(ConnectionId peerAddr)
    -- ^ An established mini-protocol terminated; This is only logged for
    -- connections in compat mode.
  deriving Show
