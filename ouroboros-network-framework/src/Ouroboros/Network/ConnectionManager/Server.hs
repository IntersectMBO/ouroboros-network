{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Server implementation based on 'ConnectionManager'
--
-- TODO: in the futures this should be moved to `Ouroboros.Network.Server`, but
-- to avoid confusion it will be kept here for now.
--
module Ouroboros.Network.ConnectionManager.Server
  ( ServerArguments (..)
  , run
  , newStateVar
  , newStateVarIO
  , newStateVarFromSeed
  -- * PrunePolicy
  , randomPrunePolicy
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (assert)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.Bool (bool)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (($>))
import           Data.Void (Void)
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable
import           System.Random (StdGen)
import qualified System.Random as Rnd

import qualified Network.Mux as Mux
import qualified Network.Mux.Types as Mux

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Server.ControlChannel (ServerControlChannel)
import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server
import           Ouroboros.Network.Mux hiding (ControlMessage)
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Snocket


-- | 'InboundGovernorState'
--
data InboundGovernorState muxMode peerAddr versionNumber m a b =
    InboundGovernorState {
        -- | Map of connections state.  Modifying 'igsConnections' outside of
        -- 'inboundGovernorThread' is not safe.
        --
        igsConnections :: Map (ConnectionId peerAddr)
                              (ConnectionState muxMode peerAddr versionNumber m a b),

        -- | PRNG used available to 'PrunePolicy'.
        --
        igsPrng        :: StdGen
      }


-- | Create new server setate 'TVar'.
--
newStateVar
    :: MonadSTM m
    => StdGen
    -> m (StrictTVar m (InboundGovernorState muxMode peerAddr versionNumber m a b))
newStateVar igsPrng =
    newTVarIO InboundGovernorState {
        igsConnections = Map.empty,
        igsPrng
      }


-- | Using the global 'StdGen'.
--
newStateVarIO
    :: IO (StrictTVar IO (InboundGovernorState muxMode peerAddr versionNumber IO a b))
newStateVarIO = do
    g <- Rnd.getStdGen
    let (g', igsPrng) = Rnd.split g
    Rnd.setStdGen g'
    newStateVar igsPrng


-- | Useful for testing. Using 'Rnd.mkStdGen'.
--
newStateVarFromSeed
    :: MonadSTM m
    => Int
    -> m (StrictTVar m (InboundGovernorState muxMode peerAddr versionNumber m a b))
newStateVarFromSeed = newStateVar . Rnd.mkStdGen


-- | Per connection state tracked by /inbound protocol governor/.
--
data ConnectionState muxMode peerAddr versionNumber m a b = ConnectionState {
      -- | Mux interface.
      csMux                 :: !(Mux.Mux muxMode m),

      -- | All supported mini-protocols.
      csMiniProtocolsMap    :: !(Map MiniProtocolNum (WithSomeProtocolTemperature (MiniProtocol muxMode ByteString m a b))),

      -- | Mini protocol stm actions to monitor.
      csMiniProtocolActions :: !(Map MiniProtocolNum ((STM m (WithSomeProtocolTemperature (Either SomeException b))))),

      -- | When we enter termination we hold an stm action which blocks until
      -- all the protocol terminate or a timeout is reached or the connection is
      -- promoted to 'DuplexState'.
      csProtocolState       :: !(ProtocolState muxMode peerAddr versionNumber m a b),

      csIsEstablishedMiniProtocol
                            :: MiniProtocolNum -> Bool
    }


-- | 'ProtocolState' can be either in running mode or terminating state.  It
-- should not be confused with a mini-protocol state (protocol is a suite of
-- mini-protocols, e.g. node-to-node protocol or node-to-client protocol).
--
-- In the 'IsTerminating' sate it holds a last to finish synchronisation between
-- terminating protocols and a timeout 'TVar', which will be reused to construct
-- 'csProtocolState' whenever a mini-protocol terminates.  This alows us to set
-- overall timeout on termination of all mini-protocols.
--
data ProtocolState muxMode peerAddr versionNumber m a b
  -- | Protocol is running.
  = IsRunning

  -- | An established protcol terminated, we will wait for all mini-protocols to
  -- shutdown if the connection is in @'InboundState' dataFlow@.
  | IsTerminating
    !(LazySTM.TVar m Bool)
    -- ^ timeout tvar,
    !(STM m (MonitorEvent muxMode peerAddr versionNumber m a b))
    -- ^ An STM transactions which returns if the connection changes its state
    -- to @'DuplexState'@.  It returns the original 'MonitorEvent' which put the
    -- connection in this 'IsTerminating' state.


--
-- Utility functions
--

-- |  A utility function used to update 'igsConnections' inside
-- 'InboundGovernorState'.
--
igsUpdateConnections
  :: (Map (ConnectionId peerAddr)
          (ConnectionState muxMode peerAddr versionNumber m a b)
      -> Map (ConnectionId peerAddr)
             (ConnectionState muxMode peerAddr versionNumber m a b))
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
igsUpdateConnections update state@InboundGovernorState { igsConnections } =
    state { igsConnections = update igsConnections }


-- | When we re-start a mini-protocol, we register its stm action in
-- 'csMiniProtocolActions' map.  We also set protocol state to 'IsRunning'.
--
registerMiniProtocol
  :: Ord peerAddr
  => ConnectionId peerAddr
  -> MiniProtocolNum
  -> STM m (WithSomeProtocolTemperature (Either SomeException b))
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
registerMiniProtocol connId miniProtocolNum stm =
    igsUpdateConnections $
      Map.update
        (\st@ConnectionState { csMiniProtocolActions } ->
          Just st { csMiniProtocolActions =
                      Map.insert
                        miniProtocolNum
                        stm
                        csMiniProtocolActions
                  , csProtocolState = IsRunning
                  }
        )
        connId

-- | Unregister a terminated mini-protocol for some peer.
--
unregisterMiniProtocol
  :: Ord peerAddr
  => ConnectionId peerAddr
  -> MiniProtocolNum
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
  -> InboundGovernorState muxMode peerAddr versionNumber m a b
unregisterMiniProtocol connId miniProtocolNum =
    igsUpdateConnections $
      Map.update
        (\cs -> Just cs { csMiniProtocolActions =
                            Map.delete miniProtocolNum (csMiniProtocolActions cs)
                        })
        connId


-- | Register a new instance of a running mini-protocol for some peer.
--
insertConnection
    :: Ord peerAddr
    => ConnectionId peerAddr
    -> Mux.Mux muxMode m
    -> Map MiniProtocolNum (WithSomeProtocolTemperature (MiniProtocol muxMode ByteString m a b))
    -> Map MiniProtocolNum (STM m (WithSomeProtocolTemperature (Either SomeException b)))
    -> (MiniProtocolNum -> Bool)
    -> InboundGovernorState muxMode peerAddr versionNumber m a b
    -> InboundGovernorState muxMode peerAddr versionNumber m a b
insertConnection connId csMux csMiniProtocolsMap csMiniProtocolActions isEstablishedMiniProtocol =
    igsUpdateConnections $
      Map.insert connId
        ConnectionState {
            csMux,
            csMiniProtocolsMap,
            csMiniProtocolActions,
            csProtocolState = IsRunning,
            csIsEstablishedMiniProtocol = isEstablishedMiniProtocol
        }


-- | Lookup a mini-protocol.  Useful to run a mini-protocol using mux for
-- a given connection.
--
lookupMiniProtocol
    :: Ord peerAddr
    => ConnectionId peerAddr
    -> MiniProtocolNum
    -> InboundGovernorState muxMode peerAddr versionNumber m a b
    -> Maybe ( Mux.Mux muxMode m
             , WithSomeProtocolTemperature (MiniProtocol muxMode ByteString m a b)
             , ProtocolState muxMode peerAddr versionNumber m a b
             )
lookupMiniProtocol connId miniProtocolNum InboundGovernorState { igsConnections } =
    case Map.lookup connId igsConnections of
      Nothing -> Nothing
      Just ConnectionState { csMux,
                             csMiniProtocolsMap,
                             csProtocolState } ->
        Just ( csMux
             -- 'csMiniProtocolsMap' is static, it never changes during during
             -- connection lifetime.
             , csMiniProtocolsMap Map.! miniProtocolNum 
             , csProtocolState
             )


-- | Server arguments.
--
data ServerArguments (muxMode :: MuxMode) socket peerAddr versionNumber bytes m a b = ServerArguments {
      serverSockets           :: NonEmpty socket,
      serverSnocket           :: Snocket m socket peerAddr,
      serverTracer            :: Tracer m (ServerTrace peerAddr),
      serverConnectionLimits  :: AcceptedConnectionsLimit,
      serverConnectionManager :: MuxConnectionManager muxMode socket peerAddr
                                                      versionNumber bytes m a b,

      -- | Server control var is passed as an argument; this allows to use the
      -- server to run and manage responders which needs to be started on
      -- inbound connections.
      --
      serverControlChannel    :: ServerControlChannel m muxMode peerAddr
                                                      versionNumber a b,
      serverStateVar          :: StrictTVar m
                                  (InboundGovernorState muxMode peerAddr versionNumber m a b)
    }


-- | Events to which the /inbound protocol governor/ is listening too.  They
-- split into two groups: ones that are generataed by the /inbound protocol
-- goveror/ itself, usually as a result of mux state change (i.e. mini-protocol
-- termination) and ones that can also come from outside as a result of
-- connection state change in the /connection manager/.
--
data MonitorEvent (muxMode :: MuxMode) peerAddr versionNumber m a b =
    -- | A request to start mini-protocol bundle, either from the server or from
    -- connection manager.
    --
      MonitorMessage !(Server.ControlMessage (muxMode :: MuxMode) peerAddr versionNumber m a b)

    -- | A mini-protocol terminated with an exception.
    --
    | MiniProtocolErrored
        !(ConnectionId peerAddr)
        !MiniProtocolNum
        !SomeException

    -- | A mini-protocol terminated.  This initialises procedure
    -- which might either terminate all protocols or restart the protocol,
    -- depending on connection state.
    --
    | MiniProtocolReturned
        !(ConnectionId peerAddr)
        !MiniProtocolNum
        !(WithSomeProtocolTemperature b)

    -- | All mini-protocols terminated.
    --
    | ProtocolTerminated
        !(ConnectionId peerAddr)

    -- | Not all mini-protocols terminated within a timeout.
    --
    | ProtocolTerminationTimeout
        !(ConnectionId peerAddr)
        !MiniProtocolNum

    -- | Remote end send a message through any established mini-protocol.
    --
    | RemotePromotedToWarm
        !(ConnectionId peerAddr)


-- | Termination timeout.  This is a common timeout for termination all of the
-- mini-protocols.  It should be kept in sync with what we use on the outbound
-- side.
--
-- TODO: it should be made configurable, and defined where outbound timeouts are
-- defined.
--
terminationTimeout :: DiffTime
terminationTimeout = 120


firstMiniProtocolToFinish
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr versionNumber m a b
    -> STM m (MonitorEvent muxMode peerAddr versionNumber m a b)
firstMiniProtocolToFinish =
      Map.foldrWithKey
        (\connId ConnectionState { csMiniProtocolActions, csProtocolState } outerAcc ->
          Map.foldrWithKey
            (\miniProtocolNum stm innerAcc ->
                  (\resultWithTemp ->
                    case withoutSomeProtocolTemperature resultWithTemp of
                      Left err -> MiniProtocolErrored  connId miniProtocolNum err
                      Right a  -> MiniProtocolReturned connId miniProtocolNum (resultWithTemp $> a)
                  )
               <$> stm
               <|> (case csProtocolState of
                      IsTerminating _ awaitTermination -> awaitTermination
                      IsRunning -> retry)
               <|> innerAcc
            )
            outerAcc
            csMiniProtocolActions)
        retry
    . igsConnections


-- | Monitor all muxes, for the remote peer to promote us from cold to warm or
-- hot state.
-- 
firstEstablishedResponderToStart
    :: MonadSTM m
    => InboundGovernorState muxMode peerAddr versionNumber m a b
    -> STM m (MonitorEvent muxMode peerAddr versionNumber m a b)
firstEstablishedResponderToStart =
      Map.foldrWithKey
        (\connId ConnectionState { csMux, csIsEstablishedMiniProtocol } outerAcc ->
          Map.foldr
            (\miniProtocolStatusVar innerAcc ->
                do st <- readTVar miniProtocolStatusVar
                   check (st == Mux.StatusRunning)
                   return (RemotePromotedToWarm connId) 
                `orElse` innerAcc
            )
            outerAcc
          . Map.filterWithKey
            (\(miniProtocolNum, miniProtocolDir) _ ->
              case (csIsEstablishedMiniProtocol miniProtocolNum, miniProtocolDir) of
                (a, Mux.ResponderDir) -> a
                (_, Mux.InitiatorDir) -> False)
          $ Mux.miniProtocolStateMap csMux
        )
        retry
    . igsConnections
      


-- | Start and run the server, which consists of two parts:
--
-- * inbound governor thread (which corresponds to p2p-governor on oubound side
-- * accept loop(s), one per given ip.  We support up to one ipv4 address and
--   up to one ipv6 address, i.e. an ipv6 enabled node will run two accept
--   loops on different addresses with shared inbound governor thread.
--
run :: forall muxMode socket peerAddr versionNumber m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadTime  m
       , MonadTimer m
       , HasResponder muxMode ~ True
       , Typeable (HasInitiator muxMode)
       , Ord      peerAddr
       , Typeable peerAddr
       , Show     peerAddr
       )
    => ServerArguments muxMode socket peerAddr versionNumber ByteString m a b
    -> m Void
run ServerArguments {
      serverSockets,
      serverSnocket,
      serverTracer = tracer,
      serverConnectionLimits,
      serverConnectionManager,
      serverControlChannel,
      serverStateVar = stateVar

    } = do
      let sockets = NonEmpty.toList serverSockets
      localAddresses <- traverse (getLocalAddr serverSnocket) sockets
      traceWith tracer (Started localAddresses)
      runConcurrently
        $ foldr1 (<>)
        $ Concurrently inboundGovernorThread
        : (Concurrently . acceptLoop . accept serverSnocket) `map` sockets
      `finally`
        traceWith tracer Stopped
  where
    -- The inbound protocol governor.
    --
    -- Inbound governor thread could be written using a recursive style,
    -- passing pure state through recursive calls.  But we need to be able to
    -- observe the state from outside to support 'PrunePolicy'-ies which can
    -- access the state.
    inboundGovernorThread
      :: m Void
    inboundGovernorThread = forever $ do
      monitorMessage
        <- atomically $ do
              state <- readTVar stateVar
              (MonitorMessage <$> Server.readControlMessage serverControlChannel)
                `orElse`
                firstMiniProtocolToFinish state
                `orElse`
                firstEstablishedResponderToStart state

      case monitorMessage of
        MonitorMessage
          -- new connection has been announced by either accept loop or
          -- by connection manager (in which case the connection is in
          -- 'DuplexState').
          (Server.NewConnection
            connId
            (Handle mux
                    (Bundle
                      (WithHot hotPtls)
                      (WithWarm warmPtls)
                      (WithEstablished establishedPtls))
                    _)) -> do
              traceWith tracer
                (StartRespondersOnInboundConncetion
                  connId)
              let miniProtocolsMap :: Map MiniProtocolNum
                                          (WithSomeProtocolTemperature
                                            (MiniProtocol muxMode ByteString m a b))
                  miniProtocolsMap =
                    Map.fromList $
                      [ (miniProtocolNum p, WithSomeProtocolTemperature (WithEstablished p))
                      | p <- establishedPtls
                      ] ++
                      [ (miniProtocolNum p, WithSomeProtocolTemperature (WithWarm p))
                      | p <- warmPtls
                      ] ++
                      [ (miniProtocolNum p, WithSomeProtocolTemperature (WithHot p))
                      | p <- hotPtls
                      ]

                  establishedNums :: [MiniProtocolNum]
                  establishedNums = map miniProtocolNum establishedPtls

                  isEstablishedMiniProtocol :: MiniProtocolNum -> Bool
                  isEstablishedMiniProtocol = \pnum -> pnum `elem` establishedNums

              -- start responder protocols
              miniProtocolActions
                <- Map.traverseWithKey
                     (\pnum miniProtocol ->
                       runResponder
                         (WithResponder @muxMode)
                         mux
                         (if isEstablishedMiniProtocol pnum
                            then Mux.StartOnDemand
                            else Mux.StartEagerly)
                         miniProtocol)
                     miniProtocolsMap

              atomically $
                modifyTVar stateVar $
                  insertConnection
                    connId mux
                    miniProtocolsMap
                    miniProtocolActions
                    isEstablishedMiniProtocol

        MiniProtocolErrored connId miniProtocolNum err -> do
          case fromException err of
            -- Do not log 'MuxError's here; That's already logged by
            -- 'ConnectionManager' and we don't want to multiplex these errors
            -- for each mini-protocol.
            Just Mux.MuxError {} -> pure ()
            Nothing -> traceWith tracer (MiniProtocolError connId miniProtocolNum err)

          -- exceptions raised by mini-protocols are terminal to the bearer; we
          -- can stop tracking thet connection.
          atomically $
            modifyTVar stateVar $
              igsUpdateConnections (Map.delete connId)


        msg@(MiniProtocolReturned connId miniProtocolNum resultWithTemp) -> do
          case resultWithTemp of
            -- established protocol terminated, this triggers termination
            WithSomeProtocolTemperature (WithEstablished _) -> do
              state <- atomically (readTVar stateVar)
              -- timeout 'TVar'; only create a new one if there doesn't exists one
              -- for this connection.
              timeoutVar <-
                case csProtocolState <$> Map.lookup connId (igsConnections state) of
                  Nothing                      -> registerDelay terminationTimeout
                  Just IsRunning               -> registerDelay terminationTimeout
                  Just (IsTerminating tv _stm) -> return tv

              (inDuplexState, state') <-
                atomically $ do
                  state' <- readTVar stateVar
                  -- isInbDuplexState can error if the connection is in
                  -- 'UnnegotiatedState' or 'ReservedOutboundState', this is
                  -- impossible if a mini-protocol returned cleanly.
                  cmConnState <-
                    isInDuplexState serverConnectionManager (remoteAddress connId)

                  case cmConnState of
                    InDuplexState -> do
                      -- state is updated after restarting a mini-protocol, we
                      -- don't need to remove the mini-protocol which return as
                      -- shortly we will start it.
                      return ( True, state' )

                    AwaitForDuplexState promotedSTM -> do
                      let state'' =
                            igsUpdateConnections
                              (Map.update
                                (Just
                                  . demotingToColdRemote
                                      connId miniProtocolNum
                                      timeoutVar promotedSTM msg)
                                connId)
                              state
                      writeTVar stateVar state''
                      return ( False, state'' )


              if inDuplexState
                -- in 'DuplexState'
                then 
                  case lookupMiniProtocol connId miniProtocolNum state' of
                    Nothing ->
                      assert False $ return ()

                    Just (mux, miniProtocol, _) -> do
                      stm <- runResponder (WithResponder @muxMode) mux Mux.StartEagerly miniProtocol
                      traceWith
                        tracer
                        (MiniProtocolRestarted connId miniProtocolNum)
                      atomically $
                        modifyTVar stateVar $
                          registerMiniProtocol
                            connId
                            miniProtocolNum
                            stm

                -- not in 'DuplexState'
                else do
                  traceWith
                    tracer
                    (MiniProtocolTerminated connId miniProtocolNum)


            -- hot or warm protocol terminated, just restart it.  This is a
            -- resutl of `hot → warm` or `warm → hot` transition on the remote
            -- end or part of `any → cold` transition.
            _ -> do
              state <- atomically $ readTVar stateVar
              case lookupMiniProtocol connId miniProtocolNum state of
                Nothing ->
                  assert False $ return ()

                Just (mux, miniProtocol, protocolState) ->
                  case protocolState of
                    -- remote end is exuecuting `any → cold` transition.
                    IsTerminating {} -> do
                      traceWith
                        tracer
                        (MiniProtocolTerminated connId miniProtocolNum)
                      atomically $
                        modifyTVar stateVar $
                          unregisterMiniProtocol
                            connId
                            miniProtocolNum

                    -- remote end is executing `wamr → hot` or `hot → warm`
                    -- transition.
                    IsRunning -> do
                      stm <- runResponder (WithResponder @muxMode) mux Mux.StartEagerly miniProtocol
                      traceWith
                        tracer
                        (MiniProtocolRestarted connId miniProtocolNum)
                      -- overwrite mini-protocol action; the 'inboundGovernorThread'
                      -- will now seek when @stm@ returns.
                      atomically $
                        modifyTVar stateVar $
                          registerMiniProtocol
                            connId
                            miniProtocolNum
                            stm


        ProtocolTerminated connId -> do
          a <- unregisterInboundConnection serverConnectionManager (remoteAddress connId)
              `catch` \(_ :: ConnectionManagerError peerAddr) -> return True
          if a
            then
              atomically $
                modifyTVar stateVar $
                  igsUpdateConnections (Map.delete connId)
            else do
              -- all responder protocols terminated, but the connection manager
              -- refused to unregister the connection.  We are now respondible
              -- for starting all mini-protocols.
              state <- atomically $ readTVar stateVar
              case Map.lookup connId (igsConnections state) of
                Nothing -> return ()

                Just ConnectionState { csMux, csMiniProtocolsMap } -> do
                  miniProtocolActions
                    <- traverse (runResponder (WithResponder @muxMode) csMux Mux.StartEagerly)
                                csMiniProtocolsMap
                  atomically $
                    modifyTVar stateVar $
                      igsUpdateConnections
                        (Map.update
                          (\cs -> Just cs { csMiniProtocolActions = miniProtocolActions })
                          connId)


        ProtocolTerminationTimeout connId miniProtocolNum -> do
          a <- unregisterInboundConnection serverConnectionManager (remoteAddress connId)
              `catch` \(_ :: ConnectionManagerError peerAddr) -> return True
          if a
            then
              atomically $
                modifyTVar stateVar $
                  igsUpdateConnections (Map.delete connId)

            -- connection manager refused to unregister the connection; At
            -- this point we know that 'miniProtocolNum' terminated, so we
            -- need to restart it.
            else do
              state <- atomically $ readTVar stateVar
              let ConnectionState { csMux,
                                    csMiniProtocolsMap
                                    } = igsConnections state Map.! connId
              stm <- runResponder (WithResponder @muxMode) csMux Mux.StartEagerly (csMiniProtocolsMap Map.! miniProtocolNum)
              atomically $
                modifyTVar stateVar $
                  registerMiniProtocol
                    connId
                    miniProtocolNum
                    stm


        RemotePromotedToWarm connId -> do
          -- dependent types or ad-hock polymorphism,
          -- whatever name you prerfer ;)
          case eqT :: Maybe (HasInitiator muxMode :~: True) of
            Just Refl ->
              promotedToWarmRemote serverConnectionManager (remoteAddress connId)
            Nothing -> pure ()


    acceptLoop :: Accept m socket peerAddr
               -> m Void
    acceptLoop acceptOne = do
      runConnectionRateLimits
        (AcceptPolicyTrace `contramap` tracer)
        (numberOfConnections serverConnectionManager)
        serverConnectionLimits
      result <- runAccept acceptOne
      case result of
        (AcceptFailure err, acceptNext) -> do
          traceWith tracer (AcceptError err)
          acceptLoop acceptNext
        (Accepted socket peerAddr, acceptNext) -> do
          traceWith tracer (AcceptConnection peerAddr)
          -- using withAsync ensures that the thread that includes inbound
          -- connection (which is a blockng operation), is killed when the
          -- server is killed, possibley by an async exception.
          withAsync
            (do
              a <-
                includeInboundConnection
                  serverConnectionManager
                  socket peerAddr
              case a of
                Connected connId handle ->
                  Server.writeControlMessage
                    serverControlChannel
                    (Server.NewConnection connId handle)
                Disconnected {} ->
                  pure ()
            )
            $ \_ -> acceptLoop acceptNext


    -- Update connection state when discovered that the remote peer put as
    -- a cold peer, e.g.
    -- @
    --    DemotedToCold^{Duplex}_{Remote}
    -- @
    --
    demotingToColdRemote
        :: ConnectionId peerAddr
        -> MiniProtocolNum
        -> LazySTM.TVar m Bool
        -> STM m Bool
        -> MonitorEvent (muxMode :: MuxMode) peerAddr versionNumber m a b
        -> ConnectionState muxMode peerAddr versionNumber m a b
        -> ConnectionState muxMode peerAddr versionNumber m a b
    demotingToColdRemote
        connId miniProtocolNum timeoutVar promotedSTM msg
        connState@ConnectionState { csMiniProtocolActions } =
      -- remove the mini-protocol which terminated, and register last to finish
      -- synchronisation between all mini-protocol threads
      let miniProtocolActions =
            Map.delete miniProtocolNum
                       csMiniProtocolActions

          -- this synchronisation must terminate if first of the three
          -- conditions is met:
          --
          -- * all mini-protocols terminated (hence the name),
          -- * timeout fired,
          -- * connection was promoted to 'DuplexState'.
          --
          -- In the last case, we will come back to his case and we will
          -- consider if we actually can restart the protocol.
          --
          -- Life time of 'lastToFinish' starts here.  It will be garbage
          -- collected once: 
          --
          -- * all mini-protocols terminated
          -- * timeout fired
          -- * connection was promoted to 'DuplexState'.
          --
          -- In the first two cases it will be gc-ed together with
          -- 'ConnectionState', in the last case by 'registerMiniProtocol'
          --
          csProtocolState :: ProtocolState muxMode peerAddr
                                           versionNumber m a b
          csProtocolState = IsTerminating
              timeoutVar
              ( -- wait for all mini-protocols to termnate
                    (sequence_ miniProtocolActions
                       $> ProtocolTerminated connId)
                -- or else wait for timeout
                <|> ((LazySTM.readTVar timeoutVar >>= check)
                       $> ProtocolTerminationTimeout connId miniProtocolNum)
                -- or else wait for promotion of the
                -- connection to 'DuplexState'.
                <|> (bool (ProtocolTerminated connId) msg
                      <$> promotedSTM)
              )
      in connState { csMiniProtocolActions = miniProtocolActions,
                     csProtocolState }


-- | 'HasResponder muxMode ~ True' constraint.
--
data WithResponder (muxMode :: MuxMode) where
    WithResponder :: HasResponder muxMode ~ True => WithResponder muxMode


-- | Run a responder mini-protocol.
--
runResponder :: ( MonadAsync m
                , MonadCatch m
                )
             => WithResponder muxMode
             -- ^ we pass the @'HasResponder' ~ muxMode@ explicitly, otherwise
             -- the constraint would be redundant.
             -> Mux.Mux muxMode m
             -> Mux.StartOnDemandOrEagerly
             -> WithSomeProtocolTemperature (MiniProtocol muxMode ByteString m a b)
             -> m (STM m (WithSomeProtocolTemperature (Either SomeException b)))
runResponder WithResponder
             mux
             startOnDemandOrEagerly
             miniProtocol =
  case withoutSomeProtocolTemperature miniProtocol of
    MiniProtocol {
        miniProtocolNum,
        miniProtocolRun
      } ->
        case miniProtocolRun of
          ResponderProtocolOnly responder ->
            -- thread through the same 'WithProtocolTemperature'
            fmap (\res -> miniProtocol $> res) <$>
              Mux.runMiniProtocol
                mux miniProtocolNum
                Mux.ResponderDirectionOnly
                startOnDemandOrEagerly
                -- TODO: eliminate 'fromChannel'
                (runMuxPeer responder . fromChannel)

          InitiatorAndResponderProtocol _ responder ->
              -- thread through the same 'WithProtocolTemperature'
            fmap (\res -> miniProtocol $> res) <$>
              Mux.runMiniProtocol
                mux miniProtocolNum
                Mux.ResponderDirection
                startOnDemandOrEagerly
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
randomPrunePolicy :: ( MonadSTM m
                     , Ord peerAddr
                     )
                  => StrictTVar m (InboundGovernorState muxMode peerAddr versionNumber m a b)
                  -> PrunePolicy peerAddr (STM m)
randomPrunePolicy stateVar mp n = do
    state <- readTVar stateVar
    let (igsPrng', prng) = Rnd.split (igsPrng state)
    writeTVar stateVar state { igsPrng = igsPrng' }

    return
      $ Set.fromList
      . take n
      . map (fst . fst)
      -- 'True' values (upstream / outbound connections) will sort last.
      . sortOn (\((_, connType), score) -> (isUpstream connType, score))
      . zip (Map.assocs mp)
      $ (Rnd.randoms prng :: [Int])
  where
    isUpstream :: ConnectionType -> Bool
    isUpstream = \connType ->
      case connType of
        UnnegotiatedConn Outbound -> True
        UnnegotiatedConn Inbound  -> False
        NegotiatedConn Outbound _ -> True
        NegotiatedConn Inbound  _ -> False
        DuplexConn                -> True

--
-- Trace
--

data ServerTrace peerAddr
    = AcceptConnection                    !peerAddr
    | StartRespondersOnInboundConncetion  !(ConnectionId peerAddr)
    | StartRespondersOnOutboundConnection !(ConnectionId peerAddr)
    | AcceptError                         !SomeException
    | AcceptPolicyTrace                   !AcceptConnectionsPolicyTrace
    | Started                             ![peerAddr]
    | Stopped
    | MiniProtocolRestarted  !(ConnectionId peerAddr) !MiniProtocolNum
    | MiniProtocolError      !(ConnectionId peerAddr) !MiniProtocolNum !SomeException
    | MiniProtocolTerminated !(ConnectionId peerAddr) !MiniProtocolNum
  deriving Show
