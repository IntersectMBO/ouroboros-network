{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
-- Undecidable instances are need for 'Show' instance of 'ConnectionState'.
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The implementation of connection manager.
--
module Ouroboros.Network.ConnectionManager.Core
  ( ConnectionManagerArguments (..)
  , withConnectionManager
  , defaultTimeWaitTimeout
  , defaultProtocolIdleTimeout
  , defaultResetTimeout
  , ConnectionState (..)
  , abstractState
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (forM_, guard, when, (>=>))
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (throwTo)
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix
import Control.Tracer (Tracer, contramap, traceWith)
import Data.Foldable (foldMap', traverse_)
import Data.Function (on)
import Data.Functor (void, ($>))
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Stack (CallStack, HasCallStack, callStack)
import System.Random (StdGen, split)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

import Data.Monoid.Synchronisation
import Data.Wedge
import Data.Word (Word32)

import Network.Mux.Trace (MuxTrace, WithMuxBearer (..))
import Network.Mux.Types (MuxMode)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.InformationChannel
           (InformationChannel)
import Ouroboros.Network.ConnectionManager.InformationChannel qualified as InfoChannel
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.ConnectionManager.Types qualified as CM
import Ouroboros.Network.InboundGovernor.Event (NewConnectionInfo (..))
import Ouroboros.Network.MuxMode
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Snocket


-- | Arguments for a 'ConnectionManager' which are independent of 'MuxMode'.
--
data ConnectionManagerArguments handlerTrace socket peerAddr handle handleError versionNumber versionData m =
    ConnectionManagerArguments {
        -- | Connection manager tracer.
        --
        cmTracer              :: Tracer m (ConnectionManagerTrace peerAddr handlerTrace),

        -- | Trace state transitions.
        --
        cmTrTracer            :: Tracer m (TransitionTrace peerAddr
                                            (ConnectionState peerAddr handle handleError versionNumber m)),

        -- | Mux trace.
        --
        cmMuxTracer           :: Tracer m (WithMuxBearer (ConnectionId peerAddr) MuxTrace),

        -- | @IPv4@ address of the connection manager.  If given, outbound
        -- connections to an @IPv4@ address will bound to it.  To use
        -- bidirectional @TCP@ connections, it must be the same as the server
        -- listening @IPv4@ address.
        --
        cmIPv4Address         :: Maybe peerAddr,

        -- | @IPv6@ address of the connection manager.  If given, outbound
        -- connections to an @IPv6@ address will bound to it.  To use
        -- bidirectional @TCP@ connections, it must be the same as the server
        -- listening @IPv6@ address.
        --
        cmIPv6Address         :: Maybe peerAddr,

        cmAddressType         :: peerAddr -> Maybe AddressType,

        -- | Snocket for the 'socket' type.
        --
        cmSnocket             :: Snocket m socket peerAddr,

        -- | Make MuxBearer.
        --
        cmMakeBearer          :: MakeBearer m socket,

        -- | Socket configuration.
        --
        cmConfigureSocket     :: socket -> Maybe peerAddr -> m (),

        -- | @TCP@ will held connections in @TIME_WAIT@ state for up to two MSL
        -- (maximum segment time).  On Linux this is set to '60' seconds on
        -- other system this might be up to four minutes.
        --
        -- This is configurable, so we can set different value in tests.
        --
        -- When this timeout expires a connection will transition from
        -- 'TerminatingState' to 'TerminatedState'.
        --
        cmTimeWaitTimeout     :: DiffTime,

        -- | Inactivity timeout before the connection will be reset.  It is the
        -- timeout attached to the 'OutboundIdleState'.
        --
        cmOutboundIdleTimeout :: DiffTime,

        -- | Given a version number and respective version data, get the
        -- 'DataFlow'.
        --
        connectionDataFlow    :: versionNumber -> versionData -> DataFlow,

        -- | Prune policy
        --
        cmPrunePolicy         :: PrunePolicy peerAddr,

        -- | StdGen used by the `PrunePolicy`
        --
        cmStdGen              :: StdGen,

        cmConnectionsLimits   :: AcceptedConnectionsLimit
      }


-- | 'MutableConnState', which supplies a unique identifier.
--
-- TODO: We can get away without id, by tracking connections in
-- `TerminatingState` using a separate priority search queue.
--
data MutableConnState peerAddr handle handleError version m = MutableConnState {
    -- | A unique identifier
    --
    connStateId  :: !Int

  , -- | Mutable state
    --
    connVar      :: !(StrictTVar m (ConnectionState peerAddr handle handleError
                                                    version m))
  }


instance Eq (MutableConnState peerAddr handle handleError version m) where
    (==) =  (==) `on` connStateId


-- | A supply of fresh id's.
--
-- We use a fresh ids for 'MutableConnState'.
--
newtype FreshIdSupply m = FreshIdSupply { getFreshId :: STM m Int }


-- | Create a 'FreshIdSupply' inside an 'STM' monad.
--
newFreshIdSupply :: forall m. MonadSTM m
                 => Proxy m -> STM m (FreshIdSupply m)
newFreshIdSupply _ = do
    (v :: StrictTVar m Int) <- newTVar 0
    let getFreshId :: STM m Int
        getFreshId = do
          c <- readTVar v
          writeTVar v (succ c)
          return c
    return $ FreshIdSupply { getFreshId }


newMutableConnState :: forall peerAddr handle handleError version m.
                      ( MonadTraceSTM m
                      , Typeable peerAddr
                      )
                    => peerAddr
                    -> FreshIdSupply m
                    -> ConnectionState peerAddr handle handleError
                                       version m
                    -> STM m (MutableConnState peerAddr handle handleError
                                               version m)
newMutableConnState peerAddr freshIdSupply connState = do
      connStateId <- getFreshId freshIdSupply
      connVar <- newTVar connState
      -- This tracing is a no op in IO.
      --
      -- We need this for IOSimPOR testing of connection manager state
      -- transition tests. It can happen that the transitions happen
      -- correctly but IOSimPOR reorders the threads that log the transitions.
      -- This is a false positive and we don't want that to happen.
      --
      -- The simplest way to do so is to leverage the `traceTVar` IOSim
      -- capabilities. These trace messages won't be reordered by IOSimPOR
      -- since these happen atomically in STM.
      --
      -- Another thing to note is that this trace differs from the IO one in
      -- the fact that all connections terminate with a trace to
      -- 'UnknownConnectionSt', since we can't do that here we limit ourselves
      -- to 'TerminatedSt'.
      --
      traceTVar
        (Proxy @m) connVar
        (\mbPrev curr ->
          let currAbs = abstractState (Known curr)
           in case mbPrev of
                Just prev |
                    let prevAbs = abstractState (Known prev)
                  , prevAbs /= currAbs -> pure
                                       $ TraceDynamic
                                       $ TransitionTrace peerAddr
                                       $ mkTransition prevAbs
                                                      currAbs
                Nothing                -> pure
                                       $ TraceDynamic
                                       $ TransitionTrace peerAddr
                                       $ mkTransition TerminatedSt
                                                      currAbs
                _                      -> pure DontTrace
        )
      return $ MutableConnState { connStateId, connVar }


-- | 'ConnectionManager' state: for each peer we keep a 'ConnectionState' in
-- a mutable variable, which reduces congestion on the 'TMVar' which keeps
-- 'ConnectionManagerState'.
--
-- It is important we can lookup by remote @peerAddr@; this way we can find if
-- the connection manager is already managing a connection towards that
-- @peerAddr@ and reuse the 'ConnectionState'.
--
type ConnectionManagerState peerAddr handle handleError version m
  = Map peerAddr (MutableConnState peerAddr handle handleError version m)

connectionManagerStateToCounters
  :: Map peerAddr (ConnectionState peerAddr handle handleError version m)
  -> ConnectionManagerCounters
connectionManagerStateToCounters =
    foldMap' connectionStateToCounters

-- | State of a connection.
--
data ConnectionState peerAddr handle handleError version m =
    -- | Each outbound connections starts in this state.
    ReservedOutboundState

    -- | Each inbound connection starts in this state, outbound connection
    -- reach this state once `connect` call returns.
    --
    -- note: the async handle is lazy, because it's passed with 'mfix'.
  | UnnegotiatedState   !Provenance
                        !(ConnectionId peerAddr)
                         (Async m ())

    -- | @OutboundState Unidirectional@ state.
  | OutboundUniState    !(ConnectionId peerAddr) !(Async m ()) !handle

    -- | Either @OutboundState Duplex@ or @OutboundState^\tau Duplex@.
  | OutboundDupState    !(ConnectionId peerAddr) !(Async m ()) !handle !TimeoutExpired

    -- | Before connection is reset it is put in 'OutboundIdleState' for the
    -- duration of 'cmOutboundIdleTimeout'.
    --
  | OutboundIdleState   !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | InboundIdleState    !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | InboundState        !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | DuplexState         !(ConnectionId peerAddr) !(Async m ()) !handle
  | TerminatingState    !(ConnectionId peerAddr) !(Async m ()) !(Maybe handleError)
  | TerminatedState                              !(Maybe handleError)


-- | Return 'True' for states in which the connection was already closed.
--
connectionTerminated :: ConnectionState peerAddr handle handleError version m
                     -> Bool
connectionTerminated TerminatingState {} = True
connectionTerminated TerminatedState  {} = True
connectionTerminated _                   = False


-- | Perform counting from an 'AbstractState'
connectionStateToCounters
    :: ConnectionState peerAddr handle handleError version m
    -> ConnectionManagerCounters
connectionStateToCounters state =
    case state of
      ReservedOutboundState                 -> mempty

      UnnegotiatedState Inbound _ _         -> inboundConn

      UnnegotiatedState Outbound _ _        -> outboundConn

      OutboundUniState _ _ _                -> unidirectionalConn
                                            <> outboundConn

      OutboundDupState  _ _ _ _             -> duplexConn
                                            <> outboundConn

      OutboundIdleState _ _ _ Unidirectional -> unidirectionalConn
                                             <> outboundConn

      OutboundIdleState _ _ _ Duplex         -> duplexConn
                                             <> outboundConn

      InboundIdleState _ _ _ Unidirectional -> unidirectionalConn
                                            <> inboundConn

      InboundIdleState _ _ _ Duplex         -> duplexConn
                                            <> inboundConn

      InboundState _ _ _ Unidirectional     -> unidirectionalConn
                                            <> inboundConn

      InboundState _ _ _ Duplex             -> duplexConn
                                            <> inboundConn

      DuplexState _ _ _                     -> fullDuplexConn
                                            <> duplexConn
                                            <> inboundConn
                                            <> outboundConn

      TerminatingState _ _ _                -> mempty
      TerminatedState _                     -> mempty
  where
    fullDuplexConn     = ConnectionManagerCounters 1 0 0 0 0
    duplexConn         = ConnectionManagerCounters 0 1 0 0 0
    unidirectionalConn = ConnectionManagerCounters 0 0 1 0 0
    inboundConn        = ConnectionManagerCounters 0 0 0 1 0
    outboundConn       = ConnectionManagerCounters 0 0 0 0 1


instance ( Show peerAddr
         , Show handleError
         , MonadAsync m
         )
      => Show (ConnectionState peerAddr handle handleError version m) where
    show ReservedOutboundState = "ReservedOutboundState"
    show (UnnegotiatedState pr connId connThread) =
      concat ["UnnegotiatedState "
             , show pr
             , " "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (OutboundUniState connId connThread _handle) =
      concat [ "OutboundState Unidirectional "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (OutboundDupState connId connThread _handle expired) =
      concat [ "OutboundState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show expired
             ]
    show (OutboundIdleState connId connThread _handle df) =
      concat [ "OutboundIdleState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (InboundIdleState connId connThread _handle df) =
      concat [ "InboundIdleState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (InboundState  connId connThread _handle df) =
      concat [ "InboundState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             , " "
             , show df
             ]
    show (DuplexState   connId connThread _handle) =
      concat [ "DuplexState "
             , show connId
             , " "
             , show (asyncThreadId connThread)
             ]
    show (TerminatingState connId connThread handleError) =
      concat ([ "TerminatingState "
              , show connId
              , " "
              , show (asyncThreadId connThread)
              ]
              ++ maybeToList ((' ' :) . show <$> handleError))
    show (TerminatedState handleError) =
      concat (["TerminatedState"]
              ++ maybeToList ((' ' :) . show <$> handleError))


getConnThread :: ConnectionState peerAddr handle handleError version m
              -> Maybe (Async m ())
getConnThread ReservedOutboundState                                     = Nothing
getConnThread (UnnegotiatedState _pr   _connId connThread)              = Just connThread
getConnThread (OutboundUniState        _connId connThread _handle )     = Just connThread
getConnThread (OutboundDupState        _connId connThread _handle _te)  = Just connThread
getConnThread (OutboundIdleState       _connId connThread _handle _df)  = Just connThread
getConnThread (InboundIdleState        _connId connThread _handle _df)  = Just connThread
getConnThread (InboundState            _connId connThread _handle _df)  = Just connThread
getConnThread (DuplexState             _connId connThread _handle)      = Just connThread
getConnThread (TerminatingState        _connId connThread _handleError) = Just connThread
getConnThread TerminatedState {}                                        = Nothing

-- | Get 'DataFlow' for a connection.  It returns 'Nowhere' if that connection
-- is either not yet created or in terminating state, 'There' for  unnegotiated
-- connections and 'Here' if the data flow is known.
--
getConnType :: ConnectionState peerAddr handle handleError version m
            -> Maybe ConnectionType
getConnType ReservedOutboundState                                    = Nothing
getConnType (UnnegotiatedState pr  _connId _connThread)              = Just (UnnegotiatedConn pr)
getConnType (OutboundUniState      _connId _connThread _handle)      = Just (NegotiatedConn Outbound Unidirectional)
getConnType (OutboundDupState      _connId _connThread _handle _te)  = Just (NegotiatedConn Outbound Duplex)
getConnType (OutboundIdleState     _connId _connThread _handle df)   = Just (OutboundIdleConn df)
getConnType (InboundIdleState      _connId _connThread _handle df)   = Just (InboundIdleConn df)
getConnType (InboundState          _connId _connThread _handle df)   = Just (NegotiatedConn Inbound df)
getConnType (DuplexState           _connId _connThread _handle)      = Just DuplexConn
getConnType (TerminatingState      _connId _connThread _handleError) = Nothing
getConnType TerminatedState {}                                       = Nothing


-- | Return 'True' if a connection is inbound.  This must agree with
-- 'connectionStateToCounters'.  Both are used for pruning.
--
isInboundConn :: ConnectionState peerAddr handle handleError version m -> Bool
isInboundConn ReservedOutboundState                      = False
isInboundConn (UnnegotiatedState pr _connId _connThread) = pr == Inbound
isInboundConn OutboundUniState {}                        = False
isInboundConn OutboundDupState {}                        = False
isInboundConn OutboundIdleState {}                       = False
isInboundConn InboundIdleState {}                        = True
isInboundConn InboundState {}                            = True
isInboundConn DuplexState {}                             = True
isInboundConn TerminatingState {}                        = False
isInboundConn TerminatedState {}                         = False


abstractState :: MaybeUnknown (ConnectionState muxMode peerAddr m a b) -> AbstractState
abstractState = \case
    Unknown  -> UnknownConnectionSt
    Race s'  -> go s'
    Known s' -> go s'
  where
    go :: ConnectionState muxMode peerAddr m a b -> AbstractState
    go ReservedOutboundState {}       = ReservedOutboundSt
    go (UnnegotiatedState pr _ _)     = UnnegotiatedSt pr
    go (OutboundUniState    _ _ _)    = OutboundUniSt
    go (OutboundDupState    _ _ _ te) = OutboundDupSt te
    go (OutboundIdleState _ _ _ df)   = OutboundIdleSt df
    go (InboundIdleState _ _ _ df)    = InboundIdleSt df
    go (InboundState     _ _ _ df)    = InboundSt df
    go DuplexState {}                 = DuplexSt
    go TerminatingState {}            = TerminatingSt
    go TerminatedState {}             = TerminatedSt


-- | The default value for 'cmTimeWaitTimeout'.
--
defaultTimeWaitTimeout :: DiffTime
defaultTimeWaitTimeout = 60

-- | Inactivity timeout.  It configures how long to wait since the local side
-- demoted remote peer to /cold/, before closing the connection.
--
defaultProtocolIdleTimeout :: DiffTime
defaultProtocolIdleTimeout = 5

defaultResetTimeout :: DiffTime
defaultResetTimeout = 5


newtype PruneAction m = PruneAction { runPruneAction :: m () }

-- | Instruction used internally in @unregisterOutboundConnectionImpl@, e.g. in
-- the implementation of one of the two  @DemotedToCold^{dataFlow}_{Local}@
-- transitions.
--
data DemoteToColdLocal peerAddr handlerTrace handle handleError version m
    -- | Any @DemotedToCold@ transition which terminates the connection:
    -- @
    --   DemotedToCold^{Duplex}_{Local} : * -> TerminatingState
    -- @
    -- from the spec.
    --
    = DemotedToColdLocal      (ConnectionId peerAddr)
                              (Async m ())
                              (StrictTVar m (ConnectionState
                                              peerAddr handle
                                              handleError version m))
                             !(Transition (ConnectionState
                                            peerAddr handle
                                            handleError version m))

    -- | Any @DemoteToCold@ transition which does not terminate the connection, i.e.
    -- @
    --   DemotedToCold^{Duplex}_{Local} : OutboundState^\tau Duplex
    --                                  → InboundIdleState^\tau
    -- @
    -- or the case where the connection is already in 'TerminatingState' or
    -- 'TerminatedState'.
    --
    | DemoteToColdLocalNoop !(Maybe (Transition (ConnectionState
                                                  peerAddr handle
                                                  handleError version m)))
                            !AbstractState

    -- | Duplex connection was demoted, prune connections.
    --
    | PruneConnections       (PruneAction m)
                             -- ^ prune action

                            !(Either
                               (ConnectionState
                                 peerAddr handle
                                 handleError version m)
                               (Transition (ConnectionState
                                             peerAddr handle
                                             handleError version m))
                             )

                             -- ^ Left case is for when the connection which
                             -- triggered pruning is pruned in this case we do
                             -- not want to trace a new transition.
                             --
                             -- Right case is for when the connection which
                             -- triggered pruning isn't pruned. In this case
                             -- we do want to trace a new transition.


    -- | Demote error.
    | DemoteToColdLocalError  (ConnectionManagerTrace peerAddr handlerTrace)
                             !AbstractState


-- | Entry point for using the connection manager.  This is a classic @with@ style
-- combinator, which cleans resources on exit of the callback (whether cleanly
-- or through an exception).
--
-- Including a connection (either inbound or outbound) is an idempotent
-- operation on connection manager state.  The connection manager will always
-- return the handle that was first to be included in its state.
--
-- Once an inbound connection is passed to the 'ConnectionManager', the manager
-- is responsible for the resource.
--
withConnectionManager
    :: forall (muxMode :: MuxMode) peerAddr socket handlerTrace handle handleError version versionData m a.
       ( Alternative (STM m)
       , MonadLabelledSTM   m
       , MonadTraceSTM      m
       -- 'MonadFork' is only to get access to 'throwTo'
       , MonadFork          m
       , MonadAsync         m
       , MonadDelay         m
       , MonadEvaluate      m
       , MonadFix           m
       , MonadMask          m
       , MonadThrow    (STM m)
       , MonadTimer         m

       , Ord      peerAddr
       , Show     peerAddr
       , Typeable peerAddr
       )
    => ConnectionManagerArguments handlerTrace socket peerAddr handle handleError version versionData m
    -> ConnectionHandler  muxMode handlerTrace socket peerAddr handle handleError (version, versionData) m
    -- ^ Callback which runs in a thread dedicated for a given connection.
    -> (handleError -> HandleErrorType)
    -- ^ classify 'handleError's
    -> InResponderMode muxMode (InformationChannel (NewConnectionInfo peerAddr handle) m)
    -- ^ On outbound duplex connections we need to notify the server about
    -- a new connection.
    -> (ConnectionManager muxMode socket peerAddr handle handleError m -> m a)
    -- ^ Continuation which receives the 'ConnectionManager'.  It must not leak
    -- outside of scope of this callback.  Once it returns all resources
    -- will be closed.
    -> m a
withConnectionManager args@ConnectionManagerArguments {
                          cmTracer    = tracer,
                          cmTrTracer  = trTracer,
                          cmMuxTracer = muxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType,
                          cmSnocket,
                          cmMakeBearer,
                          cmConfigureSocket,
                          cmTimeWaitTimeout,
                          cmOutboundIdleTimeout,
                          connectionDataFlow,
                          cmPrunePolicy,
                          cmConnectionsLimits
                        }
                      ConnectionHandler {
                          connectionHandler
                        }
                      classifyHandleError
                      inboundGovernorInfoChannel
                      k = do
    ((freshIdSupply, stateVar, stdGenVar)
       ::  ( FreshIdSupply m
           , StrictTMVar m (ConnectionManagerState peerAddr handle handleError
                                                   version m)
           , StrictTVar m StdGen
           ))
      <- atomically $  do
          v  <- newTMVar Map.empty
          labelTMVar v "cm-state"
          traceTMVar (Proxy :: Proxy m) v
                   $ \old new ->
                     case (old, new) of
                       (Nothing, _)             -> pure DontTrace
                       -- taken
                       (Just (Just _), Nothing) -> pure (TraceString "cm-state: taken")
                       -- released
                       (Just Nothing,  Just _)  -> pure (TraceString "cm-state: released")
                       (_, _)                   -> pure DontTrace

          freshIdSupply <- newFreshIdSupply (Proxy :: Proxy m)
          stdGenVar <- newTVar (cmStdGen args)
          return (freshIdSupply, v, stdGenVar)

    let readState
          :: STM m (Map peerAddr AbstractState)
        readState = do
          state <- readTMVar stateVar
          traverse ( fmap (abstractState . Known)
                   . readTVar
                   . connVar
                   )
                   state

        waitForOutboundDemotion
          :: peerAddr
          -> STM m ()
        waitForOutboundDemotion addr = do
          state <- readState
          case Map.lookup addr state of
            Nothing                        -> return ()
            Just UnknownConnectionSt       -> return ()
            Just InboundIdleSt {}          -> return ()
            Just InboundSt {}              -> return ()
            Just WaitRemoteIdleSt          -> return ()
            Just TerminatedSt              -> return ()
            Just (UnnegotiatedSt Inbound)  -> return ()
            Just (UnnegotiatedSt Outbound) -> retry
            Just ReservedOutboundSt        -> retry
            Just OutboundUniSt             -> retry
            Just OutboundIdleSt {}         -> retry
            Just OutboundDupSt {}          -> retry
            Just DuplexSt                  -> retry
            Just TerminatingSt             -> retry

        connectionManager :: ConnectionManager muxMode socket peerAddr
                                               handle handleError m
        connectionManager =
          case connectionHandler of
            WithInitiatorMode outboundHandler ->
              ConnectionManager {
                getConnectionManager =
                  WithInitiatorMode
                    OutboundConnectionManager {
                        ocmRequestConnection =
                          requestOutboundConnectionImpl freshIdSupply stateVar
                                                        outboundHandler,
                        ocmUnregisterConnection =
                          unregisterOutboundConnectionImpl stateVar stdGenVar
                      },
                readState,
                waitForOutboundDemotion
              }

            WithResponderMode inboundHandler ->
              ConnectionManager {
                getConnectionManager =
                  WithResponderMode
                    InboundConnectionManager {
                        icmIncludeConnection =
                          includeInboundConnectionImpl freshIdSupply stateVar
                                                       inboundHandler,
                        icmUnregisterConnection =
                          unregisterInboundConnectionImpl stateVar,
                        icmPromotedToWarmRemote =
                          promotedToWarmRemoteImpl stateVar stdGenVar,
                        icmDemotedToColdRemote =
                          demotedToColdRemoteImpl stateVar,
                        icmNumberOfConnections =
                          readTMVar stateVar >>= countIncomingConnections
                      },
                readState,
                waitForOutboundDemotion
              }

            WithInitiatorResponderMode outboundHandler inboundHandler ->
              ConnectionManager {
                getConnectionManager =
                  WithInitiatorResponderMode
                    OutboundConnectionManager {
                        ocmRequestConnection =
                          requestOutboundConnectionImpl freshIdSupply stateVar
                                                        outboundHandler,
                        ocmUnregisterConnection =
                          unregisterOutboundConnectionImpl stateVar stdGenVar
                      }
                    InboundConnectionManager {
                        icmIncludeConnection =
                          includeInboundConnectionImpl freshIdSupply stateVar
                                                       inboundHandler,
                        icmUnregisterConnection =
                          unregisterInboundConnectionImpl stateVar,
                        icmPromotedToWarmRemote =
                          promotedToWarmRemoteImpl stateVar stdGenVar,
                        icmDemotedToColdRemote =
                          demotedToColdRemoteImpl stateVar,
                        icmNumberOfConnections =
                          readTMVar stateVar >>= countIncomingConnections
                      },
                readState,
                waitForOutboundDemotion
              }

    k connectionManager
      -- Since this exception handler is blocking it might receive exceptions
      -- during its execution, which we want to avoid, so we wrap it around
      -- uninterruptibleMask_.
      `finally` uninterruptibleMask_ (do
        traceWith tracer TrShutdown

        state <- atomically $ readTMVar stateVar
        -- Spawning one thread for each connection cleanup avoids spending time
        -- waiting for locks and cleanup logic that could delay closing the
        -- connections and making us not respecting certain timeouts.
        asyncs <- Map.elems
          <$> Map.traverseMaybeWithKey
          (\peerAddr MutableConnState { connVar } -> do
            -- cleanup handler for that thread will close socket associated
            -- with the thread.  We put each connection in 'TerminatedState' to
            -- try that none of the connection threads will enter
            -- 'TerminatingState' (and thus delay shutdown for 'tcp_WAIT_TIME'
            -- seconds) when receiving the 'AsyncCancelled' exception. However,
            -- we can have a race between the finally handler and the `cleanup`
            -- callback. If the finally block loses the race, the received
            -- 'AsyncCancelled' should interrupt the 'threadDelay'.
            --
            (connState, trT, trU , shouldTraceTerminated, shouldTraceUnknown)
              <- atomically $ do
                  connState <- readTVar connVar
                  let connState'            = TerminatedState Nothing
                      trT                   =
                        TransitionTrace peerAddr (mkTransition connState connState')
                      absConnState          = abstractState (Known connState)
                      shouldTraceTerminated = absConnState /= TerminatedSt
                      shouldTraceUnknown    = absConnState == ReservedOutboundSt
                      trU = TransitionTrace
                              peerAddr
                              (Transition { fromState = Known connState'
                                          , toState   = Unknown
                                          })

                  writeTVar connVar connState'
                  return (connState, trT, trU
                         , shouldTraceTerminated, shouldTraceUnknown)

            when shouldTraceTerminated $ do
              traceWith trTracer trT

              -- If an Async exception is received after a connection gets set
              -- to ReservedOutboundSt, BUT after a connect call is made and,
              -- therefore still does not have a connection handler thread, we
              -- should trace the unknown transition as well.
              when shouldTraceUnknown $
                traceWith trTracer trU

            -- using 'throwTo' here, since we want to block only until connection
            -- handler thread receives an exception so as to not take up extra
            -- time and making us go above timeout schedules.
            traverse
              (\thread -> do
                throwTo (asyncThreadId thread) AsyncCancelled
                pure thread
              )
              (getConnThread connState)
          ) state

        atomically $ runLastToFinishM
                   $ foldMap (LastToFinishM . void <$> waitCatchSTM) asyncs
      )
  where
    traceCounters :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m) -> m ()
    traceCounters stateVar = do
      mState <- atomically $ readTMVar stateVar >>= traverse (readTVar . connVar)
      traceWith tracer (TrConnectionManagerCounters (connectionManagerStateToCounters mState))

    countIncomingConnections
        :: ConnectionManagerState peerAddr handle handleError version m
        -> STM m Int
    countIncomingConnections st =
          inboundConns
        . connectionManagerStateToCounters
      <$> traverse (readTVar . connVar) st


    -- Fork connection thread.
    --
    -- TODO: We could probably elegantly eliminate 'PromiseWriter', now that we use
    -- MonadFix.
    forkConnectionHandler
      :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
      -> MutableConnState peerAddr handle handleError version m
      -> socket
      -> ConnectionId peerAddr
      -> PromiseWriter m (Either handleError (HandshakeConnectionResult handle (version, versionData)))
      -> ConnectionHandlerFn handlerTrace socket peerAddr handle handleError (version, versionData) m
      -> m (Async m ())
    forkConnectionHandler stateVar
                          mutableConnState@MutableConnState { connVar }
                          socket
                          connId@ConnectionId { remoteAddress = peerAddr }
                          writer
                          handler =
        mask $ \unmask -> async $ do
          runWithUnmask
            (handler socket writer
                     (TrConnectionHandler connId `contramap` tracer)
                     connId
                     (\bearerTimeout ->
                       getBearer cmMakeBearer
                         bearerTimeout
                         (WithMuxBearer connId `contramap` muxTracer)))
            unmask
          `finally` cleanup
      where
        cleanup :: m ()
        cleanup =
          -- We must ensure that we update 'connVar',
          -- `requestOutboundConnection` might be blocked on it awaiting for:
          -- - handshake negotiation; or
          -- - `Terminate: TerminatingState → TerminatedState` transition.
          -- That's why we use 'uninterruptibleMask'. Note that this cleanup
          -- function after all is interruptible, because we unmask async
          -- exceptions around 'threadDelay', but even if an async exception
          -- hits there we will update `connVar`.
          uninterruptibleMask $ \unmask -> do
            traceWith tracer (TrConnectionCleanup connId)
            eTransition <- modifyTMVar stateVar $ \state -> do
              eTransition <- atomically $ do
                connState <- readTVar connVar
                let connState' = TerminatedState Nothing
                    transition = mkTransition connState connState'
                    transitionTrace = TransitionTrace peerAddr transition
                case connState of
                  ReservedOutboundState -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  UnnegotiatedState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  OutboundUniState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  OutboundDupState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  OutboundIdleState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  InboundIdleState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  InboundState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  DuplexState {} -> do
                    writeTVar connVar connState'
                    return $ Left (Just transitionTrace)
                  TerminatingState {} -> do
                    return $ Right transition
                  TerminatedState {} ->
                    return $ Left Nothing

              case eTransition of
                Left mbTransition -> do
                  traverse_ (traceWith trTracer) mbTransition
                  close cmSnocket socket
                  return ( state
                         , Left ()
                         )
                Right transition -> do
                  close cmSnocket socket
                  return ( state
                         , Right transition
                         )

            case eTransition of
              Left () -> do

                let transition =
                      TransitionTrace
                        peerAddr
                        Transition
                           { fromState = Known (TerminatedState Nothing)
                           , toState   = Unknown
                           }
                mbTransition <- modifyTMVar stateVar $ \state ->
                  case Map.lookup peerAddr state of
                    Nothing -> pure (state, Nothing)
                    Just v  ->
                      if mutableConnState == v
                         then pure (Map.delete peerAddr state , Just transition)
                         else pure (state                     , Nothing)

                traverse_ (traceWith trTracer) mbTransition
                traceCounters stateVar
              Right transition ->
                do traceWith tracer (TrConnectionTimeWait connId)
                   when (cmTimeWaitTimeout > 0) $
                     let -- make sure we wait at least 'cmTimeWaitTimeout', we
                         -- ignore all 'AsyncCancelled' exceptions.
                         forceThreadDelay delay | delay <= 0 = pure ()
                         forceThreadDelay delay = do
                           t <- getMonotonicTime
                           unmask (threadDelay delay)
                             `catch` \e ->
                                case fromException e
                                of Just AsyncCancelled -> do
                                     t' <- getMonotonicTime
                                     forceThreadDelay (delay - t' `diffTime` t)
                                   _ -> throwIO e
                     in forceThreadDelay cmTimeWaitTimeout
                `finally` do
                  -- We must ensure that we update 'connVar',
                  -- `requestOutboundConnection` might be blocked on it awaiting for:
                  -- - handshake negotiation; or
                  -- - `Terminate: TerminatingState → TerminatedState` transition.
                  traceWith tracer (TrConnectionTimeWaitDone connId)

                  trs <- atomically $ do
                    connState <- readTVar connVar
                    let transition' = transition { fromState = Known connState }
                        shouldTrace = abstractState (Known connState)
                                   /= TerminatedSt
                    writeTVar connVar (TerminatedState Nothing)
                    --  We have to be careful when deleting it from
                    --  'ConnectionManagerState'.
                    updated <-
                      modifyTMVarPure
                        stateVar
                        ( \state ->
                          case Map.lookup peerAddr state of
                            Nothing -> (state, False)
                            Just v  ->
                              if mutableConnState == v
                                then (Map.delete peerAddr state , True)
                                else (state                     , False)
                        )

                    if updated
                       then do
                      -- Key was present in the dictionary (stateVar) and
                      -- removed so we trace the removal.
                        let trs = [ Transition
                                     { fromState = Known (TerminatedState Nothing)
                                     , toState   = Unknown
                                     }
                                  ]
                        return $
                          if shouldTrace
                             then transition' : trs
                             else trs
                      -- Key was not present in the dictionary (stateVar),
                      -- so we do not trace anything as it was already traced upon
                      -- deletion.
                      --
                      -- OR
                      --
                      -- Key was overwritten in the dictionary (stateVar),
                      -- so we do not trace anything as it was already traced upon
                      -- overwriting.
                       else return [ ]

                  traverse_ (traceWith trTracer . TransitionTrace peerAddr) trs
                  traceCounters stateVar

    -- Pruning is done in two stages:
    -- * an STM transaction which selects which connections to prune, and sets
    --   their state to 'TerminatedState';
    -- * an io action which logs and cancels all the connection handler
    --   threads.
    mkPruneAction :: peerAddr
                  -> Int
                  -- ^ number of connections to prune
                  -> ConnectionManagerState peerAddr handle handleError version m
                  -> ConnectionState peerAddr handle handleError version  m
                  -- ^ next connection state, if it will not be pruned.
                  -> StrictTVar m (ConnectionState peerAddr handle handleError version m)
                  -> StrictTVar m StdGen
                  -> Async m ()
                  -> STM m (Bool, PruneAction m)
                  -- ^ return if the connection was choose to be pruned and the
                  -- 'PruneAction'
    mkPruneAction peerAddr numberToPrune state connState' connVar stdGenVar connThread = do
      (choiceMap' :: Map peerAddr ( ConnectionType
                                  , Async m ()
                                  , StrictTVar m
                                      (ConnectionState
                                        peerAddr
                                        handle handleError
                                        version m)
                                  ))
        <- flip Map.traverseMaybeWithKey state $ \_peerAddr MutableConnState { connVar = connVar' } ->
             (\cs -> do
                 -- this expression returns @Maybe (connType, connThread)@;
                 -- 'traverseMaybeWithKey' collects all 'Just' cases.
                 guard (isInboundConn cs)
                 (,,connVar') <$> getConnType cs
                              <*> getConnThread cs)
         <$> readTVar connVar'
      let choiceMap =
            case getConnType connState' of
              Nothing -> assert False choiceMap'
              Just a  -> Map.insert peerAddr (a, connThread, connVar)
                                    choiceMap'

      stdGen <- stateTVar stdGenVar split
      let pruneSet = cmPrunePolicy
                       stdGen
                       ((\(a,_,_) -> a) <$> choiceMap)
                       numberToPrune

      let pruneMap = choiceMap `Map.restrictKeys` pruneSet
      forM_ pruneMap $ \(_, _, connVar') ->
        writeTVar connVar' (TerminatedState Nothing)

      return ( peerAddr `Set.member` pruneSet
             , PruneAction $ do
                 traceWith tracer (TrPruneConnections (Map.keysSet pruneMap)
                                                      numberToPrune
                                                      (Map.keysSet choiceMap))
                 -- we don't block until the thread terminates, delivering the
                 -- async exception is enough (although in this case, there's no
                 -- difference, since we put the connection in 'TerminatedState'
                 -- which avoids the 'cmTimeWaitTimeout').
                 forM_ pruneMap $ \(_, connThread', _) ->
                                   throwTo (asyncThreadId connThread')
                                           AsyncCancelled
             )

    includeInboundConnectionImpl
        :: HasCallStack
        => FreshIdSupply m
        -> StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> ConnectionHandlerFn handlerTrace socket peerAddr handle handleError (version, versionData) m
        -> Word32
        -- ^ inbound connections hard limit
        -- TODO: This is needed because the accept loop can not guarantee that
        -- includeInboundConnection can run safely without going above the
        -- hard limit.  We have to check if we are not above the hard limit
        -- after locking the connection manager state `TMVar` and  then decide
        -- whether we can include the connection or not.
        -> socket
        -- ^ resource to include in the state
        -> peerAddr
        -- ^ remote address used as an identifier of the resource
        -> m (Connected peerAddr handle handleError)
    includeInboundConnectionImpl freshIdSupply
                                 stateVar
                                 handler
                                 hardLimit
                                 socket
                                 peerAddr = do
        (r, connId) <- modifyTMVar stateVar $ \state -> do
          localAddress <- getLocalAddr cmSnocket socket
          numberOfCons <- atomically $ countIncomingConnections state

          let connId = ConnectionId { localAddress, remoteAddress = peerAddr }

              -- Check if after accepting this connection we get above the
              -- hard limit
              canAccept = numberOfCons + 1 <= fromIntegral hardLimit

          if canAccept
          then do
            let provenance = Inbound
            traceWith tracer (TrIncludeConnection provenance peerAddr)
            (reader, writer) <- newEmptyPromiseIO
            (connThread, connVar, connState0, connState) <-
              mfix $ \ ~(connThread, _mutableConnVar, _connState0, _connState) -> do
                -- Either
                -- @
                --   Accepted    : ● → UnnegotiatedState Inbound
                --   Overwritten : ● → UnnegotiatedState Inbound
                --   SelfConn    : UnnegotiatedState Outbound
                --               → UnnegotiatedState Inbound
                -- @
                --
                -- This is subtle part, which needs to handle a near simultaneous
                -- open.  We cannot rely on 'ReservedOutboundState' state as
                -- a lock.  It may happen that the `requestOutboundConnection`
                -- will put 'ReservedOutboundState', but before it will call `connect`
                -- the `accept` call will return.  We overwrite the state and
                -- replace the connection state 'TVar' with a fresh one.  Nothing
                -- is blocked on the replaced 'TVar'.
                --
                -- The `SelfConn` transition can happen when the node tries to
                -- connect to itself (and inbound & outbound addresses are the
                -- same).
                let connState' = UnnegotiatedState provenance connId connThread
                (mutableConnVar', connState0') <-
                  atomically $ do
                    let v0 = Map.lookup peerAddr state
                    case v0 of
                      Nothing -> do
                        -- 'Accepted'
                        v <- newMutableConnState peerAddr freshIdSupply connState'
                        labelTVar (connVar v) ("conn-state-" ++ show connId)
                        return (v, Nothing)
                      Just v -> do
                        -- 'Overwritten', 'SelfConn' or accepting a new
                        -- connection while the old one is terminating.
                        connState0' <- readTVar (connVar v)
                        !v' <- case connState0' of
                           -- Overwritten
                           ReservedOutboundState {} -> writeTVar (connVar v) connState'
                                                    $> v
                           -- SelfConn
                           UnnegotiatedState     {} -> writeTVar (connVar v) connState'
                                                    $> v

                           OutboundUniState      {} -> writeTVar (connVar v) connState'
                                                    $> assert False v
                           OutboundDupState      {} -> writeTVar (connVar v) connState'
                                                    $> assert False v
                           OutboundIdleState     {} -> writeTVar (connVar v) connState'
                                                    $> assert False v
                           DuplexState           {} -> writeTVar (connVar v) connState'
                                                    $> assert False v
                           InboundIdleState      {} -> writeTVar (connVar v) connState'
                                                    $> assert False v
                           InboundState          {} -> writeTVar (connVar v) connState'
                                                    $> assert False v

                           TerminatingState      {} -> newMutableConnState peerAddr freshIdSupply connState'
                           TerminatedState       {} -> newMutableConnState peerAddr freshIdSupply connState'

                        labelTVar (connVar v') ("conn-state-" ++ show connId)
                        return (v', Just connState0')

                connThread' <-
                  forkConnectionHandler
                     stateVar mutableConnVar' socket connId writer handler
                return (connThread', mutableConnVar', connState0', connState')

            traceWith trTracer (TransitionTrace peerAddr
                                 Transition { fromState = maybe Unknown Known connState0
                                            , toState   = Known connState
                                            })
            return ( Map.insert peerAddr connVar state
                   , (Just (connVar, connThread, reader), connId)
                   )
          else
            return ( state
                   , (Nothing, connId)
                   )

        case r of
          Nothing ->
            return (Disconnected connId Nothing)

          Just (mutableConnState@MutableConnState { connVar }
               , connThread, reader) -> do
            traceCounters stateVar

            res <- atomically $ readPromise reader
            case res of
              Left handleError -> do
                terminateInboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState $ Just handleError

              Right HandshakeConnectionQuery -> do
                terminateInboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState Nothing

              Right (HandshakeConnectionResult handle (version, versionData)) -> do
                let dataFlow = connectionDataFlow version versionData
                (connected, mbTransition, provenance) <- atomically $ do
                  connState <- readTVar connVar
                  case connState of
                    -- Inbound connections cannot be found in this state at this
                    -- stage.
                    ReservedOutboundState ->
                      throwSTM (withCallStack (ImpossibleState peerAddr))

                    --
                    -- The common case.
                    --
                    -- Note: we don't set an explicit timeout here.  The
                    -- server will set a timeout and call
                    -- 'unregisterInboundConnection' when it expires.
                    --
                    UnnegotiatedState {} -> do
                      let connState' = InboundIdleState
                                         connId connThread handle dataFlow
                      writeTVar connVar connState'
                      return ( True
                             , Just $ mkTransition connState connState'
                             , Inbound
                             )

                    -- Self connection: the inbound side lost the race to update
                    -- the state after negotiating the connection.
                    OutboundUniState {} -> return (True, Nothing, Outbound)
                    OutboundDupState {} -> return (True, Nothing, Outbound)

                    OutboundIdleState _ _ _ dataFlow' -> do
                      let connState' = InboundIdleState
                                         connId connThread handle
                                         dataFlow'
                      writeTVar connVar connState'
                      return ( True
                             , Just $ mkTransition connState connState'
                             , Outbound
                             )

                    InboundIdleState {} ->
                      throwSTM (withCallStack (ImpossibleState peerAddr))

                    -- At this stage the inbound connection cannot be in
                    -- 'InboundState', it would mean that there was another thread
                    -- that included that connection, but this would violate @TCP@
                    -- constraints.
                    InboundState {} ->
                      throwSTM (withCallStack (ImpossibleState peerAddr))

                    DuplexState {} ->
                      throwSTM (withCallStack (ImpossibleState peerAddr))

                    TerminatingState {} -> return (False, Nothing, Inbound)

                    TerminatedState {} -> return (False, Nothing, Inbound)

                traverse_ (traceWith trTracer . TransitionTrace peerAddr) mbTransition
                traceCounters stateVar

                -- Note that we don't set a timeout thread here which would
                -- perform:
                -- @
                --   Commit^{dataFlow}
                --     : InboundIdleState dataFlow
                --     → TerminatingState
                -- @
                -- This is not needed!  When we return from this call, the inbound
                -- protocol governor will monitor the connection.  Once it becomes
                -- idle, it will call 'unregisterInboundConnection' which will
                -- perform the aforementioned @Commit@ transition.

                if connected
                  then do
                    case inboundGovernorInfoChannel of
                      InResponderMode infoChannel ->
                        atomically $ InfoChannel.writeMessage
                                       infoChannel
                                       (NewConnectionInfo provenance connId dataFlow handle)
                      _ -> return ()
                    return $ Connected connId dataFlow handle

                  -- the connection is in `TerminatingState` or
                  -- `TerminatedState`.
                  else
                    return $ Disconnected connId Nothing

    terminateInboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState handleErrorM = do
        transitions <- atomically $ do
          connState <- readTVar connVar

          let connState' =
                case classifyHandleError <$> handleErrorM of
                  Just HandshakeFailure ->
                    TerminatingState connId connThread
                                    handleErrorM
                  Just HandshakeProtocolViolation ->
                    TerminatedState handleErrorM
                  -- On inbound query, connection is terminating.
                  Nothing ->
                    TerminatingState connId connThread
                                    handleErrorM
              transition = mkTransition connState connState'
              absConnState = abstractState (Known connState)
              shouldTrace = absConnState /= TerminatedSt

          updated <-
            modifyTMVarSTM
              stateVar
              ( \state ->
                case Map.lookup peerAddr state of
                  Nothing -> return (state, False)
                  Just mutableConnState'  ->
                    if mutableConnState' == mutableConnState
                      then do
                        -- 'handleError' might be either a handshake
                        -- negotiation a protocol failure (an IO
                        -- exception, a timeout or codec failure).  In
                        -- the first case we should not reset the
                        -- connection as this is not a protocol error.
                        --
                        -- If we are deleting the connState from the
                        -- state then connState' can be TerminatingSt in
                        -- which case we are going to transition
                        -- TerminatingSt -> TerminatedSt. Otherwise,
                        -- Connection Manager cleanup will take care of
                        -- tracing accordingly.
                        writeTVar connVar connState'

                        return (Map.delete peerAddr state , True)
                      else return (state                  , False)
              )

          if updated
             then
            -- Key was present in the dictionary (stateVar) and
            -- removed so we trace the removal.
              return $
                if shouldTrace
                   then [ transition
                        , Transition
                           { fromState = Known (TerminatedState Nothing)
                           , toState   = Unknown
                           }
                        ]
                   else [ Transition
                           { fromState = Known (TerminatedState Nothing)
                           , toState   = Unknown
                           }
                        ]
            -- Key was not present in the dictionary (stateVar),
            -- so we do not trace anything as it was already traced upon
            -- deletion.
            --
            -- OR
            --
            -- Key was overwritten in the dictionary (stateVar),
            -- so we do not trace anything as it was already traced upon
            -- overwriting.
             else return [ ]

        traverse_ (traceWith trTracer . TransitionTrace peerAddr) transitions
        traceCounters stateVar

        return (Disconnected connId handleErrorM)

    -- We need 'mask' in order to guarantee that the traces are logged if an
    -- async exception lands between the successful STM action and the logging
    -- action.
    unregisterInboundConnectionImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m (OperationResult DemotedToColdRemoteTr)
    unregisterInboundConnectionImpl stateVar peerAddr = mask_ $ do
      traceWith tracer (TrUnregisterConnection Inbound peerAddr)
      (mbThread, mbTransition, result, mbAssertion) <- atomically $ do
        state <- readTMVar stateVar
        case Map.lookup peerAddr state of
          Nothing -> do
            -- Note: this can happen if the inbound connection manager is
            -- notified late about the connection which has already terminated
            -- at this point.
            pure ( Nothing
                 , Nothing
                 , OperationSuccess CommitTr
                 , Nothing
                 )
          Just MutableConnState { connVar } -> do
            connState <- readTVar connVar
            let st = abstractState (Known connState)
            case connState of
              -- In any of the following two states unregistering is not
              -- supported.  'includeInboundConnection' is a synchronous
              -- operation which returns only once the connection is
              -- negotiated.
              ReservedOutboundState ->
                return ( Nothing
                       , Nothing
                       , UnsupportedState st
                       , Nothing
                       )
              UnnegotiatedState {} ->
                return ( Nothing
                       , Nothing
                       , UnsupportedState st
                       , Nothing
                       )

              -- @
              --   TimeoutExpired : OutboundState^\tau Duplex
              --                  → OutboundState      Duplex
              -- @
              OutboundDupState connId connThread handle Ticking -> do
                let connState' = OutboundDupState connId connThread handle Expired
                writeTVar connVar connState'
                return ( Nothing
                       , Just (mkTransition connState connState')
                       , OperationSuccess KeepTr
                       , Nothing
                       )
              OutboundDupState connId _connThread _handle Expired ->
                assert False $
                return ( Nothing
                       , Nothing
                       , OperationSuccess KeepTr
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                 (UnregisterInboundConnection (Just connId)
                                                              st)
                                 )
                       )

              OutboundUniState _connId _connThread _handle ->
                return ( Nothing
                       , Nothing
                       , UnsupportedState st
                       , Nothing
                       )

              -- unexpected state, this state is reachable only from outbound
              -- states
              OutboundIdleState connId _connThread _handle _dataFlow ->
                return ( Nothing
                       , Nothing
                       , OperationSuccess CommitTr
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                 (UnregisterInboundConnection (Just connId)
                                                              st)
                                 )
                       )

              -- @
              --   Commit^{dataFlow} : InboundIdleState dataFlow
              --                     → TerminatingState
              -- @
              --
              -- Note: the 'TrDemotedToColdRemote' is logged by the server.
              InboundIdleState connId connThread _handle _dataFlow -> do
                let connState' = TerminatingState connId connThread Nothing
                writeTVar connVar connState'
                return ( Just connThread
                       , Just (mkTransition connState connState')
                       , OperationSuccess CommitTr
                       , Nothing
                       )

              -- the inbound protocol governor was supposed to call
              -- 'demotedToColdRemote' first.
              InboundState connId connThread _handle _dataFlow -> do
                let connState' = TerminatingState connId connThread Nothing
                writeTVar connVar connState'
                return ( Just connThread
                       , Just (mkTransition connState connState')
                       , UnsupportedState st
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                 (UnregisterInboundConnection (Just connId)
                                                              st)
                                 )
                       )

              -- the inbound connection governor ought to call
              -- 'demotedToColdRemote' first.
              DuplexState connId connThread handle -> do
                let connState' = OutboundDupState connId connThread handle Ticking
                writeTVar connVar connState'
                return ( Nothing
                       , Just (mkTransition connState connState')
                       , UnsupportedState st
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                 (UnregisterInboundConnection (Just connId)
                                                              st)
                                 )
                       )

              -- If 'unregisterOutboundConnection' is called just before
              -- 'unregisterInboundConnection', the latter one might observe
              -- 'TerminatingState'.
              TerminatingState _connId _connThread _handleError ->
                return ( Nothing
                       , Nothing
                       , OperationSuccess CommitTr
                       , Nothing
                       )
              -- However, 'TerminatedState' should not be observable by
              -- 'unregisterInboundConnection', unless 'cmTimeWaitTimeout' is
              -- close to 'serverProtocolIdleTimeout'.
              TerminatedState _handleError ->
                return ( Nothing
                       , Nothing
                       , UnsupportedState TerminatedSt
                       , Nothing
                       )

      traverse_ (traceWith trTracer . TransitionTrace peerAddr) mbTransition
      traceCounters stateVar

      -- 'throwTo' avoids blocking until 'cmTimeWaitTimeout' expires.
      traverse_ (flip throwTo AsyncCancelled . asyncThreadId)
                mbThread

      whenJust mbAssertion $ \tr -> do
        traceWith tracer tr
        _ <- evaluate (assert False)
        pure ()

      return result

    requestOutboundConnectionImpl
        :: HasCallStack
        => FreshIdSupply m
        -> StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> ConnectionHandlerFn handlerTrace socket peerAddr handle handleError (version, versionData) m
        -> peerAddr
        -> m (Connected peerAddr handle handleError)
    requestOutboundConnectionImpl freshIdSupply stateVar handler peerAddr = do
        let provenance = Outbound
        traceWith tracer (TrIncludeConnection provenance peerAddr)
        (trace, mutableConnState@MutableConnState { connVar }
              , eHandleWedge) <- atomically $ do
          state <- readTMVar stateVar
          case Map.lookup peerAddr state of
            Just mutableConnState@MutableConnState { connVar } -> do
              connState <- readTVar connVar
              let st = abstractState (Known connState)
              case connState of
                ReservedOutboundState ->
                  return ( Just (Right (TrConnectionExists provenance peerAddr st))
                         , mutableConnState
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                UnnegotiatedState Outbound _connId _connThread -> do
                  return ( Just (Right (TrConnectionExists provenance peerAddr st))
                         , mutableConnState
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                UnnegotiatedState Inbound connId _connThread ->
                  -- we must not block inside @modifyTVar stateVar@, we
                  -- return 'There' to indicate that we need to block on
                  -- the connection state.
                  return ( Nothing
                         , mutableConnState
                         , Right (There connId)
                         )

                OutboundUniState {} -> do
                  return ( Just (Right (TrConnectionExists provenance peerAddr st))
                         , mutableConnState
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                OutboundDupState {} -> do
                  return ( Just (Right (TrConnectionExists provenance peerAddr st))
                         , mutableConnState
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                OutboundIdleState _connId _connThread _handle _dataFlow ->
                  let tr = abstractState (Known connState) in
                  return ( Just (Right (TrForbiddenOperation peerAddr tr))
                         , mutableConnState
                         , Left (withCallStack (ForbiddenOperation peerAddr tr))
                         )

                InboundIdleState connId _connThread _handle Unidirectional -> do
                  return ( Just (Right (TrForbiddenConnection connId))
                         , mutableConnState
                         , Left (withCallStack
                                  (ForbiddenConnection connId))
                         )

                InboundIdleState connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   Awake^{Duplex}_{Local} : InboundIdleState Duplex
                  --                          → OutboundState^\tau Duplex
                  -- @
                  let connState' = OutboundDupState connId connThread handle Ticking
                  writeTVar connVar connState'
                  return ( Just (Left (TransitionTrace
                                         peerAddr
                                         (mkTransition connState connState')))
                         , mutableConnState
                         , Right (Here (Connected connId dataFlow handle))
                         )

                InboundState connId _connThread _handle Unidirectional -> do
                  -- the remote side negotiated unidirectional connection, we
                  -- cannot re-use it.
                  return ( Just (Right (TrForbiddenConnection connId))
                         , mutableConnState
                         , Left (withCallStack
                                  (ForbiddenConnection connId))
                         )

                InboundState connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   PromotedToWarm^{Duplex}_{Local} : InboundState Duplex
                  --                                   → DuplexState
                  -- @
                  let connState' = DuplexState connId connThread handle
                  writeTVar connVar connState'
                  return ( Just (Left (TransitionTrace
                                        peerAddr
                                        (mkTransition connState connState')))
                         , mutableConnState
                         , Right (Here (Connected connId dataFlow handle))
                         )

                DuplexState _connId _connThread  _handle ->
                  return ( Just (Right (TrConnectionExists provenance peerAddr st))
                         , mutableConnState
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                TerminatingState _connId _connThread _handleError ->
                  -- await for 'TerminatedState' or for removal of the
                  -- connection from the state.
                  retry

                TerminatedState _handleError -> do
                  -- the connection terminated; we can not reset 'connVar' and
                  -- start afresh. We should wait for the removal of the
                  -- connection from the state.
                  retry

            Nothing -> do
              let connState' = ReservedOutboundState
              (mutableConnState :: MutableConnState peerAddr handle handleError
                                                    version m)
                <- newMutableConnState peerAddr freshIdSupply connState'
              -- TODO: label `connVar` using 'ConnectionId'
              labelTVar (connVar mutableConnState) ("conn-state-" ++ show peerAddr)

              -- record the @connVar@ in 'ConnectionManagerState' we can use
              -- 'swapTMVar' as we did not use 'takeTMVar' at the beginning of
              -- this transaction.  Since we already 'readTMVar', it will not
              -- block.
              (mbConnState
                 :: Maybe (ConnectionState peerAddr handle handleError version m))
                   <- swapTMVar stateVar
                        (Map.insert peerAddr mutableConnState state)
                        >>= traverse (readTVar . connVar) . Map.lookup peerAddr
              return ( Just (Left (TransitionTrace
                                    peerAddr
                                    Transition {
                                        fromState = maybe Unknown Known mbConnState,
                                        toState   = Known connState'
                                      }))
                     , mutableConnState
                     , Right Nowhere
                     )

        traverse_ (either (traceWith trTracer) (traceWith tracer)) trace
        traceCounters stateVar
        case eHandleWedge of
          Left e ->
            throwIO e

          -- connection manager does not have a connection with @peerAddr@.
          Right Nowhere -> do
            (reader, writer) <- newEmptyPromiseIO

            (connId, connThread) <-
              -- This section of code passes the control over socket from
              -- `bracketOnError` which is responsible for:
              --
              --    * creating socket
              --    * connecting to remote host
              --    * obtaining local address of the connection
              --
              -- to the connection handler and its resource cleanup.
              -- Both the 'bracketOnError''s resource handler and the
              -- connection handler cleanup function are responsible for:
              --
              --  * closing the socket
              --  * freeing the slot in connection manager state map
              --
              mask $ \unmask -> do

                --
                -- connect
                --

                (socket, connId) <-
                  unmask $ bracketOnError
                    (openToConnect cmSnocket peerAddr)
                    (\socket -> uninterruptibleMask_ $ do
                      close cmSnocket socket
                      trs <- atomically $ modifyTMVarSTM stateVar $ \state -> do
                        case Map.lookup peerAddr state of
                          -- Lookup failed, which means connection was already
                          -- removed.  So we just update the connVar and trace
                          -- accordingly.
                          Nothing -> do
                            connState <- readTVar connVar
                            let connState' = TerminatedState Nothing
                            writeTVar connVar connState'
                            return
                              ( state
                              , [ mkTransition connState connState'
                                , Transition (Known connState') Unknown
                                ]
                              )

                          -- Current connVar.
                          Just mutableConnState' -> do
                            connState <- readTVar connVar
                            case connState of
                              -- Update the state only if the connection was in
                              -- 'ReservedOutboundState'.  This covers the case
                              -- when we connect to ourselves, in which case: we
                              -- first set the connection state to
                              -- `ReservedOutboundState`, then race connect
                              -- & accept calls.  If the connection was
                              -- accepted it, it will use the same
                              -- 'MutableConnState', and if the inbound side is
                              -- using the connection the state will be
                              -- different than `ReservedOutboundState`.
                              ReservedOutboundState | mutableConnState' == mutableConnState -> do
                                let state' = Map.delete peerAddr state
                                    connState' = TerminatedState Nothing
                                writeTVar connVar connState'
                                return
                                  ( state'
                                  , [ mkTransition connState connState'
                                    , Transition (Known connState')
                                                 Unknown
                                    ]
                                  )

                                -- self connection: the connection might have
                                -- been accepted, in such case do not modify its
                                -- state.
                              _ -> return (state, [])

                      traverse_ (traceWith trTracer . TransitionTrace peerAddr) trs
                      traceCounters stateVar
                    )
                    $ \socket -> do
                      traceWith tracer (TrConnectionNotFound provenance peerAddr)
                      let addr = case cmAddressType peerAddr of
                                   Nothing          -> Nothing
                                   Just IPv4Address -> cmIPv4Address
                                   Just IPv6Address -> cmIPv6Address
                      cmConfigureSocket socket addr
                      case cmAddressType peerAddr of
                        Nothing -> pure ()
                        Just IPv4Address ->
                             traverse_ (bind cmSnocket socket)
                                       cmIPv4Address
                        Just IPv6Address ->
                             traverse_ (bind cmSnocket socket)
                                       cmIPv6Address

                      traceWith tracer (TrConnect addr peerAddr)
                      connect cmSnocket socket peerAddr
                        `catch` \e -> do
                          traceWith tracer (TrConnectError addr peerAddr e)
                          -- the handler attached by `bracketOnError` will
                          -- reset the state
                          throwIO e
                      localAddress <- getLocalAddr cmSnocket socket
                      let connId = ConnectionId { localAddress
                                                , remoteAddress = peerAddr
                                                }
                      return (socket, connId)

                --
                -- fork connection handler; it will unmask exceptions
                --

                connThread <-
                  forkConnectionHandler
                    stateVar mutableConnState socket connId writer handler
                return (connId, connThread)

            (trans, mbAssertion) <- atomically $ do
              connState <- readTVar connVar

              -- @
              --  Connected : ReservedOutboundState
              --            → UnnegotiatedState Outbound
              -- @
              case connState of
                ReservedOutboundState -> do
                  let connState' = UnnegotiatedState provenance connId connThread
                  writeTVar connVar connState'
                  return ( Just $ mkTransition connState connState'
                         , Nothing
                         )

                -- @
                --  SelfConn⁻¹ : UnnegotiatedState Inbound
                --             → UnnegotiatedState Outbound
                -- @
                --
                UnnegotiatedState Inbound _connId _connThread -> do
                  let connState' = UnnegotiatedState Outbound connId connThread
                  writeTVar connVar connState'
                  return ( Just $ mkTransition connState connState'
                         , Nothing
                         )


                TerminatingState {} ->
                  return (Nothing, Nothing)
                TerminatedState {} ->
                  return (Nothing, Nothing)
                _ ->
                  return ( Nothing
                         , Just (CM.TrUnexpectedlyFalseAssertion
                                   (RequestOutboundConnection
                                     (Just connId)
                                     (abstractState (Known connState))
                                   )
                                )
                         )

            traverse_ (traceWith trTracer . TransitionTrace peerAddr) trans
            traverse_ (traceWith tracer >=> evaluate . assert True)
                      mbAssertion
            traceCounters stateVar

            res <- atomically (readPromise reader)
            case res of
              Left handleError -> do
                terminateOutboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState $ Just handleError

              Right HandshakeConnectionQuery -> do
                terminateOutboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState Nothing

              Right (HandshakeConnectionResult handle (version, versionData)) -> do
                let dataFlow = connectionDataFlow version versionData
                -- We can safely overwrite the state: after successful
                -- `connect` it's not possible to have a race condition
                -- with any other inbound thread.  We are also guaranteed
                -- to have exclusive access as an outbound thread.
                mbTransition <- atomically $ do
                  connState <- readTVar  connVar
                  case connState of
                    UnnegotiatedState provenance' _ _ ->
                      case dataFlow of
                        Unidirectional -> do
                          -- @
                          --  Negotiated^{Unidirectional}_{Outbound}
                          --    : UnnegotiatedState Outbound
                          --    → OutboundUniState Outbound
                          -- @
                          let connState' = OutboundUniState connId connThread handle
                          writeTVar connVar connState'
                          return (Just $ mkTransition connState connState')
                        Duplex -> do
                          -- @
                          --  Negotiated^{Duplex}_{Outbound}
                          --    : UnnegotiatedState Outbound
                          --    → OutboundDupState^\tau Outbound
                          -- @
                          let connState' = OutboundDupState connId connThread handle Ticking
                              notifyInboundGov =
                                case provenance' of
                                  Inbound  -> False
                                  -- This is a connection to oneself; We don't
                                  -- need to notify the inbound governor, as
                                  -- it's already done by
                                  -- `includeInboundConnectionImpl`
                                  Outbound -> True
                          writeTVar connVar connState'
                          case inboundGovernorInfoChannel of
                            InResponderMode infoChannel | notifyInboundGov ->
                              InfoChannel.writeMessage
                                infoChannel
                                (NewConnectionInfo provenance' connId dataFlow handle)
                            _ -> return ()
                          return (Just $ mkTransition connState connState')

                    -- @
                    --   SelfConn'^{-1}
                    --     : InboundIdleState Unidirectional
                    --     → OutboundUniState
                    -- @
                    InboundIdleState connId' connThread' handle' Unidirectional -> do
                      let connState' = OutboundUniState connId' connThread' handle'
                      writeTVar connVar connState'
                      return (Just $ mkTransition connState connState')

                    -- @
                    --   SelfConn'^{-1}
                    --     : InboundIdleState Duplex
                    --     → OutboundDupState
                    -- @
                    InboundIdleState connId' connThread' handle' Duplex -> do
                      let connState' = OutboundDupState connId' connThread' handle' Ticking
                      writeTVar connVar connState'
                      return (Just $ mkTransition connState connState')

                    TerminatedState _ ->
                      return Nothing
                    _ ->
                      let st = abstractState (Known connState) in
                      throwSTM (withCallStack (ForbiddenOperation peerAddr st))
                traverse_ (traceWith trTracer .  TransitionTrace peerAddr)
                          mbTransition
                traceCounters stateVar
                return $ case mbTransition of
                  Just _  -> Connected    connId dataFlow handle
                  Nothing -> Disconnected connId Nothing

          Right (There connId) -> do
            -- We can only enter the 'There' case if there is an inbound
            -- connection, and we are about to reuse it, but we need to wait
            -- for handshake.
            (etr, connected) <- atomically $ do
              connState <- readTVar connVar
              case connState of
                ReservedOutboundState {} ->
                  throwSTM
                    (withCallStack (ImpossibleState (remoteAddress connId)))
                UnnegotiatedState Outbound _ _ ->
                  throwSTM
                    (withCallStack (ConnectionExists provenance connId))

                UnnegotiatedState Inbound _ _ ->
                  -- await for connection negotiation
                  retry

                OutboundUniState {} ->
                  throwSTM (withCallStack (ConnectionExists provenance connId))

                OutboundDupState {} ->
                  throwSTM (withCallStack (ConnectionExists provenance connId))

                OutboundIdleState _connId _connThread _handle _dataFlow ->
                  let tr = abstractState (Known connState) in
                  throwSTM (withCallStack (ForbiddenOperation peerAddr tr))

                InboundIdleState _connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   Awake^{Duplex}_{Local} : InboundIdleState Duplex
                  --                          → OutboundState^\tau Duplex
                  -- @
                  -- This transition can happen if there are concurrent
                  -- `includeInboundConnection` and `requestOutboundConnection`
                  -- calls.
                  let connState' = OutboundDupState connId connThread handle Ticking
                  writeTVar connVar connState'
                  return ( Left (TransitionTrace
                                  peerAddr
                                  (mkTransition connState connState'))
                         , Connected connId dataFlow handle
                         )

                InboundIdleState _connId _connThread _handle Unidirectional ->
                  throwSTM
                    (withCallStack (ForbiddenConnection connId))

                InboundState _connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   PromotedToWarm^{Duplex}_{Local} : InboundState Duplex
                  --                                   → DuplexState
                  -- @
                  --
                  -- Note: this is unlikely to observe: @There connId@ only
                  -- appears if an inbound connection is unnegotiated, it is
                  -- more likely to observe the
                  -- @
                  --    InboundIdleState Duplex -> OutboundDupState
                  -- @
                  -- transition.
                  let connState' = DuplexState connId connThread handle
                  writeTVar connVar connState'
                  return ( Left (TransitionTrace
                                  peerAddr
                                  (mkTransition connState connState'))
                         , Connected connId dataFlow handle
                         )

                InboundState _ _ _ Unidirectional ->
                  throwSTM (withCallStack (ForbiddenConnection connId))

                DuplexState {} ->
                  throwSTM (withCallStack (ConnectionExists provenance connId))

                TerminatingState _connId _connThread handleError ->
                  return ( Right (TrTerminatingConnection provenance connId)
                         , Disconnected connId handleError
                         )
                TerminatedState handleError ->
                  return ( Right (TrTerminatedConnection provenance
                                                         (remoteAddress connId))
                         , Disconnected connId handleError
                         )

            case etr of
              Left tr'  -> traceWith trTracer tr'
              Right tr' -> traceWith tracer   tr'
            traceCounters stateVar
            return connected

          -- Connection manager has a connection which can be reused.
          Right (Here connected) -> do
            traceCounters stateVar
            return connected

    terminateOutboundWithErrorOrQuery
        :: ConnectionId peerAddr
        -> StrictTVar m (ConnectionState peerAddr handle handleError version m)
        -> Async m ()
        -> peerAddr
        -> StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> MutableConnState peerAddr handle handleError version m
        -> Maybe handleError
        -> m (Connected peerAddr handle handleError)
    terminateOutboundWithErrorOrQuery connId connVar connThread peerAddr stateVar mutableConnState handleErrorM = do
        transitions <- atomically $ do
          connState <- readTVar connVar

          let connState' =
                case classifyHandleError <$> handleErrorM of
                  Just HandshakeFailure ->
                    TerminatingState connId connThread
                                    handleErrorM
                  Just HandshakeProtocolViolation ->
                    TerminatedState handleErrorM
                  -- On outbound query, connection is terminated.
                  Nothing ->
                    TerminatedState handleErrorM
              transition = mkTransition connState connState'
              absConnState = abstractState (Known connState)
              shouldTrace = absConnState /= TerminatedSt

          -- 'handleError' might be either a handshake negotiation
          -- a protocol failure (an IO exception, a timeout or
          -- codec failure).  In the first case we should not reset
          -- the connection as this is not a protocol error.
          writeTVar connVar connState'

          updated <-
            modifyTMVarPure
              stateVar
              ( \state ->
                case Map.lookup peerAddr state of
                  Nothing -> (state, False)
                  Just mutableConnState'  ->
                    if mutableConnState' == mutableConnState
                      then (Map.delete peerAddr state , True)
                      else (state                     , False)
              )

          if updated
             then
            -- Key was present in the dictionary (stateVar) and
            -- removed so we trace the removal.
              return $
                if shouldTrace
                   then [ transition
                        , Transition
                           { fromState = Known (TerminatedState Nothing)
                           , toState   = Unknown
                           }
                        ]
                   else [ Transition
                           { fromState = Known (TerminatedState Nothing)
                           , toState   = Unknown
                           }
                        ]
            -- Key was not present in the dictionary (stateVar),
            -- so we do not trace anything as it was already traced upon
            -- deletion.
            --
            -- OR
            --
            -- Key was overwritten in the dictionary (stateVar),
            -- so we do not trace anything as it was already traced upon
            -- overwriting.
             else return [ ]


        traverse_ (traceWith trTracer . TransitionTrace peerAddr) transitions
        traceCounters stateVar

        return (Disconnected connId handleErrorM)


    unregisterOutboundConnectionImpl
        :: StrictTMVar m
            (ConnectionManagerState peerAddr handle handleError version m)
        -> StrictTVar m StdGen
        -> peerAddr
        -> m (OperationResult AbstractState)
    unregisterOutboundConnectionImpl stateVar stdGenVar peerAddr = do
      traceWith tracer (TrUnregisterConnection Outbound peerAddr)
      (transition, mbAssertion)
        <- atomically $ do
        state <- readTMVar stateVar
        case Map.lookup peerAddr state of
          -- if the connection errored, it will remove itself from the state.
          -- Calling 'unregisterOutboundConnection' is a no-op in this case.
          Nothing -> pure ( DemoteToColdLocalNoop Nothing UnknownConnectionSt
                          , Nothing)

          Just MutableConnState { connVar } -> do
            connState <- readTVar connVar
            let st = abstractState (Known connState)
            case connState of
              -- In any of the following three states unregistering is not
              -- supported.  'requestOutboundConnection' is a synchronous
              -- operation which returns only once the connection is
              -- negotiated.
              ReservedOutboundState ->
                return
                  ( DemoteToColdLocalError
                     (TrForbiddenOperation peerAddr st)
                     st
                  , Nothing
                  )

              UnnegotiatedState _ _ _ ->
                return
                  ( DemoteToColdLocalError
                     (TrForbiddenOperation peerAddr st)
                     st
                  , Nothing
                  )

              OutboundUniState connId connThread handle -> do
                -- @
                --   DemotedToCold^{Unidirectional}_{Local}
                --     : OutboundState Unidirectional
                --     → OutboundIdleState Unidirectional
                -- @
                let connState' = OutboundIdleState connId connThread handle
                                                   Unidirectional
                writeTVar connVar connState'
                return ( DemotedToColdLocal connId connThread connVar
                          (mkTransition connState connState')
                       , Nothing
                       )

              OutboundDupState connId connThread handle Expired -> do
                -- @
                --   DemotedToCold^{Duplex}_{Local}
                --     : OutboundState Duplex
                --     → OutboundIdleState^\tau
                -- @
                let connState' = OutboundIdleState connId connThread handle
                                                   Duplex
                writeTVar connVar connState'
                return ( DemotedToColdLocal connId connThread connVar
                          (mkTransition connState connState')
                       , Nothing
                       )

              OutboundDupState connId connThread handle Ticking -> do
                let connState' = InboundIdleState connId connThread handle Duplex
                    tr = mkTransition connState connState'

                numberOfConns <- countIncomingConnections state

                -- use 'numberOfConns + 1' because we want to know if we
                -- actually let this connection evolve or if we need to make
                -- room for it by pruning.  This is because
                -- 'countIncomingConnections' does not count 'OutboundDupState'
                -- as an inbound connection, but does so for 'InboundIdleState'.
                let numberToPrune =
                      numberOfConns + 1
                      - fromIntegral
                          (acceptedConnectionsHardLimit cmConnectionsLimits)
                if numberToPrune > 0
                then do
                  (_, prune)
                    <- mkPruneAction peerAddr numberToPrune state connState' connVar stdGenVar connThread
                  return
                    ( PruneConnections prune (Left connState)
                    , Nothing
                    )

                else do
                  -- @
                  --   DemotedToCold^{Duplex}_{Local}
                  --     : OutboundState^\tau Duplex
                  --     → InboundIdleState^\tau Duplex
                  -- @
                  -- does not require to perform any additional io action (we
                  -- already updated 'connVar').
                  writeTVar connVar connState'
                  return ( DemoteToColdLocalNoop (Just tr) st
                         , Nothing
                         )

              OutboundIdleState _connId _connThread _handleError _dataFlow ->
                return ( DemoteToColdLocalNoop Nothing st
                       , Nothing
                       )

              -- TODO: This assertion is benign and also hit rarely (once per
              -- 100_000 simulations)
              InboundIdleState _connId _connThread _handle _dataFlow ->
                -- assert (dataFlow == Duplex) $
                return ( DemoteToColdLocalNoop Nothing st
                       , Nothing
                       )
              InboundState connId _connThread _handle dataFlow -> do
                let mbAssertion =
                      if dataFlow == Duplex
                         then Nothing
                         else Just (CM.TrUnexpectedlyFalseAssertion
                                      (UnregisterOutboundConnection
                                        (Just connId)
                                        st)
                                   )
                return
                  ( DemoteToColdLocalError
                     (TrForbiddenOperation peerAddr st)
                     st
                  , mbAssertion
                  )

              DuplexState connId connThread handle -> do
                -- @
                --   DemotedToCold^{Duplex}_{Local} : DuplexState
                --                                  → InboundState Duplex
                -- @
                --
                let connState' = InboundState connId connThread handle Duplex
                    tr = mkTransition connState connState'

                -- @
                -- DemotedToCold^{Duplex}_{Local} : DuplexState
                --                                → InboundState Duplex
                -- @
                -- does not require to perform any additional io action (we
                -- already updated 'connVar').
                writeTVar connVar connState'
                return ( DemoteToColdLocalNoop (Just tr) st
                       , Nothing
                       )

              TerminatingState _connId _connThread _handleError ->
                return (DemoteToColdLocalNoop Nothing st
                       , Nothing
                       )
              TerminatedState _handleError ->
                return ( DemoteToColdLocalNoop Nothing st
                       , Nothing
                       )

      whenJust mbAssertion $ \tr' -> do
        traceWith tracer tr'
        _ <- evaluate (assert False)
        pure ()

      case transition of
        DemotedToColdLocal connId connThread connVar tr -> do
          traceWith trTracer (TransitionTrace peerAddr tr)
          traceCounters stateVar
          timeoutVar <- registerDelay cmOutboundIdleTimeout
          r <- atomically $ runFirstToFinish $
               FirstToFinish (do connState <- readTVar connVar
                                 check (case connState of
                                          OutboundIdleState {} -> False
                                          _                    -> True
                                       )
                                 return (Left connState)
                             )
            <> FirstToFinish (do b <- LazySTM.readTVar timeoutVar
                                 check b
                                 Right <$> readTVar connVar
                             )
          case r of
            Right connState -> do
              let connState' = TerminatingState connId connThread Nothing
              atomically $ writeTVar connVar connState'
              traceWith trTracer (TransitionTrace peerAddr
                                   (mkTransition connState connState'))
              traceCounters stateVar
              -- We rely on the `finally` handler of connection thread to:
              --
              -- - close the socket,
              -- - set the state to 'TerminatedState'
              -- - 'throwTo' avoids blocking until 'cmTimeWaitTimeout' expires.
              throwTo (asyncThreadId connThread)
                      AsyncCancelled
              return (OperationSuccess (abstractState $ Known connState'))

            Left connState  | connectionTerminated connState
                           ->
              return (OperationSuccess (abstractState $ Known connState))
            Left connState ->
              return (UnsupportedState (abstractState $ Known connState))

        PruneConnections prune eTr -> do
          traverse_ (traceWith trTracer . TransitionTrace peerAddr) eTr
          runPruneAction prune
          traceCounters stateVar
          return (OperationSuccess (abstractState (either Known fromState eTr)))

        DemoteToColdLocalError trace st -> do
          traceWith tracer trace
          return (UnsupportedState st)

        DemoteToColdLocalNoop tr a -> do
          traverse_ (traceWith trTracer) (TransitionTrace peerAddr <$> tr)
          traceCounters stateVar
          return (OperationSuccess a)


    -- Needs to mask the STM action and the tracing for the case of an async
    -- exception falls right between the STM commit and the IO tracing. This
    -- guarantees that the same order of transitions and its trace.
    promotedToWarmRemoteImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> StrictTVar m StdGen
        -> peerAddr
        -> m (OperationResult AbstractState)
    promotedToWarmRemoteImpl stateVar stdGenVar peerAddr = mask_ $ do
      (result, pruneTr, mbAssertion) <- atomically $ do
        state <- readTMVar stateVar
        let mbConnVar = Map.lookup peerAddr state
        case mbConnVar of
          Nothing -> return ( UnsupportedState UnknownConnectionSt
                            , Nothing
                            , Nothing
                            )
          Just MutableConnState { connVar } -> do
            connState <- readTVar connVar
            let st = abstractState (Known connState)
            case connState of
              ReservedOutboundState {} -> do
                return ( UnsupportedState st
                       , Nothing
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (PromotedToWarmRemote
                                  Nothing
                                  st)
                              )
                       )
              UnnegotiatedState _ connId _ ->
                return ( UnsupportedState st
                       , Nothing
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (PromotedToWarmRemote
                                  (Just connId)
                                  st)
                              )
                       )
              OutboundUniState connId _connThread _handle ->
                return ( UnsupportedState st
                       , Nothing
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (PromotedToWarmRemote
                                  (Just connId)
                                  st)
                              )
                       )
              OutboundDupState connId connThread handle _expired -> do
                -- @
                --   PromotedToWarm^{Duplex}_{Remote} : OutboundState Duplex
                --                                    → DuplexState
                -- @
                --
                -- For connections that reach DuplexState we are not sure if
                -- this was due to a connection that was established due to TCP
                -- simultaneous open or normal connect/accept. If it was
                -- established due to TCP simultaneous open a DuplexState can
                -- make us go above the Server number of connections hard limit,
                -- hence we need to prune connections.
                --
                let connState' = DuplexState connId connThread handle
                    tr = mkTransition connState connState'

                numberOfConns <- countIncomingConnections state

                -- use 'numberOfConns + 1' because we want to know if we
                -- actually let this connection evolve if we need to make
                -- room for them by pruning.
                let numberToPrune =
                      numberOfConns + 1
                      - fromIntegral
                          (acceptedConnectionsHardLimit cmConnectionsLimits)

                -- Are we above the hard limit?
                if numberToPrune > 0
                then do
                  (pruneSelf, prune)
                    <- mkPruneAction peerAddr numberToPrune state connState' connVar stdGenVar connThread

                  when (not pruneSelf)
                    $ writeTVar connVar connState'

                  return
                    ( OperationSuccess tr
                    , Just prune
                    , Nothing
                    )

                else do
                  writeTVar connVar connState'
                  return ( OperationSuccess tr
                         , Nothing
                         , Nothing
                         )
              OutboundIdleState connId connThread handle dataFlow@Duplex -> do
                -- @
                --   Awake^{Duplex}_{Remote} : OutboundIdleState^\tau Duplex
                --                           → InboundState Duplex
                -- @
                let connState' = InboundState connId connThread handle dataFlow
                    tr = mkTransition connState connState'

                numberOfConns <- countIncomingConnections state

                -- use 'numberOfConns + 1' because we want to know if we
                -- actually let this connection evolve if we need to make
                -- room for it by pruning.
                let numberToPrune =
                      numberOfConns + 1
                      - fromIntegral
                          (acceptedConnectionsHardLimit cmConnectionsLimits)

                -- Are we above the hard limit?
                if numberToPrune > 0
                then do
                  (pruneSelf, prune)
                    <- mkPruneAction peerAddr numberToPrune state connState' connVar stdGenVar connThread
                  when (not pruneSelf)
                     $ writeTVar connVar connState'

                  return
                    ( OperationSuccess (mkTransition connState (TerminatedState Nothing))
                    , Just prune
                    , Nothing
                    )

                else do
                  writeTVar connVar connState'
                  return ( OperationSuccess tr
                         , Nothing
                         , Nothing
                         )
              OutboundIdleState _connId _connThread _handle Unidirectional ->
                return ( UnsupportedState st
                       , Nothing
                       , Nothing
                       )
              InboundIdleState connId connThread handle dataFlow -> do
                -- @
                --   Awake^{dataFlow}_{Remote} : InboundIdleState Duplex
                --                             → InboundState Duplex
                -- @
                let connState' = InboundState connId connThread handle dataFlow
                writeTVar connVar connState'
                return ( OperationSuccess (mkTransition connState connState')
                       , Nothing
                       , Nothing
                       )
              InboundState connId _ _ _ ->
                return ( OperationSuccess (mkTransition connState connState)
                       , Nothing
                       -- already in 'InboundState'?
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (PromotedToWarmRemote
                                  (Just connId)
                                  st)
                              )
                       )
              DuplexState {} ->
                return ( OperationSuccess (mkTransition connState connState)
                       , Nothing
                       , Nothing
                       )
              TerminatingState {} ->
                return ( UnsupportedState TerminatingSt
                       , Nothing
                       , Nothing
                       )
              TerminatedState {} ->
                return ( UnsupportedState TerminatedSt
                       , Nothing
                       , Nothing
                       )

      whenJust mbAssertion $ \tr' -> do
        traceWith tracer tr'
        _ <- evaluate (assert False)
        pure ()

      -- trace transition
      case (result, pruneTr) of
        (OperationSuccess tr, Nothing) -> do
          traceWith trTracer (TransitionTrace peerAddr tr)
          traceCounters stateVar

        (OperationSuccess tr, Just prune) -> do
          traceWith trTracer (TransitionTrace peerAddr tr)
          runPruneAction prune
          traceCounters stateVar

        _ -> return ()
      return (abstractState . fromState <$> result)


    demotedToColdRemoteImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m (OperationResult AbstractState)
    demotedToColdRemoteImpl stateVar peerAddr = do
      (result, mbAssertion) <- atomically $ do
        mbConnVar <- Map.lookup peerAddr <$> readTMVar stateVar
        case mbConnVar of
          Nothing -> return ( UnsupportedState UnknownConnectionSt
                            , Nothing
                            )
          Just MutableConnState { connVar } -> do
            connState <- readTVar connVar
            let st = abstractState (Known connState)
            case connState of
              ReservedOutboundState {} -> do
                return ( UnsupportedState st
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (DemotedToColdRemote
                                  Nothing
                                  st)
                              )
                       )
              UnnegotiatedState _ connId _ ->
                return ( UnsupportedState st
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (DemotedToColdRemote
                                  (Just connId)
                                  st)
                              )
                       )
              OutboundUniState connId _connThread _handle ->
                return ( UnsupportedState st
                       , Just (CM.TrUnexpectedlyFalseAssertion
                                (DemotedToColdRemote
                                  (Just connId)
                                  st)
                              )
                       )
              OutboundDupState _connId _connThread _handle _expired ->
                return ( OperationSuccess (mkTransition connState connState)
                       , Nothing
                       )
              -- one can only enter 'OutboundIdleState' if remote state is
              -- already cold.
              OutboundIdleState _connId _connThread _handle _dataFlow ->
                return ( OperationSuccess (mkTransition connState connState)
                       , Nothing
                       )
              InboundIdleState _connId _connThread _handle _dataFlow ->
                return ( OperationSuccess (mkTransition connState connState)
                       , Nothing
                       )

              -- @
              --   DemotedToCold^{dataFlow}_{Remote}
              --     : InboundState dataFlow
              --     → InboundIdleState^\tau dataFlow
              -- @
              InboundState connId connThread handle dataFlow -> do
                let connState' = InboundIdleState connId connThread handle dataFlow
                writeTVar connVar connState'
                return ( OperationSuccess (mkTransition connState connState')
                       , Nothing
                       )

              -- @
              --   DemotedToCold^{dataFlow}_{Remote}
              --     : DuplexState
              --     → OutboundState^\tau Duplex
              -- @
              DuplexState connId connThread handle -> do
                let connState' = OutboundDupState connId connThread handle Ticking
                writeTVar connVar connState'
                return ( OperationSuccess (mkTransition connState connState')
                       , Nothing
                       )

              TerminatingState {} ->
                return ( TerminatedConnection st
                       , Nothing
                       )
              TerminatedState {} ->
                return ( TerminatedConnection st
                       , Nothing
                       )

      whenJust mbAssertion $ \tr' -> do
        traceWith tracer tr'
        _ <- evaluate (assert False)
        pure ()

      -- trace transition
      case result of
        OperationSuccess tr ->
          traceWith trTracer (TransitionTrace peerAddr tr)
        _ -> return ()

      traceCounters stateVar
      return (abstractState . fromState <$> result)


--
-- Utilities
--

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | Like 'modifyMVar' but strict in @a@ and for 'TMVar's
--
modifyTMVar :: ( MonadEvaluate m
               , MonadMask     m
               , MonadSTM      m
               )
            => StrictTMVar m a
            -> (a -> m (a, b))
            -> m b
modifyTMVar v k =
  mask $ \restore -> do
    a <- atomically (takeTMVar v)
    (a',b) <- restore (k a >>= evaluate)
      `onException`
        atomically (putTMVar v a)
    atomically (putTMVar v a')
    return b

modifyTMVarSTM :: MonadSTM m => StrictTMVar m a -> (a -> STM m (a, b)) -> STM m b
modifyTMVarSTM v k = do
    a <- takeTMVar v
    (a',b) <- k a
    putTMVar v a'
    return b

-- | Like 'modifyMVar' but pure.
--
modifyTMVarPure :: MonadSTM m
                => StrictTMVar m a
                -> (a -> (a, b))
                -> STM m b
modifyTMVarPure v k = do
    a <- takeTMVar v
    let (a', b) = k a
    putTMVar v a'
    return b

--
-- Exceptions
--

-- | Useful to attach 'CallStack' to 'ConnectionManagerError'.
--
withCallStack :: HasCallStack => (CallStack -> a) -> a
withCallStack k = k callStack
