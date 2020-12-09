{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
-- Undecidable instances are need for 'Show' instance of 'ConnectionState'.
{-# LANGUAGE UndecidableInstances #-}

-- | The implementation of connection manager.
--
module Ouroboros.Network.ConnectionManager.Core
  ( ConnectionManagerArguments (..)
  , withConnectionManager
  , defaultWaitTimeTimeout
  , defaultProtocolIdleTimeout
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (assert)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTimer
import qualified Control.Monad.Class.MonadSTM as LazySTM
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer, traceWith, contramap)
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Typeable (Typeable)
import           GHC.Stack (CallStack, HasCallStack, callStack)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Network.Mux.Types (MuxMode, MiniProtocolDir (..))
import           Network.Mux.Trace (MuxTrace, WithMuxBearer (..))

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))


-- | Arguments for a 'ConnectionManager' whch are independent of 'MuxMode'.
--
data ConnectionManagerArguments handlerTrace socket peerAddr version m =
    ConnectionManagerArguments {
        -- | Connection manager tracer.
        --
        cmTracer              :: Tracer m (ConnectionManagerTrace peerAddr handlerTrace),

        -- | Mux trace.
        --
        cmMuxTracer           :: Tracer m (WithMuxBearer (ConnectionId peerAddr) MuxTrace),

        -- | @IPv4@ address of the connection manager.  If given, outbound
        -- connections to an @IPv4@ address will bound to it.  To use
        -- bidirectional @TCP@ conections, it must be the same as the server
        -- listening @IPv4@ address.
        --
        cmIPv4Address         :: Maybe peerAddr,

        -- | @IPv6@ address of the connection manager.  If given, outbound
        -- connections to an @IPv6@ address will bound to it.  To use
        -- bidirectional @TCP@ conections, it must be the same as the server
        -- listening @IPv6@ address.
        --
        cmIPv6Address         :: Maybe peerAddr,

        cmAddressType         :: peerAddr -> Maybe AddressType,

        -- | Snocket for the 'socket' type.
        --
        cmSnocket             :: Snocket m socket peerAddr,

        -- | Timeout used to detect inactivity of the protocol; if it expires
        -- and there was no activity detected a connection transitions from
        -- 'WaitRemoteIdleState' to 'TerminatingState'.
        --
        cmProtocolIdleTimeout   :: DiffTime,

        -- | @TCP@ will held connections in @TIME_WAIT@ state for up to two MSL
        -- (maximum segment time).  On Linux this is set to '60' seconds on
        -- other system this might be up to four minutes.
        --
        -- This is configurable, so we can set different value in tests.
        --
        -- When this timeout expires a connection will transition from
        -- 'TerminatingState' to 'TerminatedState'.
        --
        cmWaitTimeTimeout     :: DiffTime,

        -- | @version@ represnts the tuple of @versionNumber@ and
        -- @agreedOptions@.
        --
        connectionDataFlow    :: version -> DataFlow,

        -- | Prune policy
        --
        cmPrunePolicy         :: PrunePolicy peerAddr (STM m),
        cmConnectionsLimits   :: AcceptedConnectionsLimit
      }


-- | 'ConnectionManager' state: for each peer we keep a 'ConnectionState' in
-- a mutable variable, which reduce congestion on the 'TMVar' which keeps
-- 'ConnectionManagerState'.
--
-- It is important we can lookup by remote @peerAddr@; this way we can find if
-- the connection manager is already managing a connection towards that
-- @peerAddr@ and reuse the 'ConnectionState'.
--
type ConnectionManagerState peerAddr handle handleError version m
  = Map peerAddr (StrictTVar m (ConnectionState peerAddr handle handleError version m))



-- | State of a connection.
--
data ConnectionState peerAddr handle handleError version m =
    -- | Each outbound connections starts in this state.
    ReservedOutboundState

    -- | Each inbound connnection starts in this state, outbound connection
    -- reach this state once `connect` call returned.
  | UnnegotiatedState   !Provenance
                        !(ConnectionId peerAddr)
                        !(Async m ())

  | InboundIdleState    !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | InboundState        !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | OutboundState       !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | DuplexState         !(ConnectionId peerAddr) !(Async m ()) !handle
  | TerminatingState    !(ConnectionId peerAddr) !(Async m ()) !(Maybe handleError)
  | TerminatedState                              !(Maybe handleError)


instance ( Show peerAddr
         , Show handleError
         , Show (ThreadId m)
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
             , show (asyncThreadId (Proxy :: Proxy m) connThread)
             ]
    show (InboundIdleState connId connThread _handle df) =
      concat ([ "InboundIdleState "
              , show connId
              , " "
              , show (asyncThreadId (Proxy :: Proxy m) connThread)
              , " "
              , show df
              ])
    show (InboundState  connId connThread _handle df) =
      concat [ "InboundState "
             , show connId
             , " "
             , show (asyncThreadId (Proxy :: Proxy m) connThread)
             , " "
             , show df
             ]
    show (OutboundState connId connThread _handle df) =
      concat [ "OutboundState "
             , show connId
             , " "
             , show (asyncThreadId (Proxy :: Proxy m) connThread)
             , " "
             , show df
             ]
    show (DuplexState   connId connThread _handle) =
      concat [ "DuplexState "
             , show connId
             , " "
             , show (asyncThreadId (Proxy :: Proxy m) connThread)
             ]
    show (TerminatingState connId connThread handleError) =
      concat ([ "TerminatingState "
              , show connId
              , " "
              , show (asyncThreadId (Proxy :: Proxy m) connThread)
              ]
              ++ maybeToList (((' ' :) . show) <$> handleError))
    show (TerminatedState handleError) =
      concat (["TerminatedState"]
              ++ maybeToList (((' ' :) . show) <$> handleError))


getConnThread :: ConnectionState peerAddr handle handleError version m
              -> Maybe (Async m ())
getConnThread ReservedOutboundState                                     = Nothing
getConnThread (UnnegotiatedState _pr   _connId connThread)              = Just connThread
getConnThread (InboundIdleState        _connId connThread _handle _df)  = Just connThread
getConnThread (InboundState            _connId connThread _handle _df)  = Just connThread
getConnThread (OutboundState           _connId connThread _handle _df)  = Just connThread
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
getConnType (InboundIdleState      _connId _connThread _handle df)   = Just (InboundIdleConn df)
getConnType (InboundState          _connId _connThread _handle df)   = Just (NegotiatedConn Inbound df)
getConnType (OutboundState         _connId _connThread _handle df)   = Just (NegotiatedConn Outbound df)
getConnType (DuplexState           _connId _connThread _handle)      = Just DuplexConn
getConnType (TerminatingState      _connId _connThread _handleError) = Nothing
getConnType TerminatedState {}                                       = Nothing


-- | The default value for 'cmWaitTimeTimeout'.
--
defaultWaitTimeTimeout :: DiffTime
defaultWaitTimeTimeout = 60

-- | Inactivity timeout.  It condigures how long to wait since the local side
-- demoted remote peer to /cold/, before closing the connection.
--
defaultProtocolIdleTimeout :: DiffTime
defaultProtocolIdleTimeout = 5


-- | A wedge product
-- <https://hackage.haskell.org/package/smash/docs/Data-Wedge.html#t:Wedge>
--
data Wedge a b =
    Nowhere
  | Here a
  | There b


-- | Instruction used internally in @unregisterOuboundConectionImpl@, e.g. in
-- the implemntation of one of the two  @DemotedToCold^{dataFlow}_{Local}@
-- transitions.
--
data DemoteToColdLocal peerAddr handlerTrace handle handleError version m
    -- | Terminate the connection.
    --
    = TerminateOutboundConnection (ConnectionId peerAddr)
                                  (Async m ())

    -- | Put the connection in 'InboundIdleState'.
    --
    | WaitIdleConnection
        (StrictTVar m (ConnectionState peerAddr handle handleError version m))
        (ConnectionId peerAddr)
        (Async m ())
        handle

    -- | Duplex connection was demoted.
    --
    | DemotedLocalDuplex     (ConnectionId peerAddr)

    -- | Duplex connection was demoted, prune connections.
    --
    | PruneConnections       (ConnectionId peerAddr)
                             (Map peerAddr (Async m ()))

    -- | Demote error.
    | DemoteToColdLocalError (ConnectionManagerTrace peerAddr handlerTrace)
                             InState

    -- | Connection was already in 'TerminatingState' or 'TerminatedState'.
    | DemoteToColdLocalNoop


-- | Entry point for using the connection manager.  This is a classic @with@ style
-- combinator, which cleans resources on exit of the callback (whether cleanly
-- or through an exception).
--
-- Including a connection (either inbound or outbound) is an indepotent
-- operation on connection manager state.  The connection manager will always
-- return the handle that was first to be included in its state.
--
-- Once an inbound connection is passed to the 'ConnectionManager', the manager
-- is responsible for the resource.
--
withConnectionManager
    :: forall (muxMode :: MuxMode) peerAddr socket handlerTrace handle handleError version m a.
       ( Monad              m
       , MonadLabelledSTM   m
       , MonadAsync         m
       , MonadEvaluate      m
       , MonadMask          m
       , MonadTimer         m
       , MonadThrow    (STM m)

       , Ord      peerAddr
       , Show     peerAddr
       , Typeable peerAddr
       )
    => ConnectionManagerArguments handlerTrace socket peerAddr version m
    -> ConnectionHandler muxMode handlerTrace peerAddr handle handleError version m
    -- ^ Callback which runs in a thread dedicated for a given connection.
    -> (handleError -> HandleErrorType)
    -- ^ classify 'handleError's
    -> (ConnectionManager muxMode socket peerAddr handle handleError m -> m a)
    -- ^ Continuation which receives the 'ConnectionManager'.  It must not leak
    -- outside of scope of this callback.  Once it returns all resources
    -- will be closed.
    -> m a
withConnectionManager ConnectionManagerArguments {
                          cmTracer    = tracer,
                          cmMuxTracer = muxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType,
                          cmSnocket,
                          cmWaitTimeTimeout,
                          cmProtocolIdleTimeout,
                          connectionDataFlow,
                          cmPrunePolicy,
                          cmConnectionsLimits
                        }
                      ConnectionHandler {
                          connectionHandler,
                          connectionIdle
                        }
                      classifyHandleError
                      k = do
    (stateVar ::  StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m))
      <- atomically $  do
          v <- newTMVar Map.empty
          labelTMVar v "cm-state"
          return v
    let connectionManager :: ConnectionManager muxMode socket peerAddr
                                               handle handleError m
        connectionManager =
          case connectionHandler of
            WithInitiatorMode outboundHandler ->
              ConnectionManager
                (WithInitiatorMode
                  OutboundConnectionManager {
                      ocmRequestConnection =
                        requestOutboundConnectionImpl stateVar outboundHandler,
                      ocmPromotedToWarmRemote =
                        promotedToWarmRemoteImpl stateVar,
                      ocmUnregisterConnection =
                        unregisterOutboundConnectionImpl stateVar
                    })

            WithResponderMode inboundHandler ->
              ConnectionManager
                (WithResponderMode
                  InboundConnectionManager {
                      icmIncludeConnection =
                        includeInboundConnectionImpl stateVar inboundHandler,
                      icmUnregisterConnection =
                        unregisterInboundConnectionImpl stateVar,
                      icmNumberOfConnections =
                        readTMVar stateVar >>= countConnections
                    })

            WithInitiatorResponderMode outboundHandler inboundHandler ->
              ConnectionManager
                (WithInitiatorResponderMode
                  OutboundConnectionManager {
                      ocmRequestConnection =
                        requestOutboundConnectionImpl stateVar outboundHandler,
                      ocmPromotedToWarmRemote =
                        promotedToWarmRemoteImpl stateVar,
                      ocmUnregisterConnection =
                        unregisterOutboundConnectionImpl stateVar
                    }
                  InboundConnectionManager {
                      icmIncludeConnection =
                        includeInboundConnectionImpl stateVar inboundHandler,
                      icmUnregisterConnection =
                        unregisterInboundConnectionImpl stateVar,
                      icmNumberOfConnections =
                        readTMVar stateVar >>= countConnections
                    })

    k connectionManager
      `finally` do
        traceWith tracer TrShutdown
        state <- atomically $ readTMVar stateVar
        traverse_
          (\connVar -> do
            -- cleanup handler for that thread will close socket associated
            -- with the thread.  We put each connection in 'TerminatedState' to
            -- guarantee, that non of the connection threads will enter
            -- 'TerminatingState' (and thus delay shutdown for 'tcp_WAIT_TIME'
            -- seconds) when receiving the 'AsyncCancelled' exception.
            connState <- atomically $ do
              connState <- readTVar connVar
              writeTVar connVar (TerminatedState Nothing)
              return connState
            traverse_ cancel (getConnThread connState) )
          state
  where
    countConnections :: ConnectionManagerState peerAddr handle handleError version m
                     -> STM m Int
    countConnections state =
        Map.size
      . Map.filter
              (\connState -> case connState of
                ReservedOutboundState          -> False
                UnnegotiatedState Inbound  _ _ -> True
                UnnegotiatedState Outbound _ _ -> False
                InboundIdleState {}            -> True
                InboundState {}                -> True
                OutboundState {}               -> False
                DuplexState {}                 -> True
                TerminatingState {}            -> False
                TerminatedState {}             -> False)
     <$> traverse readTVar state


    -- Start connection thread and run connection handler on it.
    --
    -- TODO: We don't have 'MonadFix' instance for 'IOSim', so we cannot
    -- directly pass 'connVar' (which requires @Async ()@ returned by this
    -- function.  If we had 'MonadFix' at hand We could then also elegantly
    -- eliminate 'PromiseWriter'.
    runConnectionHandler :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
                         -> ConnectionHandlerFn handlerTrace peerAddr handle handleError version m
                         -> socket
                         -> peerAddr
                         -> PromiseWriter m (Either handleError (handle, version))
                         -> m (ConnectionId peerAddr, Async m ())
    runConnectionHandler stateVar handler socket peerAddr writer = do
      localAddress <- getLocalAddr cmSnocket socket
      let connId = ConnectionId { remoteAddress = peerAddr
                                , localAddress
                                }
      let cleanup = do
            traceWith tracer (TrCleanup connId)
            wConnVar <- atomically $ do
              state' <- readTMVar stateVar
              case Map.lookup peerAddr state' of
                Nothing      -> return Nowhere
                Just connVar -> do
                  connState <- readTVar connVar
                  -- return:
                  -- 'There': to reset a connection
                  -- 'Here' : to close a connection, and set WAIT_TIME timer
                  --
                  case connState of
                    ReservedOutboundState -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ There ()
                    UnnegotiatedState {} -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ There ()
                    InboundIdleState {} -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ Here connVar
                    OutboundState {} -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ There ()
                    InboundState {} -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ There ()
                    DuplexState {} -> do
                      writeTVar connVar (TerminatedState Nothing)
                      return $ There ()
                    TerminatingState {} -> do
                      return $ Here connVar
                    TerminatedState {} ->
                      return $ There ()

            case wConnVar of
              Nowhere -> do
                traceWith tracer (TrConnectionExit connId NotFound)
                reset cmSnocket socket
              Here connVar -> do
                close cmSnocket socket
                traceWith tracer (TrConnectionExit connId WaitTime)
                threadDelay cmWaitTimeTimeout
                atomically $ do
                  writeTVar connVar (TerminatedState Nothing)
                  modifyTMVarPure_ stateVar (Map.delete peerAddr)
              There _ -> do
                traceWith tracer (TrConnectionExit connId Reset)
                reset cmSnocket socket
                modifyTMVar_ stateVar (pure . Map.delete peerAddr)

      case
        handler
          writer
          (TrConnectionHandler connId `contramap` tracer)
          connId
          (\bearerTimeout ->
            toBearer
              cmSnocket
              bearerTimeout
              (WithMuxBearer connId `contramap` muxTracer)
              socket) of
        Action action errorHandler -> do
          -- start connection thread
          connThread <-
            mask $ \unmask ->
              async $ do
                labelThisThread "conn-handler"
                errorHandler (unmask action `finally` cleanup)
          return ( connId
                 , connThread
                 )


    includeInboundConnectionImpl
        :: HasCallStack
        => StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> ConnectionHandlerFn handlerTrace peerAddr handle handleError version m
        -> socket
        -- ^ resource to include in the state
        -> peerAddr
        -- ^ remote address used as an identifier of the resource
        -> m (Connected peerAddr handle handleError)
    includeInboundConnectionImpl stateVar
                                 handler
                                 socket
                                 peerAddr = do
        let provenance = Inbound
        traceWith tracer (TrIncludeConnection provenance peerAddr)
        (connVar, connId, connThread, reader)
          <- modifyTMVar stateVar $ \state -> do
              (reader, writer) <- newEmptyPromiseIO
              (connId, connThread)
                <- runConnectionHandler stateVar handler
                                        socket peerAddr writer
              traceWith tracer (TrIncludedConnection provenance connId)

              -- Either 
              -- @
              --   Accepted    : ● → UnnegotiatedState Inbound
              --   Overwritten : ● → UnnegotiatedState Inbound
              -- @
              --
              -- This is subtle part, which needs to handle a near simultanous
              -- open.  We cannot relay on 'ReservedOutboundState' state as
              -- a lock.  It may happen that the `requestOutboudConnection`
              -- will put 'ReservedOutboundState', but before it will call `connect`
              -- the `accept` call will return.  We overwrite the state and
              -- replace the connection state 'TVar' with a fresh one.  Nothing
              -- is blocked on the replaced 'TVar'.
              connVar <-
                atomically $
                  newTVar (UnnegotiatedState provenance connId connThread)
                  >>= \v -> labelTVar v ("conn-state-" ++ show connId) $> v
              return ( Map.insert peerAddr connVar state
                     , (connVar, connId, connThread, reader)
                     )

        res <- atomically $ readPromise reader
        case res of
          Left handleError -> do
            atomically $ do
              writeTVar connVar $
                case classifyHandleError handleError of
                  HandshakeFailure           -> TerminatingState connId connThread
                                                                 (Just handleError)
                  HandshakeProtocolViolation -> TerminatedState  (Just handleError)
              modifyTMVarPure_ stateVar (Map.delete peerAddr)
            return (Disconnected connId (Just handleError))

          Right (handle, version) -> do
            let dataFlow = connectionDataFlow version
            -- TODO: tracing!
            atomically $ do
              connState <- readTVar connVar
              case connState of
                -- Inbound connections cannot be found in this state at this
                -- stage.
                ReservedOutboundState ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                InboundIdleState {} ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- At this stage the inbound connection cannot be in
                -- 'InboundState', it would mean that there was another thread
                -- that included that connection, but this would violate @TCP@
                -- constraints.
                InboundState {} ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- It is impossible to find a connection in 'OutboundState'
                -- since 'requestOutboundConnectionImpl' blocks until
                -- 'InboundState'.  This guarantees that this transactions runs
                -- first.
                OutboundState _connId _connThread _handle _df ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                DuplexState {} ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- The common case.
                UnnegotiatedState {} ->
                  writeTVar connVar (InboundIdleState connId connThread handle
                                    (connectionDataFlow version))

                TerminatingState {} ->
                  writeTVar connVar (InboundState connId connThread handle
                                    (connectionDataFlow version))

                TerminatedState {} ->
                  writeTVar connVar (InboundState connId connThread handle
                                    (connectionDataFlow version))

            -- Note that we don't set a timeout thread here which would perform
            -- @
            --   Commit^{dataFlow}
            --     : InboundIdleState dataFlow
            --     → TerminatingState
            -- @
            -- This is not needed!  When we return from this call, the inbound
            -- protocol governor will monitor the connection.  Once it becomes
            -- idle, it will call 'unregisterInboundConnection' which will
            -- perform the aformentioned @Commit@ transition.

            traceWith tracer (TrNegotiatedConnection provenance connId dataFlow)
            return (Connected connId dataFlow handle)

    unregisterInboundConnectionImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m (OperationResult DemotedToColdRemoteTr)
    unregisterInboundConnectionImpl stateVar peerAddr = do
      traceWith tracer (TrUnregisterConnection Inbound peerAddr)
      (mbThread, result) <- atomically $ do
        state <- readTMVar stateVar
        case Map.lookup peerAddr state of
          Nothing -> pure ( Nothing
                          , UnsupportedState UnknownConnection )
          Just connVar -> do
            connState <- readTVar connVar
            case connState of
              -- In any of the following two states unregistering is not
              -- supported.  'includeInboundConnection' is a synchronous
              -- opeartion which returns only once the connection is
              -- negotiated.
              ReservedOutboundState ->
                return ( Nothing
                       , UnsupportedState InReservedOutboundState )
              UnnegotiatedState {} ->
                return ( Nothing
                       , UnsupportedState InUnnegotiatedState )

              -- @
              --   Commit^{dataFlow} : InboundIdleState dataFlow
              --                     → TerminatingState
              -- @
              --
              -- Note: the 'TrDemotedToColdRemote' is logged by the server.
              InboundIdleState connId connThread _handle _dataFlow -> do
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return ( Just connThread
                       , OperationSuccess CommitTr )

              OutboundState _ _ _ dataFlow ->
                return ( Nothing
                       , UnsupportedState (InOutboundState dataFlow))

              -- @
              --   DemotedToCold^{Unidirectional}_{Remote} : InboundState Unidirectional
              --                                           → TerminatingState
              -- @
              InboundState connId connThread _handle Unidirectional -> do
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return ( Just connThread
                       , OperationSuccess DemotedToColdRemoteTr )

              -- @
              --   DemotedToCold^{Duplex}_{Remote} : InboundState Duplex
              --                                   → TerminatingState
              -- @
              InboundState connId connThread _handle Duplex -> do
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return ( Just connThread
                       , OperationSuccess DemotedToColdRemoteTr ) 

              -- @
              --   DemotedToCold^{Duplex}_{Remote} : DuplexState
              --                                   → OutboundState Duplex
              -- @
              DuplexState connId connThread handle -> do
                writeTVar connVar (OutboundState connId connThread handle Duplex)
                return ( Nothing
                       , OperationSuccess DemotedToColdRemoteDuplexTr )

              TerminatingState _connId _connThread _handleError ->
                return ( Nothing
                       , UnsupportedState InTerminatingState )
              TerminatedState _handleError ->
                return ( Nothing
                       , UnsupportedState InTerminatedState )

      traverse_ cancel mbThread
      return result


    requestOutboundConnectionImpl
        :: HasCallStack
        => StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> ConnectionHandlerFn handlerTrace peerAddr handle handleError version m
        -> peerAddr
        -> m (Connected peerAddr handle handleError)
    requestOutboundConnectionImpl stateVar handler peerAddr = do
        let provenance = Outbound
        traceWith tracer (TrIncludeConnection provenance peerAddr)
        (tr, connVar, eHandleWedge) <- atomically $ do
          state <- readTMVar stateVar
          case Map.lookup peerAddr state of
            Just connVar -> do
              connState <- readTVar connVar
              case connState of
                ReservedOutboundState ->
                  return ( Just (TrConnectionExists provenance peerAddr)
                         , connVar
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                UnnegotiatedState Outbound _connId _connThread -> do
                  return ( Just (TrConnectionExists provenance peerAddr)
                         , connVar
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                InboundIdleState connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   Awake^{Duplex}_{Local} : InboundIdleState Duplex
                  --                          → OutboundState Duplex
                  -- @
                  writeTVar connVar (OutboundState connId connThread handle dataFlow)
                  return ( Just (TrReusedConnection connId)
                         , connVar
                         , Right (Here (Connected connId dataFlow handle))
                         )

                InboundIdleState connId _connThread _handle Unidirectional -> do
                  return ( Just (TrForbiddenConnection connId)
                         , connVar
                         , Left (withCallStack
                                  (ForbiddenConnection connId))
                         )

                UnnegotiatedState Inbound connId _connThread ->
                  -- we must not block inside @modifyTVar stateVar@, we
                  -- return 'There' to indicate that we need to block on
                  -- the connection state.
                  return ( Nothing
                         , connVar
                         , Right (There connId)
                         )

                OutboundState {} -> do
                  return ( Just (TrConnectionExists provenance peerAddr)
                         , connVar
                         , Left (withCallStack
                                  (ConnectionExists provenance peerAddr))
                         )

                InboundState connId _connThread _handle Unidirectional -> do
                  -- the remote side negotiated unidirectional connection, we
                  -- cannot re-use it.
                  return ( Just (TrForbiddenConnection connId)
                         , connVar
                         , Left (withCallStack
                                  (ForbiddenConnection connId))
                         )

                InboundState connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   PromotedToWarm^{Duplex}_{Local} : InboundState Duplex
                  --                                   → DuplexState
                  -- @
                  writeTVar connVar (DuplexState connId connThread handle)
                  return ( Just (TrReusedConnection connId)
                         , connVar
                         , Right (Here (Connected connId dataFlow handle))
                         )

                DuplexState connId _connThread  _handle ->
                  return ( Nothing
                         , connVar
                         , Left (withCallStack
                                  (ImpossibleConnection connId))
                         )

                TerminatingState _connId _connThread _handleError ->
                  -- await for 'TerminatedState' or for removal of the
                  -- connection from the state.
                  retry

                TerminatedState _handleError -> do
                  -- the connection terminated; we can reset 'connVar' and
                  -- start afresh.
                  writeTVar connVar ReservedOutboundState
                  return ( Nothing
                         , connVar
                         , Right Nowhere 
                         )

            Nothing -> do
              connVar <- newTVar ReservedOutboundState
              -- record the @connVar@ in 'ConnectionManagerState' we can use
              -- 'swapTMVar' as we did not use 'takeTMVar' at the begining of
              -- this transaction.  Since we already 'readTMVar', it will not
              -- block.
              _ <- swapTMVar stateVar
                    (Map.insert peerAddr connVar state)
              return ( Nothing
                     , connVar
                     , Right Nowhere
                     )

        traverse_ (traceWith tracer) tr
        case eHandleWedge of
          Left e ->
            throwIO e

          -- connection manager does not have a connection with @peerAddr@.
          Right Nowhere ->
            bracketOnError
              (openToConnect cmSnocket peerAddr)
              (\socket -> do
                  close cmSnocket socket
                  atomically $ do
                    writeTVar connVar (TerminatedState Nothing)
                    modifyTMVarPure_ stateVar (Map.delete peerAddr)
              )
              $ \socket -> do
                (reader, writer) <- newEmptyPromiseIO
                traceWith tracer (TrConnectionNotFound provenance peerAddr)
                addr <-
                  case cmAddressType peerAddr of
                    Nothing -> pure Nothing
                    Just IPv4Address ->
                         traverse_ (bind cmSnocket socket)
                                   cmIPv4Address
                      $> cmIPv4Address
                    Just IPv6Address ->
                         traverse_ (bind cmSnocket socket)
                                   cmIPv6Address
                      $> cmIPv6Address

                --
                -- connect
                --

                traceWith tracer (TrConnect addr peerAddr)
                connect cmSnocket socket peerAddr
                  `catch` \e -> traceWith tracer (TrConnectError addr peerAddr e)
                             -- the handler attached by `bracketOnError` will
                             -- clear the state
                             >> throwIO e

                (connId, connThread)
                  <- runConnectionHandler stateVar handler
                                          socket peerAddr writer
                traceWith tracer (TrIncludedConnection provenance connId)
                res <- atomically (readPromise reader)
                case res of
                  Left handleError -> do
                    modifyTMVar stateVar $ \state -> do
                      -- 'handleError' might be either a handshake negotiation
                      -- a protocol failure (an IO excpetion, a timeout or
                      -- codec failure).  In the first case we should not reset
                      -- the connection as this is not a protocol error.
                      atomically $ writeTVar connVar $
                        case classifyHandleError handleError of
                          HandshakeFailure ->
                            TerminatingState connId connThread
                                            (Just handleError)
                          HandshakeProtocolViolation ->
                            TerminatedState (Just handleError)
                      return ( Map.delete peerAddr state
                             , Disconnected connId (Just handleError)
                             )

                  -- @
                  --  Connected : ReservedOutboundStae
                  --            → UnnegotiatedState Outbound
                  -- @
                  Right (handle, version) -> do
                    let dataFlow = connectionDataFlow version
                    -- We can safely overwrite the state: after successful
                    -- `connect` it's not possible to have a race condition
                    -- with any other inbound thread.  We are also guaranteed
                    -- to have exclusive access as an outbound thread.
                    atomically
                      (writeTVar
                        connVar
                        (OutboundState connId connThread handle dataFlow))
                    traceWith
                      tracer
                      (TrNegotiatedConnection provenance connId dataFlow)
                    return (Connected connId dataFlow handle)

          Right (There connId) -> do
            -- We can only enter the 'There' case if there is an inbound
            -- connection, and we are about to reuse it, but we need to wait
            -- for handshake.
            (tr', connected) <- atomically $ do
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

                InboundIdleState _connId _connThread _handle _dataFlow ->
                  throwSTM
                    (withCallStack (ImpossibleState (remoteAddress connId)))

                InboundState _ _ _ Unidirectional ->
                  throwSTM (withCallStack (ForbiddenConnection connId))

                InboundState _connId connThread handle dataFlow@Duplex -> do
                  -- @
                  --   PromotedToWarm^{Duplex}_{Local} : InboundState Duplex
                  --                                   → DuplexState
                  -- @
                  writeTVar connVar (DuplexState connId connThread handle)
                  return ( TrReusedConnection connId
                         , Connected connId dataFlow handle
                         )

                OutboundState {} ->
                  throwSTM (withCallStack (ConnectionExists provenance connId))
                DuplexState {} ->
                  throwSTM (withCallStack (ConnectionExists provenance connId))

                TerminatingState _connId _connThread handleError ->
                  return ( TrConnectionTerminating provenance connId
                         , Disconnected connId handleError
                         )
                TerminatedState handleError ->
                  return ( TrConnectionTerminated provenance
                                                 (remoteAddress connId)
                         , Disconnected connId handleError
                         )

            traceWith tracer tr'
            return connected

          -- Connection manager has a connection which can be reused.
          Right (Here connected) ->
            return connected


    unregisterOutboundConnectionImpl
        :: StrictTMVar m
            (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m (OperationResult ())
    unregisterOutboundConnectionImpl stateVar peerAddr = do
      traceWith tracer (TrUnregisterConnection Outbound peerAddr)
      demoteToLocal <- atomically $ do
        state <- readTMVar stateVar
        case Map.lookup peerAddr state of
          -- if the connection errored, it will remove itself from the state.
          -- Calling 'unregisterOutboundConnection' is a no-op in this case.
          Nothing -> pure DemoteToColdLocalNoop

          Just connVar -> do
            connState <- readTVar connVar
            case connState of
              -- In any of the following three states unregistering is not
              -- supported.  'requestOutboundConnection' is a synchronous
              -- opeartion which returns only once the connection is
              -- negotiated.
              ReservedOutboundState -> do
                let inState = InReservedOutboundState
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr inState)
                    inState
              UnnegotiatedState {} -> do
                let inState = InUnnegotiatedState
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr inState)
                    inState
              InboundIdleState _connId _connThread _handle dataFlow ->
                assert (dataFlow == Duplex) $
                return DemoteToColdLocalNoop
              InboundState _peerAddr _connThread _handle dataFlow ->
                assert (dataFlow == Duplex) $ do
                let inState = InInboundState dataFlow
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr inState)
                    inState

              OutboundState connId connThread _handle Unidirectional -> do
                --  This triggers an action which will close the socket.
                -- @
                --   DemotedToCold^{Unidirectional}_{Local} : OutboundState Unidirectional
                --                                          → TerminatingState
                -- @
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return (TerminateOutboundConnection connId connThread)

              OutboundState connId connThread handle dataFlow@Duplex -> do
                -- @
                --   DemotedToCold^{Duplex}_{Local} : OutboundState Duplex
                --                                  → InboundIdleState
                -- @
                writeTVar connVar (InboundIdleState connId connThread handle dataFlow)
                return (WaitIdleConnection connVar connId connThread handle)

              DuplexState connId connThread handle -> do
                -- @
                --   DemotedToCold^{Duplex}_{Local} : DuplexState
                --                                  → InboundState Duplex
                -- @
                --
                writeTVar connVar (InboundState connId connThread handle Duplex)

                numberOfConns <- countConnections state
                case numberOfConns
                      - fromIntegral (acceptedConnectionsHardLimit cmConnectionsLimits)
                      of
                  -- We are above the server hard limit, we need to prune
                  -- existing connections.
                  numberToPrune | numberToPrune > 0 -> do
                    -- traverse the state and get only the connection which
                    -- have 'ConnectionType' and are running (have a thread).
                    -- This excludes connections in 'ReservedOutboundState',
                    -- 'TerminatingState' and 'TermintedState'.
                    (choiseMap :: Map peerAddr (ConnectionType, Async m ()))
                      <- flip Map.traverseMaybeWithKey state $ \_peerAddr connVar' ->
                           (\connState' ->
                              -- this expression returns @Maybe (connType, connThread)@;
                              -- 'traverseMabyeWithKey' collects all 'Just' cases.
                              (,) <$> getConnType connState' <*> getConnThread connState')
                       <$> readTVar connVar'

                    pruneSet <-
                      cmPrunePolicy
                        (fmap fst choiseMap)
                        numberToPrune
                    return $
                      PruneConnections connId $
                          fmap snd
                        . Map.restrictKeys choiseMap
                        $ pruneSet

                  _ | otherwise ->
                    return (DemotedLocalDuplex connId)

              TerminatingState _connId _connThread _handleError ->
                return DemoteToColdLocalNoop
              TerminatedState _handleError ->
                return DemoteToColdLocalNoop

      case demoteToLocal of
        TerminateOutboundConnection connId connThread -> do
          traceWith tracer (TrDemotedToColdLocal connId InTerminatingState)
          cancel connThread
          return (OperationSuccess ())

        WaitIdleConnection connVar connId connThread handle -> do
          traceWith tracer (TrWaitIdle connId)
          timeoutVar <- registerDelay cmProtocolIdleTimeout
          atomically $ do
            isInactive <-
                  (connectionIdle handle)
              <|> (LazySTM.readTVar timeoutVar >>= ($> Inactive) . check)
            case isInactive of
              Active InitiatorDir ->
                -- the connection became active in the initiator direction, this
                -- implies that 'requestOutboundConnection' was called, hence we
                -- don't need to update the state.
                return ()

              Active ResponderDir ->
                -- @
                --   Awake^{Duplex}_{Remote} : InboundIdleState Duplex
                --                           → InboundState 
                -- @
                writeTVar connVar (InboundState connId connThread handle Duplex)

              Inactive ->
                -- @
                --   Commit^{Duplex} : InboundIdleState Duplex
                --                   → TerminatingState
                -- @
                writeTVar connVar (TerminatingState connId connThread Nothing)

          return (OperationSuccess ())

        DemotedLocalDuplex connId -> do
          traceWith tracer (TrDemotedToColdLocal connId (InInboundState Duplex))
          return (OperationSuccess ())

        PruneConnections connId pruneMap -> do
          traceWith tracer (TrDemotedToColdLocal connId (InInboundState Duplex))
          traceWith tracer (TrPruneConnections (Map.keys pruneMap))
          traverse_ cancel pruneMap
          return (OperationSuccess ())

        DemoteToColdLocalError tr inState -> do
          traceWith tracer tr
          return (UnsupportedState inState)

        DemoteToColdLocalNoop ->
          return (OperationSuccess ())


    -- 'promotedToWarmRemote' should be idemponent.  It will be called whenver
    -- an established protocol started actively running (though currently we
    -- have only one established mini-protocol, i.e. `keep-alive`).
    promotedToWarmRemoteImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m (OperationResult ())
    promotedToWarmRemoteImpl stateVar peerAddr = do
      result <- atomically $ do
        mbConnVar <- Map.lookup peerAddr <$> readTMVar stateVar
        case mbConnVar of
          Nothing -> return (UnsupportedState UnknownConnection)
          Just connVar -> do
            connState <- readTVar connVar
            case connState of
              ReservedOutboundState {} -> assert False $
                return (UnsupportedState InReservedOutboundState)
              UnnegotiatedState {} -> assert False $
                return (UnsupportedState InUnnegotiatedState)
              InboundIdleState connId connThread handle dataFlow -> do
                writeTVar connVar (InboundState connId connThread handle dataFlow)
                return (OperationSuccess ())
              InboundState {}          ->
                return (OperationSuccess ())
              OutboundState connId connThread handle dataFlow ->
                case dataFlow of
                  Unidirectional -> assert False $
                    return (UnsupportedState (InOutboundState dataFlow))
                  Duplex -> do
                    writeTVar connVar (DuplexState connId connThread handle)
                    return (OperationSuccess ())
              DuplexState {} ->
                return (OperationSuccess ())
              TerminatingState {} ->
                return (UnsupportedState InTerminatingState)
              TerminatedState {} ->
                return (UnsupportedState InTerminatedState)
      return result



--
-- Utils to update stm variables.
--

-- | Like 'modifyMVar_' but strict
--
modifyTMVar_ :: ( MonadSTM  m
                , MonadMask m
                )
             => StrictTMVar m a -> (a -> m a) -> m ()
modifyTMVar_ v io =
    mask $ \unmask -> do
      a <- atomically (takeTMVar v)
      a' <- unmask (io a) `onException` atomically (putTMVar v a)
      atomically (putTMVar v a')


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
    a      <- atomically (takeTMVar v)
    (!a',b) <- restore (k a >>= evaluate) `onException` atomically (putTMVar v a)
    atomically (putTMVar v a')
    return b


-- | Like 'modifyMVar_' but pure.
--
modifyTMVarPure_ :: MonadSTM m
                 => StrictTMVar m a
                 -> (a -> a)
                 -> STM m ()
modifyTMVarPure_ v k = takeTMVar v >>= putTMVar v . k


--
-- Exceptions
--

-- | Useful to attach 'CallStack' to 'ConnectionManagerError'.
--
withCallStack :: HasCallStack => (CallStack -> a) -> a
withCallStack k = k callStack
