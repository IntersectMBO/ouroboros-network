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
  ) where

import           Control.Exception (assert)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer, traceWith, contramap)
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           GHC.Stack (CallStack, HasCallStack, callStack)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Network.Mux.Types (MuxMode)
import           Network.Mux.Trace (MuxTrace, WithMuxBearer (..))

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.HasIPAddress
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))


-- | Assumptions \/ arguments for a 'ConnectionManager'.
--
data ConnectionManagerArguments (muxMode :: MuxMode) handlerTrace socket peerAddr ipAddr handle handleError version m =
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

        -- | Snocket for the 'socket' type.
        --
        cmSnocket             :: Snocket m socket peerAddr,

        -- | For 'InitiatorMode' or 'InitiatorAndResponderMode' we need
        -- 'HasIPAddress' record.  'ResponderMode' is only used when running
        -- a local service (node-to-client).  'LocalAddress' does not support
        -- 'HasIPAddress',  while in the other two modes we only use
        -- 'peerAddr ~ SockAddr' which does support 'HasIPAddress' interface.
        --
        cmHasIPAddress        :: WithMuxMode muxMode (HasIPAddress peerAddr ipAddr) (),

        -- | Callback which runs in a thread dedicated for a given connection.
        --
        connectionHandler     :: ConnectionHandler muxMode handlerTrace peerAddr handle handleError version m,

        -- | @version@ represnts the tuple of @versionNumber@ and
        -- @agreedOptions@.
        --
        connectionDataFlow    :: version -> DataFlow,

        -- | Prune policy
        --
        cmPrunePolicy         :: PrunePolicy peerAddr (STM m),
        cmConnectionsLimits   :: AcceptedConnectionsLimit,
        cmClassifyHandleError :: handleError -> HandleErrorType,

        -- | Set of local peers;  For a local peer (either a relay or a core
        -- node) the connection manager will not bind to its local address.  In
        -- case of a system restart a node will be able to immedately reconnect
        -- to its local peers.  Duplex connections could be held in `TIME_WAIT`
        -- state, but since we will use ephemeral ports we avoid this, but also
        -- the node will not use duplex connections with its local relays.
        --
        cmLocalIPs            :: STM m (Set ipAddr)
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
  | UnnegotiatedState !Provenance
                      !(ConnectionId peerAddr)
                      !(Async m ())

  | InboundState      !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | OutboundState     !(ConnectionId peerAddr) !(Async m ()) !handle !DataFlow
  | DuplexState       !(ConnectionId peerAddr) !(Async m ()) !handle
  | TerminatingState  !(ConnectionId peerAddr) !(Async m ()) !(Maybe handleError)
  | TerminatedState                            !(Maybe handleError)


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
getConnType (InboundState          _connId _connThread _handle df)   = Just (NegotiatedConn Inbound df)
getConnType (OutboundState         _connId _connThread _handle df)   = Just (NegotiatedConn Outbound df)
getConnType (DuplexState           _connId _connThread _handle)      = Just DuplexConn
getConnType (TerminatingState      _connId _connThread _handleError) = Nothing
getConnType TerminatedState {}                                       = Nothing


-- | @WAIT_TIME@ delay
--
-- TODO: use system value. 'sys.net.ipv4.tcp_fin_timeout'
--
tcp_WAIT_TIME :: DiffTime
tcp_WAIT_TIME = 60


-- | A wedge product
-- <https://hackage.haskell.org/package/smash/docs/Data-Wedge.html#t:Wedge>
--
data Wedge a b =
    Nowhere
  | Here a
  | There b


-- | Instruction used internally in @unregisterOuboundConectionImpl@, e.g. in
-- the implemntation of one of the two  @DemotedToCold^{dataFlow}_{local}@
-- transitions.
--
data DemoteToColdLocal peerAddr handlerTrace m
    -- | Terminate the connection.
    --
    = TerminateOutboundConnection (ConnectionId peerAddr)
                                  (Async m ())

    -- | Duplex connection was demoted.
    --
    | DemotedLocalDuplex     (ConnectionId peerAddr)

    -- | Duplex connection was demoted, prune connections.
    --
    | PruneConnections       (ConnectionId peerAddr)
                             (Map peerAddr (Async m ()))

    -- | Demote error.
    | DemoteToColdLocalError (ConnectionManagerTrace peerAddr handlerTrace)
                             (ConnectionManagerError peerAddr)

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
    :: forall muxMode peerAddr ipAddr socket handlerTrace handle handleError version m a.
       ( Monad             m
       , MonadAsync        m
       , MonadDelay        m
       , MonadEvaluate     m
       , MonadMask         m
       , MonadThrow   (STM m)

       , Ord      peerAddr
       , Show     peerAddr
       , Typeable peerAddr
       , Ord      ipAddr
       )
    => ConnectionManagerArguments
         muxMode handlerTrace socket peerAddr ipAddr handle handleError version m
    -> (ConnectionManager
         muxMode              socket peerAddr        handle handleError         m
         -> m a)
    -- ^ Continuation which receives the 'ConnectionManager'.  It must not leak
    -- outside of scope of this callback.  Once it returns all resources
    -- will be closed.
    -> m a
withConnectionManager ConnectionManagerArguments {
                        cmTracer    = tracer,
                        cmMuxTracer = muxTracer,
                        cmIPv4Address,
                        cmIPv6Address,
                        cmSnocket,
                        cmHasIPAddress,
                        connectionHandler,
                        connectionDataFlow,
                        cmPrunePolicy,
                        cmConnectionsLimits,
                        cmClassifyHandleError,
                        cmLocalIPs
                      } k = do
    stateVar <-
      newTMVarIO
        (Map.empty
          :: ConnectionManagerState peerAddr handle handleError version m)
    let connectionManager :: ConnectionManager muxMode socket peerAddr
                                               handle handleError m
        connectionManager =
          case connectionHandler of
            ConnectionHandler (WithInitiatorMode outboundHandler) ->
              ConnectionManager
                (WithInitiatorMode
                  OutboundConnectionManager {
                      ocmIncludeConnection =
                        includeOutboundConnectionImpl stateVar outboundHandler,
                      ocmPromotedToWarmRemote =
                        promotedToWarmRemoteImpl stateVar,
                      ocmUnregisterConnection =
                        unregisterOutboundConnectionImpl stateVar
                    })

            ConnectionHandler (WithResponderMode inboundHandler) ->
              ConnectionManager
                (WithResponderMode
                  InboundConnectionManager {
                      icmIncludeConnection =
                        includeInboundConnectionImpl stateVar inboundHandler,
                      icmUnregisterConnection =
                        unregisterInboundConnectionImpl stateVar,
                      icmIsInDuplexState =
                        isInDuplexStateImpl stateVar,
                      icmNumberOfConnections =
                        readTMVar stateVar >>= countConnections
                    })

            ConnectionHandler (WithInitiatorResponderMode outboundHandler inboundHandler) ->
              ConnectionManager
                (WithInitiatorResponderMode
                  OutboundConnectionManager {
                      ocmIncludeConnection =
                        includeOutboundConnectionImpl stateVar outboundHandler,
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
                      icmIsInDuplexState =
                        isInDuplexStateImpl stateVar,
                      icmNumberOfConnections =
                        readTMVar stateVar >>= countConnections
                    })

    k connectionManager
      `finally` do
        traceWith tracer TrShutdown
        state <- atomically $ takeTMVar stateVar
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
              Nowhere ->
                return ()
              Here connVar -> do
                close cmSnocket socket
                threadDelay tcp_WAIT_TIME
                atomically (writeTVar connVar (TerminatedState Nothing))
                modifyTMVar_ stateVar (pure . Map.delete peerAddr)
              There _ -> do
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
                labelThisThread "connection-handler"
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
        (connVar, connId, connThread, reader)
          <- modifyTMVar stateVar $ \state -> do
              (reader, writer) <- newEmptyPromiseIO
              (connId, connThread)
                <- runConnectionHandler stateVar handler
                                        socket peerAddr writer
              traceWith tracer (TrIncludedConnection connId provenance)

              -- Either 
              -- @
              --   Accepted    : ● → UnnegotiatedState Inbound
              --   Overwritten : ● → UnnegotiatedState Inbound
              -- @
              --
              -- This is subtle part, which needs to handle a near simultanous
              -- open.  We cannot relay on 'ReservedOutboundState' state as
              -- a lock.  It may happen that the `includeOutboudConnection`
              -- will put 'ReservedOutboundState', but before it will call `connect`
              -- the `accept` call will return.  We overwrite the state and
              -- replace the connection state 'TVar' with a fresh one.  Nothing
              -- is blocked on the replaced 'TVar'.
              connVar <- newTVarIO (UnnegotiatedState provenance connId connThread)
              return ( Map.insert peerAddr connVar state
                     , (connVar, connId, connThread, reader)
                     )

        res <- atomically $ readPromise reader
        case res of
          Left handleError -> do
            atomically $ do
              writeTVar connVar $
                case cmClassifyHandleError handleError of
                  HandshakeFailure           -> TerminatingState connId connThread
                                                                 (Just handleError)
                  HandshakeProtocolViolation -> TerminatedState  (Just handleError)
              modifyTMVarPure_ stateVar (Map.delete peerAddr)
            return (Disconnected connId (Just handleError))

          Right (handle, version) -> do
            let df = connectionDataFlow version
            -- TODO: tracing!
            atomically $ do
              connState <- readTVar connVar
              case connState of
                -- Inbound connections cannot be found in this state at this
                -- stage.
                ReservedOutboundState ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- At this stage the inbound connection cannot be in
                -- 'InboundState', it would mean that there was another thread
                -- that included that connection, but this would violate @TCP@
                -- constraints.
                InboundState {} ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- It is impossible to find a connection in 'OutboundState'
                -- since 'includeOutboundConnectionImpl' blocks until
                -- 'InboundState'.  This guarantees that this transactions runs
                -- first.
                OutboundState _connId _thread _handle _df ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                DuplexState {} ->
                  throwSTM (withCallStack (ImpossibleState peerAddr))

                -- The common case.
                UnnegotiatedState {} ->
                  writeTVar connVar (InboundState connId connThread handle
                                    (connectionDataFlow version))

                TerminatingState {} ->
                  writeTVar connVar (InboundState connId connThread handle
                                    (connectionDataFlow version))

                TerminatedState {} ->
                  writeTVar connVar (InboundState connId connThread handle
                                    (connectionDataFlow version))

            traceWith tracer (TrNegotiatedConnection connId provenance df)
            return (Connected connId handle)


    unregisterInboundConnectionImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m Bool
    unregisterInboundConnectionImpl stateVar peerAddr = do
      (mbThread, result) <- atomically $ do
        state <- readTMVar stateVar
        case Map.lookup peerAddr state of
          Nothing -> pure ( Nothing, True )
          Just connVar -> do
            connState <- readTVar connVar
            case connState of
              -- In any of the following two states unregistering is not
              -- supported.  'includeInboundConnection' is a synchronous
              -- opeartion which returns only once the connection is
              -- negotiated.
              ReservedOutboundState ->
                throwSTM (withCallStack (ForbiddenOperation peerAddr InReservedOutboundState))
              UnnegotiatedState {} ->
                throwSTM (withCallStack (ForbiddenOperation peerAddr InUnnegotiatedState))
              OutboundState {} ->
                throwSTM (withCallStack (ForbiddenOperation peerAddr InUnnegotiatedState))

              -- @
              --   DemotedToCold^{unidirectional}_{remote} : InboundState Unidirectional
              --                                           → TerminatingState
              -- @
              InboundState connId connThread _handle Unidirectional -> do
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return ( Just connThread
                       , True
                       )

              -- @
              --   DemotedToCold^{duplex}_{remote} : InboundState Duplex
              --                                   → TerminatingState@
              -- @
              InboundState connId connThread _handle Duplex -> do
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return ( Just connThread
                       , True
                       ) 

              -- @
              --   DemotedToCold^{duplex}_{remote} : DuplexState
              --                                   → OutboundState Duplex
              -- @
              DuplexState connId connThread handle -> do
                writeTVar connVar (OutboundState connId connThread handle Duplex)
                return ( Nothing
                       , False
                       )

              TerminatingState _connId _thread _handleError ->
                throwSTM (withCallStack (ForbiddenOperation peerAddr InTerminatingState))

              TerminatedState _handleError ->
                throwSTM (withCallStack (ForbiddenOperation peerAddr InTerminatingState))

      traverse_ cancel mbThread
      return result


    includeOutboundConnectionImpl
        :: HasCallStack
        => StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> ConnectionHandlerFn handlerTrace peerAddr handle handleError version m
        -> peerAddr
        -> m (Connected peerAddr handle handleError)
    includeOutboundConnectionImpl stateVar handler peerAddr = do
        let provenance = Outbound
        (connVar, handleWedge) <-
          modifyTMVar stateVar $ \state ->
            case Map.lookup peerAddr state of
              Just connVar -> do
                (tr, res) <- atomically $ do
                  connState <- readTVar connVar
                  case connState of
                    ReservedOutboundState ->
                      return ( Just (TrConnectionExists peerAddr provenance)
                             , Left (withCallStack
                                      (ConnectionExists peerAddr provenance))
                             )

                    UnnegotiatedState Outbound _connId _connThread -> do
                      return ( Just (TrConnectionExists peerAddr provenance)
                             , Left (withCallStack
                                      (ConnectionExists peerAddr provenance))
                             )

                    UnnegotiatedState Inbound connId _connThread ->
                      -- we must not block inside @modifyTVar stateVar@, we
                      -- return 'There' to indicate that we need to block on
                      -- the connection state.
                      return ( Nothing
                             , Right (There connId)
                             )

                    OutboundState {} -> do
                      return ( Just (TrConnectionExists peerAddr provenance)
                             , Left (withCallStack
                                      (ConnectionExists peerAddr provenance))
                             )

                    InboundState connId _connThread _handle Unidirectional -> do
                      -- the remote side negotiated unidirectional connection, we
                      -- cannot re-use it.
                      return ( Just (TrForbiddenConnection connId)
                             , Left (withCallStack
                                      (ForbiddenConnection connId))
                             )

                    InboundState connId connThread handle Duplex -> do
                      -- @
                      --   PromotedToWarm^{duplex}_{local} : InboundState Duplex
                      --                                   → DuplexState
                      -- @
                      writeTVar connVar (DuplexState connId connThread handle)
                      return ( Just (TrReusedConnection connId)
                             , Right (Here (Connected connId handle))
                             )

                    DuplexState connId _connThread  _handle ->
                      return ( Nothing
                             , Left (withCallStack
                                      (ImpossibleConnection connId))
                             )

                    TerminatingState connId _thread _handleError ->
                      return ( Nothing
                             , Left (withCallStack
                                      (ConnectionTerminating connId))
                             )
                    TerminatedState _handleError -> do
                      -- the connection terminated; we can reset 'connVar' and
                      -- start afresh.
                      writeTVar connVar ReservedOutboundState
                      return ( Nothing
                             , Right Nowhere 
                             )

                traverse_ (traceWith tracer) tr
                case res of
                  Left err -> throwIO err
                  -- in this branch we don't modify the state
                  Right w  ->
                    return ( state
                           , ( connVar, w )
                           )

              Nothing -> do
                connVar <- newTVarIO ReservedOutboundState
                -- record the @connVar@ in 'ConnectionManagerState'
                return ( Map.insert peerAddr
                                    connVar
                                    state
                       , ( connVar, Nowhere )
                       )

        case handleWedge of
          -- connection manager does not have a connection with @peerAddr@.
          Nowhere ->
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
                traceWith tracer (TrConnectionNotFound peerAddr provenance)
                let mbIPAddr =
                      case cmHasIPAddress of
                        WithInitiatorMode hasIPAddress ->
                          let addr = getIPAddress hasIPAddress peerAddr in
                          Just (addr, isIPv6 hasIPAddress addr)
                        WithResponderMode {} -> Nothing

                        WithInitiatorResponderMode hasIPAddress _ ->
                          let addr = getIPAddress hasIPAddress peerAddr in
                          Just (addr, isIPv6 hasIPAddress addr)

                localIPs <- atomically cmLocalIPs
                addr <-
                  -- bind only if `peerAddr` is not a member of 'localIPs'
                  case mbIPAddr of
                    Just (ip, ipv6) | ip `Set.notMember` localIPs ->
                      let addr = if ipv6 then cmIPv6Address
                                         else cmIPv4Address in
                      traverse_ (bind cmSnocket socket) addr
                        $> addr

                    _ -> return Nothing

                -- TODO: with @'MonadFix' m@ instance, we could log the real
                -- address in 'TrConnect' and 'TrConnectError' messages.
                traceWith tracer (TrConnect addr peerAddr)
                connect cmSnocket socket peerAddr
                  `catch` \e -> traceWith tracer (TrConnectError addr peerAddr e)
                             -- the handler attached by `bracketOnError` will
                             -- clear the state
                             >> throwIO e

                (connId, connThread)
                  <- runConnectionHandler stateVar handler
                                          socket peerAddr writer
                traceWith tracer (TrIncludedConnection connId provenance)
                res <- atomically (readPromise reader)
                case res of
                  Left handleError -> do
                    modifyTMVar stateVar $ \state -> do
                      -- 'handleError' might be either a handshake negotiation
                      -- a protocol failure (an IO excpetion, a timeout or
                      -- codec failure).  In the first case we should not reset
                      -- the connection as this is not a protocol error.
                      atomically $ writeTVar connVar $
                        case cmClassifyHandleError handleError of
                          HandshakeFailure           -> TerminatingState connId connThread
                                                                         (Just handleError)
                          HandshakeProtocolViolation -> TerminatedState  (Just handleError)
                      return ( Map.delete peerAddr state
                             , Disconnected connId (Just handleError)
                             )

                  -- @
                  --  Connected : ReservedOutboundStae
                  --            → UnnegotiatedState Outbound
                  -- @
                  Right (handle, version) -> do
                    let df = connectionDataFlow version
                    -- We can safely overwrite the state: after successful
                    -- `connect` it's not possible to have a race condition
                    -- with any other inbound thread.  We are also guaranteed
                    -- to have exclusive access as an outbound thread.
                    atomically (writeTVar connVar (OutboundState connId connThread handle df))
                    traceWith tracer (TrNegotiatedConnection connId provenance df)
                    return (Connected connId handle)

          There connId -> do
            -- We can only enter the 'There' case if there is an inbound
            -- connection, and we are about to reuse it, but we need to wait
            -- for handshake.
            (tr, connected) <- atomically $ do
              connState <- readTVar connVar
              case connState of
                ReservedOutboundState {} ->
                  throwSTM (withCallStack (ImpossibleState (remoteAddress connId)))
                UnnegotiatedState Outbound _ _ ->
                  throwSTM (withCallStack (ConnectionExists connId provenance))

                UnnegotiatedState Inbound _ _ ->
                  -- await for connection negotiation
                  retry

                InboundState _ _ _ Unidirectional ->
                  throwSTM (withCallStack (ForbiddenConnection connId))

                InboundState _connId connThread handle Duplex -> do
                  -- @
                  --   PromotedToWarm^{duplex}_{local} : InboundState Duplex
                  --                                   → DuplexState
                  -- @
                  writeTVar connVar (DuplexState connId connThread handle)
                  return ( TrReusedConnection connId
                         , Connected connId handle
                         )

                OutboundState {} ->
                  throwSTM (withCallStack (ConnectionExists connId provenance))
                DuplexState {} ->
                  throwSTM (withCallStack (ConnectionExists connId provenance))

                TerminatingState _connId _connThread handleError ->
                  return ( TrConnectionTerminating connId provenance
                         , Disconnected connId handleError
                         )
                TerminatedState handleError ->
                  return ( TrConnectionTerminated (remoteAddress connId) provenance
                         , Disconnected connId handleError
                         )

            traceWith tracer tr
            return connected

          -- Connection manager has a connection which can be reused.
          Here connected ->
            return connected


    unregisterOutboundConnectionImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m ()
    unregisterOutboundConnectionImpl stateVar peerAddr = do
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
              -- supported.  'includeOutboundConnection' is a synchronous
              -- opeartion which returns only once the connection is
              -- negotiated.
              ReservedOutboundState ->
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr InReservedOutboundState)
                    (withCallStack (ForbiddenOperation peerAddr InReservedOutboundState))
              UnnegotiatedState {} ->
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr InUnnegotiatedState)
                    (withCallStack (ForbiddenOperation peerAddr InUnnegotiatedState))
              InboundState _peerAddr _thread _handle _dataFlow ->
                return $
                  DemoteToColdLocalError
                    (TrForbiddenOperation peerAddr InInboundState)
                    (withCallStack (ForbiddenOperation peerAddr InInboundState))

              OutboundState connId connThread _handle _df -> do
                --  This triggers an action which will close the socket.
                -- @
                --   DemotedToCold^{unidirectional}_{local} : OutboundState Unidirectional
                --                                          → TerminatingState
                -- @
                -- or
                -- @
                --   DemotedToCold^{duplex}_{local} : OutboundState Duplex
                --                                  → TerminatingState
                -- @
                writeTVar connVar (TerminatingState connId connThread Nothing)
                return (TerminateOutboundConnection connId connThread
                          :: DemoteToColdLocal peerAddr handlerTrace m)

              DuplexState connId connThread handle -> do
                -- @
                --   DemotedToCold^{duplex}_{local} : DuplexState
                --                                  → InboundState Duplex
                -- @
                --
                writeTVar connVar (OutboundState connId connThread handle Duplex)

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

              TerminatingState _connId _thread _handleError -> do
                return DemoteToColdLocalNoop

              TerminatedState _handleError -> do
                return DemoteToColdLocalNoop

      case demoteToLocal of
        TerminateOutboundConnection connId connThread -> do
          traceWith tracer (TrConnectionDemoted connId)
          cancel connThread
        DemotedLocalDuplex connId ->
          traceWith tracer (TrConnectionDemoted connId)
        PruneConnections connId pruneMap -> do
          traceWith tracer (TrConnectionDemoted connId)
          traceWith tracer (TrPruneConnections (Map.keys pruneMap))
          traverse_ cancel pruneMap
        DemoteToColdLocalError tr err -> do
          traceWith tracer tr
          throwIO err
        DemoteToColdLocalNoop ->
          return ()


    -- 'promotedToWarmRemote' should be idemponent.  It will be called whenver
    -- an established protocol started actively running (though currently we
    -- have only one established mini-protocol, i.e. `keep-alive`).
    promotedToWarmRemoteImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> m ()
    promotedToWarmRemoteImpl stateVar peerAddr =
      atomically $ do
        mbConnVar <- Map.lookup peerAddr <$> readTMVar stateVar
        case mbConnVar of
          Nothing -> pure ()
          Just connVar ->
            modifyTVar connVar $ \connState -> 
              case connState of
                ReservedOutboundState {} -> assert False $ connState
                UnnegotiatedState {}     -> assert False $ connState

                InboundState {}          ->                connState
                OutboundState connId connThread handle df ->
                  case df of
                    Unidirectional       -> assert False $ connState
                    Duplex               ->                DuplexState connId connThread handle
                DuplexState {}           ->                connState

                TerminatingState {}      -> assert False $ connState
                TerminatedState {}       -> assert False $ connState


    isInDuplexStateImpl
        :: StrictTMVar m (ConnectionManagerState peerAddr handle handleError version m)
        -> peerAddr
        -> STM m (IsInDuplexState m)
    isInDuplexStateImpl stateVar peerAddr = do
      state <- readTMVar stateVar
      case Map.lookup peerAddr state of

        Nothing ->
          throwSTM (withCallStack (UnknownPeer peerAddr))

        Just connVar -> do
          connState <- readTVar connVar
          case connState of
            -- All four states 'ReservedOutboundState', 'UnnegotiatedState'
            -- (with any provenance) and 'OutboundStates' are forbidden.
            ReservedOutboundState ->
              throwSTM (withCallStack (ForbiddenOperation peerAddr InReservedOutboundState))
            UnnegotiatedState {} ->
              throwSTM (withCallStack (ForbiddenOperation peerAddr InUnnegotiatedState))
            OutboundState {} ->
              throwSTM (withCallStack (ForbiddenOperation peerAddr InOutboundState))

            DuplexState {} ->
              return InDuplexState

            InboundState _ _ _ Unidirectional -> 
              -- duplex state will never be reached, 'DataFlow' is a property of
              -- a connection.
              pure (AwaitForDuplexState retry)

            InboundState _ _ _ Duplex ->
              pure (AwaitForDuplexState $ do
                      connState' <- readTVar connVar
                      case connState' of
                        DuplexState {}      -> return True
                        TerminatingState {} -> return False
                        TerminatedState {}  -> return False
                        _                   -> retry
                   )

            TerminatingState connId _thread _handleError ->
              throwSTM (withCallStack (ConnectionTerminating connId))
            TerminatedState _handleError ->
              throwSTM (withCallStack (ConnectionTerminated peerAddr))


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
