{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

-- | The implementation of connection manager's resource managment.
--
module Ouroboros.Network.ConnectionManager.Core
  ( withConnectionManager
  ) where

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (traceWith, contramap)
import           Data.Foldable (traverse_)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Network.Mux.Trace (WithMuxBearer (..))

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Snocket


-- | Internal type to the 'ConnectionManager'; this the state the connection manager
-- keeps per peer.
--
data ConnectionHandle peerAddr socket muxPromise m = ConnectionHandle {
      -- | Socket with a close callback.
      --
      chSocket       :: !socket,

      -- | A uniqe connection identifier.
      --
      chConnectionId :: ConnectionId peerAddr,

      -- | The connection manager shares a muxPromise between inbound and
      -- outbound connections.
      --
      chMuxPromise   :: !(StrictTVar m (Promise muxPromise)),

      -- | Action which stop the connection.
      --
      chThread       :: !(Async m ()),

      -- | Internal state of the 'ConnectionHandle'.  It can be 'Inbound',
      -- 'Outbound' or 'InboundOutbound'.
      --
      chState        :: !ConnectionState
    }


-- | 'ConnectionManager' state: for each peer we keep a 'ConnectionHandle'.
--
-- It is important we can lookup by remote @peerAddr@; this way we can find if
-- the connection manager is already managing a connection towards that
-- @peerAddr@ and reuse the 'ConnectionHandle'.
--
type State peerAddr socket muxPromise m
  = Map peerAddr (ConnectionHandle peerAddr socket muxPromise m)


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
    :: forall muxMode peerAddr socket handlerTrace muxPromise m a.
       ( Monad         m
       -- We use 'MonadFork' to rethrow exceptions in the main thread.
       , MonadFork     m
       , MonadAsync    m
       , MonadEvaluate m
       , MonadMask     m

       , Ord      peerAddr
       )
    => ConnectionManagerArguments muxMode handlerTrace socket peerAddr muxPromise m
    -> (ConnectionManager         muxMode              socket peerAddr muxPromise m -> m a)
    -- ^ Continuation which receives the 'ConnectionManager'.  It must not leak
    -- outside of scope of this callback.  Once it returns all resources
    -- will be closed.
    -> m a
withConnectionManager ConnectionManagerArguments {
                        connectionManagerTracer    = tracer,
                        connectionManagerMuxTracer = muxTracer,
                        connectionManagerIPv4Address,
                        connectionManagerIPv6Address,
                        connectionManagerAddressType,
                        connectionHandler,
                        connectionSnocket
                      } k = do
    stateVar <- newTMVarM (Map.empty :: State peerAddr sockert muxPromise m)
    let connectionManager :: ConnectionManager muxMode socket peerAddr muxPromise m
        connectionManager =
          case connectionHandler of
            ConnectionHandler (WithInitiatorMode outboundHandler) ->
              ConnectionManager
                (WithInitiatorMode
                  (connectAndInclude stateVar outboundHandler))

            ConnectionHandler (WithResponderMode inboundHandler) ->
              ConnectionManager
                (WithResponderMode
                  InboundConnectionManager {
                      icmIncludeConnection =
                        includeConnection stateVar inboundHandler Inbound,
                      icmNumberOfConnections =
                        countConnections stateVar
                    })

            ConnectionHandler (WithInitiatorResponderMode outboundHandler inboundHandler) ->
              ConnectionManager
                (WithInitiatorResponderMode
                  (connectAndInclude stateVar outboundHandler)
                  InboundConnectionManager {
                      icmIncludeConnection =
                        includeConnection stateVar inboundHandler Inbound,
                      icmNumberOfConnections =
                        countConnections stateVar
                    })

    k connectionManager
      `finally` do
        traceWith tracer ShutdownConnectionManager
        state <- atomically $ readTMVar stateVar
        traverse_
          (\ConnectionHandle { chSocket, chThread }
              -> cancel chThread
              >> close connectionSnocket chSocket )
          state
  where
    countConnections :: StrictTMVar m (State peerAddr socket muxPromise m) -> STM m Int
    countConnections stateVar = Map.size <$> readTMVar stateVar

    -- Include a connection in the 'State'; we use this for both inbound and
    -- outbound (via 'connectAndInclude' below) connections.
    --
    -- This operation is idempotent.  If we try to include the connection to the
    -- same peer multiple times, it will also return the already existing handle
    -- and it will close the given one.  Why closing it here, and not by the
    -- caller? This makes it more homogeneus:  the connection mamanger is
    -- responsible for handling all connections weather included or not in
    -- its state.
    includeConnection
        :: StrictTMVar m (State peerAddr socket muxPromise m)
        -> ConnectionHandlerFn handlerTrace peerAddr muxPromise m
        -> ConnectionState
        -- ^ initialt connection state
        -> socket
        -- ^ resource to include in the state
        -> peerAddr
        -- ^ remote address used as an identifier of the resource
        -> m (STM m muxPromise)
    includeConnection stateVar
                      handler
                      connectionState
                      socket
                      peerAddr =
        modifyTMVar stateVar $ \state ->
          case Map.lookup peerAddr state of

            -----------------
            -- New connection
            --

            Nothing -> do

              localAddress <- getLocalAddr connectionSnocket socket
              let connectionId = ConnectionId { remoteAddress = peerAddr
                                              , localAddress
                                              }
              !muxPromise <- newTVarM Empty
              let cleanup =
                    modifyTMVar_ stateVar $ \state' -> do
                      close connectionSnocket socket
                      let ConnectionHandle { chState } =  state' Map.! peerAddr
                      traceWith tracer (ConnectionFinished connectionId chState)
                      pure $ Map.delete peerAddr state'

              case handler
                    muxPromise
                    (ConnectionTrace connectionId `contramap` tracer)
                    connectionId
                    (\bearerTimeout ->
                      toBearer
                        connectionSnocket
                        bearerTimeout
                        (WithMuxBearer connectionId `contramap` muxTracer)
                        socket) of
                Action action errorHandler -> do
                  thread <-
                    mask $ \unmask ->
                      async $ errorHandler (unmask action) `finally` cleanup
                  let conn = ConnectionHandle {
                          chSocket       = socket,
                          chConnectionId = connectionId,
                          chMuxPromise   = muxPromise,
                          chThread       = thread,
                          chState        = connectionState
                        }
                  traceWith tracer (IncludedConnection connectionId connectionState)
                  pure ( Map.insert peerAddr conn state
                       , muxPromiseSTM muxPromise )

            ----------------------
            -- Existing connection
            --

            Just conn@ConnectionHandle { chMuxPromise } -> do
                let conn' = conn { chState = InboundOutbound }
                -- Say go away!  There are two cases:
                --
                -- 1. for inbound connections: this means we've been contacted
                --    twice from the same peer.  We might be using two ports (or
                --    two addresses), and the other end didn't realised they lead
                --    to the same peer.
                -- 2. for outbound connections: we might have tried connect to
                --    the same peer.  This might be the case if the same ip
                --    address gets resolved from two different domain names.
                --
                close connectionSnocket socket

                pure ( Map.update (const (Just conn')) peerAddr state
                     , muxPromiseSTM chMuxPromise )

    connectAndInclude
        :: StrictTMVar m (State peerAddr socket muxPromise m)
        -> ConnectionHandlerFn handlerTrace peerAddr muxPromise m
        -> peerAddr
        -> m (STM m muxPromise)
    connectAndInclude stateVar handler peerAddr = do
        -- Three three stages:
        --
        -- 1. Check if there is a recorded connection, if there is return it.
        -- 2. Otherwise, connect the peer.
        -- 3. Now try to include the existing resource.
        --
        -- In steps 1 and 3 we can hold a lock on `state` as these are non
        -- blocking operations; but is not the case for 2. During 2 the state
        -- could have changed, i.e. the peer might contacted us before we
        -- contacted them.  Simultaneous open will not error on this level
        -- (though it will when running the handshake mini-protocol).
        --
        mbMuxPromise <-
          modifyTMVar stateVar $ \state ->
            case Map.lookup peerAddr state of
              Just conn@ConnectionHandle { chMuxPromise } -> do
                  let conn' = conn  { chState = InboundOutbound }
                  pure ( Map.update (const (Just conn')) peerAddr state
                       , Just chMuxPromise )

              Nothing -> pure (state, Nothing)

        case mbMuxPromise of
          Just muxPromise -> do
            traceWith tracer (ReusedConnection peerAddr InboundOutbound)
            pure (muxPromiseSTM muxPromise)
          Nothing -> do
            socket <- openToConnect connectionSnocket peerAddr
            case connectionManagerAddressType peerAddr of
              Nothing -> pure ()
              Just IPv4Address ->
                traverse_ (bind connectionSnocket socket) connectionManagerIPv4Address
              Just IPv6Address ->
                traverse_ (bind connectionSnocket socket) connectionManagerIPv6Address
            connect connectionSnocket socket peerAddr
            includeConnection stateVar handler
                              Outbound socket peerAddr

    muxPromiseSTM :: StrictTVar m (Promise muxPromise) -> STM m muxPromise
    muxPromiseSTM v = do
      mm <- readTVar v
      case mm of
        Promised muxPromise -> pure muxPromise
        Empty -> retry

--
-- Utils
--


-- | Like 'modifyMVar_' but strict
--
modifyTMVar_ :: ( MonadSTM  m
                , MonadMask m
                )
             => StrictTMVar m a -> (a -> m a) -> m ()
modifyTMVar_ m io =
    mask $ \unmask -> do
      a <- atomically (takeTMVar m)
      a' <- unmask (io a) `onException` atomically (putTMVar m a)
      atomically (putTMVar m a')


-- | Like 'modifyMVar' but strict in @a@ and for 'TMVar's
--
modifyTMVar :: ( MonadEvaluate m
               , MonadMask     m
               , MonadSTM      m
               )
            => StrictTMVar m a
            -> (a -> m (a, b))
            -> m b
modifyTMVar m k =
  mask $ \restore -> do
    a      <- atomically (takeTMVar m)
    (!a',b) <- restore (k a >>= evaluate) `onException` atomically (putTMVar m a)
    atomically (putTMVar m a')
    return b
