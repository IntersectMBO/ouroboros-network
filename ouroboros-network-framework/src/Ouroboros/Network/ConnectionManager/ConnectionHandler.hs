{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

-- | Implementation of 'ConnectionHandler'
--
module Ouroboros.Network.ConnectionManager.ConnectionHandler
  ( MuxPromise (..)
  , MuxConnectionHandler
  , makeConnectionHandler
  , MuxConnectionManager
  -- * tracing
  , ConnectionTrace (..)
  ) where

import           Control.Exception (SomeAsyncException)
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable (Typeable)

import           Network.Mux hiding (miniProtocolNum)

import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.RethrowPolicy
import           Ouroboros.Network.ConnectionManager.Types

-- | We place an upper limit of `30s` on the time we wait on receiving an SDU.
-- There is no upper bound on the time we wait when waiting for a new SDU.
-- This makes it possible for miniprotocols to use timeouts that are larger
-- than 30s or wait forever.  `30s` for receiving an SDU corresponds to
-- a minimum speed limit of 17kbps.
--
-- ( 8      -- mux header length
-- + 0xffff -- maximum SDU payload
-- )
-- * 8
-- = 524_344 -- maximum bits in an SDU
--
--  524_344 / 30 / 1024 = 17kbps
--
sduTimeout :: DiffTime
sduTimeout = 30


-- | For handshake, we put a limit of `10s` for sending or receiving a single
-- `MuxSDU`.
--
sduHandshakeTimeout :: DiffTime
sduHandshakeTimeout = 10


-- | States of the connection handler thread.
--
-- * 'MuxRunning'      - sucessful Handshake, mux started
-- * 'MuxStopped'      - either mux was gracefully stopped (using 'Mux' or by
--                     'killThread'; the latter is done by
--                     'Ouoroboros.Network.ConnectinoManager.withConnectionManager')
-- * 'MuxPromiseHandshakeClientError'
--                     - the connection handler thread was running client side
--                     of the handshake negotiation, which failed with
--                     'HandshakeException'
-- * 'MuxPromiseHandshakeServerError'
--                     - the conneciton hndler thread was running server side
--                     of the handshake protocol, which faile with
--                     'HandshakeException'
-- * 'MuxPromiseError' - the multiplexer thrown 'MuxError'.
--
data MuxPromise (muxMode :: MuxMode) peerAddr verionNumber bytes m a b where
    MuxRunning
      :: forall muxMode peerAddr versionNumber bytes m a b.
         !(ConnectionId peerAddr)
      -> !(Mux muxMode m)
      -> !(MuxBundle muxMode bytes m a b)
      -> !(Bundle (StrictTVar m ControlMessage))
      -> MuxPromise muxMode peerAddr versionNumber bytes m a b

    MuxStopped
      :: MuxPromise muxMode peerAddr versionNumber bytes m a b

    MuxPromiseHandshakeClientError
     :: HasInitiator muxMode ~ True
     => !(HandshakeException (HandshakeClientProtocolError versionNumber))
     -> MuxPromise muxMode peerAddr versionNumber bytes m a b

    MuxPromiseHandshakeServerError
      :: HasResponder muxMode ~ True
      => !(HandshakeException (RefuseReason versionNumber))
      -> MuxPromise muxMode peerAddr versionNumber bytes m a b

    MuxPromiseError
     :: !SomeException
     -> MuxPromise muxMode peerAddr versionNumber bytes m a b


-- | A predicate which returns 'True' if connection handler thread has stopped running.
--
isConnectionHandlerRunning :: MuxPromise muxMode peerAddr verionNumber bytes m a b -> Bool
isConnectionHandlerRunning muxPromise =
    case muxPromise of
      MuxRunning{}                     -> True
      MuxPromiseHandshakeClientError{} -> False
      MuxPromiseHandshakeServerError{} -> False
      MuxPromiseError{}                -> False
      MuxStopped                       -> False


-- | Type of 'ConnectionHandler' implemented in this module.
--
type MuxConnectionHandler muxMode peerAddr versionNumber bytes m a b =
    ConnectionHandler muxMode
                      (ConnectionTrace versionNumber)
                      peerAddr
                      (MuxPromise muxMode peerAddr versionNumber bytes m a b)
                      m

-- | Type alias for 'ConnectionManager' using 'MuxPromise'.
--
type MuxConnectionManager muxMode socket peerAddr versionNumber bytes m a b =
    ConnectionManager muxMode socket peerAddr
                      (MuxPromise muxMode peerAddr versionNumber bytes m a b) m

-- | To be used as `makeConnectionHandler` field of 'ConnectionManagerArguments'.
--
-- Note: We need to pass `MiniProtocolBundle` what forces us to have two
-- different `ConnectionManager`s: one for `node-to-client` and another for
-- `node-to-node` connections.  But this is ok, as these resources are
-- independent.
--
makeConnectionHandler
    :: forall peerAddr muxMode versionNumber extra m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadFork  m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadTimer m
       , MonadMask  m
       , Ord      versionNumber
       , Typeable versionNumber
       , Show     peerAddr
       , Typeable peerAddr
       )
    => Tracer m (WithMuxBearer (ConnectionId peerAddr) MuxTrace)
    -> SingInitiatorResponderMode muxMode
    -- ^ describe whether this is outbound or inbound connection, and bring
    -- evidence that we can use mux with it.
    -> MiniProtocolBundle muxMode
    -> HandshakeArguments (ConnectionId peerAddr) versionNumber extra m
                          (OuroborosBundle muxMode peerAddr ByteString m a b)
    -> (MuxPromise muxMode peerAddr versionNumber ByteString m a b -> m ())
    -- ^ This method allows to pass control over responders to the server (for
    -- outbound connections), see
    -- 'Ouroboros.Network.ConnectionManager.Server.ControlChannel.newOutboundConnection'.
    -> (ThreadId m, RethrowPolicy)
    -> MuxConnectionHandler muxMode peerAddr versionNumber ByteString m a b
makeConnectionHandler muxTracer singMuxMode
                      miniProtocolBundle
                      handshakeArguments
                      announceOutboundConnection
                      (mainThreadId, rethrowPolicy) =
    ConnectionHandler $
      case singMuxMode of
        SInitiatorMode          -> WithInitiatorMode          outboundConnectionHandler
        SResponderMode          -> WithResponderMode          inboundConnectionHandler
        SInitiatorResponderMode -> WithInitiatorResponderMode outboundConnectionHandler
                                                              inboundConnectionHandler
  where
    outboundConnectionHandler
      :: HasInitiator muxMode ~ True
      => ConnectionHandlerFn (ConnectionTrace versionNumber)
                             peerAddr
                             (MuxPromise muxMode peerAddr versionNumber ByteString m a b)
                             m
    outboundConnectionHandler
        muxPromiseVar
        tracer
        connectionId@ConnectionId { remoteAddress }
        muxBearer =
          Action {
              action       = outboundAction,
              errorHandler = exceptionHandling muxPromiseVar tracer remoteAddress OutboundError
            }
      where
        outboundAction = do
          traceWith tracer ConnectionStart
          hsResult <- runHandshakeClient (muxBearer sduHandshakeTimeout)
                                         connectionId
                                         handshakeArguments
          case hsResult of
            Left !err -> do
              atomically $ writeTVar muxPromiseVar (Promised (MuxPromiseHandshakeClientError err))
              traceWith tracer (ConnectionTraceHandshakeClientError err)
            Right app -> do
              traceWith tracer ConnectionTraceHandshakeSuccess
              !scheduleStopVarBundle
                <- (\a b c -> Bundle (WithHot a) (WithWarm b) (WithEstablished c))
                    <$> newTVarM Continue
                    <*> newTVarM Continue
                    <*> newTVarM Continue
              let muxApp
                    = mkMuxApplicationBundle
                        connectionId
                        (readTVar <$> scheduleStopVarBundle)
                        app
              !mux <- newMux miniProtocolBundle
              let muxPromise =
                    MuxRunning
                      connectionId mux
                      muxApp scheduleStopVarBundle
              atomically $ writeTVar muxPromiseVar (Promised muxPromise)

              -- For outbound connections we need to on demand start receivers.
              -- This is, in a sense, a no man land: the server will not act, as
              -- it's only reacting to inbound connections, and it also does not
              -- belong to initiator (peer-2-peer governor).
              case singMuxMode of
                SInitiatorResponderMode ->
                  announceOutboundConnection muxPromise
                _ -> pure ()

              runMux (WithMuxBearer connectionId `contramap` muxTracer)
                     mux (muxBearer sduTimeout)


    inboundConnectionHandler
      :: HasResponder muxMode ~ True
      => ConnectionHandlerFn (ConnectionTrace versionNumber)
                             peerAddr
                             (MuxPromise muxMode peerAddr versionNumber ByteString m a b)
                             m
    inboundConnectionHandler muxPromiseVar tracer connectionId@ConnectionId { remoteAddress } muxBearer =
          Action {
              action       = inboundAction,
              errorHandler = exceptionHandling muxPromiseVar tracer remoteAddress InboundError
            }
      where
        inboundAction = do
          traceWith tracer ConnectionStart
          hsResult <- runHandshakeServer (muxBearer sduHandshakeTimeout)
                                         connectionId
                                         (\_ _ _ -> Accept) -- we accept all connections
                                         handshakeArguments
          case hsResult of
            Left !err -> do
              atomically $ writeTVar muxPromiseVar (Promised (MuxPromiseHandshakeServerError err))
              traceWith tracer (ConnectionTraceHandshakeServerError err)
            Right app -> do
              traceWith tracer ConnectionTraceHandshakeSuccess
              !scheduleStopVarBundle
                <- (\a b c -> Bundle (WithHot a) (WithWarm b) (WithEstablished c))
                    <$> newTVarM Continue
                    <*> newTVarM Continue
                    <*> newTVarM Continue
              let muxApp
                    = mkMuxApplicationBundle
                        connectionId
                        (readTVar <$> scheduleStopVarBundle)
                        app
              !mux <- newMux miniProtocolBundle
              atomically $ writeTVar muxPromiseVar
                            (Promised
                              (MuxRunning connectionId
                                          mux
                                          muxApp
                                          scheduleStopVarBundle))
              runMux (WithMuxBearer connectionId `contramap` muxTracer)
                         mux (muxBearer sduTimeout)

    -- minimal error handling, just to make adequate changes to
    -- `muxPromiseVar`; Classification of errors is done by
    -- 'withConnectionManager' when the connection handler thread is started..
    exceptionHandling :: forall x.
                         StrictTVar m
                           (Promise
                             (MuxPromise muxMode peerAddr versionNumber ByteString m a b))
                      -> Tracer m (ConnectionTrace versionNumber)
                      -> peerAddr
                      -> ErrorContext
                      -> m x -> m x
    exceptionHandling muxPromiseVar tracer remoteAddress errorContext io =
      -- handle non-async exceptions
      catchJust
        (\e -> case fromException e :: Maybe SomeAsyncException of
                Just _ -> Nothing
                Nothing -> Just e)
        io
        (\e -> do
          atomically (writeTVar muxPromiseVar (Promised (MuxPromiseError e)))
          case runRethrowPolicy rethrowPolicy errorContext e of
            ShutdownNode -> throwTo mainThreadId (ExceptionInHandler remoteAddress e)
                         >> throwM e
            ShutdownPeer -> throwM e)
      -- the default for normal exit and unhandled error is to write
      -- `MusStopped`, but we don't want to override handshake errors.
      `finally` do
        atomically $ do
          st <- readTVar muxPromiseVar
          when (case st of
                  Promised muxPromise -> isConnectionHandlerRunning muxPromise
                  Empty -> True)
            $ writeTVar muxPromiseVar (Promised MuxStopped)
        traceWith tracer ConnectionStopped

      -- if 'MuxError' was thrown by the conneciton handler, let the other side
      -- know.


--
-- Tracing
--


-- | 'ConnectionTrace' is embedded into 'ConnectionManagerTrace' with
-- 'Ouroboros.Network.ConnectionMamanger.Types.ConnectionTrace' constructor.
--
-- TODO: when 'Handshake' will get it's own tracer, independent of 'Mux', it
-- should be embedded into 'ConnectionTrace'.
--
data ConnectionTrace versionNumber =
      ConnectionStart
    | ConnectionTraceHandshakeSuccess
    | ConnectionTraceHandshakeClientError
        !(HandshakeException (HandshakeClientProtocolError versionNumber))
    | ConnectionTraceHandshakeServerError
        !(HandshakeException (RefuseReason versionNumber))
    | ConnectionStopped
  deriving Show
