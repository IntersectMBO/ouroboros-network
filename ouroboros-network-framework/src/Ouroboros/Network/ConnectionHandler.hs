{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

-- | Implementation of 'ConnectionHandler'
--
-- While connection manager responsibility is to keep track of resources:
-- sockets and threads running connection and their state changes (including
-- changes imposed by 'ConnectionHandler', e.g. weather a uni- or duplex- data
-- flow was negotiated), the responsibility of 'ConnectionHandler' is to:
--
-- * run handshake protocol on the underlying bearer
-- * start mux
--
-- 'ConnectionHandler' is run on each inbound or outbound connection and returns
-- 'Handle'.  Upon successful handshake negotiation it returns all the
-- necessary information to run mini-protocols.  Note that it is not responsible
-- for running them: that's what a server does or p2p-governor by means of
-- 'PeerStateActions'.
--
module Ouroboros.Network.ConnectionHandler
  ( Handle (..)
  , HandleWithExpandedCtx
  , HandleWithMinimalCtx
  , HandleError (..)
  , classifyHandleError
  , MuxConnectionHandler
  , makeConnectionHandler
  , MuxConnectionManager
  , ConnectionManagerWithExpandedCtx
    -- * tracing
  , ConnectionHandlerTrace (..)
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (SomeAsyncException)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, contramap, traceWith)

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)

import Network.Mux (Mux)
import Network.Mux qualified as Mx

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context (ExpandedInitiatorContext,
           MinimalInitiatorContext, ResponderContext)
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Mux
import Ouroboros.Network.MuxMode
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Version qualified as Handshake
import Ouroboros.Network.RethrowPolicy


-- | We place an upper limit of `30s` on the time we wait on receiving an SDU.
-- There is no upper bound on the time we wait when waiting for a new SDU.
-- This makes it possible for mini-protocols to use timeouts that are larger
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
-- * 'MuxRunning' - successful Handshake, mux started
-- * 'HandleHandshakeClientError'
--                - the connection handler thread was running client side
--                of the handshake negotiation, which failed with
--                a 'HandshakeException'
-- * 'HandleHandshakeServerError'
--                - the connection handler thread was running server side of the
--                handshake protocol, which fail with 'HandshakeException'
-- * 'HandleError'
--                - the multiplexer thrown 'MuxError'.
--
data Handle (muxMode :: Mx.Mode) initiatorCtx responderCtx versionData bytes m a b =
    Handle {
        hMux            :: !(Mux muxMode m),
        hMuxBundle      :: !(OuroborosBundle muxMode initiatorCtx responderCtx bytes m a b),
        hControlMessage :: !(TemperatureBundle (StrictTVar m ControlMessage)),
        hVersionData    :: !versionData
      }


-- | 'Handle' used by `node-to-node` P2P connections.
--
type HandleWithExpandedCtx muxMode peerAddr versionData bytes m a b =
     Handle    muxMode (ExpandedInitiatorContext peerAddr m)
                       (ResponderContext peerAddr)
                       versionData bytes m a b

-- | 'Handle' used by:
--
-- * `node-to-node` non P2P mode;
-- * `node-to-client` connections.
--
type HandleWithMinimalCtx muxMode peerAddr versionData bytes m a b =
     Handle       muxMode (MinimalInitiatorContext peerAddr)
                          (ResponderContext peerAddr)
                          versionData bytes m a b

data HandleError (muxMode :: Mx.Mode) versionNumber where
    HandleHandshakeClientError
      :: HasInitiator muxMode ~ True
      => !(HandshakeException versionNumber)
      -> HandleError muxMode versionNumber

    HandleHandshakeServerError
      :: HasResponder muxMode ~ True
      => !(HandshakeException versionNumber)
      -> HandleError muxMode versionNumber

    HandleError
     :: !SomeException
     -> HandleError muxMode versionNumber

instance Show versionNumber
      => Show (HandleError muxMode versionNumber) where
    show (HandleHandshakeServerError err) = "HandleHandshakeServerError " ++ show err
    show (HandleHandshakeClientError err) = "HandleHandshakeClientError " ++ show err
    show (HandleError err)                = "HandleError " ++ show err


classifyHandleError :: HandleError muxMode versionNumber
                    -> HandleErrorType
classifyHandleError (HandleHandshakeClientError (HandshakeProtocolLimit _)) =
    HandshakeProtocolViolation
-- TODO: 'HandshakeProtocolError' is not a protocol error! It is just
-- a negotiation failure.  It should be renamed.
classifyHandleError (HandleHandshakeClientError (HandshakeProtocolError _)) =
    HandshakeFailure
classifyHandleError (HandleHandshakeServerError (HandshakeProtocolLimit _)) =
    HandshakeProtocolViolation
classifyHandleError (HandleHandshakeServerError (HandshakeProtocolError _)) =
    HandshakeFailure
-- any other exception, e.g. MuxError \/ IOError, codec errors, etc.
classifyHandleError (HandleError _) =
    HandshakeProtocolViolation


-- | Type of 'ConnectionHandler' implemented in this module.
--
type MuxConnectionHandler muxMode socket initiatorCtx responderCtx peerAddr versionNumber versionData bytes m a b =
    ConnectionHandler muxMode
                      (ConnectionHandlerTrace versionNumber versionData)
                      socket
                      peerAddr
                      (Handle muxMode initiatorCtx responderCtx versionData bytes m a b)
                      (HandleError muxMode versionNumber)
                      versionNumber
                      versionData
                      m

-- | Type alias for 'ConnectionManager' using 'Handle'.
--
type MuxConnectionManager muxMode socket initiatorCtx responderCtx peerAddr versionData versionNumber bytes m a b =
    ConnectionManager muxMode socket peerAddr
                      (Handle muxMode initiatorCtx responderCtx versionData bytes m a b)
                      (HandleError muxMode versionNumber)
                      m

-- | Type alias for 'ConnectionManager' which is using expanded context.
--
type ConnectionManagerWithExpandedCtx muxMode socket peerAddr versionData versionNumber bytes m a b =
    ConnectionManager muxMode socket peerAddr
                      (HandleWithExpandedCtx muxMode peerAddr versionData bytes m a b)
                      (HandleError muxMode versionNumber)
                      m

-- | To be used as `makeConnectionHandler` field of 'ConnectionManagerArguments'.
--
-- Note: We need to pass `MiniProtocolBundle` what forces us to have two
-- different `ConnectionManager`s: one for `node-to-client` and another for
-- `node-to-node` connections.  But this is ok, as these resources are
-- independent.
--
makeConnectionHandler
    :: forall initiatorCtx responderCtx peerAddr muxMode socket versionNumber versionData m a b.
       ( Alternative (STM m)
       , MonadAsync m
       , MonadDelay m
       , MonadFork  m
       , MonadLabelledSTM m
       , MonadThrow (STM m)
       , MonadTimer m
       , MonadMask  m
       , Ord      versionNumber
       , Show     peerAddr
       , Typeable peerAddr
       )
    => Tracer m (Mx.WithBearer (ConnectionId peerAddr) Mx.Trace)
    -> SingMuxMode muxMode
    -> ForkPolicy peerAddr
    -- ^ describe whether this is outbound or inbound connection, and bring
    -- evidence that we can use mux with it.
    -> HandshakeArguments (ConnectionId peerAddr) versionNumber versionData m
    -> Versions versionNumber versionData
                (OuroborosBundle muxMode initiatorCtx responderCtx ByteString m a b)
    -> (ThreadId m, RethrowPolicy)
    -- ^ 'ThreadId' and rethrow policy.  Rethrow policy might throw an async
    -- exception to that thread, when trying to terminate the process.
    -> MuxConnectionHandler muxMode socket initiatorCtx responderCtx peerAddr versionNumber versionData ByteString m a b
makeConnectionHandler muxTracer singMuxMode
                      forkPolicy
                      handshakeArguments
                      versionedApplication
                      (mainThreadId, rethrowPolicy) =
    ConnectionHandler {
        connectionHandler =
          case singMuxMode of
            SingInitiatorMode ->
              WithInitiatorMode outboundConnectionHandler
            SingResponderMode ->
              WithResponderMode inboundConnectionHandler
            SingInitiatorResponderMode ->
              WithInitiatorResponderMode outboundConnectionHandler
                                         inboundConnectionHandler
      }
  where
    -- install classify exception handler
    classifyExceptions :: forall x.
                          Tracer m (ConnectionHandlerTrace versionNumber versionData)
                       -> peerAddr
                       -> ErrorContext
                       -> m x -> m x
    classifyExceptions tracer remoteAddress ctx io =
      -- handle non-async exceptions
      catchJust
        (\e -> case fromException e :: Maybe SomeAsyncException of
                Just _  -> Nothing
                Nothing -> Just e)
        io
        $ \err -> do
          let cmd = runRethrowPolicy rethrowPolicy ctx err
          traceWith tracer (TrConnectionHandlerError ctx err cmd)
          case cmd of
            ShutdownNode -> do
              throwTo mainThreadId (ExceptionInHandler remoteAddress err)
              throwIO err
            ShutdownPeer ->
              throwIO (ExceptionInHandler remoteAddress err)

    outboundConnectionHandler
      :: HasInitiator muxMode ~ True
      => ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode initiatorCtx responderCtx versionData ByteString m a b)
                             (HandleError muxMode versionNumber)
                             versionNumber
                             versionData
                             m
    outboundConnectionHandler versionDataFn
                              socket
                              PromiseWriter { writePromise }
                              tracer
                              connectionId@ConnectionId { localAddress
                                                        , remoteAddress }
                              mkMuxBearer
                              withBuffer
        = MaskedAction { runWithUnmask }
      where
        runWithUnmask :: (forall x. m x -> m x) -> m ()
        runWithUnmask unmask =
          classifyExceptions tracer remoteAddress OutboundError $ do
            labelThisThread (concat ["out-conn-hndlr-"
                                    , show localAddress
                                    , "-"
                                    , show remoteAddress
                                    ])
            handshakeBearer <- mkMuxBearer sduHandshakeTimeout socket Nothing
            hsResult <-
              unmask (runHandshakeClient handshakeBearer
                                         connectionId
                                         handshakeArguments
                                         (Handshake.updateVersionData versionDataFn versionedApplication))
              -- 'runHandshakeClient' only deals with protocol limit errors or
              -- handshake negotiation failures, but not with 'IOException's or
              -- 'MuxError's.
              `catch` \(err :: SomeException) -> do
                atomically $ writePromise (Left (HandleError err))
                throwIO err
            case hsResult of
              Left !err -> do
                atomically $ writePromise (Left (HandleHandshakeClientError err))
                traceWith tracer (TrHandshakeClientError err)

              Right (HandshakeNegotiationResult app versionNumber agreedOptions) ->
                unmask $ do
                  traceWith tracer (TrHandshakeSuccess versionNumber agreedOptions)
                  controlMessageBundle
                    <- (\a b c -> TemperatureBundle (WithHot a) (WithWarm b) (WithEstablished c))
                        <$> newTVarIO Continue
                        <*> newTVarIO Continue
                        <*> newTVarIO Continue
                  mux <- Mx.new (mkMiniProtocolInfos (runForkPolicy forkPolicy remoteAddress) app)
                  let !handle = Handle {
                          hMux            = mux,
                          hMuxBundle      = app,
                          hControlMessage = controlMessageBundle,
                          hVersionData    = agreedOptions
                        }
                  atomically $ writePromise (Right $ HandshakeConnectionResult handle (versionNumber, agreedOptions))
                  withBuffer (\buffer -> do
                      bearer <- mkMuxBearer sduTimeout socket buffer
                      Mx.run (Mx.WithBearer connectionId `contramap` muxTracer)
                             mux bearer
                    )

              Right (HandshakeQueryResult vMap) -> do
                atomically $ writePromise (Right HandshakeConnectionQuery)
                traceWith tracer $ TrHandshakeQuery vMap


    inboundConnectionHandler
      :: HasResponder muxMode ~ True
      => ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode initiatorCtx responderCtx versionData ByteString m a b)
                             (HandleError muxMode versionNumber)
                             versionNumber
                             versionData
                             m
    inboundConnectionHandler updateVersionDataFn
                             socket
                             PromiseWriter { writePromise }
                             tracer
                             connectionId@ConnectionId { localAddress
                                                       , remoteAddress }
                             mkMuxBearer
                             withBuffer
        = MaskedAction { runWithUnmask }
      where
        runWithUnmask :: (forall x. m x -> m x) -> m ()
        runWithUnmask unmask =
          classifyExceptions tracer remoteAddress InboundError $ do
            labelThisThread (concat ["in-conn-hndlr-"
                                    , show localAddress
                                    , "-"
                                    , show remoteAddress
                                    ])
            handshakeBearer <- mkMuxBearer sduHandshakeTimeout socket Nothing
            hsResult <-
              unmask (runHandshakeServer handshakeBearer
                                         connectionId
                                         handshakeArguments
                                         (Handshake.updateVersionData updateVersionDataFn versionedApplication))
              -- 'runHandshakeServer' only deals with protocol limit errors or
              -- handshake negotiation failures, but not with 'IOException's or
              -- 'MuxError's.
              `catch` \(err :: SomeException) -> do
                atomically $ writePromise (Left (HandleError err))
                throwIO err

            case hsResult of
              Left !err -> do
                atomically $ writePromise (Left (HandleHandshakeServerError err))
                traceWith tracer (TrHandshakeServerError err)
              Right (HandshakeNegotiationResult app versionNumber agreedOptions) ->
                unmask $ do
                  traceWith tracer (TrHandshakeSuccess versionNumber agreedOptions)
                  controlMessageBundle
                    <- (\a b c -> TemperatureBundle (WithHot a) (WithWarm b) (WithEstablished c))
                        <$> newTVarIO Continue
                        <*> newTVarIO Continue
                        <*> newTVarIO Continue
                  mux <- Mx.new (mkMiniProtocolInfos (runForkPolicy forkPolicy remoteAddress) app)

                  let !handle = Handle {
                          hMux            = mux,
                          hMuxBundle      = app,
                          hControlMessage = controlMessageBundle,
                          hVersionData    = agreedOptions
                        }
                  atomically $ writePromise (Right $ HandshakeConnectionResult handle (versionNumber, agreedOptions))
                  withBuffer (\buffer -> do
                      bearer <- mkMuxBearer sduTimeout socket buffer
                      Mx.run (Mx.WithBearer connectionId `contramap` muxTracer)
                             mux bearer
                    )
              Right (HandshakeQueryResult vMap) -> do
                atomically $ writePromise (Right HandshakeConnectionQuery)
                traceWith tracer $ TrHandshakeQuery vMap
                -- Wait 20s for client to receive response, who should close the connection.
                threadDelay handshake_QUERY_SHUTDOWN_DELAY



--
-- Tracing
--


-- | 'ConnectionHandlerTrace' is embedded into
-- 'Ouroboros.Network.ConnectionManager.Core.Trace' with
-- 'Ouroboros.Network.ConnectionManager.Types.TrConnectionHandler' constructor.
-- It already includes 'ConnectionId' so we don't need to take care of it here.
--
-- TODO: when 'Handshake' will get its own tracer, independent of 'Mux', it
-- should be embedded into 'ConnectionHandlerTrace'.
--
data ConnectionHandlerTrace versionNumber versionData =
      TrHandshakeSuccess versionNumber versionData
    | TrHandshakeQuery (Map versionNumber (Either Text versionData))
    | TrHandshakeClientError
        (HandshakeException versionNumber)
    | TrHandshakeServerError
        (HandshakeException versionNumber)
    | TrConnectionHandlerError ErrorContext SomeException ErrorCommand
  deriving Show
