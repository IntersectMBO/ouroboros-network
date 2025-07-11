{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
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
  , MkMuxConnectionHandler (..)
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
import Control.Tracer (Tracer, traceWith)

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Maybe.Strict
import Data.Text (Text)
import Data.Typeable (Typeable)

import Network.Mux (Mux)
import Network.Mux qualified as Mx
import Network.Mux.Trace

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context (ExpandedInitiatorContext,
           MinimalInitiatorContext, ResponderContext)
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.InboundGovernor.State
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

data MkMuxConnectionHandler (muxMode :: Mx.Mode) socket initiatorCtx responderCtx
                            peerAddr versionNumber versionData bytes m a b where
  MuxInitiatorConnectionHandler :: MkMuxConnectionHandler
                                     Mx.InitiatorMode socket initiatorCtx responderCtx
                                     peerAddr versionNumber versionData bytes m a b
  MuxResponderConnectionHandler :: (   StrictTVar m (StrictMaybe ResponderCounters)
                                    -> Tracer m (WithBearer (ConnectionId peerAddr) Trace))
                                -> MkMuxConnectionHandler
                                     Mx.ResponderMode socket initiatorCtx responderCtx
                                     peerAddr versionNumber versionData bytes m a b
  MuxInitiatorResponderConnectionHandler
    :: (versionData -> DataFlow)
    -> (   StrictTVar m (StrictMaybe ResponderCounters)
        -> Tracer m (WithBearer (ConnectionId peerAddr) Trace))
    -> MkMuxConnectionHandler Mx.InitiatorResponderMode socket initiatorCtx responderCtx peerAddr
                              versionNumber versionData bytes m a b

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
-- When a server is running, the inbound governor creates a tracer which is passed here,
-- and the connection handler appends it to the muxer tracer for
-- inbound and (negotiated) outbound duplex connections. This tracer
-- efficiently informs the IG loop of miniprotocol activity.
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
    => Mx.TracersWithBearer (ConnectionId peerAddr) m
    -> ForkPolicy peerAddr
    -> HandshakeArguments (ConnectionId peerAddr) versionNumber versionData m
    -> Versions versionNumber versionData
                (OuroborosBundle muxMode initiatorCtx responderCtx ByteString m a b)
    -> (ThreadId m, RethrowPolicy)
    -- ^ 'ThreadId' and rethrow policy.  Rethrow policy might throw an async
    -- exception to that thread, when trying to terminate the process.
    -> MkMuxConnectionHandler muxMode socket initiatorCtx responderCtx peerAddr versionNumber versionData ByteString m a b
    -> MuxConnectionHandler muxMode socket initiatorCtx responderCtx peerAddr
                            versionNumber versionData ByteString m a b
makeConnectionHandler muxTracers forkPolicy
                      handshakeArguments
                      versionedApplication
                      (mainThreadId, rethrowPolicy) =
  \case
    MuxInitiatorConnectionHandler ->
      ConnectionHandler . WithInitiatorMode
      $ outboundConnectionHandler NotInResponderMode
    MuxResponderConnectionHandler inboundGovernorMuxTracer ->
      ConnectionHandler . WithResponderMode . inboundConnectionHandler $ inboundGovernorMuxTracer
    MuxInitiatorResponderConnectionHandler connectionDataFlow inboundGovernorMuxTracer ->
      ConnectionHandler $ WithInitiatorResponderMode
        (outboundConnectionHandler $ InResponderMode (inboundGovernorMuxTracer, connectionDataFlow))
        (inboundConnectionHandler inboundGovernorMuxTracer)
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
      => InResponderMode muxMode (   StrictTVar m (StrictMaybe ResponderCounters)
                                  -> Tracer m (WithBearer (ConnectionId peerAddr) Trace)
                                 , versionData -> DataFlow)
      -> ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode initiatorCtx responderCtx versionData ByteString m a b)
                             (HandleError muxMode versionNumber)
                             versionNumber
                             versionData
                             m
    outboundConnectionHandler inResponderMode
                              versionDataFn
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

              Right (HandshakeNegotiationResult app versionNumber agreedOptions) -> do
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
                withBuffer \buffer -> do
                  bearer <- mkMuxBearer sduTimeout socket buffer
                  muxTracers' <- case inResponderMode of
                    InResponderMode (inboundGovernorMuxTracer, connectionDataFlow)
                      | Duplex <- connectionDataFlow agreedOptions -> do
                          countersVar <- newTVarIO . SJust $ ResponderCounters 0 0
                          pure $ Mx.tracersWithBearer connectionId muxTracers {
                              Mx.tracer = Mx.tracer muxTracers <> inboundGovernorMuxTracer countersVar
                            }
                    _notResponder ->
                          -- If this is InitiatorOnly, or a server where unidirectional flow was negotiated
                          -- the IG will never be informed of this remote for obvious reasons.
                          pure $ Mx.tracersWithBearer connectionId muxTracers
                  unmask $ Mx.run muxTracers' mux bearer

              Right (HandshakeQueryResult vMap) -> do
                atomically $ writePromise (Right HandshakeConnectionQuery)
                traceWith tracer $ TrHandshakeQuery vMap


    inboundConnectionHandler
      :: HasResponder muxMode ~ True
      => (   StrictTVar m (StrictMaybe ResponderCounters)
          -> Tracer m (WithBearer (ConnectionId peerAddr) Trace))
      -> ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode initiatorCtx responderCtx versionData ByteString m a b)
                             (HandleError muxMode versionNumber)
                             versionNumber
                             versionData
                             m
    inboundConnectionHandler inboundGovernorMuxTracer
                             updateVersionDataFn
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
              Right (HandshakeNegotiationResult app versionNumber agreedOptions) -> do
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
               withBuffer \buffer -> do
                 bearer <- mkMuxBearer sduTimeout socket buffer
                 countersVar <- newTVarIO . SJust $ ResponderCounters 0 0
                 unmask $ Mx.run (Mx.tracersWithBearer connectionId muxTracers {
                               Mx.tracer = Mx.tracer muxTracers <> inboundGovernorMuxTracer countersVar
                             })
                             mux bearer
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
