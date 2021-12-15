{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}

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
  , HandleError (..)
  , classifyHandleError
  , MuxConnectionHandler
  , makeConnectionHandler
  , MuxConnectionManager
    -- * tracing
  , ConnectionHandlerTrace (..)
  ) where

import           Control.Exception (SomeAsyncException)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable (Typeable)

import           Network.Mux hiding (miniProtocolNum)

import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Mux
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.RethrowPolicy

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
data Handle (muxMode :: MuxMode) peerAddr bytes m a b =
    Handle {
        hMux            :: !(Mux muxMode m),
        hMuxBundle      :: !(MuxBundle muxMode bytes m a b),
        hControlMessage :: !(Bundle (StrictTVar m ControlMessage))
      }


data HandleError (muxMode :: MuxMode) versionNumber where
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
type MuxConnectionHandler muxMode socket peerAddr versionNumber versionData bytes m a b =
    ConnectionHandler muxMode
                      (ConnectionHandlerTrace versionNumber versionData)
                      socket
                      peerAddr
                      (Handle muxMode peerAddr bytes m a b)
                      (HandleError muxMode versionNumber)
                      (versionNumber, versionData)
                      m

-- | Type alias for 'ConnectionManager' using 'Handle'.
--
type MuxConnectionManager muxMode socket peerAddr versionNumber bytes m a b =
    ConnectionManager muxMode socket peerAddr
                      (Handle muxMode peerAddr bytes m a b)
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
    :: forall peerAddr muxMode socket versionNumber versionData m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadFork  m
       , MonadLabelledSTM m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadTimer m
       , MonadMask  m
       , Ord      versionNumber
       , Show     peerAddr
       , Typeable peerAddr
       )
    => Tracer m (WithMuxBearer (ConnectionId peerAddr) MuxTrace)
    -> SingMuxMode muxMode
    -- ^ describe whether this is outbound or inbound connection, and bring
    -- evidence that we can use mux with it.
    -> MiniProtocolBundle muxMode
    -> HandshakeArguments (ConnectionId peerAddr) versionNumber versionData m
    -> Versions versionNumber versionData
                (OuroborosBundle muxMode peerAddr ByteString m a b)
    -> (ThreadId m, RethrowPolicy)
    -- ^ 'ThreadId' and rethrow policy.  Rethrow policy might throw an async
    -- exception to that thread, when trying to terminate the process.
    -> MuxConnectionHandler muxMode socket peerAddr versionNumber versionData ByteString m a b
makeConnectionHandler muxTracer singMuxMode
                      miniProtocolBundle
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
          traceWith tracer (TrError ctx err cmd)
          case cmd of
            ShutdownNode -> do
              throwTo mainThreadId (ExceptionInHandler remoteAddress err)
              throwIO err
            ShutdownPeer ->
              throwIO err

    outboundConnectionHandler
      :: HasInitiator muxMode ~ True
      => ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode peerAddr ByteString m a b)
                             (HandleError muxMode versionNumber)
                             (versionNumber, versionData)
                             m
    outboundConnectionHandler socket
                              PromiseWriter { writePromise }
                              tracer
                              connectionId@ConnectionId { localAddress
                                                        , remoteAddress }
                              mkMuxBearer
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
            handshakeBearer <- mkMuxBearer sduHandshakeTimeout socket
            hsResult <-
              unmask (runHandshakeClient handshakeBearer
                                         connectionId
                                         handshakeArguments
                                         versionedApplication)
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

              Right (app, versionNumber, agreedOptions) ->
                unmask $ do
                  traceWith tracer (TrHandshakeSuccess versionNumber agreedOptions)
                  controlMessageBundle
                    <- (\a b c -> Bundle (WithHot a) (WithWarm b) (WithEstablished c))
                        <$> newTVarIO Continue
                        <*> newTVarIO Continue
                        <*> newTVarIO Continue
                  let muxBundle
                        = mkMuxApplicationBundle
                            connectionId
                            (readTVar <$> controlMessageBundle)
                            app
                  mux <- newMux miniProtocolBundle
                  let !handle = Handle {
                          hMux            = mux,
                          hMuxBundle      = muxBundle,
                          hControlMessage = controlMessageBundle
                        }
                  atomically $ writePromise (Right (handle, (versionNumber, agreedOptions)))
                  bearer <- mkMuxBearer sduTimeout socket
                  runMux (WithMuxBearer connectionId `contramap` muxTracer)
                         mux bearer


    inboundConnectionHandler
      :: HasResponder muxMode ~ True
      => ConnectionHandlerFn (ConnectionHandlerTrace versionNumber versionData)
                             socket
                             peerAddr
                             (Handle muxMode peerAddr ByteString m a b)
                             (HandleError muxMode versionNumber)
                             (versionNumber, versionData)
                             m
    inboundConnectionHandler socket
                             PromiseWriter { writePromise }
                             tracer
                             connectionId@ConnectionId { localAddress
                                                       , remoteAddress }
                             mkMuxBearer
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
            handshakeBearer <- mkMuxBearer sduHandshakeTimeout socket
            hsResult <-
              unmask (runHandshakeServer handshakeBearer
                                         connectionId
                                         handshakeArguments
                                         versionedApplication)
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
              Right (app, versionNumber, agreedOptions) ->
                unmask $ do
                  traceWith tracer (TrHandshakeSuccess versionNumber agreedOptions)
                  controlMessageBundle
                    <- (\a b c -> Bundle (WithHot a) (WithWarm b) (WithEstablished c))
                        <$> newTVarIO Continue
                        <*> newTVarIO Continue
                        <*> newTVarIO Continue
                  let muxBundle
                        = mkMuxApplicationBundle
                            connectionId
                            (readTVar <$> controlMessageBundle)
                            app
                  mux <- newMux miniProtocolBundle
                  let !handle = Handle {
                          hMux            = mux,
                          hMuxBundle      = muxBundle,
                          hControlMessage = controlMessageBundle
                        }
                  atomically $ writePromise (Right (handle, (versionNumber, agreedOptions)))
                  bearer <- mkMuxBearer sduTimeout socket
                  runMux (WithMuxBearer connectionId `contramap` muxTracer)
                             mux bearer



--
-- Tracing
--


-- | 'ConnectionHandlerTrace' is embedded into 'ConnectionManagerTrace' with
-- 'Ouroboros.Network.ConnectionManager.Types.ConnectionHandlerTrace'
-- constructor.  It already includes 'ConnectionId' so we don't need to take
-- care of it here.
--
-- TODO: when 'Handshake' will get its own tracer, independent of 'Mux', it
-- should be embedded into 'ConnectionHandlerTrace'.
--
data ConnectionHandlerTrace versionNumber versionData =
      TrHandshakeSuccess versionNumber versionData
    | TrHandshakeClientError
        (HandshakeException versionNumber)
    | TrHandshakeServerError
        (HandshakeException versionNumber)
    | TrError ErrorContext SomeException ErrorCommand
  deriving Show
