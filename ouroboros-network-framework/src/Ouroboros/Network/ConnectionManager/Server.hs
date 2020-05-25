{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Server implementation based on 'ConnectionManager'
--
-- TODO: in the futures this should be moved to `Ouroboros.Network.Server`, but
-- to avoid confusion it will be kept here for now.
--
module Ouroboros.Network.ConnectionManager.Server
  ( ServerArguments (..)
  , run
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  ) where

import           Control.Exception (SomeException)
import           Control.Monad (forever)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (traverse_)
import           Data.Void (Void)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Network.Mux as Mux

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionManager.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Server.ControlChannel (ServerControlChannel)
import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Snocket


data ServerArguments (muxMode :: MuxMode) socket peerAddr versionNumber bytes m a b = ServerArguments {
      serverSockets           :: NonEmpty socket,
      serverSnocket           :: Snocket m socket peerAddr,
      serverTracer            :: Tracer m (ServerTrace peerAddr),
      serverConnectionLimits  :: AcceptedConnectionsLimit,
      serverConnectionManager :: MuxConnectionManager muxMode socket peerAddr
                                                      versionNumber bytes m a b,

      -- | Server control var is passed as an argument; this allows to use
      -- the server to run and manage responders which needs to be started on
      -- inbound connections.
      --
      serverControlChannel    :: ServerControlChannel m muxMode peerAddr
                                                      versionNumber a b
    }


run :: forall muxMode socket peerAddr versionNumber m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadDelay m
       , MonadTime  m
       , Mux.HasResponder muxMode ~ True
       )
    => ServerArguments muxMode socket peerAddr versionNumber ByteString m a b
    -> m Void
run ServerArguments {
      serverSockets,
      serverSnocket,
      serverTracer,
      serverConnectionLimits,
      serverConnectionManager,
      serverControlChannel
    } = do
      let sockets = NonEmpty.toList serverSockets
      localAddresses <- traverse (getLocalAddr serverSnocket) sockets
      traceWith serverTracer (ServerStarted localAddresses)
      -- concurrently run accept loops and the monitoring thread
      runConcurrently
        $ foldr1 (<>)
        $ Concurrently monitoringThread
        : (Concurrently . acceptLoop . accept serverSnocket) `map` sockets
      `finally`
        traceWith serverTracer ServerStopped
  where
    monitoringThread :: m Void
    monitoringThread = forever $ do
      controlMessage <- Server.readControlMessage serverControlChannel
      case controlMessage of
        Server.NewInboundConnection
          (MuxRunning connectionId
                      mux
                      (Bundle
                        (WithHot hotPtls)
                        (WithWarm warmPtls)
                        (WithEstablished establishedPtls))
                      _) -> do
          traceWith serverTracer
            (ServerStartRespondersOnInboundConncetion
              (remoteAddress connectionId))
          traverse_
            (\miniProtocol -> do
              !controlMessageSTM
                <- runResponder mux connectionId Mux.StartEagerly miniProtocol
              Server.writeControlMessage serverControlChannel controlMessageSTM)
            (establishedPtls ++ warmPtls ++ hotPtls)

        Server.NewOutboundConnection
          (MuxRunning connectionId
                      mux
                      (Bundle
                        (WithHot hotPtls)
                        (WithWarm warmPtls)
                        (WithEstablished establishedPtls))
                      _) -> do
          traceWith serverTracer
            (ServerStartRespondersOnOutboundConnection 
              (remoteAddress connectionId))
          traverse_
            (\miniProtocol -> do
              !controlMessageSTM
                <- runResponder mux connectionId Mux.StartOnDemand miniProtocol
              Server.writeControlMessage serverControlChannel controlMessageSTM)
            (establishedPtls ++ warmPtls ++ hotPtls)

        -- There was a problem establishing mux (e.g. a Handshake error or
        -- an IO exception during Handshake negotation).  We don't need to act
        -- on it, the connection manager will close the connection.
        Server.NewInboundConnection _ ->
          pure ()
        Server.NewOutboundConnection _ ->
          pure ()

        Server.MiniProtocolException {} ->
          -- error handling is done in 'ConnectionHandler'
          pure ()

        Server.MiniProtocolCompleted mux connectionId miniProtocol@MiniProtocol { miniProtocolNum } -> do
          traceWith
            serverTracer
            (ServerMiniProtocolRestarted (remoteAddress connectionId) miniProtocolNum)
          controlMessageSTM <- runResponder mux connectionId Mux.StartOnDemand miniProtocol
          Server.writeControlMessage serverControlChannel controlMessageSTM


    runResponder :: Mux.Mux muxMode m
                 -> ConnectionId peerAddr
                 -> Mux.StartOnDemandOrEagerly
                 -> MiniProtocol muxMode ByteString m a b
                 -> m (STM m (Server.ControlMessage muxMode peerAddr versionNumber m a b))
    runResponder mux connectionId
                 startOnDemandOrEagerly
                 miniProtocol@MiniProtocol {
                    miniProtocolNum,
                    miniProtocolRun
                  } =
        (fmap . fmap)
          (\(x :: Either SomeException b) ->
            case x of
             Left err -> Server.MiniProtocolException mux connectionId miniProtocolNum err
             Right _  -> Server.MiniProtocolCompleted mux connectionId miniProtocol)
          $ case miniProtocolRun of
              ResponderProtocolOnly responder ->
                  Mux.runMiniProtocol
                    mux miniProtocolNum
                    Mux.ResponderDirectionOnly
                    startOnDemandOrEagerly
                    -- TODO: eliminate 'fromChannel'
                    (runMuxPeer responder . fromChannel)
              InitiatorAndResponderProtocol _ responder ->
                  Mux.runMiniProtocol
                    mux miniProtocolNum
                    Mux.ResponderDirection
                    startOnDemandOrEagerly
                    (runMuxPeer responder . fromChannel)


    acceptLoop :: Accept m SomeException peerAddr socket
               -> m Void
    acceptLoop acceptOne = do
      runConnectionRateLimits
        (ServerAcceptPolicyTrace `contramap` serverTracer)
        (numberOfConnections serverConnectionManager)
        serverConnectionLimits
      result <- runAccept acceptOne
      case result of
        (AcceptException err, acceptNext) -> do
          traceWith serverTracer (ServerAcceptError err)
          acceptLoop acceptNext
        (Accepted socket peerAddr, acceptNext) -> do
          traceWith serverTracer (ServerAcceptConnection peerAddr)
          !muxPromise <-
            includeInboundConnection
              serverConnectionManager
              socket peerAddr
          Server.writeControlMessage
            serverControlChannel
            (Server.NewInboundConnection <$> muxPromise)
          acceptLoop acceptNext

--
-- Trace
--

data ServerTrace peerAddr
    = ServerAcceptConnection      !peerAddr
    | ServerStartRespondersOnInboundConncetion  !peerAddr
    | ServerStartRespondersOnOutboundConnection !peerAddr
    | ServerAcceptError                     !SomeException
    | ServerAcceptPolicyTrace               !AcceptConnectionsPolicyTrace
    | ServerStarted               ![peerAddr]
    | ServerStopped
    | ServerFatalError            !peerAddr !MiniProtocolNum !SomeException
    | ServerPeerError             !peerAddr !MiniProtocolNum !SomeException
    | ServerError                 !peerAddr !MiniProtocolNum !SomeException
    | ServerMiniProtocolRestarted !peerAddr !MiniProtocolNum
  deriving Show
