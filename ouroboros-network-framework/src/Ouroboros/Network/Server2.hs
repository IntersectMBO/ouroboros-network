{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Server implementation based on 'ConnectionManager'
--
module Ouroboros.Network.Server2
  ( ServerArguments (..)
  , InboundGovernorObservableState (..)
  , newObservableStateVar
  , newObservableStateVarIO
  , newObservableStateVarFromSeed
  -- * Run server
  , run
  -- * PrunePolicy
  , randomPrunePolicy
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  -- * ControlChannel
  , module ControlChannel
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.InboundGovernor
import           Ouroboros.Network.InboundGovernor.ControlChannel
import qualified Ouroboros.Network.InboundGovernor.ControlChannel as ControlChannel
import           Ouroboros.Network.Mux hiding (ControlMessage)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Snocket


--
-- Server API
--


-- | Server static configuration.
--
data ServerArguments (muxMode  :: MuxMode) socket peerAddr versionNumber bytes m a b =
    ServerArguments {
      serverSockets               :: NonEmpty socket,
      serverSnocket               :: Snocket m socket peerAddr,
      serverTracer                :: Tracer m (ServerTrace peerAddr),
      serverInboundGovernorTracer :: Tracer m (InboundGovernorTrace peerAddr),
      serverConnectionLimits      :: AcceptedConnectionsLimit,
      serverConnectionManager     :: MuxConnectionManager muxMode socket peerAddr
                                                        versionNumber bytes m a b,

      -- | Time for which all protocols need to be idle to trigger
      -- 'DemotedToCold' transition.
      --
      serverInboundIdleTimeout    :: DiffTime,

      -- | Server control var is passed as an argument; this allows to use the
      -- server to run and manage responders which needs to be started on
      -- inbound connections.
      --
      serverControlChannel        :: ServerControlChannel muxMode peerAddr bytes m a b,

      -- | Observable mutable s tate.
      --
      serverObservableStateVar    :: StrictTVar m InboundGovernorObservableState
    }


-- | Run the server, which consists of the following components:
--
-- * /inbound governor/, it corresponds to p2p-governor on outbound side
-- * /accept loop(s)/, one per given ip address.  We support up to one ipv4
--   address and up to one ipv6 address, i.e. an ipv6 enabled node will run two
--   accept loops on listening on different addresses with shared /inbound governor/.
--
-- The server can be run in either of two 'MuxMode'-es:
--
-- * 'InitiatorResponderMode'
-- * 'ResponderMode'
--
-- The first one is used in data diffusion for /Node-To-Node protocol/, while the
-- other is useful for running a server for the /Node-To-Client protocol/.
--
run :: forall muxMode socket peerAddr versionNumber m a b.
       ( MonadAsync    m
       , MonadCatch    m
       , MonadThrow   (STM m)
       , MonadTime     m
       , MonadTimer    m
       , HasResponder muxMode ~ True
       , Ord      peerAddr
       )
    => ServerArguments muxMode socket peerAddr versionNumber ByteString m a b
    -> m Void
run ServerArguments {
      serverSockets,
      serverSnocket,
      serverTracer = tracer,
      serverInboundGovernorTracer = inboundGovernorTracer,
      serverConnectionLimits,
      serverInboundIdleTimeout,
      serverConnectionManager,
      serverControlChannel,
      serverObservableStateVar
    } = do
      let sockets = NonEmpty.toList serverSockets
      localAddresses <- traverse (getLocalAddr serverSnocket) sockets
      traceWith tracer (TrServerStarted localAddresses)
      let threads = inboundGovernor inboundGovernorTracer
                                    serverControlChannel
                                    serverInboundIdleTimeout
                                    serverConnectionManager
                                    serverObservableStateVar
                  : [ accept serverSnocket socket >>= acceptLoop
                    | socket <- sockets
                    ]

      raceAll threads
        `finally`
          traceWith tracer TrServerStopped
        `catch`
          \(e :: SomeException) -> do
            case fromException e of
              Just (_ :: AsyncCancelled) -> pure ()
              Nothing -> traceWith tracer (TrServerError e)
            throwIO e
  where
    raceAll :: [m x] -> m x
    raceAll []       = error "raceAll: invariant violation"
    raceAll [t]      = t
    raceAll (t : ts) = either id id <$> race t (raceAll ts)

    acceptLoop :: Accept m socket peerAddr
               -> m Void
    acceptLoop acceptOne = do
      runConnectionRateLimits
        (TrAcceptPolicyTrace `contramap` tracer)
        (numberOfConnections serverConnectionManager)
        serverConnectionLimits
      result <- runAccept acceptOne
      case result of
        (AcceptFailure err, acceptNext) -> do
          traceWith tracer (TrAcceptError err)
          acceptLoop acceptNext
        (Accepted socket peerAddr, acceptNext) -> do
          traceWith tracer (TrAcceptConnection peerAddr)
          -- using withAsync ensures that the thread that includes inbound
          -- connection (which is a blocking operation), is killed when the
          -- server terminates.
          withAsync
            (do
              a <-
                includeInboundConnection
                  serverConnectionManager
                  socket peerAddr
              case a of
                Connected connId dataFlow handle ->
                  atomically $
                    ControlChannel.writeMessage
                      serverControlChannel
                      (ControlChannel.NewConnection Inbound connId dataFlow handle)
                Disconnected {} ->
                  pure ()
            )
            $ \_ -> acceptLoop acceptNext

--
-- Trace
--

data ServerTrace peerAddr
    = TrAcceptConnection            !peerAddr
    | TrAcceptError                 !SomeException
    | TrAcceptPolicyTrace           !AcceptConnectionsPolicyTrace
    | TrServerStarted               ![peerAddr]
    | TrServerStopped
    | TrServerError                 !SomeException
    -- ^ similar to 'TrAcceptConnection' but it is logged once the connection is
    -- handed to inbound connection manager, e.g. after handshake negotiation.
  deriving Show
