{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Server implementation based on 'ConnectionManager'
--
module Ouroboros.Network.Server2
  ( ServerArguments (..)
  , PublicInboundGovernorState (..)
    -- * Run server
  , with
    -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, contramap, traceWith)

import Data.ByteString.Lazy (ByteString)
import Data.List as List (foldl')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void, absurd)
import GHC.IO.Exception
#if !defined(mingw32_HOST_OS)
import Foreign.C.Error
#endif

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.InformationChannel
           (InboundGovernorInfoChannel)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context (ResponderContext)
import Ouroboros.Network.InboundGovernor
import Ouroboros.Network.Mux
import Ouroboros.Network.Server.RateLimiting
import Ouroboros.Network.Snocket


--
-- Server API
--


-- | Server static configuration.
--
data ServerArguments (muxMode  :: MuxMode) socket initiatorCtx peerAddr versionData versionNumber bytes m a b =
    ServerArguments {
      serverSockets               :: NonEmpty socket,
      serverSnocket               :: Snocket m socket peerAddr,
      serverTracer                :: Tracer m (ServerTrace peerAddr),
      serverTrTracer              :: Tracer m (RemoteTransitionTrace peerAddr),
      serverInboundGovernorTracer :: Tracer m (InboundGovernorTrace peerAddr),
      serverDebugInboundGovernor  :: Tracer m (DebugInboundGovernor peerAddr versionData),
      serverConnectionLimits      :: AcceptedConnectionsLimit,
      serverConnectionManager     :: MuxConnectionManager muxMode socket initiatorCtx (ResponderContext peerAddr)
                                                          peerAddr versionData versionNumber bytes m a b,

      -- | Time for which all protocols need to be idle to trigger
      -- 'DemotedToCold' transition.
      --
      serverInboundIdleTimeout    :: Maybe DiffTime,

      serverConnectionDataFlow    :: versionData -> DataFlow,

      -- | Server control var is passed as an argument; this allows to use the
      -- server to run and manage responders which needs to be started on
      -- inbound connections.
      --
      serverInboundInfoChannel    :: InboundGovernorInfoChannel muxMode initiatorCtx peerAddr versionData
                                                                bytes m a b
    }

-- | Server pauses accepting connections after an 'CONNABORTED' error.
--
server_CONNABORTED_DELAY :: DiffTime
server_CONNABORTED_DELAY = 0.5

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
with :: forall muxMode socket initiatorCtx peerAddr versionData versionNumber m a b x.
       ( Alternative (STM m)
       , MonadAsync    m
       , MonadDelay    m
       , MonadCatch    m
       , MonadEvaluate m
       , MonadLabelledSTM  m
       , MonadMask     m
       , MonadThrow   (STM m)
       , MonadTime     m
       , MonadTimer    m
       , HasResponder muxMode ~ True
       , Ord      peerAddr
       , Show     peerAddr
       )
    => ServerArguments muxMode socket initiatorCtx peerAddr versionData versionNumber ByteString m a b
    -- ^ record which holds all server arguments
    -> (Async m Void -> m (PublicInboundGovernorState peerAddr versionData) -> m x)
    -- ^ a callback which receives a handle to inbound governor thread and can
    -- read `PublicInboundGovernorState`.
    --
    -- Note that as soon as the callback returns, all threads run by the server
    -- will be stopped.
    -> m x
with ServerArguments {
      serverSockets,
      serverSnocket,
      serverTrTracer,
      serverTracer = tracer,
      serverInboundGovernorTracer = inboundGovernorTracer,
      serverDebugInboundGovernor,
      serverConnectionLimits =
        serverLimits@AcceptedConnectionsLimit { acceptedConnectionsHardLimit = hardLimit },
      serverInboundIdleTimeout,
      serverConnectionManager,
      serverConnectionDataFlow,
      serverInboundInfoChannel
    }
    k = do
      let sockets = NonEmpty.toList serverSockets
      localAddresses <- traverse (getLocalAddr serverSnocket) sockets
      traceWith tracer (TrServerStarted localAddresses)
      withInboundGovernor
        InboundGovernorArguments {
          igaTransitionTracer   = serverTrTracer,
          igaTracer             = inboundGovernorTracer,
          igaDebugTracer        = serverDebugInboundGovernor,
          igaConnectionDataFlow = serverConnectionDataFlow,
          igaInfoChannel        = serverInboundInfoChannel,
          igaIdleTimeout        = serverInboundIdleTimeout,
          igaConnectionManager  = serverConnectionManager
        } $ \inboundGovernorThread readPublicInboundState ->
        withAsync (k inboundGovernorThread readPublicInboundState) $ \actionThread -> do
          let acceptLoops :: [m Void]
              acceptLoops =
                          [ (accept serverSnocket socket >>= acceptLoop localAddress)
                              `finally` close serverSnocket socket
                          | (localAddress, socket) <- localAddresses `zip` sockets
                          ]
          -- race all `acceptLoops` with `actionThread` and
          -- `inboundGovernorThread`
          List.foldl' (\as io -> fn <$> as `race` io)
                 (fn <$> actionThread `waitEither` inboundGovernorThread)
                 acceptLoops
            `finally`
              traceWith tracer TrServerStopped
            `catch`
              \(e :: SomeException) -> do
                case fromException e of
                  Just (_ :: AsyncCancelled) -> pure ()
                  Nothing                    -> traceWith tracer (TrServerError e)
                throwIO e
  where
    fn :: Either x Void -> x
    fn (Left x)  = x
    fn (Right v) = absurd v

    iseCONNABORTED :: IOError -> Bool
#if defined(mingw32_HOST_OS)
    -- On Windows the network packet classifies all errors
    -- as OtherError. This means that we're forced to match
    -- on the error string. The text string comes from
    -- the network package's winSockErr.c, and if it ever
    -- changes we must update our text string too.
    iseCONNABORTED (IOError _ _ _ "Software caused connection abort (WSAECONNABORTED)" _ _) = True
    iseCONNABORTED _ = False
#else
    iseCONNABORTED (IOError _ _ _ _ (Just cerrno) _) = eCONNABORTED == Errno cerrno
#if defined(darwin_HOST_OS)
    -- There is a bug in accept for IPv6 sockets. Instead of returning -1
    -- and setting errno to ECONNABORTED an invalid (>= 0) file descriptor
    -- is returned, with the client address left unchanged. The uninitialized
    -- client address causes the network package to throw the user error below.
    iseCONNABORTED (IOError _ UserError _ "Network.Socket.Types.peekSockAddr: address family '0' not supported." _ _) = True
#endif
    iseCONNABORTED _ = False
#endif

    acceptLoop :: peerAddr
               -> Accept m socket peerAddr
               -> m Void
    acceptLoop localAddress acceptOne0 = mask $ \unmask -> do
        labelThisThread ("accept-loop-" ++ show localAddress)
        go unmask acceptOne0
        `catch` \ e -> traceWith tracer (TrServerError e)
                    >> throwIO e
      where
        -- we must guarantee that 'includeInboundConnection' is called,
        -- otherwise we will have a resource leak.
        --
        -- The 'mask' makes sure that exceptions are not delivered once
        -- between accepting a socket and starting thread that runs
        -- 'includeInboundConnection'.
        --
        -- NOTE: when we will make 'includeInboundConnection' a non blocking
        -- (issue #3478) we still need to guarantee the above property.
        --
        go :: (forall y. m y -> m y)
           -> Accept m socket peerAddr
           -> m Void
        go unmask acceptOne = do
          result <- unmask $ do
            runConnectionRateLimits
              (TrAcceptPolicyTrace `contramap` tracer)
              (numberOfConnections serverConnectionManager)
              serverLimits
            runAccept acceptOne

          case result of
            (AcceptFailure err, acceptNext) -> do
              traceWith tracer (TrAcceptError err)
              -- Try to determine if the connection was aborted by the remote end
              -- before we could process the accept, or if it was a resource
              -- exhaustion problem.
              -- NB. This piece of code is fragile and depends on specific
              -- strings/mappings in the network and base libraries.
              case fromException err of
                 Just ioErr ->
                   if iseCONNABORTED ioErr
                      then threadDelay server_CONNABORTED_DELAY
                        >> go unmask acceptNext
                      else throwIO ioErr
                 Nothing -> throwIO err

            (Accepted socket peerAddr, acceptNext) ->
              (do
                  traceWith tracer (TrAcceptConnection peerAddr)
                  async $
                    do a <-
                         unmask
                           (includeInboundConnection
                             serverConnectionManager
                             hardLimit socket peerAddr)
                       case a of
                         Connected {}    -> pure ()
                         Disconnected {} -> do
                           close serverSnocket socket
                           pure ()
                    `onException`
                      close serverSnocket socket
              `onException`
                 close serverSnocket socket
              )
              >> go unmask acceptNext

--
-- Trace
--

data ServerTrace peerAddr
    = TrAcceptConnection            peerAddr
    | TrAcceptError                 SomeException
    | TrAcceptPolicyTrace           AcceptConnectionsPolicyTrace
    | TrServerStarted               [peerAddr]
    | TrServerStopped
    | TrServerError                 SomeException
    -- ^ similar to 'TrAcceptConnection' but it is logged once the connection is
    -- handed to inbound connection manager, e.g. after handshake negotiation.
  deriving Show

