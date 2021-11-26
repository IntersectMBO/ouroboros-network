{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
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
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)
  , RemoteSt (..)
  , RemoteTransition
  , RemoteTransitionTrace
  -- * ControlChannel
  , module ControlChannel
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow hiding (handle)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.List (intercalate)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.IO.Exception
#if !defined(mingw32_HOST_OS)
import           Foreign.C.Error
#endif

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
      serverTrTracer              :: Tracer m (RemoteTransitionTrace peerAddr),
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

      -- | Observable mutable state.
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
    => ServerArguments muxMode socket peerAddr versionNumber ByteString m a b
    -> m Void
run ServerArguments {
      serverSockets,
      serverSnocket,
      serverTrTracer,
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
      labelTVarIO serverObservableStateVar
                  (  "server-observable-state-"
                  ++ intercalate "-" (show <$> localAddresses)
                  )
      traceWith tracer (TrServerStarted localAddresses)
      let threads = (do labelThisThread (  "inbound-governor-"
                                        ++ intercalate "-" (show <$> localAddresses)
                                        )
                        inboundGovernor serverTrTracer
                                        inboundGovernorTracer
                                        serverControlChannel
                                        serverInboundIdleTimeout
                                        serverConnectionManager
                                        serverObservableStateVar)
                    : [ (accept serverSnocket socket >>= acceptLoop localAddress)
                        `finally` close serverSnocket socket
                      | (localAddress, socket) <- localAddresses `zip` sockets
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

    raceAll :: [m x] -> m x
    raceAll []       = error "raceAll: invariant violation"
    raceAll [t]      = t
    raceAll (t : ts) = either id id <$> race t (raceAll ts)

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
        go :: (forall x. m x -> m x)
           -> Accept m socket peerAddr
           -> m Void
        go unmask acceptOne = do
          result <- unmask $ do
            runConnectionRateLimits
              (TrAcceptPolicyTrace `contramap` tracer)
              (numberOfConnections serverConnectionManager)
              serverConnectionLimits
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
                      then threadDelay 0.5 >> go unmask acceptNext
                      else throwIO ioErr
                 Nothing -> throwIO err

            (Accepted socket peerAddr, acceptNext) ->
              (do
                  let hardLimit =
                        acceptedConnectionsHardLimit serverConnectionLimits

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
    = TrAcceptConnection            !peerAddr
    | TrAcceptError                 !SomeException
    | TrAcceptPolicyTrace           !AcceptConnectionsPolicyTrace
    | TrServerStarted               ![peerAddr]
    | TrServerStopped
    | TrServerError                 !SomeException
    -- ^ similar to 'TrAcceptConnection' but it is logged once the connection is
    -- handed to inbound connection manager, e.g. after handshake negotiation.
  deriving Show
