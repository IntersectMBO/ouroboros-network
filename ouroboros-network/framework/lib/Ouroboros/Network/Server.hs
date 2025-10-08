{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- 'runResponder' is using a redundant constraint.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Server implementation based on 'ConnectionManager'
--
-- This module should be imported qualified.
--
module Ouroboros.Network.Server
  ( Arguments (..)
    -- * Run server
  , with
    -- * Trace
  , Trace (..)
  , AcceptConnectionsPolicyTrace (..)
  , InboundGovernor.RemoteSt (..)
  , InboundGovernor.RemoteTransition
  , InboundGovernor.RemoteTransitionTrace
  , isECONNABORTED
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)

import Control.Tracer (Tracer, contramap, traceWith)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isNothing)
import Data.Void (Void, absurd)
import GHC.IO.Exception
#if !defined(mingw32_HOST_OS)
import Foreign.C.Error
#endif

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.InboundGovernor qualified as InboundGovernor
import Ouroboros.Network.Mux
import Ouroboros.Network.Server.RateLimiting
import Ouroboros.Network.Snocket


--
-- Server API
--


-- | Server static configuration.
--
data Arguments muxMode socket peerAddr initiatorCtx responderCtx handle handlerTrace handleError versionNumber versionData bytes m a b x =
    Arguments {
      sockets               :: NonEmpty socket,
      snocket               :: Snocket m socket peerAddr,
      tracer                :: Tracer m (Trace peerAddr),
      connectionLimits      :: AcceptedConnectionsLimit,
      inboundGovernorArgs   :: InboundGovernor.Arguments muxMode handlerTrace socket peerAddr initiatorCtx responderCtx handle handleError versionNumber versionData bytes m a b x
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
with :: forall muxMode socket peerAddr initiatorCtx responderCtx handle handlerTrace handleError versionNumber versionData bytes m a b x.
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
       , MonadTraceSTM m
       , MonadFork m
       , MonadFix m
       )
    => Arguments muxMode socket peerAddr initiatorCtx responderCtx handle handlerTrace
                 handleError versionNumber versionData bytes m a b x
    -- ^ record which holds all server arguments
    -> (   Async m Void
        -> m (InboundGovernor.PublicState peerAddr versionData)
        -> ConnectionManager
              muxMode socket peerAddr handle handleError m
        -> m x)
    -- ^ a callback which receives a handle to inbound governor thread and can
    -- read `PublicState`.
    --
    -- Note that as soon as the callback returns, all threads run by the server
    -- will be stopped.
    -> m x
with Arguments {
      sockets = socks,
      snocket,
      tracer,
      connectionLimits =
        limits@AcceptedConnectionsLimit { acceptedConnectionsHardLimit = hardLimit },
      inboundGovernorArgs
    }
    k
    = do
      let sockets = NonEmpty.toList socks
      localAddresses <- traverse (getLocalAddr snocket) sockets
      InboundGovernor.with inboundGovernorArgs
        \inboundGovernorThread readPublicInboundState connectionManager ->
          withAsync do
            labelThisThread "Server2 (ouroboros-network-framework)"
            k inboundGovernorThread readPublicInboundState connectionManager
          \actionThread -> do
            traceWith tracer (TrServerStarted localAddresses)
            let acceptLoops :: [m Void]
                acceptLoops =
                            [ (do
                                  labelThisThread ("accept " ++ show localAddress)
                                  accept snocket socket >>= acceptLoop localAddress connectionManager)
                                `finally` close snocket socket
                            | (localAddress, socket) <- localAddresses `zip` sockets
                            ]
            -- race all `acceptLoops` with `actionThread` and
            -- `inboundGovernorThread`
            let waiter = fn <$> (do
                                    labelThisThread "racing-action-inbound-governor"
                                    actionThread `waitEither` inboundGovernorThread)

            (fn <$> waiter `race` (labelThisThread "racing-accept-loops" >> raceAll acceptLoops))
              `finally`
                traceWith tracer TrServerStopped
              `catch`
                \(e :: SomeException) -> do
                  when (isNothing $ fromException @SomeAsyncException e) $
                    traceWith tracer (TrServerError e)
                  throwIO e
  where
    fn :: Either x Void -> x
    fn (Left x)  = x
    fn (Right v) = absurd v

    raceAll asyncs = withAsyncAll asyncs (fmap snd . waitAny)

    withAsyncAll xs0 action = go [] xs0
      where
        go as []     = action (reverse as)
        go as (x:xs) = withAsync x (\a -> go (a:as) xs)

    acceptLoop :: peerAddr
               -> ConnectionManager muxMode socket peerAddr handle handleError m
               -> Accept m socket peerAddr
               -> m Void
    acceptLoop localAddress connectionManager acceptOne0 = mask $ \unmask -> do
        labelThisThread ("accept-loop-" ++ show localAddress)
        go unmask acceptOne0
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
              (numberOfConnections connectionManager)
              limits
            runAccept acceptOne

          case result of
            (AcceptFailure err, acceptNext) -> do
              traceWith tracer (TrAcceptError err)
              -- Try to determine if the connection was aborted by the remote
              -- end before we could process the accept, or if it was a resource
              -- exhaustion problem. NB. This piece of code is fragile and
              -- depends on specific strings/mappings in the network and base
              -- libraries.
              case fromException err of
                Just ioErr | isECONNABORTED ioErr -> do
                  threadDelay server_CONNABORTED_DELAY
                  go unmask acceptNext
                -- all other exceptions are fatal for the whole process, hence
                -- no need to use a rethrow policy
                _ -> throwIO err

            (Accepted socket remoteAddress, acceptNext) ->
              (do
                  localAddress' <- getLocalAddr snocket socket
                  let connId = ConnectionId { localAddress = localAddress',
                                              remoteAddress }
                  traceWith tracer (TrAcceptConnection connId)
                  async $
                    do
                       a <-
                         unmask
                           (includeInboundConnection
                             connectionManager
                             hardLimit socket connId)
                       case a of
                         Connected {}    -> pure ()
                         Disconnected {} -> close snocket socket
                    `onException`
                      close snocket socket
              `onException`
                 close snocket socket
              )
              >> go unmask acceptNext


isECONNABORTED :: IOError -> Bool
#if defined(mingw32_HOST_OS)
-- On Windows the network package classifies all errors as OtherError. This
-- forced us to match on the error string. The text string comes from the
-- network package's winSockErr.c, and if it ever changes we must update our
-- text string too.
isECONNABORTED (IOError _ _ _ "Software caused connection abort (WSAECONNABORTED)" _ _) = True
isECONNABORTED _ = False
#else
isECONNABORTED (IOError _ _ _ _ (Just cerrno) _) = eCONNABORTED == Errno cerrno
#if defined(darwin_HOST_OS)
-- There is a bug in accept for IPv6 sockets. Instead of returning -1 and
-- setting errno to ECONNABORTED an invalid (>= 0) file descriptor is returned,
-- with the client address left unchanged. The uninitialized client address
-- causes the network package to throw the user error below.
isECONNABORTED (IOError _ UserError _ "Network.Socket.Types.peekSockAddr: address family '0' not supported." _ _) = True
#endif
isECONNABORTED _ = False
#endif

--
-- Trace
--

data Trace peerAddr
    = TrAcceptConnection            (ConnectionId peerAddr)
    | TrAcceptError                 SomeException
    | TrAcceptPolicyTrace           AcceptConnectionsPolicyTrace
    | TrServerStarted               [peerAddr]
    | TrServerStopped
    | TrServerError                 SomeException
    -- ^ similar to 'TrAcceptConnection' but it is logged once the connection is
    -- handed to inbound connection manager, e.g. after handshake negotiation.
  deriving Show
