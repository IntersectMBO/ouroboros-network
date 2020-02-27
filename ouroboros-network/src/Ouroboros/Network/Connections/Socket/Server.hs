{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Ouroboros.Network.Connections.Socket.Server
  ( acceptOne
  , acceptLoop
  , withSocket
  ) where

import Control.Monad.Class.MonadThrow
import Data.Void (Void, absurd)
import qualified Network.Socket as Socket (SockAddr)

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Connections.Types
import Ouroboros.Network.Snocket (Snocket)
import qualified Ouroboros.Network.Snocket as Snocket

-- | Brackets a socket of a given family, bound to a given address. In the
-- continuation, you probably want to use 'acceptOne' or 'acceptLoop'.
withSocket
  :: forall m socket addr t .
     ( MonadCatch m, MonadMask m )
  => Snocket m socket addr
  -> addr
  -> (addr -> socket -> m t)
  -- ^ When this is called, the server is up and listening. When the callback
  -- returns or dies exceptionally, the listening socket is closed.
  -- A SockAddr is given, which may differ from the requested bind address, in
  -- case you give a 0 port, for instance.
  -> m t
withSocket snocket bindaddr k = bracket openSocket (closeSocket . snd) (uncurry k)
  where

  -- Use bracketOnError to ensure the socket is closed if any of the preparation
  -- (setting options, bind, listen) fails. If not, the socket is returned
  -- and the caller is responsible for closing it (the bracket at the top level
  -- of this server definition).
  openSocket :: m (addr, socket)
  openSocket = bracketOnError createSocket closeSocket $ \socket -> do
    Snocket.bind snocket socket bindaddr
    Snocket.listen snocket socket
    -- May be different from bindaddr, in case it's a TCP address with zeros
    -- for instance.
    boundAddr <- Snocket.getLocalAddr snocket socket
    return (boundAddr, socket)

  createSocket :: m socket
  createSocket = Snocket.openToConnect snocket bindaddr

  closeSocket :: socket -> m ()
  closeSocket = Snocket.close snocket

-- | Use a Snocket and an Accept term to attempt to get a new connection
-- resource and include it in a Connections term.
--
-- If an exception is raised, it must be fatal. "Normal" error cases like too
-- many file handlers are expected to be dealt with by the Accept term and
-- appear as NotAcquired outcomes.
acceptOne
  :: forall m socket addr err request reject accept .
     ( MonadMask m )
  => Snocket m socket addr
  -> Connections (ConnectionId addr) socket request reject accept m
  -> addr -- Bind address; needed to construct ConnectionId
  -> request Remote
  -> Snocket.Accept m err addr socket
  -> m (Outcome Remote err reject accept socket m, Snocket.Accept m err addr socket)
acceptOne snocket connections bindaddr request accept = mask $ \restore -> do
  (accepted, accept') <- restore (Snocket.runAccept accept)
  case accepted of
    Snocket.AcceptException err ->
      pure (NotAcquired err, accept')
    Snocket.AcceptOk sock peeraddr -> do
      let connid = ConnectionId { localAddress = bindaddr, remoteAddress = peeraddr }
          resource = Existing (AcquiredResource sock (Snocket.close snocket sock))
      outcome <- restore (includeResource connections connid resource request)
        `onException`
        Snocket.close snocket sock
      -- The type of `outcome` has the `err` type specialized to Void, so
      -- we have to free it up.
      case outcome of
        NotAcquired void  -> absurd void
        Acquired decision -> pure (Acquired decision, accept')

-- | A common pattern: accept in a loop, passing each connection through a
-- Connections term, and handling exceptions without necessarily dying.
-- The decision given by the Connections term is ignored.
--
-- Be prudent in choosing what to do in the exception handler.
-- Async exceptions should be re-thrown.
acceptLoop
  :: ( MonadMask m )
  => Snocket m socket addr
  -> Connections (ConnectionId addr) socket request reject accept m
  -> addr -- Bind address; needed to construct ConnectionId
  -> request Remote
  -> (err -> m ())
  -> Snocket.Accept m err addr socket
  -> m Void
acceptLoop snocket connections bindaddr request handleErr accept = do
  (outcome, accept') <- acceptOne snocket connections bindaddr request accept
  case outcome of
    NotAcquired err -> handleErr err
    _ -> pure ()
  acceptLoop snocket connections bindaddr request handleErr accept'
