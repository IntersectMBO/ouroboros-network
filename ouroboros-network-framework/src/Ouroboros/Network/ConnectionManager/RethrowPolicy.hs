{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Rethrow policy for connection manager.
--
-- Connection manager has a centralised way of handling exceptions.
-- 'RethrowPolicy' is a way to decided wheather it is enough to shutdown
-- connection or the node should shut down itself.  Theis mechanism is affected
-- by the design choices in the mutliplexer.
--
-- Whenever a mini-protocol throws an exception, the bearer is closed.  There is
-- no way to recover a bidirectional connection if one side failed, in such way
-- that the other end could still re-use it, e.g.  if the initiator throws, we
-- cannot just restart it on the same bearer, since there might be unconsumed
-- bytes on the other end.
--
-- 'RethrowPolicy' is supplied to 'makeConnectionHandler' which creates both
-- the action that runs on each connection and error handler.  Error handler is
-- attached by the connection manager (see
-- 'Ouroboros.Network.ConnectionManager.Core').  This error handler is using
-- 'RethrowPolicy'.
--
-- This mechanism is enough for both:
--
--  * the server implemented in `Ouroboros.Network.ConnectionManager.Server',
--  * 'PeerStateActions' used by peer-to-peer governor.
--
-- Since both start mini-protocols with 'runMiniProtocol' they can also have
-- access to the result / exception thrown of a mini-protocol.
-- 'PeerStateActions' are only using this to inform the governor that the
-- peer transitioned to 'PeerCold' or to deactivate the peer.
--
module Ouroboros.Network.ConnectionManager.RethrowPolicy
  ( RethrowPolicy (..)
  , mkRethrowPolicy
  , ErrorCommand (..)
  , ErrorContext (..)
  ) where

import           Control.Exception


data ErrorCommand =
    -- | Shutdown node.
    ShutdownNode

    -- | Shutdown connection with the peer.
    --
  | ShutdownPeer

-- | 'ErrorCommand' is a commutative semigroup with 'ShutdownNode' being an
-- absorbing element, and 'ShutdownPeer' is the unit element.
--
instance Semigroup ErrorCommand where
    ShutdownNode <> _ = ShutdownNode
    _ <> ShutdownNode = ShutdownNode
    ShutdownPeer <> ShutdownPeer = ShutdownPeer

instance Monoid ErrorCommand where
    mempty = ShutdownPeer


data ErrorContext = OutboundError
                  | InboundError


type RethrowPolicy_ = ErrorContext -> SomeException -> ErrorCommand

newtype RethrowPolicy = RethrowPolicy {
    runRethrowPolicy :: RethrowPolicy_
  }
  deriving Semigroup via RethrowPolicy_
  deriving Monoid    via RethrowPolicy_


-- | Smart constructor for 'RethrowPolicy'.
--
mkRethrowPolicy :: forall e.
                   Exception e
                => (ErrorContext -> e -> ErrorCommand)
                -> RethrowPolicy
mkRethrowPolicy fn =
    RethrowPolicy $ \ctx err ->
      case fromException err of
        Just e  -> fn ctx e
        Nothing -> ShutdownPeer
