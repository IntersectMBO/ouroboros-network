{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Rethrow policy for 'MuxConnectionHandler'.
--
-- Connection manager has a centralised way of handling exceptions.
-- 'RethrowPolicy' is a way to decided whether it is enough to shutdown
-- connection or the node should shut down itself.  Theis mechanism is affected
-- by the design choices in the mutliplexer.
--
-- Whenever a mini-protocol throws an exception, the bearer is closed.  There is
-- no way to recover a bidirectional connection if one side failed, in such way
-- that the other end could still re-use it, e.g.  if the initiator throws, we
-- cannot just restart it on the same bearer, since there might be unconsumed
-- bytes on the other end.
--
-- 'RethrowPolicy' is supplied to 'makeMuxConnectionHandler' which creates both
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
module Ouroboros.Network.RethrowPolicy
  ( RethrowPolicy (..)
  , mkRethrowPolicy
  , ErrorCommand (..)
  , ErrorContext (..)
    -- * Example policies
  , muxErrorRethrowPolicy
  , ioErrorRethrowPolicy
  ) where

import Control.Exception

import Network.Mux.Trace qualified as Mx
import Network.Mux.Types qualified as Mx


data ErrorCommand =
    -- | Shutdown node.
    ShutdownNode

    -- | Shutdown connection with the peer.
    --
  | ShutdownPeer
  deriving Show

-- | 'ErrorCommand' is a commutative semigroup with 'ShutdownNode' being an
-- absorbing element, and 'ShutdownPeer' is the unit element.
--
instance Semigroup ErrorCommand where
    ShutdownNode <> _            = ShutdownNode
    _ <> ShutdownNode            = ShutdownNode
    ShutdownPeer <> ShutdownPeer = ShutdownPeer

instance Monoid ErrorCommand where
    mempty = ShutdownPeer


-- | Whether an exception happened on outbound or inbound connection.
--
-- TODO: It would be more useful to have access to whether the exception
-- happened on initiator or responder. The easiest way to fix this is make mux
-- throw the exception together with context.  This allows to keep error
-- handling be done only by the connection manager (rather than by server and
-- 'PeerStateActions').
--
data ErrorContext = OutboundError
                  | InboundError
    deriving Show


newtype RethrowPolicy = RethrowPolicy {
    runRethrowPolicy :: ErrorContext -> SomeException -> ErrorCommand
  }
  deriving newtype Semigroup
  deriving newtype Monoid


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

--
-- Some example error policies
--

muxErrorRethrowPolicy, ioErrorRethrowPolicy :: RethrowPolicy

muxErrorRethrowPolicy = mkRethrowPolicy ( \_ (_ :: Mx.Error) -> ShutdownPeer )
                     <> mkRethrowPolicy ( \_ (e :: Mx.RuntimeError) ->
                                          case e of
                                            Mx.ProtocolAlreadyRunning       {} -> ShutdownPeer
                                            Mx.UnknownProtocolInternalError {} -> ShutdownNode
                                            Mx.BlockedOnCompletionVar       {} -> ShutdownPeer
                                        )

ioErrorRethrowPolicy  = mkRethrowPolicy $ \_ (_ :: IOError)  -> ShutdownPeer
