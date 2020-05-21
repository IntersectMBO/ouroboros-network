{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- 'withInitiatorMode' has @HasInitiator muxMode ~ True@ constraint, which is
-- not redundant at all!  It limits case analysis.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- Connection manager core types.
--
module Ouroboros.Network.ConnectionManager.Types
  ( -- * Connection manager core types
    ConnectionState (..)
  , ConnectionHandler (..)
  , ConnectionHandlerFn
  , Action (..)
  , ConnectionManagerArguments (..)
  , AddressType (..)
    -- * 'ConnectionManager'
  , ConnectionManager (..)
  , InboundConnectionManager (..)
  , IncludeOutboundConnection
  , includeOutboundConnection
  , IncludeInboundConnection
  , includeInboundConnection
  , numberOfConnections
    -- * Exceptions
  , ExceptionInHandler (..)
    -- * Mux types
  , WithMuxMode (..)
  , WithMuxTuple
  , withInitiatorMode
  , withResponderMode
  , SingInitiatorResponderMode (..)
    -- * Tracing
  , ConnectionManagerTrace (..)
   -- * Auxiliary types
  , Promise (..)
  ) where

import           Control.Exception ( Exception
                                   , SomeException )
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Tracer (Tracer)
import           Data.Typeable (Typeable)

import           Network.Mux.Types ( MuxBearer
                                   , MuxMode (..)
                                   , HasInitiator
                                   , HasResponder
                                   )
import           Network.Mux.Trace ( MuxTrace
                                   , WithMuxBearer )

import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.Snocket (Snocket)


-- | During its lifetime, each connection is in either one of the three states:
--
data ConnectionState =
    --  | An inbound connection: one that was initiated by a remote peer.
    --
    Inbound

    -- | An outbound connection: one that was initiated by us.
  | Outbound

    -- | A connection which was initiated by either side and is not used in
    -- duplex mode.
    --
  | InboundOutbound
  deriving (Eq, Show)


--
-- Mux types
--
-- TODO: find a better place for them, maybe 'Ouroboros.Network.Mux'
--

data WithMuxMode (muxMode :: MuxMode) a b where
    WithInitiatorMode          :: a -> WithMuxMode InitiatorMode a b
    WithResponderMode          :: b -> WithMuxMode ResponderMode a b
    WithInitiatorResponderMode :: a -> b -> WithMuxMode InitiatorResponderMode a b

type WithMuxTuple muxMode a = WithMuxMode muxMode a a

withInitiatorMode :: HasInitiator muxMode ~ True
                  => WithMuxMode muxMode a b
                  -> a
withInitiatorMode (WithInitiatorMode          a  ) = a
withInitiatorMode (WithInitiatorResponderMode a _) = a

withResponderMode :: HasResponder muxMode ~ True
                  => WithMuxMode muxMode a b
                  -> b
withResponderMode (WithResponderMode            b) = b
withResponderMode (WithInitiatorResponderMode _ b) = b


-- | Singletons for matching the 'muxMode'.
--
data SingInitiatorResponderMode (muxMode :: MuxMode) where
    SInitiatorMode          :: SingInitiatorResponderMode InitiatorMode
    SResponderMode          :: SingInitiatorResponderMode ResponderMode
    SInitiatorResponderMode :: SingInitiatorResponderMode InitiatorResponderMode


-- | Promise is a strict version of 'Maybe'
--
data Promise a
    = Promised !a
    | Empty


-- | Split error handling from action.  The indentend usage is:
-- ```
-- \(Action action errorHandler) -> mask (errorHandler (unmask action))
-- ```
-- This allows to attach various error handlers to the action, e.g. both
-- `finally` and `catch`.
data Action m a = Action {
    action       :: m a,
    errorHandler :: m a -> m a
  }


-- | Action which is executed by thread designated for a given connection.
--
type ConnectionHandlerFn handlerTrace peerAddr muxPromise m
     = StrictTVar m (Promise muxPromise)
    -> Tracer m handlerTrace
    -> ConnectionId peerAddr
    -> (DiffTime -> MuxBearer m)
    -> Action m ()


newtype ConnectionHandler muxMode handlerTrace peerAddr muxPromise m =
    ConnectionHandler
      (WithMuxTuple muxMode (ConnectionHandlerFn handlerTrace peerAddr muxPromise m))

-- | Exception which where caught in the connection thread and were re-thrown
-- in the main thread by the 'rethrowPolicy'.
--
data ExceptionInHandler peerAddr where
    ExceptionInHandler :: !peerAddr
                       -> !SomeException
                       -> ExceptionInHandler peerAddr
  deriving Typeable

instance   Show peerAddr => Show (ExceptionInHandler peerAddr) where
    show (ExceptionInHandler peerAddr e) = "ExceptionInHandler "
                                        ++ show peerAddr
                                        ++ " "
                                        ++ show e
instance ( Show peerAddr
         , Typeable peerAddr ) => Exception (ExceptionInHandler peerAddr)


-- | Connection maanger supports `IPv4` and `IPv6` addresses.
--
data AddressType = IPv4Address | IPv6Address


-- | Assumptions \/ arguments for a 'ConnectionManager'.
--
data ConnectionManagerArguments (muxMode :: MuxMode) handlerTrace socket peerAddr muxPromise m =
    ConnectionManagerArguments {
        connectionManagerTracer      :: Tracer m (ConnectionManagerTrace peerAddr handlerTrace),

        -- | Mux trace.
        --
        connectionManagerMuxTracer   :: Tracer m (WithMuxBearer (ConnectionId peerAddr) MuxTrace),

        -- | Local @IPv4@ address of the connection manager.  If given, outbound
        -- connections to an @IPv4@ address will bound to it.
        --
        connectionManagerIPv4Address :: Maybe peerAddr,

        -- | Local @IPv6@ address of the connection manager.  If given, outbound
        -- connections to an @IPv6@ address will bound to it.
        --
        connectionManagerIPv6Address :: Maybe peerAddr,

        connectionManagerAddressType :: peerAddr -> Maybe AddressType,

        -- | Callback which runs in a thread dedicated for a given connection.
        --
        connectionHandler            :: ConnectionHandler muxMode handlerTrace peerAddr muxPromise m,

        -- | Snocket for the 'socket' type.
        --
        connectionSnocket            :: Snocket m socket peerAddr
      }


type IncludeOutboundConnection        peerAddr muxPromise m
    =           peerAddr -> m (STM m muxPromise)
type IncludeInboundConnection  socket peerAddr muxPromise m
    = socket -> peerAddr -> m (STM m muxPromise)


-- | Inbound connection manager.  For a server implementation we also need to
-- know how many connections are now managed by the connection manager.
--
-- This type is an internal detail of 'Ouroboros.Network.ConnectionManager'
--
data InboundConnectionManager (muxMode :: MuxMode) socket peerAddr muxPromise m where
    InboundConnectionManager
      :: HasResponder muxMode ~ True
      => { icmIncludeConnection   :: IncludeInboundConnection socket peerAddr muxPromise m
         , icmNumberOfConnections :: STM m Int
         }
      -> InboundConnectionManager muxMode socket peerAddr muxPromise m

-- | 'ConnectionManager'.
--
-- We identify resources (e.g. 'Network.Socket.Socket') by their address.   It
-- is enough for us to use just the remote address rather than connection
-- identifier, since we just need one connection towards that peer, even if we
-- are connected through multiple addresses.  It is safe to share a connection
-- manager with all the accepting sockets.
--
newtype ConnectionManager (muxMode :: MuxMode) socket peerAddr muxPromise m = ConnectionManager {
    runConnectionManager
      :: WithMuxMode muxMode (IncludeOutboundConnection                 peerAddr muxPromise m)
                             (InboundConnectionManager  muxMode socket peerAddr muxPromise m)
  }

--
-- ConnectionManager API
--

-- | Include outbound connection into 'ConnectionManager'.
--
includeOutboundConnection :: HasInitiator muxMode ~ True
                          => ConnectionManager muxMode socket peerAddr muxPromise m
                          -> IncludeOutboundConnection        peerAddr muxPromise m
includeOutboundConnection = withInitiatorMode . runConnectionManager

-- | Include an inbound connection into 'ConnectionManager'.
--
includeInboundConnection :: HasResponder muxMode ~ True
                         => ConnectionManager muxMode socket peerAddr muxPromise m
                         -> IncludeInboundConnection  socket peerAddr muxPromise m
includeInboundConnection =  icmIncludeConnection . withResponderMode . runConnectionManager

-- | Number of currently included connections.
--
-- Note: we count all connection: both inbound and outbound.  In a future
-- version we could count only inbound connections, but this will require
-- tracking state inside mux if the responder side has started running (through
-- on-demand interface).  This is currently not exposed by mux.
--
numberOfConnections :: HasResponder muxMode ~ True
                    => ConnectionManager muxMode socket peerAddr muxPromise m
                    -> STM m Int
numberOfConnections = icmNumberOfConnections . withResponderMode . runConnectionManager

--
-- Tracing
--

-- | 'ConenctionManagerTrace' contains a hole for a trace of single connection
-- which is filled with 'ConnectionTrace'.
--
data ConnectionManagerTrace peerAddr a
  = IncludedConnection        !(ConnectionId peerAddr) !ConnectionState
  | ReusedConnection          !peerAddr                !ConnectionState
  | ConnectionFinished        !(ConnectionId peerAddr) !ConnectionState
  | ErrorFromHandler          !(ConnectionId peerAddr) !SomeException
  | RethrownErrorFromHandler                           !(ExceptionInHandler peerAddr)
  | ConnectionTrace           !(ConnectionId peerAddr) !a
  | ShutdownConnectionManager
  deriving Show
