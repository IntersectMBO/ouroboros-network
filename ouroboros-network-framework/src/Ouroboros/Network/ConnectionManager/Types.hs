{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- 'withInitiatorMode' has @HasInitiator muxMode ~ True@ constraint, which is
-- not redundant at all!  It limits case analysis.
--
-- TODO: this might not by needed by `ghc-8.10`.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Connection manager core types.
--
-- Connection manager is responsible for managing uni- and bi-directional
-- connections and threads which are running network applications using
-- 'network-mux'.  In particular it is responsible for:
--
-- * opening new connection / reusing connections (for bidirectional
-- connections);
--
-- * run connection handler, i.e. 'ConnectionHandler', which runs handshake
-- negotiation, notifiies connection manager on the results and starts the
-- multiplexer;
--
-- * error handling for connection threads;
--
-- * keeping track of handshake negotiation: weather a uni- or bidirectional
-- connection was negotiated;
--
-- * tracking state of each connection;
--
-- * keep inbound connections under limits.
--
-- Connection manager is disigned to work for any 'MuxMode'.  This way we can
-- use it for managing connections from a local clients, i.e. share the same
-- server implementation which accepts connections over a Unix socket / Windows
-- named pipe.
--
-- The calls 'requestOutboundConnection' and 'includeInboundConnection' return
-- once a connection has been negotiated.  The returned 'handle' contains all
-- the information that is need to start and monitor mini-protocols through
-- the mux interface.
--
-- To support bi-directional connections we need to be able to (on-demand)
-- start responder sides of mini-protocols on incoming connections.  The
-- interface to give control over bi-directional outbound connection to the
-- server is using an @STM@ queue (see 'ControlChannel') over which messages
-- are passed to the server.  The server runs a single thread which accepts
-- them and acts on them.
--
-- When calling 'requestOutboundConnection' the connection manager will wait for
-- handshake negotiation, once resolved it will create the @handle@ and pass it
-- to 'PeerStateActions' (the peer-to-peer governor component which interacts
-- with connection manager).  If that connection was negotiated as duplex, it
-- will also be passed to the server. @handle@
-- ('Ouroboros.Network.ConnectionHandler.Handle'), gives a way to control which
-- protocols are running by the multiplexer and monitor them. 
--
-- For inbound connections, the connection manager will pass handle (also after
-- negotiation).
--
-- >                                                                                   
-- > ┌────────────────────────┐                          
-- > │                        │        ┏━━━━━━━━━━━━━━━━┓     ┌──────────────────┐ 
-- > │   ConnectionHandler    │        ┃                ┃     │                  │ 
-- > │                        ┝━━━━━━━▶┃     handle     ┣━━━━▶│ PeerStateActions ├───▶ P2P Governor
-- > │  inbound / outbound    │        ┃                ┃     │                  │ 
-- > │         ┃              │        ┗━━┳━━━━━━━━━━━━━┛     └──────────────────┘ 
-- > └─────────╂──────────────┘           ┃
-- >           ┃                          ┃
-- >           ▼                          ┃
-- >    ┏━━━━━━━━━━━━━━━━━┓               ┃
-- >    ┃ Control Channel ┃               ┃
-- >    ┗━━━━━━┳━━━━━━━━━━┛               ┃
-- >           ┃                          ┃
-- >           ┃                          ┃
-- >           ▼                          ┃
-- > ┌────────────────────────┐           ┃
-- > │                        │           ┃
-- > │         Server         │◀━━━━━━━━━━┛
-- > │                        │
-- > └────────────────────────┘
--
-- Termination prcedure as well as connection state machine is not described in
-- this haddock, see associated specification.
--
module Ouroboros.Network.ConnectionManager.Types
  ( -- * Connection manager core types
    -- ** Connection Types
    AddressType (..)
  , Provenance (..)
  , DataFlow  (..)
  , ConnectionType (..)
    -- ** Connection Handler
    -- $connectionhandler
  , Action (..)
  , ConnectionHandlerFn
  , ConnectionHandler (..)
  , Inactive (..)
  , ExceptionInHandler (..)
  , HandleErrorType (..)
    -- ** Prune Policy
  , PrunePolicy
  , simplePrunePolicy
    -- * Connection Manager
    -- ** Connection Manager Arguments
  , ConnectionManager (..)
  -- ** API
  , Connected (..)
  , OperationResult (..)
  , DemotedToColdRemoteTr (..)
  , RequestOutboundConnection
  , IncludeInboundConnection
  -- *** Outbound side
  , requestOutboundConnection
  , promotedToWarmRemote
  , unregisterOutboundConnection
  -- *** Inbound side
  , includeInboundConnection
  , unregisterInboundConnection
  , numberOfConnections
  -- ** Private API
  -- Includes all constructors required to create a 'ConnectionManager'.
  , OutboundConnectionManager (..)
  , InboundConnectionManager (..)
    -- * Exceptions
  , ConnectionManagerError (..)
  , InState (..)
    -- * Mux types
  , WithMuxMode (..)
  , withInitiatorMode
  , withResponderMode
   -- * Promise
   -- $promise
  , newEmptyPromiseIO
  , PromiseReader (..)
  , readPromiseIO
  , PromiseWriter (..)
  , PromiseWriterException (..)
    -- * Tracing
  , ConnectionManagerTrace (..)
  , ConnectionExitType (..)
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadThrow
import           Control.Monad (unless)
import           Control.Tracer (Tracer)
import           Data.Functor (void)
import           Data.List (sortOn)
import           Data.Typeable (Typeable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack (CallStack, prettyCallStack)

import           Network.Mux.Types ( MuxBearer
                                   , MuxMode (..)
                                   , HasInitiator
                                   , HasResponder
                                   , MiniProtocolDir
                                   )

import           Ouroboros.Network.ConnectionId (ConnectionId)
import           Ouroboros.Network.MuxMode


-- | Connection manager supports `IPv4` and `IPv6` addresses.
--
data AddressType = IPv4Address | IPv6Address
    deriving Show


-- | Each connection is is either initiated locally (outbound) or by a remote
-- peer (inbound).
--
data Provenance =
    -- | An inbound connection: one that was initiated by a remote peer.
    --
    Inbound

    -- | An outbound connection: one that was initiated by us.
    --
  | Outbound
  deriving (Eq, Ord, Show)


-- | Each connection negotiates if it is uni- or bi-directionalal.  'DataFlow'
-- is a lifte time property of a connection, once negotiated it never changes.
--
data DataFlow
    = Unidirectional
    | Duplex
  deriving (Eq, Ord, Show)


-- | Either unnegotiated or negotiated unidirecitonal or duplex connections.
-- This is not a static property of a connection.  It is used by 'PrunePolicy'.
--
data ConnectionType
    -- | An unnegotiated connection.
    --
    = UnnegotiatedConn !Provenance

    -- | An inbound idle connection.
    --
    | InboundIdleConn !DataFlow

    -- | A negotiated connection, which is used in only one direction indicated
    -- by 'Provenance'.  The connection could itself negotiated either 'Duplex'
    -- or 'Unidirectional' data flow.
    --
    | NegotiatedConn   !Provenance !DataFlow

    -- | A connection which is runnng in full duplex mode.
    --
    | DuplexConn
    deriving (Eq, Ord)


-- $promise
--
-- Promise interface, backed by a `StrictTMVar`.
--
-- Making two seprate interfaces: 'PromiseWriter' and 'PromiseReader' allows us
-- to make a clear distinction between consumer and producers threads.

data PromiseWriter m a = PromiseWriter {
    -- | 'putPromise', is a non-blocking operation, it throws
    -- 'PromiseWriterException' if it would block.
    --
    writePromise :: a -> STM m (),

    -- | If the promise is empty it fills it, if it is non-empty it replaces
    -- the current value.
    --
    forcePromise :: a -> STM m ()
  }

data PromiseWriterException = PromiseWriterBlocked
  deriving (Show, Typeable)

instance Exception PromiseWriterException


newtype PromiseReader m a = PromiseReader {
    -- | A blocking read operation.
    readPromise :: STM m a
  }

readPromiseIO :: MonadSTM m => PromiseReader m a -> m a
readPromiseIO = atomically . readPromise

newEmptyPromise :: forall m a.
                   ( MonadSTM m
                   , MonadThrow (STM m) )
                => STM m (PromiseReader m a, PromiseWriter m a)
newEmptyPromise = do
    (v :: StrictTMVar m a) <- newEmptyTMVar
    let reader = PromiseReader { readPromise = readTMVar v }
        writer = PromiseWriter {
                    writePromise = \a -> do
                      r <- tryPutTMVar v a
                      unless r
                        (throwSTM PromiseWriterBlocked),

                    -- Both 'putTMVar' and 'swapTMVar' are blocking
                    -- operations, but the first blocks if @v@ is non-empty
                    -- and the latter blocks when @b@ is empty.  Combining them
                    -- with 'orElse' is a non-blocking operation.
                    forcePromise = \a -> putTMVar v a
                        `orElse` void (swapTMVar v a)
                  }
    pure (reader, writer)

newEmptyPromiseIO :: ( MonadSTM m
                     , MonadThrow (STM m) )
                  => m (PromiseReader m a, PromiseWriter m a)
newEmptyPromiseIO = atomically newEmptyPromise


--
-- ConnectionHandler
--
-- $connectionhandler
-- 'ConnectionHandler' provides monadic action which runs handshake
-- negotiation and starts the multiplexer.  It's the component which has access
-- to underlying socket.  There's one-to-one correspondence between sockets and
-- threads that run the handler.
--
-- [@'ConnectionHandlerFn'@]:
--   is the type of callback executed for each connection. All arguments are
--   provided by the connection manager.
-- [@'ConnectionHandler'@]:
--   is a newtype wrapper which provides inbound \/ outbound handlers depending
--   on @'MuxMode'@.
--


-- | Split error handling from a monadic action.  The intended usage is:
--
-- >
-- > \(Action action errorHandler) -> mask $ \unmask -> errorHandler (unmask action)
-- >
--
-- This allows to attach various error handlers to the action, e.g. both
-- `finally` and `catch`.
--
data Action m a = Action {
    action       :: m a,
    errorHandler :: m a -> m a
  }


-- | Action which is executed by thread designated for a given connection.
--
-- 'PromiseWriter' allows to notify the 'ConnectionManager' about the result of
-- handshake negotiation.
--
-- Note: 'PromiseWriter' could be replaced with an stm action which is
-- accessing the 'TVar' which holds state of the connection.
--
type ConnectionHandlerFn handlerTrace peerAddr handle handleError version m
     = PromiseWriter m (Either handleError (handle, version))
    -> Tracer m handlerTrace
    -> ConnectionId peerAddr
    -> (DiffTime -> MuxBearer m)
    -> Action m ()


-- | Connection handler action.  It is index by @muxMode :: 'MuxMode'@.
-- There's one 'ConnectionHandlerFn' per provenance, possibly limited by
-- @muxMode@.
--
data ConnectionHandler muxMode handlerTrace peerAddr handle handleError version m =
    ConnectionHandler {
        -- | Connection handler.
        --
        connectionHandler ::
          (WithMuxTuple muxMode
            (ConnectionHandlerFn handlerTrace peerAddr handle handleError version m)),

        -- | Detect that the underlaying connection is inactive for the
        -- specified time,  e.g blocks until any of the responder mini-protocols
        -- becomes active.
        --
        connectionIdle :: handle -> STM m Inactive
      }


-- | Boolean like type
--
data Inactive =
    Active MiniProtocolDir
  | Inactive


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


-- | Data type used to classify 'handleErrors'.
--
data HandleErrorType =
    -- | Handshake negotiation failed.  This is not a protocol error. 
    HandshakeFailure

    -- | Handshake protocol error.  This should include timeout errors or any
    -- IO errors.
  | HandshakeProtocolViolation


-- | 'PrunePolicy' allows to pick a select peers from which we will disconnect
-- (we use @TCP@ reset).  The choosen connections will be terminated by the
-- connection manger once it detects that there are too many inbound
-- conncections.
--
type PrunePolicy peerAddr stm =  Map peerAddr ConnectionType
                              -> Int
                              -> stm (Set peerAddr)


-- | The simplest 'PrunePolicy', it should only be used for tests.
--
simplePrunePolicy :: ( Applicative stm, Ord peerAddr )
                  => PrunePolicy peerAddr stm
simplePrunePolicy m n =
    pure
  . Set.fromList
  . map fst
  . take n
  . sortOn snd
  . Map.toList
  $ m



-- | Custom either type for result of various methods.
--
data OperationResult a
    = UnsupportedState !InState
    | OperationSuccess !a


-- | Return value of 'unregisterInboundConnection'
--
data DemotedToColdRemoteTr =
    CommitTr
    -- ^ Connection in 'InboundIdleState' was put in 'TerminatingState' (The
    -- @Commit@ transition)
  | DemotedToColdRemoteTr
    -- ^ @DemotedToCold^{Remote}@ transtion from @'InboundIdleState' dataFlow@
  | DemotedToColdRemoteDuplexTr
    -- ^ @DemotedToCold^{Remote}@ transition from @'DuplexState'@.
  deriving Show


-- | Result of 'requestOutboundConnection' or 'includeInboundConnection'.
--
data Connected peerAddr handle handleError =
    -- | We are connected and mux is running.
    --
    Connected    !(ConnectionId peerAddr) !DataFlow !handle

    -- | There was an error during handshake negotiation.
    --
    -- /Implementation detail:/ we return @'Maybe' handleError@, rather than
    -- 'handleError'.  In case of an existing inbound connection, the
    -- implmentation of 'requestOutboundConnection' is awaiting on handshake
    -- through the conection state.  The 'TerminatingState' or
    -- 'TerminatedState' are not only used for handshake errors, but also for
    -- normal termination, hence the @'Maybe'@.  We could await on
    -- update from the handshake instead, but this would introduce a race
    -- between inbound \/ outbound threads.
    --
  | Disconnected !(ConnectionId peerAddr) !(Maybe handleError)


type RequestOutboundConnection peerAddr handle handleError m
    =           peerAddr -> m (Connected peerAddr handle handleError)
type IncludeInboundConnection socket peerAddr handle handleError m
    = socket -> peerAddr -> m (Connected peerAddr handle handleError)


-- | Outbound connection manager API.
--
data OutboundConnectionManager (muxMode :: MuxMode) socket peerAddr handle handleError m where
    OutboundConnectionManager
      :: HasInitiator muxMode ~ True
      => { ocmRequestConnection    :: RequestOutboundConnection peerAddr handle handleError m
         , ocmUnregisterConnection :: peerAddr -> m (OperationResult ())
         , ocmPromotedToWarmRemote :: peerAddr -> m (OperationResult ())
         }
      -> OutboundConnectionManager muxMode socket peerAddr handle handleError m

-- | Inbound connection manager API.  For a server implementation we also need
-- to know how many connections are now managed by the connection manager.
--
-- This type is an internal detail of 'Ouroboros.Network.ConnectionManager'
--
data InboundConnectionManager (muxMode :: MuxMode) socket peerAddr handle handleError m where
    InboundConnectionManager
      :: HasResponder muxMode ~ True
      => { icmIncludeConnection    :: IncludeInboundConnection socket peerAddr handle handleError m
         , icmUnregisterConnection :: peerAddr -> m (OperationResult DemotedToColdRemoteTr)
         , icmNumberOfConnections  :: STM m Int
         }
      -> InboundConnectionManager muxMode socket peerAddr handle handleError m

-- | 'ConnectionManager'.
--
-- We identify resources (e.g. 'Network.Socket.Socket' or
-- 'System.Win32.Types.HANDLE') by their address.   It is enough for us to use
-- just the remote address rather than connection identifier, since we need one
-- connection towards that peer, even if we are connected through multiple
-- local addresses.  It is safe to share a connection manager with multiple
-- listening sockets.
--
newtype ConnectionManager (muxMode :: MuxMode) socket peerAddr handle handleError m =
    ConnectionManager {
        getConnectionManager
          :: WithMuxMode
              muxMode
              (OutboundConnectionManager muxMode socket peerAddr handle handleError m)
              (InboundConnectionManager  muxMode socket peerAddr handle handleError m)
      }

--
-- ConnectionManager API
--

-- | Include outbound connection into 'ConnectionManager'.
--
requestOutboundConnection :: HasInitiator muxMode ~ True
                          => ConnectionManager muxMode socket peerAddr handle handleError m
                          -> RequestOutboundConnection        peerAddr handle handleError m
requestOutboundConnection = ocmRequestConnection . withInitiatorMode . getConnectionManager

-- | Unregister outbound connection.
--
unregisterOutboundConnection :: HasInitiator muxMode ~ True
                             => ConnectionManager muxMode socket peerAddr handle handleError m
                             -> peerAddr -> m (OperationResult ())
unregisterOutboundConnection = ocmUnregisterConnection . withInitiatorMode . getConnectionManager

-- | Notify the 'ConnectionManager' that a remote end promoted us to a
-- /warm peer/ ; this runs the \(PromotedToWarm^{duplex}_{remote}\) transition
-- from the specification.
--
promotedToWarmRemote :: HasInitiator muxMode ~ True
                     => ConnectionManager muxMode socket peerAddr handle handleError m
                     -> peerAddr -> m (OperationResult ())
promotedToWarmRemote = ocmPromotedToWarmRemote . withInitiatorMode . getConnectionManager

-- | Include an inbound connection into 'ConnectionManager'.
--
includeInboundConnection :: HasResponder muxMode ~ True
                         => ConnectionManager muxMode socket peerAddr handle handleError m
                         -> IncludeInboundConnection  socket peerAddr handle handleError m
includeInboundConnection =  icmIncludeConnection . withResponderMode . getConnectionManager

-- | Unregister outbound connection.  Returns if the operation was successul.
--
unregisterInboundConnection :: HasResponder muxMode ~ True
                            => ConnectionManager muxMode socket peerAddr handle handleError m
                            -> peerAddr -> m (OperationResult DemotedToColdRemoteTr)
unregisterInboundConnection = icmUnregisterConnection . withResponderMode . getConnectionManager

-- | Number of connections tracked by the server.
--
numberOfConnections :: HasResponder muxMode ~ True
                    => ConnectionManager muxMode socket peerAddr handle handleError m
                    -> STM m Int
numberOfConnections = icmNumberOfConnections . withResponderMode . getConnectionManager


--
-- Errors
--


-- | Useful for tracing and error messages.
--
data InState
    = UnknownConnection
    | InReservedOutboundState
    | InUnnegotiatedState
    | InInboundIdleState !DataFlow
    | InInboundState     !DataFlow
    | InOutboundState    !DataFlow
    | InDuplexState
    | InWaitRemoteIdleState
    | InTerminatingState
    | InTerminatedState
    deriving (Show, Typeable)


-- | Exceptions used by 'ConnectionManager'.
--
data ConnectionManagerError peerAddr
    -- | A connection manager was asked for an outbound connection and there
    -- either exists a connection used in outbound direction or a reservation
    -- for an outbound connection.
    --
    = ConnectionExists      !Provenance !peerAddr    !CallStack

    -- | Connection manager was asked for an outbound connection which turned
    -- out to be unidrectional inbound, and thus it cannot be re-used..
    --
    | ForbiddenConnection   !(ConnectionId peerAddr) !CallStack

    -- | Connections that would be forbidden by the kernel (@TCP@ semantics).
    --
    | ImpossibleConnection  !(ConnectionId peerAddr) !CallStack

    -- | Connection is now terminating.
    --
    | ConnectionTerminating !(ConnectionId peerAddr) !CallStack

    -- | Connection has terminated.
    --
    | ConnectionTerminated  !peerAddr                !CallStack

    -- | Connection manager in impossible state.
    | ImpossibleState       !peerAddr                !CallStack

    -- | A forbidden operation in the given connection state.
    | ForbiddenOperation    !peerAddr !InState    !CallStack

    -- | A connection does not exists.  Only thrown when an existing connection
    -- was expected.
    --
    | UnknownPeer           !peerAddr                !CallStack
    deriving (Show, Typeable)


instance ( Show peerAddr
         , Typeable peerAddr ) => Exception (ConnectionManagerError peerAddr) where

    displayException (ConnectionExists provenance peerAddr cs) =
      concat [ "Connection already exists with peer "
             , show provenance
             , " "
             , show peerAddr
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ForbiddenConnection connId cs) =
      concat [ "Forbidden to reuse a connection (UnidirectionalDataFlow) with peer "
             , show connId
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ImpossibleConnection connId cs) =
      concat [ "Impossible connection with peer "
             , show connId
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ConnectionTerminating connId cs) =
      concat [ "Connection terminating "
             , show connId
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ConnectionTerminated peerAddr cs) =
      concat [ "Connection terminated "
             , show peerAddr
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ImpossibleState peerAddr cs) =
      concat [ "Imposible connection state for peer "
             , show peerAddr
             , "\n"
             , prettyCallStack cs
             ]
    displayException (ForbiddenOperation peerAddr reason cs) =
      concat [ "Forbidden operation"
             , show peerAddr
             , " "
             , show reason
             , "\n"
             , prettyCallStack cs
             ]
    displayException (UnknownPeer peerAddr cs) =
      concat [ "Forbidden operation"
             , show peerAddr
             , "\n"
             , prettyCallStack cs
             ]


--
-- Tracing
--

-- | 'ConenctionManagerTrace' contains a hole for a trace of single connection
-- which is filled with 'ConnectionHandlerTrace'.
--
data ConnectionManagerTrace peerAddr handlerTrace
  = TrIncludeConnection          !Provenance !peerAddr
  | TrUnregisterConnection       !Provenance !peerAddr
  | TrIncludedConnection         !Provenance !(ConnectionId peerAddr)
  | TrNegotiatedConnection       !Provenance !(ConnectionId peerAddr) !DataFlow
  | TrConnect                    !(Maybe peerAddr) !peerAddr
  | TrConnectError               !(Maybe peerAddr) !peerAddr !SomeException
  -- | We reused a duplex connection.  This can only be emitted by
  -- 'requestOutboundConnection'.
  | TrReusedConnection           !(ConnectionId peerAddr)
  | TrConnectionTerminating      !Provenance !(ConnectionId peerAddr)
  | TrConnectionTerminated       !Provenance !peerAddr
  | TrConnectionHandler          !(ConnectionId peerAddr) !handlerTrace
  | TrShutdown

  | TrConnectionExists           !Provenance !peerAddr
  | TrForbiddenConnection        !(ConnectionId peerAddr)
  | TrImpossibleConnection       !(ConnectionId peerAddr)
  | TrConnectionFailure          !(ConnectionId peerAddr)
  | TrConnectionNotFound         !Provenance !peerAddr
  | TrForbiddenOperation         !peerAddr                !InState
  -- | A connection transitioned from 'DuplexState' to @'Inbound' 'Duplex'@.
  --
  | TrDemotedToColdLocal         !(ConnectionId peerAddr) !InState
  | TrWaitIdle                   !(ConnectionId peerAddr)
  | TrPruneConnections           ![peerAddr]

  | TrCleanup                    !(ConnectionId peerAddr)
  -- | Connection exit
  | TrConnectionExit             !(ConnectionId peerAddr) ConnectionExitType
  deriving Show


data ConnectionExitType
  -- | Connection not found in state.
  = NotFound

  -- | Reset connection.
  | Reset

  -- | Close and set 'WaitTime' timeout.
  --
  | WaitTime
  deriving Show
