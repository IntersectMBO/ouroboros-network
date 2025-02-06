{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
-- connections) and exposes a method to register inbound connections;
--
-- * run connection handler, i.e. 'ConnectionHandler', which runs handshake
-- negotiation, notifies connection manager on the results and starts the
-- multiplexer;
--
-- * error handling for connection threads;
--
-- * keeping track of handshake negotiation: whether a unidirectional or duplex
--   connection was negotiated;
--
-- * tracking state of each connection;
--
-- * keep inbound connections under limits.
--
-- Connection manager is designed to work for any 'Network.Mux.Mode', though
-- the most useful ones are 'Mux.ResponderMode' and 'Mux.InitiatorResponderMode':
--
-- * 'InitiatorResponderMode' - useful for node-to-node applications, which
--                              needs to create outbound connections as well as
--                              accept inbound ones;
-- * 'ResponderMode'          - useful for server side of node-to-client; it
--                              allows us to share the same server between
--                              node-to-client and node-to-node;
-- * 'InitiatorMode'          - could be used on client side of node-to-client
--                              applications.
--
-- The calls 'acquireOutboundConnection' and 'includeInboundConnection' return
-- once a connection has been negotiated.  The returned 'handle' contains all
-- the information that is needed to start and monitor mini-protocols through
-- the mux interface.
--
-- For inbound connections, the connection manager will pass handle (also after
-- negotiation).
--
-- >
-- > ┌────────────────────────┐
-- > │                        │        ┏━━━━━━━━━━━━━━━━┓
-- > │   ConnectionHandler    │        ┃                ┃
-- > │                        ┝━━━━━━━▶┃     handle     ┃
-- > │  inbound / outbound    │        ┃                ┃
-- > │         ┃              │        ┗━━┳━━━━━━━━━━━━━┛
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
-- Termination procedure as well as connection state machine is not described in
-- this haddock, see associated specification.
--
-- The 'handle' is used in `ouroboros-network` package to construct
-- `PeerStateActions` which allow for the outbound governor to
--

module Ouroboros.Network.ConnectionManager.Types
  ( -- * Connection manager core types
    -- ** Connection Types
    AddressType (..)
  , Provenance (..)
  , DataFlow (..)
  , TimeoutExpired (..)
  , ConnectionType (..)
    -- ** Connection Handler
    -- $connectionhandler
  , MaskedAction (..)
  , ConnectionHandlerFn
  , ConnectionHandler (..)
  , Inactive (..)
  , ExceptionInHandler (..)
  , HandleErrorType (..)
  , HandshakeConnectionResult (..)
    -- ** Prune Policy
  , PrunePolicy
  , simplePrunePolicy
    -- * Connection Manager
    -- ** Connection Manager Arguments
  , ConnectionManager (..)
    -- ** API
  , Connected (..)
  , OperationResult (..)
  , resultInState
  , DemotedToColdRemoteTr (..)
  , AcquireOutboundConnection
  , IncludeInboundConnection
    -- *** Outbound side
  , acquireOutboundConnection
  , promotedToWarmRemote
  , demotedToColdRemote
  , releaseOutboundConnection
    -- *** Inbound side
  , includeInboundConnection
  , releaseInboundConnection
  , numberOfConnections
    -- ** Private API
    -- Includes all constructors required to create a 'ConnectionManager'.
  , OutboundConnectionManager (..)
  , InboundConnectionManager (..)
    -- * Exceptions
  , ConnectionManagerError (..)
  , SomeConnectionManagerError (..)
  , AbstractState (..)
    -- * Counters
  , ConnectionManagerCounters (..)
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
  , AssertionLocation (..)
  , MaybeUnknown (..)
  , Transition' (..)
  , Transition
  , AbstractTransition
  , mkTransition
  , mkAbsTransition
  , TransitionTrace
  , TransitionTrace' (..)
  , AbstractTransitionTrace
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (unless)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Tracer (Tracer)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)
import GHC.Stack (CallStack, prettyCallStack)
import System.Random (StdGen)

import Network.Mux.Types (HasInitiator, HasResponder, MiniProtocolDir)
import Network.Mux.Types qualified as Mux

import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ConnectionManager.ConnMap (ConnMap)
import Ouroboros.Network.MuxMode
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))


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


-- | Each connection negotiates if it is uni- or bi-directional.  'DataFlow'
-- is a life time property of a connection, once negotiated it never changes.
--
-- NOTE: This type is isomorphic to `DiffusionMode` for `node-to-node`
-- connections (see `Ouroboros.Network.Diffusion.P2P.ntnDataFlow`), but it isn't
-- for `node-to-client` connections (see
-- `Ouroboros.Network.Diffusion.P2P.ntcDataFlow).
--
data DataFlow
    = Unidirectional
    | Duplex
  deriving (Eq, Ord, Show)


-- | Boolean like type which indicates if the timeout on 'OutboundStateDuplex'
-- has expired.
data TimeoutExpired = Expired | Ticking
  deriving (Eq, Ord, Show)



-- | Either unnegotiated or negotiated unidirectional or duplex connections.
-- This is not a static property of a connection.  It is used by 'PrunePolicy'.
--
-- Note: the order matters, it can be used by a 'PickPolicy', e.g.
-- 'simplePickPolicy'.
--
data ConnectionType
    -- | An unnegotiated connection.
    --
    = UnnegotiatedConn !Provenance

    -- | An inbound idle connection.
    --
    | InboundIdleConn !DataFlow

    -- | An outbound idle connection.
    --
    | OutboundIdleConn !DataFlow

    -- | A negotiated connection, which is used in only one direction indicated
    -- by 'Provenance'.  The connection could itself negotiated either 'Duplex'
    -- or 'Unidirectional' data flow.
    --
    | NegotiatedConn   !Provenance !DataFlow

    -- | A connection which is running in full duplex mode.
    --
    | DuplexConn
    deriving (Eq, Ord, Show)


-- $promise
--
-- Promise interface, backed by a `StrictTMVar`.
--
-- Making two separate interfaces: 'PromiseWriter' and 'PromiseReader' allows us
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
  deriving (Show)

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
--   on @'Network.Mux.Mode'@.
--


-- | Handler action is started with asynchronous exceptions masked; this allows
-- to install exception handlers in an async-safe way.
--
newtype MaskedAction m a = MaskedAction {
    runWithUnmask :: (forall x. m x -> m x) -> m a
  }


-- | MaskedAction which is executed by thread designated for a given connection.
--
-- 'PromiseWriter' allows to notify the 'ConnectionManager' about the result of
-- handshake negotiation.
--
-- Note: 'PromiseWriter' could be replaced with an stm action which is
-- accessing the 'TVar' which holds state of the connection.
--
type ConnectionHandlerFn handlerTrace socket peerAddr handle handleError versionNumber versionData m
     = (versionData -> versionData)
    -> socket
    -> PromiseWriter m (Either handleError (HandshakeConnectionResult handle (versionNumber, versionData)))
    -> Tracer m handlerTrace
    -> ConnectionId peerAddr
    -> (DiffTime -> socket -> Maybe (Mux.ReadBuffer m) -> m (Mux.Bearer m))
    -> ((Maybe (Mux.ReadBuffer m) -> m ()) -> m ())
    -> MaskedAction m ()

data HandshakeConnectionResult handle version
  -- | Handshake saw a query.
  --
  = HandshakeConnectionQuery

  -- | Handshake resulted in a connection and version.
  --
  | HandshakeConnectionResult handle version

-- | Connection handler action.  It is index by @muxMode :: 'Network.Mux.Mode'@.
-- There's one 'ConnectionHandlerFn' per provenance, possibly limited by
-- @muxMode@.
--
newtype ConnectionHandler muxMode handlerTrace socket peerAddr handle handleError versionNumber versionData m =
    ConnectionHandler {
        -- | Connection handler.
        --
        connectionHandler ::
          WithMuxTuple muxMode
            (ConnectionHandlerFn handlerTrace socket peerAddr handle handleError versionNumber versionData m)
      }


-- | Boolean like type
--
data Inactive =
    Active MiniProtocolDir
  | Inactive
  deriving (Eq, Show)


-- | Exception which where caught in the connection thread and were re-thrown in
-- the main thread by the 'rethrowPolicy'.
--
data ExceptionInHandler where
    ExceptionInHandler :: forall peerAddr.
                          (Typeable peerAddr, Show peerAddr)
                       => !peerAddr
                       -> !SomeException
                       -> ExceptionInHandler

instance Show ExceptionInHandler where
    show (ExceptionInHandler peerAddr e) = "ExceptionInHandler "
                                        ++ show peerAddr
                                        ++ " "
                                        ++ show e
instance Exception ExceptionInHandler


-- | Data type used to classify 'handleErrors'.
--
data HandleErrorType =
    -- | Handshake negotiation failed.  This is not a protocol error.
    HandshakeFailure

    -- | Handshake protocol error.  This should include timeout errors or any
    -- IO errors.
  | HandshakeProtocolViolation


-- | 'PrunePolicy' allows to pick a select peers from which we will disconnect
-- (we use @TCP@ reset).  The chosen connections will be terminated by the
-- connection manger once it detects that there are too many inbound
-- connections.
--
type PrunePolicy peerAddr = StdGen
                         -> Map (ConnectionId peerAddr) ConnectionType
                         -> Int
                         -> Set (ConnectionId peerAddr)


-- | The simplest 'PrunePolicy', it should only be used for tests.
--
simplePrunePolicy :: Ord peerAddr
                  => PrunePolicy peerAddr
simplePrunePolicy _ m n =
    Set.fromList
  . map fst
  . take n
  . sortOn snd
  . Map.toList
  $ m



-- | Custom either type for result of various methods.
--
data OperationResult a
    = UnsupportedState !AbstractState
    | OperationSuccess !a
    | TerminatedConnection !AbstractState
    deriving (Show, Functor)


resultInState :: OperationResult AbstractState -> AbstractState
resultInState (UnsupportedState     st) = st
resultInState (OperationSuccess     st) = st
resultInState (TerminatedConnection st) = st


-- | Return value of 'releaseInboundConnection' to inform the caller about
-- the transition.
--
data DemotedToColdRemoteTr =
    -- | @Commit^{dataFlow}@ transition from @'InboundIdleState' dataFlow@.
    --
    CommitTr

    -- | Either @DemotedToCold^{Remote}@ transition from @'DuplexState'@, or
    -- a level triggered @Awake^{Duplex}_{Local}@ transition.  In both cases
    -- the server must keep the responder side of all protocols ready.
  | KeepTr
  deriving Show


-- | Result of 'acquireOutboundConnection' or 'includeInboundConnection'.
--
data Connected peerAddr handle handleError =
    -- | We are connected and mux is running.
    --
    Connected    !(ConnectionId peerAddr) !DataFlow !handle

    -- | There was an error during handshake negotiation.
    --
    -- /Implementation detail:/ we return @'Maybe' handleError@, rather than
    -- 'handleError'.  In case of an existing inbound connection, the
    -- implementation of 'acquireOutboundConnection' is awaiting on handshake
    -- through the connection state.  The 'TerminatingState' or
    -- 'TerminatedState' are not only used for handshake errors, but also for
    -- normal termination, hence the @'Maybe'@.  We could await on
    -- update from the handshake instead, but this would introduce a race
    -- between inbound \/ outbound threads.
    --
  | Disconnected !(ConnectionId peerAddr) !(Maybe handleError)


type AcquireOutboundConnection peerAddr handle handleError m
    = DiffusionMode -> peerAddr -> m (Connected peerAddr handle handleError)
type IncludeInboundConnection socket peerAddr handle handleError m
    = Word32
    -- ^ inbound connections hard limit.
    -- NOTE: Check TODO over at includeInboundConnectionImpl
    -- definition.
    -> socket -> ConnectionId peerAddr -> m (Connected peerAddr handle handleError)


-- | Outbound connection manager API.
--
data OutboundConnectionManager (muxMode :: Mux.Mode) socket peerAddr handle handleError m where
    OutboundConnectionManager
      :: HasInitiator muxMode ~ True
      => { ocmAcquireConnection :: AcquireOutboundConnection peerAddr handle handleError m
         , ocmReleaseConnection :: ConnectionId peerAddr -> m (OperationResult AbstractState)
         }
      -> OutboundConnectionManager muxMode socket peerAddr handle handleError m

-- | Inbound connection manager API.  For a server implementation we also need
-- to know how many connections are now managed by the connection manager.
--
-- This type is an internal detail of 'Ouroboros.Network.ConnectionManager'
--
data InboundConnectionManager (muxMode :: Mux.Mode) socket peerAddr handle handleError m where
    InboundConnectionManager
      :: HasResponder muxMode ~ True
      => { icmIncludeConnection    :: IncludeInboundConnection socket peerAddr handle handleError m
         , icmReleaseConnection    :: ConnectionId peerAddr -> m (OperationResult DemotedToColdRemoteTr)
         , icmPromotedToWarmRemote :: ConnectionId peerAddr -> m (OperationResult AbstractState)
         , icmDemotedToColdRemote
                                   :: ConnectionId peerAddr -> m (OperationResult AbstractState)
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
data ConnectionManager (muxMode :: Mux.Mode) socket peerAddr handle handleError m =
    ConnectionManager {
        getConnectionManager
          :: WithMuxMode
              muxMode
              (OutboundConnectionManager muxMode socket peerAddr handle handleError m)
              (InboundConnectionManager  muxMode socket peerAddr handle handleError m),

        readState
          :: STM m (ConnMap peerAddr AbstractState),

        -- | This STM action will block until the given connection is fully
        -- closed/terminated. If the connection manager doesn't have any connection to
        -- that peer it won't block.
        waitForOutboundDemotion
          :: ConnectionId peerAddr
          -> STM m ()
      }

--
-- ConnectionManager API
--

-- | Include outbound connection into 'ConnectionManager'.
--
--   This executes:
--
-- * \(Reserve\) to \(Negotiated^{*}_{Outbound}\) transitions
-- * \(PromotedToWarm^{Duplex}_{Local}\) transition
-- * \(Awake^{Duplex}_{Local}\) transition
acquireOutboundConnection
    :: HasInitiator muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> AcquireOutboundConnection        peerAddr handle handleError m
acquireOutboundConnection =
    ocmAcquireConnection . withInitiatorMode . getConnectionManager

-- | Release outbound connection.
--
--   This executes:
--
-- * \(DemotedToCold^{*}_{Local}\) transitions
releaseOutboundConnection
    :: HasInitiator muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> ConnectionId peerAddr
    -> m (OperationResult AbstractState)
    -- ^ reports the from-state.
releaseOutboundConnection =
    ocmReleaseConnection . withInitiatorMode . getConnectionManager

-- | Notify the 'ConnectionManager' that a remote end promoted us to a
-- /warm peer/.
--
-- This executes either:
--
-- * \(PromotedToWarm^{Duplex}_{Remote}\) transition,
-- * \(Awake^{*}_{Remote}\) transition
--
-- from the specification.
--
promotedToWarmRemote
    :: HasResponder muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> ConnectionId peerAddr -> m (OperationResult AbstractState)
promotedToWarmRemote =
    icmPromotedToWarmRemote . withResponderMode . getConnectionManager

-- | Notify the 'ConnectionManager' that a remote end demoted us to a /cold
-- peer/.
--
-- This executes:
--
-- * \(DemotedToCold^{*}_{Remote}\) transition.
--
-- This method is idempotent.
--
demotedToColdRemote
    :: HasResponder muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> ConnectionId peerAddr -> m (OperationResult AbstractState)
demotedToColdRemote =
    icmDemotedToColdRemote . withResponderMode . getConnectionManager

-- | Include an inbound connection into 'ConnectionManager'.
--   This executes:
--
-- * \(Reserve\) to \(Negotiated^{*}_{Outbound}\) transitions
-- * \(PromotedToWarm^{Duplex}_{Local}\) transition
-- * \(Awake^{Duplex}_{Local}\) transition
includeInboundConnection
    :: HasResponder muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> IncludeInboundConnection  socket peerAddr handle handleError m
includeInboundConnection =
    icmIncludeConnection . withResponderMode . getConnectionManager

-- | Release inbound connection. Returns if the operation was successful.
--
-- This executes:
--
-- * \(Commit*{*}\) transition
-- * \(TimeoutExpired\) transition
releaseInboundConnection
    :: HasResponder muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> ConnectionId peerAddr -> m (OperationResult DemotedToColdRemoteTr)
releaseInboundConnection =
    icmReleaseConnection . withResponderMode . getConnectionManager

-- | Number of connections tracked by the server.
--
numberOfConnections
    :: HasResponder muxMode ~ True
    => ConnectionManager muxMode socket peerAddr handle handleError m
    -> STM m Int
numberOfConnections =
    icmNumberOfConnections . withResponderMode . getConnectionManager


--
-- Errors
--


-- | Useful for tracing and error messages.
--
data AbstractState =
    -- | Unknown connection.  This state indicates the connection manager
    -- removed this connection from its state.
      UnknownConnectionSt
    | ReservedOutboundSt
    | UnnegotiatedSt !Provenance
    | InboundIdleSt  !DataFlow
    | InboundSt      !DataFlow
    | OutboundUniSt
    | OutboundDupSt  !TimeoutExpired
    | OutboundIdleSt !DataFlow
    | DuplexSt
    | WaitRemoteIdleSt
    | TerminatingSt
    | TerminatedSt
    deriving (Eq, Ord, Show)


-- | Counters for tracing and analysis purposes
--
data ConnectionManagerCounters = ConnectionManagerCounters {
      fullDuplexConns     :: !Int, -- ^ number of full duplex connections
      duplexConns         :: !Int, -- ^ number of negotiated duplex connections
                                   --   (including DuplexState connections)
      unidirectionalConns :: !Int, -- ^ number of negotiated unidirectional connections
      inboundConns        :: !Int, -- ^ number of inbound connections
      outboundConns       :: !Int  -- ^ number of outbound connections
    }
  deriving (Show, Eq, Ord)

instance Semigroup ConnectionManagerCounters where
    ConnectionManagerCounters fd1 d1 s1 i1 o1 <> ConnectionManagerCounters fd2 d2 s2 i2 o2 =
      ConnectionManagerCounters (fd1 + fd2) (d1 + d2) (s1 + s2) (i1 + i2) (o1 + o2)

instance Monoid ConnectionManagerCounters where
    mempty = ConnectionManagerCounters 0 0 0 0 0

-- | Exceptions used by 'ConnectionManager'.
--
data ConnectionManagerError peerAddr
    -- | A connection manager was asked for an outbound connection and there
    -- either exists a connection used in outbound direction or a reservation
    -- for an outbound connection.
    --
    = ConnectionExists      !Provenance !peerAddr    !CallStack

    -- | Connection manager was asked for an outbound connection which turned
    -- out to be unidirectional inbound, and thus it cannot be re-used..
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
    | ForbiddenOperation    !peerAddr !AbstractState !CallStack

    -- | A connection does not exists.  Only thrown when an existing connection
    -- was expected.
    --
    | UnknownPeer           !peerAddr                !CallStack
    deriving (Show)


instance ( Show peerAddr
         , Typeable peerAddr ) => Exception (ConnectionManagerError peerAddr) where

    toException   = connectionManagerErrorToException
    fromException = connectionManagerErrorFromException

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
      concat [ "Forbidden operation "
             , show peerAddr
             , " "
             , show reason
             , "\n"
             , prettyCallStack cs
             ]
    displayException (UnknownPeer peerAddr cs) =
      concat [ "UnknownPeer "
             , show peerAddr
             , "\n"
             , prettyCallStack cs
             ]


-- | Existential wrapper for @'ConnectionManagerError' peerAddr@.  It allows to
-- use 'fromException', without being bothered about the address type.
--
data SomeConnectionManagerError =
    forall addr. ( Typeable addr
                 , Show addr
                 )
    => SomeConnectionManagerError !(ConnectionManagerError addr)

instance Show SomeConnectionManagerError where
    show (SomeConnectionManagerError e) = show e

instance Exception SomeConnectionManagerError where
    displayException (SomeConnectionManagerError e) = displayException e

connectionManagerErrorToException :: (Typeable addr, Show addr)
                                  => ConnectionManagerError addr
                                  -> SomeException
connectionManagerErrorToException = toException . SomeConnectionManagerError

connectionManagerErrorFromException :: (Typeable addr, Show addr)
                                    => SomeException
                                    -> Maybe (ConnectionManagerError addr)
connectionManagerErrorFromException x = do
    SomeConnectionManagerError a <- fromException x
    cast a

--
-- Tracing
--

-- | 'AssertionLocation' contains constructors that situate the location of the tracing so
-- one can be sure where the assertion came from as well as the all relevant information.
--
data AssertionLocation peerAddr
  = ReleaseInboundConnection  !(Maybe (ConnectionId peerAddr)) !AbstractState
  | AcquireOutboundConnection !(Maybe (ConnectionId peerAddr)) !AbstractState
  | ReleaseOutboundConnection !(Maybe (ConnectionId peerAddr)) !AbstractState
  | PromotedToWarmRemote      !(Maybe (ConnectionId peerAddr)) !AbstractState
  | DemotedToColdRemote       !(Maybe (ConnectionId peerAddr)) !AbstractState
  deriving Show


-- | A custom version of 'Maybe' type, which allows to explicitly represent
-- connections which are not registered by the connection manager.
--
data MaybeUnknown state
    -- | Known connection in 'state'
    = Known !state
    -- | There is a possible race condition between connection finalizer and
    -- either inbound or outbound connection registration.  If that happens we
    -- use 'Race' constructor.
    | Race  !state
    -- | Connection is is not known to the connection manager.
    | Unknown
  deriving (Show, Functor)


data Transition' state = Transition
    { fromState :: !state
    , toState   :: !state
    }
  deriving (Eq, Functor)

instance Show state
      => Show (Transition' state) where
    show Transition { fromState, toState } =
      concat [ show fromState
             , " → "
             , show toState
             ]

type Transition state   = Transition' (MaybeUnknown state)
type AbstractTransition = Transition' AbstractState

mkTransition :: state -> state -> Transition state
mkTransition from to = Transition { fromState = Known from
                                  , toState   = Known to
                                  }

mkAbsTransition :: AbstractState -> AbstractState -> AbstractTransition
mkAbsTransition from to = Transition { fromState = from
                                     , toState   = to
                                     }

data TransitionTrace' id state = TransitionTrace
    { ttPeerAddr   :: id -- ^ an id of a connection, not necessarily an address, e.g. `State.ConnStateId`
    , ttTransition :: Transition' state
    }
  deriving Functor

instance (Show peerAddr, Show state)
      =>  Show (TransitionTrace' peerAddr state) where
    show (TransitionTrace addr tr) =
      concat [ "TransitionTrace @("
             , show addr
             , ") ("
             , show tr
             , ")"
             ]

type TransitionTrace peerAddr state = TransitionTrace' peerAddr (MaybeUnknown state)
type AbstractTransitionTrace peerAddr = TransitionTrace' peerAddr AbstractState
