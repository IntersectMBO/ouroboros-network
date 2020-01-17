{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Ouroboros.Network.Connections.Concurrent
  ( Decision (..)
  , Handler (..)
  , concurrent
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (finally, mask, onException)
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Ouroboros.Network.Connections.Types hiding (Decision (..))
import qualified Ouroboros.Network.Connections.Types as Types

data Reject reason (p :: Provenance) where
  -- | A remote connection may be rejected if there is already a connection
  -- established at its identifier.
  -- Locally-initiated connections cannot fail in this way, because the
  -- connection manager will use the existing remotely-initiated one.
  --
  -- For TCP sockets, and connection identified by local and remote
  -- socket addresses, this should never happen (TCP spec sorts out crossed
  -- connections).
  --
  -- Indeed, the choice of connection identifier and resource type should be
  -- done in such a way that duplicates do not arise, i.e. the identifier
  -- actually does uniquely identify the resource.
  Duplicate      :: Reject reason Incoming
  -- | Rejected according to the domain-specific handler routine.
  DomainSpecific :: reason -> Reject reason any

data Accept handle (p :: Provenance) = Accepted handle

-- | Returned by the definition continuation of a `concurrent` `Connections`,
-- this determines whether a connection is accepted or rejected.
-- If it's accepted, a `Handler` must be constructed, with the `Async` of
-- its own action in scope (use it only lazily).
data Decision reason handle where
  Reject :: reason -> Decision reason handle
  Accept :: (Async () -> IO (Handler handle)) -> Decision reason handle

-- | An action to run for each connection, and a handle giving an interface
-- to that action, probably using STM for synchronization.
-- The connection for which this handler is spawned will be closed whenever
-- the action returns, exceptionally or not.
data Handler handle = Handler
  { handle :: !handle
  , action :: !(IO ())
  }

data Connection handle = Connection
  { connectionThread :: !(Async ())
  , connectionHandle :: !handle
  }

-- | Each connection gets a unique number.
type ConnectionNumber = Word32

data NumberedConnection handle = NumberedConnection
  { number     :: !ConnectionNumber
  , connection :: !(Connection handle)
  }

-- | State of the connections manager. Will be held in an MVar.
data State identifier handle = State
  { connectionMap    :: !(Map identifier (NumberedConnection handle))
  -- | The next available connection number.
  -- Will be incremented for each new connection.
  -- If it overflows, then there could be duplicate connection numbers, but
  -- that's unlikely to happen and if it does, it won't cause failures, because
  -- the ConnectionNumber is only used to make visibile the difference between
  -- connections from the same identifier which have gone down and then come
  -- up again.
  , connectionNumber :: !ConnectionNumber
  }

insertConnection
  :: ( Ord identifier )
  => identifier
  -> Connection handle
  -> State identifier handle
  -> State identifier handle
insertConnection identifier conn state = state
  { connectionMap    = Map.insert identifier numConn (connectionMap state)
  , connectionNumber = connectionNumber state + 1
  }
  where
  !numConn = NumberedConnection
    { number     = connectionNumber state
    , connection = conn
    }

removeConnection
  :: ( Ord identifier )
  => identifier
  -> State identifier handle
  -> State identifier handle
removeConnection identifier state =
  let alteration mentry = case mentry of
        -- removeConnection is called by the thread that was spawned by the
        -- handler before insertConnection happened for this identifier. If
        -- there's nothing here then there's a bug in this module.
        Nothing -> error "bug: entry vanished"
        Just _  -> Nothing
  in  state { connectionMap = Map.alter alteration identifier (connectionMap state) }

-- | Generic concurrent connection manager in which at most one connection for
-- a given identifier is allowed. New connections will re-use existing
-- connections, and existing connections will be rejected if there is already
-- one here.
--
-- For example, we could choose
--   resource := Socket
--   connectionId := exists sockType . (SockAddr sockType, SockAddr sockType)
-- and this would guarantee at most one connection per socket address pair,
-- when the sockType is IPv* and it's a TCP socket.
-- However, for Unix domain sockets, this is no good, because unnamed sockets
-- all have (==) SockAddr values. A richer connection identifier type would be
-- needed, and any servers or clients calling into this concurrent connection
-- manager with Unix domain sockets would be required to come up with unique
-- identifiers for unnamed ones (probably by taking the file descriptor number).
concurrent
  :: forall connectionId resource handle reason t .
     ( Ord connectionId )
  => (Provenance -> connectionId -> resource -> IO (Decision reason handle))
  -- ^ A callback to run for each connection. The `handle` gives an interface
  -- to that connection, allowing for inspection and control etc. of whatever
  -- it's doing.
  -- If this throws an exception, it is re-thrown, so that calls to
  -- `include` will get them.
  -- Non-exceptional domain-specific reasons to reject a connection are given
  -- in the `Left` variant.
  -> (Connections connectionId resource (Reject reason) (Accept handle) IO -> IO t)
  -> IO t
concurrent withResource k = do
  stateVar :: MVar (State connectionId handle) <- newMVar $ State
    { connectionMap    = Map.empty
    , connectionNumber = 0
    }
  let connections = Connections { include = includeOne stateVar }
  k connections `finally` killThreads stateVar

  where

  -- Exception handling in here should ensure that if the handler is succesfully
  -- created, then it ends up in the shared state.
  --
  -- FIXME this one definition is too complex. Try to factor it into simpler
  -- pieces.
  includeOne
    :: MVar (State connectionId handle)
    -> connectionId
    -> Resource provenance IO resource
    -> IO (Types.Decision provenance (Reject reason) (Accept handle))
  includeOne stateVar connId resource = mask $ \restore -> modifyMVar stateVar $ \state ->
    case Map.lookup connId (connectionMap state) of
      Nothing   -> case resource of
        Existing res closeResource -> do
          -- User must ensure that the handler does not take very long to.
          -- set up. It should just be creating shared state.
          -- Do not catch exceptions: the caller is responsible for closing
          -- the resource in case of exception.
          outcome <- restore (withResource Incoming connId res)
          case outcome of
            Reject reason -> pure (state, Types.Rejected (DomainSpecific reason))
            Accept mkhandler -> do
              -- If there was no exception, we are now responsible for closing
              -- the resource. That's done in a finally after the handler's
              -- action.
              rec let cleanup = do
                        closeResource
                        modifyMVar_ stateVar $ \state' ->
                          let !state'' = removeConnection connId state'
                          in  pure state''
                  handler <- mkhandler thread
                  thread <- asyncWithUnmask $ \unmask ->
                    unmask (action handler) `finally` cleanup
              let conn = Connection
                    { connectionThread = thread
                    , connectionHandle = handle handler
                    }
                  !state' = insertConnection connId conn state
              pure (state', Types.Accepted (Accepted (connectionHandle conn)))
        New acquire release -> do
          -- If acquiring the resource fails, we just re-throw the exception.
          -- Thus `include`ing a new resource is just like bracketing the
          -- acquire and release: any exception in acquire will be re-thrown.
          res <- restore acquire
          outcome <- restore (withResource Outgoing connId res)
                       `onException`
                       release res
          case outcome of
            Reject reason -> do
              release res
              pure (state, Types.Rejected (DomainSpecific reason))
            Accept mkhandler -> do
              -- Just like for existing connections, the resource will be closed
              -- when the handler's action finishes.
              rec let cleanup = do
                        release res
                        modifyMVar_ stateVar $ \state' ->
                          let !state'' = removeConnection connId state'
                          in  pure state''
                  handler <- mkhandler thread
                  thread <- asyncWithUnmask $ \unmask ->
                    unmask (action handler) `finally` cleanup
              let conn = Connection
                    { connectionThread = thread
                    , connectionHandle = handle handler
                    }
                  !state' = insertConnection connId conn state
              pure (state', Types.Accepted (Accepted (connectionHandle conn)))
      Just numConn -> case resource of
        -- Do not call _close; the caller is responsible for that, and knows
        -- it because we give `Rejected`.
        Existing _resource _close   -> pure (state, Types.Rejected Duplicate)
        -- Give a handle to the existing connection.
        New      _acquire  _release -> pure (state, Types.Accepted (Accepted h))
          where
          h = connectionHandle (connection numConn)

  -- NB: to kill the connection threads, we must not do so while holding
  -- the MVar, because there are `finally` handlers on each thread which
  -- attempt to get the MVar and remove themselves.
  --
  -- It is incorrect use to include a connection after the continuation for
  -- this `concurrent` call has ended (user must ensure that).
  -- TODO extend the State type to include a "closed" variant that we can
  -- swap in here to make it more user friendly: give an informative error
  -- when a user tries to include a connection after the manager has gone
  -- down.
  killThreads :: MVar (State connectionId handle) -> IO ()
  killThreads stateVar = do
    state <- readMVar stateVar
    forM_ (Map.toList (connectionMap state)) $ \(_, conn) ->
      cancel (connectionThread (connection conn))
