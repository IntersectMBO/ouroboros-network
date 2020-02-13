{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Connections.Concurrent
  ( Accept (..)
  , Reject (..)
  , Decision (..)
  , Handler (..)
  , ExceptionInHandler (..)
  , concurrent
  ) where

import Control.Exception (Exception, fromException)
import Control.Monad.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadFork, ThreadId, myThreadId, throwTo)
import Control.Monad.Class.MonadThrow hiding (handle)
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Ouroboros.Network.Connections.Types hiding (Decision (..))
import qualified Ouroboros.Network.Connections.Types as Types

data Reject reject (p :: Provenance) where
  -- | A remote connection may be rejected if there is already a connection
  -- established at its identifier.
  -- Locally-initiated connections cannot fail in this way, because the
  -- connection manager will use the existing remotely-initiated one.
  --
  -- For TCP sockets, and connection identified by local and remote
  -- socket addresses, this should never happen if you bind connecting sockets
  -- to the same host/port as the listening socket (TCP spec sorts out crossed
  -- connections).
  --
  -- Indeed, the choice of connection identifier and resource type should be
  -- done in such a way that duplicates do not arise, i.e. the identifier
  -- actually does uniquely identify the resource.
  Duplicate      :: Reject reject Remote
  -- | Rejected according to the domain-specific handler routine.
  DomainSpecific :: reject p -> Reject reject p

data Accept handle (p :: Provenance) where
  Accepted :: handle -> Accept handle p

-- | Returned by the definition continuation of a `concurrent` `Connections`,
-- this determines whether a connection is accepted or rejected.
-- If it's accepted, a `Handler` must be constructed, with the `Async` of
-- its own action in scope (use it only lazily).
data Decision m (provenance :: Provenance) reject handle where
  Reject :: reject provenance -> Decision m provenance reject handle
  Accept :: (Async m () -> m (Handler m handle)) -> Decision m provenance reject handle

-- | An action to run for each connection, and a handle giving an interface
-- to that action, probably using STM for synchronization.
-- The connection for which this handler is spawned will be closed whenever
-- the action returns, exceptionally or not.
data Handler m handle = Handler
  { handle :: !handle
  , action :: !(m ())
  }

data Connection m handle = Connection
  { connectionThread :: !(Async m ())
  , connectionHandle :: !handle
  }

-- | Each connection gets a unique number.
type ConnectionNumber = Word32

data NumberedConnection m handle = NumberedConnection
  { number     :: !ConnectionNumber
  , connection :: !(Connection m handle)
  }

-- | State of the connections manager. Will be held in an MVar.
data State m identifier handle = State
  { connectionMap    :: !(Map identifier (NumberedConnection m handle))
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
  -> Connection m handle
  -> State m identifier handle
  -> State m identifier handle
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
  -> State m identifier handle
  -> State m identifier handle
removeConnection identifier state =
  let alteration mentry = case mentry of
        -- removeConnection is called by the thread that was spawned by the
        -- handler before insertConnection happened for this identifier. If
        -- there's nothing here then there's a bug in this module.
        Nothing -> error "bug: entry vanished"
        Just _  -> Nothing
  in  state { connectionMap = Map.alter alteration identifier (connectionMap state) }

-- | Exception type for exceptions thrown by threads spawned by the `concurrent`
-- connections manager. An exception of this type will be `throwTo`ed to the
-- thread which calls `concurrent` whenever a spawned thread throws any
-- exception. You can catch all or some of these in the usual way, by
-- specializing the `e` parameter.
--
-- NB: AsyncCancelled will _not_ be re-thrown, since this is used by the
-- connections manager to kill running threads when it's time to shut down.
-- This is dodgy but hopefully it will work well enough.
--
-- TODO include some identifying information? The connection number?
data ExceptionInHandler e where
  ExceptionInHandler :: !e -> ExceptionInHandler e

deriving instance Show e => Show (ExceptionInHandler e)
instance Exception e => Exception (ExceptionInHandler e)

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
  :: forall connectionId resource request reject handle m t .
     ( MonadMask m
     , MonadAsync m
     , MonadFork m -- Not implied by MonadAsync
     , MonadSTM m
     , MonadFix m
     , Ord connectionId
     )
  => (forall provenance .
         Initiated provenance
      -> connectionId
      -> resource
      -> request provenance
      -> m (Decision m provenance reject handle)
     )
  -- ^ A callback to run for each connection. The `handle` gives an interface
  -- to that connection, allowing for inspection and control etc. of whatever
  -- it's doing.
  -- If this throws an exception, it is re-thrown, so that calls to
  -- `include` will get them.
  -- Non-exceptional domain-specific reasons to reject a connection are given
  -- in the rejection variant.
  -> (Connections connectionId resource request (Reject reject) (Accept handle) m -> m t)
  -> m t
concurrent withResource k = do
  tid <- myThreadId
  stateVar :: MVar m (State m connectionId handle) <- newMVar $ State
    { connectionMap    = Map.empty
    , connectionNumber = 0
    }
  let connections = Connections { include = includeOne tid stateVar }
  k connections `finally` killThreads stateVar

  where

  -- Throws an exception to the master thread (caller of `concurrent`) wrapped
  -- in a special type. This is always called with async exceptions masked.
  -- AsyncCancelled is never re-thrown, since this system uses `cancel` to
  -- shut down threads when the continuation finishes.
  --
  -- TODO come up with a better way to do this.
  --
  -- TODO make it possible for users of `concurrent` to wait for threads to
  -- finish rather than kill them?
  rethrow :: ThreadId m -> SomeException -> m x
  rethrow tid e = case fromException e of
    Just AsyncCancelled -> throwM e
    Nothing             -> throwTo tid (ExceptionInHandler e) >> throwM e

  -- Exception handling in here should ensure that if the handler is succesfully
  -- created, then it ends up in the shared state.
  --
  -- FIXME this one definition is too complex. Try to factor it into simpler
  -- pieces.
  includeOne
    :: ThreadId m
    -> MVar m (State m connectionId handle)
    -> connectionId
    -> Resource provenance m resource
    -> request provenance
    -> m (Types.Decision provenance (Reject reject) (Accept handle))
  includeOne tid stateVar connId resource request = mask $ \restore -> modifyMVar' stateVar $ \state ->
    case Map.lookup connId (connectionMap state) of
      Nothing   -> case resource of
        -- The caller gave an existing resource, and we don't have anything
        -- for this identifier. So, use `withResource` to get a handler.
        -- This action is intended to allow for setting up shared state etc.
        -- and should not be expensive. Exceptions thrown by it will not be
        -- caught here (will be re-thrown to the caller). In this case, the
        -- caller is responsible for closing the resource.
        --
        -- However, if it passes normally, the resulting handler action will
        -- be run in a thread and registered here. In this case, the caller
        -- must _not_ close the resource; we will ensure that happens when the
        -- handler ends (exceptionally or normally).
        Existing res closeResource -> do
          outcome <- restore (withResource Incoming connId res request)
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
                    (unmask (action handler) `finally` cleanup) `catch` rethrow tid
              let conn = Connection
                    { connectionThread = thread
                    , connectionHandle = handle handler
                    }
                  !state' = insertConnection connId conn state
              pure (state', Types.Accepted (Accepted (connectionHandle conn)))
        -- Caller indicates how to make a new resource. Similar story for the
        -- above Existing case, except that we create the resource first and
        -- ensure that it gets closed no matter what.
        New acquire release -> do
          -- If acquiring the resource fails, we just re-throw the exception.
          -- Thus `include`ing a new resource is just like bracketing the
          -- acquire and release: any exception in acquire will be re-thrown.
          res <- restore acquire
          outcome <- restore (withResource Outgoing connId res request)
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
                    (unmask (action handler) `finally` cleanup) `catch` rethrow tid
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
  killThreads :: MVar m (State m connectionId handle) -> m ()
  killThreads stateVar = do
    state <- readMVar stateVar
    forM_ (Map.toList (connectionMap state)) $ \(_, conn) ->
      cancel (connectionThread (connection conn))

-- IO sim stuff does not have MVars, so we use STM to approximate them
-- Hopefully this is not too much slower.
type MVar m = StrictTMVar m

newMVar :: (MonadSTM m) => a -> m (MVar m a)
newMVar = atomically . newTMVar

modifyMVar' :: (MonadSTM m, MonadMask m) => MVar m a -> (a -> m (a, b)) -> m b
modifyMVar' tmvar k = mask $ \restore -> do
  st <- atomically (takeTMVar tmvar)
  (!st', b) <- restore (k st) `onException` atomically (putTMVar tmvar st)
  atomically (putTMVar tmvar st')
  pure b

modifyMVar_ :: (MonadSTM m, MonadMask m) => MVar m a -> (a -> m a) -> m ()
modifyMVar_ tmvar k = mask $ \restore -> do
  st <- atomically (takeTMVar tmvar)
  !st' <- restore (k st) `onException` atomically (putTMVar tmvar st)
  atomically (putTMVar tmvar st')

readMVar :: (MonadSTM m) => MVar m a -> m a
readMVar tmvar = atomically (readTMVar tmvar)
