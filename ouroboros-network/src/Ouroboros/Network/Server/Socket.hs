{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- `accept` is shadowed, but so what?
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Server.Socket
  ( BeginConnection
  , HandleConnection (..)
  , CompleteConnection
  , Result (..)
  , Main
  , run

  , Socket (..)
  , ioSocket
  ) where

import Control.Exception (SomeException (..), mask, mask_, finally, onException, try)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, forM_, join)
import Control.Monad.Class.MonadTime (Time, getMonotonicTime)
import Data.Set (Set)
import qualified Data.Set as Set

-- | Abstraction of something that can provide connections.
-- A `Network.Socket` can be used to get a
-- `Socket SockAddr (Channel IO Lazy.ByteString)`
-- It's not defined in here, though, because we don't want the dependency
-- on typed-protocols or even on network.
data Socket addr channel = Socket
  { acceptConnection :: IO (addr, channel, IO ())
    -- ^ The address, a channel, IO to close the channel.
  }

-- | Expected to be useful for testing.
ioSocket :: IO (addr, channel) -> Socket addr channel
ioSocket io = Socket
  { acceptConnection = do
      (addr, channel) <- io
      pure (addr, channel, pure ())
  }

type StatusVar st = STM.TVar st

-- | What to do with a new connection: reject it and give a new state, or
-- accept it and give a new state with a continuation to run against the
-- resulting channel.
-- See also `CompleteConnection`, which is run for every connection when it finishes, and
-- can also update the state.
data HandleConnection channel st r where
  Reject :: !st -> HandleConnection channel st r
  Accept :: !st -> !(channel -> IO r) -> HandleConnection channel st r

-- | What to do on a new connection: accept and run this `IO`, or reject.
type BeginConnection addr channel st r = Time IO -> addr -> st -> STM (HandleConnection channel st r)

-- | A call back which runs when application starts;
--
-- It is needed only because 'BeginConnection' does not have access to the
-- thread which runs the application.
--
type ApplicationStart addr st = addr -> Async () -> st -> STM st

-- | How to update state when a connection finishes. Can use `throwSTM` to
-- terminate the server.
type CompleteConnection addr st r = Result addr r -> st -> STM (st, IO ())

-- | Given a current state, `retry` unless you want to stop the server.
-- When this transaction returns, any running threads spawned by the server
-- will be killed.
--
-- It's possible that a connection is accepted after the main thread
-- returns, but before the server stops. In that case, it will be killed, and
-- the `CompleteConnection` will not run against it.
type Main st t = st -> STM t

-- | To avoid repeatedly blocking on the set of all running threads (a
-- potentially very large STM transaction) the results come in by way of a
-- `TQueue`. Using a queue rather than, say, a `TMVar`, also finesses a
-- potential deadlock when shutting down the server and killing spawned threads:
-- the server can stop pulling from the queue, without causing the child
-- threads to hang attempting to write to it.
type ResultQ addr r = STM.TQueue (Result addr r)

-- | The product of a spawned thread. We catch all (even async) exceptions.
data Result addr r = Result
  { resultThread :: !(Async ())
  , resultAddr   :: !addr
  , resultTime   :: !(Time IO)
  , resultValue  :: !(Either SomeException r)
  }

-- | The set of all spawned threads. Used for waiting or cancelling them when
-- the server shuts down.
type ThreadsVar = STM.TVar (Set (Async ()))


-- | The action runs inside `try`, and when it finishes, puts its result
-- into the `ResultQ`. Takes care of inserting/deleting from the `ThreadsVar`.
--
-- Async exceptions are masked to ensure that if the thread is spawned, it
-- always gets into the `ThreadsVar`. Exceptions are unmasked in the
-- spawned thread.
spawnOne
  :: addr
  -> StatusVar st
  -> ResultQ addr r
  -> ThreadsVar
  -> ApplicationStart addr st
  -> IO r
  -> IO ()
spawnOne remoteAddr statusVar resQ threadsVar applicationStart io = mask_ $ do
  rec let threadAction = \unmask -> do
            STM.atomically $
                  STM.readTVar statusVar
              >>= applicationStart remoteAddr thread
              >>= (STM.writeTVar statusVar $!)
            val <- try (unmask io)
            t <- getMonotonicTime
            -- No matter what the exception, async or sync, this will not
            -- deadlock, since we use a `TQueue`. If the server kills its
            -- children, and stops clearing the queue, it will be collected
            -- shortly thereafter, so no problem.
            STM.atomically $ STM.writeTQueue resQ (Result thread remoteAddr t val)
      thread <- Async.asyncWithUnmask $ \unmask ->
          threadAction unmask
  -- The main loop `connectionTx` will remove this entry from the set, once
  -- it receives the result.
  STM.atomically $ STM.modifyTVar' threadsVar (Set.insert thread)


-- | The accept thread is controlled entirely by the `accept` call. To
-- stop it, whether normally or exceptionally, it must be killed by an async
-- exception, or the exception callback here must re-throw.
acceptLoop
  :: ResultQ addr r
  -> ThreadsVar
  -> StatusVar st
  -> BeginConnection addr channel st r
  -> ApplicationStart addr st
  -> (SomeException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket addr channel
  -> IO x
acceptLoop resQ threadsVar statusVar beginConnection applicationStart acceptException socket = forever $
  acceptOne resQ threadsVar statusVar beginConnection applicationStart acceptException socket

-- | Accept once from the socket, use the `Accept` to make a decision (accept
-- or reject), and spawn the thread if accepted.
acceptOne
  :: ResultQ addr r
  -> ThreadsVar
  -> StatusVar st
  -> BeginConnection addr channel st r
  -> ApplicationStart addr st
  -> (SomeException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket addr channel
  -> IO ()
acceptOne resQ threadsVar statusVar beginConnection applicationStart acceptException socket = mask $ \restore -> do
  -- mask is to assure that every socket is closed.
  outcome <- try (restore (acceptConnection socket))
  case outcome of
    Left ex -> restore (acceptException ex)
    Right (addr, channel, close) -> do
      -- Decide whether to accept or reject, using the current state, and
      -- update it according to the decision.
      t <- getMonotonicTime
      let decision = STM.atomically $ do
            st <- STM.readTVar statusVar
            !handleConn <- beginConnection t addr st
            case handleConn of
              Reject st' -> do
                STM.writeTVar statusVar st'
                pure Nothing
              Accept st' io -> do
                STM.writeTVar statusVar st'
                pure $ Just io
      -- this could be interrupted, so we use `onException` to close the
      -- socket.
      choice <- decision `onException` close
      case choice of
        Nothing -> close
        Just io -> spawnOne addr statusVar resQ threadsVar applicationStart (io channel `finally` close)

-- | Main server loop, which runs alongside the `acceptLoop`. It waits for
-- the results of connection threads, as well as the `Main` action, which
-- determines when/if the server should stop.
mainLoop
  :: forall addr st r t .
     ResultQ addr r
  -> ThreadsVar
  -> StatusVar st
  -> CompleteConnection addr st r
  -> Main st t
  -> IO t
mainLoop resQ threadsVar statusVar complete main =
  join (STM.atomically $ mainTx `STM.orElse` connectionTx)

  where

  -- Sample the status, and run the main action. If it does not retry, then
  -- the `mainLoop` finishes with `pure t` where `t` is the main action result.
  mainTx :: STM (IO t)
  mainTx = do
    st <- STM.readTVar statusVar
    t <- main st
    pure $ pure t

  -- Wait for some connection to finish, update the state with its result,
  -- then recurse onto `mainLoop`.
  connectionTx :: STM (IO t)
  connectionTx = do
    result <- STM.readTQueue resQ
    st <- STM.readTVar statusVar
    (!st', io) <- complete result st
    STM.writeTVar statusVar st'
    -- It was inserted by `spawnOne`.
    STM.modifyTVar' threadsVar (Set.delete (resultThread result))
    pure $ io >> mainLoop resQ threadsVar statusVar complete main

-- | Run a server.
run
  :: Socket addr channel
  -> (SomeException -> IO ())
  -> BeginConnection addr channel st r
  -> ApplicationStart addr st
  -> CompleteConnection addr st r
  -> Main st t
  -> STM.TVar st
  -> IO t
run socket acceptException beginConnection applicationStart complete main statusVar = do
  resQ <- STM.newTQueueIO
  threadsVar <- STM.newTVarIO Set.empty
  let acceptLoopDo = acceptLoop resQ threadsVar statusVar beginConnection applicationStart acceptException socket
      -- The accept loop is killed when the main loop stops.
      mainDo = Async.withAsync acceptLoopDo $ \_ ->
        mainLoop resQ threadsVar statusVar complete main
      killChildren = do
        children <- STM.atomically $ STM.readTVar threadsVar
        forM_ (Set.toList children) Async.cancel
  -- After both the main and accept loop have been killed, any remaining
  -- spawned threads are cancelled.
  mainDo `finally` killChildren
