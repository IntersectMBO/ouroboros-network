{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- `accept` is shadowed, but so what?
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Server.Socket
  ( Accept
  , Complete
  , Main
  , Stop (..)
  , accept
  , reject
  , run

  , Socket (..)
  , ioSocket
  ) where

import Control.Exception (SomeException, mask, mask_, finally, onException, try)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, forM_, join)
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

-- | Status of a server, and whether or not new connections should be accepted
-- (no if Finished).
data Status st where
  Finished :: !st -> Status st
  Continue :: !st -> Status st

type StatusVar st = STM.TVar (Status st)

-- | What to do on a new connection: accept and run this `IO`, or reject.
type Accept addr channel st r = addr -> st -> STM (Maybe (channel -> IO r))

-- The following 2 equations explain the meaning of the `Maybe` in `Accept`.

reject :: Maybe (channel -> IO r)
reject = Nothing

accept :: (channel -> IO r) -> Maybe (channel -> IO r)
accept = Just

-- | How to update state when a connection finishes. Can use `throwSTM` to
-- terminate the server.
type Complete st r = Either SomeException r -> st -> STM st

-- | Given a current state, `retry` unless you want to stop the server. When
-- this transaction finishes, you can choose whether to kill any running
-- threads, or to wait for them. If waiting, `st -> t` will be used on the
-- final `st`, which may be different from the `st` given at first, because
-- threads that finish up may modify it.
type Main st t = st -> STM (Stop st t)

-- | See the comment on `Main`.
data Stop st t where
  -- | Don't wait for existing threads to finish.
  Now  :: t -> Stop st t
  -- | Do wait for existing threads to finish.
  Wait :: (st -> t) -> Stop st t

-- | To avoid repeatedly blocking on the set of all running threads (a
-- potentially very large STM transaction) the results come in by way of a
-- `TQueue`. Using a queue rather than, say, a `TMVar`, also finesses a
-- potential deadlock when shutting down the server and killing spawned threads:
-- the server can stop pulling from the queue, without causing the child
-- threads to hang attempting to write to it.
type ResultQ r = STM.TQueue (Result r)

-- | The product of a spawned thread. We catch all (even async) exceptions.
data Result r = Result
  { resultValue  :: !(Either SomeException r)
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
  :: ResultQ r
  -> ThreadsVar
  -> IO r
  -> IO ()
spawnOne resQ threadsVar io = mask_ $ do
  rec let threadAction = \unmask -> do
            val <- try (unmask io)
            -- No matter what the exception, async or sync, this will not
            -- deadlock, since we use a `TQueue`. If the server kills its
            -- children, and stops clearing the queue, it will be collected
            -- shortly thereafter, so no problem.
            STM.atomically $ do
              STM.writeTQueue resQ (Result val)
              STM.modifyTVar' threadsVar (Set.delete thread)
      thread <- Async.asyncWithUnmask threadAction
  STM.atomically $ STM.modifyTVar' threadsVar (Set.insert thread)

-- | The accept thread is controlled entirely by the `accept` call. To
-- stop it, whether normally or exceptionally, it must be killed by an async
-- exception, or the exception callback here must re-throw.
acceptLoop
  :: ResultQ r
  -> ThreadsVar
  -> StatusVar st
  -> Accept addr channel st r
  -> (SomeException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket addr channel
  -> IO x
acceptLoop resQ threadsVar statusVar accept acceptException socket = forever $
  acceptOne resQ threadsVar statusVar accept acceptException socket

-- | Accept once from the socket, use the `Accept` to make a decision (accept
-- or reject), and spawn the thread if accepted.
acceptOne
  :: ResultQ r
  -> ThreadsVar
  -> StatusVar st
  -> Accept addr channel st r
  -> (SomeException -> IO ()) -- ^ Exception on `Socket.accept`.
  -> Socket addr channel
  -> IO ()
acceptOne resQ threadsVar statusVar accept acceptException socket = mask $ \restore -> do
  -- mask is to assure that every socket is closed.
  outcome <- try (restore (acceptConnection socket))
  case outcome of
    Left ex -> restore (acceptException ex)
    Right (addr, channel, close) -> do
      -- Whether we accept depends upon the current status: never accept if
      -- it's 'Finished'; otherwise, use the `Accept` function.
      let decision = STM.atomically $ do
            status <- STM.readTVar statusVar
            case status of
              Finished _  -> pure Nothing
              Continue st -> accept addr st
      -- this could be interrupted, so we use `onException` to close the
      -- socket.
      chosen <- decision `onException` close
      case chosen of
        Nothing -> close
        Just io -> spawnOne resQ threadsVar (io channel `finally` close)

-- | Main server loop, which runs alongside the `acceptLoop`. It waits for
-- the results of connection threads, as well as the `Main` action, which
-- determines when/if the server should stop.
mainLoop
  :: forall st r t .
     ResultQ r
  -> ThreadsVar
  -> StatusVar st
  -> Complete st r
  -> Main st t
  -> IO t
mainLoop resQ threadsVar statusVar complete main =
  join (STM.atomically $ mainTx `STM.orElse` connectionTx)

  where

  mainTx :: STM (IO t)
  mainTx = do
    status <- STM.readTVar statusVar
    case status of
      Finished _ -> STM.retry
      Continue st -> do
        -- user-supplied transaction can retry if it doesn't want to close.
        stop <- main st
        STM.writeTVar statusVar (Finished st)
        case stop of
          Wait f -> pure $ waitShutdown f
          Now t  -> pure $ nowShutdown t

  connectionTx :: STM (IO t)
  connectionTx = do
    result <- STM.readTQueue resQ
    status <- STM.readTVar statusVar
    case status of
      Finished st -> do
        !st' <- complete (resultValue result) st
        STM.writeTVar statusVar (Finished st')
      Continue st -> do
        !st' <- complete (resultValue result) st
        STM.writeTVar statusVar (Continue st')
    pure (mainLoop resQ threadsVar statusVar complete main)

  waitShutdown :: (st -> t) -> IO t
  waitShutdown f = do
    children <- STM.atomically $ STM.readTVar threadsVar
    forM_ (Set.toList children) Async.wait
    status <- STM.atomically $ STM.readTVar statusVar
    case status of
      -- `waitShutdown` is only used after writing `Finished` to the `statusVar`,
      -- and nowhere is it ever switched from `Finished` back to `Continue`.
      Continue _ -> error "impossible"
      Finished st -> pure (f st)

  nowShutdown :: t -> IO t
  nowShutdown t = do
    children <- STM.atomically $ STM.readTVar threadsVar
    forM_ (Set.toList children) Async.cancel
    pure t

-- | Run a server.
run
  :: Socket addr channel
  -> (SomeException -> IO ())
  -> Accept addr channel st r
  -> Complete st r
  -> Main st t
  -> st
  -> IO t
run socket acceptException accept complete main st = do
  resQ <- STM.newTQueueIO
  threadsVar <- STM.newTVarIO Set.empty
  statusVar <- STM.newTVarIO (Continue st)
  let acceptLoopDo = acceptLoop resQ threadsVar statusVar accept acceptException socket
      mainDo = Async.withAsync acceptLoopDo $ \_ ->
        mainLoop resQ threadsVar statusVar complete main
      exceptionalShutdown = do
        children <- STM.atomically $ STM.readTVar threadsVar
        forM_ (Set.toList children) Async.cancel
  mainDo `onException` exceptionalShutdown

