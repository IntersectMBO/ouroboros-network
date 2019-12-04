{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

-- `accept` is shadowed, but so what?
{-# OPTIONS_GHC "-fno-warn-name-shadowing" #-}

module Ouroboros.Network.Server.Socket
  ( BeginConnection
  , HandleConnection (..)
  , ApplicationStart
  , CompleteConnection
  , CompleteApplicationResult (..)
  , Result (..)
  , Main
  , run

  , Socket (..)
  , ioSocket
  ) where

import Data.Foldable (traverse_)
import Data.Proxy (Proxy(Proxy))
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Control.Monad (forever, join)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Class.MonadFork (ThreadId)
import Control.Monad.Class.MonadAsync as Async
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadSTM as STM
import Control.Monad.Class.MonadTime
import Control.Tracer (Tracer(..), traceWith)
import Control.Exception (SomeException, IOException)

import Ouroboros.Network.ErrorPolicy
         (CompleteApplicationResult (..), WithAddr, ErrorPolicyTrace)


-- | Abstraction of something that can provide connections.
-- A `Network.Socket` can be used to get a
-- `Socket SockAddr (Channel IO Lazy.ByteString)`
-- It's not defined in here, though, because we don't want the dependency
-- on typed-protocols or even on network.
data Socket m addr channel = Socket
  { acceptConnection :: m (addr, channel, m ())
    -- ^ The address, a channel, IO to close the channel.
  }

-- | Expected to be useful for testing.
ioSocket :: Monad m => m (addr, channel) -> Socket m addr channel
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
data HandleConnection m channel st r where
  Reject :: !st -> HandleConnection m channel st r
  Accept :: !st -> !(channel -> m r) -> HandleConnection m channel st r

-- | What to do on a new connection: accept and run this `IO`, or reject.
type BeginConnection m addr channel st r = Time -> addr -> st -> STM m (HandleConnection m channel st r)

-- | A call back which runs when application starts;
--
-- It is needed only because 'BeginConnection' does not have access to the
-- thread which runs the application.
--
type ApplicationStart m addr st = addr -> Async m () -> st -> STM m st

-- | How to update state when a connection finishes. Can use `throwSTM` to
-- terminate the server.
--
-- TODO: remove 'async', use `Async m ()` from 'MonadAsync'.
type CompleteConnection m addr st tr r =
    Result m addr r -> st -> STM m (CompleteApplicationResult m addr st)

-- | Given a current state, `retry` unless you want to stop the server.
-- When this transaction returns, any running threads spawned by the server
-- will be killed.
--
-- It's possible that a connection is accepted after the main thread
-- returns, but before the server stops. In that case, it will be killed, and
-- the `CompleteConnection` will not run against it.
type Main m st t = st -> STM m t

-- | To avoid repeatedly blocking on the set of all running threads (a
-- potentially very large STM transaction) the results come in by way of a
-- `TQueue`. Using a queue rather than, say, a `TMVar`, also finesses a
-- potential deadlock when shutting down the server and killing spawned threads:
-- the server can stop pulling from the queue, without causing the child
-- threads to hang attempting to write to it.
type ResultQ m addr r = STM.TQueue m (Result m addr r)

-- | The product of a spawned thread. We catch all (even async) exceptions.
data Result m addr r = Result
  { resultThread :: !(Async m ())
  , resultAddr   :: !addr
  , resultTime   :: !Time 
  , resultValue  :: !(Either SomeException r)
  }

-- | The set of all spawned threads. Used for waiting or cancelling them when
-- the server shuts down.
type ThreadsVar m = STM.TVar m (Map (ThreadId m) (Async m ()))


-- | The action runs inside `try`, and when it finishes, puts its result
-- into the `ResultQ`. Takes care of inserting/deleting from the `ThreadsVar`.
--
-- Async exceptions are masked to ensure that if the thread is spawned, it
-- always gets into the `ThreadsVar`. Exceptions are unmasked in the
-- spawned thread.
spawnOne
  :: forall m addr st r.
     (MonadFix m, MonadAsync m, MonadMask m, MonadTime m)
  => addr
  -> StatusVar m st
  -> ResultQ m addr r
  -> ThreadsVar m
  -> ApplicationStart m addr st
  -> m r
  -> m ()
spawnOne remoteAddr statusVar resQ threadsVar applicationStart io =
  mask $ \unmask -> do
  rec let threadAction = do
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
      thread <- Async.async threadAction
  -- The main loop `connectionTx` will remove this entry from the set, once
  -- it receives the result.
  STM.atomically $
    let tid = asyncThreadId (Proxy :: Proxy m) thread in
    STM.modifyTVar' threadsVar (Map.insert tid thread)


-- | The accept thread is controlled entirely by the `accept` call. To
-- stop it, whether normally or exceptionally, it must be killed by an async
-- exception, or the exception callback here must re-throw.
acceptLoop
  :: (MonadFix m, MonadAsync m, MonadMask m, MonadTime m)
  => ResultQ m addr r
  -> ThreadsVar m
  -> StatusVar m st
  -> BeginConnection m addr channel st r
  -> ApplicationStart m addr st
  -> (IOException -> m ()) -- ^ Exception on `Socket.accept`.
  -> Socket m addr channel
  -> m x
acceptLoop resQ threadsVar statusVar beginConnection applicationStart acceptException socket = forever $
  acceptOne resQ threadsVar statusVar beginConnection applicationStart acceptException socket

-- | Accept once from the socket, use the `Accept` to make a decision (accept
-- or reject), and spawn the thread if accepted.
acceptOne
  :: forall m addr channel st r.
     (MonadFix m, MonadAsync m, MonadMask m, MonadTime m)
  => ResultQ m addr r
  -> ThreadsVar m
  -> StatusVar m st
  -> BeginConnection m addr channel st r
  -> ApplicationStart m addr st
  -> (IOException -> m ()) -- ^ Exception on `Socket.accept`.
  -> Socket m addr channel
  -> m ()
acceptOne resQ threadsVar statusVar beginConnection applicationStart acceptException socket = mask $ \restore -> do
  -- mask is to assure that every socket is closed.
  outcome <- try (restore (acceptConnection socket))
  case outcome :: Either IOException (addr, channel, m ()) of
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
  :: forall m addr st tr r t .
     MonadAsync m
  => Tracer m (WithAddr addr ErrorPolicyTrace)
  -> ResultQ m addr r
  -> ThreadsVar m
  -> StatusVar m st
  -> CompleteConnection m addr st tr r
  -> Main m st t
  -> m t
mainLoop errorPolicyTrace resQ threadsVar statusVar complete main =
  join (STM.atomically $ mainTx `STM.orElse` connectionTx)

  where

  -- Sample the status, and run the main action. If it does not retry, then
  -- the `mainLoop` finishes with `pure t` where `t` is the main action result.
  mainTx :: STM m (m t)
  mainTx = do
    st <- STM.readTVar statusVar
    t <- main st
    pure $ pure t

  -- Wait for some connection to finish, update the state with its result,
  -- then recurse onto `mainLoop`.
  connectionTx :: STM m (m t)
  connectionTx = do
    result <- STM.readTQueue resQ
    st <- STM.readTVar statusVar
    CompleteApplicationResult
      { carState
      , carThreads
      , carTrace
      } <- complete result st
    -- 'CompleteConnectionResult' is strict in 'ccrState', thus we write
    -- evaluted state to 'statusVar'
    STM.writeTVar statusVar carState
    -- It was inserted by `spawnOne`.
    let tid = asyncThreadId (Proxy :: Proxy m) (resultThread result)
    STM.modifyTVar' threadsVar (Map.delete tid)
    pure $ do
      traverse_ Async.cancel carThreads
      traverse_ (traceWith errorPolicyTrace) carTrace
      mainLoop errorPolicyTrace resQ threadsVar statusVar complete main

-- | Run a server.
run
  :: (MonadFix m, MonadAsync m, MonadMask m, MonadTime m)
  => Tracer m (WithAddr addr ErrorPolicyTrace)
  -- TODO: extend this trace to trace server action (this might be useful for
  -- debugging)
  -> Socket m addr channel
  -> (IOException -> m ())
  -> BeginConnection m addr channel st r
  -> ApplicationStart m addr st
  -> CompleteConnection m addr st tr r
  -> Main m st t
  -> STM.TVar m st
  -> m t
run errroPolicyTrace socket acceptException beginConnection applicationStart complete main statusVar = do
  resQ <- STM.atomically STM.newTQueue
  threadsVar <- STM.newTVarM Map.empty
  let acceptLoopDo = acceptLoop resQ threadsVar statusVar beginConnection applicationStart acceptException socket
      -- The accept loop is killed when the main loop stops.
      mainDo = Async.withAsync acceptLoopDo $ \_ ->
        mainLoop errroPolicyTrace resQ threadsVar statusVar complete main
      killChildren = do
        children <- STM.atomically $ STM.readTVar threadsVar
        mapM_ Async.cancel (Map.elems children)
  -- After both the main and accept loop have been killed, any remaining
  -- spawned threads are cancelled.
  mainDo `finally` killChildren
