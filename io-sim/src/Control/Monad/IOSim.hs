{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Control.Monad.IOSim (
  SimM,
  runSim,
  runSimStrictShutdown,
  Failure(..),
  runSimTrace,
  runSimTraceST,
  liftST,
  VTime(..),
  VTimeDuration(..),
  ThreadId,
  Trace(..),
  TraceEvent(..),
  ) where

import           Prelude hiding (read)

import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

import qualified Data.List as L
import           Data.Fixed (Micro)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set

import           Control.Exception (Exception(..), SomeException, assert)
import qualified System.IO.Error as IO.Error (userError)

import           Control.Monad
import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Strict as StrictST
import           Data.STRef.Lazy

import           Control.Monad.Fail as MonadFail
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM hiding (TVar)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadProbe hiding (Probe)
import qualified Control.Monad.Class.MonadProbe as MonadProbe

{-# ANN module "HLint: ignore Use readTVarIO" #-}


--
-- Simulation monad for protocol testing
--

newtype SimM s a = SimM { unSimM :: forall r. (a -> SimA s r) -> SimA s r }

runSimM :: SimM s a -> SimA s a
runSimM (SimM k) = k Return

data SimA s a where
  Return       :: a -> SimA s a

  Say          :: String -> SimA s b -> SimA s b
  Output       :: Probe s o -> o -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetTime      :: (VTime -> SimA s b) -> SimA s b
  NewTimeout   :: VTimeDuration -> (Timeout (SimM s) -> SimA s b) -> SimA s b
  UpdateTimeout:: Timeout (SimM s) -> VTimeDuration -> SimA s b -> SimA s b
  CancelTimeout:: Timeout (SimM s) -> SimA s b -> SimA s b

  Throw        :: Exception e => e -> SimA s a

  Fork         :: SimM s () -> SimA s b -> SimA s b
  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

newtype STM s a = STM { unSTM :: forall r. (a -> StmA s r) -> StmA s r }

runSTM :: STM s a -> StmA s a
runSTM (STM k) = k ReturnStm

data StmA s a where
  ReturnStm    :: a -> StmA s a
--FailStm      :: ...

  NewTVar      :: x -> (TVar s x -> StmA s b) -> StmA s b
  ReadTVar     :: TVar s a -> (a -> StmA s b) -> StmA s b
  WriteTVar    :: TVar s a ->  a -> StmA s b  -> StmA s b
  Retry        :: StmA s b


type ProbeTrace a = [(VTime, a)]

newtype Probe s a = Probe (STRef s (ProbeTrace a))


instance Functor (SimM s) where
    {-# INLINE fmap #-}
    fmap f = \d -> SimM $ \k -> unSimM d (k . f)

instance Applicative (SimM s) where
    {-# INLINE pure #-}
    pure = \x -> SimM $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> SimM $ \k ->
                        unSimM df (\f -> unSimM dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> SimM $ \k -> unSimM dm (\_ -> unSimM dn k)

instance Monad (SimM s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> SimM $ \k -> unSimM dm (\m -> unSimM (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

    fail = MonadFail.fail


instance Functor (STM s) where
    {-# INLINE fmap #-}
    fmap f = \d -> STM $ \k -> unSTM d (k . f)

instance Applicative (STM s) where
    {-# INLINE pure #-}
    pure = \x -> STM $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> STM $ \k ->
                        unSTM df (\f -> unSTM dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> STM $ \k -> unSTM dm (\_ -> unSTM dn k)

instance Monad (STM s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> STM $ \k -> unSTM dm (\m -> unSTM (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)


--
-- Monad class instances
--

newtype VTime         = VTime Micro
  deriving (Eq, Ord, Show)
newtype VTimeDuration = VTimeDuration Micro
  deriving (Eq, Ord, Show, Num, Real, Fractional)

instance TimeMeasure VTime where
  type Duration VTime = VTimeDuration

  diffTime (VTime t) (VTime t') = VTimeDuration (t-t')
  addTime  (VTimeDuration d) (VTime t) = VTime (t+d)
  zero = VTime 0

instance MonadFail (SimM s) where
  fail msg = SimM $ \_ -> Throw (IO.Error.userError msg)

instance MonadSay (SimM s) where
  say msg = SimM $ \k -> Say msg (k ())

instance MonadFork (SimM s) where
  fork task = SimM $ \k -> Fork task (k ())

instance MonadSTM (SimM s) where
  type Tr    (SimM s)   = STM s
  type TVar  (SimM s)   = TVar s
  type TMVar (SimM s)   = TMVarDefault (SimM s)
  type TBQueue (SimM s) = TBQueueDefault (SimM s)

  atomically action = SimM $ \k -> Atomically action k

  newTVar         x = STM $ \k -> NewTVar x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry

  newTMVar          = newTMVarDefault
  newTMVarIO        = newTMVarIODefault
  newEmptyTMVar     = newEmptyTMVarDefault
  newEmptyTMVarIO   = newEmptyTMVarIODefault
  takeTMVar         = takeTMVarDefault
  tryTakeTMVar      = tryTakeTMVarDefault
  putTMVar          = putTMVarDefault
  tryPutTMVar       = tryPutTMVarDefault
  readTMVar         = readTMVarDefault
  tryReadTMVar      = tryReadTMVarDefault
  swapTMVar         = swapTMVarDefault
  isEmptyTMVar      = isEmptyTMVarDefault

  newTBQueue        = newTBQueueDefault
  readTBQueue       = readTBQueueDefault
  writeTBQueue      = writeTBQueueDefault

instance MonadST (SimM s) where
  withLiftST f = f liftST

liftST :: StrictST.ST s a -> SimM s a
liftST action = SimM $ \k -> LiftST action k

instance MonadTime (SimM s) where
  type Time (SimM s) = VTime

  getMonotonicTime = SimM $ \k -> GetTime k

instance MonadTimer (SimM s) where
  data Timeout (SimM s) = Timeout !(TVar s TimeoutState) !TimeoutId

  readTimeout (Timeout var _key) = readTVar var

  newTimeout      d = SimM $ \k -> NewTimeout      d k
  updateTimeout t d = SimM $ \k -> UpdateTimeout t d (k ())
  cancelTimeout t   = SimM $ \k -> CancelTimeout t   (k ())

instance MonadProbe (SimM s) where
  type Probe (SimM s) = Probe s
  probeOutput p o = SimM $ \k -> Output p o (k ())

instance MonadRunProbe (SimM s) (ST s) where
  newProbe = Probe <$> newSTRef []
  readProbe (Probe p) = reverse <$> readSTRef p
  runM = void . runSimTraceST

--
-- Simulation interpreter
--

data Thread s a = Thread {
       threadId     :: ThreadId,
       threadAction :: SimA s (ThreadResult a)
     }

data ThreadResult a = MainThreadResult a
                    | ForkThreadResult

newtype ThreadId  = ThreadId  Int deriving (Eq, Ord, Enum, Show)
newtype TVarId    = TVarId    Int deriving (Eq, Ord, Enum, Show)
newtype TimeoutId = TimeoutId Int deriving (Eq, Ord, Enum, Show)

data Trace a = Trace !VTime !ThreadId !TraceEvent (Trace a)
             | TraceMainReturn    !VTime a             ![ThreadId]
             | TraceMainException !VTime SomeException ![ThreadId]
             | TraceDeadlock      !VTime               ![ThreadId]

data TraceEvent
  = EventFail String
  | EventSay  String
  | EventThrow SomeException
  | EventThreadForked ThreadId
  | EventThreadStopped                 -- terminated normally
  | EventThreadException SomeException -- terminated due to unhandled exception
  | EventTxComitted    [TVarId] -- tx wrote to these
                       [TVarId] -- and created these
  | EventTxBlocked     [TVarId] -- tx blocked reading these
  | EventTxWakeup      [TVarId] -- changed vars causing retry
  | EventTimerCreated   TimeoutId TVarId VTime
  | EventTimerUpdated   TimeoutId        VTime
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId
  deriving Show

-- | Simulation termination with failure
--
data Failure =
       -- | The main thread terminated with an exception
       FailureException SomeException

       -- | The threads all deadlocked
     | FailureDeadlock

       -- | The main thread terminated normally but other threads were still
       -- alive, and strict shutdown checking was requested.
       -- See 'runSimStrictShutdown'
     | FailureSloppyShutdown
  deriving Show

runSim :: forall a. (forall s. SimM s a) -> Either Failure a
runSim initialThread =
    collectSimResult False (runSimTrace initialThread)

-- | Like 'runSim' but also fail if when the main thread terminates, there
-- are other threads still running or blocked. If one is trying to follow
-- a strict thread cleanup policy then this helps testing for that.
--
runSimStrictShutdown :: forall a. (forall s. SimM s a) -> Either Failure a
runSimStrictShutdown initialThread =
    collectSimResult True (runSimTrace initialThread)

collectSimResult :: Bool -> Trace a -> Either Failure a
collectSimResult strict = go
  where
    go (Trace _ _ _ t)                      = go t
    go (TraceMainReturn _ _ (_:_)) | strict = Left FailureSloppyShutdown
    go (TraceMainReturn _ x _)              = Right x
    go (TraceMainException _ e _)           = Left (FailureException e)
    go (TraceDeadlock   _   _)              = Left FailureDeadlock


runSimTrace :: forall a. (forall s. SimM s a) -> Trace a
runSimTrace initialThread = runST (runSimTraceST initialThread)

runSimTraceST :: forall s a. SimM s a -> ST s (Trace a)
runSimTraceST initialThread = schedule (initialState initialThread)

data SimState s a = SimState {
       runqueue :: ![Thread s a],
       blocked  :: !(Map ThreadId (Thread s a)),
       curTime  :: !VTime,
       timers   :: !(OrdPSQ TimeoutId VTime (TVar s TimeoutState)),
       nextTid  :: !ThreadId,   -- ^ next unused 'ThreadId'
       nextVid  :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid :: !TimeoutId   -- ^ next unused 'TimeoutId'
     }

initialState :: forall s a. SimM s a -> SimState s a
initialState initialThread =
  SimState {
    runqueue = [Thread (ThreadId 0) initialThread'],
    blocked  = Map.empty,
    curTime  = VTime 0,
    timers   = PSQ.empty,
    nextTid  = ThreadId 1,
    nextVid  = TVarId 0,
    nextTmid = TimeoutId 0
  }
  where
    initialThread' :: SimA s (ThreadResult a)
    initialThread' = runSimM (fmap MainThreadResult initialThread)

schedule :: SimState s a -> ST s (Trace a)
schedule simstate@SimState {
           runqueue = thread@(Thread tid action):remaining,
           blocked, timers, nextTid, nextVid, nextTmid,
           curTime  = time
         } =
  case action of

    Return (MainThreadResult a) -> do
      -- the main thread is done, so we're done
      -- even if other threads are still running
      return (TraceMainReturn time a remainingThreadIds)
      where
        remainingThreadIds = (map threadId remaining) ++ Map.keys blocked

    Return ForkThreadResult -> do
      -- this thread is done
      trace <- schedule simstate { runqueue = remaining }
      return (Trace time tid EventThreadStopped trace)

    Throw e | tid == ThreadId 0 -> do
      -- Exception in main thread stops whole sim (no catch yet)
      let e' = toException e
      return (Trace time tid (EventThrow e') $
              Trace time tid (EventThreadException e') $
              TraceMainException time e' remainingThreadIds)
      where
        remainingThreadIds = (map threadId remaining) ++ Map.keys blocked

    Throw e -> do
      -- stop this thread on failure (no catch yet)
      trace <- schedule simstate { runqueue = remaining }
      let e' = toException e
      return (Trace time tid (EventThrow e') $
              Trace time tid (EventThreadException e') trace)

    Say msg k -> do
      let thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining }
      return (Trace time tid (EventSay msg) trace)

    Output (Probe p) o k -> do
      modifySTRef p ((time, o):)
      let thread' = Thread tid k
      schedule simstate { runqueue = thread':remaining }

    LiftST st k -> do
      x <- strictToLazyST st
      let thread' = Thread tid (k x)
      schedule simstate { runqueue = thread':remaining }

    GetTime k -> do
      let thread' = Thread tid (k time)
      schedule simstate { runqueue = thread':remaining }

    NewTimeout d k -> do
      tvar <- execNewTVar nextVid TimeoutPending
      let expiry  = d `addTime` time
          timeout = Timeout tvar nextTmid
          timers' = PSQ.insert nextTmid expiry tvar timers
          thread' = Thread tid (k timeout)
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers'
                                 , nextVid  = succ nextVid
                                 , nextTmid = succ nextTmid }
      return (Trace time tid (EventTimerCreated nextTmid nextVid expiry) trace)

    UpdateTimeout (Timeout _tvar tmid) d k -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimout  Nothing       = ((), Nothing)
          updateTimout (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimout tmid timers)
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return (Trace time tid (EventTimerUpdated tmid expiry) trace)

    CancelTimeout (Timeout _tvar tmid) k -> do
      let timers' = PSQ.delete tmid timers
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return (Trace time tid (EventTimerCancelled tmid) trace)

    Fork a k -> do
      let thread'  = Thread tid k
          thread'' = Thread tid' (runSimM (a >> return ForkThreadResult))
          tid'     = nextTid
      trace <- schedule simstate
        { runqueue = thread':remaining ++ [thread'']
        , nextTid  = succ nextTid
        }
      return (Trace time tid (EventThreadForked tid') trace)

    Atomically a k -> do
      (res, nextVid') <- execAtomically tid nextVid (runSTM a)
      case res of
        StmTxComitted x written wakeup -> do
          let thread'   = Thread tid (k x)
              unblocked = catMaybes [ Map.lookup tid' blocked | (tid', _) <- wakeup ]
              -- We don't interrupt runnable threads to provide fairness
              -- anywhere else. We do it here by putting the tx that comitted
              -- a transaction to the back of the runqueue, behind all other
              -- runnable threads, and behind the unblocked threads.
              -- For testing, we should have a more sophiscated policy to show
              -- that algorithms are not sensitive to the exact policy, so long
              -- as it is a fair policy (all runnable threads eventually run).
              runqueue  = remaining ++ reverse (thread' : unblocked)
          trace <- schedule simstate {
                     runqueue,
                     blocked  = blocked `Map.difference`
                                Map.fromList [ (tid', ()) | (tid', _) <- wakeup ],
                     nextVid  = nextVid'
                   }
          return $
            Trace time tid (EventTxComitted written [nextVid..pred nextVid']) $
            traceMany
              [ (time, tid', EventTxWakeup vids) | (tid', vids) <- wakeup ]
              trace

        StmTxBlocked vids -> do
          let blocked' = Map.insert tid thread blocked
          trace <- schedule simstate {
                     runqueue = remaining,
                     blocked  = blocked',
                     nextVid  = nextVid'
                   }
          return (Trace time tid (EventTxBlocked vids) trace)

-- no runnable threads, advance the time to the next timer event, or stop.
schedule simstate@SimState {
           runqueue = [],
           blocked, timers,
           curTime = time
         } =

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return (TraceDeadlock time (Map.keys blocked))

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        wakeup <- execAtomically' (runSTM $ mapM_ timeoutAction fired)

        let runnable  = catMaybes    [ Map.lookup tid' blocked
                                     | (tid', _) <- wakeup ]
            unblocked = Map.fromList [ (tid', ())
                                     | (tid', _) <- wakeup ]
        trace <- schedule simstate {
                   runqueue = reverse runnable,
                   blocked  = blocked `Map.difference` unblocked,
                   curTime  = time',
                   timers   = timers'
                 }
        return $
          traceMany ([ (time', ThreadId (-1), EventTimerExpired tmid)
                     | tmid <- tmids ]
                  ++ [ (time', tid', EventTxWakeup vids)
                     | (tid', vids) <- wakeup ])
                    trace
  where
    timeoutAction var = do
      x <- readTVar var
      case x of
        TimeoutPending   -> writeTVar var TimeoutFired
        TimeoutFired     -> error "MonadTimer(Sim): invariant violation"
        TimeoutCancelled -> return ()

removeMinimums :: (Ord k, Ord p)
               => OrdPSQ k p a
               -> Maybe ([k], p, [a], OrdPSQ k p a)
removeMinimums = \psq ->
    case PSQ.minView psq of
      Nothing              -> Nothing
      Just (k, p, x, psq') -> Just (collectAll [k] p [x] psq')
  where
    collectAll ks p xs psq =
      case PSQ.minView psq of
        Just (k, p', x, psq')
          | p == p' -> collectAll (k:ks) p (x:xs) psq'
        _           -> (reverse ks, p, reverse xs, psq)

traceMany :: [(VTime, ThreadId, TraceEvent)] -> Trace a -> Trace a
traceMany []                      trace = trace
traceMany ((time, tid, event):ts) trace =
    Trace time tid event (traceMany ts trace)


--
-- Executing STM Transactions
--

data TVar s a = TVar !TVarId
                     !(STRef s a)          -- current value
                     !(STRef s (Maybe a))  -- saved revert value
                     !(STRef s [ThreadId]) -- threads blocked on read

data StmTxResult s a = StmTxComitted a [TVarId] [(ThreadId, [TVarId])] -- wake up
                     | StmTxBlocked    [TVarId]               -- blocked on

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

execAtomically :: ThreadId
               -> TVarId
               -> StmA s a
               -> ST s (StmTxResult s a, TVarId)
execAtomically mytid = go [] []
  where
    go :: [SomeTVar s] -> [SomeTVar s] -> TVarId
       -> StmA s a -> ST s (StmTxResult s a, TVarId)
    go read written nextVid action = case action of
      ReturnStm x     -> do (vids, tids) <- finaliseCommit written
                            return (StmTxComitted x vids tids, nextVid)
      Retry           -> do vids <- finaliseRetry read written
                            return (StmTxBlocked vids, nextVid)
      NewTVar x k     -> do v <- execNewTVar nextVid x
                            go read written (succ nextVid) (k v)
      ReadTVar v k    -> do x <- execReadTVar v
                            go (SomeTVar v : read) written nextVid (k x)
      WriteTVar v x k -> do execWriteTVar v x
                            go read (SomeTVar v : written) nextVid k

    -- Revert all the TVar writes and put this thread on the blocked queue for
    -- all the TVars read in this transaction
    finaliseRetry :: [SomeTVar s] -> [SomeTVar s] -> ST s [TVarId]
    finaliseRetry read written = do
      sequence_ [ revertTVar  tvar | SomeTVar tvar <- reverse written ]
      sequence_ [ blockOnTVar tvar | SomeTVar tvar <- read ]
      return [ vid | SomeTVar (TVar vid _ _ _) <- read ]

    blockOnTVar :: TVar s a -> ST s ()
    blockOnTVar (TVar _vid _vcur _vsaved blocked) =
      --TODO: avoid duplicates!
      modifySTRef blocked (mytid:)


execAtomically' :: StmA s () -> ST s [(ThreadId, [TVarId])]
execAtomically' = go []
  where
    go :: [SomeTVar s] -> StmA s () -> ST s [(ThreadId, [TVarId])]
    go written action = case action of
      ReturnStm ()    -> do (_vids, tids) <- finaliseCommit written
                            return tids
      ReadTVar v k    -> do x <- execReadTVar v
                            go written (k x)
      WriteTVar v x k -> do execWriteTVar v x
                            go (SomeTVar v : written) k
      _ -> error "execAtomically': only for special case of reads and writes"


-- Commit each TVar and collect all the other threads blocked on the TVars
-- written to in this transaction.
finaliseCommit :: [SomeTVar s] -> ST s ([TVarId], [(ThreadId, [TVarId])])
finaliseCommit written = do
  tidss <- sequence [ (,) vid <$> commitTVar tvar
                    | SomeTVar tvar@(TVar vid _ _ _) <- written ]
  let -- for each thread, what var writes woke it up
      wokeVars    = Map.fromListWith (\l r -> L.nub $ l ++ r)
                      [ (tid, [vid]) | (vid, tids) <- tidss, tid <- tids ]
      -- threads to wake up, in wake up order, with assoicated vars
      wokeThreads = [ (tid, wokeVars Map.! tid)
                    | tid <- ordNub [ tid | (_, tids) <- tidss, tid <- reverse tids ]
                    ]
      writtenVids = [ vid | SomeTVar (TVar vid _ _ _) <- written ]
  return (writtenVids, wokeThreads)

execNewTVar :: TVarId -> a -> ST s (TVar s a)
execNewTVar nextVid x = do
  vcur    <- newSTRef x
  vsaved  <- newSTRef Nothing
  blocked <- newSTRef []
  return (TVar nextVid vcur vsaved blocked)

execReadTVar :: TVar s a -> ST s a
execReadTVar (TVar _vid vcur _vsaved _blocked) =
  readSTRef vcur

execWriteTVar :: TVar s a -> a -> ST s ()
execWriteTVar (TVar _vid vcur vsaved _blocked) x = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first write, save original value
    Nothing -> writeSTRef vsaved . Just =<< readSTRef vcur
    -- on subsequent writes do nothing
    Just _  -> return ()
  writeSTRef vcur x

revertTVar :: TVar s a -> ST s ()
revertTVar (TVar _vid vcur vsaved _blocked) = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first revert, restore original value
    Just saved -> writeSTRef vcur saved >> writeSTRef vsaved Nothing
    -- on subsequent reverts do nothing
    Nothing    -> return ()

commitTVar :: TVar s a -> ST s [ThreadId]
commitTVar (TVar _vid _vcur vsaved blocked) = do
  msaved <- readSTRef vsaved
  case msaved of
    -- on first commit, forget saved value and collect blocked threads
    Just _  -> writeSTRef vsaved Nothing >> readSTRef blocked
    -- on subsequent commits do nothing
    Nothing -> return []

ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `Set.member` s = go s xs
      | otherwise        = x : go (Set.insert x s) xs


--
-- Examples
--
{-
example0 :: (MonadSay m, MonadTimer m, MonadSTM m) => m ()
example0 = do
  say "starting"
  t <- atomically (newTVar (0 :: Int))
  fork $ threadDelay 2 >> do
    say "timer fired!"
    atomically $
      writeTVar t 1
  atomically $ do
    x <- readTVar t
    unless (x == 1) retry
  say "main done"

example1 :: SimM s ()
example1 = do
  say "starting"
  chan <- atomically (newTVar ([] :: [Int]))
  fork $ forever $ do
    atomically $ do
      x <- readTVar chan
      writeTVar chan (1:x)
  fork $ forever $ do
    atomically $ do
      x <- readTVar chan
      writeTVar chan (2:x)
  -- This demonstrates an interesting aspect of the simulator:
  -- it is quite happy to let you do an infinite amount of work
  -- in zero time. These two busy loops do not progress in time.

  threadDelay 1 >> do
    x <- atomically $ readTVar chan
    say $ show x

-- the trace should contain "1" followed by "2"
example2 :: (MonadSay m, MonadSTM m) => m ()
example2 = do
  say "starting"
  v <- atomically $ newTVar Nothing
  fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "1"
  fork $ do
    atomically $ do
      x <- readTVar v
      case x of
        Nothing -> retry
        Just _  -> return ()
    say "2"
  atomically $ writeTVar v (Just ())
-}
