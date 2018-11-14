{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Network.Sim {-(
  SimF,
  SimM,
  Probe,
  SimChan (..),
  runSimM,
  runSimMST,
  )-} where

import           Prelude hiding (read)

import           Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

import qualified Data.List as L
import           Data.Fixed (Micro)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set

import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Free (Free)
import           Control.Monad.Free as Free
import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Strict as StrictST
import           Data.STRef.Lazy
import           Numeric.Natural (Natural)

import           Ouroboros.Network.MonadClass.MonadFork
import           Ouroboros.Network.MonadClass.MonadSay
import           Ouroboros.Network.MonadClass.MonadST
import           Ouroboros.Network.MonadClass.MonadSTM hiding (TVar)
import qualified Ouroboros.Network.MonadClass.MonadSTM as MonadSTM
import           Ouroboros.Network.MonadClass.MonadTimer

{-# ANN module "HLint: ignore Use readTVarIO" #-}


--
-- Simulation monad for protocol testing
--

data SimF (s :: *) a where
  Fail         :: String -> SimF s a

  Say          :: String -> b -> SimF s b
  Output       :: Probe s o -> o -> b -> SimF s b

  LiftST       :: StrictST.ST s a -> (a -> b) -> SimF s b

  NewTimeout   :: VTimeDuration -> (Timeout (Free (SimF s)) -> b) -> SimF s b
  UpdateTimeout:: Timeout (Free (SimF s)) -> VTimeDuration -> b -> SimF s b
  CancelTimeout:: Timeout (Free (SimF s)) -> b -> SimF s b

  Fork         :: Free (SimF s) () -> b -> SimF s b
  Atomically   :: Free (StmF s) a -> (a -> b) -> SimF s b

data StmF (s :: *) a where

  NewTVar      :: x -> (TVar s x -> b) -> StmF s b
  ReadTVar     :: TVar s a -> (a -> b) -> StmF s b
  WriteTVar    :: TVar s a ->  a -> b  -> StmF s b
  Retry        :: StmF s b

type SimM s a = Free (SimF s) a
type STM  s   = Free (StmF s)

type ProbeTrace a = [(VTime, a)]

newtype Probe s a = Probe (STRef s (ProbeTrace a))

failSim :: String -> Free (SimF s) ()
failSim = Free.liftF . Fail

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

instance Functor (SimF s) where
  fmap _ (Fail f)         = Fail f
  fmap f (Say s b)        = Say s $ f b
  fmap f (Output p o b)   = Output p o $ f b
  fmap f (LiftST a b)     = LiftST a (f . b)
  fmap f (Fork s b)       = Fork s $ f b
  fmap f (Atomically a k) = Atomically a (f . k)
  fmap f (NewTimeout      d b) = NewTimeout      d (f . b)
  fmap f (UpdateTimeout t d b) = UpdateTimeout t d (f b)
  fmap f (CancelTimeout t   b) = CancelTimeout t   (f b)

instance Functor (StmF s) where
  fmap f (NewTVar   x k)   = NewTVar   x (f . k)
  fmap f (ReadTVar  v k)   = ReadTVar  v (f . k)
  fmap f (WriteTVar v a b) = WriteTVar v a (f b)
  fmap _  Retry            = Retry

instance MonadSay (Free (SimF s)) where
  say msg = Free.liftF $ Say msg ()

instance MonadFork (Free (SimF s)) where
  fork          task = Free.liftF $ Fork task ()

instance MonadSTM (Free (SimF s)) where
  type Tr    (Free (SimF s)) = Free (StmF s)
  type TVar  (Free (SimF s)) = TVar s
  type TMVar (Free (SimF s)) = TMVarDefault (Free (SimF s))

  atomically action = Free.liftF $ Atomically action id
  newTVar         x = Free.liftF $ NewTVar x id
  readTVar   tvar   = Free.liftF $ ReadTVar tvar id
  writeTVar  tvar x = Free.liftF $ WriteTVar tvar x ()
  retry             = Free.liftF $ Retry

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

instance MonadST (Free (SimF s)) where
  withLiftST f = f liftST
    where
      liftST :: StrictST.ST s a -> Free (SimF s) a
      liftST action = Free.liftF (LiftST action id)

data TBQueueSim s a = TBQueueSim
  !(TVar s Natural) -- read capacity
  !(TVar s [a]) -- elements waiting for read
  !(TVar s Natural) -- write capacity 
  !(TVar s [a]) -- written elements
  !Natural

instance MonadTBQueue (Free (SimF s)) where
  type TBQueue (Free (SimF s)) = TBQueueSim s
  newTBQueue size = do
    rsize <- newTVar 0
    read  <- newTVar []
    wsize <- newTVar size
    write <- newTVar []
    return (TBQueueSim rsize read wsize write size)

  readTBQueue (TBQueueSim rsize read _wsize write _size) = do
    xs <- readTVar read
    r <- readTVar rsize
    writeTVar rsize $! r + 1
    case xs of
      (x:xs') -> do
        writeTVar read xs'
        return x
      [] -> do
        ys <- readTVar write
        case ys of
          [] -> retry
          _  -> do
            let (z:zs) = reverse ys -- NB. lazy: we want the transaction to be
                                    -- short, otherwise it will conflict
            writeTVar write []
            writeTVar read zs
            return z

  writeTBQueue (TBQueueSim rsize _read wsize write _size) a = do
    w <- readTVar wsize
    if (w > 0)
      then do writeTVar wsize $! w - 1
      else do
            r <- readTVar rsize
            if (r > 0)
              then do writeTVar rsize 0
                      writeTVar wsize $! r - 1
              else retry
    listend <- readTVar write
    writeTVar write (a:listend)

  lengthTBQueue (TBQueueSim rsize _read wsize _write size) = do
    r <- readTVar rsize
    w <- readTVar wsize
    return $! size - r - w

instance MonadTimer (Free (SimF s)) where
  type Time    (Free (SimF s)) = VTime
  data Timeout (Free (SimF s)) = Timeout !(TVar s TimeoutState) !TimeoutId

  readTimeout (Timeout var _key) = readTVar var

  newTimeout      d = Free.liftF $ NewTimeout      d id
  updateTimeout t d = Free.liftF $ UpdateTimeout t d ()
  cancelTimeout t   = Free.liftF $ CancelTimeout t   ()

--
-- Simulation interpreter
--

data Thread s = Thread ThreadId (SimM s ())

threadId :: Thread s -> ThreadId
threadId (Thread tid _) = tid

newtype ThreadId  = ThreadId  Int deriving (Eq, Ord, Enum, Show)
newtype TVarId    = TVarId    Int deriving (Eq, Ord, Enum, Show)
newtype TimeoutId = TimeoutId Int deriving (Eq, Ord, Enum, Show)

type Trace = [(VTime, ThreadId, TraceEvent)]

data TraceEvent
  = EventFail String
  | EventSay  String
  | EventThreadForked ThreadId
  | EventThreadStopped
  | EventTxComitted    [TVarId] -- tx wrote to these
                       [TVarId] -- and created these
  | EventTxBlocked     [TVarId] -- tx blocked reading these
  | EventTxWakeup      [TVarId] -- changed vars causing retry
  | EventTimerCreated   TimeoutId TVarId VTime
  | EventTimerUpdated   TimeoutId        VTime
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId
  deriving Show

filterTrace :: (VTime, ThreadId, TraceEvent) -> Bool
filterTrace (_, _, EventFail _)         = True
filterTrace (_, _, EventSay _)          = True
filterTrace (_, _, EventThreadForked _) = True
filterTrace (_, _, EventThreadStopped)  = True
filterTrace _                           = False

filterByThread :: ThreadId -> (VTime, ThreadId, TraceEvent) -> Bool
filterByThread tid (_, tid', _) = tid == tid'

runSimM :: (forall s. SimM s ()) -> Trace
runSimM initialThread = runST (runSimMST initialThread)

runSimMST :: forall s. SimM s () -> ST s Trace
runSimMST initialThread = schedule (initialState initialThread)

data SimState s = SimState {
       runqueue :: ![Thread s],
       blocked  :: !(Map ThreadId (Thread s)),
       curTime  :: !VTime,
       timers   :: !(OrdPSQ TimeoutId VTime (TVar s TimeoutState)),
       nextTid  :: !ThreadId,   -- ^ next unused 'ThreadId'
       nextVid  :: !TVarId,     -- ^ next unused 'TVarId'
       nextTmid :: !TimeoutId   -- ^ next unused 'TimeoutId'
     }

initialState :: SimM s () -> SimState s
initialState initialThread =
  SimState {
    runqueue = [Thread (ThreadId 0) initialThread],
    blocked  = Map.empty,
    curTime  = VTime 0,
    timers   = PSQ.empty,
    nextTid  = ThreadId 1,
    nextVid  = TVarId 0,
    nextTmid = TimeoutId 0
  }

schedule :: SimState s -> ST s Trace
schedule simstate@SimState {
           runqueue = thread@(Thread tid action):remaining,
           blocked, timers, nextTid, nextVid, nextTmid,
           curTime  = time
         } =
  case action of

    Pure () -> do
      -- this thread is done
      trace <- schedule simstate { runqueue = remaining }
      return ((time,tid,EventThreadStopped):trace)

    Free (Fail msg) -> do
      -- stop the whole sim on failure
      return [(time,tid,EventFail msg)]

    Free (Say msg k) -> do
      let thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining }
      return ((time,tid,EventSay msg):trace)

    Free (Output (Probe p) o k) -> do
      modifySTRef p ((time, o):)
      let thread' = Thread tid k
      schedule simstate { runqueue = thread':remaining }

    Free (LiftST st k) -> do
      x <- strictToLazyST st
      let thread' = Thread tid (k x)
      schedule simstate { runqueue = thread':remaining }

    Free (NewTimeout d k) -> do
      tvar <- execNewTVar nextVid TimeoutPending
      let expiry  = d `addTime` time
          timeout = Timeout tvar nextTmid
          timers' = PSQ.insert nextTmid expiry tvar timers
          thread' = Thread tid (k timeout)
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers'
                                 , nextVid  = succ nextVid
                                 , nextTmid = succ nextTmid }
      return ((time, tid, EventTimerCreated nextTmid nextVid expiry):trace)

    Free (UpdateTimeout (Timeout _tvar tmid) d k) -> do
          -- updating an expired timeout is a noop, so it is safe
          -- to race using a timeout with updating or cancelling it
      let updateTimout  Nothing       = ((), Nothing)
          updateTimout (Just (_p, v)) = ((), Just (expiry, v))
          expiry  = d `addTime` time
          timers' = snd (PSQ.alter updateTimout tmid timers)
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return ((time,tid,EventTimerUpdated tmid expiry):trace)

    Free (CancelTimeout (Timeout _tvar tmid) k) -> do
      let timers' = PSQ.delete tmid timers
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return ((time,tid,EventTimerCancelled tmid):trace)

    Free (Fork a k) -> do
      let thread'  = Thread tid k
          thread'' = Thread tid' a
          tid'     = nextTid
      trace <- schedule simstate
        { runqueue = thread':remaining ++ [thread'']
        , nextTid  = succ nextTid
        }
      return ((time,tid,EventThreadForked tid'):trace)

    Free (Atomically a k) -> do
      (res, nextVid') <- execAtomically tid nextVid a
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
          return $ (time, tid, EventTxComitted written [nextVid..pred nextVid'])
                 : [ (time, tid', EventTxWakeup vids) | (tid', vids) <- wakeup ]
                ++ trace

        StmTxBlocked vids -> do
          let blocked' = Map.insert tid thread blocked
          trace <- schedule simstate {
                     runqueue = remaining,
                     blocked  = blocked',
                     nextVid  = nextVid'
                   }
          return ((time,tid,EventTxBlocked vids):trace)

-- no runnable threads, advance the time to the next timer event, or stop.
schedule simstate@SimState {
           runqueue = [],
           blocked, timers,
           curTime = time
         } =

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return []

      Just (tmids, time', fired, timers') -> assert (time' >= time) $ do

        -- Reuse the STM functionality here to write all the timer TVars.
        -- Simplify to a special case that only reads and writes TVars.
        wakeup <- execAtomically' (mapM_ timeoutAction fired)

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
        return $ [ (time', ThreadId (-1), EventTimerExpired tmid)
                 | tmid <- tmids ]
              ++ [ (time', tid', EventTxWakeup vids)
                 | (tid', vids) <- wakeup ]
              ++ trace
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
               -> STM s a
               -> ST s (StmTxResult s a, TVarId)
execAtomically mytid = go [] []
  where
    go :: [SomeTVar s] -> [SomeTVar s] -> TVarId
       -> STM s a -> ST s (StmTxResult s a, TVarId)
    go read written nextVid action = case action of
      Pure x                 -> do (vids, tids) <- finaliseCommit written
                                   return (StmTxComitted x vids tids, nextVid)
      Free Retry             -> do vids <- finaliseRetry read written
                                   return (StmTxBlocked vids, nextVid)
      Free (NewTVar x k)     -> do v <- execNewTVar nextVid x
                                   go read written (succ nextVid) (k v)
      Free (ReadTVar v k)    -> do x <- execReadTVar v
                                   go (SomeTVar v : read) written nextVid (k x)
      Free (WriteTVar v x k) -> do execWriteTVar v x
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


execAtomically' :: STM s () -> ST s [(ThreadId, [TVarId])]
execAtomically' = go []
  where
    go :: [SomeTVar s] -> STM s () -> ST s [(ThreadId, [TVarId])]
    go written action = case action of
      Pure ()                -> do (_vids, tids) <- finaliseCommit written
                                   return tids
      Free (ReadTVar v k)    -> do x <- execReadTVar v
                                   go written (k x)
      Free (WriteTVar v x k) -> do execWriteTVar v x
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

example1 :: Free (SimF s) ()
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
