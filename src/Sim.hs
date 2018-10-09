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

module Sim {-(
  SimF,
  SimM,
  Probe,
  SimChan (..),
  newProbe,
  runSimM,
  runSimMST,
  )-} where

import           Prelude hiding (read)

import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set

import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Free (Free)
import           Control.Monad.Free as Free
import           Control.Monad.ST.Lazy
import           Data.STRef.Lazy

import           MonadClass.MonadSay
import           MonadClass.MonadFork
import           MonadClass.MonadSTM hiding (TVar)
import qualified MonadClass.MonadSTM as MonadSTM
import           MonadClass.MonadTimer
import           MonadClass.MonadSendRecv

{-# ANN module "HLint: ignore Use readTVarIO" #-}


--
-- Simulation monad for protocol testing
--

data SimF (s :: *) a where
  Fail         :: String -> SimF s a

  Say          :: [String] -> b -> SimF s b
  Output       :: Probe s o -> o -> b -> SimF s b

  Timer        :: VTimeDuration -> Free (SimF s) () -> b -> SimF s b

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

newtype VTime         = VTime Rational
  deriving (Eq, Ord, Show)
newtype VTimeDuration = VTimeDuration Rational
  deriving (Eq, Ord, Show, Num, Enum, Real, Fractional)

instance TimeMeasure VTime where
  type Duration VTime = VTimeDuration

  diffTime (VTime t) (VTime t') = VTimeDuration (t-t')
  addTime  (VTimeDuration d) (VTime t) = VTime (t+d)
  zero = VTime 0

instance Functor (SimF s) where
  fmap _ (Fail f)         = Fail f
  fmap f (Say ss b)       = Say ss $ f b
  fmap f (Output p o b)   = Output p o $ f b
  fmap f (Timer d s b)    = Timer d s $ f b
  fmap f (Fork s b)       = Fork s $ f b
  fmap f (Atomically a k) = Atomically a (f . k)

instance Functor (StmF s) where
  fmap f (NewTVar   x k)   = NewTVar   x (f . k)
  fmap f (ReadTVar  v k)   = ReadTVar  v (f . k)
  fmap f (WriteTVar v a b) = WriteTVar v a (f b)
  fmap _  Retry            = Retry

instance MonadSay (Free (SimF s)) where
  say msg = Free.liftF $ Say [msg] ()

instance MonadTimer (Free (SimF s)) where
  type Time (Free (SimF s)) = VTime
  timer t action = Free.liftF $ Timer t action ()

instance MonadFork (Free (SimF s)) where
  fork          task = Free.liftF $ Fork task ()

instance MonadSTM (Free (SimF s)) (Free (StmF s)) where
  type TVar (Free (SimF s)) = TVar s
  atomically action = Free.liftF $ Atomically action id
  newTVar         x = Free.liftF $ NewTVar x id
  readTVar   tvar   = Free.liftF $ ReadTVar tvar id
  writeTVar  tvar x = Free.liftF $ WriteTVar tvar x ()
  retry             = Free.liftF $ Retry

instance MonadSendRecv (Free (SimF s)) where
  type BiChan (Free (SimF s)) = SimChan s

  newChan = SimChan <$> atomically (newTVar Nothing) <*> atomically (newTVar Nothing)
  sendMsg (SimChan  s _r) = atomically . writeTVar s . Just
  recvMsg (SimChan _s  r) = atomically $ do
      mmsg <- readTVar r
      case mmsg of
        Nothing  -> retry
        Just msg -> writeTVar r Nothing >> return msg

data SimChan s send recv = SimChan (TVar s (Maybe send)) (TVar s (Maybe recv))

flipSimChan :: SimChan s recv send -> SimChan s send recv
flipSimChan (SimChan unichanAB unichanBA) = SimChan unichanBA unichanAB

data Thread s = Thread ThreadId (SimM s ())

threadId :: Thread s -> ThreadId
threadId (Thread tid _) = tid

newtype ThreadId = ThreadId Int deriving (Eq, Ord, Show)
newtype TVarId   = TVarId   Int deriving (Eq, Ord, Show)

succThreadId :: ThreadId -> ThreadId
succThreadId (ThreadId n) = ThreadId (n+1)

succTVarId :: TVarId -> TVarId
succTVarId (TVarId n) = TVarId (n+1)

type Trace = [(VTime, ThreadId, TraceEvent)]

data TraceEvent
  = EventFail String
  | EventSay [String]
  | EventTimerCreated VTime
  | EventTimerExpired
  | EventThreadForked ThreadId
  | EventThreadStopped
  | EventTxComitted    [TVarId] -- tx wrote to these
  | EventTxBlocked     [TVarId] -- tx blocked reading these
  | EventTxRetry       [TVarId] -- changed vars causing retry
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
       timers   :: !(PQueue VTime (SimM s ())),
       nextTid  :: !ThreadId,   -- ^ next unused 'ThreadId'
       nextVid  :: !TVarId      -- ^ next unused 'TVarId'
     }

initialState :: SimM s () -> SimState s
initialState initialThread =
  SimState {
    runqueue = [Thread (ThreadId 0) initialThread],
    blocked  = Map.empty,
    curTime  = VTime 0,
    timers   = PQueue.empty,
    nextTid  = ThreadId 1,
    nextVid  = TVarId 0
  }

schedule :: SimState s -> ST s Trace
schedule simstate@SimState {
           runqueue = thread@(Thread tid action):remaining,
           blocked, timers, nextTid, nextVid,
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

    Free (Timer t a k) -> do
      let expiry  = t `addTime` time
          timers' = PQueue.add expiry a timers
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return ((time,tid,EventTimerCreated expiry):trace)

    Free (Fork a k) -> do
      let thread'  = Thread tid k
          thread'' = Thread tid' a
          tid'     = nextTid
      trace <- schedule simstate
        { runqueue = thread':remaining ++ [thread'']
        , nextTid  = succThreadId nextTid
        }
      return ((time,tid,EventThreadForked tid'):trace)

    Free (Atomically a k) -> do
      (res, nextVid') <- execAtomically tid nextVid a
      case res of
        StmTxComitted x written wakeup -> do
          let thread'   = Thread tid (k x)
              unblocked = catMaybes [ Map.lookup tid' blocked | (tid', _) <- wakeup ]
              runqueue =  remaining ++ (reverse $ thread' : unblocked)
          trace <- schedule simstate {
                     runqueue,
                     blocked  = blocked `Map.difference`
                                Map.fromList [ (tid', ()) | (tid', _) <- wakeup ],
                     nextVid  = nextVid'
                   }
          return ((time,tid,EventTxComitted written):
                  [(time,tid',EventTxRetry vids) | (tid', vids) <- wakeup ]
                  ++trace)

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
           timers, nextTid,
           curTime = time
         } =

    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return []

      Just (time', events, timers') -> assert (time' > time) $ do
        trace <- schedule simstate {
                   runqueue = map (Thread nextTid) events,
                   curTime  = time',
                   timers   = timers',
                   nextTid  = succThreadId nextTid
                 }
        return ((time', nextTid, EventTimerExpired):trace)

removeMinimums :: Ord k => PQueue k a -> Maybe (k, [a], PQueue k a)
removeMinimums = \pqueue ->
    case PQueue.minViewWithKey pqueue of
      Nothing                -> Nothing
      Just ((k, x), pqueue') -> Just (collectAll k [x] pqueue')
  where
    collectAll k xs pqueue =
      case PQueue.minViewWithKey pqueue of
        Just ((k', x'), pqueue')
          | k == k' -> collectAll k (x':xs) pqueue'
        _           -> (k, reverse xs, pqueue)

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
                                   go read written (succTVarId nextVid) (k v)
      Free (ReadTVar v k)    -> do x <- execReadTVar v
                                   go (SomeTVar v : read) written nextVid (k x)
      Free (WriteTVar v x k) -> do execWriteTVar v x
                                   go read (SomeTVar v : written) nextVid k

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

    -- Revert all the TVar writes and put this thread on the blocked queue for
    -- all the TVars read in this transaction
    finaliseRetry :: [SomeTVar s] -> [SomeTVar s] -> ST s [TVarId]
    finaliseRetry read written = do
      sequence_ [ revertTVar  tvar | SomeTVar tvar <- reverse written ]
      sequence_ [ blockOnTVar tvar | SomeTVar tvar <- read ]
      return [ vid | SomeTVar (TVar vid _ _ _) <- read ]

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

    blockOnTVar :: TVar s a -> ST s ()
    blockOnTVar (TVar _vid _vcur _vsaved blocked) =
      --TODO: avoid duplicates!
      modifySTRef blocked (mytid:)

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

example0 :: (MonadSay m, MonadTimer m, MonadSTM m stm) => m ()
example0 = do
  say "starting"
  t <- atomically (newTVar (0 :: Int))
  timer 2 $ do
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

  timer 1 $ do
    x <- atomically $ readTVar chan
    say $ show x

-- the trace should contain "1" followed by "2"
example2 :: (MonadSay m, MonadSTM m stm) => m ()
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
