{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields          #-}

module Control.Monad.IOSim.Types
  ( IOSim (..)
  , runIOSim
  , traceM
  , traceSTM
  , liftST
  , SimA (..)
  , StepId
  , STMSim
  , STM (..)
  , runSTM
  , StmA (..)
  , StmTxResult (..)
  , StmStack (..)
  , Timeout (..)
  , TimeoutException (..)

  , setCurrentTime
  , unshareClock

  , ScheduleControl (..)
  , ScheduleMod (..)
  , ExplorationOptions (..)
  , ExplorationSpec
  , withScheduleBound
  , withBranching
  , withStepTimelimit
  , withReplay
  , stdExplorationOptions

  , EventlogEvent (..)
  , EventlogMarker (..)

  , SimEventType (..)
  , SimEvent (..)
  , SimResult (..)
  , SimTrace
  , Trace.Trace (Trace, SimTrace, SimPORTrace, TraceMainReturn, TraceMainException,
                 TraceDeadlock, TraceRacesFound, TraceLoop)
  , ppTrace
  , ppTrace_
  , ppSimEvent
  , ppDebug
  , TraceEvent
  , Labelled (..)

  , module Control.Monad.IOSim.CommonTypes
  , SimM
  , SimSTM
  ) where

import           Control.Exception (ErrorCall (..), asyncExceptionFromException, asyncExceptionToException)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix (MonadFix (..))

import           Control.Monad.Class.MonadAsync hiding (Async)
import qualified Control.Monad.Class.MonadAsync as MonadAsync
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import qualified Control.Monad.Class.MonadFork as MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadTest
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadEventlog
import           Control.Monad.Class.MonadSTM (MonadSTM, MonadInspectSTM (..),
                   MonadLabelledSTM (..), MonadTraceSTM (..), TMVarDefault)
import qualified Control.Monad.Class.MonadSTM as MonadSTM
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadThrow as MonadThrow hiding (getMaskingState)
import qualified Control.Monad.Class.MonadThrow as MonadThrow
import           Control.Monad.ST.Lazy
import qualified Control.Monad.ST.Strict as StrictST

import qualified Control.Monad.Catch as Exceptions
import qualified Control.Monad.Fail as Fail

import           Data.Bifoldable
import           Data.Bifunctor (bimap)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Endo (..))
import           Data.Dynamic (Dynamic, toDyn)
import           Data.Semigroup (Max (..))
import           Data.Typeable
import           Data.STRef.Lazy
import qualified Data.List.Trace as Trace
import qualified Debug.Trace as Debug
import           Text.Printf

import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Control.Monad.IOSim.CommonTypes
import           Control.Monad.IOSim.STM
import           Control.Monad.IOSimPOR.Types


import qualified System.IO.Error as IO.Error (userError)

{-# ANN module "HLint: ignore Use readTVarIO" #-}
newtype IOSim s a = IOSim { unIOSim :: forall r. (a -> SimA s r) -> SimA s r }

type SimM s = IOSim s
{-# DEPRECATED SimM "Use IOSim" #-}

runIOSim :: IOSim s a -> SimA s a
runIOSim (IOSim k) = k Return

traceM :: Typeable a => a -> IOSim s ()
traceM x = IOSim $ \k -> Output (toDyn x) (k ())

traceSTM :: Typeable a => a -> STMSim s ()
traceSTM x = STM $ \k -> OutputStm (toDyn x) (k ())

data SimA s a where
  Return       :: a -> SimA s a

  Say          :: String -> SimA s b -> SimA s b
  Output       :: Dynamic -> SimA s b -> SimA s b

  LiftST       :: StrictST.ST s a -> (a -> SimA s b) -> SimA s b

  GetMonoTime  :: (Time    -> SimA s b) -> SimA s b
  GetWallTime  :: (UTCTime -> SimA s b) -> SimA s b
  SetWallTime  ::  UTCTime -> SimA s b  -> SimA s b
  UnshareClock :: SimA s b -> SimA s b

  NewTimeout   :: DiffTime -> (Timeout (IOSim s) -> SimA s b) -> SimA s b
  UpdateTimeout:: Timeout (IOSim s) -> DiffTime -> SimA s b -> SimA s b
  CancelTimeout:: Timeout (IOSim s) -> SimA s b -> SimA s b

  Throw        :: SomeException -> SimA s a
  Catch        :: Exception e =>
                  SimA s a -> (e -> SimA s a) -> (a -> SimA s b) -> SimA s b
  Evaluate     :: a -> (a -> SimA s b) -> SimA s b

  Fork         :: IOSim s () -> (ThreadId -> SimA s b) -> SimA s b
  GetThreadId  :: (ThreadId -> SimA s b) -> SimA s b
  LabelThread  :: ThreadId -> String -> SimA s b -> SimA s b

  Atomically   :: STM  s a -> (a -> SimA s b) -> SimA s b

  ThrowTo      :: SomeException -> ThreadId -> SimA s a -> SimA s a
  SetMaskState :: MaskingState  -> IOSim s a -> (a -> SimA s b) -> SimA s b
  GetMaskState :: (MaskingState -> SimA s b) -> SimA s b

  ExploreRaces :: SimA s b -> SimA s b

  Fix          :: (x -> IOSim s x) -> (x -> SimA s r) -> SimA s r


newtype STM s a = STM { unSTM :: forall r. (a -> StmA s r) -> StmA s r }

runSTM :: STM s a -> StmA s a
runSTM (STM k) = k ReturnStm

data StmA s a where
  ReturnStm    :: a -> StmA s a
  ThrowStm     :: SomeException -> StmA s a

  NewTVar      :: Maybe String -> x -> (TVar s x -> StmA s b) -> StmA s b
  LabelTVar    :: String -> TVar s a -> StmA s b -> StmA s b
  ReadTVar     :: TVar s a -> (a -> StmA s b) -> StmA s b
  WriteTVar    :: TVar s a ->  a -> StmA s b  -> StmA s b
  Retry        :: StmA s b
  OrElse       :: StmA s a -> StmA s a -> (a -> StmA s b) -> StmA s b

  SayStm       :: String -> StmA s b -> StmA s b
  OutputStm    :: Dynamic -> StmA s b -> StmA s b
  TraceTVar    :: forall s a b tr. Typeable tr
               => TVar s a
               -> (Maybe a -> a -> ST s tr)
               -> StmA s b -> StmA s b

-- Exported type
type STMSim = STM

type SimSTM = STM
{-# DEPRECATED SimSTM "Use STMSim" #-}

--
-- Monad class instances
--

instance Functor (IOSim s) where
    {-# INLINE fmap #-}
    fmap f = \d -> IOSim $ \k -> unIOSim d (k . f)

instance Applicative (IOSim s) where
    {-# INLINE pure #-}
    pure = \x -> IOSim $ \k -> k x

    {-# INLINE (<*>) #-}
    (<*>) = \df dx -> IOSim $ \k ->
                        unIOSim df (\f -> unIOSim dx (\x -> k (f x)))

    {-# INLINE (*>) #-}
    (*>) = \dm dn -> IOSim $ \k -> unIOSim dm (\_ -> unIOSim dn k)

instance Monad (IOSim s) where
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = \dm f -> IOSim $ \k -> unIOSim dm (\m -> unIOSim (f m) k)

    {-# INLINE (>>) #-}
    (>>) = (*>)

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Semigroup a => Semigroup (IOSim s a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (IOSim s a) where
    mempty = pure mempty

#if !(MIN_VERSION_base(4,11,0))
    mappend = liftA2 mappend
#endif

instance Fail.MonadFail (IOSim s) where
  fail msg = IOSim $ \_ -> Throw (toException (IO.Error.userError msg))

instance MonadFix (IOSim s) where  
    mfix f = IOSim $ \k -> Fix f k 


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

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Fail.MonadFail (STM s) where
  fail msg = STM $ \_ -> ThrowStm (toException (ErrorCall msg))

instance Alternative (STM s) where
    empty = MonadSTM.retry
    (<|>) = MonadSTM.orElse

instance MonadPlus (STM s) where

instance MonadSay (IOSim s) where
  say msg = IOSim $ \k -> Say msg (k ())

instance MonadThrow (IOSim s) where
  throwIO e = IOSim $ \_ -> Throw (toException e)

instance MonadEvaluate (IOSim s) where
  evaluate a = IOSim $ \k -> Evaluate a k

instance Exceptions.MonadThrow (IOSim s) where
  throwM = MonadThrow.throwIO

instance MonadThrow (STM s) where
  throwIO e = STM $ \_ -> ThrowStm (toException e)

  -- Since these involve re-throwing the exception and we don't provide
  -- CatchSTM at all, then we can get away with trivial versions:
  bracket before after thing = do
    a <- before
    r <- thing a
    _ <- after a
    return r

  finally thing after = do
    r <- thing
    _ <- after
    return r

instance Exceptions.MonadThrow (STM s) where
  throwM = MonadThrow.throwIO

instance MonadCatch (IOSim s) where
  catch action handler =
    IOSim $ \k -> Catch (runIOSim action) (runIOSim . handler) k

instance Exceptions.MonadCatch (IOSim s) where
  catch = MonadThrow.catch

instance MonadMask (IOSim s) where
  mask action = do
      b <- getMaskingStateImpl
      case b of
        Unmasked              -> block $ action unblock
        MaskedInterruptible   -> action block
        MaskedUninterruptible -> action blockUninterruptible

  uninterruptibleMask action = do
      b <- getMaskingStateImpl
      case b of
        Unmasked              -> blockUninterruptible $ action unblock
        MaskedInterruptible   -> blockUninterruptible $ action block
        MaskedUninterruptible -> action blockUninterruptible

instance MonadMaskingState (IOSim s) where
  getMaskingState = getMaskingStateImpl

instance Exceptions.MonadMask (IOSim s) where
  mask                = MonadThrow.mask
  uninterruptibleMask = MonadThrow.uninterruptibleMask

  generalBracket acquire release use =
    mask $ \unmasked -> do
      resource <- acquire
      b <- unmasked (use resource) `catch` \e -> do
        _ <- release resource (Exceptions.ExitCaseException e)
        throwIO e
      c <- release resource (Exceptions.ExitCaseSuccess b)
      return (b, c)


getMaskingStateImpl :: IOSim s MaskingState
unblock, block, blockUninterruptible :: IOSim s a -> IOSim s a

getMaskingStateImpl    = IOSim  GetMaskState
unblock              a = IOSim (SetMaskState Unmasked a)
block                a = IOSim (SetMaskState MaskedInterruptible a)
blockUninterruptible a = IOSim (SetMaskState MaskedUninterruptible a)

instance MonadThread (IOSim s) where
  type ThreadId (IOSim s) = ThreadId
  myThreadId       = IOSim $ \k -> GetThreadId k
  labelThread t l  = IOSim $ \k -> LabelThread t l (k ())

instance MonadFork (IOSim s) where
  forkIO task        = IOSim $ \k -> Fork task k
  forkIOWithUnmask f = forkIO (f unblock)
  throwTo tid e      = IOSim $ \k -> ThrowTo (toException e) tid (k ())

instance MonadTest (IOSim s) where
  exploreRaces       = IOSim $ \k -> ExploreRaces (k ())

instance MonadSay (STMSim s) where
  say msg = STM $ \k -> SayStm msg (k ())


instance MonadLabelledSTM (IOSim s) where
  labelTVar tvar label = STM $ \k -> LabelTVar label tvar (k ())
  labelTMVar   = MonadSTM.labelTMVarDefault
  labelTQueue  = labelTQueueDefault
  labelTBQueue = labelTBQueueDefault

instance MonadSTM (IOSim s) where
  type STM       (IOSim s) = STM s
  type TVar      (IOSim s) = TVar s
  type TMVar     (IOSim s) = TMVarDefault (IOSim s)
  type TQueue    (IOSim s) = TQueueDefault (IOSim s)
  type TBQueue   (IOSim s) = TBQueueDefault (IOSim s)

  atomically action = IOSim $ \k -> Atomically action k

  newTVar         x = STM $ \k -> NewTVar Nothing x k
  readTVar   tvar   = STM $ \k -> ReadTVar tvar k
  writeTVar  tvar x = STM $ \k -> WriteTVar tvar x (k ())
  retry             = STM $ \_ -> Retry
  orElse        a b = STM $ \k -> OrElse (runSTM a) (runSTM b) k

  newTMVar          = MonadSTM.newTMVarDefault
  newEmptyTMVar     = MonadSTM.newEmptyTMVarDefault
  takeTMVar         = MonadSTM.takeTMVarDefault
  tryTakeTMVar      = MonadSTM.tryTakeTMVarDefault
  putTMVar          = MonadSTM.putTMVarDefault
  tryPutTMVar       = MonadSTM.tryPutTMVarDefault
  readTMVar         = MonadSTM.readTMVarDefault
  tryReadTMVar      = MonadSTM.tryReadTMVarDefault
  swapTMVar         = MonadSTM.swapTMVarDefault
  isEmptyTMVar      = MonadSTM.isEmptyTMVarDefault

  newTQueue         = newTQueueDefault
  readTQueue        = readTQueueDefault
  tryReadTQueue     = tryReadTQueueDefault
  peekTQueue        = peekTQueueDefault
  tryPeekTQueue     = tryPeekTQueueDefault
  writeTQueue       = writeTQueueDefault
  isEmptyTQueue     = isEmptyTQueueDefault

  newTBQueue        = newTBQueueDefault
  readTBQueue       = readTBQueueDefault
  tryReadTBQueue    = tryReadTBQueueDefault
  peekTBQueue       = peekTBQueueDefault
  tryPeekTBQueue    = tryPeekTBQueueDefault
  flushTBQueue      = flushTBQueueDefault
  writeTBQueue      = writeTBQueueDefault
  lengthTBQueue     = lengthTBQueueDefault
  isEmptyTBQueue    = isEmptyTBQueueDefault
  isFullTBQueue     = isFullTBQueueDefault

  newTMVarIO        = MonadSTM.newTMVarIODefault
  newEmptyTMVarIO   = MonadSTM.newEmptyTMVarIODefault

instance MonadInspectSTM (IOSim s) where
  type InspectMonad (IOSim s) = ST s
  inspectTVar  _                 TVar { tvarCurrent }  = readSTRef tvarCurrent
  inspectTMVar _ (MonadSTM.TMVar TVar { tvarCurrent }) = readSTRef tvarCurrent

-- | This instance adds a trace when a variable was written, just after the
-- stm transaction was committed.
--
instance MonadTraceSTM (IOSim s) where
  traceTVar _ tvar f = STM $ \k -> TraceTVar tvar f (k ())
  traceTQueue  = traceTQueueDefault
  traceTBQueue = traceTBQueueDefault

data Async s a = Async !ThreadId (STM s (Either SomeException a))

instance Eq (Async s a) where
    Async tid _ == Async tid' _ = tid == tid'

instance Ord (Async s a) where
    compare (Async tid _) (Async tid' _) = compare tid tid'

instance Functor (Async s) where
  fmap f (Async tid a) = Async tid (fmap f <$> a)

instance MonadAsync (IOSim s) where
  type Async (IOSim s) = Async s

  async action = do
    var <- MonadSTM.newEmptyTMVarIO
    tid <- mask $ \restore ->
             forkIO $ try (restore action)
                  >>= MonadSTM.atomically . MonadSTM.putTMVar var
    MonadSTM.labelTMVarIO var ("async-" ++ show tid)
    return (Async tid (MonadSTM.readTMVar var))

  asyncThreadId (Async tid _) = tid

  waitCatchSTM (Async _ w) = w
  pollSTM      (Async _ w) = (Just <$> w) `MonadSTM.orElse` return Nothing

  cancel a@(Async tid _) = throwTo tid AsyncCancelled <* waitCatch a
  cancelWith a@(Async tid _) e = throwTo tid e <* waitCatch a

  asyncWithUnmask k = async (k unblock)

instance MonadST (IOSim s) where
  withLiftST f = f liftST

liftST :: StrictST.ST s a -> IOSim s a
liftST action = IOSim $ \k -> LiftST action k

instance MonadMonotonicTime (IOSim s) where
  getMonotonicTime = IOSim $ \k -> GetMonoTime k

instance MonadTime (IOSim s) where
  getCurrentTime   = IOSim $ \k -> GetWallTime k

-- | Set the current wall clock time for the thread's clock domain.
--
setCurrentTime :: UTCTime -> IOSim s ()
setCurrentTime t = IOSim $ \k -> SetWallTime t (k ())

-- | Put the thread into a new wall clock domain, not shared with the parent
-- thread. Changing the wall clock time in the new clock domain will not affect
-- the other clock of other threads. All threads forked by this thread from
-- this point onwards will share the new clock domain.
--
unshareClock :: IOSim s ()
unshareClock = IOSim $ \k -> UnshareClock (k ())

instance MonadDelay (IOSim s) where
  -- Use default in terms of MonadTimer

instance MonadTimer (IOSim s) where
  data Timeout (IOSim s) = Timeout !(TVar s TimeoutState) !(TVar s Bool) !TimeoutId
                         -- ^ a timeout; we keep both 'TVar's to support
                         -- `newTimer` and 'registerTimeout'.
                         | NegativeTimeout !TimeoutId
                         -- ^ a negative timeout

  readTimeout (Timeout var _bvar _key) = MonadSTM.readTVar var
  readTimeout (NegativeTimeout _key)   = pure TimeoutCancelled

  newTimeout      d = IOSim $ \k -> NewTimeout      d k
  updateTimeout t d = IOSim $ \k -> UpdateTimeout t d (k ())
  cancelTimeout t   = IOSim $ \k -> CancelTimeout t   (k ())

  timeout d action
    | d <  0    = Just <$> action
    | d == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        t@(Timeout _ _ tid) <- newTimeout d
        handleJust
          (\(TimeoutException tid') -> if tid' == tid
                                         then Just ()
                                         else Nothing)
          (\_ -> return Nothing) $
          bracket
            (forkIO $ do
                labelThisThread "<<timeout>>"
                fired <- MonadSTM.atomically $ awaitTimeout t
                when fired $ throwTo pid (TimeoutException tid))
            (\pid' -> do
                  cancelTimeout t
                  throwTo pid' AsyncCancelled)
            (\_ -> Just <$> action)

  registerDelay d = IOSim $ \k -> NewTimeout d (\(Timeout _var bvar _) -> k bvar)

newtype TimeoutException = TimeoutException TimeoutId deriving Eq

instance Show TimeoutException where
    show _ = "<<timeout>>"

instance Exception TimeoutException where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException

-- | Wrapper for Eventlog events so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogEvent = EventlogEvent String

-- | Wrapper for Eventlog markers so they can be retrieved from the trace with
-- 'selectTraceEventsDynamic'.
newtype EventlogMarker = EventlogMarker String

instance MonadEventlog (IOSim s) where
  traceEventIO = traceM . EventlogEvent
  traceMarkerIO = traceM . EventlogMarker

-- | 'Trace' is a recursive data type, it is the trace of a 'IOSim' computation.
-- The trace will contain information about thread sheduling, blocking on
-- 'TVar's, and other internal state changes of 'IOSim'.  More importantly it
-- also supports traces generated by the computation with 'say' (which
-- corresponds to using 'putStrLn' in 'IO'), 'traceEventM', or dynamically typed
-- traces with 'traceM' (which generalise the @base@ library
-- 'Debug.Trace.traceM')
--
-- It also contains information on races discovered.
--
-- See also: 'traceEvents', 'traceResult', 'selectTraceEvents',
-- 'selectTraceEventsDynamic' and 'printTraceEventsSay'.
--
data SimEvent
  = SimEvent {
      seTime        :: !Time,
      seThreadId    :: !ThreadId,
      seThreadLabel :: !(Maybe ThreadLabel),
      seType        :: !SimEventType
    }
  | SimPOREvent {
      seTime        :: !Time,
      seThreadId    :: !ThreadId,
      seStep        :: !Int,
      seThreadLabel :: !(Maybe ThreadLabel),
      seType        :: !SimEventType
    }
  | SimRacesFound [ScheduleControl]
  deriving Generic
  deriving Show via Quiet SimEvent


ppSimEvent :: Int -- ^ width of the time
           -> Int -- ^ width of thread id
           -> Int -- ^ width of thread label
           -> SimEvent
           -> String
ppSimEvent timeWidth tidWidth tLabelWidth SimEvent {seTime, seThreadId, seThreadLabel, seType} =
    printf "%-*s - %-*s %-*s - %s"
           timeWidth
           (show seTime)
           tidWidth
           (show seThreadId)
           tLabelWidth
           threadLabel
           (show seType)
  where
    threadLabel = fromMaybe "" seThreadLabel
ppSimEvent timeWidth tidWidth tLableWidth SimPOREvent {seTime, seThreadId, seStep, seThreadLabel, seType} =
    printf "%-*s - %-*s %-*s - %s"
           timeWidth
           (show seTime)
           tidWidth
           (show (seThreadId, seStep))
           tLableWidth
           threadLabel
           (show seType)
  where
    threadLabel = fromMaybe "" seThreadLabel
ppSimEvent _ _ _ (SimRacesFound controls) =
    "RacesFound "++show controls

data SimResult a
    = MainReturn    !Time a             ![Labelled ThreadId]
    | MainException !Time SomeException ![Labelled ThreadId]
    | Deadlock      !Time               ![Labelled ThreadId]
    | Loop
    deriving Show


type SimTrace a = Trace.Trace (SimResult a) SimEvent

-- | Pretty print simulation trace.
--
ppTrace :: Show a => SimTrace a -> String
ppTrace tr = Trace.ppTrace
               show
               (ppSimEvent timeWidth tidWith labelWidth)
               tr
  where
    (Max timeWidth, Max tidWith, Max labelWidth) =
        bimaximum
      . bimap (const (Max 0, Max 0, Max 0))
              (\a -> case a of
                SimEvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimPOREvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimRacesFound {} ->
                  (Max 0, Max 0, Max 0)
              )
      $ tr


-- | Like 'ppTrace' but does not show the result value.
--
ppTrace_ :: SimTrace a -> String
ppTrace_ tr = Trace.ppTrace
                (const "")
                (ppSimEvent timeWidth tidWith labelWidth)
                tr
  where
    (Max timeWidth, Max tidWith, Max labelWidth) =
        bimaximum
      . bimap (const (Max 0, Max 0, Max 0))
              (\a -> case a of
                SimEvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimPOREvent {seTime, seThreadId, seThreadLabel} ->
                  ( Max (length (show seTime))
                  , Max (length (show (seThreadId)))
                  , Max (length seThreadLabel)
                  )
                SimRacesFound {} ->
                  (Max 0, Max 0, Max 0)
              )
      $ tr

-- | Trace each event using 'Debug.trace'; this is useful when a trace ends with
-- a pure error, e.g. an assertion.
--
ppDebug :: SimTrace a -> x -> x
ppDebug = appEndo
        . foldMap (Endo . Debug.trace . show)
        . Trace.toList

pattern Trace :: Time -> ThreadId -> Maybe ThreadLabel -> SimEventType -> SimTrace a
              -> SimTrace a
pattern Trace time threadId threadLabel traceEvent trace =
    Trace.Cons (SimEvent time threadId threadLabel traceEvent)
               trace

{-# DEPRECATED Trace "Use 'SimTrace' instead." #-}

pattern SimTrace :: Time -> ThreadId -> Maybe ThreadLabel -> SimEventType -> SimTrace a
                 -> SimTrace a
pattern SimTrace time threadId threadLabel traceEvent trace =
    Trace.Cons (SimEvent time threadId threadLabel traceEvent)
               trace

pattern SimPORTrace :: Time -> ThreadId -> Int -> Maybe ThreadLabel -> SimEventType -> SimTrace a
                    -> SimTrace a
pattern SimPORTrace time threadId step threadLabel traceEvent trace =
    Trace.Cons (SimPOREvent time threadId step threadLabel traceEvent)
               trace

pattern TraceRacesFound :: [ScheduleControl] -> SimTrace a
                        -> SimTrace a
pattern TraceRacesFound controls trace =
    Trace.Cons (SimRacesFound controls)
               trace

pattern TraceMainReturn :: Time -> a -> [Labelled ThreadId]
                        -> SimTrace a
pattern TraceMainReturn time a threads = Trace.Nil (MainReturn time a threads)

pattern TraceMainException :: Time -> SomeException -> [Labelled ThreadId]
                           -> SimTrace a
pattern TraceMainException time err threads = Trace.Nil (MainException time err threads)

pattern TraceDeadlock :: Time -> [Labelled ThreadId]
                      -> SimTrace a
pattern TraceDeadlock time threads = Trace.Nil (Deadlock time threads)

pattern TraceLoop :: SimTrace a
pattern TraceLoop = Trace.Nil Loop

{-# COMPLETE SimTrace, SimPORTrace, TraceMainReturn, TraceMainException, TraceDeadlock, TraceLoop #-}
{-# COMPLETE Trace,                 TraceMainReturn, TraceMainException, TraceDeadlock, TraceLoop #-}


data SimEventType
  = EventSimStart      ScheduleControl
  | EventSay  String
  | EventLog  Dynamic
  | EventMask MaskingState

  | EventThrow          SomeException
  | EventThrowTo        SomeException ThreadId -- This thread used ThrowTo
  | EventThrowToBlocked                        -- The ThrowTo blocked
  | EventThrowToWakeup                         -- The ThrowTo resumed
  | EventThrowToUnmasked (Labelled ThreadId)   -- A pending ThrowTo was activated

  | EventThreadForked    ThreadId
  | EventThreadFinished                  -- terminated normally
  | EventThreadUnhandled SomeException   -- terminated due to unhandled exception

  | EventTxCommitted   [Labelled TVarId] -- tx wrote to these
                       [Labelled TVarId] -- and created these
                       (Maybe Effect)    -- effect performed (only for `IOSimPOR`)
  | EventTxAborted     (Maybe Effect)    -- effect performed (only for `IOSimPOR`)
  | EventTxBlocked     [Labelled TVarId] -- tx blocked reading these
                       (Maybe Effect)    -- effect performed (only for `IOSimPOR`)
  | EventTxWakeup      [Labelled TVarId] -- changed vars causing retry

  | EventTimerCreated   TimeoutId TVarId Time
  | EventTimerUpdated   TimeoutId        Time
  | EventTimerCancelled TimeoutId
  | EventTimerExpired   TimeoutId

  -- the following events are inserted to mark the difference between
  -- a failed trace and a similar passing trace of the same action
  | EventThreadSleep                      -- the labelling thread was runnable,
                                          -- but its execution was delayed
  | EventThreadWake                       -- until this point
  | EventDeschedule    Deschedule
  | EventFollowControl        ScheduleControl
  | EventAwaitControl  StepId ScheduleControl
  | EventPerformAction StepId
  | EventReschedule           ScheduleControl
  | EventUnblocked     [ThreadId]
  deriving Show

type TraceEvent = SimEventType
{-# DEPRECATED TraceEvent "Use 'SimEventType' instead." #-}

data Labelled a = Labelled {
    l_labelled :: !a,
    l_label    :: !(Maybe String)
  }
  deriving (Eq, Ord, Generic)
  deriving Show via Quiet (Labelled a)

--
-- Executing STM Transactions
--

data StmTxResult s a =
       -- | A committed transaction reports the vars that were written (in order
       -- of first write) so that the scheduler can unblock other threads that
       -- were blocked in STM transactions that read any of these vars.
       --
       -- It reports the vars that were read, so we can update vector clocks
       -- appropriately.
       --
       -- The third list of vars is ones that were created during this
       -- transaction.  This is useful for an implementation of 'traceTVar'.
       --
       -- It also includes the updated TVarId name supply.
       --
       StmTxCommitted a [SomeTVar s] -- ^ written tvars
                        [SomeTVar s] -- ^ read tvars
                        [SomeTVar s] -- ^ created tvars
                        [Dynamic]
                        TVarId -- updated TVarId name supply

       -- | A blocked transaction reports the vars that were read so that the
       -- scheduler can block the thread on those vars.
       --
     | StmTxBlocked  [SomeTVar s]

       -- | An aborted transaction reports the vars that were read so that the
       -- vector clock can be updated.
       --
     | StmTxAborted  [SomeTVar s] SomeException

data StmStack s b a where
  -- | Executing in the context of a top level 'atomically'.
  AtomicallyFrame  :: StmStack s a a

  -- | Executing in the context of the /left/ hand side of an 'orElse'
  OrElseLeftFrame  :: StmA s a                -- orElse right alternative
                   -> (a -> StmA s b)         -- subsequent continuation
                   -> Map TVarId (SomeTVar s) -- saved written vars set
                   -> [SomeTVar s]            -- saved written vars list
                   -> [SomeTVar s]            -- created vars list
                   -> StmStack s b c
                   -> StmStack s a c

  -- | Executing in the context of the /right/ hand side of an 'orElse'
  OrElseRightFrame :: (a -> StmA s b)         -- subsequent continuation
                   -> Map TVarId (SomeTVar s) -- saved written vars set
                   -> [SomeTVar s]            -- saved written vars list
                   -> [SomeTVar s]            -- created vars list
                   -> StmStack s b c
                   -> StmStack s a c

---
--- Schedules
---

data ScheduleControl = ControlDefault
                     -- ^ default scheduling mode
                     | ControlAwait [ScheduleMod]
                     -- ^ if the current control is 'ControlAwait', the normal
                     -- scheduling will proceed, until the thread found in the
                     -- first 'ScheduleMod' reaches the given step.  At this
                     -- point the thread is put to sleep, until after all the
                     -- steps are followed.
                     | ControlFollow [StepId] [ScheduleMod]
                     -- ^ follow the steps then continue with schedule
                     -- modifications.  This control is set by 'followControl'
                     -- when 'controlTargets' returns true.
  deriving (Eq, Ord, Show)

data ScheduleMod = ScheduleMod{
    scheduleModTarget    :: StepId,   -- when we reach this step
    scheduleModControl   :: ScheduleControl,
                                      -- which happens with this control
    scheduleModInsertion :: [StepId]  -- we should instead perform this sequence
                                      -- this *includes* the target step,
                                      -- not necessarily as the last step.
  }
  deriving (Eq, Ord)

type StepId = (ThreadId, Int)

instance Show ScheduleMod where
  showsPrec d (ScheduleMod tgt ctrl insertion) =
    showParen (d>10) $
      showString "ScheduleMod " .
      showsPrec 11 tgt .
      showString " " .
      showsPrec 11 ctrl .
      showString " " .
      showsPrec 11 insertion

---
--- Exploration options
---

data ExplorationOptions = ExplorationOptions{
    explorationScheduleBound :: Int,
    explorationBranching     :: Int,
    explorationStepTimelimit :: Maybe Int,
    explorationReplay        :: Maybe ScheduleControl
  }
  deriving Show

stdExplorationOptions :: ExplorationOptions
stdExplorationOptions = ExplorationOptions{
    explorationScheduleBound = 100,
    explorationBranching     = 3,
    explorationStepTimelimit = Nothing,
    explorationReplay        = Nothing
    }

type ExplorationSpec = ExplorationOptions -> ExplorationOptions

withScheduleBound :: Int -> ExplorationSpec
withScheduleBound n e = e{explorationScheduleBound = n}

withBranching :: Int -> ExplorationSpec
withBranching n e = e{explorationBranching = n}

withStepTimelimit :: Int -> ExplorationSpec
withStepTimelimit n e = e{explorationStepTimelimit = Just n}

withReplay :: ScheduleControl -> ExplorationSpec
withReplay r e = e{explorationReplay = Just r}
