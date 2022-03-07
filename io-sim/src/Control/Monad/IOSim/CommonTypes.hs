{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Common types shared between `IOSim` and `IOSimPOR`.
--
module Control.Monad.IOSim.CommonTypes where

import           Control.Monad.Class.MonadSTM (TraceValue)
import           Control.Monad.ST.Lazy

import           Data.Function (on)
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.STRef.Lazy

data ThreadId = RacyThreadId [Int]
              | ThreadId     [Int]    -- non racy threads have higher priority
  deriving (Eq, Ord, Show)

childThreadId :: ThreadId -> Int -> ThreadId
childThreadId (RacyThreadId is) i = RacyThreadId (is ++ [i])
childThreadId (ThreadId     is) i = ThreadId     (is ++ [i])

setRacyThread :: ThreadId -> ThreadId
setRacyThread (ThreadId is)      = RacyThreadId is
setRacyThread tid@RacyThreadId{} = tid


newtype TVarId      = TVarId    Int   deriving (Eq, Ord, Enum, Show)
newtype TimeoutId   = TimeoutId Int   deriving (Eq, Ord, Enum, Show)
newtype ClockId     = ClockId   [Int] deriving (Eq, Ord, Show)
newtype VectorClock = VectorClock { getVectorClock :: Map ThreadId Int }
  deriving Show

unTimeoutId :: TimeoutId -> Int
unTimeoutId (TimeoutId a) = a

type ThreadLabel = String
type TVarLabel   = String

data TVar s a = TVar {

       -- | The identifier of this var.
       --
       tvarId      :: !TVarId,

       -- | Label.
       tvarLabel   :: !(STRef s (Maybe TVarLabel)),

       -- | The var's current value
       --
       tvarCurrent :: !(STRef s a),

       -- | A stack of undo values. This is only used while executing a
       -- transaction.
       --
       tvarUndo    :: !(STRef s [a]),

       -- | Thread Ids of threads blocked on a read of this var. It is
       -- represented in reverse order of thread wakeup, without duplicates.
       --
       -- To avoid duplicates efficiently, the operations rely on a copy of the
       -- thread Ids represented as a set.
       --
       tvarBlocked :: !(STRef s ([ThreadId], Set ThreadId)),

       -- | The vector clock of the current value.
       --
       tvarVClock :: !(STRef s VectorClock),

       -- | Callback to construct a trace which will be attached to the dynamic
       -- trace.
       tvarTrace  :: !(STRef s (Maybe (Maybe a -> a -> ST s TraceValue)))
     }

instance Eq (TVar s a) where
    (==) = on (==) tvarId

data SomeTVar s where
  SomeTVar :: !(TVar s a) -> SomeTVar s

data Deschedule = Yield | Interruptable | Blocked | Terminated | Sleep
  deriving Show
