{-# LANGUAGE TemplateHaskell #-}

module Process.Simulation
    ( simulate
    ) where

import           Control.Lens hiding (Empty)
import           Control.Monad.Free (Free (..))
import           Control.Monad.Random
import           Control.Monad.RWS.Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import Utils.Priority
import Process.Core

type Proc = (ProcId, Process ())

type Mailbox = Either (Seq String) (String -> Process ())

data SimState = SimState
    { _mailboxes  :: !(Map ProcId Mailbox)
    , _queue      :: !(PriorityQueue Time Proc)
    , _active     :: !(Seq Proc)
    , _time       :: !Time
    , _nextProcId :: !ProcId
    }

initSimState :: Process () -> SimState
initSimState p = SimState
    { _mailboxes  = Map.singleton 0 $ Left Empty
    , _queue      = empty
    , _active     = Seq.singleton (0, p)
    , _time       = 0
    , _nextProcId = 1
    }

makeLenses ''SimState

simulate :: MonadRandom m => Process () -> m [(Time, String)]
simulate p = do
    ((), _, logs) <- runRWST simulateM () $ initSimState p
    return logs

type M m = RWST () [(Time, String)] SimState m

simulateM :: MonadRandom m => M m ()
simulateM = do
    ps <- use active
    case ps of
        Empty -> do
            q <- use queue
            case dequeue q of
                Nothing             -> return ()
                Just (t, p, q') -> do
                    queue  .= q'
                    active .= Seq.singleton p 
                    time   .= t
                    simulateM
        ((pid, p) :<| ps') -> do
            active .= ps'
            case p of

                Pure () -> return ()

                Free (Coin prob cont) -> do
                    q <- lift $ getRandomR (0, 1)
                    let b = (q :: Double) < prob
                    active %= (|> (pid, cont b))

                Free (Delay t cont) -> do
                    now <- use time
                    queue %= enqueue (now + t) (pid, cont)

                Free (GetProcId cont) ->
                    active %= (|> (pid, cont pid))

                Free (Fork x cont) -> do
                    pid' <- use nextProcId
                    nextProcId += 1
                    mailboxes %= Map.insert pid' (Left Empty)
                    active %= (|> (pid, cont pid'))
                    active %= (|> (pid', x))

                Free (Send pid' msg cont) -> do
                    e <- use $ mailboxes . singular (ix pid')
                    active %= (|> (pid, cont))
                    case e of
                        Left _  -> mailboxes . ix pid' . _Left %= (|> msg)
                        Right q -> do
                            mailboxes . ix pid' .= Left Empty
                            active %= (|> (pid', q msg))

                Free (Receive cont) -> do
                    e <- use $ mailboxes . singular (ix pid)
                    case e of
                        Left Empty          -> mailboxes . ix pid .= Right cont
                        Left (msg :<| msgs) -> do
                            mailboxes . ix pid .= Left msgs
                            active %= (|> (pid, cont msg))
                        Right _             -> error "impossible case"

                Free (LogMsg msg cont) -> do
                    t <- use time
                    tell [(t, msg)]
                    active %= (|> (pid, cont))

                Free Fail -> return ()
                
            simulateM 
