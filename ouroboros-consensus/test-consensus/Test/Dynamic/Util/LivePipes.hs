{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Dynamic.Util.LivePipes
  ( -- * Latency configuration
    LatencyInjection (..)
  , MaxLatencies (..)
  , -- * Live Pipes
    LivePipes (..)
  , LivePipesVar
  , Pipe (..)
  , PipeId (..)
  , blockUntilQuiescent
  , newLivePipe
  , forgetLivePipeSTM
  ) where

import           Control.Monad (forM, forever, void, when)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time.Clock (picosecondsToDiffTime)
import           Data.Word (Word64)
import           GHC.Stack
import           System.Random.SplitMix (SMGen)
import qualified System.Random.SplitMix as SM

import qualified Control.Monad.Class.MonadSTM as Q
import           Control.Monad.Class.MonadSTM.Strict

import           Network.TypedProtocol.Channel

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import qualified Ouroboros.Consensus.Util.ResourceRegistry as RR

{-------------------------------------------------------------------------------
  Latency Injection specification
-------------------------------------------------------------------------------}

data LatencyInjection a
  = DoNotInjectLatencies
    -- ^ Do not inject any latencies
  | InjectLatencies a
    -- ^ Inject actual latencies
  | InjectTrivialLatencies
    -- ^ Inject latencies of 0 duration, just to test effect on scheduler
  deriving (Eq, Foldable, Functor, Show, Traversable)

-- | The maximum possible latencies used by 'newLivePipe'
--
data MaxLatencies = MaxLatencies
  { maxSendLatency :: !DiffTime
    -- ^ Similar to the Serialisation latency of GSV, though see the NOTE on
    -- 'newLivePipe' regarding back pressure
    --
  , maxVar1Latency :: !DiffTime
    -- ^ Similar to the Variable latency of GSV, though the NOTE on
    -- 'newLivePipe' regarding pipelining
  }

{-------------------------------------------------------------------------------
  Live Pipes
-------------------------------------------------------------------------------}

-- | One half of a pair of connected 'Channel's: 'send' for one and 'recv' for
-- the other
--
newtype Pipe m a = Pipe (Channel m a)

-- | How many messages have been sent on this pipe but not yet received
--
newtype InFlightCount = InFlightCount Word64
  deriving (Enum, Eq, Ord, Show)

-- | How many times has a pipe been emptied
--
newtype EmptiedCount = EmptiedCount Word64
  deriving (Enum, Eq, Ord, Show)

newtype PipeId = PipeId Word64
  deriving (Enum, Eq, Ord, Show)

data LPMetaVars m = LPMetaVars
  { lpmvCount    :: !(StrictTVar m EmptiedCount)
  , lpmvInFlight :: !(StrictTVar m InFlightCount)
  }

-- | The set of live pipes
--
-- The primary use case is 'blockUntilQuiescent'.
--
data LivePipes a = LivePipes
  { livePipes  :: !(Map PipeId a)
  , nextPipeId :: !PipeId
  }

type LivePipesVar m = StrictTVar m (LivePipes (LPMetaVars m))

-- | Block until all live pipes have been empty for at least the given duration
--
blockUntilQuiescent ::
  forall m. IOLike m => LivePipesVar m -> DiffTime -> m ()
blockUntilQuiescent livePipesVar dur = get >>= go
  where
    get :: m (Map PipeId EmptiedCount)
    get = atomically $ do
        LivePipes{livePipes} <- readTVar livePipesVar
        forM livePipes $ \LPMetaVars{lpmvCount, lpmvInFlight} -> do
            readTVar lpmvInFlight >>= check . (== InFlightCount 0)
            readTVar lpmvCount

    go sig1 = do
        threadDelay dur
        sig2 <- get
        if sig1 == sig2 then pure () else go sig2

-- | Create a new live pipe
--
-- This pipe supports multiple readers and/or multiple writers. It supports the
-- 'Channel' assumptions: reliability, order preservation, etc. Each send
-- blocks for a random amount (uniform) of time, and each message takes a
-- random amount (uniform) of time to travel from the sender to the receiver.
-- Moreover, each message only begins traveling once the previous arrives.
--
-- NOTE This mock \"physical layer\" carries one message at a time (hence the
-- name 'maxVar1Latency'). Though it uses queues so that a (pipelined) sender
-- is never blocked, there will be no _latency hiding_. In other words, the
-- stream of Variable latency samples have _already_ taken into account any
-- possible latency hiding.
--
-- NOTE Similarly, it is assumed that the Serialisation latency sufficiently
-- subsumes _back pressure_, since otherwise sending never blocks the sender.
--
-- The motivation for including latencies in this mock network is to explore
-- different permutations/ways to interleave concurrent events. While realistic
-- latencies would exhibit (conditional) (temporal) correlations due to
-- pipelining, back pressure, and so on, we assume that samplers crafted to
-- accurately synthesize those correlations would explore fewer scheduling
-- permutations.
--
-- NOTE Closing the registry fully deallocates all of the returned live pipe's
-- resources. In particular, it will call 'forgetLivePipeSTM'.
--
newLivePipe ::
  forall m a.
     ( IOLike m
     , HasCallStack
     )
  => MaxLatencies
  -> LatencyInjection (StrictTVar m SMGen)
  -> LivePipesVar m
  -> ResourceRegistry m
  -> m (PipeId, Pipe m a)
newLivePipe
  MaxLatencies{maxVar1Latency, maxSendLatency} liSMG tvar registry = do
    lpmvCount    <- uncheckedNewTVarM (EmptiedCount 0)
    lpmvInFlight <- uncheckedNewTVarM (InFlightCount 0)
    (_, pid)     <- RR.allocate registry
        (\_key -> atomically $ do
            LivePipes{nextPipeId, livePipes} <- readTVar tvar
            writeTVar tvar LivePipes
              { nextPipeId = succ nextPipeId
              , livePipes  =
                    Map.insert nextPipeId
                        LPMetaVars{lpmvCount, lpmvInFlight}
                        livePipes
              }
            pure nextPipeId)
        (atomically . forgetLivePipeSTM tvar)

    -- create a thread that reliably and order-preservingly transports messages
    -- down the pipe
    inBuf     <- atomically Q.newTQueue
    outBuf    <- atomically Q.newTQueue
    liVar1SMG <- mapM split liSMG
    void $ RR.forkThread registry $ forever $ do
        x <- atomically $ Q.readTQueue inBuf

        mapM (genDelay maxVar1Latency) liVar1SMG >>= threadDelayLI

        atomically $ Q.writeTQueue outBuf x

    liSendSMG <- mapM split liSMG
    let pipe = Pipe Channel
          { send = \ !x -> do
                atomically $ modifyTVar lpmvInFlight succ

                mapM (genDelay maxSendLatency) liSendSMG >>= threadDelayLI

                atomically $ Q.writeTQueue inBuf x
          , recv = atomically $ do
                x <- Q.readTQueue outBuf

                n <- readTVar lpmvInFlight
                when (InFlightCount 0 == n) $ error "impossible!"
                when (InFlightCount 1 == n) $ modifyTVar lpmvCount succ
                writeTVar lpmvInFlight (pred n)

                pure $ Just x
          }

    pure (pid, pipe)
  where
    split :: StrictTVar m SMGen -> m (StrictTVar m SMGen)
    split smgVar = atomically $ do
        (smg1, smg') <- SM.splitSMGen <$> readTVar smgVar
        writeTVar smgVar smg'
        newTVar smg1

    -- interpret 'LatencyInjection' via 'threadDelay'
    threadDelayLI :: LatencyInjection DiffTime -> m ()
    threadDelayLI = \case
        DoNotInjectLatencies   -> pure ()
        InjectTrivialLatencies -> threadDelay 0
        InjectLatencies d      -> threadDelay d

    -- generate a delay in the interval @[0, mx)@
    genDelay :: DiffTime -> StrictTVar m SMGen -> m DiffTime
    genDelay mx smgVar = atomically $ do
        (i, smg') <- SM.nextInt <$> readTVar smgVar
        writeTVar smgVar smg'
        let denom = picosecondsToDiffTime 1000
        let frac = toEnum (abs i `mod` 1000) / denom
        pure $ mx * frac

-- | Remove the identified pipe
--
forgetLivePipeSTM :: IOLike m => LivePipesVar m -> PipeId -> STM m ()
forgetLivePipeSTM tvar pid = modifyTVar tvar $ \lp@LivePipes{livePipes} ->
    lp{livePipes = Map.delete pid livePipes}
