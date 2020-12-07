{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Scratch.ThreadNet.Throttler (
  -- * Th reset slot governor
  ThrottlerArgs (..),
  blockUntilShutdownState,
  enterForge,
  exitForge,
  flushOnce,
  newArgs,
  truncateToLastFlushState,
  -- * Miscellany
  modifyingTVar
  ) where

import           Control.Monad (join)
import           Data.Function (fix)
import           Data.Functor ((<&>))
import           Data.Word (Word64)

import           Control.Tracer (Tracer (..), traceWith)

import           Control.Monad.Class.MonadSTM (MonadSTM, STM, atomically, orElse)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import qualified Control.Monad.Class.MonadTimer as MonadTimer

import           Cardano.Slotting.Slot (SlotNo (..))

import           Ouroboros.Consensus.Block.Abstract (BlockNo (..))
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike)
import qualified Ouroboros.Consensus.Util.IOLike as IOLike

import           Scratch.ThreadNet.Types

{-------------------------------------------------------------------------------
  The arguments
-------------------------------------------------------------------------------}

data ThrottlerArgs m = ThrottlerArgs
  { taStateVar :: IOLike.StrictTVar m (ThrottlerState m)
    -- ^ the current 'ThrottlerState'
  , taRecvVar  :: IOLike.StrictTVar m Word64
    -- ^ how many messages have ever been sent
  , taSendVar  :: IOLike.StrictTVar m Word64
    -- ^ how many messages have ever been received
  }

newArgs :: MonadSTM m => BlockNo -> m (ThrottlerArgs m)
newArgs firstForgedBlockNo = do
    taSendVar  <- IOLike.uncheckedNewTVarM 0
    taRecvVar  <- IOLike.uncheckedNewTVarM 0
    taStateVar <- IOLike.uncheckedNewTVarM $ Observing firstForgedBlockNo
    pure ThrottlerArgs
      { taStateVar
      , taRecvVar
      , taSendVar
      }

{-------------------------------------------------------------------------------
  Updates
-------------------------------------------------------------------------------}

-- | Update the 'ThrottlerState' when a node checks if it should lead.
enterForge ::
     MonadSTM m
  => (NodeId, IOLike.ThreadId m)
     -- ^ who exactly entered the forge
  -> SlotNo
  -> ThrottlerState m
  -> STM m (WhetherToSuppress, ThrottlerState m)
enterForge myId sl st = case st of
    Observing{}             -> pure (DoNotSuppress, st)
    Flushing1 kthActiveSlot ->
        -- It's OK for the k-th active slot to have multiple leaders.
        pure
          ( if sl == kthActiveSlot then DoNotSuppress else DoSuppress
          , st
          )
    Extending mbOccupant    ->
        -- Enforce critical section.
        case mbOccupant of
          Just{}  -> IOLike.retry
          Nothing -> pure (DoNotSuppress, Extending (Just myId))
    Flushing2{}             -> pure (DoSuppress, st)
    LastFlush{}             -> pure (DoSuppress, st)
    Shutdown{}              -> pure (DoSuppress, st)

-- | Update the 'ThrottlerState' for the observed 'ForgeExitCase'.
exitForge :: forall m.
     (Eq (IOLike.ThreadId m), Show (IOLike.ThreadId m))
  => (NodeId, IOLike.ThreadId m)
     -- ^ who exactly exited the forge
  -> SecurityParam
  -> SlotNo
     -- ^ the slot of the block that was being forged
  -> ForgeExitCase
  -> ThrottlerState m
  -> ThrottlerState m
exitForge myId (SecurityParam k) sl exitCase st = case st of
    Observing nextBno -> case exitCase of
        ForgeExitEarly   -> st
        ForgeFail        -> bad
        ForgeSuccess bno ->
          if bno + 1 == nextBno + BlockNo k then Flushing1 sl else st
    Flushing1 kthActiveSlot              ->
        -- Additional leaders of the k-th active slot can have been forging in
        -- this state; they may have entered before we started suppressing.
        if sl == kthActiveSlot then st else onlyExitEarlys
    Extending Nothing                    ->
        -- No one can exit the forge if no one has entered the forge; note the
        -- 'IOLike.retry' in 'enterForge'.
        bad
    Extending (Just occupant)
        | occupant /= myId               -> bad
        | otherwise                      -> case exitCase of
        ForgeExitEarly   ->
            -- EG PBFT's signature window prevent the node from forging.
            Extending Nothing
        ForgeFail        ->
            -- The net is fully synchronized and no messages are inflight, so
            -- no forge should fail.
            bad
        ForgeSuccess bno -> Flushing2 sl bno
    Flushing2{}                          ->
        -- We've suppressed all leaders of the @k+1@-th slot except for the one
        -- whose success transitioned us to @Flushing2@. So only
        -- 'ForgeExitEarly' is allowed here.
        onlyExitEarlys
    LastFlush{}                          ->
        -- For example, the Usher truncates to this state _during_ the forging
        -- of an test-ending block.
        st
    Shutdown{}                           -> onlyExitEarlys
  where
    bad :: forall a. a
    bad = error $ "bad exitForge " <> show (myId, sl, exitCase, st)

    onlyExitEarlys :: ThrottlerState m
    onlyExitEarlys = case exitCase of
        ForgeExitEarly -> st   -- NB suppressed leaders ExitEarly
        ForgeFail      -> bad
        ForgeSuccess{} -> bad

-- | Monitor messages in order to transition out of 'Flushing1', 'Flushing2',
-- and 'LastFlush'
--
-- ASSUMPTION No other thread writes to the net state var when it's in a
-- @Flushing_@ state.
--
-- NOTE Whether we enter the @Flushing_@ state before or after the new block
-- has sent a new message may ultimately depend on thread scheduling, so we
-- want to be robust with respect to that.
flushOnce :: forall m.
     (IOLike m, MonadTimer m)
  => Tracer m (ThrottlerEvent m)
  -> ThrottlerArgs m
  -> m ()
flushOnce tracer args = do
    -- block until we're in a relevant state
    --
    -- By design, only this thread can advance a relevant state.
    flushingState <- atomically $ IOLike.readTVar taStateVar >>= \case
      Observing{}      -> IOLike.retry
      Flushing1 sl     -> pure $ FlushingState1 sl
      Extending{}      -> IOLike.retry
      Flushing2 sl bno -> pure $ FlushingState2 sl bno
      LastFlush{}      -> pure FlushingStateLast
      Shutdown{}       -> IOLike.retry

    traceWith tracer $ FlushingOnce flushingState

    let -- After this delay, new messages --- if there would ever be any ---
        -- will have been sent.
        conservativeDelay :: IOLike.DiffTime
        conservativeDelay =
            maximumComputationalDelay + minimumPositiveDelay
          where
            -- The maximum time that can pass between a node receiving a
            -- message and that node sending a new message as a consequence of
            -- the reception.
            maximumComputationalDelay :: IOLike.DiffTime
            maximumComputationalDelay = 0   -- TODO confirm this value

            minimumPositiveDelay :: IOLike.DiffTime
            minimumPositiveDelay = 1e-12   -- cf 'IOLike.DiffTime'

    -- block until the net quiesces and then update the net state
    fix $ \loop -> do
      -- block until there are no messages inflight
      fingerprint <- atomically $ do
        sends <- IOLike.readTVar taSendVar
        recvs <- IOLike.readTVar taRecvVar
        IOLike.check $ sends == recvs
        pure sends

      traceWith tracer $ FlushingNoneInflight fingerprint

      -- block until either of the following
      --
      --   o there are new CS or BF messages
      --
      --   o there cannot be any new CS or BF messages (ie the net quiesced)
      to <- MonadTimer.newTimeout conservativeDelay
      let restart :: STM m (m ())
          restart = do
              sends <- IOLike.readTVar taSendVar
              IOLike.check $ sends > fingerprint
              pure $ do
                -- new messages have been sent, so restart the loop
                traceWith tracer $ FlushingNewMessages sends
                loop

          stop :: STM m (m ())
          stop = do
              MonadTimer.readTimeout to >>= \case
                MonadTimer.TimeoutPending   -> IOLike.retry
                MonadTimer.TimeoutFired     -> do
                    nextSt <- IOLike.readTVar taStateVar <&> \case
                      Observing{} -> error "impossible"
                      Flushing1{} ->
                          -- If we're in Flushing1, we must have been.
                          case flushingState of
                            FlushingState1{} -> Extending Nothing
                            _                -> error "impossible"
                      Extending{} -> error "impossible"
                      Flushing2{} ->
                          -- If we're in Flushing2, we must have been.
                          case flushingState of
                            FlushingState2 _sl bno -> Observing (bno + 1)
                            _                      -> error "impossible"
                      LastFlush{} ->
                          -- We transitioned to 'LastFlush' while flushing.
                          Shutdown
                      Shutdown{}  -> error "impossible"
                    IOLike.writeTVar taStateVar nextSt
                MonadTimer.TimeoutCancelled ->
                    error "this timeout is never cancelled"
              pure $ do
                -- any new message would have been sent after this much time,
                -- so conclude there will never be any and terminate the loop
                traceWith tracer $ FlushingQuiescence flushingState
      join $ atomically $ restart `orElse` stop
  where
    ThrottlerArgs
      { taStateVar
      , taRecvVar
      , taSendVar
      } = args

-- | Truncate the current state
--
-- NOTE Nodes cannot enter the forge after the
-- 'Test.Util.HardFork.OracularClock.OracularClock' is exhausted, so this
-- update does not race against 'enterForge' or 'exitForge' (see the
-- 'enterForge' call site) when called after the
-- 'Test.Util.HardFork.OracularClock.OracularClock' is exhausted.
truncateToLastFlushState ::
     IOLike m
  => Tracer m (ThrottlerEvent m)
  -> ThrottlerArgs m
  -> m ()
truncateToLastFlushState tracer throttlerArgs = do
    prevSt <- atomically $ do
      st <- IOLike.readTVar taStateVar
      pure $! case st of
              Observing{} -> ()
              Flushing1{} -> ()
              Extending{} -> ()
              Flushing2{} -> ()
              LastFlush{} -> () -- eg multiple test-ending blocks are forged
              Shutdown{}  -> error "impossible"
      IOLike.writeTVar taStateVar LastFlush
      pure st
    traceWith tracer $ EndOfDays prevSt
  where
    ThrottlerArgs { taStateVar } = throttlerArgs
    
-- | Block until 'Shutdown' is reached
blockUntilShutdownState :: IOLike m => ThrottlerArgs m -> m ()
blockUntilShutdownState throttlerArgs =
    atomically $ IOLike.readTVar taStateVar >>= \case
      Observing{} -> IOLike.retry
      Flushing1{} -> IOLike.retry
      Extending{} -> IOLike.retry
      Flushing2{} -> IOLike.retry
      LastFlush{} -> IOLike.retry
      Shutdown{}  -> pure ()
  where
    ThrottlerArgs { taStateVar } = throttlerArgs

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

modifyingTVar ::
     (Eq st, MonadSTM m)
  => IOLike.StrictTVar m st
  -> (st -> STM m (a, st))
  -> STM m (a, Maybe st)
modifyingTVar var f = do
    state <- IOLike.readTVar var
    (a, nextState) <- f state
    if state == nextState then pure (a, Nothing) else do
      IOLike.writeTVar var nextState
      pure (a, Just nextState)
