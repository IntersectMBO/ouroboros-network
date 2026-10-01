{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- TODO: GHC-8.10 on Windows
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | Network multiplexer API.
--
-- The module should be imported qualified.
module Network.Mux
  ( -- * Defining 'Mux' protocol bundles
    new
  , newWithEgress
  , EgressPolicy (..)
  , directionSplit
  , egressBuckets
  , Lane (..)
  , Mux
  , Mode (..)
  , HasInitiator
  , HasResponder
  , MiniProtocolInfo (..)
  , MiniProtocolNum (..)
  , MiniProtocolDirection (..)
  , MiniProtocolLimits (..)
    -- * Running the Mux
  , run
  , stop
    -- ** Run a mini-protocol
  , runMiniProtocol
  , runMiniProtocolSTM
  , StartOnDemandOrEagerly (..)
  , ByteChannel
  , Channel (..)
    -- * Bearer
  , Bearer
  , MakeBearer (..)
  , ReadBuffer
  , MonadReadBuffer (..)
  , SDUSize (..)
    -- * Egress scheduling
  , Bucket
  , newBucket
  , setBucketRate
  , Rank (..)
  , Rotation (..)
  , setEgressRank
  , setEgressRankSource
  , ChargeSink
  , setEgressChargeSink
    -- * Monitoring
  , miniProtocolStateMap
  , stopped
    -- * Counters
  , Counters
  , withCounters
  , Counts (..)
  , Failures (..)
  , noFailures
  , noCounts
  , ByteCounts
  , SchedulingCounts (..)
  , TierCounts (..)
  , CountersTrace (..)
  , countersInterval
  , egressWaitBounds
  , egressBurstBounds
  , egressBurstWidths
    -- * Errors
  , Error (..)
  , RuntimeError (..)
    -- * Tracing
  , Tracers' (..)
  , Tracers
  , nullTracers
  , contramapTracers'
  , Trace (..)
  , BearerTrace (..)
  , ChannelTrace (..)
  , State (..)
  , traceBearerState
  , WithBearer (..)
  , TracersWithBearer
  , tracersWithBearer
  ) where

import Data.ByteString.Builder (lazyByteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Monoid.Synchronisation (FirstToFinish (..))

import Control.Applicative
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.JobPool qualified as JobPool
import Control.Exception (SomeAsyncException (..), SomeException (..), assert)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (Time)
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer

import Network.Mux.Bearer
import Network.Mux.Channel
import Network.Mux.Counters
import Network.Mux.Egress as Egress
import Network.Mux.Egress.Bucket (Bucket, BucketHandle, Rotation (..),
           newBucket, registerBearer, setBucketRate, setRank, setRankSource)
import Network.Mux.Egress.Counters (SchedulingCounts (..), TierCounts (..),
           egressBurstBounds, egressBurstWidths, egressWaitBounds)
import Network.Mux.Ingress as Ingress
import Network.Mux.Timeout
import Network.Mux.Trace
import Network.Mux.Types


-- | Mux handle which allows to control the multiplexer, e.g.
--
-- * `run`: run the multiplexer
-- * `runMiniProtocol`: start a mini-protocol (eagerly or lazily)
-- * `stop`: stop the multiplexer, causing `run` to return.
--
data Mux (mode :: Mode) m =
     Mux {
       muxMiniProtocols   :: !(Map (MiniProtocolNum, MiniProtocolDir)
                                   (MiniProtocolState mode m)),
       muxControlCmdQueue :: !(StrictTQueue m (ControlCmd mode m)),
       muxStatus          :: StrictTVar m Status,
       muxTracers         :: Tracers m,
       muxCounters        :: !(Maybe (Counters m)),
       -- ^ Node-wide counters this mux's failures and bytes are counted in,
       -- given to 'new'.
       muxEgress          :: !(Maybe (MuxEgress m))
       -- ^ this connection's part of the node's scheduled egress, if any
       -- ('newWithEgress')
     }

-- | Scheduled egress: the budget's bucket, an optional reserved slice with its
-- own bucket, and the lane each mini-protocol travels in. The default lane
-- rule is 'directionSplit'; diffusion sends TxSubmission's initiator to the
-- slice.
data EgressPolicy m = EgressPolicy {
    egressBudget :: Bucket m,
    egressSlice  :: Maybe (Bucket m),
    egressLaneOf :: MiniProtocolNum -> MiniProtocolDir -> Lane
  }

-- | The buckets behind a policy, for 'countersLoop'.
egressBuckets :: EgressPolicy m -> (Bucket m, Maybe (Bucket m))
egressBuckets EgressPolicy { egressBudget, egressSlice } = (egressBudget, egressSlice)

-- | Our own requests go direct; what we serve is scheduled.
directionSplit :: MiniProtocolNum -> MiniProtocolDir -> Lane
directionSplit _ InitiatorDir = Direct
directionSplit _ ResponderDir = Scheduled

-- | A connection's handles on the node's egress buckets.
data MuxEgress m = MuxEgress {
    meBudget       :: Bucket m,
    meBudgetHandle :: BucketHandle m,
    meSlice        :: Maybe (BucketHandle m),
    meLaneOf       :: MiniProtocolNum -> MiniProtocolDir -> Lane,
    meCharge       :: StrictTVar m (ChargeSink m)
    -- ^ where the scheduled lane reports what it wrote; a no-op until installed
  }


-- | Get information about all statically registered mini-protocols.
--
miniProtocolStateMap :: MonadSTM m
                     => Mux mode m
                     -> Map (MiniProtocolNum, MiniProtocolDir)
                            (STM m MiniProtocolStatus)
miniProtocolStateMap = fmap (readTVar . miniProtocolStatusVar)
                     . muxMiniProtocols

-- | Await until mux stopped.
--
stopped :: MonadSTM m => Mux mode m -> STM m (Maybe SomeException)
stopped Mux { muxStatus } =
    readTVar muxStatus >>= \case
      Ready      -> retry
      Failed err -> return (Just err)
      Stopping   -> retry
      Stopped    -> return Nothing


-- | Create a mux handle in `Mode` and register mini-protocols.
--
new :: forall (mode :: Mode) m.
       MonadLabelledSTM m
    => Tracers m
    -> Maybe (Counters m)
    -- ^ node-wide counters to count the mux's failures and bytes in, shared
    -- with every other mux made with them, or none; a set is only ever read
    -- by the loop 'withCounters' runs for it.
    -> [MiniProtocolInfo mode]
    -- ^ description of protocols run by the mux layer.  Only these protocols
    -- one will be able to execute.
    -> m (Mux mode m)
new = mkMux Nothing

-- | Like 'new', with this connection's egress scheduled by the node's
-- 'EgressPolicy': one queue and one muxer per lane, the scheduled lane taking a
-- grant from the budget's bucket for every batch it writes.
--
newWithEgress :: forall (mode :: Mode) m.
                 MonadLabelledSTM m
              => EgressPolicy m
              -> Tracers m
              -> Maybe (Counters m)
              -> [MiniProtocolInfo mode]
              -> m (Mux mode m)
newWithEgress EgressPolicy { egressBudget, egressSlice, egressLaneOf } muxTracers muxCounters ptcls = do
    meBudgetHandle <- registerBearer egressBudget
    meSlice        <- traverse registerBearer egressSlice
    meCharge       <- newTVarIO (\_ _ _ -> return ())
    mkMux (Just MuxEgress { meBudget = egressBudget, meBudgetHandle, meSlice,
                            meLaneOf = egressLaneOf, meCharge })
          muxTracers muxCounters ptcls

mkMux :: forall (mode :: Mode) m.
         MonadLabelledSTM m
      => Maybe (MuxEgress m)
      -> Tracers m
      -> Maybe (Counters m)
      -> [MiniProtocolInfo mode]
      -> m (Mux mode m)
mkMux muxEgress muxTracers muxCounters ptcls = do
    traceWith (tracer_ muxTracers) (TraceNewMux ptcls)
    muxMiniProtocols   <- mkMiniProtocolStateMap ptcls
    muxControlCmdQueue <- atomically newTQueue
    muxStatus <- newTVarIO Ready
    return Mux {
      muxMiniProtocols,
      muxControlCmdQueue,
      muxStatus,
      muxTracers,
      muxCounters,
      muxEgress
    }

-- | Service order of this connection under scheduled egress (lower first).
-- No effect on a mux created with 'new'.
--
setEgressRank :: MonadSTM m => Mux mode m -> Rank -> STM m ()
setEgressRank Mux { muxEgress } rank =
    case muxEgress of
         Just MuxEgress { meBudgetHandle } -> setRank meBudgetHandle rank
         Nothing                           -> return ()

-- | Like 'setEgressRank', with a rule asked whenever the bearer joins the
-- queue, given that moment.
setEgressRankSource :: MonadSTM m => Mux mode m -> (Time -> STM m Rank) -> STM m ()
setEgressRankSource Mux { muxEgress } rank =
    case muxEgress of
         Just MuxEgress { meBudgetHandle } -> setRankSource meBudgetHandle rank
         Nothing                           -> return ()

-- | Install where the scheduled lane reports the bytes it wrote, per
-- mini-protocol. Nothing on an unscheduled mux.
setEgressChargeSink :: MonadSTM m => Mux mode m -> ChargeSink m -> STM m ()
setEgressChargeSink Mux { muxEgress } sink =
    case muxEgress of
         Just MuxEgress { meCharge } -> writeTVar meCharge sink
         Nothing                     -> return ()

mkMiniProtocolStateMap :: MonadSTM m
                       => [MiniProtocolInfo mode]
                       -> m (Map (MiniProtocolNum, MiniProtocolDir)
                                 (MiniProtocolState mode m))
mkMiniProtocolStateMap ptcls =
    Map.fromList <$>
    sequence
      [ do state <- mkMiniProtocolState ptcl
           return ((miniProtocolNum, protocolDirEnum miniProtocolDir), state)
      | ptcl@MiniProtocolInfo {miniProtocolNum, miniProtocolDir} <- ptcls ]

mkMiniProtocolState :: MonadSTM m
                    => MiniProtocolInfo mode
                    -> m (MiniProtocolState mode m)
mkMiniProtocolState miniProtocolInfo = do
    miniProtocolIngressQueue <- newIngressQueue
    miniProtocolStatusVar    <- newTVarIO StatusIdle
    return MiniProtocolState {
       miniProtocolInfo,
       miniProtocolIngressQueue,
       miniProtocolStatusVar
     }

-- | Shut down the mux. This will cause 'run' to return. It does not
-- wait for any protocol threads to finish, so you should do that first if
-- necessary.
--
stop :: MonadSTM m  => Mux mode m -> m ()
stop Mux{muxControlCmdQueue} =
    atomically $ writeTQueue muxControlCmdQueue CmdShutdown


-- | Mux classification of 'Job's
--
data Group = MuxJob
           | MiniProtocolJob
  deriving (Eq, Ord)


-- | run starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
--
-- __Isometric flow control: analysis of head-of-line blocking of the ingress side of the multiplexer__
--
-- For each mini-protocol (enumerated by @ptcl@), mux will create two
-- channels. One for initiator and one for the responder.  Each channel will use
-- a single 'Wanton'.  When it is filled, it is put in a common queue
-- 'tsrQueue'.  This means that the queue is bound by @2 * |ptcl|@.  Every side
-- of a mini-protocol is served by a single 'Wanton': when an application sends
-- data, the channel will try to put it into the 'Wanton' (which might block).
-- 'Wanton's are taken from the 'tsrQueue' queue by one of mux threads.  This
-- eliminates head of line blocking: each mini-protocol thread can block on
-- putting more bytes into its 'Wanton', but it cannot block the other
-- mini-protocols or the thread that is reading the 'tsrQueue' queue.  This is
-- ensured since the 'muxChannel' will put only a non-empty 'Wanton' to the
-- 'tsrQueue' queue, and on such wantons the queue is never blocked.  This means
-- that  the only way the queue can block is when its empty, which means that
-- none of the mini-protocols wanted to send.  The egress part will read
-- a 'Wanton', take a fixed amount of bytes encode them in as an 'MuxSDU'; if
-- there are leftovers it will put them back in the 'Wanton' and place it at the
-- end of the queue (reading and writing to it will happen in a single STM
-- transaction which assures that the order of requests from a mini-protocol is
-- preserved.
--
-- Properties:
--
-- * at any given time the 'tsrQueue' contains at most one
--   'TranslocationServiceRequest' from a given mini-protocol of the given
--   'MiniProtocolDir', thus the queue contains at most @2 * |ptcl|@
--   translocation requests.
-- * at any given time each @TranslocationServiceRequest@ contains a non-empty
-- 'Wanton'
--
-- A mux is run once: nothing returns its status to 'Ready' afterwards, so a
-- second run could start no mini-protocol.
--
run :: forall m (mode :: Mode).
       ( MonadAsync m
       , MonadDelay m
       , MonadEvaluate m
       , MonadFork m
       , MonadLabelledSTM m
       , Alternative (STM m)
       , MonadThrow (STM m)
       , MonadTimer m
       , MonadMask m
       )
    => Mux mode m
    -> Bearer m
    -> m ()
run Mux { muxMiniProtocols,
          muxControlCmdQueue,
          muxStatus,
          muxTracers = tracers@TracersI {
              tracer_,
              bearerTracer_
            },
          muxCounters,
          muxEgress
        }
    bearer@Bearer{name} = do

    traceWith tracer_ TraceStarting
    (lanes, laneMuxers) <- mkLanes

    -- label shared variables
    labelTVarIO muxStatus (name ++ "-mux-status")
    labelTQueueIO muxControlCmdQueue (name ++ "-mux-ctrl")

    -- The cells outlive the job pool, so every byte the muxer counted is in
    -- them when they are retired; the demuxer counts in the ingress queues.
    withByteCells muxCounters (miniProtocolIngressQueue <$> muxMiniProtocols) $ \sent ->
      JobPool.withJobPool
        (\jobpool -> do
          mapM_ (JobPool.forkJob jobpool) (laneMuxers sent)
          JobPool.forkJob jobpool demuxerJob
          traceWith tracer_ (TraceState Mature)

          -- Wait for someone to shut us down by calling muxStop or an error.
          -- Outstanding jobs are shut down Upon completion of withJobPool.
          withTimeoutSerial $ \timeout ->
            monitor tracers
                    muxCounters
                    (Map.elems muxMiniProtocols)
                    timeout
                    jobpool
                    lanes
                    muxControlCmdQueue
                    muxStatus
        )
    -- Only handle async exceptions, 'monitor' sets 'muxStatus' before throwing
    -- an exception.  Setting 'muxStatus' is necessary to resolve a possible
    -- deadlock of mini-protocol completion action.
    `catch` \(SomeAsyncException e) -> do
      atomically $ writeTVar muxStatus (Failed $ toException e)
      traceWith tracer_ $ TraceState Dead
      throwIO e
  where

    -- One egress queue and one muxer per lane in use. Unscheduled: a single
    -- queue and a single muxer. Scheduled: the lanes write through one lock
    -- so their batches never interleave on the wire. The lock is held for the
    -- write only, never while a lane waits for a grant, so a direct SDU waits
    -- for at most one batch of each other lane. Each lane passes the
    -- writability gate on its own, so a lane granted its tokens may find the
    -- socket filled by another lane's batch in between and block in its
    -- write for that batch's worth: the gate's promise holds per lane, to
    -- within a batch of each other lane of the bearer. The muxers share the
    -- mux's byte slots, given once the cells exist.
    mkLanes :: m (Lanes m, ByteSlots m -> [JobPool.Job Group m JobResult])
    mkLanes = case muxEgress of
      Nothing -> do
        q <- newQueue "egress"
        return ( Lanes { laneQueue = const q
                       , laneOf    = \_ _ -> Scheduled
                       , laneAll   = [(Scheduled, q)] }
               , \sent -> [muxerJob "muxer" q sent Unscheduled bearer] )

      Just MuxEgress { meBudget, meBudgetHandle, meSlice, meLaneOf, meCharge } -> do
        lock <- newTMVarIO ()
        let locked = bearer { writeMany = \tr timeout sdus ->
                                bracket_ (atomically (takeTMVar lock))
                                         (atomically (putTMVar lock ()))
                                         (writeMany bearer tr timeout sdus) }
            inUse  = [Direct, Scheduled] ++ [Slice | isJust meSlice]
            egressOf Direct    = Credit meBudget
            egressOf Slice     = maybe (Credit meBudget) (`Reserved` meBudget) meSlice
            egressOf Scheduled = Scheduled_ meBudgetHandle meCharge
        qs <- forM inUse $ \lane -> (\q -> (lane, q)) <$> newQueue ("egress-" ++ laneName lane)
        let queueOf lane = case lookup lane qs of
                                Just q  -> q
                                -- 'Slice' without a slice bucket: ranked and
                                -- charged like the rest of what we serve,
                                -- rather than direct and uncapped
                                Nothing -> queueOf Scheduled
        return ( Lanes { laneQueue = queueOf, laneOf = meLaneOf, laneAll = qs }
               , \sent -> [ muxerJob ("muxer-" ++ laneName lane) q sent (egressOf lane) locked
                          | (lane, q) <- qs ] )

    newQueue label = do
      q <- atomically $ newTBQueue 100
      labelTBQueueIO q (name ++ "-mux-" ++ label)
      return q

    muxerJob label q sent laneEgress b =
      JobPool.Job (muxer q bearerTracer_ sent muxCounters laneEgress b)
                  (return . MuxerException)
                  MuxJob
                  (name ++ "-" ++ label)

    demuxerJob =
      JobPool.Job (demuxer (Map.elems muxMiniProtocols) bearerTracer_ bearer)
                  (return . DemuxerException)
                  MuxJob
                  (name ++ "-demuxer")

-- | Mini-protocol thread executed by `JobPool` which executes `protocolAction`.
--
miniProtocolJob
  :: forall mode m.
     ( MonadCatch m
     , MonadEvaluate m
     , MonadSTM m
     , MonadThrow (STM m)
     )
  => Tracers m
  -> Lanes m
  -> MiniProtocolState mode m
  -> MiniProtocolAction m
  -> JobPool.Job Group m JobResult
miniProtocolJob TracersI {
                  tracer_,
                  channelTracer_
                }
                Lanes { laneQueue, laneOf }
                MiniProtocolState {
                  miniProtocolInfo =
                    MiniProtocolInfo {
                      miniProtocolNum,
                      miniProtocolDir
                    },
                  miniProtocolIngressQueue,
                  miniProtocolStatusVar
                }
               MiniProtocolAction {
                 miniProtocolAction,
                 completionVar
               } =
    JobPool.Job jobAction
                jobHandler
                MiniProtocolJob
                ("prtcl-" ++ (case miniProtocolNum of { MiniProtocolNum a -> show a}) ++ "." ++ show miniProtocolDirEnum)
  where
    jobAction = do
      w <- newTVarIO BL.empty
      let egressQueue = laneQueue (laneOf miniProtocolNum miniProtocolDirEnum)
          chan = muxChannel channelTracer_ egressQueue (Wanton w)
                            miniProtocolNum miniProtocolDirEnum
                            miniProtocolIngressQueue
      markKilled $ do
        (result, remainder) <- miniProtocolAction chan
        traceWith tracer_ (TraceTerminating miniProtocolNum miniProtocolDirEnum)
        atomically $ do
          -- The Wanton w is the SDUs that are queued but not yet sent for this job.
          -- Job threads will be prevented from exiting until all their SDUs have been
          -- transmitted unless an exception/error is encountered. In that case all
          -- jobs will be cancelled directly.
          readTVar w >>= check . BL.null
          writeTVar miniProtocolStatusVar StatusIdle
          putTMVar completionVar (Right result)
            `orElse` throwSTM (BlockedOnCompletionVar miniProtocolNum)
          case remainder of
            Just trailing ->
              modifyTVar miniProtocolIngressQueue $ \q ->
                q { iqLength  = BL.length trailing + iqLength q,
                    iqBuilder = lazyByteString trailing <> iqBuilder q }
            Nothing ->
              pure ()

      return (MiniProtocolShutdown miniProtocolNum miniProtocolDirEnum)

    -- A thread killed from outside, by an asynchronous exception the job
    -- pool does not report, is no longer running: its status goes killed, so
    -- a close read after it is a normal end, as after a protocol that
    -- finished, and a start of the protocol is refused, the peer's side of
    -- it being wherever the kill left it. The kill may land in the action,
    -- in the trace or in the wait for the muxer to take the last bytes,
    -- which lasts as long as the write the muxer is in; past the last
    -- transaction, which fills this start's completion var as it writes the
    -- status, the status is left alone, as it may be a later start's. The
    -- exception goes on; the completion var is left to the mux's end, as
    -- before.
    markKilled :: m a -> m a
    markKilled action =
      action `catch` \(SomeAsyncException e) -> do
        atomically $ do
          done <- not <$> isEmptyTMVar completionVar
          unless done $ writeTVar miniProtocolStatusVar StatusKilled
        throwIO e

    -- NOTE: `jobHandler` is never executed on asynchronous exceptions (see
    -- `Control.Concurrent.JobPool.forkJob')`.
    jobHandler :: SomeException -> m JobResult
    jobHandler (SomeException err) = do
      -- evaluate error to WHNF; Note that this happens in the mini-protocol
      -- handler thread.
      e <- (toException <$> evaluate err) `catch` return
      _ <- atomically $ tryPutTMVar completionVar (Left e)
      return (MiniProtocolException miniProtocolNum miniProtocolDirEnum e)

    miniProtocolDirEnum :: MiniProtocolDir
    miniProtocolDirEnum = protocolDirEnum miniProtocolDir

data ControlCmd mode m =
     CmdStartProtocolThread
       !StartOnDemandOrEagerly
       !(MiniProtocolState mode m)
       !(MiniProtocolAction m)
   | CmdShutdown

data MiniProtocolAction m where
    MiniProtocolAction :: forall m a.
      { miniProtocolAction :: ByteChannel m -> m (a, Maybe BL.ByteString),
        -- ^ mini-protocol action
        completionVar      :: StrictTMVar m (Either SomeException a)
        -- ^ Completion var
      }
      -> MiniProtocolAction m

type MiniProtocolKey = (MiniProtocolNum, MiniProtocolDir)

data MonitorCtx m mode = MonitorCtx {
    -- | Mini-Protocols started on demand and waiting to be scheduled.
    --
    mcOnDemandProtocols :: !(Map MiniProtocolKey
                               (MiniProtocolState mode m, MiniProtocolAction m))
    -- | Mini-Protocols started on demand any and waiting to be scheduled.
    -- Disjoint from `mcOnDemandProtocols`.
    --
  , mcOnDemandAnyProtocols :: !(Map MiniProtocolKey
                                  (MiniProtocolState mode m, MiniProtocolAction m))
  }

-- | The monitoring loop does two jobs:
--
--  1. It waits for mini-protocol threads to terminate.
--  2. It starts responder protocol threads on demand when the first
--     incoming message arrives.
--
monitor :: forall mode m.
           ( MonadAsync m
           , MonadEvaluate m
           , MonadMask m
           , Alternative (STM m)
           , MonadThrow (STM m)
           )
        => Tracers m
        -> Maybe (Counters m)
        -> [MiniProtocolState mode m]
        -> TimeoutFn m
        -> JobPool.JobPool JobPool.WithQueue Group m JobResult
        -> Lanes m
        -> StrictTQueue m (ControlCmd mode m)
        -> StrictTVar m Status
        -> m ()
monitor tracers@TracersI {
          tracer_       = tracer,
          bearerTracer_ = bearerTracer
        }
        counters ptcls timeout jobpool lanes cmdQueue muxStatus =
    go (MonitorCtx Map.empty Map.empty)
  where
    -- Whether a mini-protocol is running, by the status each keeps: running
    -- from its start command, idle from its last transaction, killed from
    -- its thread's death by an asynchronous exception; a close the demuxer
    -- reads is a failure only then. So a protocol the application has
    -- started whose thread is not yet forked counts as running, and the peer
    -- closing then is a failure, as a close an instant later is; and a
    -- protocol past its last transaction whose job has not yet deregistered
    -- does not.
    anyRunning :: STM m Bool
    anyRunning = elem StatusRunning <$> mapM (readTVar . miniProtocolStatusVar) ptcls

    go :: MonitorCtx m mode -> m ()
    go monitorCtx@MonitorCtx { mcOnDemandProtocols
                             , mcOnDemandAnyProtocols } = do
      result <- atomically $ runFirstToFinish $
            -- wait for a mini-protocol thread to terminate
           FirstToFinish (EventJobResult <$> JobPool.waitForJob jobpool)

            -- wait for a new control command
        <> FirstToFinish (EventControlCmd <$> readTQueue cmdQueue)

            -- or wait for data to arrive on the channels that do not yet have
            -- responder threads running
        <> foldMap
             (\(ptclState, ptclAction) ->
               FirstToFinish $ do
                 checkNonEmptyQueue (miniProtocolIngressQueue ptclState)
                 return (EventStartOnDemand ptclState ptclAction)
             )
             mcOnDemandProtocols
       <> foldMap
             (\(ptclState, _ptclAction) ->
               FirstToFinish $ do
                 checkNonEmptyQueue (miniProtocolIngressQueue ptclState)
                 return EventStartOnDemandAny
             )
             mcOnDemandAnyProtocols
      case result of
        -- Protocols that runs to completion are not automatically restarted.
        EventJobResult (MiniProtocolShutdown pnum pmode) -> do
          traceWith tracer (TraceCleanExit pnum pmode)
          traceWith bearerTracer TraceEmitDeltaQ
          go monitorCtx

        EventJobResult (MiniProtocolException pnum pmode e) -> do
          atomically $ do
            writeTVar muxStatus $ Failed e
            countMiniProtocolFailure counters pnum pmode e
          traceWith tracer (TraceExceptionExit pnum pmode e)
          traceWith tracer (TraceState Dead)
          traceWith bearerTracer TraceEmitDeltaQ
          throwIO e

        -- These two cover internal and protocol errors.  The muxer exception is
        -- always fatal.  The demuxer exception 'BearerClosed' when all
        -- mini-protocols stopped indicates a normal shutdown and thus it is not
        -- propagated.
        --
        -- TODO: decide if we should have exception wrappers here to identify
        -- the source of the failure, e.g. specific mini-protocol. If we're
        -- propagating exceptions, we don't need to log them.
        EventJobResult (MuxerException e) -> do
          atomically $ do
            writeTVar muxStatus $ Failed e
            countMuxerFailure counters e
          traceWith tracer (TraceState Dead)
          throwIO e
        EventJobResult (DemuxerException e) -> do
          -- A close read after every mini-protocol has finished is a normal
          -- end and counts nothing; one read while a protocol runs, and a
          -- reset at any time, fail the mux and are counted.
          r <- atomically $ do
            stopped <- case fromException e of
              Just BearerClosed {} -> not <$> anyRunning
              _                    -> return False
            if stopped
               then writeTVar muxStatus Stopped
               else countDemuxerFailure counters e
                 >> writeTVar muxStatus (Failed e)
            return stopped
          traceWith tracer (TraceState Dead)
          unless r (throwIO e)

        EventControlCmd (CmdStartProtocolThread
                           StartEagerly
                           ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir,
                               miniProtocolCapability
                             }
                           }
                           ptclAction) -> do
          traceWith tracer (TraceStartEagerly miniProtocolNum
                                              (protocolDirEnum miniProtocolDir))
          case miniProtocolCapability of
            Nothing ->
              JobPool.forkJob jobpool $
                miniProtocolJob
                  tracers
                  lanes
                  ptclState
                  ptclAction
            Just cap ->
              JobPool.forkJobOn cap jobpool $
                miniProtocolJob
                  tracers
                  lanes
                  ptclState
                  ptclAction
          go monitorCtx

        EventControlCmd (CmdStartProtocolThread
                           StartOnDemand
                           ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir
                             }
                           }
                           ptclAction) -> do
          let monitorCtx' = monitorCtx { mcOnDemandProtocols =
                                           Map.insert (protocolKey ptclState)
                                                      (ptclState, ptclAction)
                                                      mcOnDemandProtocols
                                       }
          traceWith tracer (TraceStartOnDemand miniProtocolNum
                             (protocolDirEnum miniProtocolDir))
          go monitorCtx'

        EventControlCmd (CmdStartProtocolThread
                           StartOnDemandAny
                           ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir
                             }
                           }
                           ptclAction) -> do
          let monitorCtx' = monitorCtx { mcOnDemandAnyProtocols =
                                           Map.insert (protocolKey ptclState)
                                                      (ptclState, ptclAction)
                                                      mcOnDemandAnyProtocols
                                       }
          traceWith tracer (TraceStartOnDemandAny miniProtocolNum
                             (protocolDirEnum miniProtocolDir))
          go monitorCtx'

        EventControlCmd CmdShutdown -> do
          traceWith tracer TraceStopping
          atomically $ writeTVar muxStatus Stopping
          JobPool.cancelGroup jobpool MiniProtocolJob
          -- wait for 2 seconds before every lane's egress queue is drained
          _ <- timeout 2 $
            atomically $ do
              heads <- mapM (tryPeekTBQueue . snd) (laneAll lanes)
              check (all isNothing heads)
          atomically $ writeTVar muxStatus Stopped
          traceWith tracer TraceStopped
          -- by exiting the 'monitor' loop we let the job pool kill demuxer and
          -- muxer threads

        -- Data has arrived on a channel for a mini-protocol for which we have
        -- an on-demand-start protocol thread. So we start it now along with all
        -- StartOnDemandAny protocols.
        EventStartOnDemand ptclState ptclAction ->
          let ptclKey = protocolKey ptclState in
          assert (Map.null (mcOnDemandAnyProtocols `Map.intersection` mcOnDemandProtocols)) $ do
          doStartOnDemand ptclState ptclAction

          -- Also start any StartOnDemandAny protocols
          mapM_ (uncurry doStartOnDemand) mcOnDemandAnyProtocols

          let monitorCtx' = MonitorCtx { mcOnDemandProtocols =
                                           Map.delete ptclKey mcOnDemandProtocols
                                       , mcOnDemandAnyProtocols = Map.empty
                                       }

          go monitorCtx'

        -- Data has arrived on a channel for a mini-protocol for which we have
        -- an on-demand-start-any protocol thread. So we start them all now.
        EventStartOnDemandAny  -> do
          mapM_ (uncurry doStartOnDemand) mcOnDemandAnyProtocols

          go $ monitorCtx { mcOnDemandAnyProtocols = Map.empty }

    doStartOnDemand :: MiniProtocolState mode m
                        -> MiniProtocolAction m
                        -> m ()
    doStartOnDemand ptclState@MiniProtocolState {
                      miniProtocolInfo = MiniProtocolInfo {
                           miniProtocolNum,
                           miniProtocolDir,
                           miniProtocolCapability
                      },
                      miniProtocolStatusVar
                    }
                    ptclAction = do
      traceWith tracer (TraceStartedOnDemand miniProtocolNum
                                             (protocolDirEnum miniProtocolDir))
      atomically $ modifyTVar miniProtocolStatusVar (\a -> assert (a /= StatusRunning) StatusRunning)
      case miniProtocolCapability of
        Nothing ->
          JobPool.forkJob jobpool $
            miniProtocolJob
              tracers
              lanes
              ptclState
              ptclAction
        Just cap ->
          JobPool.forkJobOn cap jobpool $
            miniProtocolJob
              tracers
              lanes
              ptclState
              ptclAction

    checkNonEmptyQueue :: IngressQueue m -> STM m ()
    checkNonEmptyQueue q = do
      l <- iqLength <$> readTVar q
      check (l /= 0)

    protocolKey :: MiniProtocolState mode m -> MiniProtocolKey
    protocolKey MiniProtocolState {
                  miniProtocolInfo = MiniProtocolInfo {
                    miniProtocolNum,
                    miniProtocolDir
                  }
                } =
      (miniProtocolNum, protocolDirEnum miniProtocolDir)

data MonitorEvent mode m =
     EventJobResult  JobResult
   | EventControlCmd (ControlCmd mode m)
   | EventStartOnDemand (MiniProtocolState mode m)
                        (MiniProtocolAction m)
   | EventStartOnDemandAny

-- | The mux forks off a number of threads and its main thread waits and
-- monitors them all. This type covers the different thread and their possible
-- termination behaviour.
--
data JobResult =

       -- | A mini-protocol thread terminated with a result.
       --
       MiniProtocolShutdown MiniProtocolNum MiniProtocolDir

       -- | A mini-protocol thread terminated with an exception. We always
       -- respond by terminating the whole mux.
     | MiniProtocolException MiniProtocolNum MiniProtocolDir SomeException

       -- | Exception in the 'mux' thread. Always fatal.
     | MuxerException   SomeException

       -- | Exception in the 'demux' thread. Always fatal.
     | DemuxerException SomeException


-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and
-- 'MiniProtocolDir'.
--
muxChannel
    :: forall m.
       ( MonadEvaluate m
       , MonadSTM m
       )
    => Tracer m ChannelTrace
    -> EgressQueue m
    -> Wanton m
    -> MiniProtocolNum
    -> MiniProtocolDir
    -> IngressQueue m
    -> ByteChannel m
muxChannel tracer egressQueue want@(Wanton w) mc md q =
    Channel { send, recv }
  where
    -- The length of a lazy 'BL.ByteString' counted no further than @n@
    -- bytes. It forces the chunks holding them and nothing after, so it is
    -- below @n@ exactly when the string ended within @n@.
    lengthUpTo :: Int64 -> BL.ByteString -> Int64
    lengthUpTo n = BL.length . BL.take n

    send :: BL.ByteString -> m ()
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.
        traceWith tracer $ TraceChannelSendStart mc

        -- Force the encoding up to the soft limit here, in the mini-protocol's
        -- thread, to a chunk boundary: unforced, a waiting message keeps alive
        -- whatever its encoder refers to. The muxer encodes the rest as it
        -- sends it.
        held <- evaluate (lengthUpTo egressSoftBufferLimit encoding)

        atomically $ do
            buf <- readTVar w
            -- Count no further than the limit, so the check forces nothing
            -- past it; BL.length only stops there with -O, by a rewrite rule.
            if lengthUpTo egressSoftBufferLimit buf < egressSoftBufferLimit
               then do
                   let wasEmpty = BL.null buf
                   writeTVar w (BL.append buf encoding)
                   when wasEmpty $
                     writeTBQueue egressQueue (TLSRDemand mc md want)
               else retry

        -- The length: the count already made for a message that ended within
        -- the limit; for a longer one, counted as the event is rendered.
        let len = if held < egressSoftBufferLimit
                     then fromIntegral held
                     else fromIntegral (BL.length encoding)
        traceWith tracer $ TraceChannelSendEnd mc len

    recv :: m (Maybe BL.ByteString)
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queue. This is the same queue the 'demux' thread writes to.
        traceWith tracer $ TraceChannelRecvStart mc
        blob <- atomically $ do
            queued <- readTVar q
            if iqLength queued == 0
                then retry
                else do
                  -- The bytes read for the protocol stay counted.
                  writeTVar q queued { iqLength = 0, iqBuilder = mempty }
                  return (toLazyByteString (iqBuilder queued))
        traceWith tracer $ TraceChannelRecvEnd mc (fromIntegral $ BL.length blob)
        return $ Just blob

traceBearerState :: Monad m => Tracer m Trace -> State -> m ()
traceBearerState tracer state =
    traceWith tracer (TraceState state)


--
-- Starting mini-protocol threads
--

-- | An underlying STM version of `runMiniProtocol`.
--
runMiniProtocolSTM
  :: forall mode m a.
     ( Alternative (STM m)
     , MonadSTM   m
     , MonadThrow (STM m)
     )
  => Mux mode m
  -> MiniProtocolNum
  -> MiniProtocolDirection mode
  -> StartOnDemandOrEagerly
  -> (ByteChannel m -> m (a, Maybe BL.ByteString))
  -> STM m (STM m (Either SomeException a))
runMiniProtocolSTM Mux { muxMiniProtocols,
                      muxControlCmdQueue,
                      muxStatus
                    }
                ptclNum ptclDir
                startMode
                protocolAction

    -- Ensure the mini-protocol is known and get the status var
  | Just ptclState@MiniProtocolState{miniProtocolStatusVar}
      <- Map.lookup (ptclNum, ptclDir') muxMiniProtocols

  = do
      st <- readTVar muxStatus
      case st of
        Stopping -> throwSTM (Shutdown Nothing st)
        Stopped  -> throwSTM (Shutdown Nothing st)
        Failed e -> throwSTM (Shutdown (Just e) st)
        Ready {} -> return ()

      -- Make sure no thread is currently running, or was killed, and update
      -- the status to indicate a thread is running (or ready to start on
      -- demand)
      status <- readTVar miniProtocolStatusVar
      unless (status == StatusIdle) $
        throwSTM (ProtocolAlreadyRunning ptclNum ptclDir' status)
      let !status' = case startMode of
                       StartOnDemand    -> StatusStartOnDemand
                       StartOnDemandAny -> StatusStartOnDemandAny
                       StartEagerly     -> StatusRunning
      writeTVar miniProtocolStatusVar status'

      -- Tell the mux control to start the thread
      completionVar <- newEmptyTMVar
      writeTQueue muxControlCmdQueue $
        CmdStartProtocolThread
          startMode
          ptclState
          (MiniProtocolAction protocolAction completionVar)

      return $ completionAction completionVar

    -- It is a programmer error to get the wrong protocol, but this is also
    -- very easy to avoid.
  | otherwise
  = throwSTM (UnknownProtocolInternalError ptclNum ptclDir')
  where
    ptclDir' = protocolDirEnum ptclDir

    -- Wait for the miniprotocol to complete.
    -- If the mux was stopped through a call to 'stop' (Stopped)
    -- or in case of an error (Failed) we return the result of
    -- the miniprotocol, or a `Error` if it was still running.
    completionAction :: StrictTMVar m (Either SomeException b)
                     -> STM m (Either SomeException b)
    completionAction completionVar = do
      st <- readTVar muxStatus
      case st of
           Ready    -> readTMVar completionVar
           Stopping -> readTMVar completionVar
                   <|> return (Left $ toException (Shutdown Nothing st))
           Stopped  -> readTMVar completionVar
                   <|> return (Left $ toException (Shutdown Nothing st))
           Failed e -> readTMVar completionVar
                   <|> return (Left $ toException (Shutdown (Just e) st))


-- | Arrange to run a protocol thread (for a particular 'MiniProtocolNum' and
-- 'MiniProtocolDirection') to interact on this protocol's 'Channel'.
--
-- The protocol thread can either be started eagerly or on-demand:
--
-- * With 'StartEagerly', the thread is started promptly. This is appropriate
--   for mini-protocols where the opening message may be sent by this thread.
--
-- * With 'StartOnDemand', the thread is not started until the first data is
--   received for this mini-protocol. This is appropriate for mini-protocols
--   where the opening message is sent by the remote peer.
--
-- The result is a STM action to block and wait on the protocol completion.
-- It is safe to call this completion action multiple times: it will always
-- return the same result once the protocol thread completes.
-- In case the Mux has stopped, either due to an exception or because of a call
-- to muxStop a `Left Error` will be returned from the STM action.
--
-- It is an error to start a new protocol thread while one is still running,
-- for the same 'MiniProtocolNum' and 'MiniProtocolDirection'. This can easily be
-- avoided by using the STM completion action to wait for the previous one to
-- finish.
--
-- It is safe to ask to start a protocol thread before 'run'. In this case
-- the protocol thread will not actually start until 'run' is called,
-- irrespective of the 'StartOnDemandOrEagerly' value.
--
runMiniProtocol
  :: forall mode m a.
     ( Alternative (STM m)
     , MonadSTM   m
     , MonadThrow (STM m)
     )
  => Mux mode m
  -> MiniProtocolNum
  -> MiniProtocolDirection mode
  -> StartOnDemandOrEagerly
  -> (ByteChannel m -> m (a, Maybe BL.ByteString))
  -> m (STM m (Either SomeException a))
runMiniProtocol mux ptclNum ptclDir startMode protocolAction =
    atomically $ runMiniProtocolSTM mux ptclNum ptclDir startMode protocolAction
