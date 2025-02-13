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

-- | Network multiplexer API.
--
-- The module should be imported qualified.
module Network.Mux
  ( -- * Defining 'Mux' protocol bundles
    new
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
  , StartOnDemandOrEagerly (..)
  , ByteChannel
  , Channel (..)
    -- * Bearer
  , Bearer
  , MakeBearer (..)
  , SDUSize (..)
    -- * Monitoring
  , miniProtocolStateMap
  , stopped
    -- * Errors
  , Error (..)
  , RuntimeError (..)
    -- * Tracing
  , traceBearerState
  , BearerState (..)
  , Trace (..)
  , WithBearer (..)
  ) where

import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Monoid.Synchronisation (FirstToFinish (..))

import Control.Applicative
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.JobPool qualified as JobPool
import Control.Exception (SomeAsyncException (..), assert)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer

import Network.Mux.Bearer
import Network.Mux.Channel
import Network.Mux.Egress as Egress
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
       muxStatus          :: StrictTVar m Status
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
    => [MiniProtocolInfo mode]
    -- ^ description of protocols run by the mux layer.  Only these protocols
    -- one will be able to execute.
    -> m (Mux mode m)
new ptcls = do
    muxMiniProtocols   <- mkMiniProtocolStateMap ptcls
    muxControlCmdQueue <- atomically newTQueue
    muxStatus <- newTVarIO Ready
    return Mux {
      muxMiniProtocols,
      muxControlCmdQueue,
      muxStatus
    }

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
    miniProtocolIngressQueue <- newTVarIO BL.empty
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
run :: forall m (mode :: Mode).
       ( MonadAsync m
       , MonadFork m
       , MonadLabelledSTM m
       , Alternative (STM m)
       , MonadThrow (STM m)
       , MonadTimer m
       , MonadMask m
       )
    => Tracer m Trace
    -> Mux mode m
    -> Bearer m
    -> m ()
run tracer
    Mux { muxMiniProtocols,
          muxControlCmdQueue,
          muxStatus
        }
    bearer@Bearer{name} = do
    egressQueue <- atomically $ newTBQueue 100

    -- label shared variables
    labelTBQueueIO egressQueue (name ++ "-mux-egress")
    labelTVarIO muxStatus (name ++ "-mux-status")
    labelTQueueIO muxControlCmdQueue (name ++ "-mux-ctrl")

    JobPool.withJobPool
      (\jobpool -> do
        JobPool.forkJob jobpool (muxerJob egressQueue)
        JobPool.forkJob jobpool demuxerJob
        traceWith tracer (TraceState Mature)

        -- Wait for someone to shut us down by calling muxStop or an error.
        -- Outstanding jobs are shut down Upon completion of withJobPool.
        withTimeoutSerial $ \timeout ->
          monitor tracer
                  timeout
                  jobpool
                  egressQueue
                  muxControlCmdQueue
                  muxStatus
      )
    -- Only handle async exceptions, 'monitor' sets 'muxStatus' before throwing
    -- an exception.  Setting 'muxStatus' is necessary to resolve a possible
    -- deadlock of mini-protocol completion action.
    `catch` \(SomeAsyncException e) -> do
      atomically $ writeTVar muxStatus (Failed $ toException e)
      throwIO e
  where
    muxerJob egressQueue =
      JobPool.Job (muxer egressQueue bearer)
                  (return . MuxerException)
                  MuxJob
                  (name ++ "-muxer")

    demuxerJob =
      JobPool.Job (demuxer (Map.elems muxMiniProtocols) bearer)
                  (return . DemuxerException)
                  MuxJob
                  (name ++ "-demuxer")

-- | Mini-protocol thread executed by `JobPool` which executes `protocolAction`.
--
miniProtocolJob
  :: forall mode m.
     ( MonadSTM m
     , MonadThread m
     , MonadThrow (STM m)
     )
  => Tracer m Trace
  -> EgressQueue m
  -> MiniProtocolState mode m
  -> MiniProtocolAction m
  -> JobPool.Job Group m JobResult
miniProtocolJob tracer egressQueue
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
                (show miniProtocolNum ++ "." ++ show miniProtocolDirEnum)
  where
    jobAction = do
      labelThisThread (case miniProtocolNum of
                        MiniProtocolNum a -> "prtcl-" ++ show a)
      w <- newTVarIO BL.empty
      let chan = muxChannel tracer egressQueue (Wanton w)
                            miniProtocolNum miniProtocolDirEnum
                            miniProtocolIngressQueue
      (result, remainder) <- miniProtocolAction chan
      traceWith tracer (TraceTerminating miniProtocolNum miniProtocolDirEnum)
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
            modifyTVar miniProtocolIngressQueue (BL.append trailing)
          Nothing ->
            pure ()

      return (MiniProtocolShutdown miniProtocolNum miniProtocolDirEnum)

    jobHandler :: SomeException -> m JobResult
    jobHandler e = do
      atomically $
        putTMVar completionVar (Left e)
        `orElse`
        throwSTM (BlockedOnCompletionVar miniProtocolNum)
      return (MiniProtocolException miniProtocolNum miniProtocolDirEnum e)

    miniProtocolDirEnum :: MiniProtocolDir
    miniProtocolDirEnum = protocolDirEnum miniProtocolDir

data ControlCmd mode m =
     CmdStartProtocolThread
       !StartOnDemandOrEagerly
       !(MiniProtocolState mode m)
       !(MiniProtocolAction m)
   | CmdShutdown

-- | Strategy how to start a mini-protocol.
--
data StartOnDemandOrEagerly =
    -- | Start a mini-protocol promptly.
    StartEagerly
    -- | Start a mini-protocol when data is received for the given
    -- mini-protocol.  Must be used only when initial message is sent by the
    -- remote side.
  | StartOnDemand
    -- | Like `StartOnDemand`, but start a mini-protocol if data is received for
    -- any mini-protocol set to `StartOnDemand`.
  | StartOnDemandAny
  deriving (Eq, Show)

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
           , MonadMask m
           , Alternative (STM m)
           , MonadThrow (STM m)
           )
        => Tracer m Trace
        -> TimeoutFn m
        -> JobPool.JobPool Group m JobResult
        -> EgressQueue m
        -> StrictTQueue m (ControlCmd mode m)
        -> StrictTVar m Status
        -> m ()
monitor tracer timeout jobpool egressQueue cmdQueue muxStatus =
    go (MonitorCtx Map.empty Map.empty)
  where
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
          go monitorCtx

        EventJobResult (MiniProtocolException pnum pmode e) -> do
          traceWith tracer (TraceState Dead)
          traceWith tracer (TraceExceptionExit pnum pmode e)
          atomically $ writeTVar muxStatus $ Failed e
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
          traceWith tracer (TraceState Dead)
          atomically $ writeTVar muxStatus $ Failed e
          throwIO e
        EventJobResult (DemuxerException e) -> do
          traceWith tracer (TraceState Dead)
          r <- atomically $ do
            size <- JobPool.readGroupSize jobpool MiniProtocolJob
            case size of
              0  | Just BearerClosed {} <- fromException e
                -> writeTVar muxStatus Stopped
                >> return True
              _ -> writeTVar muxStatus (Failed e)
                >> return False
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
                  tracer
                  egressQueue
                  ptclState
                  ptclAction
            Just cap ->
              JobPool.forkJobOn cap jobpool $
                miniProtocolJob
                  tracer
                  egressQueue
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
          -- wait for 2 seconds before the egress queue is drained
          _ <- timeout 2 $
            atomically $
                  tryPeekTBQueue egressQueue
              >>= check . isNothing
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
              tracer
              egressQueue
              ptclState
              ptclAction
        Just cap ->
          JobPool.forkJobOn cap jobpool $
            miniProtocolJob
              tracer
              egressQueue
              ptclState
              ptclAction

    checkNonEmptyQueue :: IngressQueue m -> STM m ()
    checkNonEmptyQueue q = do
      buf <- readTVar q
      check (not (BL.null buf))

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
       ( MonadSTM m
       )
    => Tracer m Trace
    -> EgressQueue m
    -> Wanton m
    -> MiniProtocolNum
    -> MiniProtocolDir
    -> IngressQueue m
    -> ByteChannel m
muxChannel tracer egressQueue want@(Wanton w) mc md q =
    Channel { send, recv}
  where
    -- Limit for the message buffer between send and mux thread.
    perMiniProtocolBufferSize :: Int64
    perMiniProtocolBufferSize = 0x3ffff

    send :: BL.ByteString -> m ()
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.

        traceWith tracer $ TraceChannelSendStart mc (fromIntegral $ BL.length encoding)

        atomically $ do
            buf <- readTVar w
            if BL.length buf < perMiniProtocolBufferSize
               then do
                   let wasEmpty = BL.null buf
                   writeTVar w (BL.append buf encoding)
                   when wasEmpty $
                     writeTBQueue egressQueue (TLSRDemand mc md want)
               else retry

        traceWith tracer $ TraceChannelSendEnd mc

    recv :: m (Maybe BL.ByteString)
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queue. This is the same queue the 'demux' thread writes to.
        traceWith tracer $ TraceChannelRecvStart mc
        blob <- atomically $ do
            blob <- readTVar q
            if blob == BL.empty
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        traceWith tracer $ TraceChannelRecvEnd mc (fromIntegral $ BL.length blob)
        return $ Just blob

traceBearerState :: Tracer m Trace -> BearerState -> m ()
traceBearerState tracer state =
    traceWith tracer (TraceState state)


--
-- Starting mini-protocol threads
--

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
runMiniProtocol :: forall mode m a.
                   ( Alternative (STM m)
                   , MonadSTM   m
                   , MonadThrow m
                   , MonadThrow (STM m)
                   )
                => Mux mode m
                -> MiniProtocolNum
                -> MiniProtocolDirection mode
                -> StartOnDemandOrEagerly
                -> (ByteChannel m -> m (a, Maybe BL.ByteString))
                -> m (STM m (Either SomeException a))
runMiniProtocol Mux { muxMiniProtocols, muxControlCmdQueue , muxStatus}
                ptclNum ptclDir startMode protocolAction

    -- Ensure the mini-protocol is known and get the status var
  | Just ptclState@MiniProtocolState{miniProtocolStatusVar}
      <- Map.lookup (ptclNum, ptclDir') muxMiniProtocols

  = atomically $ do
      st <- readTVar muxStatus
      case st of
        Stopping -> throwSTM (Shutdown Nothing st)
        Stopped  -> throwSTM (Shutdown Nothing st)
        _        -> return ()

      -- Make sure no thread is currently running, and update the status to
      -- indicate a thread is running (or ready to start on demand)
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
  = throwIO (UnknownProtocolInternalError ptclNum ptclDir')
  where
    ptclDir' = protocolDirEnum ptclDir

    -- Wait for the miniprotocol to complete.
    -- If the mux was stopped through a call to 'stop' (Stopped)
    -- or in case of an error (Failed) we return the result of
    -- the miniprotocol, or a `Error` if it was still running.
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

