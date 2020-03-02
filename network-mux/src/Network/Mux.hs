{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Network.Mux (
      muxStart

      -- * Mux bearers
    , MuxBearer

    -- * Defining 'MuxApplication's
    , MuxMode (..)
    , HasInitiator
    , HasResponder
    , MuxApplication (..)
    , MuxMiniProtocol (..)
    , RunMiniProtocol (..)
    , MiniProtocolNum (..)
    , MiniProtocolLimits (..)
    , MiniProtocolDir (..)

      -- * Errors
    , MuxError (..)
    , MuxErrorType (..)

      -- * Tracing
    , traceMuxBearerState
    , MuxBearerState (..)
    , MuxTrace (..)
    , WithMuxBearer (..)
    ) where

import           Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import           Data.Map (Map)

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer
import           GHC.Stack

import           Network.Mux.Channel
import           Network.Mux.Egress  as Egress
import           Network.Mux.Ingress as Ingress
import           Network.Mux.Types
import           Network.Mux.Trace
import qualified Network.Mux.JobPool as JobPool


muxStart
    :: forall m mode a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadFork m
       , MonadSTM m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadTimer m
       , MonadMask m
       )
    => Tracer m MuxTrace
    -> MuxApplication mode m a b
    -> MuxBearer m
    -> m ()
muxStart tracer muxapp bearer = do
    mux <- newMux (toMiniProtocolBundle muxapp)

    sequence_
      [ runMiniProtocol
          mux
          miniProtocolNum
          ptclDir
          action
      | let MuxApplication ptcls = muxapp
      , MuxMiniProtocol{miniProtocolNum, miniProtocolRun} <- ptcls
      , (ptclDir, action) <- selectRunner miniProtocolRun
      ]

    runMux tracer mux bearer
  where
    toMiniProtocolBundle :: MuxApplication mode m a b -> MiniProtocolBundle mode
    toMiniProtocolBundle (MuxApplication ptcls) =
      MiniProtocolBundle
        [ MiniProtocolInfo {
            miniProtocolNum,
            miniProtocolDir,
            miniProtocolLimits
          }
        | MuxMiniProtocol {
            miniProtocolNum,
            miniProtocolLimits,
            miniProtocolRun
          } <- ptcls
        , miniProtocolDir <- case miniProtocolRun of
            InitiatorProtocolOnly{} -> [InitiatorDirectionOnly]
            ResponderProtocolOnly{} -> [ResponderDirectionOnly]
            InitiatorAndResponderProtocol{} -> [InitiatorDirection, ResponderDirection]
        ]

    selectRunner :: RunMiniProtocol mode m a b
                 -> [(MiniProtocolDirection mode, Channel m -> m ())]
    selectRunner (InitiatorProtocolOnly initiator) =
      [(InitiatorDirectionOnly, void . initiator)]
    selectRunner (ResponderProtocolOnly responder) =
      [(ResponderDirectionOnly, void . responder)]
    selectRunner (InitiatorAndResponderProtocol initiator responder) =
      [(InitiatorDirection, void . initiator)
      ,(ResponderDirection, void . responder)]

data Mux (mode :: MuxMode) m =
     Mux {
       muxMiniProtocols   :: !(Map (MiniProtocolNum, MiniProtocolDir)
                                   (MiniProtocolState mode m)),
       muxControlCmdQueue :: !(TQueue m (ControlCmd m))
     }

newMux :: MonadSTM m  => MiniProtocolBundle mode -> m (Mux mode m)
newMux (MiniProtocolBundle ptcls) = do
    muxMiniProtocols   <- mkMiniProtocolStateMap ptcls
    muxControlCmdQueue <- atomically newTQueue
    return Mux {
      muxMiniProtocols,
      muxControlCmdQueue
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
    miniProtocolIngressQueue <- newTVarM BL.empty
    return MiniProtocolState {
       miniProtocolInfo,
       miniProtocolIngressQueue
     }


-- | muxStart starts a mux bearer for the specified protocols corresponding to
-- one of the provided Versions.
--
-- __Isometric flow control: analysis of head-of-line blocking of the ingress side of the multiplexer__
--
-- For each mini-protocol (enumeratated by @ptcl@), mux will create two
-- channels. One for initiator and one for the responder.  Each channel will use
-- a single 'Wanton'.  When it is filled, it is put in a common queue
-- 'tsrQueue'.  This means that the queue is bound by @2 * |ptcl|@.  Every side
-- of a mini-protocol is served by a single 'Wanton': when an applicaiton sends
-- data, the channel will try to put it into the 'Wanton' (which might block).
-- 'Wanton's are taken from the 'tsrQueue' queue by one of mux threads.  This
-- elimnates head of line blocking: each mini-protocol thread can block on
-- puting more bytes into its 'Wanton', but it cannot block the other
-- mini-protocols or the thread that is reading the 'tsrQueue' queue.  This is
-- ensured since the 'muxChannel' will put only a non-empty 'Wanton' to the
-- 'tsrQueue' queue, and on such wantons the queue is never blocked.  This means
-- that  the only way the queue can block is when its empty, which means that
-- none of the mini-protocols wanted to send.  The egress part will read
-- a 'Wanton', take a fixed amount of bytes encode them in as an 'MuxSDU'; if
-- there are leftovers it will put them back in the 'Wanton' and place it at the
-- end of the queue (reading and writting to it will happen in a single STM
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
runMux :: forall m mode.
          ( MonadAsync m
          , MonadCatch m
          , MonadFork m
          , MonadSTM m
          , MonadThrow (STM m)
          , MonadTime  m
          , MonadTimer m
          , MonadMask m
          )
       => Tracer m MuxTrace
       -> Mux mode m
       -> MuxBearer m
       -> m ()
runMux tracer Mux {muxMiniProtocols, muxControlCmdQueue} bearer = do
    egressQueue <- atomically $ newTBQueue 100

    JobPool.withJobPool $ \jobpool -> do
      JobPool.forkJob jobpool (muxerJob egressQueue)
      JobPool.forkJob jobpool demuxerJob
      traceWith tracer (MuxTraceState Mature)

      -- Wait for the first job to terminate, successfully or otherwise.
      -- All the other jobs are shut down Upon completion of withJobPool.
      monitor tracer jobpool egressQueue muxControlCmdQueue
  where
    muxerJob egressQueue =
      JobPool.Job (muxer egressQueue bearer)
                  MuxerException "muxer"

    demuxerJob =
      JobPool.Job (demuxer (Map.elems muxMiniProtocols) bearer)
                  DemuxerException "demuxer"

miniProtocolJob
  :: forall m c.
     (MonadSTM m, MonadThrow m)
  => Tracer m MuxTrace
  -> MiniProtocolNum
  -> MiniProtocolDir
  -> (Channel m -> m c)
  -> EgressQueue m
  -> IngressQueue m
  -> JobPool.Job m MuxJobResult
miniProtocolJob tracer pnum pmode run tq inq =
    JobPool.Job jobAction (MiniProtocolException pnum pmode)
                          (show pnum ++ "." ++ show pmode)
  where
    jobAction = do
      w       <- newTVarM BL.empty
      let chan = muxChannel tracer tq (Wanton w)
                            pnum pmode inq
      _result <- run chan
      mpsJobExit w
      return (MiniProtocolShutdown pnum pmode)

    -- The Wanton w is the SDUs that are queued but not yet sent for this job.
    -- Job threads will be prevented from exiting until all their SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: IngressQueue m -> m ()
    mpsJobExit w = do
        traceWith tracer (MuxTraceState Dying)
        atomically $ do
            buf <- readTVar w
            check (BL.null buf)

data ControlCmd m =
     CmdStartProtocolThread
       MiniProtocolNum
       MiniProtocolDir
       (IngressQueue m)
       (MiniProtocolAction m)

data MiniProtocolAction m where
     MiniProtocolAction :: (Channel m -> m a)     -- ^ Action
                        -> StrictTMVar m a        -- ^ Completion var
                        -> MiniProtocolAction m

-- | The monitoring loop does two jobs:
--
--  1. it waits for mini-protocol threads to terminate
--  2. it starts responder protocol threads on demand when the first
--     incoming message arrives.
--
monitor :: forall m. (MonadSTM m, MonadAsync m, MonadMask m)
        => Tracer m MuxTrace
        -> JobPool.JobPool m MuxJobResult
        -> EgressQueue m
        -> TQueue m (ControlCmd m)
        -> m ()
monitor tracer jobpool egressQueue cmdQueue =
    go
  where
    go :: m ()
    go = do
      result <- atomically $
            -- wait for a mini-protocol thread to terminate
            (EventJobResult <$> JobPool.collect jobpool)

            -- wait for a new control command
        <|> (EventControlCmd <$> readTQueue cmdQueue)

      case result of
        -- For now we do not restart protocols, when any stop we terminate
        -- and the whole bundle will get cleaned up.
        EventJobResult (MiniProtocolShutdown pnum pmode) -> do
          traceWith tracer (MuxTraceState Dead)
          traceWith tracer (MuxTraceCleanExit pnum pmode)

        EventJobResult (MiniProtocolException pnum pmode e) -> do
          traceWith tracer (MuxTraceState Dead)
          traceWith tracer (MuxTraceExceptionExit pnum pmode e)
          throwM e

        -- These would always be internal errors, so propagate.
        --TODO: decide if we should have exception wrappers here to identify
        -- the source of the failure, e.g. specific mini-protocol. If we're
        -- propagating exceptions, we don't need to log them.
        EventJobResult (MuxerException e) -> do
          traceWith tracer (MuxTraceState Dead)
          throwM e
        EventJobResult (DemuxerException e) -> do
          traceWith tracer (MuxTraceState Dead)
          throwM e

        EventControlCmd (CmdStartProtocolThread
                           ptclNum
                           ptclDir
                           queue
                           (MiniProtocolAction action _completionVar)) -> do
          JobPool.forkJob jobpool $
            miniProtocolJob
              tracer
              ptclNum
              ptclDir
              action
              egressQueue
              queue
          go

data MonitorEvent m =
     EventJobResult  MuxJobResult
   | EventControlCmd (ControlCmd m)

-- | The mux forks off a number of threads and its main thread waits and
-- monitors them all. This type covers the different thread and their possible
-- termination behaviour.
--
data MuxJobResult =

       -- | A mini-protocol thread terminated with a result.
       --
       MiniProtocolShutdown MiniProtocolNum MiniProtocolDir

       -- | A mini-protocol thread terminated with an exception. We always
       -- respond by terminating the whole mux.
     | MiniProtocolException MiniProtocolNum MiniProtocolDir SomeException

       -- | Exception in the 'mux' thread. Always unexpected and fatal.
     | MuxerException   SomeException

       -- | Exception in the 'demux' thread. Always unexpected and fatal.
     | DemuxerException SomeException


-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and
-- 'MiniProtocolDir'.
--
muxChannel
    :: forall m.
       ( MonadSTM m
       , MonadThrow m
       , HasCallStack
       )
    => Tracer m MuxTrace
    -> EgressQueue m
    -> Wanton m
    -> MiniProtocolNum
    -> MiniProtocolDir
    -> IngressQueue m
    -> Channel m
muxChannel tracer tq want@(Wanton w) mc md q =
    Channel { send, recv}
  where
    -- Limit for the message buffer between send and mux thread.
    perMiniProtocolBufferSize :: Int64
    perMiniProtocolBufferSize = 0x3ffff

    send :: BL.ByteString -> m ()
    send encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.

        traceWith tracer $ MuxTraceChannelSendStart mc (fromIntegral $ BL.length encoding)

        atomically $ do
            buf <- readTVar w
            if BL.length buf < perMiniProtocolBufferSize
               then do
                   let wasEmpty = BL.null buf
                   writeTVar w (BL.append buf encoding)
                   when wasEmpty $
                       writeTBQueue tq (TLSRDemand mc md want)
               else retry

        traceWith tracer $ MuxTraceChannelSendEnd mc

    recv :: m (Maybe BL.ByteString)
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        traceWith tracer $ MuxTraceChannelRecvStart mc
        blob <- atomically $ do
            blob <- readTVar q
            if blob == BL.empty
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        traceWith tracer $ MuxTraceChannelRecvEnd mc (fromIntegral $ BL.length blob)
        return $ Just blob

traceMuxBearerState :: Tracer m MuxTrace -> MuxBearerState -> m ()
traceMuxBearerState tracer state =
    traceWith tracer (MuxTraceState state)

--
-- Starting mini-protocol threads
--

-- | Arrange to run a protocol thread (for a particular 'MiniProtocolNum' and
-- 'MiniProtocolDirection') to interact on this protocol's 'Channel'.
--
-- The result is a STM action to block and wait on the protocol completion.
-- It is safe to call this completion action multiple times: it will always
-- return the same result once the protocol thread completes.
--
-- It is an error to start a new protocol thread while one is still running,
-- for the same 'MiniProtocolNum' and 'MiniProtocolDirection'. This can easily be
-- avoided by using the STM completion action to wait for the previous one to
-- finish.
--
-- It is safe to ask to start a protocol thread before 'runMux'. In this case
-- the protocol thread will not actually start until 'runMux' is called,
-- irrespective of the 'StartOnDemandOrEagerly' value.
--
runMiniProtocol :: forall mode m a.
                   (MonadSTM m, MonadThrow (STM m))
                => Mux mode m
                -> MiniProtocolNum
                -> MiniProtocolDirection mode
                -> (Channel m -> m a)
                -> m (STM m a)
runMiniProtocol Mux { muxMiniProtocols, muxControlCmdQueue }
                ptclNum ptclDir protocolAction

    -- Ensure the mini-protocol is known
  | Just MiniProtocolState{miniProtocolIngressQueue}
      <- Map.lookup (ptclNum, ptclDir') muxMiniProtocols

  = atomically $ do

      -- Tell the mux control to start the thread
      completionVar <- newEmptyTMVar
      writeTQueue muxControlCmdQueue $
        CmdStartProtocolThread
          ptclNum
          ptclDir'
          miniProtocolIngressQueue
          (MiniProtocolAction protocolAction completionVar)

      let completionAction = readTMVar completionVar
      return completionAction

    -- It is a programmer error to get the wrong protocol, but this is also
    -- very easy to avoid.
  | otherwise
  = error $ "runMiniProtocol: no such protocol num and mode in this mux: "
         ++ show ptclNum ++ " " ++ show ptclDir'
  where
    ptclDir' = protocolDirEnum ptclDir

