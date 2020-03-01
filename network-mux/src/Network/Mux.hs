{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTSyntax          #-}
{-# LANGUAGE ExistentialQuantification #-}

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

import           Control.Applicative
import qualified Control.Concurrent.JobPool as JobPool
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
muxStart tracer (MuxApplication ptcls) bearer = do
    ptcls' <- mapM addProtocolQueues ptcls
    tq <- atomically $ newTBQueue 100
    cmdQueue <- atomically newTQueue

    atomically $
      sequence_
        [ do completionVar <- newEmptyTMVar
             writeTQueue cmdQueue $
               CmdStartProtocolThread
                 ptclNum
                 ptclDir
                 queue
                 (MiniProtocolAction action completionVar)
        | (ptcl, initQ, respQ) <- ptcls'
        , let ptclNum = miniProtocolNum ptcl
        , (ptclDir, action, queue) <- selectRunner (miniProtocolRun ptcl)
                                                   initQ respQ
        ]

    JobPool.withJobPool $ \jobpool -> do
      JobPool.forkJob jobpool (muxerJob tq)
      JobPool.forkJob jobpool (demuxerJob ptcls')
      traceWith tracer (MuxTraceState Mature)

      -- Wait for the first job to terminate, successfully or otherwise.
      -- All the other jobs are shut down Upon completion of withJobPool.
      monitor tracer jobpool tq cmdQueue
  where
    addProtocolQueues :: MuxMiniProtocol mode m a b
                      -> m ( MuxMiniProtocol mode m a b
                           , IngressQueue m
                           , IngressQueue m
                           )
    addProtocolQueues ptcl = do
        initiatorQ <- newTVarM BL.empty
        responderQ <- newTVarM BL.empty
        return (ptcl, initiatorQ, responderQ)

    muxerJob tq =
      JobPool.Job (muxer tq bearer) MuxerException "muxer"

    demuxerJob ptcls' =
      JobPool.Job (demuxer ptcls' bearer) DemuxerException "demuxer"

    selectRunner :: RunMiniProtocol mode m a b
                 -> IngressQueue m
                 -> IngressQueue m
                 -> [(MiniProtocolDir, Channel m -> m (), IngressQueue m)]
    selectRunner (InitiatorProtocolOnly initiator) initQ _ =
      [(InitiatorDir, void . initiator, initQ)]
    selectRunner (ResponderProtocolOnly responder) _ respQ =
      [(ResponderDir, void . responder, respQ)]
    selectRunner (InitiatorAndResponderProtocol initiator responder) initQ respQ =
      [(InitiatorDir, void . initiator, initQ)
      ,(ResponderDir, void . responder, respQ)]

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
