{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Mux (
      muxStart

      -- * Mux bearers
    , MuxBearer

    -- * Defining 'MuxApplication's
    , AppType (..)
    , HasInitiator
    , HasResponder
    , MuxApplication (..)
    , MuxMiniProtocol (..)
    , RunMiniProtocol (..)
    , MiniProtocolNum (..)
    , MiniProtocolLimits (..)
    , MiniProtocolMode (..)

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
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BL
import           Data.Array
import qualified Data.Set as Set
import           Data.Set (Set)

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
--   'MiniProtocolMode', thus the queue contains at most @2 * |ptcl|@
--   translocation requests.
-- * at any given time each @TranslocationServiceRequest@ contains a non-empty
-- 'Wanton'
--
muxStart
    :: forall m appType a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadFork m
       , MonadSTM m
       , MonadThrow m
       , MonadThrow (STM m)
       , MonadTime  m
       , MonadTimer m
       , MonadMask m
       , Eq (Async m ())
       )
    => Tracer m MuxTrace
    -> MuxApplication appType m a b
    -> MuxBearer m
    -> m ()
muxStart tracer (MuxApplication ptcls) bearer = do
    ptcls' <- mapM addProtocolQueues ptcls
    let dispatchtbl = setupDispatchTable  ptcls'
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0

    let jobs = muxerJob tq cnt
             : demuxerJob dispatchtbl
             : catMaybes [ miniProtocolInitiatorJob cnt tq ptcl pix initQ respQ
                         | (pix, (ptcl, initQ, respQ)) <- zip [0..] ptcls' ]
            ++ catMaybes [ miniProtocolResponderJob cnt tq ptcl pix initQ respQ
                         | (pix, (ptcl, initQ, respQ)) <- zip [0..] ptcls' ]

        respondertbl = setupResponderTable
                         [ miniProtocolResponderJob cnt tq ptcl pix initQ respQ
                         | (pix, (ptcl, initQ, respQ)) <- zip [0..] ptcls' ]

    JobPool.withJobPool $ \jobpool -> do
      mapM_ (JobPool.forkJob jobpool) jobs
      traceWith tracer (MuxTraceState Mature)

      -- Wait for the first job to terminate, successfully or otherwise.
      -- All the other jobs are shut down Upon completion of withJobPool.
      monitor tracer jobpool dispatchtbl respondertbl
  where
    addProtocolQueues :: MuxMiniProtocol appType m a b
                      -> m ( MuxMiniProtocol appType m a b
                           , StrictTVar m BL.ByteString
                           , StrictTVar m BL.ByteString
                           )
    addProtocolQueues ptcl = do
        initiatorQ <- newTVarM BL.empty
        responderQ <- newTVarM BL.empty
        return (ptcl, initiatorQ, responderQ)

    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupDispatchTable :: [( MuxMiniProtocol appType m a b
                           , StrictTVar m BL.ByteString
                           , StrictTVar m BL.ByteString
                           )]
                       -> MiniProtocolDispatch m
    setupDispatchTable ptcls' =
        MiniProtocolDispatch
          (array (mincode, maxcode) $
                 [ (code, Nothing)    | code <- [mincode..maxcode] ]
              ++ [ (code, Just pix)
                 | (pix, (ptcl, _, _)) <- zip [0..] ptcls'
                 , let code = miniProtocolNum ptcl ])
          (array ((minpix, ModeInitiator), (maxpix, ModeResponder))
                 [ ((pix, mode), MiniProtocolDispatchInfo q qMax)
                 | (pix, (ptcl, initQ, respQ)) <- zip [0..] ptcls'
                 , let qMax = maximumIngressQueue (miniProtocolLimits ptcl)
                 , (mode, q) <- [ (ModeInitiator, initQ)
                                , (ModeResponder, respQ) ]
                 ])
      where
        minpix = 0
        maxpix = fromIntegral (length ptcls' - 1)

        codes   = [ miniProtocolNum ptcl | (ptcl, _, _) <- ptcls' ]
        mincode = minimum codes
        maxcode = maximum codes

    setupResponderTable :: [Maybe (JobPool.Job m MuxJobResult)]
                        -> MiniProtocolResponders m
    setupResponderTable jobs =
        MiniProtocolResponders $ array (minpix, maxpix) (zip [0..] jobs)
      where
        minpix = 0
        maxpix = fromIntegral (length jobs - 1)

    muxerJob tq cnt =
      JobPool.Job (mux cnt MuxState { egressQueue   = tq,  Egress.bearer })
                  MuxerException "muxer"

    demuxerJob tbl =
      JobPool.Job (demux DemuxState { dispatchTable = tbl, Ingress.bearer })
                  DemuxerException "demuxer"

    miniProtocolInitiatorJob = miniProtocolJob selectInitiator ModeInitiator
    miniProtocolResponderJob = miniProtocolJob selectResponder ModeResponder

    selectInitiator :: RunMiniProtocol appType m a b -> Maybe (Channel m -> m a)
    selectInitiator (ResponderProtocolOnly                   _) = Nothing
    selectInitiator (InitiatorProtocolOnly         initiator)   = Just initiator
    selectInitiator (InitiatorAndResponderProtocol initiator _) = Just initiator
    
    selectResponder :: RunMiniProtocol appType m a b -> Maybe (Channel m -> m b)
    selectResponder (ResponderProtocolOnly           responder) = Just responder
    selectResponder (InitiatorProtocolOnly         _)           = Nothing
    selectResponder (InitiatorAndResponderProtocol _ responder) = Just responder

    miniProtocolJob
      :: (RunMiniProtocol appType m a b -> Maybe (Channel m -> m c))
      -> MiniProtocolMode
      -> StrictTVar m Int
      -> EgressQueue m
      -> MuxMiniProtocol appType m a b
      -> MiniProtocolIx
      -> StrictTVar m BL.ByteString
      -> StrictTVar m BL.ByteString
      -> Maybe (JobPool.Job m MuxJobResult)
    miniProtocolJob selectRunner pmode cnt tq
                    MuxMiniProtocol {
                      miniProtocolNum    = pnum,
                      miniProtocolRun    = prunner
                    }
                    pix initQ respQ =
        job <$> selectRunner prunner
      where
        job run = JobPool.Job (jobAction run)
                              (MiniProtocolException pnum pix pmode)
                              ((show pix) ++ "." ++ (show pmode))

        jobAction run = do
          chan    <- mkChannel
          _result <- run chan
          mpsJobExit cnt
          return (MiniProtocolShutdown pnum pix pmode)

        mkChannel = muxChannel tracer tq pnum pmode
                               (selectQueue pmode) cnt

        selectQueue ModeInitiator = initQ
        selectQueue ModeResponder = respQ

    -- cnt represent the number of SDUs that are queued but not yet sent.  Job
    -- threads will be prevented from exiting until all SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: StrictTVar m Int -> m ()
    mpsJobExit cnt = do
        traceWith tracer (MuxTraceState Dying)
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

newtype MiniProtocolResponders m =
        MiniProtocolResponders
          (Array MiniProtocolIx (Maybe (JobPool.Job m MuxJobResult)))

-- | The monitoring loop does two jobs:
--
--  1. it waits for mini-protocol threads to terminate
--  2. it starts responder protocol threads on demand when the first
--     incoming message arrives.
--
monitor :: forall m. (MonadSTM m, MonadAsync m, MonadMask m)
        => Tracer m MuxTrace
        -> JobPool.JobPool m MuxJobResult
        -> MiniProtocolDispatch m
        -> MiniProtocolResponders m
        -> m ()
monitor tracer jobpool
        (MiniProtocolDispatch _ dispatchtbl)
        (MiniProtocolResponders respondertbl) =
    go Set.empty
--  TODO: for the moment on-demand starting is disabled and all mini-protocol
--  threads are started eagerly. Doing this properly involves distinguishing
--  between on-demand and eager mini-protocols, but doing so independently of
--  whether overall we established the bearer as an initiator or as a responder.
--  The on-demand vs eager distinction can be derived from which peer(s) have
--  agency in the initial state of each mini-protocol.
--
--  go (Set.fromList . range . bounds $ respondertbl)
  where
    -- To do this second job it needs to keep track of which responder protocol
    -- threads are running.
    go :: Set MiniProtocolIx
       -> m ()
    go !idleResponders = do
      result <- atomically $
            -- wait for a mini-protocol thread to terminate
            (Left  <$> JobPool.collect jobpool)
            -- or wait for data to arrive on the channels that do not yet have
            -- responder threads running
        <|> (Right <$> foldr (<|>) retry
                         [ checkNonEmptyBuf dispatchInfo >> return ix
                         | ix <- Set.elems idleResponders
                         , let dispatchInfo = dispatchtbl ! (ix, ModeResponder)
                         ])

      case result of
        -- For now we do not restart protocols, when any stop we terminate
        -- and the whole bundle will get cleaned up.
        Left (MiniProtocolShutdown pnum _pix pmode) -> do
          traceWith tracer (MuxTraceState Dead)
          traceWith tracer (MuxTraceCleanExit pnum pmode)

        Left (MiniProtocolException pnum _pix pmode e) -> do
          traceWith tracer (MuxTraceState Dead)
          traceWith tracer (MuxTraceExceptionExit pnum pmode e)
          throwM e

        -- These would always be internal errors, so propagate.
        --TODO: decide if we should have exception wrappers here to identify
        -- the source of the failure, e.g. specific mini-protocol. If we're
        -- propagating exceptions, we don't need to log them.
        Left (MuxerException   e) -> do
          traceWith tracer (MuxTraceState Dead)
          throwM e
        Left (DemuxerException e) -> do
          traceWith tracer (MuxTraceState Dead)
          throwM e

        -- Data has arrived on a channel for a mini-protocol that do not yet
        -- have a responder thread running. So we start it now.
        Right ix | Just job <- respondertbl ! ix -> do
         JobPool.forkJob jobpool job
         go (Set.delete ix idleResponders)

        Right _ix ->
          --TODO: it would be cleaner to do this check in the demuxer.
          throwM $ MuxError MuxInitiatorOnly
                      ("Received data on a responder channel but this mux "
                       ++ "instance is initiator only")
                   callStack

    checkNonEmptyBuf :: MiniProtocolDispatchInfo m -> STM m ()
    checkNonEmptyBuf (MiniProtocolDispatchInfo q _) = do
      buf <- readTVar q
      check (not (BL.null buf))


-- | The mux forks off a number of threads and its main thread waits and
-- monitors them all. This type covers the different thread and their possible
-- termination behaviour.
--
data MuxJobResult =

       -- | A mini-protocol thread terminated with a result.
       --
       MiniProtocolShutdown MiniProtocolNum MiniProtocolIx MiniProtocolMode

       -- | A mini-protocol thread terminated with an exception. We always
       -- respond by terminating the whole mux.
     | MiniProtocolException MiniProtocolNum MiniProtocolIx MiniProtocolMode
                             SomeException

       -- | Exception in the 'mux' thread. Always unexpected and fatal.
     | MuxerException   SomeException

       -- | Exception in the 'demux' thread. Always unexpected and fatal.
     | DemuxerException SomeException


-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and
-- 'MiniProtocolMode'.
--
muxChannel
    :: forall m.
       ( MonadSTM m
       , MonadThrow m
       , HasCallStack
       )
    => Tracer m MuxTrace
    -> EgressQueue m
    -> MiniProtocolNum
    -> MiniProtocolMode
    -> StrictTVar m BL.ByteString
    -> StrictTVar m Int
    -> m (Channel m)
muxChannel tracer tq mc md q cnt = do
    w <- newTVarM BL.empty
    return $ Channel { send = send (Wanton w)
                     , recv}
  where
    -- Limit for the message buffer between send and mux thread.
    perMiniProtocolBufferSize :: Int64
    perMiniProtocolBufferSize = 0x3ffff

    send :: Wanton m
         -> BL.ByteString
         -> m ()
    send want@(Wanton w) encoding = do
        -- We send CBOR encoded messages by encoding them into by ByteString
        -- forwarding them to the 'mux' thread, see 'Desired servicing semantics'.

        traceWith tracer $ MuxTraceChannelSendStart mc (fromIntegral $ BL.length encoding)

        atomically $ do
            buf <- readTVar w
            if BL.length buf < perMiniProtocolBufferSize
               then do
                   let wasEmpty = BL.null buf
                   writeTVar w (BL.append buf encoding)
                   when wasEmpty $ do
                       modifyTVar cnt (+ 1)
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
