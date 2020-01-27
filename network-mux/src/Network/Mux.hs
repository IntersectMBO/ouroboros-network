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

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Tracer
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           GHC.Stack
import           Text.Printf

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
       , MonadSTM m
       , MonadThrow m
       , MonadThrow (STM m)
       , MonadMask m
       , Eq (Async m ())
       )
    => Tracer m MuxTrace
    -> MuxApplication appType m a b
    -> MuxBearer m
    -> m ()
muxStart tracer (MuxApplication ptcls) bearer = do
    ptcls' <- mapM addProtocolQueues ptcls
    let tbl = setupTbl ptcls'
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0

    let jobs = [ (demux DemuxState { dispatchTable = tbl, Ingress.bearer }, "Demuxer")
               , (mux cnt MuxState { egressQueue   = tq,  Egress.bearer }, "Muxer")
               ]
            ++ [ job
               | (ptcl, initQ, respQ) <- ptcls'
               , job <- mpsJob cnt tq (miniProtocolNum ptcl)
                               (maximumMessageSize (miniProtocolLimits ptcl))
                               initQ respQ (miniProtocolRun ptcl)
               ]

    result <- JobPool.withJobPool $ \jobpool -> do
      sequence_ [ JobPool.forkJob jobpool (JobPool.Job job' handler)
                | (job, jobname) <- jobs
                , let job'      = job >> return (MiniProtocolShutdown jobname)
                      handler e = MiniProtocolException jobname e
                ]

      traceWith tracer (MuxTraceState Mature)

      -- Wait for the first job to terminate, successfully or otherwise
      atomically (JobPool.collect jobpool)

    -- Upon completion of withJobPool all the other jobs have been shut down.
    traceWith tracer (MuxTraceState Dead)

    case result of
      MiniProtocolException jobname e -> do
        traceWith tracer (MuxTraceExceptionExit e jobname)
        throwM e
      MiniProtocolShutdown jobname ->
        traceWith tracer (MuxTraceCleanExit jobname)
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
    setupTbl :: [( MuxMiniProtocol appType m a b
                 , StrictTVar m BL.ByteString
                 , StrictTVar m BL.ByteString
                 )]
             -> MiniProtocolDispatch m
    setupTbl ptcls' =
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
        maxpix = length ptcls' - 1

        codes   = [ miniProtocolNum ptcl | (ptcl, _, _) <- ptcls' ]
        mincode = minimum codes
        maxcode = maximum codes


    mpsJob
      :: StrictTVar m Int
      -> EgressQueue m
      -> MiniProtocolNum
      -> Int64
      -> StrictTVar m BL.ByteString
      -> StrictTVar m BL.ByteString
      -> RunMiniProtocol appType m a b
      -> [(m (), String)]
    mpsJob cnt tq mc msgMax initQ respQ run =
        case run of
          InitiatorProtocolOnly initiator ->
            [ ( do chan <- mkChannel ModeInitiator
                   _    <- initiator chan
                   mpsJobExit cnt
              , show mc ++ " Initiator" )]
          ResponderProtocolOnly responder ->
            [ ( do chan <- mkChannel ModeResponder
                   _    <- responder chan
                   mpsJobExit cnt
              , show mc ++ " Responder" )]
          InitiatorAndResponderProtocol initiator responder ->
            [ ( do chan <- mkChannel ModeInitiator
                   _    <- initiator chan
                   mpsJobExit cnt
              , show mc ++ " Initiator" )
            , ( do chan <- mkChannel ModeResponder
                   _    <- responder chan
                   mpsJobExit cnt
              , show mc ++ " Responder" )
            ]
      where
        mkChannel ModeInitiator = muxChannel tracer tq mc ModeInitiator msgMax initQ cnt
        mkChannel ModeResponder = muxChannel tracer tq mc ModeResponder msgMax respQ cnt

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

data MiniProtocolResult =
       MiniProtocolException String SomeException
     | MiniProtocolShutdown  String


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
    -> Int64
    -> StrictTVar m BL.ByteString
    -> StrictTVar m Int
    -> m (Channel m)
muxChannel tracer tq mc md msgMax q cnt = do
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
        -- This check is dependant on the good will of the sender and a receiver can't
        -- assume that it will never receive messages larger than maximumMessageSize.
        --say $ printf "send mid %s mode %s" (show mid) (show md)
        when (BL.length encoding > msgMax) $
            throwM $ MuxError MuxTooLargeMessage
                (printf "Attempting to send a message of size %d on %s %s" (BL.length encoding)
                        (show mc) (show md))
                callStack

        traceWith tracer $ MuxTraceChannelSendStart mc encoding

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
        traceWith tracer $ MuxTraceChannelRecvEnd mc blob
        return $ Just blob

traceMuxBearerState :: Tracer m MuxTrace -> MuxBearerState -> m ()
traceMuxBearerState tracer state =
    traceWith tracer (MuxTraceState state)
