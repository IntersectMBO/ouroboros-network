{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Mux (
      muxStart
    , muxBearerSetState
    , MuxSDU (..)
    , MiniProtocolLimits (..)
    , ProtocolEnum (..)
    , MiniProtocolId (..)
    , MiniProtocolMode (..)
    , MuxBearerState (..)
    , MuxError (..)
    , MuxErrorType (..)
    , RemoteClockModel (..)
    ) where

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Tracer
import           Data.Array
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.List (lookup)
import           Data.Maybe (fromMaybe)
import           GHC.Stack
import           Text.Printf

import           Network.Mux.Channel
import           Network.Mux.Interface
import           Network.Mux.Egress
import           Network.Mux.Ingress
import           Network.Mux.Types


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
-- TODO: replace MonadSay with iohk-monitoring-framework.
--
muxStart
    :: forall m appType peerid ptcl a b.
       ( MonadAsync m
       , MonadSay m
       , MonadSTM m
       , MonadThrow m
       , MonadThrow (STM m)
       , MonadMask m
       , Ord ptcl
       , Enum ptcl
       , Bounded ptcl
       , Show ptcl
       , ProtocolEnum ptcl
       , MiniProtocolLimits ptcl
       , Eq (Async m ())
       )
    => Tracer m (MuxTrace ptcl)
    -> peerid
    -> MuxApplication appType peerid ptcl m a b
    -> MuxBearer m
    -> m ()
muxStart tracer peerid (MuxApplication ptcls) bearer = do
    tbl <- setupTbl
    tq <- atomically $ newTBQueue 100
    cnt <- newTVarM 0

    let pmss = PerMuxSS tbl tq codesTbl bearer
        jobs = [ (demux pmss, "Demuxer")
               , (mux cnt pmss, "Muxer")
               , (muxControl pmss ModeResponder, "MuxControl Responder")
               , (muxControl pmss ModeInitiator, "MuxControl Initiator")
               ]
             ++ concatMap (mpsJob cnt pmss) ptcls

    mask $ \unmask -> do
      aidsAndNames <- traverse (\(io, name) -> (,name) <$> async (unmask io)) jobs

      muxBearerSetState tracer bearer Mature
      unmask $ do
        (fa, r_e) <- waitAnyCatchCancel $ map fst aidsAndNames
        let faName = fromMaybe "Unknown Protocol" (lookup fa aidsAndNames)
        case r_e of
             Left (e::SomeException) -> do
                 traceWith tracer $ MuxTraceExceptionExit e faName
                 throwM e
             Right _                 -> traceWith tracer $ MuxTraceCleanExit faName

      muxBearerSetState tracer bearer Dead

  where
    -- Construct the array of TBQueues, one for each protocol id, and each mode
    setupTbl :: m (MiniProtocolDispatch ptcl m)
    setupTbl = MiniProtocolDispatch
            -- cover full range of type (MiniProtocolId ptcl, MiniProtocolMode)
             . array (minBound, maxBound)
           <$> sequence [ do q <- atomically (newTVar BL.empty)
                             let dispatchInfo = MiniProtocolDispatchInfo
                                                  q (maximumIngressQueue ptcl)
                             return ((ptcl, mode), dispatchInfo)
                        | ptcl <- [minBound..maxBound]
                        , mode <- [ModeInitiator, ModeResponder] ]

    -- Construct the arrays mapping between protocol ids and protocol codes
    codesTbl :: MiniProtocolCodes ptcl
    codesTbl =
        MiniProtocolCodes
            (array (mincode, maxcode)
                   [ (code, mptcl)
                   | code <- [mincode..maxcode]
                   , let mptcl = toProtocolEnum code ])
      where
        codes   = map fromProtocolEnum
                      ([minBound..maxBound] :: [MiniProtocolId ptcl])
        mincode = minimum codes
        maxcode = maximum codes


    mpsJob
      :: StrictTVar m Int
      -> PerMuxSharedState ptcl m
      -> MuxMiniProtocol appType peerid ptcl m a b
      -> [(m (), String)]
    mpsJob cnt pmss ptcl =
        case ptcl of
          InitiatorProtocolOnly mpdId initiator ->
            [ ( do chan <- mkChannel ModeInitiator mpdId
                   _    <- initiator peerid chan
                   mpsJobExit cnt
              , show mpdId ++ " Initiator" )]
          ResponderProtocolOnly mpdId responder ->
            [ ( do chan <- mkChannel ModeResponder mpdId
                   _    <- responder peerid chan
                   mpsJobExit cnt
              , show mpdId ++ " Responder" )]
          InitiatorAndResponderProtocol mpdId initiator responder ->
            [ ( do chan <- mkChannel ModeInitiator mpdId
                   _    <- initiator peerid chan
                   mpsJobExit cnt
              , show mpdId ++ " Initiator" )
            , ( do chan <- mkChannel ModeResponder mpdId
                   _    <- responder peerid chan
                   mpsJobExit cnt
              , show mpdId ++ " Responder" )
            ]
      where
        mkChannel mode mpdId =
            muxChannel tracer pmss mid mc mode msgMax cnt
          where
            mid    = AppProtocolId mpdId
            mc     = fromProtocolEnum mid
            msgMax = maximumMessageSize mid

    -- cnt represent the number of SDUs that are queued but not yet sent.  Job
    -- threads will be prevented from exiting until all SDUs have been
    -- transmitted unless an exception/error is encounter. In that case all
    -- jobs will be cancelled directly.
    mpsJobExit :: StrictTVar m Int -> m ()
    mpsJobExit cnt = do
        muxBearerSetState tracer bearer Dying
        atomically $ do
            c <- readTVar cnt
            unless (c == 0) retry

muxControl :: (HasCallStack, MonadSTM m, MonadSay m, MonadThrow m, Ord ptcl, Enum ptcl)
           => PerMuxSharedState ptcl m
           -> MiniProtocolMode
           -> m ()
muxControl pmss md = do
    _ <- atomically $ do
        let MiniProtocolDispatchInfo q _ =
              ingressQueue (dispatchTable pmss) Muxcontrol md
        buf <- readTVar q
        when (buf == BL.empty)
            retry
    throwM $ MuxError MuxControlProtocolError "MuxControl message on mature MuxBearer" callStack

-- | muxChannel creates a duplex channel for a specific 'MiniProtocolId' and
-- 'MiniProtocolMode'.
--
muxChannel
    :: forall m ptcl.
       ( MonadSTM m
       , MonadSay m
       , MonadThrow m
       , Ord ptcl
       , Enum ptcl
       , Show ptcl
       , HasCallStack
       )
    => Tracer m (MuxTrace ptcl)
    -> PerMuxSharedState ptcl m
    -> MiniProtocolId ptcl
    -> MiniProtocolCode
    -> MiniProtocolMode
    -> Int64
    -> StrictTVar m Int
    -> m (Channel m)
muxChannel tracer pmss mid mc md msgMax cnt = do
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
                       writeTBQueue (tsrQueue pmss) (TLSRDemand mc md want)
               else retry

        traceWith tracer $ MuxTraceChannelSendEnd mc

    recv :: m (Maybe BL.ByteString)
    recv = do
        -- We receive CBOR encoded messages as ByteStrings (possibly partial) from the
        -- matching ingress queueu. This is the same queue the 'demux' thread writes to.
        traceWith tracer $ MuxTraceChannelRecvStart mc
        blob <- atomically $ do
            let MiniProtocolDispatchInfo q _ =
                  ingressQueue (dispatchTable pmss) mid md
            blob <- readTVar q
            if blob == BL.empty
                then retry
                else writeTVar q BL.empty >> return blob
        -- say $ printf "recv mid %s mode %s blob len %d" (show mid) (show md) (BL.length blob)
        traceWith tracer $ MuxTraceChannelRecvEnd mc blob
        return $ Just blob

muxBearerSetState :: (MonadSTM m, Ord ptcl, Enum ptcl, Bounded ptcl)
                  => Tracer m (MuxTrace ptcl)
                  -> MuxBearer m
                  -> MuxBearerState
                  -> m ()
muxBearerSetState tracer bearer newState = do
    oldState <- atomically $ do
        -- TODO: reimplement with swapTVar once it is added to MonadSTM
        old <- readTVar (state bearer)
        writeTVar (state bearer) newState
        return old
    traceWith tracer $ MuxTraceStateChange oldState newState
