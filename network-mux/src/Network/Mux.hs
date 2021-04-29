{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Network.Mux (

    -- * Defining 'Mux' protocol bundles
      newMux
    , Mux
    , MuxMode (..)
    , HasInitiator
    , HasResponder
    , MiniProtocolBundle (..)
    , MiniProtocolInfo (..)
    , MiniProtocolNum (..)
    , MiniProtocolDirection (..)
    , MiniProtocolLimits (..)

      -- * Running the Mux
    , runMux
    , MuxBearer
    , runMiniProtocol
    , StartOnDemandOrEagerly (..)
    , stopMux

     -- * Monitoring
    , miniProtocolStateMap
    , muxStopped

      -- * Errors
    , MuxError (..)
    , MuxErrorType (..)

      -- * Tracing
    , traceMuxBearerState
    , MuxBearerState (..)
    , MuxTrace (..)
    , WithMuxBearer (..)
    ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

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

import           Network.Mux.Channel
import           Network.Mux.Egress as Egress
import           Network.Mux.Ingress as Ingress
import           Network.Mux.Trace
import           Network.Mux.Types


data Mux (mode :: MuxMode) m =
     Mux {
       muxMiniProtocols   :: !(Map (MiniProtocolNum, MiniProtocolDir)
                                   (MiniProtocolState mode m)),
       muxControlCmdQueue :: !(TQueue m (ControlCmd mode m)),
       muxStatus          :: StrictTVar m MuxStatus
     }


miniProtocolStateMap :: MonadSTM m
                     => Mux mode m
                     -> Map (MiniProtocolNum, MiniProtocolDir)
                            (STM m MiniProtocolStatus)
miniProtocolStateMap = fmap (readTVar . miniProtocolStatusVar)
                     . muxMiniProtocols

-- | Await until mux stopped.
--
muxStopped :: MonadSTM m => Mux mode m -> STM m (Maybe SomeException)
muxStopped Mux { muxStatus } =
    readTVar muxStatus >>= \status -> case status of
      MuxReady      -> retry
      MuxFailed err -> return (Just err)
      MuxStopped    -> return Nothing


data MuxStatus = MuxReady | MuxFailed SomeException | MuxStopped

newMux :: MonadSTM m  => MiniProtocolBundle mode -> m (Mux mode m)
newMux (MiniProtocolBundle ptcls) = do
    muxMiniProtocols   <- mkMiniProtocolStateMap ptcls
    muxControlCmdQueue <- atomically newTQueue
    muxStatus <- newTVarIO MuxReady
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

-- | Shut down the mux. This will cause 'runMux' to return. It does not
-- wait for any protocol threads to finish, so you should do that first if
-- necessary.
--
stopMux :: MonadSTM m  => Mux mode m -> m ()
stopMux Mux{muxControlCmdQueue} =
    atomically $ writeTQueue muxControlCmdQueue CmdShutdown


-- | runMux starts a mux bearer for the specified protocols corresponding to
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
          , MonadThrow (STM m)
          , MonadTime  m
          , MonadTimer m
          , MonadMask m
          )
       => Tracer m MuxTrace
       -> Mux mode m
       -> MuxBearer m
       -> m ()
runMux tracer Mux {muxMiniProtocols, muxControlCmdQueue, muxStatus} bearer = do
    egressQueue <- atomically $ newTBQueue 100

    JobPool.withJobPool $ \jobpool -> do
      JobPool.forkJob jobpool (muxerJob egressQueue)
      JobPool.forkJob jobpool demuxerJob
      traceWith tracer (MuxTraceState Mature)

      -- Wait for someone to shut us down by calling muxStop or an error.
      -- Outstaning jobs are shut down Upon completion of withJobPool.
      monitor tracer jobpool egressQueue muxControlCmdQueue muxStatus
  where
    muxerJob egressQueue =
      JobPool.Job (muxer egressQueue bearer)
                  (return . MuxerException)
                  "muxer"

    demuxerJob =
      JobPool.Job (demuxer (Map.elems muxMiniProtocols) bearer)
                  (return . DemuxerException)
                  "demuxer"

miniProtocolJob
  :: forall mode m.
     ( MonadSTM m
     , MonadThrow (STM m)
     )
  => Tracer m MuxTrace
  -> EgressQueue m
  -> MiniProtocolState mode m
  -> MiniProtocolAction m
  -> JobPool.Job m MuxJobResult
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
                (MiniProtocolAction protocolAction completionVar) =
    JobPool.Job jobAction
                jobHandler
                (show miniProtocolNum ++ "." ++ show miniProtocolDirEnum)
  where
    jobAction = do
      w       <- newTVarIO BL.empty
      let chan = muxChannel tracer egressQueue (Wanton w)
                           miniProtocolNum miniProtocolDirEnum
                           miniProtocolIngressQueue
      (result, remainder)  <- protocolAction chan
      traceWith tracer (MuxTraceTerminating miniProtocolNum miniProtocolDirEnum)
      atomically $ do
        -- The Wanton w is the SDUs that are queued but not yet sent for this job.
        -- Job threads will be prevented from exiting until all their SDUs have been
        -- transmitted unless an exception/error is encountered. In that case all
        -- jobs will be cancelled directly.
        readTVar w >>= check . BL.null
        writeTVar miniProtocolStatusVar StatusIdle
        putTMVar completionVar (Right result)
          `orElse` (throwSTM (MuxError (MuxBlockedOnCompletionVar miniProtocolNum) ""))
        case remainder of
          Just trailing ->
            modifyTVar miniProtocolIngressQueue (BL.append trailing)
          Nothing ->
            pure ()

      return (MiniProtocolShutdown miniProtocolNum miniProtocolDirEnum)

    jobHandler :: SomeException -> m MuxJobResult
    jobHandler e = do
      atomically $
        putTMVar completionVar (Left e)
        `orElse`
        throwSTM (MuxError (MuxBlockedOnCompletionVar miniProtocolNum)
                           ("when caught: " ++ show e))
      return (MiniProtocolException miniProtocolNum miniProtocolDirEnum e)

    miniProtocolDirEnum :: MiniProtocolDir
    miniProtocolDirEnum = protocolDirEnum miniProtocolDir

data ControlCmd mode m =
     CmdStartProtocolThread
       !StartOnDemandOrEagerly
       !(MiniProtocolState mode m)
       !(MiniProtocolAction m)
   | CmdShutdown

data StartOnDemandOrEagerly = StartOnDemand | StartEagerly
  deriving Eq

data MiniProtocolAction m where
     MiniProtocolAction :: (Channel m -> m (a, Maybe BL.ByteString)) -- ^ Action
                        -> StrictTMVar m (Either SomeException a)    -- ^ Completion var
                        -> MiniProtocolAction m

-- | The monitoring loop does two jobs:
--
--  1. it waits for mini-protocol threads to terminate
--  2. it starts responder protocol threads on demand when the first
--     incoming message arrives.
--
monitor :: forall mode m. (MonadSTM m, MonadAsync m, MonadMask m, MonadThrow (STM m))
        => Tracer m MuxTrace
        -> JobPool.JobPool m MuxJobResult
        -> EgressQueue m
        -> TQueue m (ControlCmd mode m)
        -> StrictTVar m MuxStatus
        -> m ()
monitor tracer jobpool egressQueue cmdQueue muxStatus =
    go Map.empty
  where
    go :: Map (MiniProtocolNum, MiniProtocolDir)
              (MiniProtocolState mode m, MiniProtocolAction m)
       -> m ()
    go !ptclsStartOnDemand = do
      result <- atomically $
            -- wait for a mini-protocol thread to terminate
            (EventJobResult <$> JobPool.collect jobpool)

            -- wait for a new control command
        <|> (EventControlCmd <$> readTQueue cmdQueue)

            -- or wait for data to arrive on the channels that do not yet have
            -- responder threads running
        <|> foldr (<|>) retry
              [ checkNonEmptyQueue (miniProtocolIngressQueue ptclState) >>
                return (EventStartOnDemand ptclState ptclAction)
              | (ptclState, ptclAction) <- Map.elems ptclsStartOnDemand ]

      case result of
        -- Protocols that runs to completion are not automatically restarted.
        EventJobResult (MiniProtocolShutdown pnum pmode) -> do
          traceWith tracer (MuxTraceCleanExit pnum pmode)
          go ptclsStartOnDemand

        EventJobResult (MiniProtocolException pnum pmode e) -> do
          traceWith tracer (MuxTraceState Dead)
          traceWith tracer (MuxTraceExceptionExit pnum pmode e)
          atomically $ writeTVar muxStatus $ MuxFailed e
          throwIO e

        -- These two cover internal and protocol errors, but always fatal, so propagate.
        --TODO: decide if we should have exception wrappers here to identify
        -- the source of the failure, e.g. specific mini-protocol. If we're
        -- propagating exceptions, we don't need to log them.
        EventJobResult (MuxerException e) -> do
          traceWith tracer (MuxTraceState Dead)
          atomically $ writeTVar muxStatus $ MuxFailed e
          throwIO e
        EventJobResult (DemuxerException e) -> do
          traceWith tracer (MuxTraceState Dead)
          atomically $ writeTVar muxStatus $ MuxFailed e
          throwIO e

        EventControlCmd (CmdStartProtocolThread
                           StartEagerly
                           ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir
                             }
                           }
                           ptclAction) -> do
          traceWith tracer (MuxTraceStartEagerly miniProtocolNum
                             (protocolDirEnum miniProtocolDir))
          JobPool.forkJob jobpool $
            miniProtocolJob
              tracer
              egressQueue
              ptclState
              ptclAction
          go ptclsStartOnDemand

        EventControlCmd (CmdStartProtocolThread
                           StartOnDemand
                           ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir
                             }
                           }
                           ptclAction) -> do
          let ptclsStartOnDemand' = Map.insert (protocolKey ptclState)
                                               (ptclState, ptclAction)
                                               ptclsStartOnDemand
          traceWith tracer (MuxTraceStartedOnDemand miniProtocolNum
                             (protocolDirEnum miniProtocolDir))
          go ptclsStartOnDemand'

        EventControlCmd CmdShutdown -> do
          traceWith tracer MuxTraceShutdown
          atomically $ writeTVar muxStatus MuxStopped

        -- Data has arrived on a channel for a mini-protocol for which we have
        -- an on-demand-start protocol thread. So we start it now.
        EventStartOnDemand ptclState@MiniProtocolState {
                             miniProtocolInfo = MiniProtocolInfo {
                               miniProtocolNum,
                               miniProtocolDir
                             },
                             miniProtocolStatusVar
                           }
                           ptclAction -> do
          traceWith tracer (MuxTraceStartOnDemand miniProtocolNum
                             (protocolDirEnum miniProtocolDir))
          atomically $ writeTVar miniProtocolStatusVar StatusRunning
          JobPool.forkJob jobpool $
            miniProtocolJob
              tracer
              egressQueue
              ptclState
              ptclAction
          go (Map.delete (miniProtocolNum, miniProtocolDirEnum) ptclsStartOnDemand)
          where
            miniProtocolDirEnum = protocolDirEnum miniProtocolDir

    checkNonEmptyQueue :: IngressQueue m -> STM m ()
    checkNonEmptyQueue q = do
      buf <- readTVar q
      check (not (BL.null buf))

    protocolKey MiniProtocolState {
                  miniProtocolInfo = MiniProtocolInfo {
                    miniProtocolNum,
                    miniProtocolDir
                  }
                } =
      (miniProtocolNum, protocolDirEnum miniProtocolDir)

data MonitorEvent mode m =
     EventJobResult  MuxJobResult
   | EventControlCmd (ControlCmd mode m)
   | EventStartOnDemand (MiniProtocolState mode m)
                        (MiniProtocolAction m)

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
    => Tracer m MuxTrace
    -> EgressQueue m
    -> Wanton m
    -> MiniProtocolNum
    -> MiniProtocolDir
    -> IngressQueue m
    -> Channel m
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

        traceWith tracer $ MuxTraceChannelSendStart mc (fromIntegral $ BL.length encoding)

        atomically $ do
            buf <- readTVar w
            if BL.length buf < perMiniProtocolBufferSize
               then do
                   let wasEmpty = BL.null buf
                   writeTVar w (BL.append buf encoding)
                   when wasEmpty $
                     writeTBQueue egressQueue (TLSRDemand mc md want)
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
-- Incase the Mux has stopped, either due to an exception or because of a call
-- to muxStop a `Left MuxError` will be returned from the STM action.
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
                   MonadSTM m
                => Mux mode m
                -> MiniProtocolNum
                -> MiniProtocolDirection mode
                -> StartOnDemandOrEagerly
                -> (Channel m -> m (a, Maybe BL.ByteString))
                -> m (STM m (Either SomeException a))
runMiniProtocol Mux { muxMiniProtocols, muxControlCmdQueue , muxStatus}
                ptclNum ptclDir startMode protocolAction

    -- Ensure the mini-protocol is known and get the status var
  | Just ptclState@MiniProtocolState{miniProtocolStatusVar}
      <- Map.lookup (ptclNum, ptclDir') muxMiniProtocols

  = atomically $ do

      -- Make sure no thread is currently running, and update the status to
      -- indicate a thread is running (or ready to start on demand)
      status <- readTVar miniProtocolStatusVar
      unless (status == StatusIdle) $
        error $ "runMiniProtocol: protocol thread already running for "
             ++ show ptclNum ++ " " ++ show ptclDir'
      let !status' = case startMode of
                       StartOnDemand -> StatusStartOnDemand
                       StartEagerly  -> StatusRunning
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
  = error $ "runMiniProtocol: no such protocol num and mode in this mux: "
         ++ show ptclNum ++ " " ++ show ptclDir'
  where
    ptclDir' = protocolDirEnum ptclDir

    -- Wait for the miniprotocol to complete.
    -- If the mux was stopped through a call to 'stopMux' (MuxStopped)
    -- or in case of an error (MuxFailed) we return the result of
    -- the miniprotocol, or a `MuxError` if it was still running.
    completionAction completionVar = do
      st <- readTVar muxStatus
      case st of
           MuxReady    -> readTMVar completionVar
           MuxStopped  -> readTMVar completionVar
                      <|> return (Left $ toException (MuxError MuxCleanShutdown "Mux stopped"))
           MuxFailed e -> readTMVar completionVar
                      <|> return (Left $ toException $
                            case fromException e of
                              Just e'@MuxError { errorType } ->
                                e' { errorType = MuxShutdown (Just errorType) }
                              Nothing ->
                                MuxError (MuxShutdown Nothing) (show e))

