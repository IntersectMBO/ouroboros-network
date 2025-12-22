{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Egress
  ( muxer
    -- $egress
    -- $servicingsSemantics
  , EgressQueue
  , TranslocationServiceRequest (..)
  , Wanton
  , newWanton
  , modifyWanton
  ) where

import Control.Monad
import Data.Functor.Contravariant ((>$<))
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)

import Control.Concurrent.Class.MonadChan as Chan
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadMVar.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer, traceWith)

import Network.Mux.Timeout
import Network.Mux.Types

-- $servicingsSemantics
-- = Desired Servicing Semantics
--
--  == /Constructing Fairness/
--
--   In this context we are defining fairness as:
--    - no starvation
--    - when presented with equal demand (from a selection of mini
--      protocols) deliver "equal" service.
--
--   Equality here might be in terms of equal service rate of
--   requests (or segmented requests) and/or in terms of effective
--   (SDU) data rates.
--
--
--  Notes:
--
--   1) It is assumed that (for a given peer) that bulk delivery of
--      blocks (i.e. in recovery mode) and normal, interactive,
--      operation (e.g. chain following) are mutually exclusive. As
--      such there is no requirement to create a notion of
--      prioritisation between such traffic.
--
--   2) We are assuming that the underlying TCP/IP bearer is managed
--      so that individual Mux-layer PDUs are paced. a) this is necessary
--      to mitigate head-of-line blocking effects (i.e. arbitrary
--      amounts of data accruing in the O/S kernel); b) ensuring that
--      any host egress data rate limits can be respected / enforced.
--
--  == /Current Caveats/
--
--  1) Not considering how mini-protocol associations are constructed
--     (depending on deployment model this might be resolved within
--     the instantiation of the peer relationship)
--
--  2) Not yet considered notion of orderly termination - this not
--     likely to be used in an operational context, but may be needed
--     for test harness use.
--
--  == /Principle of Operation/
--
--
--  Egress direction (mini protocol instance to remote peer)
--
--  The request for service (the demand) from a mini protocol is
--  encapsulated in a `Wanton`, such `Wanton`s are placed in a (finite)
--  queue (e.g TBMQ) of `TranslocationServiceRequest`s.
--

-- $egress
-- = Egress Path
--
-- > ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐ Every mode per miniprotocol
-- > │ muxDuplex │ │ muxDuplex │ │ muxDuplex │ │ muxDuplex │ has a dedicated thread which
-- > │ Initiator │ │ Responder │ │ Initiator │ │ Responder │ will send ByteStrings of CBOR
-- > │ ChainSync │ │ ChainSync │ │ BlockFetch│ │ BlockFetch│ encoded data.
-- > └─────┬─────┘ └─────┬─────┘ └─────┬─────┘ └─────┬─────┘
-- >       │             │             │             │
-- >       │             │             │             │
-- >       ╰─────────────┴──────┬──────┴─────────────╯
-- >                            │
-- >                     application data
-- >                            │
-- >                         ░░░▼░░
-- >                         ░│  │░ For a given Mux Bearer there is a single egress
-- >                         ░│ci│░ queue shared among all miniprotocols. To ensure
-- >                         ░│cr│░ fairness each miniprotocol can at most have one
-- >                         ░└──┘░ message in the queue, see Desired Servicing
-- >                         ░░░│░░ Semantics.
-- >                           ░│░
-- >                       ░░░░░▼░░░
-- >                       ░┌─────┐░ The egress queue is served by a dedicated thread
-- >                       ░│ mux │░ which chops up the CBOR data into MuxSDUs with at
-- >                       ░└─────┘░ most sduSize bytes of data in them.
-- >                       ░░░░│░░░░
-- >                          ░│░ MuxSDUs
-- >                          ░│░
-- >                  ░░░░░░░░░▼░░░░░░░░░░
-- >                  ░┌────────────────┐░
-- >                  ░│ Bearer.write() │░ Mux Bearer implementation specific write
-- >                  ░└────────────────┘░
-- >                  ░░░░░░░░░│░░░░░░░░░░
-- >                           │ ByteStrings
-- >                           ▼
-- >                           ●

-- TODO: use a bounded version `Chan`
type EgressQueue m = Chan m (TranslocationServiceRequest m)

-- | A TranslocationServiceRequest is a demand for the translocation
--  of a single mini-protocol message. This message can be of
--  arbitrary (yet bounded) size. This multiplexing layer is
--  responsible for the segmentation of concrete representation into
--  appropriate SDU's for onward transmission.
data TranslocationServiceRequest m =
     TLSRDemand !MiniProtocolNum !MiniProtocolDir !(Wanton m)

-- | A Wanton represent the concrete data to be translocated, note that the
--  TVar becoming empty indicates -- that the last fragment of the data has
--  been enqueued on the -- underlying bearer.
--
-- INVARIANT: `lock` is non-empty iff `want` has at most `maxSize` bytes (e.g.
-- `perMiniProtocolBufferSize`).
--
data Wanton m = Wanton {
    want    :: StrictMVar m BL.ByteString,
    lock    :: StrictMVar m (),
    -- ^ lock access to `want` if its size is greater than `maxSize`.
    maxSize :: Int64
    -- ^ minimal buffer size
 }

newWanton :: MonadMVar m => Int64 -> m (Wanton m)
newWanton maxSize = do
  want <- newMVar BL.empty
  lock <- newMVar ()
  return Wanton { want, lock, maxSize }

-- | Append a blob to a `Wanton`, making sure both `Wanton` invariants are
-- preserved even if asynchronous exceptions are present.
--
-- INVARIANT: all `appendWanton` operations must be synchronous (e.g. executed
-- in the same thread)
--
modifyWanton
   :: forall m.
      ( MonadMVar  m
      , MonadMask  m
      )
   => Wanton m
   -> (BL.ByteString -> m BL.ByteString)
   -> m ()
modifyWanton Wanton { want, lock, maxSize } k =
   bracketOnError
     (takeMVar lock)
     (\_ -> -- the error handling can only happen when the `modifyMVar` was
            -- interrupted, thus `want` wasn't updated, and we can insert the
            -- original value.
            void $ tryPutMVar lock ())
     (\_ ->
       -- Now we know the size is at most `minSize`
       modifyMVarMasked_ want $ \bs -> do
         -- We took `lock` and `want` `MVar`, thus we have exclusive access to
         -- `Wanton`.  It is not possible that `takeWanton` will update `lock`
         -- while it's our responsibility to do so.
         bs' <- k bs
         let sz' = BL.length bs'
         -- `tryPutMVar` and `tryTakeMVar` are non-blocking and thus safe in
         -- presence of asynchronous exceptions (the whole update callback is
         -- executed with asynchronous exceptions masked).
         if sz' <= maxSize 
           then void $ tryPutMVar lock ()
           else void $ tryTakeMVar lock
         return bs'
     )


-- | Run a monadic action on a `Wanton`, making sure its invariants are
-- satisfied even when there are asynchronous exceptions.
--
-- INVARIANT: all `takeWanton` operations must be synchronous (e.g. executed in
-- the same thread)
-- INVARIANT: `k` does not make the blob larger
--
takeWanton
  :: MonadMVar m
  => Tracer m BearerTrace
  -> Wanton m
  -> (BL.ByteString -> m (BL.ByteString, a))
  -> m a
takeWanton tracer Wanton { want, lock, maxSize } k =
  modifyMVarMasked want $ \bs -> do
    traceWith tracer $ TraceDebug "takeWanton started"
    (bs', a) <- k bs
    let sz' = BL.length bs'
    -- `tryPutMVar` and `tryTakeMVar` are always non-blocking.  If `k` returned
    -- we are guaranteed that `lock` will be updated with the correct value,
    -- making sure `Wanton` invariant is fulfilled.
    if sz' <= maxSize
      then void $ tryPutMVar lock ()
      else void $ tryTakeMVar lock
    traceWith tracer $ TraceDebug "takeWanton finished"
    assert (sz' <= BL.length bs) $
      return (bs', a)


-- | Process the messages from the mini protocols - there is a single
-- shared FIFO that contains the items of work. This is processed so
-- that each active demand gets a `maxSDU`s work of data processed
-- each time it gets to the front of the queue
muxer
    :: forall m void.
       ( MonadAsync m
       , MonadChan m
       , MonadDelay m
       , MonadFork m
       , MonadMask m
       , MonadThrow (STM m)
       , MonadTimer m
       )
    => EgressQueue m
    -> Tracer m BearerTrace
    -> Bearer m
    -> m void
muxer egressQueue tracer Bearer { writeMany, sduSize, batchSize, egressInterval } =
    withTimeoutSerial $ \timeout ->
    forever $ do
      traceWith tracer $ TraceDebug "egress loop: waiting for TLSR"
      start <- getMonotonicTime
      TLSRDemand mpc md d <- Chan.readChan egressQueue
      traceWith tracer $ TraceDebug $ "egress loop: TLSR " ++ show (mpc, md)
      sdu <- processSingleWanton tracer egressQueue sduSize mpc md d
      sdus <- buildBatch [sdu] (sduLength sdu)
      traceWith tracer $ TraceDebug $ "SDUs to write " ++ show (mpc, md, length sdus)
      void $ writeMany tracer timeout sdus
      end <- getMonotonicTime
      empty <- Chan.isEmpty egressQueue
      when empty $ do
        let delta = end `diffTime` start
        threadDelay (egressInterval - delta)

  where
    maxSDUsPerBatch :: Int
    maxSDUsPerBatch = 100

    sduLength :: SDU -> Int
    sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)

    -- Build a batch of SDUs to submit in one go to the bearer.
    -- The egress queue is still processed one SDU at the time
    -- to ensure that we don't cause starvation.
    -- The batch size is either limited by the bearer
    -- (e.g the SO_SNDBUF for Socket) or number of SDUs.
    --
    buildBatch s sl = reverse <$> go s sl
     where
      go sdus _ | length sdus >= maxSDUsPerBatch   = return sdus
      go sdus sdusLength | sdusLength >= batchSize = return sdus
      go sdus !sdusLength = do
        demand_m <- tryReadChan egressQueue
        case demand_m of
             Just (TLSRDemand mpc md d) -> do
               -- traceWith tracer $ TraceDebug $ "egress loop: batch TLSR " ++ show (mpc, md)
               sdu <- processSingleWanton tracer egressQueue sduSize mpc md d
               go (sdu:sdus) (sdusLength + sduLength sdu)
             Nothing -> return sdus


-- | Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: MonadChan m
                    => Tracer m BearerTrace
                    -> EgressQueue m
                    -> SDUSize
                    -> MiniProtocolNum
                    -> MiniProtocolDir
                    -> Wanton m
                    -> m SDU
processSingleWanton tracer egressQueue (SDUSize sduSize)
                    mpc md wanton = do
    -- extract next SDU
    blob <- takeWanton ((\case 
                          TraceDebug msg -> TraceDebug $ msg ++ " " ++ show (mpc, md)
                          a -> a)
                        >$< tracer
                       ) wanton $ \bs -> do
      let (frag, rest) = BL.splitAt (fromIntegral sduSize) bs
      traceWith tracer $ TraceDebug $ "takeWanton started for " ++ show (mpc, md, BL.null rest)
      -- if more to process then enqueue remaining work
      if BL.null rest
        then do
          traceWith tracer $ TraceDebug $ "takeWanton TLSR not needed " ++ show (mpc, md)
          return (BL.empty, frag)
        else do
          -- NOTE: to preserve bytestream ordering within a given mini-protocol
          -- the wanton must be unlocked after we write to the `egressQueue`.
          -- This is true since we're inside `takeWanton` and thus we have
          -- exclusive access to it.
          --
          -- NOTE: if there are multiple mini-protocols, `Chan.writeChan` might
          -- block.  Using `Chan` (based on `MVar`s) should make sure fair
          -- access to `EgressQueue` for all mini-protocols.
          traceWith tracer $ TraceDebug $ "takeWanton TLSR enqueing... " ++ show (mpc, md)
          Chan.writeChan egressQueue (TLSRDemand mpc md wanton)
          traceWith tracer $ TraceDebug $ "takeWanton TLSR enqueued " ++ show (mpc, md)
          return (rest, frag)
    let sdu = SDU {
                msHeader = SDUHeader {
                    mhTimestamp = RemoteClockModel 0,
                    mhNum       = mpc,
                    mhDir       = md,
                    mhLength    = fromIntegral $ BL.length blob
                  },
                msBlob = blob
              }
    return sdu
    --paceTransmission tNow
