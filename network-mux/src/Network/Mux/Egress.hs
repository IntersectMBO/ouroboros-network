{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Egress
  ( muxer
  , Lane (..)
  , laneName
  , LaneEgress (..)
  , ChargeSink
  , Lanes (..)
    -- $egress
    -- $servicingsSemantics
  , EgressQueue
  , TranslocationServiceRequest (..)
  , Wanton (..)
  ) where

import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer, traceWith)

import Data.Char (toLower)

import Network.Mux.Counters (ByteCounts, MuxCounters, countGateTimeout)
import Network.Mux.Egress.Bucket (Bucket, BucketHandle, GrantSource (..),
           awaitGrantWaited, takeOnCredit)
import Network.Mux.Timeout
import Network.Mux.Trace (Error (SDUWriteTimeout))
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

type EgressQueue m = StrictTBQueue m (TranslocationServiceRequest m)

-- | Which egress lane an SDU travels in. 'Direct' is the node's own requests:
-- charged to the budget on credit, never waiting. 'Slice' is a reserved share
-- with its own bucket, also charged to the budget. 'Scheduled' is what the
-- node serves, ranked in the budget's bucket.
data Lane = Direct | Slice | Scheduled
  deriving (Eq, Ord, Show, Enum, Bounded)

laneName :: Lane -> String
laneName = map toLower . show

-- | What a lane's muxer does before writing a batch.
data LaneEgress m =
    Unscheduled
  | Credit     !(Bucket m)                      -- ^ 'Direct': the budget, on credit
  | Reserved   !(BucketHandle m) !(Bucket m)    -- ^ 'Slice': its bucket or the budget's idle
                                                --   capacity; the former charged on credit
  | Scheduled_ !(BucketHandle m)                -- ^ 'Scheduled': the budget, ranked; what
               !(StrictTVar m (ChargeSink m))   --   a batch carried is handed to the sink

-- | Where the scheduled lane reports what it wrote: bytes per mini-protocol,
-- SDU headers included, once the batch has been handed to the kernel, and
-- when. The policy that installs it decides what counts against which
-- allowance; the mux only reports. Direct and slice batches are never
-- reported, so what rides those lanes is never charged.
type ChargeSink m = Time -> MiniProtocolNum -> Int -> STM m ()

-- | A bearer's egress queues, one per lane in use, and the lane of each
-- mini-protocol. An unscheduled mux has one queue and every SDU goes to it.
data Lanes m = Lanes {
    laneQueue :: Lane -> EgressQueue m,
    laneOf    :: MiniProtocolNum -> MiniProtocolDir -> Lane,
    laneAll   :: [(Lane, EgressQueue m)]
  }

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
newtype Wanton m = Wanton { want :: StrictTVar m BL.ByteString }


-- | Process the messages from the mini protocols - there is a single
-- shared FIFO that contains the items of work. This is processed so
-- that each active demand gets a `maxSDU`s work of data processed
-- each time it gets to the front of the queue
muxer
    :: forall m void.
       ( MonadAsync m
       , MonadDelay m
       , MonadFork m
       , MonadMask m
       , MonadThrow (STM m)
       , MonadTimer m
       )
    => EgressQueue m
    -> Tracer m BearerTrace
    -> Maybe (MuxCounters m)
    -> Maybe (StrictTVar m ByteCounts)
    -- ^ this mux's sent bytes, when it counts them
    -> LaneEgress m
    -> Bearer m
    -> m void
muxer egressQueue tracer counters sentCell laneEgress
      Bearer { writeMany, sduSize, batchSize, egressInterval, awaitWritable } =
    withTimeoutSerial $ \timeout ->
    forever $ do
      start <- getMonotonicTime
      TLSRDemand mpc md d <- atomically $ readTBQueue egressQueue
      sdu <- processSingleWanton egressQueue sduSize mpc md d
      (sdus, len, perProtocol) <- buildBatch sdu mpc md
      let count = forM_ sentCell $ \cell ->
                    modifyTVar cell (Map.unionWith (+) (Map.map fromIntegral perProtocol))

      -- Scheduled egress: a batch is written only once the bearer can take it
      -- without blocking AND its lane has the bytes -- granted by the budget's
      -- bucket or the slice's, or, for the node's own requests, taken on
      -- credit once written. A bearer whose peer is not draining never
      -- consumes tokens.
      case laneEgress of
           Unscheduled -> return ()
           Credit _ -> gate timeout
           Reserved slice budget -> do
             t0 <- getMonotonicTime
             gate timeout
             t1 <- getMonotonicTime
             -- the slice's own share, charged to the budget on credit in the
             -- same transaction, or the budget's idle capacity
             void $ awaitGrantWaited (Just budget) (t1 `diffTime` t0) slice len
           Scheduled_ bucketHandle _ -> do
             t0 <- getMonotonicTime
             gate timeout
             t1 <- getMonotonicTime
             source <- awaitGrantWaited Nothing (t1 `diffTime` t0) bucketHandle len
             t2 <- getMonotonicTime
             traceWith tracer (TraceEgressGrant len (t1 `diffTime` t0) (t2 `diffTime` t1)
                                                (source == FromFloor))
      void $ writeMany tracer timeout sdus
      end <- getMonotonicTime
      -- after the write, so a charge or a count means bytes handed to the kernel
      case (laneEgress, sentCell) of
           (Scheduled_ _ chargeVar, _) -> atomically $ do
             count
             sink <- readTVar chargeVar
             forM_ (Map.toList (Map.mapKeysWith (+) fst perProtocol)) (uncurry (sink end))
           (Credit budget, _) -> atomically $ do
             count
             takeOnCredit budget end len
           (_, Just _) -> atomically count
           _ -> return ()
      empty <- atomically $ isEmptyTBQueue egressQueue
      when empty $ do
        let delta = diffTime end start
        threadDelay (egressInterval - delta)

  where
    -- the writability gate; a timeout there is counted before it kills the mux
    gate :: TimeoutFn m -> m ()
    gate timeout =
      awaitWritable tracer timeout `catch` \e -> do
        case (fromException e, counters) of
             (Just SDUWriteTimeout, Just c) -> atomically (countGateTimeout c)
             _                              -> return ()
        throwIO (e :: SomeException)
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
    -- Returns the SDUs in order, their length with headers, and that length
    -- per mini-protocol and direction, all accumulated as the batch is built
    -- so nothing walks it again.
    buildBatch :: SDU -> MiniProtocolNum -> MiniProtocolDir
               -> m ([SDU], Int, Map.Map (MiniProtocolNum, MiniProtocolDir) Int)
    buildBatch sdu0 mpc0 md0 = go [sdu0] 1 len0 (Map.singleton (mpc0, md0) len0)
     where
      len0 = sduLength sdu0
      go sdus !n !len per
        | n >= maxSDUsPerBatch || len >= batchSize = return (reverse sdus, len, per)
        | otherwise = do
            demand_m <- atomically $ tryReadTBQueue egressQueue
            case demand_m of
                 Just (TLSRDemand mpc md d) -> do
                   sdu <- processSingleWanton egressQueue sduSize mpc md d
                   let !l = sduLength sdu
                   go (sdu:sdus) (n + 1) (len + l) (Map.insertWith (+) (mpc, md) l per)
                 Nothing -> return (reverse sdus, len, per)

-- | Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: MonadSTM m
                    => EgressQueue m
                    -> SDUSize
                    -> MiniProtocolNum
                    -> MiniProtocolDir
                    -> Wanton m
                    -> m SDU
processSingleWanton egressQueue (SDUSize sduSize)
                    mpc md wanton = do
    blob <- atomically $ do
      -- extract next SDU
      d <- readTVar (want wanton)
      let (frag, rest) = BL.splitAt (fromIntegral sduSize) d
      -- if more to process then enqueue remaining work
      if BL.null rest
        then writeTVar (want wanton) BL.empty
        else do
          -- Note that to preserve bytestream ordering within a given
          -- miniprotocol the readTVar and writeTVar operations
          -- must be inside the same STM transaction.
          writeTVar (want wanton) rest
          writeTBQueue egressQueue (TLSRDemand mpc md wanton)
      -- return data to send
      pure frag
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
