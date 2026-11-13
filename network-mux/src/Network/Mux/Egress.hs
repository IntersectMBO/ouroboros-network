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
  , egressSoftBufferLimit
  , newWanton
  ) where

import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict qualified as Map

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer, traceWith)

import Data.Char (toLower)

import Network.Mux.Counters (Counters, countGateTimeout)
import Network.Mux.Egress.Bucket (Bucket, BucketHandle, GrantSource (..), Rank,
           awaitGrantCharged, awaitGrantWaited, takeOnCredit)
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

-- | Where the scheduled lane reports what a batch carries: bytes per
-- mini-protocol, SDU headers included, the granted rank, and when, in the
-- transaction that grants the batch, so the next grant's rank rule sees
-- it. The whole batch is reported at once, since its rank was chosen for
-- all of it. The policy that installs it decides what counts against which
-- allowance; the mux only reports. Direct and slice batches are never
-- reported, so what rides those lanes is never charged.
type ChargeSink m = Time -> Rank -> Map.Map MiniProtocolNum Int -> STM m ()

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
--  TVar becoming empty indicates that the last fragment of the data has been
--  taken by a muxer, and 'held' reaching zero that the muxer has written it.
data Wanton m = Wanton {
    want :: !(StrictTVar m BL.ByteString),
    held :: !(EgressVar m)
    -- ^ the mini-protocol's standing: batches a muxer has taken some of its
    -- bytes into and not yet written, and the bytes written for it, counted
    -- in the write that releases a batch
  }

-- | An empty 'Wanton' on the mini-protocol's @held@.
newWanton :: MonadSTM m => EgressVar m -> m (Wanton m)
newWanton held = do
    want <- newTVarIO BL.empty
    return Wanton { want, held }

-- | An entry owns one increment of 'esHeld' on 'protocolState', released
-- by 'completeBatch' after a successful write. Its byte count includes SDU
-- headers. Further SDUs of the same protocol and direction add bytes to the
-- entry without taking another hold.
data BatchProtocol m = BatchProtocol {
    protocolState :: !(EgressVar m),
    protocolBytes :: !Int
  }

data Batch m = Batch {
    -- SDUs are accumulated in reverse; buildBatch returns them in wire order.
    batchSDUs      :: [SDU],
    batchBytes     :: !Int,
    batchProtocols :: !(Map.Map (MiniProtocolNum, MiniProtocolDir) (BatchProtocol m))
  }

emptyBatch :: Batch m
emptyBatch = Batch [] 0 Map.empty

-- | Add an `SDU` to a `Batch`.
--
-- NOTE: the `key` and `len` arguments could be derived from `SDU`, but we
-- are passing them directly since they are already evaluated at the call
-- site.
addToBatch :: (MiniProtocolNum, MiniProtocolDir)
           -> SDU
           -> Int
           -- ^ SDU size including the header
           -> BatchProtocol m
           -> Batch m
           -> Batch m
addToBatch
  key sdu len
  batchProtocol
  Batch {
    batchSDUs,
    batchBytes,
    batchProtocols
  }
  =
  Batch {
    batchSDUs      = sdu : batchSDUs,
    batchBytes     = batchBytes + len,
    batchProtocols = Map.insert key batchProtocol batchProtocols
  }


-- | Build a batch of SDUs to submit in one go to the bearer. The egress
-- queue is processed one SDU at a time to ensure that we don't cause
-- starvation. The batch size is limited by the bearer (e.g. the SO_SNDBUF
-- for Socket) or the number of SDUs.
--
-- Each SDU is taken in its own transaction. Every protocol entry owns one
-- hold, acquired by 'takeIntoBatch' in the transaction that first takes
-- its bytes. Both exit paths preserve those entries for 'completeBatch'.
withBatch :: forall m a b.
             ( MonadMonotonicTime m
             , MonadSTM m
             )
          => EgressQueue m
          -> SDUSize
          -> Int
          -- ^ stop after reaching this many bytes, including SDU headers.
          -> (Time -> Batch m -> STM m b)
          -- ^ completion callback
          -> (Batch m -> m a)
          -- ^ continuation to run once a batch is formed.  That's where we
          -- scheduling a socket write happens. 
          -> m (Time, a, b)
withBatch egressQueue sduSize batchSize completeHook k = do
    batch <- buildBatch
    a <- k batch
    end <- getMonotonicTime
    atomically $ do
      completeBatch batch
      b <- completeHook end batch
      return (end, a, b)
  where
    buildBatch :: m (Batch m)
    buildBatch = do
        batch <- atomically $ do
          demand <- readTBQueue egressQueue
          takeIntoBatch emptyBatch demand
        go 1 batch
      where
        maxSDUsPerBatch :: Int
        maxSDUsPerBatch = 100

        finish :: Batch m -> Batch m
        finish b = b { batchSDUs = reverse (batchSDUs b) }

        go :: Int -> Batch m -> m (Batch m)
        go !n batch
          | n >= maxSDUsPerBatch
          = return (finish batch)

          | batchBytes batch >= batchSize
          = return (finish batch)

          | otherwise = do
              next <- atomically $ do
                demand <- tryReadTBQueue egressQueue
                traverse (takeIntoBatch batch) demand
              case next of
                 Just batch' -> go (n + 1) batch'
                 Nothing     -> return (finish batch)

        -- The caller dequeues the demand in this transaction too: it
        -- reserves room for requeueing the remainder, and prevents a drain
        -- from seeing an empty queue before the fragment is held.
        -- Recording a new protocol and incrementing its held count always
        -- happen together, including for the batch's first SDU.
        takeIntoBatch
            :: Batch m
            -> TranslocationServiceRequest m
            -> STM m (Batch m)
        takeIntoBatch batch@Batch { batchProtocols }
                      (TLSRDemand mpc md w) = do
            sdu <- takeSingleWanton egressQueue sduSize mpc md w
            let key = (mpc, md)
                !bytes = sduLength sdu
            batchProtocol <- case Map.lookup key batchProtocols of
               Nothing -> do
                 -- We are inserting a new key (miniProtocolNum,
                 -- miniProtocolDir) into the batch, thus increment the
                 -- `esHeld` in `EgressState`
                 --
                 -- NOTE: in HEAD~ this logic was bolted into
                 -- `takeSingleWanton`'s `first` argument.
                 modifyTVar (held w) $ \es -> es { esHeld = esHeld es + 1 }
                 return (BatchProtocol (held w) bytes)
               Just p -> return p { protocolBytes = protocolBytes p + bytes }
            return $ addToBatch key sdu bytes batchProtocol batch
          where
            sduLength :: SDU -> Int
            sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)

    -- | Release each hold and count its bytes in the same transaction,
    -- once the whole batch has been written.
    completeBatch :: Batch m -> STM m ()
    completeBatch Batch { batchProtocols } =
        forM_ batchProtocols $ \BatchProtocol { protocolState, protocolBytes } ->
          modifyTVar protocolState $ \EgressState { esHeld, esSent } ->
            EgressState { esHeld = esHeld - 1,
                          esSent = esSent + fromIntegral protocolBytes }

-- | A soft limit on the egress buffer ('Wanton') size, 0x3ffff bytes, one
-- short of 256 KiB. A mini-protocol's send appends to its buffer only while
-- the buffer holds fewer bytes, and forces its message's encoding up to it,
-- to a chunk boundary; the one limit bounds both by design: the check counts
-- the buffer no further, and a message within it is admitted whole and
-- forced whole. A sender blocked behind the limit holds its buffer, the
-- limit's worth and more, and the forced prefix of the message it waits
-- with, so a channel's forced footprint is up to about twice the limit.
--
-- There is no hard limit: an investigation showed that one would only limit
-- the performance of data-intensive mini-protocols, and the mux is already
-- fair when the bandwidth is limited. See issue #5271.
--
egressSoftBufferLimit :: Int64
egressSoftBufferLimit = 0x3ffff


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
    -> Maybe (Counters m)
    -- ^ the node's counters, for a gate that times out
    -> LaneEgress m
    -> Bearer m
    -> m void
muxer egressQueue tracer counters laneEgress
      Bearer { writeMany, sduSize, batchSize, egressInterval, awaitWritable } =
    withTimeoutSerial $ \timeout ->
    -- The loop runs masked, so a count means bytes handed to the kernel:
    -- nothing lands between the write's return and the count. A
    -- cancellation lands in the write, where the bearer's timeout lands
    -- too, or in the bearer's tail once the kernel has the bytes, which
    -- leaves that batch uncounted on a mux that is ending anyway, or at the
    -- loop's other blocking points, the queue read and the poll delay. The
    -- write runs unmasked to work around an io-sim bug: a timeout under a
    -- mask leaks its exception past its frame when the timed call finishes
    -- as the timer fires (IntersectMBO/io-sim#269), which GHC retracts; once
    -- that is fixed the unmask can go, the write blocking interruptibly
    -- under the mask as the queue read does. Masked once, outside the loop,
    -- a batch pays the one unmask application.
    mask $ \unmask -> batch timeout unmask 0
  where
    completionHook :: Time -> Batch m -> STM m Bool
    completionHook = \end Batch { batchBytes = len } -> do
      case laneEgress of
        Credit budget -> takeOnCredit budget end len
        _ -> return ()
      isEmptyTBQueue egressQueue

    -- one batch, then the next, carrying how long the batch took to write,
    -- for the bearer's standing in the budget's counters
    batch :: TimeoutFn m -> (forall a. m a -> m a) -> DiffTime -> m void
    batch timeout unmask !wrote = do
      start <- getMonotonicTime
      (end, t2, empty) <-
        withBatch egressQueue sduSize batchSize completionHook $
          \Batch { batchSDUs = sdus, batchBytes = len, batchProtocols } -> do

          -- Scheduled egress: a batch is written only once the bearer can take it
          -- without blocking AND its lane has the bytes -- granted by the budget's
          -- bucket or the slice's, or, for the node's own requests, taken on
          -- credit once written. A bearer whose peer is not draining never
          -- consumes tokens. Each lane ends at the instant the write may start.
          t2 <- case laneEgress of
             Unscheduled -> getMonotonicTime

             Credit _    -> gate timeout
                         >> getMonotonicTime

             Reserved slice budget -> do
               t0 <- getMonotonicTime
               gate timeout
               t1 <- getMonotonicTime
               -- the slice's own share, charged to the budget on credit in the
               -- same transaction, or the budget's idle capacity
               void $ awaitGrantWaited (Just budget) (t1 `diffTime` t0) wrote slice len
               getMonotonicTime

             Scheduled_ bucketHandle chargeVar -> do
               let chargeNow :: Time -> Rank -> STM m ()
                   chargeNow t rank = do
                     sink <- readTVar chargeVar
                     sink t rank (Map.mapKeysWith (+) fst
                                   (Map.map protocolBytes batchProtocols))
               t0 <- getMonotonicTime
               gate timeout
               t1 <- getMonotonicTime
               -- charged in the grant's transaction, so connections sharing
               -- an allowance see each other's spending at once
               source <- awaitGrantCharged Nothing (t1 `diffTime` t0) wrote chargeNow bucketHandle len
               t2 <- getMonotonicTime
               traceWith tracer (TraceEgressGrant len (t1 `diffTime` t0) (t2 `diffTime` t1)
                                                  (source == FromFloor))
               return t2
          -- A write that throws, part of the batch sent or none, is neither
          -- counted nor charged: the connection is gone with it.
          unmask (void (writeMany tracer timeout sdus))
          return t2

      when empty $ do
        let delta = diffTime end start
        threadDelay (egressInterval - delta)
      batch timeout unmask (end `diffTime` t2)

    -- the writability gate; a timeout there is counted before it kills the mux
    gate :: TimeoutFn m -> m ()
    gate timeout =
      awaitWritable tracer timeout `catch` \e -> do
        case fromException e of
             Just SDUWriteTimeout -> atomically (countGateTimeout counters)
             _                    -> return ()
        throwIO (e :: SomeException)
-- | Pull a `maxSDU`s worth of data out of the `Wanton`. If data remains,
-- requeue the `TranslocationServiceRequest` so other queued demands get
-- service first.
takeSingleWanton :: MonadSTM m
                 => EgressQueue m
                 -> SDUSize
                 -> MiniProtocolNum
                 -> MiniProtocolDir
                 -> Wanton m
                 -> STM m SDU
takeSingleWanton egressQueue (SDUSize sduSize)
                 mpc md wanton = do
    blob <- do
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
