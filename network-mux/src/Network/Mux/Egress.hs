{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
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
  , Wanton (..)
  , egressSoftBufferLimit
  ) where

import Control.Monad
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer)

import Network.Mux.Counters (ByteSlots)
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

type EgressQueue m = StrictTBQueue m (TranslocationServiceRequest m)

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
    -> ByteSlots m
    -- ^ This mux's byte slots, one per mini-protocol and direction it runs.
    -> Bearer m
    -> m void
muxer egressQueue tracer byteSlots Bearer { writeMany, sduSize, batchSize, egressInterval } =
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
    mask $ \unmask ->
    forever $ do
      start <- getMonotonicTime
      TLSRDemand mpc md d <- atomically $ readTBQueue egressQueue
      sdu <- processSingleWanton egressQueue sduSize mpc md d
      sdus <- buildBatch [sdu] (sduLength sdu)
      -- A write that throws, part of the batch sent or none, is not counted:
      -- the connection is gone with it.
      unmask (void (writeMany tracer timeout sdus))
      end <- getMonotonicTime
      -- The batch summed by mini-protocol and direction first, so the
      -- transaction updates a slot once per protocol in the batch, not once
      -- per SDU; a mux without counters has no slots and sums nothing.
      let !bytes = if Map.null byteSlots then [] else batchBytes sdus
      empty <- atomically $ do
        forM_ bytes $ \(key, n) ->
          forM_ (Map.lookup key byteSlots) $ \slot -> modifyTVar slot (+ n)
        isEmptyTBQueue egressQueue
      when empty $ do
        let delta = diffTime end start
        threadDelay (egressInterval - delta)

  where
    maxSDUsPerBatch :: Int
    maxSDUsPerBatch = 100

    sduLength :: SDU -> Int
    sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)

    -- The bytes of a batch by mini-protocol and direction, header included,
    -- as a list: a batch holds few protocols, so it is short. A run of SDUs
    -- on one protocol is summed as it goes and merged once, at its end, so a
    -- batch on one protocol merges once.
    batchBytes :: [SDU] -> [((MiniProtocolNum, MiniProtocolDir), Word64)]
    batchBytes []           = []
    batchBytes (sdu : sdus) = runs [] (keyOf sdu) (bytesOf sdu) sdus
      where
        runs :: [((MiniProtocolNum, MiniProtocolDir), Word64)]
             -> (MiniProtocolNum, MiniProtocolDir) -> Word64 -> [SDU]
             -> [((MiniProtocolNum, MiniProtocolDir), Word64)]
        runs acc key !n []            = insert key n acc
        runs acc key !n (sdu' : rest)
          | key' == key = runs acc key (n + bytesOf sdu') rest
          | otherwise   = runs (insert key n acc) key' (bytesOf sdu') rest
          where
            key' = keyOf sdu'

        keyOf :: SDU -> (MiniProtocolNum, MiniProtocolDir)
        keyOf sdu' = (msNum sdu', msDir sdu')

        bytesOf :: SDU -> Word64
        bytesOf = fromIntegral . sduLength

        insert :: (MiniProtocolNum, MiniProtocolDir) -> Word64
               -> [((MiniProtocolNum, MiniProtocolDir), Word64)]
               -> [((MiniProtocolNum, MiniProtocolDir), Word64)]
        insert key n [] = [(key, n)]
        insert key n ((key', n') : rest)
          | key == key' = let !m = n' + n in (key', m) : rest
          | otherwise   = (key', n') : insert key n rest

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
        demand_m <- atomically $ tryReadTBQueue egressQueue
        case demand_m of
             Just (TLSRDemand mpc md d) -> do
               sdu <- processSingleWanton egressQueue sduSize mpc md d
               go (sdu:sdus) (sdusLength + sduLength sdu)
             Nothing -> return sdus

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
