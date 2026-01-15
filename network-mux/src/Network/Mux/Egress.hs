{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Egress
  ( muxer
    -- $egress
    -- $servicingsSemantics
  , EgressQueue
  , TranslocationServiceRequest (..)
  , Wanton (..)
  ) where

import Data.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad
import Data.ByteString.Lazy qualified as BL

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Tracer (Tracer)

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
     TLSRDemand !MiniProtocolNum !MiniProtocolDir !(Wanton m) !ProtocolBurst

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
    -> Bearer m
    -> m void
muxer egressQueue tracer Bearer { writeMany, sduSize, batchSize, egressInterval } =
    withTimeoutSerial $ \timeout ->
    forever $ do
      start <- getMonotonicTime
      (sdu, mBurst) <- atomically $ do
        demand@(TLSRDemand mpc md d (ProtocolBurst burst)) <- readTBQueue egressQueue
        eSdu <- processSingleWanton sduSize mpc md d
        case eSdu of
          Right sdu | burst > 1 -> do
                          (sdu, Just $ burst - 1) <$ unGetTBQueue egressQueue demand
                    | otherwise -> (sdu, Nothing) <$ writeTBQueue egressQueue demand
          Left sdu -> pure (sdu, Nothing)

      sdus <- buildBatch [sdu] (sduLength sdu) (fromIntegral <$> mBurst)
      void $ writeMany tracer timeout sdus
      end <- getMonotonicTime
      empty <- atomically $ isEmptyTBQueue egressQueue
      when empty $ do
        let delta = diffTime end start
        threadDelay (egressInterval - delta)

  where
    maxSDUsPerBatch :: Int
    maxSDUsPerBatch = 100

    sduLength :: SDU -> Int
    sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)

    -- Build a batch of SDUs to submit in one go to the bearer.
    -- Streams which are permitted to burst will have that many
    -- sdu's serviced back-to-back before the scheduler moves to process the
    -- next request on the queue. Any remaining sdu's which did not
    -- fit in the burst allowance are placed on the back of the queue
    -- to ensure that we don't cause starvation. In particular, a burst
    -- of 1 will have the muxer process one sdu at a time from the queue,
    -- and any remaining work is put on the back of the queue.
    -- The batch size is either limited by the bearer
    -- (e.g the SO_SNDBUF for Socket) or number of SDUs.
    --
    buildBatch s sl mBurst0 = reverse <$> go 1 s sl mBurst0
     where
      go !count sdus _ _ | count >= maxSDUsPerBatch      = return sdus
      go _ sdus sdusLength _  | sdusLength >= batchSize  = return sdus
      go count  sdus !sdusLength mBurst = do
        mResult <- atomically $ do
          demand_m <- tryReadTBQueue egressQueue
          case demand_m of
            Nothing -> return Nothing
            Just demand@(TLSRDemand mpc md d (ProtocolBurst burst)) -> do
              let burst' = fromMaybe (fromIntegral burst) mBurst
                  step (!count', !sdusLength', !sdus') mx = do
                    x <- lift mx
                    case x of
                      Left sdu -> do
                        let sdusLength'' = sdusLength' + sduLength sdu
                        throwE (succ count', sdusLength'', sdu:sdus')
                      Right sdu -> do
                        let sdusLength'' = sdusLength' + sduLength sdu
                        pure (succ count', sdusLength'', sdu:sdus')
              fmap Just . either pure (<$ writeTBQueue egressQueue demand) =<<
                runExceptT (foldM step (count, sdusLength, sdus)
                                       (replicate burst'
                                                  (processSingleWanton sduSize mpc md d)))

        case mResult of
          Just (count', sdusLength', sdus') -> go count' sdus' sdusLength' Nothing
          Nothing -> return sdus


-- | Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: (MonadSTM m)
                    => SDUSize
                    -> MiniProtocolNum
                    -> MiniProtocolDir
                    -> Wanton m
                       -- Right: more sdu's remain; Left: finished
                    -> STM m (Either SDU SDU)
processSingleWanton (SDUSize sduSize)
                    mpc md wanton = do
    (blob, more) <- do
      -- extract next SDU
      d <- readTVar (want wanton)
      let (frag, rest) = BL.splitAt (fromIntegral sduSize) d
      -- if more to process then enqueue remaining work
      if BL.null rest
        then (frag, False) <$ writeTVar (want wanton) BL.empty
        else do
          -- Note that to preserve bytestream ordering within a given
          -- miniprotocol the readTVar and writeTVar operations
          -- must be inside the same STM transaction.
          (frag, True) <$ writeTVar (want wanton) rest
          -- writeTBQueue egressQueue (TLSRDemand mpc md wanton)
    let sdu = SDU {
                msHeader = SDUHeader {
                    mhTimestamp = (RemoteClockModel 0),
                    mhNum       = mpc,
                    mhDir       = md,
                    mhLength    = fromIntegral $ BL.length blob
                  },
                msBlob = blob
              }
    pure $ if more then Right sdu else Left sdu
    --paceTransmission tNow
