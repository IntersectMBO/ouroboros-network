{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Egress
  ( muxer
    -- $egress
    -- $servicingsSemantics
  , EgressQueue
  , TranslocationServiceRequest (..)
  , Wanton (..)
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Bool
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word32)

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
data Wanton m = Wanton {
  want      :: !(StrictTVar m BL.ByteString),
  wLastSent :: !(StrictTVar m Time),
  wBucket   :: !(StrictTVar m Word32)
  }


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
      (sdu, mBurst) <- atomically do
        demand@(TLSRDemand mpc md d (ProtocolBurst pbMaxBytes _pbRefillRate)) <- readTBQueue egressQueue
        eSdu <- processSingleWanton sduSize mpc md d
        case eSdu of
          Right sdu | pbMaxBytes > 0 -> do
                          -- we do not check if the protocol has any tokens to burst,
                          -- that is deferred to buildBatch below.
                          (sdu, True) <$ unGetTBQueue egressQueue demand
                    | otherwise -> (sdu, False) <$ writeTBQueue egressQueue demand
          Left sdu -> pure (sdu, False)

      sdus <- buildBatch [sdu] (sduLength sdu) mBurst start
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
    buildBatch s sl mBurst0 start = reverse <$> go 1 s sl mBurst0
     where
      toDouble :: DiffTime -> Double
      toDouble = realToFrac

      go !count sdus _ _ | count >= maxSDUsPerBatch      = return sdus
      go _ sdus sdusLength _  | sdusLength >= batchSize  = return sdus
      go count  sdus !sdusLength mBurst = do
        mResult <- atomically $ tryReadTBQueue egressQueue
        case mResult of
          Nothing -> return sdus
          Just demand@(TLSRDemand mpc md d@Wanton { wLastSent, wBucket } (ProtocolBurst pbMaxBytes pbRefillRate)) -> do
            (count', sdusLength', sdus') <- atomically do
              delta <- (start `diffTime`) <$> stateTVar wLastSent (, start)
              isEmpty <- isEmptyTBQueue egressQueue
              sduSize0 <- stateTVar wBucket \tokens ->
                let tokens' = truncate $ min (fromIntegral pbMaxBytes)
                                             (fromIntegral tokens + fromIntegral pbRefillRate * toDouble delta)
                    -- we leverage burst and deduct credits only where there is contention
                    -- between protocols
                    sduSize0 = bool (Left sduSize) (Right $ min sduSize (fromIntegral tokens')) (mBurst && not isEmpty)
                in (sduSize0, tokens')
              let step (!count', !sdusLength', !sdus', !eSize) mx = do
                    -- the first one is always free
                    -- For Left's, we don't count the wanton bytes against the burst allowance
                    -- to permit a full sdu in the first iteration
                    let (size, consumedTokens) = either (, const 0) (, id) eSize
                    x <- lift $ mx size
                    case x of
                      Left sdu -> do
                        lift $ modifyTVar wBucket \tokens ->
                                 let tokens' = tokens - consumedTokens (fromIntegral (msLength sdu))
                                 in assert (tokens >= consumedTokens (fromIntegral $ msLength sdu))
                                    tokens'
                        let sdusLength'' = sdusLength' + sduLength sdu
                        throwE (succ count', sdusLength'', sdu:sdus')
                      Right sdu -> do
                        nextSdu <- lift $ stateTVar wBucket \tokens ->
                                       let tokens' = tokens - consumedTokens (fromIntegral (msLength sdu))
                                           nextSdu = min sduSize (fromIntegral tokens')
                                       in assert (tokens >= consumedTokens (fromIntegral $ msLength sdu))
                                          (nextSdu, tokens')
                        let sdusLength'' = sdusLength' + sduLength sdu
                            count'' = succ count'
                        if | nextSdu <= 400 -> do -- 8 bytes header / 2% burst efficiency
                               -- there is more payload, but burst allowance has been exhausted
                               lift $ writeTBQueue egressQueue demand
                               throwE (count'', sdusLength'', sdu:sdus')
                           | sdusLength'' >= batchSize || count'' >= maxSDUsPerBatch -> do
                               lift $ unGetTBQueue egressQueue demand
                               throwE (count'', sdusLength'', sdu:sdus')
                           | otherwise -> pure (count'', sdusLength'', sdu:sdus', Right nextSdu)
              either pure (\(a, b, c, _d) -> (a, b, c) <$ writeTBQueue egressQueue demand)
                =<< runExceptT do
                      when (either id id sduSize0 <= 400) do
                        -- edge case where the protocol is bursty, but there aren't enough tokens
                        -- available. The muxer forever loop does not check this
                        -- when it calls to build a batch, so we handle it here.
                        lift $ writeTBQueue egressQueue demand
                        throwE (count, sdusLength, sdus)
                      foldM step (count, sdusLength, sdus, sduSize0)
                                 (if isEmpty
                                    then [const $ processSingleWanton sduSize mpc md d]
                                         -- ^ grab full sdu, save the tokens for cases with contention
                                    else repeat (\sduSize' -> processSingleWanton sduSize' mpc md d))
            go count' sdus' sdusLength' False


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
processSingleWanton sduSize
                    mpc md wanton = do
    (blob, wrap) <- do
      -- extract next SDU
      d <- readTVar (want wanton)
      let (frag, rest) = BL.splitAt (fromIntegral sduSize) d
      -- if more to process then enqueue remaining work
      if BL.null rest
        then (frag, Left) <$ writeTVar (want wanton) BL.empty
        else do
          -- Note that to preserve bytestream ordering within a given
          -- miniprotocol the readTVar and writeTVar operations
          -- must be inside the same STM transaction.
          (frag, Right) <$ writeTVar (want wanton) rest
    let sdu = SDU {
                msHeader = SDUHeader {
                    mhTimestamp = RemoteClockModel 0,
                    mhNum       = mpc,
                    mhDir       = md,
                    mhLength    = fromIntegral $ BL.length blob
                  },
                msBlob = blob
              }
    pure $ wrap sdu
    --paceTransmission tNow
