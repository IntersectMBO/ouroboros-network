{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Mux.Egress
  ( muxer
    -- $egress
    -- $servicingsSemantics
  , EgressQueue
  , TranslocationServiceRequest (..)
  , Wanton (..)
  ) where

import Control.Applicative
import Control.Exception
import Data.ByteString.Lazy qualified as BL
import Data.List (tails)
import Data.Monoid (All (..), Ap (..))
import Data.Monoid.Synchronisation
import Data.Word (Word32, Word8)

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
data TranslocationServiceRequest m = TLSRDemand {
    miniProtocolNum :: !MiniProtocolNum,
    miniProtocolDir :: !MiniProtocolDir,
    wanton          :: !(Wanton m),
    protocolBurst   :: !ProtocolBurst
  }

-- | A Wanton represent the concrete data to be translocated, note that the
--  TVar becoming empty indicates -- that the last fragment of the data has
--  been enqueued on the -- underlying bearer.
data Wanton m = Wanton {
    wanton      :: !(StrictTVar m BL.ByteString),
    -- ^ data buffer
    lastSent    :: !(StrictTVar m Time),
    -- ^ the last time the protocol has sent a message
    burstBucket :: !(StrictTVar m TokenSize)
    -- ^ the number of tokens available to burst
  }


-- | A sequence of SDU's collected by the muxer to be sent to the kernel
-- in one syscall.
--
data SDUBatch = SDUBatch {
  getCount      :: !Int,
  -- ^ how many SDU's in the batch
  getSdusLength :: !Int,
  -- ^ The aggregate length of all sdu's (incl. header)
  getSdus       :: ![SDU]
  -- ^ the payload itself
  }

instance Semigroup SDUBatch where
  (SDUBatch a b c) <> (SDUBatch a' b' c') = SDUBatch (a + a') (b + b') (c <> c')

mkSingletonBatch :: SDU -> SDUBatch
mkSingletonBatch sdu = SDUBatch 1 (sduLength sdu) [sdu]


sduLength :: SDU -> Int
sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)


-- | By forming an `SDU` we also return state of the `Wanton`.
--
data SDUWithWantonState = EmptyWanton SDU | NonEmptyWanton SDU

-- | Next `SDUSize` when building a batch of `SDU`s
--
data NextSDUSize
  = BurstSize SDUSize
  -- ^ Use ` `computeSDUSize` to compute the allowed `SDUSize`
  | BearerSize
  -- ^ Use `Bearer`'s `sduSize`

computeSDUSize
  :: SDUSize   -- ^ Bearer SDUSize
  -> TokenSize -- ^ token utilised when building a batch of `SDU`s.
  -> SDUSize   -- ^ the effective `SDUSize`
computeSDUSize sduSize =
    min sduSize
  . fromIntegral @TokenSize @SDUSize                -- Word32 -> Word16
  . min (fromIntegral @SDUSize @TokenSize maxBound) -- Word16 -> Word32 (but at most Word16)

nextSDUSizeToSDUSize :: SDUSize -> NextSDUSize -> SDUSize
nextSDUSizeToSDUSize _sduSize (BurstSize sduSize) = sduSize
nextSDUSizeToSDUSize sduSize BearerSize           = sduSize
{-# INLINE nextSDUSizeToSDUSize #-}

type TokenSize = Word32

-- | Tokens consumed by an SDU.
--
consumedTokens :: NextSDUSize -> SDU -> TokenSize
-- in burst mode, we charge tokens based on the SDU payload length
consumedTokens BurstSize{} sdu = fromIntegral (msLength sdu)
-- in non-burst mode, SDU token size is 0
consumedTokens BearerSize _sdu = 0
{-# INLINE consumedTokens #-}


-- | Can we burst a single mini-protocol.
--
data CanBurst = BurstAllowed
              | BurstNotAllowed


-- | Can we batch more SDUs from different mini-protocols.
--
data CanBatch = BatchAllowed
              | BatchNotAllowed


-- | Maximal number of `SDU`s in a `SDUBatch`.
--
maxSDUsPerBatch :: Int
maxSDUsPerBatch = 100

-- | Minimal SDUSize for an SDU to be burst.
--
burstMinSdu :: SDUSize
burstMinSdu = truncate @Double $ fromIntegral msHeaderLength / 0.02


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
       , Alternative (STM m)
       )
    => [(Word8, EgressQueue m)]
    -> Tracer m BearerTrace
    -> Bearer m
    -> m void
muxer egressQueues0 tracer Bearer { writeMany, sduSize, batchSize, egressInterval } =
    withTimeoutSerial $ \timeout -> muxerLoop timeout (cycle egressQueues0)
  where
    numQueues :: Int
    numQueues = length egressQueues0
    toDouble :: DiffTime -> Double
    toDouble = realToFrac

    -- main muxer loop
    muxerLoop :: (forall a. DiffTime -> m a -> m (Maybe a))
              -> [(Word8, EgressQueue m)]
              -- ^ a cycle of egress queues
              -> m void
    muxerLoop timeout egressQueues = do
      start <- getMonotonicTime
      (sdu, egressQueues', canBatch) <- atomically do
        let -- All distinct `EgressQueue`s and their tail (so we keep reading
            -- them in a round robin way).
            available :: [(EgressQueue m, [(Word8, EgressQueue m)])]
            available = take numQueues (snd <$> egressQueues)
                        `zip`
                        tails egressQueues

        -- read first available `EgressQueue` and return its tail
        job <- runFirstToFinish
             . foldMap
                 ( FirstToFinish
                 . \(egressQueue, egressQueues') ->
                                (,egressQueues') <$> readTBQueue egressQueue
                 )
             $ available
        case job of
          (demand@(TLSRDemand mpc md d ProtocolBurst{maxBytes})
            , egressQueues'
            ) -> do
              let ((weight, egressQueue), rest) = assert (weight > 0)
                    case egressQueues' of
                      []   -> error "impossible"
                      x:xs -> (x, xs)
                  egressQueues'' | weight > 1 = (pred weight, egressQueue)
                                              : rest
                                 | otherwise  = rest
              eSdu <- processSingleWanton mpc md d sduSize
              case eSdu of
                NonEmptyWanton sdu
                  | maxBytes > 0
                  -> -- we do not check if the protocol has any tokens to
                     -- burst, that is deferred to buildBatch below.
                     (sdu, egressQueues', BatchAllowed)
                  <$ unGetTBQueue egressQueue demand

                  | otherwise
                  -> (sdu, egressQueues'', BatchNotAllowed)
                  <$ writeTBQueue egressQueue demand

                EmptyWanton sdu ->
                  pure (sdu, egressQueues'', BatchNotAllowed)

      (egressQueues'', SDUBatch { getSdus = sdus }) <-
        buildBatch (mkSingletonBatch sdu) egressQueues' canBatch start
      _ <- writeMany tracer timeout sdus
      end <- getMonotonicTime
      threadDelay $ egressInterval - end `diffTime` start
      muxerLoop timeout egressQueues''

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
    buildBatch
      :: SDUBatch
      -> [(Word8, EgressQueue m)]
      -> CanBatch -- ^ can we batch more SDUs
      -> Time
      -> m ([(Word8, EgressQueue m)], SDUBatch)
    buildBatch batch0 egressQueues1 canBatch0 start = do
        (qs, batch) <- go batch0 egressQueues1 canBatch0
        pure (qs, batch { getSdus = reverse (getSdus batch) })
     where
      go :: SDUBatch
         -> [(Word8, EgressQueue m)]
         -> CanBatch
         -> m ([(Word8, EgressQueue m)], SDUBatch)
      go !_batch [] !_canBatch = error "impossible"
      go !batch egressQueues !_canBatch
        | getCount batch >= maxSDUsPerBatch || getSdusLength batch >= batchSize
        = return (egressQueues, batch)
      go !batch egressQueues@((weight, queue):rest) !canBatch = do
        -- since the list of queues cycles, we only need to check the prefix
        -- to see if there is any more work to do.
        All allEmpty0 <-
          atomically $ getAp $ foldMap (Ap . fmap All . isEmptyTBQueue . snd)
                                       (take numQueues egressQueues)
        if allEmpty0
          then return (egressQueues, batch)
          else
            atomically (tryReadTBQueue queue) >>= \case
              Nothing -> go batch rest BatchNotAllowed
              Just demand@TLSRDemand {
                    wanton        = Wanton { lastSent, burstBucket },
                    protocolBurst = ProtocolBurst { maxBytes, refillRate }
                  } -> do
                (batch', canBurst) <- atomically do
                  delta <- (start `diffTime`) <$> stateTVar lastSent (, start)

                  nextSduSize <- stateTVar burstBucket \tokens ->
                    let tokens' :: TokenSize
                        tokens' = truncate $
                                    min (fromIntegral maxBytes)
                                        (fromIntegral tokens + fromIntegral refillRate * toDouble delta)
                        -- we leverage burst and deduct credits only where there is contention
                        -- between protocols
                        nextSduSize :: NextSDUSize
                        nextSduSize =
                          case canBatch of
                            BatchAllowed    -> BurstSize $ computeSDUSize sduSize tokens'
                            BatchNotAllowed -> BearerSize
                    in (nextSduSize, tokens')
                  if nextSDUSizeToSDUSize maxBound nextSduSize <= burstMinSdu
                    then do
                      -- edge case where the protocol is bursty, but there aren't enough tokens
                      -- available. The muxer forever loop does not check this
                      -- when it calls to build a batch, so we handle it here.
                      writeTBQueue queue demand
                      return (batch, BurstAllowed)
                    else
                      burstLoop demand batch nextSduSize

                case canBurst of
                  BurstAllowed
                    | weight > 1 ->
                      let egressQueues' = (pred weight, queue) : rest in
                      go batch' egressQueues' BatchNotAllowed
                    | otherwise ->
                      go batch' rest BatchNotAllowed
                  BurstNotAllowed ->
                    go batch' rest BatchNotAllowed
        where
          -- burst SDUs from a single mini-protocol until we consume all tokens
          -- (`TokenSize`).
          burstLoop :: TranslocationServiceRequest m
                    -> SDUBatch
                    -> NextSDUSize
                    -> STM m (SDUBatch, CanBurst)
          burstLoop demand@TLSRDemand { miniProtocolNum,
                                        miniProtocolDir,
                                        wanton = want@Wanton {burstBucket}
                                      }
                    !batch' !nextSDUSize = do
            -- The first SDU is always free. For `BearerSize` (no bursting),
            -- we don't count the wanton bytes against the burst allowance
            -- to permit a full sdu in the first iteration
            x <- processSingleWanton miniProtocolNum miniProtocolDir
                                     want
                                     (nextSDUSizeToSDUSize sduSize nextSDUSize)
            case x of
              EmptyWanton sdu -> do
                -- the `Wanton` is empty
                modifyTVar burstBucket \tokens ->
                  let consumed, tokens' :: TokenSize
                      consumed = consumedTokens nextSDUSize sdu
                      tokens'  = tokens - consumed
                  in assert (tokens >= consumed)
                     tokens'
                continue <- (\case
                                True -> BurstNotAllowed
                                False -> BurstAllowed)
                       <$> isEmptyTBQueue queue
                return (mkSingletonBatch sdu <> batch', continue)

              NonEmptyWanton sdu -> do
                -- the `Wanton` is non-empty
                nextSdu <- stateTVar burstBucket \tokens ->
                  let consumed, tokens' :: TokenSize
                      consumed = consumedTokens nextSDUSize sdu
                      tokens'  = tokens - consumed
                  in assert (tokens >= consumed)
                     (computeSDUSize sduSize tokens', tokens')
                let batch'' = mkSingletonBatch sdu <> batch'
                if nextSdu <= burstMinSdu -- 8 bytes header / 2% burst efficiency
                  then do
                       -- burst allowance has been exhausted, next
                       -- SDU would be too small
                       writeTBQueue queue demand
                       return (batch'', BurstAllowed)
                  else burstLoop demand batch'' (BurstSize nextSdu)


-- | Pull a `maxSDU`s worth of data out out the `Wanton` - if there is
-- data remaining requeue the `TranslocationServiceRequest` (this
-- ensures that any other items on the queue will get some service
-- first.
processSingleWanton :: MonadSTM m
                    => MiniProtocolNum
                    -> MiniProtocolDir
                    -> Wanton m
                    -> SDUSize
                    -> STM m SDUWithWantonState
processSingleWanton mpc md Wanton{wanton} sduSize = do
    (blob, wrap) <- do
      -- extract next SDU
      d <- readTVar wanton
      let (frag, rest) = BL.splitAt (fromIntegral sduSize) d
      -- if more to process then enqueue remaining work
      if BL.null rest
        then (frag, EmptyWanton) <$ writeTVar wanton BL.empty
        else do
          -- Note that to preserve bytestream ordering within a given
          -- miniprotocol the readTVar and writeTVar operations
          -- must be inside the same STM transaction.
          (frag, NonEmptyWanton) <$ writeTVar wanton rest
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
