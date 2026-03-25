{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
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
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bool
import Data.ByteString.Lazy qualified as BL
import Data.Either (fromRight)
import Data.List (tails)
import Data.Monoid.Synchronisation
import Data.Word (Word8, Word32)

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)

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
     TLSRDemand !MiniProtocolNum !MiniProtocolDir !(Wanton m) !ProtocolBurstLimits

-- | A Wanton represent the concrete data to be translocated, note that the
--  TVar becoming empty indicates -- that the last fragment of the data has
--  been enqueued on the -- underlying bearer.
data Wanton m = Wanton {
  want      :: !(StrictTVar m BL.ByteString),
  wLastSent :: !(StrictTVar m Time),
  wBucket   :: !(StrictTVar m Word32)
  }

data SDUBatch = SDUBatch {
  getCount :: !Int,
  getSdusLength :: !Int,
  getSdus :: ![SDU]
  }

mkSingletonBatch :: SDU -> SDUBatch
mkSingletonBatch sdu = SDUBatch 1 (sduLength sdu) [sdu]

sduLength :: SDU -> Int
sduLength sdu = fromIntegral msHeaderLength + fromIntegral (msLength sdu)

instance Semigroup SDUBatch where
  (SDUBatch a b c) <> (SDUBatch a' b' c') = SDUBatch (a + a') (b + b') (c <> c')


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
    -> Bearer m
    -> m void
muxer egressQueues0 Bearer { writeMany, sduSize, batchSize, egressInterval } = do
    let numQueues = length egressQueues0
    withTimeoutSerial $ \timeout -> (`evalStateT` cycle egressQueues0) $ forever do
      egressQueues <- get
      let job = foldMap (FirstToFinish . traverse readTBQueue)
                        (zip (tails egressQueues) (take numQueues (snd <$> egressQueues)))

      start <- lift getMonotonicTime
      (sdu, egressQueues', burst) <- lift $ atomically do
        result <- runFirstToFinish job
        case result of
          (egressQueues', demand@(TLSRDemand mpc md d (ProtocolBurstLimits pbMaxBytes _pbRefillRate))) -> do
              let ((weight, queue), rest) = assert (weight > 0) case egressQueues' of
                    [] -> error "impossible"
                    x:xs -> (x, xs)
                  egressQueues'' = (pred weight, queue) : rest
              eSdu <- processSingleWanton sduSize mpc md d
              case eSdu of
                Right sdu | pbMaxBytes > 0 ->
                              -- we do not check if the protocol has any tokens to burst,
                              -- that is deferred to buildBatch below.
                              (sdu, egressQueues', True) <$ unGetTBQueue queue demand
                          | weight > 1 ->
                              (sdu, egressQueues'', False) <$ writeTBQueue queue demand
                          | otherwise ->
                              (sdu, rest, False) <$ writeTBQueue queue demand
                Left sdu | weight > 1 ->
                            pure (sdu, egressQueues'', False)
                         | otherwise ->
                            pure (sdu, rest, False)

      (egressQueues'', batch'') <-
        lift $ buildBatch (mkSingletonBatch sdu) numQueues egressQueues' burst start
      put egressQueues''
      void . lift $ writeMany timeout (getSdus batch'')
      delta <- (`diffTime` start) <$> lift getMonotonicTime
      lift . threadDelay $ egressInterval - delta

  where
    maxSDUsPerBatch :: Int
    maxSDUsPerBatch = 100

    toDouble :: Real a => a -> Double
    toDouble = realToFrac

    burstMinSdu = truncate @Double @SDUSize $ fromIntegral msHeaderLength / 0.02

    buildBatch batch0 numQueues egressQueues1 mBurst0 start = do
        (qs, b) <- go batch0 egressQueues1 mBurst0
        pure (qs, b { getSdus = reverse (getSdus b) })
     where
      allM f = \case
        [] -> pure True
        (x:xs) -> do
          res <- f x
          if res then allM f xs else pure False

      go :: SDUBatch -> [(Word8, EgressQueue m)] -> Bool -> m ([(Word8, EgressQueue m)], SDUBatch)
      go _batch [] _burst = error "impossible"
      go !batch egressQueues _burst
        | getCount batch >= maxSDUsPerBatch || getSdusLength batch >= batchSize
          = return (egressQueues, batch)
      go batch egressQueues@((weight, queue):rest) mBurst = do
        allEmpty0 <- atomically $ allM isEmptyTBQueue (snd <$> take numQueues egressQueues)
        if allEmpty0
          then return (egressQueues, batch)
          else do
            mResult <- atomically $ tryReadTBQueue queue
            case mResult of
              Nothing -> go batch rest False
              Just demand@(TLSRDemand mpc md d@Wanton { wLastSent, wBucket } (ProtocolBurstLimits pbMaxBytes pbRefillRate)) -> do
                (batch', goAgain) <- atomically do
                  delta <- (start `diffTime`) <$> stateTVar wLastSent (, start)
                  thisEmpty <- isEmptyTBQueue queue
                  restEmpty <- allM isEmptyTBQueue (snd <$> take (pred numQueues) rest)
                  let allEmpty = thisEmpty && restEmpty
                      boundedTokens = min sduSize . fromIntegral . min (fromIntegral $ maxBound @SDUSize)
                  sduSize0 <- stateTVar wBucket \tokens ->
                    let tokens' = truncate $
                                    min (fromIntegral pbMaxBytes)
                                        (fromIntegral tokens + fromIntegral pbRefillRate * toDouble delta)
                        -- we leverage burst and deduct credits only where there is contention
                        -- between protocols
                        sduSize0 = bool (Left sduSize) (Right $ boundedTokens tokens') (mBurst && not allEmpty)
                    in (sduSize0, tokens')
                  let step (!batch', !eSize) mx = do
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
                            throwE (mkSingletonBatch sdu <> batch', not thisEmpty)
                          Right sdu -> do
                            nextSdu <- lift $ stateTVar wBucket \tokens ->
                                           let tokens' = tokens - consumedTokens (fromIntegral (msLength sdu))
                                           in assert (tokens >= consumedTokens (fromIntegral $ msLength sdu))
                                              (boundedTokens tokens', tokens')
                            let batch'' = mkSingletonBatch sdu <> batch'
                            if nextSdu <= burstMinSdu -- 8 bytes header / 2% burst efficiency
                              then do
                                   -- there is more payload, but burst allowance has been exhausted
                                   lift $ writeTBQueue queue demand
                                   throwE (batch'', True)
                              else pure (batch'', Right nextSdu)
                  -- False to stay consistent with Left branch
                  either pure ((<$ writeTBQueue queue demand) . second (const True))
                    =<< runExceptT do
                          when (fromRight maxBound sduSize0 <= burstMinSdu) do
                            -- edge case where the protocol is bursty, but there aren't enough tokens
                            -- available. The muxer forever loop does not check this
                            -- when it calls to build a batch, so we handle it here.
                            lift $ writeTBQueue queue demand
                            throwE (batch, True)
                          foldM step (batch, sduSize0)
                                     (if allEmpty
                                        then [const $ processSingleWanton sduSize mpc md d]
                                             -- ^ grab full sdu, save the tokens for cases with contention
                                        else repeat (\sduSize' -> processSingleWanton sduSize' mpc md d))

                if weight > 1 && goAgain
                  then let egressQueues' = (pred weight, queue) : rest
                        in go batch' egressQueues' False
                  else go batch' rest False


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
