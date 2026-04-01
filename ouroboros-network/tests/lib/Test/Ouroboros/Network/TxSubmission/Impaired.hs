{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test-only impairment shims for 'TxSubmissionClient'. They wrap an
-- existing outbound peer with behavioural faults at the typed-protocol
-- level: 'delayBodies' adds latency to body replies, 'omitBodies' drops
-- bodies probabilistically. Both pass txid replies through unchanged so
-- the impaired peer still advertises promptly.
--
-- The wrappers are polymorphic in @txid@ and @tx@ and depend only on the
-- TxSubmission2 client types.
module Test.Ouroboros.Network.TxSubmission.Impaired
  ( delayBodies
  , omitBodies
  , extraTxIds
  , unrequestedTx
  , Impairment (..)
  , noImpairment
  , applyImpairment
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, StrictTVar,
           atomically, newTVarIO, stateTVar)
import Control.Monad (filterM)
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTimer.SI (MonadDelay, threadDelay)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word16)
import System.Random (StdGen, mkStdGen, uniformR)

import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToReq (..))
import Ouroboros.Network.SizeInBytes (SizeInBytes)


-- | Add a fixed delay before every 'MsgReplyTxs' (body reply); txid
-- replies pass through unchanged. Models a peer that advertises promptly
-- but is slow to deliver bodies.
--
-- The wrapper is recursive: every 'ClientStIdle' continuation produced
-- by the inner peer is wrapped in turn, so the delay applies to every
-- body reply through the protocol session - not just the first.
delayBodies
  :: forall txid tx m a.
     MonadDelay m
  => DiffTime
  -> TxSubmissionClient txid tx m a
  -> TxSubmissionClient txid tx m a
delayBodies d (TxSubmissionClient mIdle) =
    TxSubmissionClient (wrapIdle <$> mIdle)
  where
    wrapIdle :: ClientStIdle txid tx m a -> ClientStIdle txid tx m a
    wrapIdle ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs } = ClientStIdle
      { recvMsgRequestTxIds = \blocking ack req -> do
          reply <- recvMsgRequestTxIds blocking ack req
          pure (wrapTxIds reply)
      , recvMsgRequestTxs   = \txids -> do
          reply <- recvMsgRequestTxs txids
          threadDelay d
          pure (wrapTxs reply)
      }

    wrapTxIds :: ClientStTxIds blocking txid tx m a
              -> ClientStTxIds blocking txid tx m a
    wrapTxIds (SendMsgReplyTxIds reply k) = SendMsgReplyTxIds reply (wrapIdle k)
    wrapTxIds (SendMsgDone a)             = SendMsgDone a

    wrapTxs :: ClientStTxs txid tx m a -> ClientStTxs txid tx m a
    wrapTxs (SendMsgReplyTxs txs k) = SendMsgReplyTxs txs (wrapIdle k)


-- | Drop each body in 'MsgReplyTxs' independently with the given
-- probability; txid replies pass through unchanged. Models a peer whose
-- mempool evicts entries between advertise and fetch - the receiver sees
-- a body list that is a subset of what it requested.
--
-- Randomness is threaded through a 'StrictTVar' so the test can seed it
-- from a QuickCheck-generated value and produce reproducible drop
-- patterns. Each body is decided independently.
--
-- Recursive in the same way as 'delayBodies'.
omitBodies
  :: forall txid tx m a.
     MonadSTM m
  => StrictTVar m StdGen
  -> Double
  -- ^ drop probability for each body, in [0, 1]
  -> TxSubmissionClient txid tx m a
  -> TxSubmissionClient txid tx m a
omitBodies genVar p (TxSubmissionClient mIdle) =
    TxSubmissionClient (wrapIdle <$> mIdle)
  where
    wrapIdle :: ClientStIdle txid tx m a -> ClientStIdle txid tx m a
    wrapIdle ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs } = ClientStIdle
      { recvMsgRequestTxIds = \blocking ack req -> do
          reply <- recvMsgRequestTxIds blocking ack req
          pure (wrapTxIds reply)
      , recvMsgRequestTxs   = \txids -> do
          SendMsgReplyTxs txs k <- recvMsgRequestTxs txids
          kept <- atomically (filterM (const rollKeep) txs)
          pure (SendMsgReplyTxs kept (wrapIdle k))
      }

    wrapTxIds :: ClientStTxIds blocking txid tx m a
              -> ClientStTxIds blocking txid tx m a
    wrapTxIds (SendMsgReplyTxIds reply k) = SendMsgReplyTxIds reply (wrapIdle k)
    wrapTxIds (SendMsgDone a)             = SendMsgDone a

    rollKeep = stateTVar genVar $ \g ->
      case uniformR (0 :: Double, 1) g of
        (x, g') -> (x >= p, g')


-- | Pad each non-empty 'MsgReplyTxIds' reply so the total length is at
-- least @req + n@, where @req@ is the count the inbound asked for.
-- Padding entries are duplicates of the first reply entry. Replies that
-- are already empty pass through unchanged.
--
-- Triggers 'ProtocolErrorTxIdsNotRequested' on the inbound side as soon
-- as @n > 0@.
extraTxIds
  :: forall txid tx m a.
     Monad m
  => Word16
  -> TxSubmissionClient txid tx m a
  -> TxSubmissionClient txid tx m a
extraTxIds n (TxSubmissionClient mIdle) =
    TxSubmissionClient (wrapIdle <$> mIdle)
  where
    wrapIdle :: ClientStIdle txid tx m a -> ClientStIdle txid tx m a
    wrapIdle ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs } = ClientStIdle
      { recvMsgRequestTxIds = \blocking ack req -> do
          reply <- recvMsgRequestTxIds blocking ack req
          pure (wrapTxIds req reply)
      , recvMsgRequestTxs   = \txids -> do
          reply <- recvMsgRequestTxs txids
          pure (wrapTxs reply)
      }

    wrapTxIds :: NumTxIdsToReq
              -> ClientStTxIds blocking txid tx m a
              -> ClientStTxIds blocking txid tx m a
    wrapTxIds req (SendMsgReplyTxIds reply k) = SendMsgReplyTxIds (pad req reply) (wrapIdle k)
    wrapTxIds _   (SendMsgDone a)             = SendMsgDone a

    wrapTxs :: ClientStTxs txid tx m a -> ClientStTxs txid tx m a
    wrapTxs (SendMsgReplyTxs txs k) = SendMsgReplyTxs txs (wrapIdle k)

    -- Append (req + n - length xs) duplicates of the head, so the final
    -- length exceeds the requested count by exactly n.
    pad :: NumTxIdsToReq
        -> BlockingReplyList blocking (txid, SizeInBytes)
        -> BlockingReplyList blocking (txid, SizeInBytes)
    pad _   r | n == 0 = r
    pad req (BlockingReply xs) =
        let target = fromIntegral (getNumTxIdsToReq req) + fromIntegral n :: Int
            need   = max 1 (target - NonEmpty.length xs)
        in BlockingReply (xs <> NonEmpty.fromList
                                 (replicate need (NonEmpty.head xs)))
    pad _   (NonBlockingReply []) = NonBlockingReply []
    pad req (NonBlockingReply xs@(x:_)) =
        let target = fromIntegral (getNumTxIdsToReq req) + fromIntegral n :: Int
            need   = max 1 (target - length xs)
        in NonBlockingReply (xs ++ replicate need x)


-- | Append a fabricated body to each non-empty 'MsgReplyTxs' whose txid
-- is not in the corresponding request list. Models a peer that replies
-- with bodies that were never asked for.
--
-- Triggers 'ProtocolErrorTxNotRequested' on the inbound side. The
-- fabricated body is produced by @mkBad reqs orig@ where @orig@ is the
-- first legitimate body in the reply (used as a template).
unrequestedTx
  :: forall txid tx m a.
     Monad m
  => ([txid] -> tx -> tx)
  -> TxSubmissionClient txid tx m a
  -> TxSubmissionClient txid tx m a
unrequestedTx mkBad (TxSubmissionClient mIdle) =
    TxSubmissionClient (wrapIdle <$> mIdle)
  where
    wrapIdle :: ClientStIdle txid tx m a -> ClientStIdle txid tx m a
    wrapIdle ClientStIdle { recvMsgRequestTxIds, recvMsgRequestTxs } = ClientStIdle
      { recvMsgRequestTxIds = \blocking ack req -> do
          reply <- recvMsgRequestTxIds blocking ack req
          pure (wrapTxIds reply)
      , recvMsgRequestTxs   = \txids -> do
          reply <- recvMsgRequestTxs txids
          pure (wrapTxs txids reply)
      }

    wrapTxIds :: ClientStTxIds blocking txid tx m a
              -> ClientStTxIds blocking txid tx m a
    wrapTxIds (SendMsgReplyTxIds reply k) = SendMsgReplyTxIds reply (wrapIdle k)
    wrapTxIds (SendMsgDone a)             = SendMsgDone a

    wrapTxs :: [txid] -> ClientStTxs txid tx m a -> ClientStTxs txid tx m a
    wrapTxs _     (SendMsgReplyTxs []          k) = SendMsgReplyTxs [] (wrapIdle k)
    wrapTxs txids (SendMsgReplyTxs txs@(x : _) k) =
        SendMsgReplyTxs (txs ++ [mkBad txids x]) (wrapIdle k)


-- | Behavioural fault injection on a peer's outbound 'TxSubmissionClient'.
-- Peers configured with 'noImpairment' run unwrapped.
data Impairment = Impairment
  { impairBodyDelay     :: Maybe DiffTime
    -- ^ added before each MsgReplyTxs; txid replies are unaffected
  , impairOmitProb      :: Double
    -- ^ per-body Bernoulli drop probability, in [0, 1]
  , impairSeed          :: Int
    -- ^ seed for the per-peer StdGen used by 'omitBodies'
  , impairExtraTxIds    :: Word16
    -- ^ pad each MsgReplyTxIds with this many duplicate entries; 0 = off
  , impairUnrequestedTx :: Bool
    -- ^ append one fabricated body whose txid is not in the request
  } deriving (Eq, Show)

-- | The neutral impairment: no delay, no omission. Equivalent to running the
-- client unwrapped.
noImpairment :: Impairment
noImpairment = Impairment { impairBodyDelay     = Nothing
                          , impairOmitProb      = 0
                          , impairSeed          = 0
                          , impairExtraTxIds    = 0
                          , impairUnrequestedTx = False
                          }

-- | Wrap a 'TxSubmissionClient' with the given 'Impairment'. Allocates a
-- per-peer 'StdGen' TVar only when the omission rate is non-zero. The
-- @mkBad@ argument is consulted only when 'impairUnrequestedTx' is set;
-- it produces a body whose txid is not in the request list.
applyImpairment :: (MonadDelay m, MonadSTM m)
                => Impairment
                -> ([txid] -> tx -> tx)
                -> TxSubmissionClient txid tx m a
                -> m (TxSubmissionClient txid tx m a)
applyImpairment Impairment { impairBodyDelay, impairOmitProb, impairSeed
                           , impairExtraTxIds, impairUnrequestedTx } mkBad c0 = do
    c1 <- if impairOmitProb > 0
            then do
              genVar <- newTVarIO (mkStdGen impairSeed)
              pure (omitBodies genVar impairOmitProb c0)
            else pure c0
    let c2 = case impairBodyDelay of
          Just d  -> delayBodies d c1
          Nothing -> c1
        c3 | impairExtraTxIds > 0 = extraTxIds impairExtraTxIds c2
           | otherwise            = c2
        c4 | impairUnrequestedTx = unrequestedTx mkBad c3
           | otherwise           = c3
    pure c4
