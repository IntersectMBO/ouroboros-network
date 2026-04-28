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
  ) where

import Control.Concurrent.Class.MonadSTM.Strict (MonadSTM, StrictTVar,
           atomically, stateTVar)
import Control.Monad (filterM)
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTimer.SI (MonadDelay, threadDelay)
import System.Random (StdGen, uniformR)

import Ouroboros.Network.Protocol.TxSubmission2.Client


-- | Add a fixed delay before every 'MsgReplyTxs' (body reply); txid
-- replies pass through unchanged. Models a peer that advertises promptly
-- but is slow to deliver bodies.
--
-- The wrapper is recursive: every 'ClientStIdle' continuation produced
-- by the inner peer is wrapped in turn, so the delay applies to every
-- body reply through the protocol session — not just the first.
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
-- mempool evicts entries between advertise and fetch — the receiver sees
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
