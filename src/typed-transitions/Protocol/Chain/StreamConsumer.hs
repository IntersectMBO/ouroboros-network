{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.StreamConsumer where

import Block
import Data.List.NonEmpty (NonEmpty)
import Protocol.Core
import Protocol.Chain.Type

-- | A consumer stream always has a read pointer header and a tip header.
-- From these, it determines where to take the consumer protocol application,
-- within some 'm'.
newtype ConsumerStream m t = ConsumerStream
  { runConsumerStream :: Header -> Header -> m (ConsumerStreamStep m t)
  }

-- | A consumer choice: improve, download, wait for next, or stop.
data ConsumerStreamStep m t where
  -- TODO improve, download need to handle new tip case.
  Improve        :: NonEmpty HeaderHash -> (HeaderHash -> m (ConsumerStreamStep m t)) -> ConsumerStreamStep m t
  -- | Download at most this many blocks from the read pointer (in scope from
  -- a 'ConsumerStream').
  DownloadBlocks :: Word -> ConsumerDownload m t -> ConsumerStreamStep m t
  -- | Await the next change in tip.
  NextTip        :: ConsumerStream m t -> ConsumerStreamStep m t
  -- | End the consumer protocol application.
  Quit           :: t -> ConsumerStreamStep m t

-- | Expresses how to deal with a block download: what to do if a block comes
-- in, and what to do if the producer indicates that it's all over.
-- TODO handle the case when the producer forks during a download.
data ConsumerDownload m t = ConsumerDownload
  { downloadBlock :: Block -> m (ConsumerDownload m t)
  , downloadOver  :: m (ConsumerStreamStep m t)
  }

-- Note: in the stream consumer, some pattern matches are incomplete, because
-- they do not yet handle the case in which there's a fork after a request.

streamConsumer
  :: ( Monad m )
  => ConsumerStream m t
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StInit) ('Yielding 'StIdle) m t
streamConsumer cs = await $ \it -> case it of
  TrInit readPointer tip -> hole $ do
    step <- runConsumerStream cs readPointer tip
    pure $ streamConsumerMain step

streamConsumerMain
  :: ( Monad m )
  => ConsumerStreamStep m t
  -> Peer ChainExchange TrChainExchange ('Yielding 'StIdle) ('Yielding 'StIdle) m t
streamConsumerMain step = case step of
  Quit t -> done t
  NextTip k -> over (TrRequest ReqNext) $ await $ \req -> case req of
    TrRespond (ResChange readPointer tip) -> hole $ do
      step' <- runConsumerStream k readPointer tip
      pure $ streamConsumerMain step'
  Improve cps k -> over (TrRequest (ReqSetHead cps)) $ await $ \req -> case req of
    TrRespond (ResSetHead h) -> hole $ do
      next <- k h
      pure $ streamConsumerMain next
  DownloadBlocks num cd -> over (TrRequest (ReqDownload num)) (streamConsumerDownload cd)

streamConsumerDownload
  :: ( Monad m )
  => ConsumerDownload m t
  -> Peer ChainExchange TrChainExchange ('Awaiting ('StBusy 'Download)) ('Yielding 'StIdle) m t
streamConsumerDownload cd = await $ \res -> case res of
  TrRespond ResDownloadDone -> hole $ do
    step <- downloadOver cd
    pure $ streamConsumerMain step
  TrRespond (ResDownloadOne block) -> hole $ do
    next <- downloadBlock cd block
    pure $ streamConsumerDownload next
