{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.ConsumerStream where

import Data.List.NonEmpty (NonEmpty)
import Protocol.Core
import Protocol.Chain.Type

-- | A consumer stream always has a read pointer header and a tip header.
-- From these, it determines where to take the consumer protocol application,
-- within some 'm'.
newtype ConsumerStream point header m t = ConsumerStream
  { runConsumerStream :: point -> header -> m (ConsumerStreamStep point header m t)
  }

-- | A consumer choice: improve, download, wait for next, or stop.
data ConsumerStreamStep point header m t where
  Improve        :: NonEmpty point
                 -> ConsumerStream point header m t -- Forked
                 -> (point -> Maybe header -> m (ConsumerStreamStep point header m t))
                 -> ConsumerStreamStep point header m t
  -- | Download at most this many blocks from the read pointer (in scope from
  -- a 'ConsumerStream').
  DownloadHeaders :: Word -> ConsumerDownload point header m t -> ConsumerStreamStep point header m t
  -- | Await the next change in tip.
  NextTip        :: ConsumerStream point header m t -- There was a fork
                 -> ConsumerNext point header m t   -- Normal next response
                 -> t                               -- Stream exhausted (producer side)
                 -> ConsumerStreamStep point header m t
  -- | End the consumer protocol application.
  Quit           :: t -> ConsumerStreamStep point header m t

-- | Expresses how to deal with a block download: what to do if a block comes
-- in, and what to do if the producer indicates that it's all over.
data ConsumerDownload point header m t = ConsumerDownload
  { downloadHeader      :: header -> Maybe header -> m (ConsumerDownload point header m t)
  , downloadOver        :: Maybe header -> m (ConsumerStreamStep point header m t)
    -- | In case there's a fork during the download.
  , downloadInterrupted :: ConsumerStream point header m t
  }

-- | Awaiting the next change. The producer can give a header and possibly
-- opt to relay the body next.
data ConsumerNext point header m t = ConsumerNext
  { runConsumerNext :: header -> m (ConsumerStreamStep point header m t)
  }

streamConsumer
  :: ( Monad m )
  => ConsumerStream point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting 'StInit) ('Finished 'StDone) m t
streamConsumer cs = await $ \it -> case it of
  TrInit readPointer tip -> hole $ do
    step <- runConsumerStream cs readPointer tip
    pure $ streamConsumerMain step

streamConsumerMain
  :: forall point header m t .
     ( Monad m )
  => ConsumerStreamStep point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Yielding 'StIdle) ('Finished 'StDone) m t
streamConsumerMain step = case step of
  Quit t -> out TrConsumerDone (done t)
  NextTip forked k exhausted -> over (TrRequest ReqNext) $ await $ \req -> case req of
    TrProducerDone -> done exhausted
    TrRespond (ResChange readPointer tip) -> hole $ do
      step' <- runConsumerStream forked readPointer tip
      pure $ streamConsumerMain step'
    TrRespond (ResExtend header) -> hole $ do
      step' <- runConsumerNext k header
      pure $ streamConsumerMain step'
  Improve cps forked k -> over (TrRequest (ReqSetHead cps)) $ await $ \req -> case req of
    TrRespond (ResSetHead h mNewTip) -> hole $ do
      next <- k h mNewTip
      pure $ streamConsumerMain next
    TrRespond (ResChange readPointer tip) -> hole $ do
      next <- runConsumerStream forked readPointer tip
      pure $ streamConsumerMain next
  DownloadHeaders num cd -> over (TrRequest (ReqDownload num)) (streamConsumerDownload cd)

streamConsumerDownload
  :: ( Monad m )
  => ConsumerDownload point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting ('StBusy 'Download)) ('Finished 'StDone) m t
streamConsumerDownload cd = await $ \res -> case res of
  TrRespond (ResDownloadDone mNewTip) -> hole $ do
    step <- downloadOver cd mNewTip
    pure $ streamConsumerMain step
  TrRespond (ResDownloadOne block mNewTip) -> hole $ do
    next <- downloadHeader cd block mNewTip
    pure $ streamConsumerDownload next
  TrRespond (ResChange readPointer tip) -> hole $ do
    step <- runConsumerStream (downloadInterrupted cd) readPointer tip
    pure $ streamConsumerMain step
