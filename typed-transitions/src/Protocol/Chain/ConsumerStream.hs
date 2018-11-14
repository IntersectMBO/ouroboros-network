{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

-- |
-- =
--
-- Definition of 'ConsumerStream'. It defines a consumer in the chain
-- exchange protocol. Its complementary type is 'ProducerStream' from
-- 'Protocol.Chain.ProducerStream'.
--
-- A 'ConsumerStream' should always have a 'point' and 'header' in scope: the
-- read pointer and tip of the producer's chain. This means that the consumer
-- in fact begins as a server, awaiting data from the producer.

module Protocol.Chain.ConsumerStream where

import Data.List.NonEmpty (NonEmpty)
import Protocol.Core
import Protocol.Chain.Type hiding (Next, Download, Improve)
import qualified Protocol.Chain.Type as Type

-- | A consumer stream always has a read pointer header and a tip header.
-- From these, it determines where to take the consumer protocol application,
-- within some 'm'.
--
--   (point, header) -o m (ConsumerChoice point header m t)
--
newtype ConsumerStream point header m t = ConsumerStream
  { runConsumerStream :: (point, point) -> m (ConsumerChoice point header m t)
  }

-- | An additive disjunction for consumer choice.
-- Notice the normal form: each constructor is a multiplicative conjunction
-- with a piece of data (possibly () for nothing) and an additive conjunction
-- to deal with responses.
--
-- TODO except for 'Quit'. How does that fit in with the normal form?
--
data ConsumerChoice point header m t where
  Improve  :: NonEmpty point
           -> ConsumerImprove point header m t
           -> ConsumerChoice point header m t
  -- | Download at most this many blocks from the read pointer (in scope from
  -- a 'ConsumerStream').
  Download :: Word
           -> ConsumerDownload point header m t
           -> ConsumerChoice point header m t
  -- | Await the next change in tip.
  Next     :: ()
           -> ConsumerNext point header m t
           -> ConsumerChoice point header m t
  -- | End the consumer protocol application.
  Stop     :: t
           -> ConsumerChoice point header m t

-- | An additive conjunction for response to 'Improve'.
data ConsumerImprove point header m t = ConsumerImprove
  { improveForked :: (point, point)       -> m (ConsumerChoice point header m t)
  , improvePoint  :: (point, Maybe point) -> m (ConsumerChoice point header m t)
  }

-- | Expresses how to deal with a block download: what to do if a block comes
-- in, and what to do if the producer indicates that it's all over.
--
-- An additive conjunction for response to 'Download'.
data ConsumerDownload point header m t = ConsumerDownload
  { downloadHeader      :: (header, Maybe point) -> m (ConsumerDownload point header m t)
  , downloadOver        :: Maybe point           -> m (ConsumerChoice point header m t)
    -- | In case there's a fork during the download.
  , downloadForked      :: (point, point)        -> m (ConsumerChoice point header m t)
  }

-- | An additive conjunction for response to 'Next'.
-- Producer can fork, extend, or stop (there is no next).
data ConsumerNext point header m t = ConsumerNext
  { nextForked    :: (point, point) -> m (ConsumerChoice point header m t)
  , nextExtended  :: point          -> m (ConsumerChoice point header m t)
  , nextExhausted :: t
  }

streamConsumer
  :: ( Monad m )
  => ConsumerStream point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting 'StInit) ('Finished 'StDone) m t
streamConsumer cs = await $ \it -> case it of
  TrInit points -> lift $ do
    next <- runConsumerStream cs points
    pure $ streamConsumerMain next

streamConsumerMain
  :: forall point header m t .
     ( Monad m )
  => ConsumerChoice point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Yielding 'StIdle) ('Finished 'StDone) m t
streamConsumerMain step = case step of
  Improve cps k -> over (TrRequest (ReqImprove cps)) $ await $ \req -> case req of
    TrRespond (ResImprove improvement) -> lift $ do
      next <- improvePoint k improvement
      pure $ streamConsumerMain next
    TrRespond (ResForked points) -> lift $ do
      next <- improveForked k points
      pure $ streamConsumerMain next
  Download num k -> over (TrRequest (ReqDownload num)) (streamConsumerDownload k)
  Next () k -> over (TrRequest ReqNext) $ await $ \req -> case req of
    TrProducerDone -> done (nextExhausted k)
    TrRespond (ResForked points) -> lift $ do
      next <- nextForked k points
      pure $ streamConsumerMain next
    TrRespond (ResExtend point) -> lift $ do
      next <- nextExtended k point
      pure $ streamConsumerMain next
  Stop t -> out TrConsumerDone (done t)

streamConsumerDownload
  :: ( Monad m )
  => ConsumerDownload point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting ('StBusy 'Type.Download)) ('Finished 'StDone) m t
streamConsumerDownload k = await $ \res -> case res of
  TrRespond (ResDownloadDone mNewTip) -> lift $ do
    next <- downloadOver k mNewTip
    pure $ streamConsumerMain next
  TrRespond (ResDownloadOne datum) -> lift $ do
    next <- downloadHeader k datum
    pure $ streamConsumerDownload next
  TrRespond (ResForked points) -> lift $ do
    next <- downloadForked k points
    pure $ streamConsumerMain next
