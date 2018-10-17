{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.StreamConsumer where

import Block
import Chain (Point)
import Data.List.NonEmpty (NonEmpty)
import Protocol.Core
import Protocol.Chain.Type

-- | A consumer stream always has a read pointer header and a tip header.
-- From these, it determines where to take the consumer protocol application,
-- within some 'm'.
newtype ConsumerStream m t = ConsumerStream
  { runConsumerStream :: Point -> Header -> m (ConsumerStreamStep m t)
  }

-- | A consumer choice: improve, download, wait for next, or stop.
data ConsumerStreamStep m t where
  Improve        :: NonEmpty Point
                 -> ConsumerStream m t -- ^ Forked
                 -> (Point -> MaybeNewTip -> m (ConsumerStreamStep m t))
                 -> ConsumerStreamStep m t
  -- | Download at most this many blocks from the read pointer (in scope from
  -- a 'ConsumerStream').
  DownloadBlocks :: Word -> ConsumerDownload m t -> ConsumerStreamStep m t
  -- | Await the next change in tip.
  NextTip        :: ConsumerStream m t
                 -> ConsumerNext m t
                 -> ConsumerStreamStep m t
  -- | End the consumer protocol application.
  Quit           :: t -> ConsumerStreamStep m t

-- | Expresses how to deal with a block download: what to do if a block comes
-- in, and what to do if the producer indicates that it's all over.
data ConsumerDownload m t = ConsumerDownload
  { downloadBlock       :: Block -> MaybeNewTip -> m (ConsumerDownload m t)
  , downloadOver        :: MaybeNewTip -> m (ConsumerStreamStep m t)
    -- | In case there's a fork during the download.
  , downloadInterrupted :: ConsumerStream m t
  }

-- | Awaiting the next change. The producer can give a header and possibly
-- opt to relay the body next.
data ConsumerNext m t = ConsumerNext
  { headerNoRelay :: Header -> m (ConsumerStreamStep m t)
  , headerRelay   :: Header -> m (ConsumerRelay m t)
  }

-- | When awaiting the body in a relay, the producer can fork, extend to a
-- new header, and possibly abandon the relay.
data ConsumerRelay m t = ConsumerRelay
  { -- | Relay complete.
    bodyRelay      :: Body -> m (ConsumerStreamStep m t)
    -- | New header, still relaying.
  , headerNewRelay :: Header -> m (ConsumerRelay m t)
    -- | New header, no relay.
  , headerNew      :: Header -> m (ConsumerStreamStep m t)
    -- | Forked.
  , abandonRelay   :: ConsumerStream m t
  }

streamConsumer
  :: ( Monad m )
  => ConsumerStream m t
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StInit) ('Yielding 'StIdle) m t
streamConsumer cs = await $ \it -> case it of
  TrInit readPointer tip -> hole $ do
    step <- runConsumerStream cs readPointer tip
    pure $ streamConsumerMain step

streamConsumerMain
  :: forall m t .
     ( Monad m )
  => ConsumerStreamStep m t
  -> Peer ChainExchange TrChainExchange ('Yielding 'StIdle) ('Yielding 'StIdle) m t
streamConsumerMain step = case step of
  Quit t -> done t
  NextTip forked k -> over (TrRequest ReqNext) $ await $ \req -> case req of
    TrRespond (ResChange readPointer tip) -> hole $ do
      step' <- runConsumerStream forked readPointer tip
      pure $ streamConsumerMain step'
    TrRespond (ResExtend header) -> hole $ do
      step' <- headerNoRelay k header
      pure $ streamConsumerMain step'
    TrRespond (ResExtendRelay header) -> hole $ do
      relay <- headerRelay k header
      pure $ relayLoop relay
      where
      relayLoop
        :: ConsumerRelay m t
        -> Peer ChainExchange TrChainExchange ('Awaiting ('StBusy 'Relay)) ('Yielding 'StIdle) m t
      relayLoop relay = await $ \req -> case req of
        TrRespond (ResRelayBody body) -> hole $ do
          next <- bodyRelay relay body
          pure $ streamConsumerMain next
        TrRespond (ResExtendNewRelay header) -> hole $ do
          relay' <- headerNewRelay relay header
          pure $ relayLoop relay'
        TrRespond (ResExtendNew header) -> hole $ do
          next <- headerNew relay header
          pure $ streamConsumerMain next
        TrRespond (ResChange readPointer tip) -> hole $ do
          next <- runConsumerStream (abandonRelay relay) readPointer tip
          pure $ streamConsumerMain next
  Improve cps forked k -> over (TrRequest (ReqSetHead cps)) $ await $ \req -> case req of
    TrRespond (ResSetHead h mNewTip) -> hole $ do
      next <- k h mNewTip
      pure $ streamConsumerMain next
    TrRespond (ResChange readPointer tip) -> hole $ do
      next <- runConsumerStream forked readPointer tip
      pure $ streamConsumerMain next
  DownloadBlocks num cd -> over (TrRequest (ReqDownload num)) (streamConsumerDownload cd)

streamConsumerDownload
  :: ( Monad m )
  => ConsumerDownload m t
  -> Peer ChainExchange TrChainExchange ('Awaiting ('StBusy 'Download)) ('Yielding 'StIdle) m t
streamConsumerDownload cd = await $ \res -> case res of
  TrRespond (ResDownloadDone mNewTip) -> hole $ do
    step <- downloadOver cd mNewTip
    pure $ streamConsumerMain step
  TrRespond (ResDownloadOne block mNewTip) -> hole $ do
    next <- downloadBlock cd block mNewTip
    pure $ streamConsumerDownload next
  TrRespond (ResChange readPointer tip) -> hole $ do
    step <- runConsumerStream (downloadInterrupted cd) readPointer tip
    pure $ streamConsumerMain step
