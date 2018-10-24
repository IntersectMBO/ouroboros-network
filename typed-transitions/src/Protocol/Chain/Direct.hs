{-# LANGUAGE GADTSyntax #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Protocol.Chain.Direct where

-- The 'ProducerStream m' and 'ConsumerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
-- The 'streamProducer' and 'streamConsumer' from the modules imported
-- below will construct 'Peer's from 'ProducerStream' and 'ConsumerStream'
-- in such a way that this 'direct' linking factors through the
-- 'TrChainExchange' transition system.

import Protocol.Chain.ConsumerStream as Consumer
import Protocol.Chain.ProducerStream as Producer

direct
  :: ( Monad m )
  => ProducerStream point header m producer
  -> ConsumerStream point header m consumer
  -> m (producer, consumer)
direct bs cs = do
  bsAt <- runProducerStream bs
  csStep <- runConsumerStream cs (bsReadPointer bsAt) (bsTip bsAt)
  directMain (bsNext bsAt) csStep

directMain
  :: ( Monad m )
  => ProducerStreamNext point header m producer
  -> ConsumerStreamStep point header m consumer
  -> m (producer, consumer)
directMain bsNext css = case css of

  Quit consumer -> pure (bsDone bsNext, consumer)

  Consumer.Improve points forked k -> do
    pStreamStep <- bsImprove bsNext points
    case pStreamStep of
      ChangeFork readPointer header bsNext' ->
        runConsumerStream forked readPointer header >>= directMain bsNext'
      ChangeExtend header (Producer.Improve point bsNext') -> do
        cStreamStep <- k point (Just header)
        directMain bsNext' cStreamStep
      NoChange (Producer.Improve point bsNext') -> do
        cStreamStep <- k point Nothing
        directMain bsNext' cStreamStep

  DownloadHeaders num cDownload -> directDownload num cDownload bsNext

  NextTip forked k finished -> do
    pStreamStep <- bsNextChange bsNext
    case pStreamStep of
      Left producer -> pure (producer, finished)
      Right (ChangeFork readPointer header bsNext') ->
        runConsumerStream forked readPointer header >>= directMain bsNext'
      Right (ChangeExtend header (NextChange bsNext')) ->
        runConsumerNext k header >>= directMain bsNext'
      Right (NoChange f) -> impossible f

directDownload
  :: ( Monad m )
  => Word
  -> ConsumerDownload point header m consumer
  -> ProducerStreamNext point header m producer
  -> m (producer, consumer)
directDownload 0 cDownload bsNext = downloadOver cDownload Nothing >>= directMain bsNext
directDownload n cDownload bsNext = bsNextHeader bsNext >>= \ss -> case ss of
  ChangeFork readPointer header bsNext' ->
    runConsumerStream (downloadInterrupted cDownload) readPointer header >>= directMain bsNext'
  ChangeExtend header (NextHeader block bsNext') ->
    downloadHeader cDownload block (Just header) >>= flip (directDownload (n-1)) bsNext'
  ChangeExtend header (NoNextHeader bsNext') ->
    downloadOver cDownload (Just header) >>= directMain bsNext'
  NoChange (NextHeader block bsNext') ->
    downloadHeader cDownload block Nothing >>= flip (directDownload (n-1)) bsNext'
  NoChange (NoNextHeader bsNext') ->
    downloadOver cDownload Nothing >>= directMain bsNext'
