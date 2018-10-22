{-# LANGUAGE GADTSyntax #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Protocol.Chain.Direct where

-- The 'BlockStream m' and 'ConsumerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
-- The 'streamProducer' and 'streamConsumer' from the modules imported
-- below will construct 'Peer's from 'BlockStream' and 'ConsumerStream'
-- in such a way that this 'direct' linking factors through the
-- 'TrChainExchange' transition system.

import Protocol.Chain.StreamConsumer as Consumer
import Protocol.Chain.StreamProducer as Producer

direct
  :: ( Monad m )
  => BlockStream p m producer
  -> ConsumerStream p m consumer
  -> m (producer, consumer)
direct bs cs = do
  bsAt <- runBlockStream bs
  csStep <- runConsumerStream cs (bsReadPointer bsAt) (bsTip bsAt)
  directMain (bsNext bsAt) csStep

directMain
  :: ( Monad m )
  => BlockStreamNext p m producer
  -> ConsumerStreamStep p m consumer
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

  DownloadBlocks num cDownload -> directDownload num cDownload bsNext

  NextTip forked k finished -> do
    pStreamStep <- bsNextChange bsNext
    case pStreamStep of
      Left producer -> pure (producer, finished)
      Right (ChangeFork readPointer header bsNext') ->
        runConsumerStream forked readPointer header >>= directMain bsNext'
      Right (ChangeExtend header (NoRelay bsNext')) ->
        headerNoRelay k header >>= directMain bsNext'
      Right (ChangeExtend header (Relaying pRelay)) -> do
        cRelay <- headerRelay k header
        pStreamStep <- pRelay
        directRelay pStreamStep cRelay
      Right (NoChange f) -> impossible f

directRelay
  :: ( Monad m )
  => StreamStep p (RelayBody p) (NextChange p) m producer
  -> ConsumerRelay p m consumer
  -> m (producer, consumer)
directRelay pStreamStep cRelay = case pStreamStep of
  ChangeFork readPointer header bsNext' ->
    runConsumerStream (abandonRelay cRelay) readPointer header >>= directMain bsNext'
  ChangeExtend header (NoRelay bsNext') ->
    headerNew cRelay header >>= directMain bsNext'
  ChangeExtend header (Relaying pRelay) -> do
    cRelay <- headerNewRelay cRelay header
    pStreamStep <- pRelay
    directRelay pStreamStep cRelay
  NoChange (RelayBody body bsNext') ->
    bodyRelay cRelay body >>= directMain bsNext'

directDownload
  :: ( Monad m )
  => Word
  -> ConsumerDownload p m consumer
  -> BlockStreamNext p m producer
  -> m (producer, consumer)
directDownload 0 cDownload bsNext = downloadOver cDownload Nothing >>= directMain bsNext
directDownload n cDownload bsNext = bsNextBlock bsNext >>= \ss -> case ss of
  ChangeFork readPointer header bsNext' ->
    runConsumerStream (downloadInterrupted cDownload) readPointer header >>= directMain bsNext'
  ChangeExtend header (NextBlock block bsNext') ->
    downloadBlock cDownload block (Just header) >>= flip (directDownload (n-1)) bsNext'
  ChangeExtend header (NoNextBlock bsNext') ->
    downloadOver cDownload (Just header) >>= directMain bsNext'
  NoChange (NextBlock block bsNext') ->
    downloadBlock cDownload block Nothing >>= flip (directDownload (n-1)) bsNext'
  NoChange (NoNextBlock bsNext') ->
    downloadOver cDownload Nothing >>= directMain bsNext'
