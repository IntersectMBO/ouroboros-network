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
  => BlockStream m t
  -> ConsumerStream m t
  -> m t
direct bs cs = do
  bsAt <- runBlockStream bs
  csStep <- runConsumerStream cs (bsReadPointer bsAt) (bsTip bsAt)
  directMain (bsNext bsAt) csStep

directMain
  :: ( Monad m )
  => BlockStreamNext m t
  -> ConsumerStreamStep m t
  -> m t
directMain bsNext css = case css of

  Quit t -> pure t

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

  NextTip forked k -> do
    pStreamStep <- bsNextChange bsNext
    case pStreamStep of
      Left x -> pure x
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
  => StreamStep RelayBody NextChange m t
  -> ConsumerRelay m t
  -> m t
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
  -> ConsumerDownload m t
  -> BlockStreamNext m t
  -> m t
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
