{-# LANGUAGE GADTSyntax #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ouroboros.Network.Protocol.Chain.Direct where

-- The 'ProducerStream m' and 'ConsumerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
-- The 'streamProducer' and 'streamConsumer' from the modules imported
-- below will construct 'Peer's from 'ProducerStream' and 'ConsumerStream'
-- in such a way that this 'direct' linking factors through the
-- 'TrChainExchange' transition system.

import Ouroboros.Network.Protocol.Chain.ConsumerStream as Consumer
import Ouroboros.Network.Protocol.Chain.ProducerStream as Producer

direct
  :: ( Monad m )
  => ProducerStream point header m producer
  -> ConsumerStream point header m consumer
  -> m (consumer, producer)
direct ps cs = do
  psInit   <- runProducerStream ps
  csChoice <- runConsumerStream cs (initPoints psInit)
  directMain csChoice (producerChoice psInit)

directMain
  :: ( Monad m )
  => ConsumerChoice point header m consumer
  -> ProducerChoice point header m producer
  -> m (consumer, producer)
directMain cchoice pchoice = case cchoice of

  Consumer.Improve points cimprove -> do
    pimprove <- producerImprove pchoice points
    case pimprove of
      ImprovePoint improvement pchoice' ->  do
        cchoice' <- improvePoint cimprove improvement
        directMain cchoice' pchoice'
      ImproveForked points pchoice' -> do
        cchoice' <- improveForked cimprove points
        directMain cchoice' pchoice'

  Consumer.Next unit cnext -> do
    pnext <- producerNext pchoice unit
    case pnext of
      NextForked points pchoice' -> do
        cchoice' <- nextForked cnext points
        directMain cchoice' pchoice'
      NextExtended point pchoice' -> do
        cchoice' <- nextExtended cnext point
        directMain cchoice' pchoice'
      NextExhausted producerValue ->
        pure (nextExhausted cnext, producerValue)

  Consumer.Download num cdownload -> do
    pdownload <- producerDownload pchoice num
    directDownload cdownload pdownload

  Stop consumerValue -> pure (consumerValue, producerDone pchoice)

directDownload
  :: ( Monad m )
  => ConsumerDownload point header m consumer
  -> ProducerDownload point header m producer
  -> m (consumer, producer)
directDownload cdownload pdownload = case pdownload of
  DownloadForked points pchoice -> do
    cchoice <- downloadForked cdownload points
    directMain cchoice pchoice
  DownloadOver mNewTip pchoice -> do
    cchoice <- downloadOver cdownload mNewTip
    directMain cchoice pchoice
  DownloadHeader datum pnext -> do
    cdownload' <- downloadHeader cdownload datum
    pdownload' <- pnext
    directDownload cdownload' pdownload'
