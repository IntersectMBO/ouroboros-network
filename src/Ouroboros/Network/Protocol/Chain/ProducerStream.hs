{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.ProducerStream where

import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import Protocol.Core
import Ouroboros.Network.Protocol.Chain.Type hiding (Improve)

newtype ProducerStream point header m t = ProducerStream
  { runProducerStream :: m (ProducerInit point header m t)
  }

-- | Multiplicative conjunction of the read pointer and header pair, with
-- the additive conjunction of responses to the client.
data ProducerInit point header m t = ProducerInit
  { initPoints     :: (point, point)
    -- ^ Read pointer, followed by tip.
  , producerChoice :: ProducerChoice point header m t
    -- ^ How to deal with consumer choices.
  }

-- | Additive conjunction of responses (which one is used is chosen by the
-- consumer).
data ProducerChoice point header m t = ProducerChoice
  { -- | Consumer asked to improve.
    producerImprove  :: NonEmpty point
                     -> m (ProducerImprove point header m t)
    -- | Consumer asked to download headers.
  , producerDownload :: Word
                     -> m (ProducerDownload point header m t)
    -- | Consumer asked for the next change.
  , producerNext     :: ()
                     -> m (ProducerNext point header m t)
    -- | Consumer terminated.
  , producerDone     :: t
  }

-- | Additive disjunction in response to 'Improve'. Compare at
-- 'ConsumerImprove'.
data ProducerImprove point header m t where
  ImprovePoint  :: (point, Maybe point)
                -> ProducerChoice point header m t
                -> ProducerImprove point header m t
  ImproveForked :: (point, point)
                -> ProducerChoice point header m t
                -> ProducerImprove point header m t

-- | Additive disjunction in response to 'Download'. Compare at
-- 'ConsumerDownload'.
data ProducerDownload point header m t where
  DownloadHeader :: (header, Maybe point)
                 -> m (ProducerDownload point header m t)
                 -> ProducerDownload point header m t
  DownloadOver   :: Maybe point
                 -> ProducerChoice point header m t
                 -> ProducerDownload point header m t
  DownloadForked :: (point, point)
                 -> ProducerChoice point header m t
                 -> ProducerDownload point header m t

-- | Additive disjunction in response to 'Next'. Compare at
-- 'ConsumerNext'.
data ProducerNext point header m t where
  NextForked    :: (point, point)
                -> ProducerChoice point header m t
                -> ProducerNext point header m t
  NextExtended  :: point
                -> ProducerChoice point header m t
                -> ProducerNext point header m t
  NextExhausted :: t
                -> ProducerNext point header m t

streamProducer
  :: ( Monad m )
  => ProducerStream point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Yielding 'StInit) ('Finished 'StDone) m t
streamProducer ps = lift $ runProducerStream ps >>= \psInit ->
  let msg = TrInit (initPoints psInit)
  in  pure $ over msg (streamProducerMain (producerChoice psInit))

streamProducerMain
  :: forall point header m t .
     ( Monad m )
  => ProducerChoice point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting 'StIdle) ('Finished 'StDone) m t
streamProducerMain choice = await $ \req -> case req of

  TrConsumerDone -> done (producerDone choice)

  TrRequest (ReqImprove cps) -> lift $ producerImprove choice cps >>= \it -> case it of
    ImproveForked points choice' ->
      pure $ streamProducerForked points choice'
    ImprovePoint improvement choice' ->
      let msg = TrRespond (ResImprove improvement)
      in  pure $ over msg (streamProducerMain choice')

  TrRequest (ReqDownload num) -> lift $ producerDownload choice num >>= \it ->
    pure $ respondDownload it

  TrRequest ReqNext -> lift $ producerNext choice () >>= \it -> case it of
    NextForked points choice' ->
      pure $ streamProducerForked points choice'
    NextExtended point choice' ->
      let msg = TrRespond (ResExtend point)
      in  pure $ over msg (streamProducerMain choice')
    NextExhausted t ->
      pure $ out TrProducerDone (done t)

  where

  respondDownload
    :: ProducerDownload point header m t
    -> Peer ChainExchange (TrChainExchange point header) ('Yielding ('StBusy 'Download)) ('Finished 'StDone) m t
  respondDownload download = case download of
    DownloadForked points choice ->
      streamProducerForked points choice
    DownloadOver mNewTip choice ->
      let msg = TrRespond (ResDownloadDone mNewTip)
      in  over msg (streamProducerMain choice)
    DownloadHeader datum nextDownload ->
      let msg = TrRespond (ResDownloadOne datum)
      in  part msg $ lift $ fmap respondDownload nextDownload

  -- | Deal with a fork in the header stream.
  streamProducerForked
    :: ( Monad m, Typeable anything )
    => (point, point) -- ^ Read pointer, then tip
    -> ProducerChoice point header m t
    -> Peer ChainExchange (TrChainExchange point header) ('Yielding ('StBusy anything)) ('Finished 'StDone) m t
  streamProducerForked points choice = over msg (streamProducerMain choice)
    where
    msg = TrRespond (ResForked points)
