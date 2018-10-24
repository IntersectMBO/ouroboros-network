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

module Protocol.Chain.ProducerStream where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import Protocol.Core
import Protocol.Chain.Type

newtype ProducerStream point header m t = ProducerStream
  { runProducerStream :: m (ProducerStreamAt point header m t)
  }

data ProducerStreamAt point header m t = ProducerStreamAt
  { bsTip         :: header
  , bsReadPointer :: point
  , bsNext        :: ProducerStreamNext point header m t
  }

data ProducerStreamNext point header m t = ProducerStreamNext
  { -- For next change, the stream can be finished (Left).
    -- The universally quanitified 'f' ensure that you can't get a
    -- 'NoChange' constructor here.
    bsNextChange :: forall f . m (Either t (StreamStep point header f (NextChange point header) m t))
    -- TODO offer a more flexible interface, which allows for streaming n blocks
    -- without doing 'bsNextHeader' every time. Perhaps give a pipe/conduit?
    --
    -- Must not block waiting for the next block if the stream is at the tip.
    -- Must give 'NoNextHeader' instead.
  , bsNextHeader  :: m (StreamStep point header (NextHeader point header) (NextHeader point header) m t)
  , bsImprove    :: NonEmpty point -> m (StreamStep point header (Improve point header) (Improve point header) m t)
    -- | Consumer terminated.
  , bsDone       :: t
  }

data VoidF (f :: Type -> Type) (m :: Type -> Type) (t :: Type) where
  VoidF :: (forall x . x) -> VoidF f m t

impossible :: VoidF f m t -> x
impossible (VoidF absurd) = absurd

-- | At each step in the stream, there could be a change to the tip and/or read
-- pointer.
data StreamStep point header f g m t where
  NoChange   :: f m t -> StreamStep point header f g m t
  -- | The tip header changed and it's not a fork. The read pointer is in
  -- the new chain. Here, the 'f m t' is given, because it's still relevant.
  --
  -- This plays a part of fast relay (header upload concurrently with body
  -- download). In that case, 'f ~ NextChange' (see 'bsNextChange').
  ChangeExtend :: header -> g m t -> StreamStep point header f g m t
  -- | The tip header changed and it's a fork (not an extension). That's to
  -- say, the read point is not in the new chain.
  -- The new read pointer is included (a hash, not the whole header, since the
  -- consumer knows it).
  ChangeFork   :: point -> header -> ProducerStreamNext point header m t -> StreamStep point header f g m t

data NextHeader point header m t where
  NextHeader :: header -> ProducerStreamNext point header m t -> NextHeader point header m t
  -- | At the tip; no next block.
  NoNextHeader :: ProducerStreamNext point header m t -> NextHeader point header m t

data Improve point header m t where
  Improve :: point -> ProducerStreamNext point header m t -> Improve point header m t

data NextChange point header m t where
  NextChange :: ProducerStreamNext point header m t -> NextChange point header m t

streamProducer
  :: ( Monad m )
  => ProducerStream point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Yielding 'StInit) ('Finished 'StDone) m t
streamProducer bs = hole $ runProducerStream bs >>= \bsAt ->
  let msg = TrInit (bsReadPointer bsAt) (bsTip bsAt)
  in  pure $ over msg (streamProducerMain (bsNext bsAt))

streamProducerMain
  :: forall point header m t .
     ( Monad m )
  => ProducerStreamNext point header m t
  -> Peer ChainExchange (TrChainExchange point header) ('Awaiting 'StIdle) ('Finished 'StDone) m t
streamProducerMain bsNext = await $ \req -> case req of

  TrConsumerDone -> done (bsDone bsNext)

  TrRequest (ReqSetHead cps) -> hole $ bsImprove bsNext cps >>= \it -> case it of
    ChangeFork readPointer tip bsNext' ->
      pure $ streamProducerChanged readPointer tip bsNext'
    ChangeExtend header (Improve bestHeaderPoint bsNext') ->
      let msg = TrRespond (ResSetHead bestHeaderPoint (Just header))
      in  pure $ over msg (streamProducerMain bsNext')
    NoChange (Improve bestHeaderPoint bsNext') ->
      let msg = TrRespond (ResSetHead bestHeaderPoint Nothing)
      in  pure $ over msg (streamProducerMain bsNext')

  TrRequest (ReqDownload num) -> respondDownload bsNext num

  TrRequest ReqNext -> hole $ bsNextChange bsNext >>= \it -> case it of

    Left t -> pure $ out TrProducerDone (done t)

    Right (NoChange anything) -> impossible anything

    Right (ChangeFork readPointer tip bsNext') ->
      pure $ streamProducerChanged readPointer tip bsNext'

    Right (ChangeExtend header (NextChange bsNext')) ->
      let msg = TrRespond (ResExtend header)
      in  pure $ over msg (streamProducerMain bsNext')

  where

  respondDownload
    :: ProducerStreamNext point header m t
    -> Word
    -> Peer ChainExchange (TrChainExchange point header) ('Yielding ('StBusy 'Download)) ('Finished 'StDone) m t
  respondDownload bsNext 0 = over (TrRespond (ResDownloadDone Nothing)) (streamProducerMain bsNext)
  respondDownload bsNext n = hole $ bsNextHeader bsNext >>= \it -> case it of
    ChangeFork readPointer tip bsNext' ->
      pure $ streamProducerChanged readPointer tip bsNext'
    ChangeExtend header (NextHeader block bsNext') ->
      let msg = TrRespond (ResDownloadOne block (Just header))
      in  pure $ part msg (respondDownload bsNext' (n-1))
    ChangeExtend header (NoNextHeader bsNext') ->
      let msg = TrRespond (ResDownloadDone (Just header))
      in  pure $ over msg (streamProducerMain bsNext')
    NoChange (NextHeader block bsNext') ->
      let msg = TrRespond (ResDownloadOne block Nothing)
      in  pure $ part msg (respondDownload bsNext' (n-1))
    NoChange (NoNextHeader bsNext') ->
      let msg = TrRespond (ResDownloadDone Nothing)
      in  pure $ over msg (streamProducerMain bsNext')

  -- | Deal with a fork in the block stream.
  streamProducerChanged
    :: ( Monad m, Typeable anything )
    => point  -- ^ Read pointer
    -> header -- ^ Tip
    -> ProducerStreamNext point header m t
    -> Peer ChainExchange (TrChainExchange point header) ('Yielding ('StBusy anything)) ('Finished 'StDone) m t
  streamProducerChanged readPointer tip bsNext = over msg (streamProducerMain bsNext)
    where
    msg = TrRespond (ResChange readPointer tip)
