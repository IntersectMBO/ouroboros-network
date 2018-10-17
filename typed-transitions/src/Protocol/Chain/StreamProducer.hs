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

module Protocol.Chain.StreamProducer where

import Block
import Chain (Point)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import Protocol.Core
import Protocol.Chain.Type

newtype BlockStream m t = BlockStream
  { runBlockStream :: m (BlockStreamAt m t)
  }

data BlockStreamAt m t = BlockStreamAt
  { bsTip         :: Header
  , bsReadPointer :: Point
  , bsNext        :: BlockStreamNext m t
  }

data BlockStreamNext m t = BlockStreamNext
  { -- For next change, the stream can be finished (Left).
    -- The universally quanitified 'f' ensure that you can't get a
    -- 'NoChange' constructor here.
    bsNextChange :: forall f . m (Either t (StreamStep f NextChange m t))
    -- TODO offer a more flexible interface, which allows for streaming n blocks
    -- without doing 'bsNextBlock' every time. Perhaps give a pipe/conduit?
  , bsNextBlock  :: m (StreamStep NextBlock NextBlock m t)
  , bsImprove    :: NonEmpty Point -> m (StreamStep Improve Improve m t)
  }

data VoidF (f :: Type -> Type) (m :: Type -> Type) (t :: Type) where
  VoidF :: (forall x . x) -> VoidF f m t

impossible :: VoidF f m t -> x
impossible (VoidF absurd) = absurd

-- | At each step in the stream, there could be a change to the tip and/or read
-- pointer.
data StreamStep f g m t where
  NoChange   :: f m t -> StreamStep f g m t
  -- | The tip header changed and it's not a fork. The read pointer is in
  -- the new chain. Here, the 'f m t' is given, because it's still relevant.
  --
  -- This plays a part of fast relay (header upload concurrently with body
  -- download). In that case, 'f ~ NextChange' (see 'bsNextChange').
  ChangeExtend :: Header -> g m t -> StreamStep f g m t
  -- | The tip header changed and it's a fork (not an extension). That's to
  -- say, the read point is not in the new chain.
  -- The new read pointer is included (a hash, not the whole header, since the
  -- consumer knows it).
  ChangeFork   :: Point -> Header -> BlockStreamNext m t -> StreamStep f g m t

data NextBlock m t where
  NextBlock :: Block -> BlockStreamNext m t -> NextBlock m t
  -- | At the tip; no next block.
  NoNextBlock :: BlockStreamNext m t -> NextBlock m t

data Improve m t where
  Improve :: Point -> BlockStreamNext m t -> Improve m t

data NextChange m t where
  NoRelay  :: BlockStreamNext m t -> NextChange m t
  Relaying :: m (StreamStep RelayBody NextChange m t) -> NextChange m t

-- | Follow-up to 'RelayHeader' from 'NextChange'. Either put the body through,
-- or give a new header to relay.
data RelayBody m t where
  RelayBody :: Body -> BlockStreamNext m t -> RelayBody m t

--
-- Can this handle some restriction on fast relay? We don't want every one of
-- our peers to fast-relay the header followed by body, apparently.
-- That requires more thought though. The whole point of doing this special case
-- is to lower the minimum time required for a block to reach the next leader,
-- by allowing body and header relays to happen concurrently. A single peer
-- taking in the header and body multiple times shouldn't (I think) cause a
-- slowdown. It will relay the first one received (after verifying it) and then
-- shoot the body out as well once it's in. I suppose the link itself will
-- become saturated but as long as it has enough bandwidth/throughput for the
-- number of peers it should be fine... that's to say, so long as the n
-- block downloads don't contend with one-another, it's fine.

streamProducer
  :: ( Monad m )
  => BlockStream m t
  -> Peer ChainExchange TrChainExchange ('Yielding 'StInit) ('Yielding ('StBusy 'Next)) m t
streamProducer bs = hole $ runBlockStream bs >>= \bsAt ->
  let msg = TrInit (bsReadPointer bsAt) (bsTip bsAt)
  in  pure $ over msg (streamProducerMain (bsNext bsAt))

streamProducerMain
  :: forall m t .
     ( Monad m )
  => BlockStreamNext m t
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StIdle) ('Yielding ('StBusy 'Next)) m t
streamProducerMain bsNext = await $ \req -> case req of

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

    Left t -> pure $ done t

    Right (NoChange anything) -> impossible anything

    Right (ChangeFork readPointer tip bsNext') ->
      pure $ streamProducerChanged readPointer tip bsNext'

    Right (ChangeExtend header (NoRelay bsNext')) ->
      let msg = TrRespond (ResExtend header)
      in  pure $ over msg (streamProducerMain bsNext')

    -- Here the fast relay case comes out.
    Right (ChangeExtend header (Relaying awaitBody)) ->
      let msg = TrRespond (ResExtendRelay header)
      in  pure $ part msg (relayLoop awaitBody)
      where
      relayLoop awaitBody = hole $ awaitBody >>= \it -> case it of
        NoChange (RelayBody body bsNext') ->
          let msg = TrRespond (ResRelayBody body)
          in  pure $ over msg (streamProducerMain bsNext')
        ChangeFork readPointer tip bsNext' ->
          pure $ streamProducerChanged readPointer tip bsNext'
        ChangeExtend header' (NoRelay bsNext') ->
          let msg = TrRespond (ResExtendNew header')
          in  pure $ over msg (streamProducerMain bsNext')
        ChangeExtend header' (Relaying awaitBody') ->
          let msg = TrRespond (ResExtendNewRelay header')
          in  pure $ part msg (relayLoop awaitBody')

  where

  respondDownload
    :: BlockStreamNext m t
    -> Word
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Download)) ('Yielding ('StBusy 'Next)) m t
  respondDownload bsNext 0 = over (TrRespond (ResDownloadDone Nothing)) (streamProducerMain bsNext)
  respondDownload bsNext n = hole $ bsNextBlock bsNext >>= \it -> case it of
    ChangeFork readPointer tip bsNext' ->
      pure $ streamProducerChanged readPointer tip bsNext'
    ChangeExtend header (NextBlock block bsNext') ->
      let msg = TrRespond (ResDownloadOne block (Just header))
      in  pure $ part msg (respondDownload bsNext' (n-1))
    ChangeExtend header (NoNextBlock bsNext') ->
      let msg = TrRespond (ResDownloadDone (Just header))
      in  pure $ over msg (streamProducerMain bsNext')
    NoChange (NextBlock block bsNext') ->
      let msg = TrRespond (ResDownloadOne block Nothing)
      in  pure $ part msg (respondDownload bsNext' (n-1))
    NoChange (NoNextBlock bsNext') ->
      let msg = TrRespond (ResDownloadDone Nothing)
      in  pure $ over msg (streamProducerMain bsNext')

  -- | Deal with a fork in the block stream.
  streamProducerChanged
    :: ( Monad m, Typeable anything )
    => Point  -- ^ Read pointer
    -> Header -- ^ Tip
    -> BlockStreamNext m t
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy anything)) ('Yielding ('StBusy 'Next)) m t
  streamProducerChanged readPointer tip bsNext = over msg (streamProducerMain bsNext)
    where
    msg = TrRespond (ResChange readPointer tip)
