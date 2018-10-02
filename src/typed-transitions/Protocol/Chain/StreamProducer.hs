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

module Protocol.ChainExchange where

import Block
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import Protocol.Core
import Protocol.Chain.Type

-- | A producer backed by a monadic stream of blocks.
--
-- It'll go until the stream is exhausted. If that happens, the state is
-- guaranteed to be  'Yielding ('StBusy 'Next)  because the consumer _must_
-- have asked for next, and the lack of a next stream left the producer hanging.
-- A 'BlockStream m Void' can be used to guarantee that this never happens.
--
-- The producer oddly begins in a 'Yielding state. That's because it must begin
-- by sending its tip and read pointer, so that the consumer always knows
-- these two things.
streamProducer
  :: ( Monad m )
  => BlockStream m t
  -> Peer ChainExchange TrChainExchange ('Yielding 'StInit) ('Yielding ('StBusy 'Next)) m t
streamProducer bs = case bs of
  BlockStreamEnd tip _ -> over (TrInit (blockHeader tip) (blockHeader tip)) (streamProducerMain bs)
  BlockStreamMid readPointer tip _ _ _ -> over (TrInit (blockHeader readPointer) (blockHeader tip)) (streamProducerMain bs)

streamProducerMain
  :: forall m t .
     ( Monad m )
  => BlockStream m t
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StIdle) ('Yielding ('StBusy 'Next)) m t
streamProducerMain bs = await $ \req -> case req of

  TrRequest ReqNext -> case bs of

    -- They asked for the next, and we're fully syncd. We'll either get a fork,
    -- or a header that extends the current tip of chain (or Nothing meaning
    -- the stream is over).
    BlockStreamEnd _ next -> hole $ next >>= \bsSyncd -> case bsSyncd of
      Left t -> pure $ done t
      -- Fast relay of header then body when sync'd.
      Right (SyncdPartial header nextSync) -> pure $
        part (TrRespond (ResRelayHeader header)) (relayBody header nextSync)
        where
        relayHeaderAgain
          :: Header
          -> m (NextSync m t)
          -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Relay)) ('Yielding ('StBusy 'Next)) m t
        relayHeaderAgain header nextSync = part (TrRespond (ResRelayHeaderAgain header)) (relayBody header nextSync)
        relayBody
          :: Header
          -> m (NextSync m t)
          -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Relay)) ('Yielding ('StBusy 'Next)) m t
        relayBody header next = hole $ next >>= \it -> case it of
          NextSyncFork bs' -> pure $ streamProducerForked bs'
          NextSyncHeader header' nextSync -> pure $ relayHeaderAgain header' nextSync
          -- When we get the body we send it down, and we're definitely in
          -- sync'd state again.
          NextSyncBody body next ->
            let block = Block header body
                bs' = BlockStreamEnd block next
            in  pure $ over (TrRespond (ResRelayBody body)) (streamProducerMain bs')

      -- Instead of getting the header and then block, we got a new stream
      -- (tip changed).
      Right (SyncdChange bs') -> pure $ streamProducerForked bs'
    -- Asked for next while mid stream.
    BlockStreamMid _ _ next _ _ -> hole $ next >>= \mBs -> case mBs of
      Left t -> pure $ done t
      Right bs -> pure $ streamProducerForked bs

  TrRequest (ReqSetHead cps) -> case bs of
    -- They tried to improve the read pointer, but we're syncd. Just send back
    -- the tip hash (no change).
    BlockStreamEnd tip _ -> over (TrRespond (ResSetHead (headerHash (blockHeader tip)))) (streamProducerMain bs)
    BlockStreamMid _ _ _ improve _ -> hole $ improve cps >>= \it -> case it of
      Forked bs -> pure $ streamProducerForked bs
      NotForked bs' ->
        let bestHash = case bs' of
              BlockStreamEnd tip _ -> headerHash (blockHeader tip)
              BlockStreamMid rp _ _ _ _ -> headerHash (blockHeader rp)
        in  pure $ over (TrRespond (ResSetHead bestHash)) (streamProducerMain bs')

  TrRequest (ReqDownload num) -> respondDownload bs num

  where

  respondDownload
    :: BlockStream m t
    -> Word
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Download)) ('Yielding ('StBusy 'Next)) m t
  respondDownload bs 0 = over (TrRespond ResDownloadDone) (streamProducerMain bs)
  respondDownload bs n = case bs of
    BlockStreamEnd _ _ -> over (TrRespond ResDownloadDone) (streamProducerMain bs)
    BlockStreamMid _ _ _ _ nextBlock -> hole $ nextBlock >>= \it -> case it of
      Forked bs' -> pure $ streamProducerForked bs'
      NotForked (block, bs') -> pure $ part (TrRespond (ResDownloadOne block)) (respondDownload bs' (n-1))

-- | Deal with a fork in the block stream.
streamProducerForked
  :: ( Monad m, Typeable anything )
  => BlockStream m t
  -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy anything)) ('Yielding ('StBusy 'Next)) m t
streamProducerForked bs = over (TrRespond (ResChange rp tip)) (streamProducerMain bs)
  where
  (rp, tip) = case bs of
    BlockStreamEnd tip _ -> (blockHeader tip, blockHeader tip)
    BlockStreamMid rp tip _ _ _ -> (blockHeader rp, blockHeader tip)

data BlockStream m t where
  -- | This is the tip of the chain and also the read pointer (the stream has
  -- been completely consumed).
  BlockStreamEnd
    :: Block -- ^ Read pointer and tip of chain
    -> m (Either t (BlockStreamSyncd m t)) -- ^ Next change to tip, or t if it's done.
    -> BlockStream m t
  -- | The stream is partially consumed.
  BlockStreamMid
    :: Block -- ^ Read pointer
    -> Block -- ^ Tip of chain (/= read pointer)
    -> m (Either t (BlockStream m t))                          -- ^ Next change to tip.
    -> (NonEmpty HeaderHash -> m (Fork m (BlockStream m t) t)) -- ^ Improve
    -> m (Fork m (Block, BlockStream m t) t)                   -- ^ Next block
    -> BlockStream m t

data Fork m r t where
  Forked    :: BlockStream m t -> Fork m r t
  NotForked :: r -> Fork m r t

data BlockStreamSyncd m t where
  -- | The stream is completely read except for the body of this header, which
  -- is the tip of chain.
  SyncdPartial :: Header -> m (NextSync m t) -> BlockStreamSyncd m t
  SyncdChange  :: BlockStream m t -> BlockStreamSyncd m t

data NextSync m t where
  NextSyncHeader    :: Header -> m (NextSync m t) -> NextSync m t
  NextSyncBody      :: Body -> m (Either t (BlockStreamSyncd m t)) -> NextSync m t
  NextSyncFork      :: BlockStream m t -> NextSync m t

pureBlockStream :: Applicative m => NonEmpty Block -> BlockStream m ()
pureBlockStream (b NE.:| []) = BlockStreamEnd b (pure (Left ()))
pureBlockStream this@(b1 NE.:| (b2 : bs)) = BlockStreamMid
  b1
  (NE.last (b2 NE.:| bs))
  (pure (Left ())) -- Never changes
  (pure . NotForked . pureBlockStream . cutoffPrefix this)
  nextBlock
  where
  nextBlock :: Applicative m => m (Fork m (Block, BlockStream m ()) ())
  nextBlock = pure (NotForked (b2, pureBlockStream (b2 NE.:| bs)))

cutoffPrefix :: NonEmpty Block -> NonEmpty HeaderHash -> NonEmpty Block
cutoffPrefix this = \cps ->
  -- Move this zipper to each of the checkpoints, then take the one whose
  -- focus has the highest slot number (or this, if no checkpoints are in
  -- this zipper).
  let zs = mapMaybe (\cp -> forwardTo ((==) cp . headerHash . blockHeader) this) (NE.toList cps)
      sorter l r = headerSlot (blockHeader (NE.head l)) `compare` headerSlot (blockHeader (NE.head r))
      sorted = NE.sortBy sorter (this NE.:| zs)
  in  NE.head sorted

forwardTo :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
forwardTo p (it NE.:| [])     = if p it then Just (it NE.:| [])     else Nothing
forwardTo p (it NE.:| (n:ns)) = if p it then Just (it NE.:| (n:ns)) else forwardTo p (n NE.:| ns)

-- | A sequence of 1 or more chains with the possibility of forking.
-- A forking chain has 1 or more blocks before the fork, and 0 or more after
-- the fork. Interpreted as a block stream, the blocks before the fork are
-- served, but not those after the fork. A consumer will see the block before
-- the fork and then a change to the fork. The fork must begin with a block
-- that was already served: appeared in the before-the-fork part of some
-- prior forking chain.
data ForkingChain where
  NoFork :: NonEmpty Block -> ForkingChain
  Fork
    :: NonEmpty Block -- ^ Before the fork
    -> ForkingChain
    -> [Block] -- ^ After the fork
    -> ForkingChain
  deriving (Show)

fcGenesis :: ForkingChain -> Block
fcGenesis fc = case fc of
  NoFork (genesis NE.:| _) -> genesis
  Fork (genesis NE.:| _) _ _ -> genesis

-- | All blocks in the forking chain which show prior to a fork.
fcPriorToFork :: ForkingChain -> NonEmpty Block
fcPriorToFork fc = case fc of
  NoFork chain -> chain
  Fork prior sub _ -> prior <> fcPriorToFork sub

noFork :: NonEmpty Block -> ForkingChain
noFork = NoFork

-- | First component is the prefix common to both lists (based on header hash).
-- It's in reverse order, so that the first element is the newest common block.
splitChains ::  [Block] -> [Block] -> ([Block], [Block], [Block])
splitChains = goSplitChains []
  where
  goSplitChains comm [] bs = (comm, [], bs)
  goSplitChains comm bs [] = (comm, bs, [])
  goSplitChains comm (this : these) (that : those) =
    if headerHash (blockHeader this) == headerHash (blockHeader that)
    then goSplitChains (this : comm) these those
    else (comm, this : these, that : those)

forkAfter :: Word -> NonEmpty Block -> ForkingChain -> ForkingChain
forkAfter n newChain fc = case splitChains (NE.toList (fcPriorToFork fc)) (NE.toList newChain) of
  ([], _, _) -> error "forkAfter: no common genesis"
  (c : _, _, newPart) -> insertFork n (c NE.:| newPart) fc
  where
  insertFork :: Word -> NonEmpty Block -> ForkingChain -> ForkingChain
  insertFork n c fc = case fc of
    NoFork chain -> case NE.splitAt (fromIntegral n) chain of
      -- Forking after 0 blocks: replace it.
      ([], _) -> NoFork c
      (pre : pres, suffix) -> Fork (pre NE.:| pres) (NoFork c) suffix
    Fork before sub after -> Fork before (insertFork n c sub) after

pureForkingChainBlockStream
  :: forall m .
     ( Applicative m )
  => ForkingChain
  -> BlockStream m ()
pureForkingChainBlockStream term = case term of
  NoFork chain -> pureBlockStream chain
  Fork before@(first NE.:| coming) forkingChain after -> BlockStreamMid
    first
    (case after of { [] -> NE.last before ; (a : as) -> NE.last (a NE.:| as) })
    -- The fork is not expected to include the intersection point. That's to
    -- say, the first block in the fork will _not_ be the read pointer, its
    -- _parent_ will.
    (pure (Right (pureForkingChainBlockStream forkingChain)))
    -- When improving, fork if the best header comes after the fork, but not
    -- if it comes before it
    improve
    nextBlock
    where
    nextBlock :: m (Fork m (Block, BlockStream m ()) ())
    nextBlock = case coming of
      [] -> pure (Forked (pureForkingChainBlockStream forkingChain))
      (b : bs) -> pure (NotForked (b, pureForkingChainBlockStream (Fork (b NE.:| bs) forkingChain after)))
    improve :: NonEmpty HeaderHash -> m (Fork m (BlockStream m ()) ())
    improve = \cps ->
      let before' = cutoffPrefix before' cps
          inAfter = case after of 
            [] -> False
            (a : as) -> headerHash (blockHeader a) /= headerHash (blockHeader (NE.head (cutoffPrefix (a NE.:| as) cps)))
      in  if inAfter
          -- The 'after' chain changed after cutoffPrefix, so the read pointer
          -- would move to after the fork.
          then pure (Forked (pureForkingChainBlockStream forkingChain))
          else pure (NotForked (pureForkingChainBlockStream (Fork before' forkingChain after)))
