{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Ouroboros.Network.Protocol.Chain.Producer where

import qualified Data.Foldable as Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Protocol.Chain.ProducerStream

import Ouroboros.Network.MonadClass.MonadSTM

data Changing t where
  Forked    :: t -> Changing t
  Extended  :: t -> Changing t
  Unchanged :: t -> Changing t

unChanging :: Changing t -> t
unChanging it = case it of
  Forked    t -> t
  Extended  t -> t
  Unchanged t -> t

type Exhausted = Bool

-- Need to express the idea that if the chain segment is already forked, and
-- we extend that fork, then it's still forked.
--
--   Changing ChainSegment -> (ChainSegment -> Changing ChainSegment) -> Changing ChainSegment
--
-- If either is forked, the result is forked. If both are extended, it's extended.
-- If both are unchanged, it's unchanged.
carryChange :: (t -> Changing t) -> Changing t -> Changing t
carryChange k (Forked s)    = Forked (unChanging (k s))
carryChange k (Extended s)  = case k s of
  Forked t -> Forked t
  Extended t -> Extended t
  Unchanged t -> Extended t
carryChange k (Unchanged t) = k t

-- | A blockchain with a read pointer and a tip picked out. Like a zipper.
data ChainSegment header where
  -- | Read pointer is at the tip.
  AtTip     :: Seq header -> header -> ChainSegment header
  -- | Read pointer is behind the tip.
  BehindTip :: Seq header -> header -> Seq header -> header -> ChainSegment header

deriving instance Show header => Show (ChainSegment header)

-- | chainSegmentToChain must always give a valid blockchain.
chainSegmentToChain :: ChainSegment header -> NonEmpty header
chainSegmentToChain cs = case cs of
  -- unsafe NE.fromList is clearly safe here.
  AtTip     prior tip         -> NE.fromList $ Foldable.toList $ prior Seq.|> tip
  BehindTip prior rp  mid tip -> NE.fromList $ Foldable.toList $ (prior Seq.|> rp) <> (mid Seq.|> tip)

-- | Read pointer is at the oldest.
chainSegmentFromChain :: NonEmpty header -> ChainSegment header
chainSegmentFromChain (b NE.:| bs) = case bs of
  [] -> AtTip mempty b
  [b'] -> BehindTip mempty b mempty b'
  (b' : bs') -> BehindTip mempty b (Seq.fromList (NE.init (b' NE.:| bs'))) (NE.last (b' NE.:| bs'))

-- | Gives 'Nothing' if the 'NonEmpty Block' has nothing in common with the
-- 'ChainSegment' (so we don't have a new read pointer).
switchToChain
  :: forall header .
     (header -> header -> Bool) -- ^ Header equality
  -> NonEmpty header
  -> ChainSegment header
  -> Maybe (Changing (ChainSegment header))
switchToChain headerEq newChain cs = case cs of
  AtTip prior tip -> case cmpChains (NE.toList newChain) (Foldable.toList (prior Seq.|> tip)) [] of
    ([], _:_, _:_) -> error "switchToChain: impossible" -- because it's NonEmpty
    -- They have nothing in common.
    (_, _, []) -> Nothing
    -- They are the same.
    ([], [], _) -> Just (Unchanged cs)
    (m:ms, excluded, p:ps) ->
      let tip' = NE.last (m NE.:| ms)
          mid  = NE.init (m NE.:| ms)
          cs'  = BehindTip (Seq.reverse (Seq.fromList ps)) p (Seq.fromList mid) tip'
      in  case excluded of
            -- All of the current chain is in the new.
            [] -> Just (Extended cs')
            -- Some of the current chain is not in the new.
            _  -> Just (Forked cs')
  -- If it's a proper fork but it includes the read pointer then we call it an
  -- extension.
  BehindTip prior rp _ tip -> case cmpChains (NE.toList newChain) (Foldable.toList (prior Seq.|> rp)) [] of
    ([], _:_, _:_) -> error "switchToChain: impossible" -- because it's NonEmpty
    -- Nothing in common with the chain up to the read pointer.
    (_, _, []) -> Nothing
    -- It is the chain up to the read pointer.
    -- It's called an "extension" even though it's now shorter. Bad terminology.
    -- Extension means that the read pointer is in the new chain.
    ([], [], p:ps) -> Just (Extended (AtTip (Seq.reverse (Seq.fromList ps)) p))
    (m:ms, excluded, p:ps) ->
      let tip' = NE.last (m NE.:| ms)
          mid' = NE.init (m NE.:| ms)
          cs' = BehindTip (Seq.reverse (Seq.fromList ps)) p (Seq.fromList mid') tip'
      in  case excluded of
            -- Includes chain up to read pointer.
            -- Even if it forks after the read pointer, we call it an extension.
            -- However, we need to check whether it does, because if it doesn't
            -- then it's Unchanged.
            -- Assuming the chain and chain segment are well-formed, we can
            -- just check the tip hashes.
            [] -> if tip' `headerEq` tip
                  then Just (Unchanged cs')
                  else Just (Extended cs')
            -- Forks before the read pointer (does not include it). Always a fork.
            _  -> Just (Forked cs')
  where
  -- The first element is the suffix of the first chain not in the second.
  -- The second element is the suffix of the second chain not in the first.
  -- The third element is the common prefix, reversed.
  cmpChains :: [header] -> [header] -> [header] -> ([header], [header], [header])
  cmpChains []     bs     acc = ([], bs, acc)
  cmpChains as     []     acc = (as, [], acc)
  cmpChains (a:as) (b:bs) acc =
    if a `headerEq` b
    then cmpChains as bs (a:acc)
    else (a:as, b:bs, acc)

chainSegmentTip :: ChainSegment header -> header
chainSegmentTip it = case it of
  AtTip     _ b     -> b
  BehindTip _ _ _ b -> b

chainSegmentReadPointer :: ChainSegment header -> header
chainSegmentReadPointer cs = case cs of
  AtTip     _ b     -> b
  BehindTip _ b _ _ -> b

-- | Simple but inefficient. Bumps up the read pointer to the newest point
-- that's an ancestor of the tip.
findBestCheckpoint
  :: forall point header .
     ( Ord point )
  => (header -> point)
  -> NonEmpty point
  -> ChainSegment header
  -> ChainSegment header
findBestCheckpoint mkPoint cps cs = case cs of
  -- No sense improving when we're already at the tip (nothing is better).
  AtTip     _     _          -> cs
  -- We'll traverse (mid |> tip) in reverse checking whether any of them are
  -- checkpoints. The first one that is becomes the new read pointer, and the
  -- prefix of the list is put into prior.
  BehindTip prior rp mid tip ->
    if isCheckpoint (mkPoint tip)
    then AtTip (prior <> (rp Seq.<| mid)) tip
    else case splitAtCheckpoint mid mempty of
            -- No checkpoint found in there; no change.
            Nothing                  -> cs
            -- Found one. The old read pointer 'tp' goes into the prior.
            Just (prior', rp', mid') -> BehindTip (prior <> (rp Seq.<| prior')) rp' mid' tip
  where
  cpSet :: Set point
  cpSet = Set.fromList (NE.toList cps)
  isCheckpoint :: point -> Bool
  isCheckpoint = flip Set.member cpSet
  -- First argument is a chain to search (newest-to-oldest)
  -- Second argument is an accumulator for the consumed part of the list.
  -- The first checkpoint found in the first list is given as the second
  -- component, and on either side of it are the parts of the chain before it
  -- and after it.
  splitAtCheckpoint :: Seq header -> Seq header -> Maybe (Seq header, header, Seq header)
  splitAtCheckpoint mid acc = case Seq.viewr mid of
    Seq.EmptyR       -> Nothing
    mid' Seq.:> here ->
      if isCheckpoint (mkPoint here)
      then Just (mid', here, acc)
      else splitAtCheckpoint mid' (here Seq.<| acc)

-- | Derive a 'ProducerStream' from a mutable 'ChainSegment'.
--
-- The oldest block in the segment is the read pointer: what the consumer will
-- get on the next request for a block (the consumer must know the parent of
-- this block).
--
-- There's a contract on the use of this 'TVar m (Changing ChainSegment)'. If
-- it's replaced with a 'ChainSegment' which extends (contains) the current one
-- then it needs to be set to 'Extended'. If the new 'ChainSegment' forks
-- (is different from and does not contain) the old one, it must be set to
-- 'Forked'. Otherwise, 'Unchanged'. Also, the 'ChainSegment' itself must
-- be set appropriately so that the parent of the oldest block is in the old
-- segment or is an ancestor of the old segment.
--
-- Failing any of these will not cause any protocol errors, it'll just confuse
-- the consumer and probably cause it to terminate the protocol.
simpleProducerStream
  :: ( MonadSTM m stm, Ord point )
  => (header -> point)
  -> TVar m (Changing  (ChainSegment header), Exhausted)
  -> ProducerStream point header m ()
simpleProducerStream mkPoint chainVar = ProducerStream $ atomically $ do
  cs <- unChanging . fst <$> readTVar chainVar
  let tip = mkPoint (chainSegmentTip cs)
      readPointer = mkPoint (chainSegmentReadPointer cs)
  pure $ ProducerInit
    { initPoints     = (readPointer, tip)
    , producerChoice = simpleProducerChoice mkPoint chainVar
    }

simpleProducerChoice
  :: forall point header m stm .
     ( MonadSTM m stm, Ord point )
  => (header -> point)
  -> TVar m (Changing (ChainSegment header), Exhausted)
  -> ProducerChoice point header m ()
simpleProducerChoice mkPoint chainVar = ProducerChoice
  { producerImprove  = simpleProducerImprove
  , producerDownload = simpleProducerDownload
  , producerNext     = simpleProducerNext
  , producerDone     = ()
  }
  where

  simpleProducerImprove
    :: NonEmpty point
    -> m (ProducerImprove point header m ())
  simpleProducerImprove cps = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      (Unchanged chain, exhausted) -> do
        let !chain'        = findBestCheckpoint mkPoint cps chain
            newReadPointer = mkPoint (chainSegmentReadPointer chain')
        writeTVar chainVar (Unchanged chain', exhausted)
        pure $ ImprovePoint (newReadPointer, Nothing) (simpleProducerChoice mkPoint chainVar)
      (Extended chain, exhausted) -> do
        let newTip         = mkPoint (chainSegmentTip chain)
            -- tip is guaranteed to be the same a chainSegmentTip chain'.
            -- 'findBestCheckpoint never changes the tip.
            !chain'        = findBestCheckpoint mkPoint cps chain
            newReadPointer = mkPoint (chainSegmentReadPointer chain')
        writeTVar chainVar (Unchanged chain', exhausted)
        pure $ ImprovePoint (newReadPointer, Just newTip) (simpleProducerChoice mkPoint chainVar)
      (Forked chain, exhausted) -> do
        let newReadPointer = mkPoint (chainSegmentReadPointer chain)
            newTip         = mkPoint (chainSegmentTip chain)
        writeTVar chainVar (Unchanged chain, exhausted)
        pure $ ImproveForked (newReadPointer, newTip) (simpleProducerChoice mkPoint chainVar)

  -- A subtle point here: for nextChange we want to wake up and send the
  -- message even if it's not a fork (just an extension).
  -- For download, an extension should be deferred until after the
  -- download, but a fork should interrupt it.
  simpleProducerNext
    :: ()
    -> m (ProducerNext point header m ())
  simpleProducerNext () = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      (Unchanged _, False) -> retry
      (Unchanged _, True)  -> pure $ NextExhausted ()
      (Forked chain, exhausted) -> do
        let readPointer = mkPoint (chainSegmentReadPointer chain)
            tip         = mkPoint (chainSegmentTip chain)
        -- Set to unchanged, since the consumer now knows it.
        writeTVar chainVar (Unchanged chain, exhausted)
        pure $ NextForked (readPointer, tip) (simpleProducerChoice mkPoint chainVar)
      (Extended chain, exhausted) -> do
        let tip = mkPoint (chainSegmentTip chain)
        writeTVar chainVar (Unchanged chain, exhausted)
        pure $ NextExtended tip (simpleProducerChoice mkPoint chainVar)

  -- Blocks are read out from the TVar one-at-a-time, as opposed to, say,
  -- getting a batch out and then serving them all up before checking again,
  -- which gives fever TVar reads but also can introduce latency in realising
  -- that a fork happened.
  --
  -- If the chain in the TVar is updated then we do the following:
  -- - if it was a fork, stop the block download and present the fork
  -- - if it was a continuation, keep going but give the new header too
  --
  simpleProducerDownload
    :: Word
    -> m (ProducerDownload point header m ())
  simpleProducerDownload 0 = pure $ DownloadOver Nothing (simpleProducerChoice mkPoint chainVar)
  simpleProducerDownload n = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      (Forked chain, exhausted) -> do
        let readPointer = mkPoint (chainSegmentReadPointer chain)
            tip         = mkPoint (chainSegmentTip chain)
        writeTVar chainVar (Unchanged chain, exhausted)
        pure $ DownloadForked (readPointer, tip) (simpleProducerChoice mkPoint chainVar)
      (Extended chain, exhausted) -> case advance chain of
        Just (h, !chain') -> do
          let tip = mkPoint (chainSegmentTip chain)
          writeTVar chainVar (Unchanged chain', exhausted)
          pure $ DownloadHeader (h, Just tip) (simpleProducerDownload (n-1))
        Nothing -> do
          let tip = mkPoint (chainSegmentTip chain)
          pure $ DownloadOver (Just tip) (simpleProducerChoice mkPoint chainVar)
      (Unchanged chain, exhausted) -> case advance chain of
        Just (h, !chain') -> do
          writeTVar chainVar (Unchanged chain', exhausted)
          pure $ DownloadHeader (h, Nothing) (simpleProducerDownload (n-1))
        Nothing ->
          pure $ DownloadOver Nothing (simpleProducerChoice mkPoint chainVar)

  -- | Advance the read pointer, giving Nothing case it's past the tip.
  -- You get the block to send (former oldest block) and the remaining
  -- segment.
  advance :: ChainSegment header -> Maybe (header, ChainSegment header)
  advance chain = case chain of
    AtTip     _     _          -> Nothing
    BehindTip prior rp mid tip -> Just $ case Seq.viewl mid of
     Seq.EmptyL -> (tip, AtTip (prior Seq.|> rp) tip)
     b Seq.:< bs -> (b, BehindTip (prior Seq.|> rp) b bs tip)
