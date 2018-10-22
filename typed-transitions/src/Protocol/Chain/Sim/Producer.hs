{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

module Protocol.Chain.Sim.Producer where

import qualified Data.Foldable as Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Protocol.Chain.StreamProducer

import Block
import Chain
import qualified Chain (head)
import MonadClass.MonadSTM

data Changing t where
  Exhausted :: t -> Changing t
  Forked    :: t -> Changing t
  Extended  :: t -> Changing t
  Unchanged :: t -> Changing t

unChanging :: Changing t -> t
unChanging it = case it of
  Forked    t -> t
  Extended  t -> t
  Unchanged t -> t
  Exhausted t -> t

-- Need to express the idea that if the chain segment is already forked, and
-- we extend that fork, then it's still forked.
--
--   Changing ChainSegment -> (ChainSegment -> Changing ChainSegment) -> Changing ChainSegment
--
-- If either is forked, the result is forked. If both are extended, it's extended.
-- If both are unchanged, it's unchanged.
carryChange :: (t -> Changing t) -> Changing t -> Changing t
carryChange _ (Exhausted t) = Exhausted t
carryChange k (Forked s)    = Forked (unChanging (k s))
carryChange k (Extended s)  = case k s of
  Exhausted t -> Exhausted t
  Forked t -> Forked t
  Extended t -> Extended t
  Unchanged t -> Extended t
carryChange k (Unchanged t) = k t

-- | A blockchain with a read pointer and a tip picked out. Like a zipper.
data ChainSegment p where
  -- | Read pointer is at the tip.
  AtTip     :: Seq (Block p) -> Block p -> ChainSegment p
  -- | Read pointer is behind the tip.
  BehindTip :: Seq (Block p) -> Block p -> Seq (Block p) -> Block p -> ChainSegment p

deriving instance Show (ChainSegment p)

-- | chainSegmentToChain must always give a valid blockchain.
chainSegmentToChain :: ChainSegment p -> NonEmpty (Block p)
chainSegmentToChain cs = case cs of
  -- unsafe NE.fromList is clearly safe here.
  AtTip     prior tip         -> NE.fromList $ Foldable.toList $ prior Seq.|> tip
  BehindTip prior rp  mid tip -> NE.fromList $ Foldable.toList $ (prior Seq.|> rp) <> (mid Seq.|> tip)

-- | Read pointer is at the oldest.
chainSegmentFromChain :: NonEmpty (Block p) -> ChainSegment p
chainSegmentFromChain (b NE.:| bs) = case bs of
  [] -> AtTip mempty b
  [b'] -> BehindTip mempty b mempty b'
  (b' : bs') -> BehindTip mempty b (Seq.fromList (NE.init (b' NE.:| bs'))) (NE.last (b' NE.:| bs'))

-- | Gives 'Nothing' if the 'NonEmpty Block' has nothing in common with the
-- 'ChainSegment' (so we don't have a new read pointer).
switchToChain :: NonEmpty (Block p) -> ChainSegment p -> Maybe (Changing (ChainSegment p))
switchToChain newChain cs = case cs of
  AtTip prior tip -> case cmpChains (NE.toList newChain) (Foldable.toList (prior Seq.|> tip)) [] of
    ([], _:_, _:_) -> error "switchToChain: impossible"
    -- They have nothing in common.
    (_, _, []) -> Nothing
    -- They are the same.
    ([], [], _) -> Just (Unchanged cs)
    (m:ms, excluded, p:ps) ->
      let tip = NE.last (m NE.:| ms)
          mid = NE.init (m NE.:| ms)
          cs' = BehindTip (Seq.reverse (Seq.fromList ps)) p (Seq.fromList mid) tip
      in  case excluded of
            -- All of the current chain is in the new.
            [] -> Just (Extended cs')
            -- Some of the current chain is not in the new.
            _  -> Just (Forked cs')
  -- If it's a proper fork but it includes the read pointer then we call it an
  -- extension.
  BehindTip prior rp mid tip -> case cmpChains (NE.toList newChain) (Foldable.toList (prior Seq.|> rp)) [] of
    ([], _:_, _:_) -> error "switchToChain: impossible"
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
            [] -> if headerHash (blockHeader tip') == headerHash (blockHeader tip)
                  then Just (Unchanged cs')
                  else Just (Extended cs')
            -- Forks before the read pointer (does not include it). Always a fork.
            _  -> Just (Forked cs')
  where
  -- The first element is the suffix of the first chain not in the second.
  -- The second element is the suffix of the second chain not in the first.
  -- The third element is the common prefix, reversed.
  cmpChains :: [Block p] -> [Block p] -> [Block p] -> ([Block p], [Block p], [Block p])
  cmpChains []     bs     acc = ([], bs, acc)
  cmpChains as     []     acc = (as, [], acc)
  cmpChains (a:as) (b:bs) acc =
    if headerHash (blockHeader a) == headerHash (blockHeader b)
    then cmpChains as bs (a:acc)
    else (a:as, b:bs, acc)

chainSegmentTip :: ChainSegment p -> Block p
chainSegmentTip it = case it of
  AtTip     _ b     -> b
  BehindTip _ _ _ b -> b

chainSegmentReadPointer :: ChainSegment p -> Block p
chainSegmentReadPointer cs = case cs of
  AtTip     _ b     -> b
  BehindTip _ b _ _ -> b

-- | Simple but inefficient. Bumps up the read pointer to the newest point
-- that's an ancestor of the tip.
findBestCheckpoint :: NonEmpty Point -> ChainSegment p -> ChainSegment p
findBestCheckpoint cps cs = case cs of
  -- No sense improving when we're already at the tip (nothing is better).
  AtTip     _     _          -> cs
  -- We'll traverse (mid |> tip) in reverse checking whether any of them are
  -- checkpoints. The first one that is becomes the new read pointer, and the
  -- prefix of the list is put into prior.
  BehindTip prior rp mid tip ->
    if isCheckpoint (blockPoint tip)
    then AtTip (prior <> (rp Seq.<| mid)) tip
    else case splitAtCheckpoint mid mempty of
            -- No checkpoint found in there; no change.
            Nothing                  -> cs
            -- Found one. The old read pointer 'tp' goes into the prior.
            Just (prior', rp', mid') -> BehindTip (prior <> (rp Seq.<| prior')) rp' mid' tip
  where
  cpSet :: Set Point
  cpSet = Set.fromList (NE.toList cps)
  isCheckpoint :: Point -> Bool
  isCheckpoint = flip Set.member cpSet
  -- First argument is a chain to search (newest-to-oldest)
  -- Second argument is an accumulator for the consumed part of the list.
  -- The first checkpoint found in the first list is given as the second
  -- component, and on either side of it are the parts of the chain before it
  -- and after it.
  splitAtCheckpoint :: Seq (Block p) -> Seq (Block p) -> Maybe (Seq (Block p), Block p, Seq (Block p))
  splitAtCheckpoint mid acc = case Seq.viewr mid of
    Seq.EmptyR       -> Nothing
    mid' Seq.:> here ->
      if isCheckpoint (blockPoint here)
      then Just (mid', here, acc)
      else splitAtCheckpoint mid' (here Seq.<| acc)

-- | Derive a 'BlockStream' from a mutable 'ChainSegment'.
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
simpleBlockStream
  :: ( MonadSTM m stm ) 
  => TVar m (Changing  (ChainSegment p))
  -> BlockStream p m ()
simpleBlockStream chainVar = BlockStream $ atomically $ do
  cs <- unChanging <$> readTVar chainVar
  let tipHeader = blockHeader (chainSegmentTip cs)
      readPointer = blockPoint (chainSegmentReadPointer cs)
  pure $ BlockStreamAt
    { bsTip = tipHeader
    , bsReadPointer = readPointer
    , bsNext = simpleBlockStreamNext chainVar
    }

simpleBlockStreamNext
  ::forall p m stm .
     ( MonadSTM m stm )
  => TVar m (Changing (ChainSegment p))
  -> BlockStreamNext p m ()
simpleBlockStreamNext chainVar = BlockStreamNext
  { bsNextChange = nextChange
  , bsNextBlock  = nextBlock
  , bsImprove    = improve
  }
  where

  -- A subtle point here: for nextChange we want to wake up and send the
  -- message even if it's not a fork (just an extension).
  --  But for the other ones, extensions can be ignored.... no, only for
  --  block download should they be ignored.
  --  Ah no, for download, an extension should be deferred until after the
  --  download, but a fork should inmterrupt it,.
  nextChange :: forall f . m (Either () (StreamStep p f (NextChange p) m ()))
  nextChange = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      Exhausted _     -> pure $ Left ()
      Unchanged _     -> retry
      Forked    chain -> do
        let readPointer = blockPoint (chainSegmentReadPointer chain)
            tip         = blockHeader (chainSegmentTip chain)
        -- Set to unchanged, since the consumer now knows it.
        writeTVar chainVar (Unchanged chain)
        pure $ Right $ ChangeFork readPointer tip (simpleBlockStreamNext chainVar)
      -- In this presentation, we always have the bodies. Fast relaying will
      -- be done whenever the chain segment is AtTip.
      Extended  chain -> do
        let tip = chainSegmentTip chain
            tipHeader = blockHeader tip
            tipBody = blockBody tip
            relaying = case chain of
              AtTip _ b ->
                Relaying (pure (NoChange (RelayBody tipBody (simpleBlockStreamNext chainVar))))
              _         ->
                NoRelay (simpleBlockStreamNext chainVar)
        writeTVar chainVar (Unchanged chain)
        pure $ Right $ ChangeExtend tipHeader relaying

  -- Blocks are read out from the TVar one-at-a-time, as opposed to, say,
  -- getting a batch out and then serving them all up before checking again,
  -- which gives fever TVar reads but also can introduce latency in realising
  -- that a fork happened.
  --
  -- If the chain in the TVar is updated then we do the following:
  -- - if it was a fork, stop the block download and present the fork
  -- - if it was a continuation, keep going but give the new header too
  --
  nextBlock :: m (StreamStep p (NextBlock p) (NextBlock p) m ())
  nextBlock = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      Exhausted chain -> case advance chain of
        Just (b, !chain') -> do
          writeTVar chainVar (Exhausted chain')
          pure $ NoChange (NextBlock b (simpleBlockStreamNext chainVar))
        Nothing ->
          pure $ NoChange (NoNextBlock (simpleBlockStreamNext chainVar))
      Forked chain    -> do
        let readPointer = blockPoint (chainSegmentReadPointer chain)
            tip         = blockHeader (chainSegmentTip chain)
        writeTVar chainVar (Unchanged chain)
        pure $ ChangeFork readPointer tip (simpleBlockStreamNext chainVar)
      Extended chain  -> case advance chain of
        Just (b, !chain') -> do
          let tip = blockHeader (chainSegmentTip chain)
          writeTVar chainVar (Unchanged chain')
          pure $ ChangeExtend tip (NextBlock b (simpleBlockStreamNext chainVar))
        Nothing -> do
          let tip = blockHeader (chainSegmentTip chain)
          pure $ ChangeExtend tip (NoNextBlock (simpleBlockStreamNext chainVar))
      Unchanged chain -> case advance chain of
        Just (b, !chain') -> do
          writeTVar chainVar (Unchanged chain')
          pure $ NoChange (NextBlock b (simpleBlockStreamNext chainVar))
        Nothing ->
          pure $ NoChange (NoNextBlock (simpleBlockStreamNext chainVar))

  -- | Advance the read pointer, giving Nothing case it's past the tip.
  -- You get the block to send (former oldest block) and the remaining
  -- segment.
  advance :: ChainSegment p -> Maybe (Block p, ChainSegment p)
  advance chain = case chain of
    AtTip     _     _          -> Nothing
    BehindTip prior rp mid tip -> Just $ case Seq.viewl mid of
     Seq.EmptyL -> (tip, AtTip (prior Seq.|> rp) tip)
     b Seq.:< bs -> (b, BehindTip (prior Seq.|> rp) b bs tip)

  improve :: NonEmpty Point -> m (StreamStep p (Improve p) (Improve p) m ())
  improve cps = atomically $ do
    cchain <- readTVar chainVar
    case cchain of
      Exhausted chain -> do
        let !chain'        = findBestCheckpoint cps chain
            newReadPointer = blockPoint (chainSegmentReadPointer chain')
        writeTVar chainVar (Exhausted chain')
        pure $ NoChange $ Improve newReadPointer (simpleBlockStreamNext chainVar)
      Unchanged chain -> do
        let !chain'        = findBestCheckpoint cps chain
            newReadPointer = blockPoint (chainSegmentReadPointer chain')
        writeTVar chainVar (Unchanged chain')
        pure $ NoChange $ Improve newReadPointer (simpleBlockStreamNext chainVar)
      Forked chain -> do
        let newReadPointer = blockPoint (chainSegmentReadPointer chain)
            tip            = blockHeader (chainSegmentTip chain)
        writeTVar chainVar (Unchanged chain)
        pure $ ChangeFork newReadPointer tip (simpleBlockStreamNext chainVar)
      Extended chain  -> do
        let tip            = blockHeader (chainSegmentTip chain)
            -- tip is guaranteed to be the same a chainSegmentTip chain'.
            -- 'findBestCheckpoint never changes the tip.
            !chain'        = findBestCheckpoint cps chain
            newReadPointer = blockPoint (chainSegmentReadPointer chain')
        writeTVar chainVar (Unchanged chain')
        pure $ ChangeExtend tip (Improve newReadPointer (simpleBlockStreamNext chainVar))
